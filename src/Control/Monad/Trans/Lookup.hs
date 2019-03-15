-- | A category of found & parsed objects. Unfound or improperly-formatted objects are collected, thus failing the whole computation while still trying to compute it up to finding-then-parsing objects, as far as it can. Often a program will say "x not found," so you set x, only for the program to now say that y isn't found. LookupT removes this pain by telling /all/ missing or improperly-formatted variables at once.
--
-- === @Applicative@ example:
--
-- @
-- {-# language TypeApplications #-}
-- import Control.Monad.Trans.Lookup
-- import Data.Functor.Identity
-- import qualified Data.Set as S
-- 
-- data C = C String String String deriving Show
-- data D = D String Int    String deriving Show
-- 
-- table1 = [("b", "value")]                               -- missing keys a and c (used for parsing into C)
-- table2 = [("b", "value"), ("a", "valA"), ("c", "valC")] -- has all keys (used for parsing into C and D)
-- table3 = [("b", "value"), ("a", "35"), ("c", "valC")]   -- has all keys, and a is an integer like it should be (used for parsing into D)
-- table4 = [("a", "3i5")]                                 -- missing keys b and c, and a is not even integer like it should be
--                                                         -- (used for parsing into D)
-- 
-- parseInt v = liftME (Improper v "not an integer") $ readMaybe @Int v
-- 
-- lkup :: [(String, String)] -> String -> Lookup String
-- lkup table i = 'llookup' i pure table i
-- 
-- testFn t = runLookup $ C -- parse into C from table t
--         \<$\> lkup t "a"
--         \<*\> lkup t "b"
--         \<*\> lkup t "c"
-- 
-- testFnWParse t = runLookup $ D
--               \<$\> lkup t "b"
--               \<*\> llookup "a" parseInt t "a"
--               \<*\> lkup t "c" -- parse into D
-- @
--
-- >>> testFn table1
-- Left (fromList [Missing: a,Missing: c])
--
-- >>> testFn table2
-- Right (C "valA" "value" "valC")
--
-- >>> testFnWParse table2
-- Left (fromList [Failed parsing valA: not an integer])
--
-- >>> testFnWParse table3
-- Right (D "value" 35 "valC")
--
-- >>> testFnWParse table4
-- Left (fromList [Missing: b,Missing: c,Failed parsing 3i5: not an integer])
--
-- === Monad Example
--
-- The monad instance short-circuits when the LHS argument to @>>=@ is of @Left@.
--
-- There are two main cases for using Kleislis:
--
-- 1. verifying a lookup-only LookupT (such as 'lookupEnv')
-- 2. using a found value as a key for looking-up another value
--
-- ==== Verification Example
--
-- @verify = LookupT . pure . liftME (S.singleton $ Improper "somevar" "not an int") . readMaybe \@Int@
--
-- >>> runLookupT $ verify =<< lookupEnv "SHLVL"
-- Right 5
--
-- >>> runLookupT $ verify =<< lookupEnv "USER"
-- Left (fromList [Failed parsing somevar: not an int])
--
-- ==== Value-is-a-Key Examples
--
-- @
-- table5 = [("X", "Y"), ("Y", "Z")]
-- lkupFn i = llookup i pure table5 i
-- @
--
-- Both keys found:
--
-- >>> runLookup $ lkupFn "X" >>= lkupFn
-- Right "Z"
--
-- First key not found:
--
-- >>> runLookup $ lkupFn "Z" >>= lkupFn
-- Left (fromList [Missing: Z])
--
-- Secound key not found:
--
-- >>> runLookup $ lkupFn "Y" >>= lkupFn
-- Left (fromList [Missing: Z])
module Control.Monad.Trans.Lookup
(
-- * Types
  LookupT (..)
, Lookup
, LookupF (..)
-- ** Decomposer
, runLookup
-- ** Constructors
, lookup
, lookupM
-- * Common Lookup Functions
, llookup
, lookupFind
, lookupFindDesperate
, lookupMaybe
, lookupEnv
, lookupFile
, lookupFileCommon
-- * Utilities
, readMaybe
, liftME
) where

-- base
import Control.Applicative
import Control.Monad (liftM, foldM)
import Data.Bool (bool)
import Data.Functor.Identity
import Data.Monoid (Alt(..))
import Data.Proxy
import Prelude hiding (lookup, Applicative(..))
import System.Directory (doesFileExist)
import qualified Data.Bifunctor as BiF
import qualified Prelude as P
import qualified System.Environment as Env

import qualified Data.Set as S -- containers

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

type family (A a) :: Bool where
    A (Either a b) = 'True
    A a = 'False

-- | Given a zero element and a non-zero element, return the non-zero one. Given two zero elements, prefer the right zero argument.
--
-- "Zero" does not necessarily refer to one element; it may refer to a partition. This is most notably the case for @Either@'s instance, which considers any @Left@ to be of a zero class
--
-- For @Alternative@s just use @(<|>)@. There's no @NonZero@ instance for @Alternative@s because that'd be messy overlapping instances, and redundant.
class NonZero (f :: Bool) a where
    nonZero :: Proxy f -> a -> a -> a

instance NonZero 'True (Either a b) where
    nonZero _ a@(Right _) _ = a
    nonZero _ _ b = b

instance (Monoid a, Eq a) => NonZero 'False a where
    nonZero _ a b = if a == mempty then b else a

newtype LookupT m a = LookupT { runLookupT :: m (Either (S.Set LookupF) a) } deriving Functor
type Lookup a = LookupT Identity a

runLookup :: Lookup a -> Either (S.Set LookupF) a
runLookup = runIdentity . runLookupT

-- | Lookup failed type
data LookupF
    -- | denotes that a variable could not be found
    = Missing { varName :: String }
    -- | denotes that a variable could not be parsed from a string
    | Improper
        { varName :: String
        , reason :: String -- ^ description of why the parsing failed
        }
    deriving Eq

instance Show LookupF where
    show (Missing x) = "Missing: " <> x
    show (Improper x c) = "Failed parsing " <> x <> ": " <> c

-- | In a set of LookupF's, I want Missing variables to be mentioned before variables that failed to parse
instance Ord LookupF where
    compare (Missing x) (Missing y) = compare x y
    compare (Missing _) (Improper _ _) = LT
    compare (Improper _ _) (Missing _) = GT
    compare (Improper x _) (Improper y _) = compare x y

-- | Monadic variant of 'lookup' (parsing function returns in a monad)
lookupM :: Monad m
       => (i -> String)
       -> (a -> m (Either LookupF b))
       -> (i -> s -> m (Maybe a))
       -> s
       -> i
       -> LookupT m b
lookupM toStr p l s i = LookupT $ l i s >>= \case
    Nothing -> pure . Left . S.singleton $ Missing (toStr i)
    Just a -> BiF.first S.singleton <$> p a

-- | The primary way to create a @LookupT@ object
--
-- Remember that you can provide default values by using @\<|\> defaultValue@ in the lookup function (the 3rd parameter)
lookup :: Functor m
       => (i -> String) -- ^ for collecting identifiers as error output strings
       -> (a -> Either LookupF b) -- ^ function that parses a looked-up value into a final value (e.g. @read \@Int@)
       -> (i -> s -> m (Maybe a)) -- ^ the lookup function (e.g. @\k -> Identity $ Prelude.lookup k@, or @\k -> Identity . Map.lookup k@, or @\f -> doesFileExist f >>= bool (pure Nothing) (readFile f)@)
       -> s -- ^ the structure to search through (e.g. @IO [(String,String)]@ or @Identity (Map String String)@)
       -> i -- ^ identifier to lookup
       -> LookupT m b
lookup toStr p l s i = LookupT $ maybe (Left . S.singleton . Missing $ toStr i) (BiF.first S.singleton . p) <$> l i s

instance Applicative m => Applicative (LookupT m) where
    pure = LookupT . pure . pure
    liftA2 f (LookupT l1) (LookupT l2) = LookupT (liftA2 g l1 l2)
        where
            g (Left set1) (Left set2) = Left (set1 <> set2)
            g (Right _)   (Left set)  = Left set
            g (Left set)  (Right _)   = Left set
            g (Right x)   (Right y)   = Right (f x y)

instance Applicative m => Alternative (LookupT m) where
    empty = LookupT $ pure (Left mempty)
    LookupT f <|> LookupT g = LookupT $ liftA2 (nonZero (Proxy :: Proxy 'True)) f g

instance Monad m => Monad (LookupT m) where
    LookupT l >>= f = LookupT $ do
        eiA <- l
        case eiA of
            Left  s -> pure $ Left s
            Right x -> runLookupT $ f x

instance MonadIO m => MonadIO (LookupT m) where
    liftIO = LookupT . liftIO . fmap pure

instance MonadTrans LookupT where
    lift = LookupT . fmap pure

-- | Prelude's 'P.lookup' function lifted into 'lookup'
llookup :: (Applicative m, Eq i)
        => String -- ^ human-readable name/descriptor of variable we're looking-up (used for error output if not found)
        -> (a -> Either LookupF b) -- ^ parsing function
        -> [(i, a)]
        -> i
        -> LookupT m b
llookup errName parseFn = lookup (const errName) parseFn (pure <% P.lookup)

-- | 'lookup' based around 'findM'
--
-- === Example
--
-- Find the first word in a sequence whose 2nd letter is \'o\'; then try to parse its 3rd character to an @Int@:
--
-- @
-- let var = "somevar"
--     lkupFn = runLookup . lookupFind
--         var
--         (pure . liftME (LookupF var "Couldn't parse to int") . readMaybe @Int . pure . (!!2))
--         (pure . (=='o') . (!!1))
--         . words
-- @
--
-- >>> lkupFn "Here are many wo4ds"
-- Right 4
--
-- >>> lkupFn "Here are some wo4ds"
-- Left (fromList [Failed parsing somevar: Couldn't parse to int])
--
-- >>> lkupFn "Here are many things"
-- Left (fromList [Missing: somevar}])
--
-- Note that inserting "some" causes failure, despite "wo4ds" being a valid candidate. To continue searching until parse matches, use 'lookupFindDesperate'.
lookupFind :: (Monad m, Foldable t)
            => String -- ^ variable name to be displayed in error messages
            -> (i -> Either LookupF b) -- ^ parsing function
            -> (i -> m Bool) -- ^ predicate to pass to find
            -> t i -- ^ list to search through
            -> LookupT m b
lookupFind str parse predicate xs = lookup (const str) parse (\_ s -> findM predicate s) xs undefined

-- God only knows why, but using NicLib.foldMapM kept getting the following error in GHCi:
-- "No instance for (Control.Monad.Trans.Error.Error LookupF) arising from a use of ‘foldMapM'"
-- So it's MaybeT to the rescue!?
-- | Like 'lookupFind', but searches through failed-parsing-elements, returning on the first element matching both predicate /and/ successfully parsing, if any such element exists.
--
-- Thus if @lookupFindDesperate@ returns a @LookupF@, it must be of the @Missing@ constructor. Be careful when using this in your logic, as saying that "\'no satisfactory & parsable object could be found\' means the same as \'undefined\'" can be misleading.
--
-- Continuing where the example from 'lookupFind' left-off, if, in the definition of @lkupFn@, we replace @lookupFind@ with @lookupFindDesperate@,
--
-- >>> lkupFn "Here are some wo4ds"
-- Right 4
lookupFindDesperate :: (Monad m, Foldable t)
                    => String -- ^ variable name to be displayed in error messages
                    -> (i -> m (Either LookupF b)) -- ^ parsing function
                    -> (i -> m Bool) -- ^ predicate to pass to 'findM'
                    -> t i -- ^ list to search through
                    -> LookupT m b
lookupFindDesperate str parse predicate = LookupT . fmap (maybe (Left . S.singleton $ Missing str) Right) . runMaybeT . getAlt
    . foldMap (\i -> Alt $ lift (predicate i) >>= MaybeT . bool (pure Nothing) (either (const Nothing) Just <$> parse i))

-- | Produce a @LookupT@ from a @Maybe@
lookupMaybe :: Monad m
            => String -- ^ variable name, for collecting into error message
            -> (i -> m (Either LookupF b)) -- ^ parsing function. As always, use @pure . pure@ to mootly lift it
            -> m (Maybe i)
            -> LookupT m b
lookupMaybe nomos p = LookupT . (>>= maybe (pure . Left . S.singleton $ Missing nomos) (fmap (BiF.first S.singleton) . p))

-- | 'System.Environment.lookupEnv' lifted into @LookupT IO@. Convenience function: 'lookupMaybe' wraps 'System.Environment.lookupEnv'.
lookupEnv :: String -> LookupT IO String
lookupEnv i = lookupMaybe i (pure . pure) (Env.lookupEnv i)

-- | Lets you use /e.g./ Text's or Bytestring's lazy or strict @readFile@ functions: /e.g./ @lookupFileCommon BSL.readFile@
lookupFileCommon :: (String -> IO b) -> String -> LookupT IO b
lookupFileCommon f = lookup id (pure) (\x _ -> doesFileExist x >>= bool (pure Nothing) (pure <$> f x)) undefined

lookupFile :: FilePath -> LookupT IO String
lookupFile = lookupFileCommon readFile

-- functions copied from NicLib.NStdLib, to avoid needing NicLib as a dependency

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldM (\acc -> fmap (mappend acc) . f) mempty

findM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = liftM getAlt . foldMapM (\x -> Alt . bool Nothing (Just x) <$> p x)

-- | Lift a Maybe into Either
liftME :: l -> Maybe a -> Either l a
liftME l Nothing = Left l
liftME _ (Just x) = Right x

infixl 2 <%
(<%) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
u <% b = curry (u . uncurry b)

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
    [(x,[])] -> Just x
    _ -> Nothing
