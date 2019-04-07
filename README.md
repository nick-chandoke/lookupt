Please see the haddocks (link to hopefully soon be posted) for documentation. This document is merely a WIP summary of the haddocks.

```LookupT``` is the [```Validation```](http://hackage.haskell.org/package/validation) applicative functor plus a monad instance based about looking-up an object in a structure LookupT was, however, not written with *validation* in mind, per se: LookupT is about *parsing*. LookupT was motivated by parsing a user-defined ```Config``` object in terms of ```Read```able values from JSON/YAML config files, or the environment &ndash; basically something like

    data Config = Config { name :: Text, age :: Natural }

    readConfig :: LookupT IO Config
    readConfig = Config <$> lookupEnv "NAME" <*> (readMaybe @Int) (lookupEnv "AGE")

For non-hierarchical structures like a map/lookup table, ```LookupT``` is just about well as ```Validation```. Notably, however, ```Validation``` is a binary type, whereas LookupT is unary; becasue of its lookup-then-parse nature, LookupT's error collection type is definitely ```Set LookupF```, where ```LookupF``` is an error type describing missing or improperly-formatted objects.

For hierarchical objects, LookupT acts like a combination of ```Either``` and ```Validation``` (example duplicated from Haddocks):

    import Data.Set (Set)
    import qualified Data.Set as S
    import qualified Data.Bifunctor as BiF
    import Control.Monad.Trans.Except -- transformers package, not mtl
    import qualified Data.ByteString.Char8 as BS'
    import Data.Yaml
    
    loadConfig :: ExceptT (Set LookupF) IO Config
    loadConfig = do
        raw <- liftIO $ BS'.readFile "config.yaml"
        yaml <- ExceptT . pure . BiF.first (S.singleton . Improper "config" . show) $ (decodeEither' raw :: Either ParseException Object)
        let getInObj :: Applicative m => (Value -> Either LookupF b) -> T'.Text -> Object -> LookupT m b
            getInObj p k o = lookup T'.unpack p (pure <% HM.lookup) yaml k
        ExceptT . runLookupT $ do
            root <- getInObj (\case Object o -> Right o; _ -> Left $ Improper "config" "not an object") "config" yaml
            Config
            <$> getInObj (\case String t -> Right t) "domain" root
            <*> getInObj (\case Number n -> Right $ truncate n) "port" root

which returns an example web server config object, from reading-in ```config.yaml```:

    config:
      domain: example.com
      port: 80
