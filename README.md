note: this module may be deprecated in favor of [validation arrows](https://github.com/mavenraven/validations). I have yet to fully compare the two, but at first glance it seems likely.

---

# ```LookupT```

*Please see the haddocks (link to hopefully soon be posted) for documentation. This document is merely a WIP summary of the haddocks.*

## It's like ```Validation``` but not Quite

```LookupT``` is the [```Validation```](http://hackage.haskell.org/package/validation) applicative functor plus a monad instance based about looking-up an object in a structure LookupT was, however, not written with *validation* in mind, per se: LookupT is about *parsing*. LookupT was motivated by parsing a user-defined ```Config``` object in terms of ```Read```able values from JSON/YAML config files, or the environment &ndash; basically something like

    data Config = Config { name :: Text, age :: Natural }

    readConfig :: LookupT IO Config
    readConfig = Config <$> lookupEnv "NAME" <*> (readMaybe @Int) (lookupEnv "AGE")

For non-hierarchical structures like a map/lookup table, ```LookupT``` is just about well as ```Validation```. Notably, however, ```Validation``` is a binary type, whereas LookupT is unary; becasue of its lookup-then-parse nature, LookupT's error collection type is definitely ```Set LookupF```, where ```LookupF``` is an error type describing missing or improperly-formatted objects.

## Monadic Usage

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

You may have noticed the use of ```ExceptT . runLookupT```. This is common, since ```LookupT``` is isoromphic with ```ExceptT (Set LookupF)```. Be careful to notice that *reading the config file* has nothing to do with either ```ExceptT``` nor ```LookupT```. ```ExceptT``` is used to interpret the parsing of the config file as YAML, and to more standardly represent the returned ```LookupT```. ```LookupT``` is used only in ```getInObj``` and is the target category for lifting the ```Config``` constructor.

### Notes to Newbies (about monads, ```ExceptT```, and ```IO```)

Yeah, it's dumb, but I gotta say it, 'cause it *can* be confusing for newcomers:

* ```ExceptT``` has nothing to do with exceptions; it should've been named "EitherT"
* Monads have nothing in particular to do with IO
* ```LookupT``` has nothing in particular to do with IO
* ```readFile``` may throw an IO exception. I assume that ```loadConfig``` would be called within ```main```, and ```main``` would use proper exception handling (e.g. ```handle``` in the ```safe-exceptions``` package.)

## TODO's:

* cf. [validations](https://github.com/mavenraven/validations) ```Category```
