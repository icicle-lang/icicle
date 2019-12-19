{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Storage.Dictionary.Toml.TomlDictionary (
    DictionaryConfig(..)
  , DictionaryInput'(..)
  , DictionaryOutput'(..)
  , ConcreteKey'(..)
  , DictionaryValidationError(..)
  , tomlDict
  ) where

import           P

import           Control.Lens

import qualified Control.Applicative as CA ((<|>))
import           Data.Text
import           Data.Validation hiding (validate)
import qualified Data.HashMap.Strict as M

import           Data.Attoparsec.Text
import qualified Text.Parsec.Pos as Pos
import           Text.Parsec (runParser)
import           Text.Parsec.Error

import           Icicle.Data
import           Icicle.Source.Lexer.Token
import           Icicle.Source.Lexer.Lexer
import           Icicle.Source.Parser.Parser
import qualified Icicle.Source.Parser.Parser as Source
import           Icicle.Source.Query

import           Icicle.Storage.Encoding

import           Icicle.Storage.Dictionary.Toml.Prisms
import           Icicle.Storage.Dictionary.Toml.Types

import           Icicle.Internal.Pretty hiding (char)


-- | Dictionary config can be inherited from higher level dictionaries, items such as
--   Namespace and tombstone can be scoped based on where they are defined, and overridden
--   inside a specific fact or feature.
data DictionaryConfig =
  DictionaryConfig {
    configTitle     :: Maybe Text
  , configVersion   :: Maybe Int64
  , configNamespace :: Maybe Namespace
  , configTombstone :: Maybe Text
  , configImports   :: [Text]
  , configChapter   :: [Text]
  } deriving (Eq, Show)

instance Semigroup DictionaryConfig where
  -- Left preferenced Semigroup instance.
  -- Use properties specified in this file, or, if they don't exist, try the parent.
  -- Don't bring in the imports or chapters, as that will cause an infinite loop.
  (<>)
    (DictionaryConfig a1 a2 a3 a4 a6 a7)
    (DictionaryConfig b1 b2 b3 b4 _  _ ) =
      (DictionaryConfig (a1 CA.<|> b1) (a2 CA.<|> b2) (a3 CA.<|> b3) (a4 CA.<|> b4) a6 a7)

instance Monoid DictionaryConfig where
  mempty  = DictionaryConfig Nothing Nothing Nothing Nothing [] []
  mappend = (<>)

data DictionaryInput' =
  DictionaryInput' {
      inputId' :: InputId
    , inputEncoding' :: Encoding
    , inputTombstone' :: Maybe Text
    , inputKey' :: ConcreteKey'
    } deriving (Eq, Show)

data DictionaryOutput' =
  DictionaryOutput' {
      outputId' :: OutputId
    , outputQuery' :: QueryTop Pos.SourcePos Variable
    } deriving (Eq, Show)

newtype ConcreteKey' = ConcreteKey' {
    concreteKey :: Maybe (Exp Pos.SourcePos Variable)
  } deriving (Eq, Show)


data DictionaryValidationError =
  UnknownElement Text Pos.SourcePos
  | BadType Text Text Pos.SourcePos
  | MissingRequired Text Text
  | EncodingError Text Text Pos.SourcePos
  | ParseError ParseError
  | InvalidName Text
  | InvalidNamespace Text
  deriving (Eq, Show)

instance Pretty DictionaryValidationError where
 pretty e
  = case e of
     UnknownElement n p
      -> vsep [ "Unknown element in dictionary:"
              , "  element : " <> pretty n
              , "  at      : " <> pretty p
              ]
     BadType n t p
      -> vsep [ "Dictionary entry has the wrong type:"
              , " entry     : " <> pretty n
              , " at        : " <> pretty p
              , " exptected : " <> pretty t
              ]
     MissingRequired n ex
      -> vsep [ "Dictionary entry is missing required field:"
              ,   "entry   : " <> pretty n
              ,   "missing :"  <> pretty ex
              ]
     EncodingError f n p
      -> vsep [ "Fact has a bad feature encoding:"
              , "  fact     : " <> pretty f
              , "  encoding : " <> pretty n
              , "  at       : " <> pretty p
              ]
     ParseError p
      -> vsep [ "Error parsing feature expression:"
              , indent 2 $ (text . show) p
              ]

     InvalidName n
      -> vsep [ "Invalid name format (must be an Ivory feature name):"
              ,  indent 2 . text . unpack $ n ]

     InvalidNamespace n
      -> vsep [ "Invalid namespace (must be an Ivory namespace):"
              ,  indent 2 . text . unpack $ n ]

type Name = Text

--------------------------------------------------------------------------------

tomlDict
  :: DictionaryConfig
  -> Table
  -> Validation [DictionaryValidationError] (DictionaryConfig, [DictionaryInput'], [DictionaryOutput'])
tomlDict parentConf x = fromEither $ do
  n <- toEither . textFocus "namespace" $ x
  nsp <- case n of
           Nothing ->
             pure Nothing
           Just a ->
             fmap Just . maybeToRight [InvalidNamespace a] . parseNamespace $ a
  let config =   DictionaryConfig
             <$> textFocus "title" x
             <*> intFocus  "version" x
             <*> pure nsp
             <*> textFocus "tombstone" x
             <*> textArrayFocus "import" x
             <*> textArrayFocus "chapter" x

  -- We need to treat it as a monad as facts require the config to know the default namespace.
  config' <- toEither config

  -- Join the config with its parent, so we are scoped correctly
  let config'' = config' <> parentConf

  -- Parse the facts, again, getting the Monad version at the ends.
  facts    <-  toEither
            $  maybe [] id
           <$> traverse (validateTableWith validateFact "fact" config'')
                        (x ^? key "fact")

  -- Parse features (without typechecking).
  features <-  toEither
            $  maybe [] id
           <$> traverse (validateTableWith validateFeature "feature" config'')
                        (x ^? key "feature")

  -- Todo: ensure that there's no extra data lying around. All valid TOML should be used.
  pure (config'', facts, features)


-- | If a namespace is given, it must be validated. If not, it must have a parent value.
validateNamespace
  :: DictionaryConfig
  -> Text
  -> Table
  -> Validation [DictionaryValidationError] Namespace
validateNamespace parent name x =
  let
    k =
      "namespace"

    valParent =
      maybeToRight [MissingRequired ("fact." <> name) k] (configNamespace parent)

  in maybe
       (either Failure Success valParent)
       (andThen parseNamespace [InvalidNamespace name] . validateText k)
       (x ^? key k)

-- | Validate a TOML node is a fact.
--
validateFact
  :: DictionaryConfig
  -> Name
  -> Table
  -> Validation [DictionaryValidationError] DictionaryInput'
validateFact conf name x =
  let fname
        = "fact." <> name

      -- Every fact needs an encoding, which can't be inherited from it's parent.
      encoding
        = maybe (Failure [MissingRequired fname "encoding"])
                (validateEncoding' fname)
                (M.lookup "encoding" x)

      -- If a namespace is given, it must be validated. If not, it must have a parent value.
      namespace'
        = validateNamespace conf name x

      -- Tombstones are not mandatory, but can be inherited.
      tombstone'
        =   (<|> configTombstone conf)
        <$> (validateText "tombstone") `traverse` (x ^? key "tombstone")

      -- Refutation key can be any expression.
      key'
        =   ConcreteKey'
        <$> ((<|> Nothing)
        <$> (validateExpression (fname <> ".key")) `traverse` (x ^? key "key"))

      attribute
        = maybe (Failure [InvalidName name]) pure (parseInputName name)

      -- Todo: ensure that there's no extra data lying around. All valid TOML should be used.
  in DictionaryInput'
       <$> (InputId <$> namespace' <*> attribute)
       <*> encoding
       <*> tombstone'
       <*> key'


validateExpression :: Text
                   -> (Node, SourcePos)
                   -> Validation [DictionaryValidationError] (Exp SourcePos Variable)
validateExpression fname expression = fromEither $ do
   expression' <- maybeToRight [BadType fname "string" (expression ^. _2)]
                $ expression ^? _1 . _NTValue . _VString

   let toks     = lexerPositions expression'
   e           <- first (pure . ParseError)
                $ runParser Source.exp () "" toks

   pure e


-- | Validate a TOML node is a feature.
--   e.g.
--     [feature.mean_age]
--        expression = "feature person ~> mean age"
--
validateFeature
  :: DictionaryConfig
  -> Name
  -> Table
  -> Validation [DictionaryValidationError] DictionaryOutput'
validateFeature conf name x = fromEither $ do
  let fname     = "feature." <> name
      fexp      = fname <> ".expression"

  -- If a namespace is specified, validate it. Otherwise inherit the parent value.
  nsp         <- toEither $ validateNamespace conf name x


  -- A feature must specify an expression
  expression  <- maybeToRight [MissingRequired fname "expression"]
               $ x ^? key "expression"

  -- The expression must be a string
  expression' <- maybeToRight [BadType fexp "string" (expression ^. _2)]
               $ expression ^? _1 . _NTValue . _VString

  attribute   <- maybeToRight [InvalidName name] (parseOutputName name)
  let oid      = OutputId nsp attribute

  -- Run the icicle expression lexer
  let toks     = lexerPositions expression'

  -- And the icicle parser
  q           <- first (pure . ParseError)
               $ runParser (top oid) () "" toks

  -- Todo: ensure that there's no extra data lying around. All valid TOML should be used.
  pure $ DictionaryOutput' oid q

-- | Validate a TOML node is a fact encoding.
--
validateEncoding'
  :: Text
  -> (Node, Pos.SourcePos)
  -> Validation [DictionaryValidationError] Encoding

-- We can accept an encoding as a string in the old form.
-- e.g. "(location:string,severity:int)"
--
validateEncoding' ofFeature (NTValue (VString encs), pos) =
  let encodingString = pack $ fst <$> encs
  in either
       (Failure . const [EncodingError ofFeature encodingString pos])
        Success
       $ parseOnly parseEncoding encodingString

-- Or as a table with string fields.
--
-- e.g. [fact.encoding]
--         location="string"
--         severity="int"
--
validateEncoding' ofFeature (NTable t, _) =
  let validated name (enc, pos')
        = either Failure Success
        $ do -- Using a monad instance here, as the encoding should be a string.
             enc' <- maybe (Left [BadType name "string" pos'])
                           (Right . fmap fst)
                           (enc ^? _NTValue . _VString)
             -- Now that we have a string, parse it with attoparsec
             (enc'', fieldType) <-  first (const [EncodingError ofFeature (pack enc') pos'])
                                 $  parseOnly
                                      ((,) <$> parseEncoding
                                           <*> (Optional <$ char '*' <|> pure Mandatory)
                                           <*  endOfInput)
                                      (pack enc')

             pure $ StructField fieldType name enc''
  -- We should get an error for every failed encoding listed.
  in StructEncoding . toList <$> M.traverseWithKey validated t

-- But all other values should be failures.
validateEncoding' ofFeature (_, pos) =
  Failure $ [BadType (ofFeature <> ".encoding") "string" pos]


--------------------------------------------------------------------------------

-- | Validate a table, using a validator for each element.
--
validateTableWith
  :: (   DictionaryConfig
      -> Text
      -> Table
      -> Validation [DictionaryValidationError] a )
  -> Text
  -> DictionaryConfig
  -> (Node, Pos.SourcePos)
  -> Validation [DictionaryValidationError] [a]
validateTableWith validator _ conf (NTable t, _) =
  let validate name (fact', pos')
        = fromEither $ do
            -- Using a monad instance here, as the fact should be a table.
            t'' <- maybeToRight ([BadType name "table" pos']) (fact' ^? _NTable)
            -- Validate the table with the given config and function.
            toEither $ (validator conf) name t''

  -- We will get an error for every failed item listed.
  in toList <$> M.traverseWithKey validate t

validateTableWith _ n _ (_, pos) = Failure $ [BadType n "table" pos]


-- | Validate a TOML node is a string.
--
validateText
  :: Text
  -> (Node, Pos.SourcePos)
  -> Validation [DictionaryValidationError] Text
validateText ttt x
  = maybe (Failure [BadType ttt "string" (x ^. _2)])
          (Success . pack . fmap fst)
          (x ^? _1 . _NTValue . _VString)


-- | Validate a TOML node is an string.
--
validateInt
  :: Text
  -> (Node, Pos.SourcePos)
  -> Validation [DictionaryValidationError] Int64
validateInt _ (NTValue (VInteger i), _) = Success i
validateInt t (_, pos)                  = Failure $ [BadType t "int" pos]


-- | Validate a TOML node is an array of strings.
--
validateTextArray
  :: Text
  -> (Node, Pos.SourcePos)
  -> Validation [DictionaryValidationError] [Text]

validateTextArray t (NTValue (VArray xs), pos) =
  let validateString (VString x) = Right $ pack $ fst <$> x
      validateString _           = Left $ [BadType t "string" pos]
  in  either Failure Success $ validateString `traverse` xs

validateTextArray t (_, pos) =
  Failure $ [BadType t "array" pos]


textArrayFocus :: Text -> Table -> Validation [DictionaryValidationError] [Text]
textArrayFocus label x'
  = maybe [] id <$> traverse (validateTextArray label) (x' ^? key label)

textFocus :: Text -> Table -> Validation [DictionaryValidationError] (Maybe Text)
textFocus label x'
  = validateText label `traverse` (x' ^? key label)

intFocus :: Text -> Table -> Validation [DictionaryValidationError] (Maybe Int64)
intFocus label x'
  = validateInt label `traverse` (x' ^? key label)

--------------------------------------------------------------------------------

andThen :: (b -> Maybe c) -> a -> Validation a b -> Validation a c
andThen f e v =
  case toEither v of
    Left t ->
      Failure t
    Right x ->
      maybe (Failure e) Success (f x)
