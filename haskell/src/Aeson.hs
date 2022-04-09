module Aeson where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Map.Strict

{-
encoding/decoding are a 2 steps process:
  1. encode data type to value
  2. encode value to bytestring

  1. decode bytestring to value
  2. decode value to datatype

That's why it's inefficient to use the toJSON/fromJSON to construct a value.
Instead use:
  - `encode :: ToJSON a => a -> ByteString`
  - `decode :: FromJSON a => ByteString -> Maybe a`
which combine both steps and is more efficient memory and speed wise

possible JSON value:

data Value
  = Object Object -- Object = HashMap Text Value
  | Array Array -- Array = Vector Value
  | String Text
  | Number Scientific
  | Bool Bool
  | Null

-}


-- * automatic derivation 1


data User01 = User01
  { firstname :: String
  , lastname :: String
  } deriving stock (Show, Generic)

{- |
>>> encode $ User01 { firstname = "John", lastname = "Doe" }
"{\"lastname\":\"Doe\",\"firstname\":\"John\"}"

>>> decode "{\"firstname\":\"John\", \"lastname\":\"Doe\"}" :: Maybe User01
Just (User01 {firstname = "John", lastname = "Doe"})
-}
instance ToJSON User01
instance FromJSON User01


-- * automatic derivation 2


-- ** record

data User02 = User02
  { firstname :: String
  , lastname :: String
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)


-- ** sum type


data UserType1 = Teacher1 | Admin1
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

{- |
>>> encode Teacher1
"\"Teacher1\""

>>> decode "\"Teacher1\"" :: Maybe UserType1
Just Teacher1
-}


-- * manual implementation


-- ** record


data User03 = User03
  { firstname :: String
  , lastname :: String
  , description :: Maybe String
  } deriving stock (Show, Generic)

{- |
>>> encode $ User02 { firstname = "John", lastname = "Doe" }
"{\"lastname\":\"Doe\",\"firstname\":\"John\"}"
-}
instance FromJSON User03 where
  parseJSON :: Value -> Parser User03
  parseJSON = withObject "User02" $ \v -> User03
    <$> v .: "firstname"
    <*> v .: "lastname"
    <*> v .:? "description"

{- |
>>> decode "{\"firstname\":\"John\", \"lastname\":\"Doe\", \"description\":null}" :: Maybe User03
Just (User03 {firstname = "John", lastname = "Doe", description = Nothing})
-}
instance ToJSON User03 where
  toJSON :: User03 -> Value
  toJSON User03 {..} =
    object [ "firstname" .= firstname
           , "lastname" .= lastname
           , "description" .= description
           ]


-- ** sum types


data UserType2 = Teacher2 | Admin2 | Student2
  deriving (Generic, Show)

{- |
>>> decode "\"Teacher2\"" :: Maybe UserType2
Just Teacher2
-}
instance FromJSON UserType2 where
  parseJSON = withText "UserType2" $ \t -> do
    case t of
      "Teacher2" -> pure Teacher2
      "Admin2"   -> pure Admin2
      _          -> fail "unknown user type"

{- |
>>> encode Teacher2
"{\"type\":\"teacher2\",\"description\":\"teacher can teach lessons\"}"
-}
instance ToJSON UserType2 where
  toJSON :: UserType2 -> Value
  toJSON = \case
    Teacher2 -> object [ "type" .= ("teacher2" :: String)
                       , "description" .= ("teacher can teach lessons" :: String)
                       ]
    Student2 -> object [ "type" .= ("student2" :: String) ]
    Admin2 -> object []


-- * dynamic decoding/encoding


-- ** decoding


{- |

json with same value types
>>> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
Just (fromList [("bar",2),("foo",1)])

json with dynamic object
>>> decode "{\"name\":\"Dave\",\"age\":2}" :: Maybe Object
Just (fromList [("age",Number 2.0),("name",String "Dave")])

parser on the fly
>>> :{
do result <- decode "{\"firstname\":\"John\",\"lastname\":\"Doe\"}"
   flip parseMaybe result $ \obj -> do
     firstname <- obj .: "firstname"
     lastname <- obj .: "lastname"
     pure $ firstname ++ " " ++ lastname
:}
Just "John Doe"

nested field with custom parser
>>> :m + Data.HashMap.Strict Data.Aeson.Types
>>> :{
let json :: Maybe Value
    json = decode @Value "{\"user\": { \"name\": \"John\",\"age\":2} }"
    customNameParser :: Value -> Parser String
    customNameParser = withObject "custom parser" $ \obj -> do
      user <- obj .: "user"
      user .: "name"
    mName :: Maybe String
    mName = parseMaybe customNameParser =<< json
in mName
:}
Just "John"

nested field with known json type (e.g: object)
>>> :{
let json :: Maybe Object
    json = decode @Object "{\"user\": { \"name\": \"John\",\"age\":2} }"
    customNameParser :: Object -> Parser String
    customNameParser obj = do
      user <- obj .: "user"
      user .: "name"
    mName :: Maybe String
    mName = parseMaybe customNameParser =<< json
in mName
:}
Just "John"

-}


-- ** encoding


{- |

object :: [Pair] -> Value -- Pair being a strict tuple (i.e: (a, b))
(.=)   :: ToJSON v => (strict) Text -> v -> Pair

$ setup
>>> :{
let user :: Value
    user = object [ "firstname" .= ("John" :: String)
                  , "lastname" .= ("Doe" :: String)
                  , "nested" .= object [ "thisIs" .= ("nested" :: String) ]
                  ]
in user
:}
Object (fromList [("firstname",String "John"),("lastname",String "Doe"),("nested",Object (fromList [("thisIs",String "nested")]))])

-}
