{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields, OverloadedStrings #-}

module Tuttifrutti.Mailchimp.Transactional where

import           Tuttifrutti.Prelude
import           Tuttifrutti.Servant

import           Data.Aeson
import           Data.Char (isUpper, toLower)
import qualified Data.Has                        as Has
import           Network.HTTP.Client             (newManager)
import           Network.HTTP.Client.TLS         (tlsManagerSettings)
import           Servant.Client

type MailchimpTransactionalApi =
  SendTemplateEndpoint

type SendTemplateEndpoint =
  Summary "Send email using HTML template"
    :> "api"
    :> "1.0"
    :> "messages"
    :> "send-template"
    :> ReqBody '[JSON] SendTemplate
    :> Post '[JSON] [SendStatus]

api :: Proxy MailchimpTransactionalApi
api = Proxy

sendTemplateEndpoint :: SendTemplate -> ClientM [SendStatus]
sendTemplateEndpoint = client api

data SendTemplate = SendTemplate
  { key :: Text
  , templateName :: Text
  , templateContent :: [TemplateContent]
  , message :: Message
  } deriving ( Generic, Data, Show )

instance ToJSON SendTemplate where
  toJSON     = genericToJSON mailchimpOptions
  toEncoding = genericToEncoding mailchimpOptions

data TemplateContent = TemplateContent
  { name :: Text
  , content :: Text
  } deriving ( Generic, Data, ToJSON, Show )

data Message = Message
  { subject :: Text
  , fromEmail :: Text
  , fromName :: Maybe Text
  , to :: [Recipient]
  , headers :: Maybe Object
  , mergeLanguage :: Maybe Text
  , globalMergeVars :: Maybe [MergeVar]
  , mergeVars :: Maybe [RecipientMergeVars]
  , tags :: [Text]
  , images :: [Image]
  } deriving ( Generic, Data, Show )

instance ToJSON Message where
  toJSON     = genericToJSON mailchimpOptions
  toEncoding = genericToEncoding mailchimpOptions

emptyMessage :: Message
emptyMessage = Message
  { subject = ""
  , fromEmail = ""
  , fromName = Nothing
  , to = []
  , headers = Nothing
  , mergeLanguage = Just "handlebars"
  , globalMergeVars = Nothing
  , mergeVars = Nothing
  , tags = []
  , images = []
  }

data Recipient = Recipient
  { email :: Text
  , name :: Maybe Text
  , type_ :: Maybe Text
  } deriving ( Generic, Data, Show )

instance ToJSON Recipient where
  toJSON     = genericToJSON mailchimpOptions
  toEncoding = genericToEncoding mailchimpOptions

data MergeVar = MergeVar
  { name :: Text
  , content :: Value
  } deriving ( Generic, Data, ToJSON, Show )

data RecipientMergeVars = RecipientMergeVars
  { rcpt :: Text
  , vars :: [MergeVar]
  } deriving ( Generic, Data, ToJSON, Show )

data Image = Image
  { type_ :: Text
  , name :: Text
  , content :: Text
  } deriving ( Generic, Data, Show )

instance ToJSON Image where
  toJSON     = genericToJSON mailchimpOptions
  toEncoding = genericToEncoding mailchimpOptions

mailchimpOptions :: Options
mailchimpOptions = defaultOptions
  { fieldLabelModifier = reverse . dropWhile (== '_') . reverse . concatMap snakeCase }
  where snakeCase x | isUpper x = ['_', toLower x]
                    | otherwise = pure x

data SendStatus = SendStatus
  { email :: Text
  , status :: Text
  , rejectReason :: Maybe Text
  , _id :: Text
  } deriving ( Generic, Data, Show )

instance FromJSON SendStatus where
  parseJSON = genericParseJSON mailchimpOptions

data Config = Config
  { configUrl :: String
  , configKey :: Text
  }

data Handle = Handle
  { unHandle :: (ClientEnv, Config)
  }

newHandle :: (MonadIO m, MonadThrow m) => Config -> m Handle
newHandle config = do
  manager <- liftIO $ newManager tlsManagerSettings
  url <- parseBaseUrl (configUrl config)
  pure $ Handle ( mkClientEnv manager url
                , config
                )

sendTemplateMessage :: (MonadReader env m, Has Handle env, MonadIO m) => SendTemplate -> m (Either ClientError [SendStatus])
sendTemplateMessage msg = do
  (clientEnv, Config{..}) <- asks (unHandle . Has.getter)
  let msg' = msg { key = configKey }
  liftIO (runClientM (sendTemplateEndpoint msg') clientEnv)
