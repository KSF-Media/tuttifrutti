module Tuttifrutti.Mail where

import           Tuttifrutti.Prelude
import qualified Data.Text.Lazy      as Lazy
import           Network.Mail.Mime
import qualified Network.Mail.SMTP   as Mail

data Config = Config
  { configEmailServer     :: String
    -- ^ name of the email server
  , configEmailServerPort :: Int
    -- ^ port number of the email server
  , configEmailUser       :: String
    -- ^ email address of the user
  , configEmailPassword   :: String
    -- ^ password for the user
  }

data Email = Email { emailFrom    :: Text
                   , emailTo      :: [Text]
                   , emailCc      :: [Text]
                   , emailBcc     :: [Text]
                   , emailSubject :: Text
                   , emailText    :: Lazy.Text
                   }

-- | Log in to a mail server, send an email, logout.
send :: Email -> Config -> IO ()
send Email{..} Config{..} =
  let to'   = Address Nothing <$> emailTo
      cc'   = Address Nothing <$> emailCc
      bcc'  = Address Nothing <$> emailBcc
      mail' = Mail.simpleMail
               (Address Nothing emailFrom)
               to'
               cc'
               bcc'
               emailSubject
               [htmlPart emailText]

  in
    Mail.sendMailWithLogin'
      configEmailServer
      (fromIntegral configEmailServerPort)
      configEmailUser
      configEmailPassword
      mail'
