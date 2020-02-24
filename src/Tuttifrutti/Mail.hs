module Tuttifrutti.Mail
  ( -- * Configuration
    Config (..)
    -- * Service handle
    , Handle
      -- ** Create basic handles
    , newMailHandle
  ) where

import           Tuttifrutti.Prelude
import qualified Data.Text.Lazy      as Lazy
import           Data.Text           as Text
import           Network.Mail.Mime
import qualified Network.Mail.SMTP   as Mail

data Config = Config
  { configEmailServer     :: Text
    -- ^ name of the email server
  , configEmailServerPort :: Int
    -- ^ port number of the email server
  , configEmailUser       :: Text
    -- ^ email address of the user
  , configEmailPassword   :: Text
    -- ^ password for the user
  }

data Handle = Handle
  { handleSendEmail :: Email -> IO ()
    -- ^ send an email
  }

data Email = Email { emailFrom    :: Text
                   , emailTo      :: [Text]
                   , emailCc      :: [Text]
                   , emailBcc     :: [Text]
                   , emailSubject :: Text
                   , emailText    :: Lazy.Text
                   }

-- | Log in to a mail server, send an email, logout.
send :: Config -> Email -> IO ()
send Config{..} Email{..}  =
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
      (Text.unpack configEmailServer)
      (fromIntegral configEmailServerPort)
      (Text.unpack configEmailUser)
      (Text.unpack configEmailPassword)
      mail'


newMailHandle :: Config -> IO Handle
newMailHandle config = do
  let handleSendEmail = send config
  pure Handle{..}
