{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Network.SendGrid.Types where

import Control.Lens.TH
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Database.Persist.Class (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..))
import qualified Data.Map as M

data SendGrid = SendGrid {
    _sendGridApiKey :: String,
    _sendGridApiSecret :: String,
    _sendGridApiKeyV3 :: String
}

data Mail = Mail {
    _mailFrom :: MailRecipient,
    _mailTo :: [MailRecipient],
    _mailCC :: [MailRecipient],
    _mailBCC :: [MailRecipient],
    _mailSubject :: String,
    _mailContent :: MailContent,
    _mailAttachments :: [(String, ByteString)],
    _mailHeaders :: M.Map String String,
    _mailXSMTP :: XSMTP
}

data MailRecipient = MailRecipient {
    _mailRecipientName :: Maybe String,
    _mailRecipientAddress :: String
}

data MailContent = MailContent {
    _mailContentHTML :: Maybe ByteString,
    _mailContentText :: Maybe ByteString
}

data XSMTP = XSMTP {
    _xSMTPUniqueArgs :: Maybe (M.Map String String)
}

instance ToJSON XSMTP where
    toJSON x =
        object [
            "unique_args" .= _xSMTPUniqueArgs x
        ]

newtype ContactListId = ContactListId { unContactListId :: Word }
  deriving (Eq, Show, ToJSON, FromJSON, PersistField, PersistFieldSql)

newtype RecipientId = RecipientId { unRecipientId :: String }
  deriving (Eq, Show, ToJSON, FromJSON, PersistField, PersistFieldSql)

makeClassy ''SendGrid
makeLenses ''Mail
makeLenses ''MailRecipient
makeLenses ''MailContent
makeLenses ''XSMTP
