{-# LANGUAGE TemplateHaskell #-}

module Network.SendGrid.Types where

import Control.Lens.TH
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Writer (MonadWriter)
import Data.ByteString.Lazy.Char8 (ByteString)

data SendGrid = SendGrid {
    _sendGridApiKey :: String,
    _sendGridApiSecret :: String
}

data Mail = Mail {
    _mailFrom :: MailRecipient,
    _mailTo :: [MailRecipient],
    _mailCC :: [MailRecipient],
    _mailBCC :: [MailRecipient],
    _mailSubject :: String,
    _mailContent :: MailContent,
    _mailAttachments :: [(String, ByteString)]
}

data MailRecipient = MailRecipient {
    _mailRecipientName :: Maybe String,
    _mailRecipientAddress :: String
}

data MailContent = MailContent {
    _mailContentHTML :: Maybe ByteString,
    _mailContentText :: ByteString
}

makeClassy ''SendGrid
makeLenses ''Mail
makeLenses ''MailRecipient
makeLenses ''MailContent
