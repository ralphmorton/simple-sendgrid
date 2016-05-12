{-# LANGUAGE TemplateHaskell #-}

module Network.SendGrid.Types where

import Control.Lens.TH

data SendGrid = SendGrid {
    _sendGridApiKey :: String,
    _sendGridApiSecret :: String
}

data Mail = Mail {
    _mailFrom :: String,
    _mailTo :: [String],
    _mailSubject :: String,
    _mailContent :: MailContent
}

data MailContent = MailContent {
    _mailContentHTML :: Maybe String,
    _mailContentText :: String
}

makeClassy ''SendGrid
makeLenses ''Mail
makeLenses ''MailContent
