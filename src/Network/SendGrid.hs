{-# LANGUAGE TupleSections #-}

module Network.SendGrid(
    module Network.SendGrid.Types,
    send
) where

import Network.SendGrid.Types

import Control.Lens (view, (^.))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe (listToMaybe, maybeToList)
import Network.HTTP.Nano

send :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasSendGrid r, MonadIO m) => Mail -> m ()
send mail = do
    apiKey <- view sendGridApiKey
    apiSecret <- view sendGridApiSecret
    let dta = [("subject", mail ^. mailSubject), ("api_user", apiKey), ("api_key", apiSecret)] ++ encodeAddresses mail ++ encodeContent (mail ^. mailContent)
    req <- buildReq POST "https://api.sendgrid.com/api/mail.send.json" $ UrlEncodedRequestData dta
    http' req

encodeAddresses :: Mail -> [(String, String)]
encodeAddresses mail =
    concat [
        [("from", mail ^. mailFrom . mailRecipientAddress)] ++ maybeToList (fmap ("fromname", ) $ mail ^. mailFrom . mailRecipientName),
        concat . fmap (encode "to" "toname") $ mail ^. mailTo,
        concat . fmap (encode "cc" "ccname") $ mail ^. mailCC,
        concat . fmap (encode "bcc" "bccname") $ mail ^. mailBCC
    ]
    where encode afld nfld (MailRecipient mname addr) = [(afld ++ "[]", addr)] ++ maybeToList (fmap (nfld ++ "[]",) mname)

encodeContent :: MailContent -> [(String, String)]
encodeContent (MailContent html text) = [("text", text)] ++ maybe [] ((:[]) . ("html",)) html
