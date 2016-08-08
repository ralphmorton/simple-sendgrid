{-# LANGUAGE OverloadedStrings #-}
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
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (listToMaybe, maybeToList)
import Data.Monoid
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Nano

send :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasSendGrid r, MonadIO m) => Mail -> m ()
send mail = do
    req <- addRequestData mail =<< buildReq POST "https://api.sendgrid.com/api/mail.send.json" NoRequestData
    http' req

addRequestData :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasSendGrid r, MonadIO m) => Mail -> Request -> m Request
addRequestData mail req = do
    apiKey <- B.pack <$> view sendGridApiKey
    apiSecret <- B.pack <$> view sendGridApiSecret
    flip formDataBody req $ base apiKey apiSecret ++ tos ++ ccs ++ bccs ++ content (mail ^. mailContent) ++ attachments
    where
    (MailRecipient mfromName fromAddr) = mail ^. mailFrom
    base apiKey apiSecret =
        [
            partBS "subject" . B.pack $ mail ^. mailSubject,
            partBS "api_user" apiKey,
            partBS "api_key" apiSecret,
            partBS "from" $ B.pack fromAddr,
            partBS "fromname" . B.pack $ maybe "" id mfromName,
            partBS "headers" . BL.toStrict . encode $ mail ^. mailHeaders,
            partLBS "x-smtpapi" . encode $ mail ^. mailXSMTP
        ]
    tos = concat (encodeRecip "to[]" "toname[]" <$> mail ^. mailTo)
    ccs = concat (encodeRecip "cc[]" "ccname[]" <$> mail ^. mailCC)
    bccs = concat (encodeRecip "bcc[]" "bccname[]" <$> mail ^. mailBCC)
    content (MailContent html text) = [partLBS "text" text] ++ maybe [] ((:[]) . partLBS "html") html
    attachments = uncurry attachmentPart <$> mail ^. mailAttachments

encodeRecip :: T.Text -> T.Text -> MailRecipient -> [Part]
encodeRecip b bn (MailRecipient mname addr) = [partBS b $ B.pack addr] ++ maybe [] ((:[]) . partBS bn . B.pack) mname

attachmentPart :: String -> BL.ByteString -> Part
attachmentPart name = partFileRequestBody (T.pack $ "files["<>name<>"]") name . RequestBodyLBS
