{-# LANGUAGE TupleSections #-}

module Network.SendGrid(
    module Network.SendGrid.Types,
    send
) where

import Network.SendGrid.Types

import Control.Lens (view)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Writer (MonadWriter)
import Data.Maybe (listToMaybe)
import Network.HTTP.Nano

send :: (MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasSendGrid r, MonadIO m) => Mail -> m ()
send (Mail from to subject content) = do
    apiKey <- view sendGridApiKey
    apiSecret <- view sendGridApiSecret
    let dta = encodeRecips to ++ encodeContent content ++ [("from", from), ("subject", subject), ("api_user", apiKey), ("api_key", apiSecret)]
    req <- buildReq POST "https://api.sendgrid.com/api/mail.send.json" $ UrlEncodedRequestData dta
    http' req

encodeRecips :: [String] -> [(String, String)]
encodeRecips = fmap ("to",)

encodeContent :: MailContent -> [(String, String)]
encodeContent (MailContent html text) = [("text", text)] ++ maybe [] ((:[]) . ("html",)) html
