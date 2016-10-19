{-# LANGUAGE OverloadedStrings #-}

module Network.SendGrid.Contacts
  ( createContactList
  , deleteContactList
  , insertRecipients
  , createRecipients )
where

import Control.Concurrent (threadDelay)
import Control.Lens (view, (^?), to)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Network.HTTP.Client hiding (method)
import Network.HTTP.Nano
import Network.SendGrid.Types
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as B8
import qualified Data.CaseInsensitive   as CI

-- | Create a new contact list with given name.

createContactList
  :: ( MonadError e m
     , MonadReader r m
     , AsHttpError e
     , HasHttpCfg r
     , HasSendGrid r
     , MonadIO m )
  => String
     -- ^ Name of the contact list
  -> m (Maybe ContactListId)
     -- ^ 'Nothing' when we cannot extract ID from response
createContactList name = do
  let val = object ["name" .= name]
  r <- httpJSON =<< sendGridReq POST "v3/contactdb/lists/" (mkJSONData val)
  return ((r :: Value) ^? (key "id" . _Integer . to (ContactListId . fromIntegral)))

-- | Delete a contract list.

deleteContactList
  :: ( MonadError e m
     , MonadReader r m
     , AsHttpError e
     , HasHttpCfg r
     , HasSendGrid r
     , MonadIO m )
  => Bool              -- ^ Should we delete all recipients as well?
  -> ContactListId     -- ^ Contract list to delete
  -> m ()
deleteContactList deleteContacts cid = do
  let val = object ["delete_contacts" .= deleteContacts]
      url = "v3/contactdb/lists/" ++ show (unContactListId cid)
  http' =<< sendGridReq DELETE url (mkJSONData val)

-- | Insert recipients into a contact list.

insertRecipients
  :: ( MonadError e m
     , MonadReader r m
     , AsHttpError e
     , HasHttpCfg r
     , HasSendGrid r
     , MonadIO m )
  => [RecipientId]     -- ^ Collection of recipients to insert
  -> ContactListId     -- ^ Target list (where to insert)
  -> m ()
insertRecipients rids cid = do
  let url = "v3/contactdb/lists/" ++ show (unContactListId cid) ++ "/recipients"
  http' =<< sendGridReq POST url (mkJSONData rids)

-- | Create a bunch of recipients. This makes a call per 1000 recipients and
-- waits 1 second between calls to make sure we don't ever exceed SendGrid's
-- limits.
--
-- If the request tries to create duplicate recipients nothing happens.

createRecipients
  :: ( MonadError e m
     , MonadReader r m
     , AsHttpError e
     , HasHttpCfg r
     , HasSendGrid r
     , MonadIO m )
  => [String]          -- ^ Collection of emails to insert
  -> m [RecipientId]   -- ^ Bunch of 'RecipientId's
createRecipients emails = do
  let (now,later) = splitAt 1000 emails
      val = object . pure . ("email" .=) <$> now
      url = "v3/contactdb/recipients"
  http' =<< sendGridReq POST url (mkJSONData val)
  unless (null later) . void $ do
    liftIO (threadDelay 1000000)
    createRecipients later
  return (recipientIdFromEmail <$> emails)

----------------------------------------------------------------------------
-- Helpers

-- | A helper for request construction.

sendGridReq
  :: ( MonadError e m
     , MonadReader r m
     , AsHttpError e
     , HasHttpCfg r
     , HasSendGrid r
     , MonadIO m )
  => HttpMethod
  -> String
  -> RequestData
  -> m Request
sendGridReq method url' dta = do
  apiKey <- view sendGridApiKeyV3
  let url = "https://api.sendgrid.com/" ++ url'
  addHeader "Authorization" ("Bearer " <> B8.pack apiKey)
    <$> buildReq method url dta

-- | Add a header to given request.

addHeader :: ByteString -> ByteString -> Request -> Request
addHeader header value req =
  req { requestHeaders = (CI.mk header, value) : requestHeaders req }

-- | Get 'RecipientId' from email. All recipient IDs are a URL-safe base64
-- encoding of the recipient's lower cased email address; for example if a
-- recipient's email is foo@example.com, their recipient ID is
-- Zm9vQGV4YW1wbGUuY29t.

recipientIdFromEmail :: String -> RecipientId
recipientIdFromEmail =
  RecipientId . B8.unpack . B64.encode . B8.map toLower . B8.pack
