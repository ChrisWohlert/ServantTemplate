{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

module API where

import Type.Reflection
import Data.Aeson                       
import qualified Data.ByteString     as B           
import qualified Data.Map            as M
import Data.Proxy                       
import Data.Text (pack, unpack, Text)
import GHC.Generics                     
import Network.Wai                      
import Network.Wai.Handler.Warp         
import Servant.API                      
import Servant.API.BasicAuth            
import Servant.API.Experimental.Auth    
import Servant                         hiding (layout)  
import Servant.Server                  hiding (layout)  
import Servant.Server.Experimental.Auth
import Web.Cookie                      
import Database
import Database.Persist.Sql
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Config
import Data.UUID
import GHC.TypeLits
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8
import Network.HTTP.Media ((//), (/:))
import Views
import Models
import Forms
import Components



-- | private data that needs protection
newtype PrivateData = PrivateData { ssshhh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | public data that anyone can use.
newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData
instance ToJSON User

type PublicAPI = "login" :> QueryParam "username" Text :> QueryParam "password" Text :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] [PublicData])
            :<|> "test" :> Get '[JSON] [User]

type GetList a = Get '[JSON, Html] (Layout [a])
type GetOne a i = Capture "id" i :> Get '[JSON, Html] (Layout a)
type CreateNew a = ReqBody '[JSON] a :> Post '[JSON, Html] NoContent

type Crud (name :: Symbol) a i = name :> 
  (    GetList a
  :<|> GetOne a i
  :<|> CreateNew a
  )


type Redirect a = Headers '[Header "location" a] NoContent

type PrivateAPI = Get '[JSON] PrivateData


instance ToMarkup a => MimeRender Html a where
    mimeRender _ = renderHtml . toHtml

instance Accept Html where
    contentType _ = "text" // "html" /: ("charset", "utf-8")


lookupAccount :: ConnectionPool -> B.ByteString -> Handler User
lookupAccount pool sessionId = do
  maybeUser <- runWithPool pool (selectFirst [] [])
  case maybeUser of
    Nothing -> throwError (err403 { errBody = "Invalid Cookie" })
    (Just u) -> return $ toUser u


authHandler :: ConnectionPool -> AuthHandler Request User
authHandler pool = mkAuthHandler handler
  where
  maybeToEither e = maybe (Left e) Right
  throw401 msg = throwError $ err401 { errBody = msg }
  handler req = either throw401 (lookupAccount pool) $ do
    cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
    maybeToEither "Missing token in cookie" $ lookup "servant-auth-cookie" $ parseCookies cookie


type AuthGenAPI = Crud "book" Book Int
            :<|> "dist" :> Raw


genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy


type instance AuthServerData (AuthProtect "cookie-auth") = User


genAuthServerContext :: ConnectionPool -> Context (AuthHandler Request User ': '[])
genAuthServerContext pool = authHandler pool :. EmptyContext


genAuthServer :: ServerT AuthGenAPI App
genAuthServer = crud (\ (UserDatabaseModel u p) -> Book "isdsadbdn222" "Heyo Post!") 
           :<|> serveDirectoryWebApp "dist"

getBooks f = do
  elements <- runQuery (selectList [] [])
  return $ layout $ map (f . (\ (Entity k v) -> v)) $ elements

editBook :: Int -> App (Layout Book)
editBook _ = return $ layout (Book "dadsa" "name")
  
createBook :: Book -> App NoContent
createBook b = return NoContent

crud f = getBooks f :<|> editBook :<|> createBook

redirect link = return $ addHeader link NoContent

privateDataFunc (User name email) = do
  pool <- asks db
  return (PrivateData ("this is a secret: " <> (pack name)))

login :: Maybe Text -> Maybe Text -> App (Headers '[Header "Set-Cookie" SetCookie] [PublicData])
login (Just name) (Just pw) = do
  pool <- db <$> ask
  pure $ addHeader (def { setCookieName = "servant-auth-cookie", setCookieValue = "key3", setCookieHttpOnly = True }) [PublicData ("Hello: " <> name)]
login _ _ = throwError err401

getUsers :: App [User]
getUsers = do
  users <- runQuery (selectList [] [])
  return $ map toUser users

toUser :: Entity UserDatabaseModel -> User
toUser (Entity k (UserDatabaseModel name pw)) = User name pw

  -- | run our server
genAuthMain :: IO ()
genAuthMain = do
  pool <- makePool
  print "Start migration"
  runSqlPool (runMigration migrateAll) pool
  print "End migration"
  run 8080 $ app $ AppEnv { db = pool }

app cfg = 
  serveWithContext genAuthAPI (genAuthServerContext (db cfg)) $
    hoistServerWithContext genAuthAPI (Proxy :: Proxy (AuthHandler Request User ': '[]))
      (flip runReaderT cfg) genAuthServer


nt :: AppEnv -> App a -> Handler a
nt s x = runReaderT x s



layout = Layout menu

api = Proxy :: Proxy AuthGenAPI

menu = LeftMenu 
  (safeLink api (Proxy :: Proxy (CrudListLink Book "book"))) 
  "Hallo" 
  [MenuLink Tag (safeLink api (Proxy :: Proxy (CrudListLink Book "book"))) "Link books!"]



type CrudListLink a n = n :> GetList a

type A = "book" :>  Get '[JSON, Html] (Layout [Book])