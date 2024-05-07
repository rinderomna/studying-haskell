import Data.Maybe (listToMaybe)
-- 1
data Privilege = Unprivileged | Admin
    deriving (Show, Eq)

-- 2
type Username = String
data Cookie = LoggedIn Username Privilege | LoggedOut

-- 3
type Password = String
type Database = [(Username, Password, Privilege)]
db :: [(String, String, Privilege)]
db = [("dumbledore", "abracadabra", Unprivileged),
      ("root", "secret", Admin),
      ("john", "apple", Unprivileged)]

-- 4
register :: Username -> Password -> Cookie -> Database -> Database
register username password (LoggedIn _ Admin) db = (username, password, Unprivileged) : db
register _ _ _ db = db

-- 5
getUser :: Username -> Database -> Maybe (Password, Privilege)
getUser username [] = Nothing
getUser username ((username', password, privilege):db)
    | username == username' = Just (password, privilege)
    | otherwise = getUser username db

getUser' :: Username -> Database -> Maybe (Password, Privilege)
getUser' username db = listToMaybe xs
    where
        xs = [
              (password, privilege) |
              (username', password, privilege) <- db,
               username' == username
            ]

-- 6
-- login "johewbacca" "alma" db == LoggedOut
login :: Username -> Password -> Database -> Cookie
login username password db = f $ getUser username db
    where
        f :: Maybe (Password, Privilege) -> Cookie
        f $ Just (password', privilege)
            | password == password' = LoggedIn username privilege
        f _ = LoggedOut

login' :: Username -> Password -> Database -> Cookie
login' username password db = case getUser username db of
    Just (password', privilege)
        | password == password' -> LoggedIn username privilege
    _ -> LoggedOut

-- 7
-- passwd "pear" (LoggedIn "john" Unprivileged) db ==
-- [("dumbledore","abracadabra",Unprivileged), ("root","secret",Admin), ("john","pear",Unprivileged)]
-- passwd "berry" LoggedOut db == db
-- passwd "misanthrope" (LoggedIn "voldemort" Unprivileged) db == db
passwd :: Password -> Cookie -> Database -> Database
passwd _ LoggedOut db = db
passwd newPassword (LoggedIn username _) db = map f db
    where
        f (username', oldPassword, privilege')
            | username == username' = (username', newPassword, privilege')
            | otherwise = (username', oldPassword, privilege')

-- 8
delete :: Username -> Cookie -> Database -> Database
delete username (LoggedIn _ Admin) db = filter (\(username', _, _) -> username /= username') db
delete username _ db = db

-- 9
users :: Database -> [Username]
users db = [username | (username, _, _) <- db]