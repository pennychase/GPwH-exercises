import qualified Data.Map as M
import Data.Map (Map)

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map GamerId UserName
userNameDB = M.fromList [ (1, "nYarlathoTep")
                        , (2, "KINGinYELLOW")
                        , (3, "dagon1997")
                        , (4, "rcarter1919")
                        , (5, "xCTHULHUx")
                        , (6, "yogSOThoth")]

creditsDB :: Map UserName PlayerCredits
creditsDB = M.fromList  [ ("nYarlathoTep", 2000)
                        , ("KINGinYELLOW", 15000)
                        , ("dagon1997", 300)
                        , ("rcarter1919", 12)
                        , ("xCTHULHUx", 50000)
                        , ("yogSOThoth", 150000)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = M.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits user = M.lookup user creditsDB

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

-- Q30.1
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM g u = u >>= (\x -> return (g x) )

-- Q30.2
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp u v = u  >>= (\f -> v 
                >>= (\x -> return (f x)))

-- Q30.3
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just a) g = g a
