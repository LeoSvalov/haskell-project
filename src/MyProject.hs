{-# LANGUAGE DeriveGeneric #-}
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import GHC.Generics (Generic)
import qualified Data.Vector as V

data Player = Player{
    player_id :: !Int, 
    name  :: !String, 
    role  :: !String, 
    acted_as_sweeper :: !String,  -- :: !Int 
    diving_save  :: !String, -- :: !Int 
    goals_conceded :: !String, -- :: !Int 
    minutes_played :: !String, -- :: !Int 
    punches :: !String,
    saves :: !String, -- :: !Int 
    saves_inside_box :: !String,  --  :: !Int 
    throws  :: !String, -- :: !Int 
    accurate_passes :: !String, --  :: !Int 
    assists :: !String, --  :: !Int 
    chances_created :: !String, --  :: !Int 
    goals :: !String,
    pass_success :: !String,  -- :: !Int 
    total_shots  :: !String, -- :: !Int 
    blocked_shots :: !String, --  :: !Int 
    shot_accuracy  :: !String,
    shot_off_target :: !String, -- :: !Int 
    shot_on_target :: !String, --  :: !Int 
    shots_woodwork :: !String, -- :: !Int 
    accurate_long_balls  :: !String, -- :: !Int 
    crosses,key_passes :: !String, --  :: !Int 
    long_balls :: !String, --  :: !Int 
    passes  :: !String, -- :: !Int 
    touches :: !String, -- :: !Int 
    aerials_lost :: !String, -- :: !Int 
    aerials_won :: !String, -- :: !Int 
    clearances :: !String, -- :: !Int 
    dispossessed :: !String, -- :: !Int 
    dribbles_attempted :: !String, --  :: !Int 
    dribbles_succeeded :: !String, -- :: !Int 
    duels_lost  :: !String, -- :: !Int 
    duels_won :: !String, -- :: !Int 
    fouls :: !String, -- :: !Int 
    interceptions :: !String, -- :: !Int 
    recoveries  :: !String, -- :: !Int 
    tackles_attempted :: !String, -- :: !Int 
    tackles_succeeded :: !String, -- :: !Int 
    was_fouled :: !String, -- :: !Int 
    is_a_sub :: !String, -- :: !Int 
    was_subbed :: !String, -- :: !Int 
    yellow_card :: !String, -- :: !Int 
    red_card :: !String, -- :: !Int 
    rating :: !String -- :: !Int 
    }  deriving (Generic, Show)

instance FromNamedRecord Player
instance ToNamedRecord Player
instance DefaultOrdered Player

main :: IO ()
main = do
    csvData <- BL.readFile "/Users/levsvalov/code_workspace/Spring2021/haskell/project/data/SoccerPlayersData.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ name p ++ " -- " ++ show (player_id p)
