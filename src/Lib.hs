{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Function (on)
import Data.List (sortBy, intercalate)

import Text.Pretty.Simple (pPrint)
import Data.KMeans as KMeans

import Graphics.Gloss as Gloss


data Player = Player
        {
            player_id             :: !Int,
            name                  :: !String,
            nationality           :: !String,
            club                  :: !String,
            rating                :: !Int,
            height                :: !String,
            weight                :: !String,
            preffered_foot        :: !String,
            age                   :: !Int,
            preffered_position    :: !String,
            work_rate             :: !String,
            weak_foot             :: !Int,
            skill_moves           :: !Int,
            ball_control          :: !Int,
            dribbling             :: !Int,
            marking               :: !Int,
            sliding_tackle        :: !Int,
            standing_tackle       :: !Int,
            aggression            :: !Int,
            reactions             :: !Int,
            attacking_position    :: !Int,
            interceptions         :: !Int,
            vision                :: !Int,
            composure             :: !Int,
            crossing              :: !Int,
            short_pass            :: !Int,
            long_pass             :: !Int,
            acceleration          :: !Int,
            speed                 :: !Int,
            stamina               :: !Int,
            strength              :: !Int,
            balance               :: !Int,
            agility               :: !Int,
            jumping               :: !Int,
            heading               :: !Int,
            shot_power            :: !Int,
            finishing             :: !Int,
            long_shots            :: !Int,
            curve                 :: !Int,
            freekick_accuracy     :: !Int,
            penalties             :: !Int,
            volleys               :: !Int,
            gk_positioning        :: !Int,
            gk_diving             :: !Int,
            gk_kicking            :: !Int,
            gk_handling           :: !Int,
            gk_reflexes           :: !Int
        }
    deriving (Eq, Show)

instance FromNamedRecord Player where
  parseNamedRecord m =
    Player
      <$>  m .: "ID"
      <*>  m .: "Name"
      <*>  m .: "Nationality"
      <*>  m .: "Club"
      <*>  m .: "Rating"
      <*>  m .: "Height"
      <*>  m .: "Weight"
      <*>  m .: "Preffered_Foot"
      <*>  m .: "Age"
      <*>  m .: "Preffered_Position"
      <*>  m .: "Work_Rate"
      <*>  m .: "Weak_foot"
      <*>  m .: "Skill_Moves"
      <*>  m .: "Ball_Control"
      <*>  m .: "Dribbling"
      <*>  m .: "Marking"
      <*>  m .: "Sliding_Tackle"
      <*>  m .: "Standing_Tackle"
      <*>  m .: "Aggression"
      <*>  m .: "Reactions"
      <*>  m .: "Attacking_Position"
      <*>  m .: "Interceptions"
      <*>  m .: "Vision"
      <*>  m .: "Composure"
      <*>  m .: "Crossing"
      <*>  m .: "Short_Pass"
      <*>  m .: "Long_Pass"
      <*>  m .: "Acceleration"
      <*>  m .: "Speed"
      <*>  m .: "Stamina"
      <*>  m .: "Strength"
      <*>  m .: "Balance"
      <*>  m .: "Agility"
      <*>  m .: "Jumping"
      <*>  m .: "Heading"
      <*>  m .: "Shot_Power"
      <*>  m .: "Finishing"
      <*>  m .: "Long_Shots"
      <*>  m .: "Curve"
      <*>  m .: "Freekick_Accuracy"
      <*>  m .: "Penalties"
      <*>  m .: "Volleys"
      <*>  m .: "GK_Positioning"
      <*>  m .: "GK_Diving"
      <*>  m .: "GK_Kicking"
      <*>  m .: "GK_Handling"
      <*>  m .: "GK_Reflexes"


path = "/Users/levsvalov/code_workspace/Spring2021/haskell/project/nearestPlayers/data/SoccerPlayersData.csv"

getByteString :: FilePath -> IO ByteString
getByteString = ByteString.readFile

decode :: IO (Either String (Header, Vector.Vector Player))
decode = (fmap Cassava.decodeByName . getByteString) path

readDataset :: Either String (Header, Vector.Vector Player) -> Vector.Vector Player
readDataset d = case d of
    Left err -> Vector.empty
    Right (_,v) -> v

features = [skill_moves, weak_foot, ball_control, dribbling, marking, sliding_tackle,
            standing_tackle, aggression, reactions, attacking_position, interceptions,
            vision, composure, crossing, short_pass, long_pass, acceleration, speed,
            stamina, strength, balance, agility, jumping, heading, shot_power,
            finishing, long_shots, curve, freekick_accuracy, volleys,
            gk_positioning, gk_diving, gk_kicking, gk_handling, gk_reflexes]

run :: IO ()
run = do
  putStrLn "Enter action: 1 - for KNN, otherwise - KMeans"
  input <- getLine
  let i = (read input :: Int)
  if i ==  1 then doKNN
  else doKMeans


doKNN :: IO ()
doKNN = do
  ds <- readDataset <$> decode
  putStrLn "Enter id of the target player:"
  input <- getLine
  let index = (read input :: Int) -- the index of target player
  let targetPlayer = ds Vector.! index

  let dataset = Vector.ifilter (\i _ -> i /= index) ds
  pPrint ("The target player: " ++ name targetPlayer)
  pPrint $ intercalate ", " $ getValuesByAttributes [name, nationality, preffered_position, club] targetPlayer
  -- pPrint targetPlayer

  let k = 4 -- top k results
  let metric = cosine -- euclidian 
  print ("------------  Suggested" ++ show k ++  "players ------------" )
  let result = knn dataset features targetPlayer metric k
  prettyKNN result
  drawChain $ (targetPlayer,0.0) : result

doKMeans :: IO ()
doKMeans = do
  ds <- readDataset <$> decode
  putStrLn "Enter size of sample for clustering:"
  input <- getLine
  let n = (read input :: Int)
  let sample = getNPlayers ds n
  print "Sample:"

  pPrint $ map (intercalate ", " . getValuesByAttributes [name,preffered_position]) sample
  let getValues player = fromIntegral <$> getValuesByAttributes features player
  let k = 3 -- number of clusters
  print ("------------ " ++ show k ++ " clusters ------------")
  let result = kmeansGen getValues k sample
  prettyKMeans $ zip [0..] result



drawChain :: [(Player, Double)] -> IO()
drawChain ps = display window background $ drawPlayers (-400) ps
    where
      window = InWindow "Similiar players" (1200, 2200) (0, 0)
      background = white

      drawPlayers :: Int -> [(Player, Double)] -> Picture
      drawPlayers _ [] = blank
      drawPlayers shift [(p,dist)] =
        translate (fromIntegral shift) 0 (drawPlayer p)
      drawPlayers shift ((p, d): (p2, dist):pls) =
        translate (fromIntegral shift) 0 (drawPlayer p)
        <> line [(fromIntegral (shift+75),0), (fromIntegral(shift+200),0)]
        <> translate (fromIntegral (shift+85)) 2 (Scale 0.1 0.1 (text (take 5 $ show dist)))
        <> drawPlayers (shift + 200) ((p2,dist):pls)

      drawPlayer p = color (dark green) (circleSolid 75)
       <> translate (-40) 0 (Scale 0.1 0.1 (text(getValueByAttribute name p)))
       <> translate (-40) (-25) (Scale 0.1 0.1 (text(getValueByAttribute preffered_position p)))



prettyKNN :: [(Player, Double)] -> IO ()
prettyKNN = mapM_ printPlayer
  where
    printPlayer (player, dist) =
      print ("Distance: " ++ show dist) <>
      pPrint (intercalate ", " $ getValuesByAttributes [name,preffered_position] player)

prettyKMeans :: [(Int,[Player])] -> IO ()
prettyKMeans = mapM_ printCluster
  where
    printCluster (i, players) =
      print ("cluster "  ++ show (i+1)) <>
      pPrint (map (intercalate ", " . getValuesByAttributes [name,preffered_position]) players)

getNPlayers :: Vector.Vector Player -> Int -> [Player]
getNPlayers ds n = Vector.toList $  Vector.take n ds 

getPlayerById :: Int -> Vector.Vector Player -> Player
getPlayerById player_id players = players Vector.! player_id

getValueByAttribute :: (Player -> v) -> Player -> v -- must be the same type!!
getValueByAttribute attribute = attribute

getValuesByAttributes :: [Player -> v] -> Player -> [v]
getValuesByAttributes attributes player = map(`getValueByAttribute` player) attributes

cosine :: [(Int, Int)] -> Double
cosine coords = dotprod / (norm v1 * norm v2)
  where
    (v1,v2) = unzip coords
    dotprod = fromIntegral $ sum $ zipWith (*) v1 v2
    norm v = sqrt $ fromIntegral $ sum $ map(^2) v


euclidian :: [(Int,Int)] -> Double
euclidian coords= sqrt $ fromIntegral $ sum $ map(\(x1,x2) -> (x1 - x2)^2) coords

distance
  :: Player
  -> Player
  -> [Player -> Int]
  -> ([(Int, Int)] -> Double) -- the method of calculating distance
  -> (Int, Double)
distance player target features metric= (getValueByAttribute player_id player, metric $ zip v vTarget)
  where
    v = getValuesByAttributes features player
    vTarget = getValuesByAttributes features target


caclulateDistance
  :: Vector.Vector Player
  -> Player
  -> [Player -> Int]
  -> ([(Int, Int)] -> Double) -- the method of calculating distance
  -> [(Int,Double)]
caclulateDistance players target features metric = Vector.toList $ Vector.map(\player -> distance player target features metric) players


knn :: Vector.Vector Player        -- dataset
    -> [Player -> Int]      -- list of features 
    -> Player               -- the player to which we search nearests
    -> ([(Int, Int)] -> Double) -- the method of calculating distance
    -> Int                  -- k nearests to return
    -> [(Player, Double)]    -- output list of players with distance to target
knn players features target metric k = nearestPlayers
  where
    distances = take k $ sortBy (compare `on` snd) $ caclulateDistance players target features metric
    nearestPlayers = map(\(i, d) -> (getPlayerById i players,d)) distances


