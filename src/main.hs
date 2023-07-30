{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Data.Text
import System.Random

-- | data representing a game state
data GameState = GameState
  {
  spaceshipX :: Double,
  aliens :: [Alien],
  bullets :: [Bullet],
  alienBullets :: [Bullet],
  score :: Int,
  health :: Int,
  alienDirection :: Double
  }

-- | data representing the coordinates of an alien
data Alien = Alien
  {
  alienX :: Double,
  alienY :: Double
  }

-- | data representing the coordinates of a bullet
data Bullet = Bullet
  {
  bulletX :: Double,
  bulletY :: Double
  }
  
-- | data representing the initial game state
initialState :: GameState
initialState = GameState
 {
     spaceshipX = 0,
     aliens = initialAliens,
     bullets = [],
     alienBullets = [],
     score = 0,
     health = 100,
     alienDirection = 2  
 }
 
-- | Build the list of initial aliens
initialAliens :: [Alien]
initialAliens = [ Alien x y | x <- [-3,-2..3], y <- [2,3..6] ]

-- | draw a game state
drawState :: GameState -> Picture
drawState gamestate 
  | (Prelude.length (aliens gamestate)) == 0 = translated 0 2 (colored white (lettering (Data.Text.pack ("You Won!"))))
                                              <> translated 0 (0) (colored white (lettering (Data.Text.pack ("Your score: " ++ show (score gamestate)))))
                                              <> translated 0 (-2) (colored white (lettering (Data.Text.pack ("Press Space to play again"))))
                                              <> colored black (solidRectangle 20 20)
  | (health gamestate) == 0 = translated 0 1 (colored white (lettering (Data.Text.pack ("You lost :("))))
                                <> translated 0 (-1) (colored white (lettering (Data.Text.pack("Press Space to play again"))))
                                <> colored black(solidRectangle 20 20)
  | otherwise = pictures
                [    
                  drawSpaceship (spaceshipX gamestate),
                  pictures $ Prelude.map drawAlien (aliens gamestate),
                  pictures $ Prelude.map drawBullet (bullets gamestate),
                  pictures $ Prelude.map drawBullet (alienBullets gamestate),
                  drawScore (score gamestate),
                  drawHealth (health gamestate)
                ] <> colored black (solidRectangle 20 20)

-- | draw a spaceship
drawSpaceship :: Double -> Picture
drawSpaceship x = translated x (-5) $ colored green $ solidPolygon [(0,0.5), (-0.5,-0.5), (0.5,-0.5)]

-- | draw an alien
drawAlien :: Alien -> Picture
drawAlien alien
  | (alienY alien) == 6 = translated (alienX alien) (alienY alien) $ colored green $ solidCircle 0.3
  | otherwise = translated (alienX alien) (alienY alien) $ colored green $ solidRectangle 0.6 0.6
 
-- | draw a bullet
drawBullet :: Bullet -> Picture
drawBullet bullet = translated (bulletX bullet) (bulletY bullet) $ colored white $ solidCircle 0.1

-- | draw the current score of the player
drawScore :: Int -> Picture
drawScore score = translated (-7) 8 $ colored white $ scaled 0.7 0.7 (lettering(Data.Text.pack ("Score: " ++ show score)))

-- | render the current health of the player
drawHealth :: Int -> Picture
drawHealth health = translated 8 8 $ (colored white $ scaled 0.6 0.6 (lettering(Data.Text.pack (show health)))) 
                                      <> translated (-3) 0 (colored color (solidRectangle (4*(fromIntegral (health) / 100)) 0.6)
                                      <> translated (-3) 0 (colored green $ scaled 0.7 0.7 (lettering(Data.Text.pack "💚"))))
  where
    color = assignColor health

-- | color the health bar according to the current life danger
assignColor :: Int -> Color
assignColor health 
  | health > 70 = green
  | health > 50 = orange
  | otherwise = red

-- | code for generating a random index of the aliens list
seed :: StdGen
seed = mkStdGen 42
randomIndex :: GameState -> Int
randomIndex gamestate = Prelude.head $ randomRs (0, Prelude.length (aliens gamestate) - 1) seed

-- | handle the time passing and key release events
handleEvent :: Event -> GameState -> GameState
handleEvent (TimePassing dt) gamestate = updatedGameState
  where
    updatedBullets = moveBullets dt (bullets gamestate)
    updatedAlienBullets = moveAlienBullets dt (alienBullets gamestate)
    (filteredBullets, filterAlienBullets, filteredAliens) = removeDuplicates updatedBullets updatedAlienBullets (aliens gamestate) gamestate
    updatedGameState = gamestate { bullets = filteredBullets, aliens = moveAliens dt (filteredAliens) (alienDirection gamestate), 
                                  alienBullets = filterAlienBullets, score = (35 - Prelude.length (aliens gamestate)), 
                                  health = updateHealth dt (alienBullets gamestate) (spaceshipX gamestate) (health gamestate), alienDirection = updateAlienDirection dt (aliens gamestate) (alienDirection gamestate)}
handleEvent (KeyRelease key) gamestate
  | key == "Left" = gamestate { spaceshipX = max (-9) (spaceshipX gamestate - 1) }
  | key == "Right" = gamestate { spaceshipX = min 9 (spaceshipX gamestate + 1) }
  | key == "Down" = gamestate {alienBullets = Bullet (alienX (((aliens gamestate) !! index))) 5 : alienBullets gamestate}
  | key == " " = if (health gamestate) == 0 || (Prelude.length (aliens gamestate)) == 0
                then initialState
                else gamestate { bullets = newBullet : bullets gamestate }
  | otherwise = gamestate
  where
    newBullet = Bullet (spaceshipX gamestate) (-4.5)
    index = randomIndex gamestate
handleEvent _ gamestate = gamestate

-- | move spaceship bullets
moveBullets :: Double -> [Bullet] -> [Bullet]
moveBullets dt = Prelude.map (\bullet -> bullet { bulletY = bulletY bullet + dt*10 })

-- | move alien bullets
moveAlienBullets :: Double -> [Bullet] -> [Bullet]
moveAlienBullets dt = Prelude.map (\bullet -> bullet { bulletY = bulletY bullet - dt*10 })

-- | move aliens
moveAliens :: Double -> [Alien] -> Double -> [Alien]
moveAliens dt aliens direction = Prelude.map (\alien -> alien { alienX = alienX alien + dt * direction }) aliens

-- | update alien direction
updateAlienDirection :: Double -> [Alien] -> Double -> Double
updateAlienDirection _ aliens prevDirection
  | Prelude.any (\alien -> alienX alien >= 9) aliens = -2
  | Prelude.any (\alien -> alienX alien <= (-9)) aliens = 2
  | otherwise = prevDirection

-- | remove killed aliens or collision bullets
removeDuplicates :: [Bullet] -> [Bullet] -> [Alien] -> GameState -> ([Bullet], [Bullet], [Alien])
removeDuplicates bullets alienBullets aliens gamestate = (updatedBullets, updatedAlienBullets, updatedAliens)
  where
    updatedBullets = Prelude.filter (\bullet -> not (Prelude.any (\alien -> distance (bulletX bullet) (bulletY bullet) (alienX alien) (alienY alien) <= 0.4) aliens)) bullets
    updatedAlienBullets = Prelude.filter (\alienBullet -> (distance (bulletX alienBullet) (bulletY alienBullet) (spaceshipX gamestate) (-5) > 0.4) && (bulletY alienBullet > -6)) alienBullets
    updatedAliens = Prelude.filter (\alien -> not (Prelude.any (\bullet -> distance (bulletX bullet) (bulletY bullet) (alienX alien) (alienY alien) <= 0.4) bullets)) aliens

distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- | update the health of a player
updateHealth :: Double -> [Bullet] -> Double -> Int -> Int
updateHealth dt bullets shipX health
  | Prelude.any (\bullet -> bulletY bullet <= -4.4 && abs (bulletX bullet - shipX) <= 0.5) bullets = max 0 (health - 1)
  | otherwise = health

main :: IO()
main = activityOf initialState handleEvent drawState

-- Future work
-- *Automate alien bullets shooting
-- *Save scores and show the best score
-- *Build game levels in increasing difficulty
