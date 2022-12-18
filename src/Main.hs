import Data.List
import SDL
import SDL.Font (Font)
import System.Random
import qualified SDL.Font as Font
import qualified Data.List as L 

data Fruit = Fruit {
    position :: (Int, Int),
    velocity :: (Int, Int)
}

data Snake = Snake {
    body :: [Segment]
}

data Segment = Segment {
    segmentPos :: (Int, Int),
    segmentVel :: (Int, Int)
}

screenWidth, screenHeight :: Int
screenWidth = 640
screenHeight = 480

-- Initializes the game with 10 random fruit positions and velocities
initGame :: IO [Fruit]
initGame = do
    gen <- getStdGen
    let fruitPositions = take 10 $ randomRs ((0,0), (screenWidth,screenHeight)) gen
    let fruitVelocities = take 10 $ randomRs ((-5,-5), (5,5)) gen
    return $ zipWith Fruit fruitPositions fruitVelocities

-- Updates the position of a fruit by adding its velocity
updateFruit :: Fruit -> Fruit
updateFruit (Fruit (x,y) (vx,vy)) = Fruit (x+vx, y+vy) (vx,vy)

-- Bounces a fruit off the edges of the screen if it goes out of bounds
bounceFruit :: Fruit -> Fruit
bounceFruit (Fruit (x,y) (vx,vy)) = Fruit (x',y') (vx',vy')
    where x' = if x < 0 then 0
               else if x > screenWidth then screenWidth else x
          y' = if y < 0 then 0
               else if y > screenHeight then screenHeight else y
          vx' = if x < 0 || x > screenWidth then -vx else vx
          vy' = if y < 0 || y > screenHeight then -vy else vy

-- Updates the snake's body by adding the new head position
updateSnake :: Snake -> (Int, Int) -> Snake
updateSnake snake newHeadPos = snake { body = newHead : body snake }
    where newHead = Segment newHeadPos (segmentVel (head (body snake)))

-- | Checks if the head of the snake collides with any of the fruits
checkCollision :: Snake -> [Fruit] -> (Bool, [Fruit])
checkCollision (Snake (Segment (x,y) _:_)) fruits = (collision, updatedFruits)
  where collision = any (\(Fruit (fx,fy) _) -> x `elem` [fx..fx+20] && y `elem` [fy..fy+20]) fruits
        updatedFruits = filter (\(Fruit (fx,fy) _) -> x `L.notElem` [fx..fx+20] || y `L.notElem` [fy..fy+20]) fruits

-- | Reads WASD input from player and updates the snake to move that direciton
playerInput :: Window -> Snake -> IO Snake
playerInput window snake = do
    event <- pollEvent
    return $ case eventPayload <$> event of
        Just (KeyboardEvent keyEvent) -> let  
            keycode = keysymKeycode $ keyboardEventKeysym keyEvent
            in if keycode `elem` [KeycodeW, KeycodeA, KeycodeS, KeycodeD]
            then case keycode of
                KeycodeW -> updateSnake snake (0, -1)
                KeycodeA -> updateSnake snake (-1, 0)
                KeycodeS -> updateSnake snake (0, 1)
                KeycodeD -> updateSnake snake (1, 0)
                _ -> snake
            else snake
        _ -> snake
    where
        updateSnake :: Snake -> (Int, Int) -> Snake
        updateSnake (Snake (x:xs)) vel = Snake $ (Segment (addTuples (segmentPos x) vel) vel) : xs
        addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
        addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Main game loop
gameLoop :: Window -> Renderer -> Font -> [Fruit] -> Snake -> IO ()
gameLoop window renderer font fruits snake = do
    -- Update fruit positions and bounce off screen if necessary
    let updatedFruits = map bounceFruit $ map updateFruit fruits
    -- Update snake position and check for collision with fruit
    movedSnake <- playerInput window $ moveSnake snake
    let (collision, updatedFruits') = checkCollision movedSnake updatedFruits
    let updatedSnake = if collision then appendSegment movedSnake (head $ body updatedSnake) else movedSnake
    -- Render updated game state
    render renderer font updatedFruits' updatedSnake
    -- Repeat game loop with updated game state
    gameLoop window renderer font updatedFruits' updatedSnake

-- Appends a new segment to the snake with an offset
appendSegment :: Snake -> Segment -> Snake
appendSegment snake segment = Snake (body snake ++ [newSegment])
    where newSegment = Segment ((fst $ segmentPos segment) + 20 * (fst $ segmentVel segment), (snd $ segmentPos segment) + 20 * (snd $ segmentVel segment)) (segmentVel segment)

moveSnake :: Snake -> Snake
moveSnake s = s { body = map moveSegment (body s) }
    where 
    moveSegment seg = seg { segmentPos = addPairs (segmentPos seg) (segmentVel seg) }
    addPairs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

render :: Renderer -> Font -> [Fruit] -> Snake -> IO ()
render renderer font fruits snake = do
    -- Clear screen
    SDL.clear renderer

    -- Draw each fruit
    mapM_ (drawFruit renderer font) fruits

    -- Draw snake
    drawSnake renderer font snake

    -- Update screen
    SDL.present renderer

drawFruit :: Renderer -> Font -> Fruit -> IO ()
drawFruit renderer font fruit = do
    -- Render fruit to screen
    let (x, y) = position fruit
    fruitSurface <- Font.solid font (V4 255 0 0 255) "F"
    texture <- SDL.createTextureFromSurface renderer fruitSurface
    SDL.copy renderer texture Nothing (Just $ fmap fromIntegral (SDL.Rectangle (P (V2 x y)) (V2 20 20)))
    SDL.destroyTexture texture

drawSnake :: Renderer -> Font -> Snake -> IO ()
drawSnake renderer font snake = do
    -- Render each segment of snake's body to screen
    mapM_ (drawSegment renderer font) $ body snake

drawSegment :: Renderer -> Font -> Segment -> IO ()
drawSegment renderer font (Segment (x, y) _) = do
    -- Render segment to screen
    segmentSurface <- Font.solid font (V4 0 255 0 255) "X"
    texture <- SDL.createTextureFromSurface renderer segmentSurface
    SDL.copy renderer texture Nothing (Just (fmap fromIntegral $ SDL.Rectangle (P (V2 x y)) (V2 20 20)))
    SDL.destroyTexture texture

main :: IO ()
main = do
    -- Initialize SDL and game
    initializeAll
    Font.initialize
    fruits <- initGame
    let snake = Snake [Segment (250, 250) (1, 0)] 

    -- Create a window
    window <- SDL.createWindow "Snake game" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 (fromIntegral screenWidth) (fromIntegral screenHeight) }

    -- Create a renderer
    renderer <- SDL.createRenderer window (-1) SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = True
        }

    -- Load the font
    font <- Font.load "monobit.ttf" 24

    -- Start game loop
    gameLoop window renderer font fruits snake

    -- Clean up SDL resources
    quit