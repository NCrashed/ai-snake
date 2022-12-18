I want you to output code in Haskell language. The program is game that renders with SDL2 library. There are 10 randomly generated fruits at the start. A fruit has velocity and bounces off the edges of screen. Player is a snake which tail grows when his head collides with a fruit. But, if a fruit collides with tail of the player, the tail is cut off and removed. 

-------- Adjustement 1

Please, implement function `render :: Renderer -> Font -> [Fruit] -> Snake -> IO ()` for the described game. Consider the following definitions for related types:
```
data Fruit = Fruit {
    position :: (Int, Int),
    velocity :: (Int, Int)
}

data Snake = Snake {
    body :: [(Int, Int)]
}
```

-------- Adjustement 2

Please, implement function `moveSnake :: Snake -> Snake` that moves all segments of the snake towards current direction.

```
data Snake = Snake {
    body :: [Segment]
}

data Segment = Segment {
    segmentPos :: (Int, Int),
    segmentVel :: (Int, Int)
}
```

--------- Adjustement 3

Please, implement function:
```
-- Updates the snake's body by adding the new head position
updateSnake :: Snake -> (Int, Int) -> Snake
```

Consider the following definitions for related types:
```
data Snake = Snake {
    body :: [Segment]
}

data Segment = Segment {
    segmentPos :: (Int, Int),
    segmentVel :: (Int, Int)
}
```

--------- Adjustement 4

Please, implement function:
```
-- | Reads WASD input from player and updates the snake to move that direciton
playerInput :: Window -> Snake -> IO Snake 
```

Consider the following definitions for related types:
```
data Snake = Snake {
    body :: [Segment]
}

data Segment = Segment {
    segmentPos :: (Int, Int),
    segmentVel :: (Int, Int)
}
```

------------ Adjustement 5
Please, modify the function:
```
-- Checks if the head of the snake collides with any of the fruits
checkCollision :: Snake -> [Fruit] -> (Bool, [Fruit])
checkCollision (Snake (Segment (x,y) _:_)) fruits = (collision, updatedFruits)
    where collision = any (\(Fruit (fx,fy) _) -> x == fx && y == fy) fruits
          updatedFruits = filter (\(Fruit (fx,fy) _) -> x /= fx || y /= fy) fruits
```

Modify the function to detect collision with fruits that has size of 20 by 20 pixels.


-------------- Adjustement 6
Please, modify the function:
```
-- Main game loop
gameLoop :: Window -> Renderer -> Font -> [Fruit] -> Snake -> IO ()
gameLoop window renderer font fruits snake = do
    -- Update fruit positions and bounce off screen if necessary
    let updatedFruits = map bounceFruit $ map updateFruit fruits
    -- Update snake position and check for collision with fruit
    movedSnake <- playerInput window $ moveSnake snake
    let (collision, updatedFruits') = checkCollision movedSnake updatedFruits
    let updatedSnake = if collision then updateSnake movedSnake (position $ head updatedFruits') else movedSnake
    -- Cut off tail of snake if it collides with its own body
    let updatedSnake' = if any (`elem` fmap segmentPos (body updatedSnake)) (fmap segmentPos $ tail $ body updatedSnake)
                        then cutTail updatedSnake else updatedSnake
    -- Render updated game state
    render renderer font updatedFruits' updatedSnake'
    -- Repeat game loop with updated game state
    gameLoop window renderer font updatedFruits' updatedSnake'
```

Modify the function such way that new segment is added to the end of the snake with an offset when collision is detected.

Consider the additional types:
```
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

```