-- Haskell Pocket Cube (2x2 Rubik's Cube)
-- 
-- Written by Leonardo Ono (ono.leo80@gmail.com)
-- May 12, 2020
--
-- References: 
-- http://www.rubiksplace.com/cubes/2x2/
-- https://ruwix.com/the-rubiks-cube/notation/
-- http://hackage.haskell.org/package/gloss-1.13.1.1/docs/Graphics-Gloss-Interface-Pure-Game.html
-- http://hackage.haskell.org/package/gloss-1.13.1.1/docs/Graphics-Gloss-Data-Vector.html
-- http://hackage.haskell.org/package/gloss-1.13.1.1/docs/Graphics-Gloss-Data-Point-Arithmetic.html
-- http://hackage.haskell.org/package/gloss-1.13.0.1/docs/src/Graphics.Gloss.Data.Point.Arithmetic.html
-- https://hackage.haskell.org/package/linear-1.21/docs/
-- https://github.com/ekmett/linear/
--
-- GHC version 8.6.5
--
-- Dependencies:
-- gloss-1.13.1.1
-- linear-1.21

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as PA

import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Linear.Matrix
import Linear.Projection

import System.Random
import Data.Array.ST
import Control.Monad.ST
import Data.STRef
import Data.List
import Data.Foldable
import Data.Fixed
import Control.Monad
import Data.Maybe
import Data.Char

import Debug.Trace as Debug

cubePoints = [ -- front
               V4 (-1) (-1) 1 1
             , V4   1 ( -1) 1 1
             , V4   1    1  1 1
             , V4 (-1)   1  1 1
               -- back
             , V4 (-1) (-1) (-1) 1
             , V4   1  (-1) (-1) 1
             , V4   1    1  (-1) 1
             , V4 (-1)   1  (-1) 1 ]

-- all faces in CCW
cubeFaces = [ [0, 1, 2, 3]   -- front 
            , [4, 7, 6, 5]   -- back
            , [3, 2, 6, 7]   -- top
            , [4, 5, 1, 0]   -- bottom
            , [4, 0, 3, 7]   -- left
            , [1, 5, 6, 2] ] -- right

-- left, right, top, down, front, back, '=CCW
data Rotation = NONE | L | L' | R | R' | T | T' | D | D' | F | F' | B | B' deriving (Show, Eq)

-- (translation, [Rotation], [modelIndexFront,modelIndexBack,...,modelIndexRight]) 
cube0 = (V4 (-1)   1    1  0, [T, L, F, T', L', F'], [4, (-1), 2, (-1), 17, (-1)])   -- top  left  front
cube1 = (V4   1    1    1  0, [T, R, F, T', R', F'], [5, (-1), 3, (-1), (-1), 20])   -- top  right front
cube2 = (V4 (-1)   1  (-1) 0, [T, L, B, T', L', B'], [(-1), 14, 0, (-1), 16, (-1)])  -- top  left  back
cube3 = (V4   1    1  (-1) 0, [T, R, B, T', R', B'], [(-1), 15, 1, (-1), (-1), 21])  -- top  right back
cube4 = (V4 (-1) (-1)   1  0, [D, L, F, D', L', F'], [6, (-1), (-1), 8, 19, (-1)])   -- down left  front
cube5 = (V4   1  (-1)   1  0, [D, R, F, D', R', F'], [7, (-1), (-1), 9, (-1), 22])   -- down right front
cube6 = (V4 (-1) (-1) (-1) 0, [D, L, B, D', L', B'], [(-1), 12, (-1), 10, 18, (-1)]) -- down left  back
cube7 = (V4   1  (-1) (-1) 0, [D, R, B, D', R', B'], [(-1), 13, (-1), 11, (-1), 23]) -- down right back

magicCube = [cube0, cube1, cube2, cube3, cube4, cube5, cube6, cube7]

allVertices = join $ fmap v magicCube
    where v (translation, _, indices) = fmap (^+^ (1.05 * translation)) cubePoints

allFaces = join $ fmap f [0..7]
    where f i = zip indices $ fmap (\xs -> fmap (+(i*8)) xs) cubeFaces
            where (_, _, indices) = magicCube !! i

rotationsList = zip [0..] (fmap rot magicCube) 
    where rot (_, rotation, _) = rotation

affectedRotationVertices :: Rotation -> [Int]
affectedRotationVertices r = join $ fmap (fixIndices . \(i, _) -> i) 
                                  $ filter (\(i,xs) -> r `elem` xs) rotationsList
    where fixIndices i = fmap (+(i*8)) [0..7]

data State = SOLVING | WAITING_CLICK | ROTATING_VIEW | WAITING_SECOND_POINT | RELEASED | ANIMATING deriving (Show, Eq)

data GameState = GameState { getState :: State
                           , solved :: Bool 
                           , clickedPosition :: (Float, Float)
                           , viewRotationOriginal :: (Float, Float) -- accumulated angle in rad (rotationX, rotationY)
                           , viewRotation :: (Float, Float) -- accumulated angle in rad (rotationX, rotationY)
                           , rotation :: Rotation
                           , rotationProgress :: Float -- 0.0 ~ 1.0
                           , firstPoint :: Char

                             --         0         1         2
                             --         012345678901234567890123
                             --         abcdefghijklmnopqrstuvwx
                             --         ttttffffddddbbbbllllrrrr
                             --model = "wwwwbbbbyyyyggggrrrroooo"
                           , model :: [Char]
                           , randomGen :: StdGen }   

-- position target upVector

position :: V3 Float
position = V3 0 0 35

target :: V3 Float
target = V3 0 0 0

upVector :: V3 Float
upVector = V3 0 1 0

view = lookAt position target upVector

-- https://stackoverflow.com/questions/3982418/how-to-represent-a-4x4-matrix-rotation
rotationX angle = V4 (V4 1 0 0 0) (V4 0 (cos angle) (-sin angle) 0) (V4 0 (sin angle) (cos angle) 0) (V4 0 0 0 1)
rotationY angle = V4 (V4 (cos angle) 0 (sin angle) 0) (V4 0 1 0 0) (V4 (-sin angle) 0 (cos angle) 0) (V4 0 0 0 1)
rotationZ angle = V4 (V4 (cos angle) (-sin angle) 0 0) (V4 (sin angle) (cos angle) 0 0) (V4 0 0 1 0) (V4 0 0 0 1)

-- fov aspectRatio near far (near & far always positive)
projection = perspective fov (4/3) 5 100
    where fov = 0.785398 -- = 45 degrees

mvp state angleScale = (projection !*! view !*! modelRotation !*! (rot rotation') !*)
    where   rot T' = rotationY angle
            rot T = rotationY (-angle)
            rot D = rotationY angle
            rot D' = rotationY (-angle)
            rot L = rotationX angle
            rot L' = rotationX (-angle)
            rot R' = rotationX angle
            rot R = rotationX (-angle)
            rot F' = rotationZ angle
            rot F = rotationZ (-angle)
            rot B = rotationZ angle
            rot B' = rotationZ (-angle)
            rot NONE = identity
            rotationProgress' = rotationProgress state
            rotation' = rotation state
            angle = angleScale * rotationProgress' * (pi / 2)
            (viewRotX, viewRotY) = viewRotation state
            modelRotation = rotationX viewRotY !*! rotationY (-viewRotX)

-- perspective division and screen transformation
screenTransformation (V4 x y z w) = (800 * x / w, 600 * y / w)

perspectivePoints state = fmap rot $ zip [0..] allVertices
    where   rot (index, vertex) = if index `elem` (affectedRotationVertices $ rotation state)
                                  then mvp state 1 $ vertex
                                  else mvp state 0 $ vertex
            

screenPoints state = fmap screenTransformation (perspectivePoints state)

toColor n model
    | n < 0     = black
    | otherwise = c (model !! n)
        where   c :: Char -> Color
                c 'w' = white
                c 'b' = blue
                c 'y' = yellow
                c 'g' = green
                c 'r' = red
                c 'o' = orange

cubePicture state = fmap toPicture allFaces 
    where toPicture (index, vertices) = color (toColor index model') 
                                      $ polygon $ fmap (screenPoints state !!) vertices
          model' = model state
          firstPoint' = firstPoint state

sortedFaces state = reverse $ fmap snd $ sort $ zip (fmap f allFaces) [0..]
    where   f (index, vertices) = sum $ fmap (\i -> z $ (perspectivePoints state) !! i) vertices
            z (V4 x y z w) = z

render state = pictures $ fmap sorted (sortedFaces state) ++ shuffleMsg ++ solvedMsg
    where   sorted z = (cubePicture state) !! z
            shuffleMsg = [color white $ translate (-350) (250) $ scale 0.25 0.25 
                                      $ text "'Space' key = scramble the cube"] 
            solvedMsg | solved state = [color white $ translate (-350) (200) $ scale 0.25 0.25 
                                                    $ text "Cube solved. Congratulations :) !"] 
                      | otherwise = []

rotateModel :: Rotation -> [Char] -> [Char]
rotateModel move [ a, b, c, d , e, f, g, h , i, j, k, l , m, n, o, p , q, r, s, t , u, v, w, x ] = 
    case move of    
        NONE -> [ a, b, c, d , e, f, g, h , i, j, k, l , m, n, o, p , q, r, s, t , u, v, w, x ]
        T    -> [ c, a, d, b , u, v, g, h , i, j, k, l , m, n, r, q , e, f, s, t , p, o, w, x ]
        D    -> [ a, b, c, d , e, f, s, t , k, i, l, j , x, w, o, p , q, r, n, m , u, v, g, h ]
        F    -> [ a, b, t, r , g, e, h, f , w, u, k, l , m, n, o, p , q, i, s, j , c, v, d, x ]
        B    -> [ v, x, c, d , e, f, g, h , i, j, q, s , o, m, p, n , b, r, a, t , u, l, w, k ]
        R    -> [ a, f, c, h , e, j, g, l , i, n, k, p , m, b, o, d , q, r, s, t , w, u, x, v ]
        L    -> [ m, b, o, d , a, f, c, h , e, j, g, l , i, n, k, p , s, q, t, r , u, v, w, x ]
        T'   -> [ b, d, a, c , q, r, g, h , i, j, k, l , m, n, v, u , p, o, s, t , e, f, w, x ] 
        D'   -> [ a, b, c, d , e, f, w, x , j, l, i, k , t, s, o, p , q, r, g, h , u, v, n, m ]
        F'   -> [ a, b, u, w , f, h, e, g , r, t, k, l , m, n, o, p , q, d, s, c , j, v, i, x ]
        B'   -> [ s, q, c, d , e, f, g, h , i, j, x, v , n, p, m, o , k, r, l, t , u, a, w, b ]
        R'   -> [ a, n, c, p , e, b, g, d , i, f, k, h , m, j, o, l , q, r, s, t , v, x, u, w ]
        L'   -> [ e, b, g, d , i, f, k, h , m, j, o, l , a, n, c, p , r, t, q, s , u, v, w, x ]

initialGameState = GameState    { getState = WAITING_CLICK
                                , solved = False
                                , clickedPosition = (0,0)
                                , viewRotationOriginal = (0.5, 0.5)
                                , viewRotation = (0.5, 0.5)
                                , rotation = T
                                , rotationProgress = 0
                                , firstPoint = '_'
                                , model = "wwwwbbbbyyyyggggrrrroooo"
                                -- TODO set date as initial random seed later
                                , randomGen = mkStdGen 100 }   

selectedFace state point = fromMaybe '_' $ fmap (chr.(+97).fst) 
                                         $ find (selected point) 
                                         $ fmap toScreen 
                                         $ filter ((&&) <$> removeBlack <*> backfaceCulling) allFaces
    where   removeBlack (i, _) = i >= 0
            screenPoints' = screenPoints state

            backfaceCulling (c, [i0, i1, i2, i3]) = 
                let pa = uncurry V2 $ screenPoints' !! i0
                    pb = uncurry V2 $ screenPoints' !! i1
                    pc = uncurry V2 $ screenPoints' !! i2
                    pd = uncurry V2 $ screenPoints' !! i2
                    v1 = pb ^-^ pa
                    v2 = pb ^-^ pc
                    cross = crossZ v2 v1
                in  cross > 0

            toScreen (index, pointIndices) = (index, fmap (\i -> screenPoints' !! i) pointIndices)
            selected p (index, polygonPoints) = pointInsidePolygon p polygonPoints

pointInsidePolygon p' polygonPoints = and $ fmap (>0) $ zipWith crossZ l3 l4
    where   p = uncurry V2 p'
            l1@(x:xs) = fmap (uncurry V2) polygonPoints
            l2 = xs ++ [x]
            l3 = zipWith (^-^) l2 l1
            l4 = zipWith (^-^) (repeat p) l1     

getRotationOrientation c1 c2 = rotations !! (fromMaybe 0 $ findIndex (>=0) check)
    where front  = "cduwjitrc"
          front' = reverse front
          back'  = "abvxlksqa"
          back   = reverse back'
          top'   = "efuvpoqre"
          top    = reverse top'
          down   = "ghwxnmstg"
          down'  = reverse down
          left   = "acegikmoa"
          left'  = reverse left
          right' = "bdfhjlnpb"
          right  = reverse right'
          possibilities = ["", front, front', back, back', top, top', down, down', left, left', right, right']
          rotations = [NONE,F,F',B,B',T,T',D,D',L,L',R,R']  
          check = let k list = fromMaybe (-1) $ findString [c1,c2] list
                  in  fmap k possibilities

-- https://stackoverflow.com/questions/48198144/how-do-i-search-for-string-within-a-string-in-haskell
findString search str = findIndex (isPrefixOf search) (tails str)

-- cube solved ?
cubeSolved model = map length (group model) == [4,4,4,4,4,4]

-- https://wiki.haskell.org/Random_shuffle
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: StdGen -> [a] -> ([a],StdGen)
shuffle gen xs = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

-- https://stackoverflow.com/questions/4342013/the-composition-of-functions-in-a-list-of-functions
scramble stdGen model = (newStdGen, composedRotations model)
    where (shuffledRotations,newStdGen) = (shuffle stdGen) $ join $ replicate 100 [F,F',B,B',T,T',D,D',L,L',R,R'] 
          shuffledRotationsFunctions = fmap rotateModel shuffledRotations
          composedRotations = foldl (.) id shuffledRotationsFunctions

main2 = print $ scramble (mkStdGen 100) "wwwwbbbbyyyyggggrrrroooo"

main :: IO ()
main = play window background fps initialGameState render handleInput update
    where   window = InWindow "Haskell Pocket Cube (2x2 Rubik's Cube)" (800, 600) (50, 50)
            background = azure
            fps = 60

startRotation state newRotation = if (getState state) /= ANIMATING && (newRotation /= NONE)
                                  then state { getState = ANIMATING
                                             , rotationProgress = 0
                                             , rotation = newRotation }
                                  else state { getState = WAITING_CLICK
                                             , rotationProgress = 0 }

handleInput :: Event -> GameState -> GameState

handleInput (EventKey (SpecialKey KeySpace) Down _ _) state = state { model = scrambledModel
                                                                    , randomGen = newRandomGen
                                                                    , solved = False }
    where   randomGen' = randomGen state
            (newRandomGen, scrambledModel) = scramble randomGen' (model state)

handleInput (EventKey (Char 'r') Down _ _) state = startRotation state T
handleInput (EventKey (Char 't') Down _ _) state = startRotation state T'
handleInput (EventKey (Char 'v') Down _ _) state = startRotation state D'
handleInput (EventKey (Char 'b') Down _ _) state = startRotation state D
handleInput (EventKey (Char 'a') Down _ _) state = startRotation state L'
handleInput (EventKey (Char 's') Down _ _) state = startRotation state L
handleInput (EventKey (Char 'd') Down _ _) state = startRotation state F'
handleInput (EventKey (Char 'f') Down _ _) state = startRotation state F
handleInput (EventKey (Char 'g') Down _ _) state = startRotation state B
handleInput (EventKey (Char 'h') Down _ _) state = startRotation state B'
handleInput (EventKey (Char 'i') Down _ _) state = startRotation state R'
handleInput (EventKey (Char 'j') Down _ _) state = startRotation state R

handleInput (EventKey (MouseButton LeftButton) Down _ m) state
    | getState state == WAITING_CLICK = Debug.trace ("left click" ++ show (selectedFace state m)) 
                                            $ state { getState = WAITING_SECOND_POINT
                                                    , firstPoint = selectedFace state m
                                                    , clickedPosition = m }
    | otherwise = state

handleInput (EventKey (MouseButton RightButton) Down _ m) state 
    | getState state == WAITING_CLICK = Debug.trace (show m) $ state { getState = ROTATING_VIEW
                                            , clickedPosition = m }
    | otherwise = state

handleInput (EventMotion m) state
    | getState state == ROTATING_VIEW = state { viewRotation = (newViewRotationX, newViewRotationY) } 
    | otherwise = state
        where   clickedPosition' = clickedPosition state
                viewRotationOriginal' = viewRotationOriginal state
                dif = 0.01 PA.* (clickedPosition' PA.- m)
                (newViewRotationX, newViewRotationY) = viewRotationOriginal' PA.+ dif

handleInput (EventKey (MouseButton LeftButton) Up _ m) state 
    | getState state == WAITING_SECOND_POINT = Debug.trace ("released click " ++ show (selectedFace state m)) 
                                                $ startRotation state (getRotationOrientation 
                                                                      (firstPoint state) 
                                                                      (selectedFace state m)) 
    | otherwise = state

handleInput (EventKey (MouseButton RightButton) Up _ m) state 
    | getState state == ROTATING_VIEW =  state { getState = WAITING_CLICK
                                                , viewRotationOriginal = viewRotation state }
    | otherwise = state

handleInput _ state = state


update time state = case (getState state) of 
                        ANIMATING -> if newRotationProgress >= 1
                                     then state { getState = WAITING_CLICK
                                                , rotationProgress = 0
                                                , model = newModel
                                                , solved = cubeSolved newModel }
                                     else state { rotationProgress = newRotationProgress }
                        _ -> state
    where   newRotationProgress = rotationProgress state + 0.025
            newModel = rotateModel (rotation state) (model state)


