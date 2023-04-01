module Main (main) where

import Data.Word (Word32)
import Data.Bits (shiftL, shiftR, xor, complement, popCount, (.|.), (.&.))
import Data.List (sortBy)
import Data.Functor ((<&>))

import Control.Monad (replicateM)

import Graphics.Matplotlib

import System.Random (randomRIO)

type Genome = Word32
type Population = [Genome]
type ScoredPopulation = [(Genome, Double)]

type Point = (Int, Int)

data CrossOverType = SinglePoint | Uniform deriving (Eq, Show)

data GASettings = GASettings {
    mutationRate :: Double,
    crossoverRate :: Double,
    crossoverType :: CrossOverType,
    elitism :: Int,
    popSize :: Int,
    gens :: Int
} deriving Show

-- Variables

-- Number where only 20/32 bits are 1
-- Used to set max limit for random generation
mask :: Genome
mask = 1048575

-- Debugging Functions
-- Only prints 20 bits (because we are only interacting with 20)
printBits :: Genome -> IO ()
printBits w = do
    mapM_ (\n -> putStr . show $ 1 .&. (w `shiftR` n)) [19,18..0] 
    putStrLn $ " = " ++ show w

-- Genetic Algorithm Functions

genRandomGenome :: IO Genome
genRandomGenome = randomRIO (0, mask)

genInitialPopulation :: GASettings -> IO Population
genInitialPopulation settings = replicateM (popSize settings) genRandomGenome

fitness :: Genome -> Double
fitness = fromIntegral . popCount

cross :: GASettings -> Genome -> Genome -> IO Genome
cross settings p1 p2
    | crossoverType settings == SinglePoint = singlePointCross p1 p2
    | otherwise = uniformCross p1 p2

singlePointCross :: Genome -> Genome -> IO Genome
singlePointCross p1 p2 = do
    crossPoint <- randomRIO (0, 20) :: IO Int
    let crossMask = complement 0 `shiftL` crossPoint

    return $ (p1 .&. crossMask) .|. (p2 .&. complement crossMask)

uniformCross :: Genome -> Genome -> IO Genome
uniformCross p1 p2 = do
    m <- randomRIO (0, mask) :: IO Genome
    return $ (p1 .&. m) .|. (p2 .&. complement m)

mutate :: GASettings -> Genome -> IO Genome
mutate settings g = do
    r <- randomRIO (0, 1) :: IO Double
    point <- randomRIO (0, 10) :: IO Int

    if r > mutationRate settings then return g
        else return $ g `xor` (1 `shiftL` point)

-- Cannot handle empty list as input
selectFromPopulation :: ScoredPopulation -> Double -> IO Genome
selectFromPopulation scored totalFitness = do
    r <- randomRIO (0, 1) :: IO Double
    return $ go (r * totalFitness) scored
        where go _ [] = undefined
              go _ [(x, _)] = x
              go dart ((x, p):xs)
                | dart > p = go dart xs
                | otherwise = x

reproduce :: GASettings -> Population -> IO Population
reproduce settings p = do
    let elite = take (elitism settings) p
    nonElite <- reproduceNonElite settings p
    return $ elite ++ nonElite

reproduceNonElite :: GASettings -> Population -> IO Population
reproduceNonElite settings p = replicateM (popSize settings - elitism settings) $ do
    p1 <- selectFromPopulation scored totalFitness
    p2 <- selectFromPopulation scored totalFitness

    crossChance <- randomRIO (0, 1) :: IO Double

    if crossChance > crossoverRate settings then return p1
        else cross settings p1 p2 >>= mutate settings
    --cross settings p1 p2 >>= mutate settings
        where summed = scanl1 (+) $ map fitness p :: [Double]
              scored = zip p summed
              totalFitness = last summed

-- Returns best genome
runGA :: GASettings -> IO Genome
runGA settings = go (gens settings) (genInitialPopulation settings)
    where nextGen = reproduce settings
          go 0 pop = pop <&> head -- Return first genome of population (list should be sorted by now)
          go n pop = go (n - 1) $ pop >>= nextGen <&> sortBy (flip compare)

runUntilOptimal :: GASettings -> IO Int
runUntilOptimal settings = go (genInitialPopulation settings) 0
    where nextGen = reproduce settings
          go pop n = do
            candidates <- pop
            if head candidates /= mask then go (pop >>= nextGen <&> sortBy (flip compare)) (n + 1)
                else return n
    

visualize :: [[Point]] -> Matplotlib
visualize [] = undefined
visualize points = go points 0 $ title "Genetic Algorithm Experiments"
    where go :: [[Point]] -> Int -> Matplotlib -> Matplotlib
          go [] _ acc = acc
          go (pt:pts) n acc = go pts (n + 1) $ acc % line xs ys @@ [o2 "label" ("EXP " ++ show n)]
            where (xs, ys) = unzip pt

main :: IO ()
main = do
    let runs = 30

        test1 = GASettings { mutationRate = 0, 
                             crossoverRate = 1, 
                             crossoverType = SinglePoint, 
                             elitism = 25, 
                             popSize = 50,
                             gens = 0 }

        test2 = GASettings { mutationRate = 1, 
                             crossoverRate = 0.8, 
                             crossoverType = SinglePoint, 
                             elitism = 1, 
                             popSize = 50,
                             gens = 0 }

        test3 = GASettings { mutationRate = 0.01,
                             crossoverRate = 0.2,
                             crossoverType = SinglePoint,
                             elitism = 0,
                             popSize = 50,
                             gens = 0 }

        test4 = GASettings { mutationRate = 0.5,
                             crossoverRate = 0.8,
                             crossoverType = SinglePoint,
                             elitism = 1,
                             popSize = 50,
                             gens = 0 }

        test5 = GASettings { mutationRate = 0.1,
                             crossoverRate = 1,
                             crossoverType = Uniform,
                             elitism = 0,
                             popSize = 100,
                             gens = 0 }

    results1 <- replicateM runs $ runUntilOptimal test1
    putStrLn "Finished EXP 0"

    results2 <- replicateM runs $ runUntilOptimal test2
    putStrLn "Finished EXP 1"

    results3 <- replicateM runs $ runUntilOptimal test3
    putStrLn "Finished EXP 2"

    results4 <- replicateM runs $ runUntilOptimal test4 
    putStrLn "Finished EXP 3"

    results5 <- replicateM runs $ runUntilOptimal test5 
    putStrLn "Finished EXP 4"

    let points1 = zip [1..] results1
        points2 = zip [1..] results2
        points3 = zip [1..] results3
        points4 = zip [1..] results4
        points5 = zip [1..] results5

        img = visualize [points1, points2, points3, points4, points5]

    svg <- toSvg $ img % xlabel "Run #" 
                       % ylabel "Generations to Optimal"
                       % legend

    case svg of
        Left err -> putStrLn $ "Error: " ++ err
        Right s -> writeFile "visualization.svg" s

--main :: IO ()
--main = do 
--    putStrLn "--------------------------------------------"
--    putStrLn $ "Population Size: " ++ show population
--    putStrLn $ "Number of Generations: " ++ show generations
--    putStrLn $ "Number of Iterations: " ++ show iterations
--    putStrLn $ "Mutation per bit: " ++ show (mutationChance * 100) ++ "%"
--    putStrLn "--------------------------------------------"
--
--    results <- go iterations (pure [])
--
--    let floats = map fromIntegral results :: [Double]
--        mean = sum floats / fromIntegral iterations
--        totalDiffSquared = sum $ map (\x -> (x - mean) * (x - mean)) floats
--        deviation = sqrt $ totalDiffSquared / fromIntegral iterations
--
--    putStrLn "--------------------------------------------"
--    putStrLn $ "Minimum: " ++ show (minimum results)
--    putStrLn $ "Maximum: " ++ show (maximum results)
--    putStrLn $ "Average: " ++ show mean
--    putStrLn $ "Standard Deviation: " ++ show deviation
--    putStrLn "--------------------------------------------"
--
--    where go 0 acc = acc
--          go n acc = do
--            best <- runGA
--            putStr ("Iteration " ++ show i ++ ": ") >> printBits best
--            go (n - 1) $ (best :) <$> acc
--                where i = iterations - n + 1
