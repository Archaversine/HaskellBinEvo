module Main (main) where

import Data.Word (Word32)
import Data.Bits (shiftL, shiftR, xor, complement, (.|.), (.&.))
import Data.List (sortBy)
import Data.Functor ((<&>))

import Control.Monad (replicateM)

import System.Random (randomRIO)

type Genome = Word32
type Population = [Genome]
type ScoredPopulation = [(Genome, Float)]

-- Variables

-- Number where only 20/32 bits are 1
-- Used to set max limit for random generation
mask :: Genome
mask = 1048575

population :: Int
population = 500

generations :: Int
generations = 500

iterations :: Int
iterations = 30

mutationChance :: Float
mutationChance = 0.001

-- Debugging Functions
-- Only prints 20 bits (because we are only interacting with 20)
printBits :: Genome -> IO ()
printBits w = do
    mapM_ (\n -> putStr . show $ 1 .&. (w `shiftR` n)) [19,18..0] 
    putStrLn $ " = " ++ show w

-- Genetic Algorithm Functions

genRandomGenome :: IO Genome
genRandomGenome = randomRIO (0, mask)

genInitialPopulation :: IO Population
genInitialPopulation = replicateM population genRandomGenome

cross :: Genome -> Genome -> IO Genome
cross p1 p2 = do
    crossPoint <- randomRIO (0, 20) :: IO Int
    let crossMask = complement 0 `shiftL` crossPoint

    return $ (p1 .&. crossMask) .|. (p2 .&. complement crossMask)

mutate :: Genome -> IO Genome
mutate g = go 20 $ return g
    where go 0 acc = acc
          go n acc = do
            r <- randomRIO (0, 1) :: IO Float
            if r > mutationChance then go (n - 1) acc
                else go (n - 1) $ acc <&> (`xor` (1 `shiftL` (n - 1)))

-- Cannot handle empty list as input
selectFromPopulation :: ScoredPopulation -> Float -> IO Genome
selectFromPopulation scored totalFitness = do
    r <- randomRIO (0, 1) :: IO Float
    return $ go (r * totalFitness) scored
        where go _ [] = undefined
              go _ [(x, _)] = x
              go dart ((x, p):xs)
                | dart > p = go dart xs
                | otherwise = x

reproduce :: Population -> IO Population
reproduce p = replicateM population $ do
    p1 <- selectFromPopulation scored totalFitness
    p2 <- selectFromPopulation scored totalFitness

    cross p1 p2 >>= mutate
        where summed = scanl1 (+) $ map fromIntegral p :: [Float]
              scored = zip p summed
              totalFitness = last summed

-- Returns best genome
runGA :: IO Genome
runGA = go generations genInitialPopulation
    where go 0 pop = pop <&> head -- Return first genome of population (list should be sorted by now)
          go n pop = go (n - 1) $ pop >>= reproduce <&> sortBy (flip compare)

main :: IO ()
main = do 
    putStrLn "--------------------------------------------"
    putStrLn $ "Population Size: " ++ show population
    putStrLn $ "Number of Generations: " ++ show generations
    putStrLn $ "Number of Iterations: " ++ show iterations
    putStrLn $ "Mutation per bit: " ++ show (mutationChance * 100) ++ "%"
    putStrLn "--------------------------------------------"

    results <- go iterations (pure [])

    let floats = map fromIntegral results :: [Float]
        mean = sum floats / fromIntegral iterations
        totalDiffSquared = sum $ map (\x -> (x - mean) ^ 2) floats
        deviation = sqrt $ totalDiffSquared / fromIntegral iterations

    putStrLn $ "Average: " ++ show mean
    putStrLn $ "Standard Deviation: " ++ show deviation

    where go 0 acc = acc
          go n acc = do
            best <- runGA
            putStr ("Iteration " ++ show i ++ ": ") >> printBits best
            go (n - 1) $ (best :) <$> acc
                where i = iterations - n + 1
