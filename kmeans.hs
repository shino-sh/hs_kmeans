import System.Random
import Control.Monad (replicateM)
import Data.List

type Vector  = [Float]
type Vectors = [Vector]
type Label   = Int

-- 2つのvectorのユークリッド距離を返す
euclid_dist :: Vector -> Vector -> Float
euclid_dist vector1 vector2 = sum (zipWith (\v1 v2 -> (v1-v2)**2) vector1 vector2)**0.5

-- vectorに対し、尤も近いラベルを返す
near :: Vector -> Vectors -> Label
near vector center_vectors = minimum_index distance
    where
        distance :: [Float]
        distance = map (\center_vector -> euclid_dist vector center_vector) center_vectors
        minimum_index :: [Float] -> Int
        minimum_index = (\list -> snd . minimum $ zip list [0 .. ])

-- K-meansの各ステップ
clustering :: Vectors -> Vectors -> [Label] -> Int -> Vectors
clustering vectors old_center_vectors old_label_vectors count
    | old_label_vectors == label_vectors = center_vectors
    | count == 0                         = center_vectors
    | otherwise = clustering vectors center_vectors label_vectors (count - 1)
    where
        labels :: [Label]
        labels = [0..(length old_center_vectors - 1)]
        vector_cluster :: [Vectors]
        vector_cluster = map (\l -> map fst $ filter (\t -> l == snd t) (zip vectors old_label_vectors)) labels
        -- 各クラスタの重心vectorの作成
        center_vectors :: Vectors
        center_vectors = zipWith (\v vc -> map (\e -> sum e / genericLength e) . transpose $ v:vc) old_center_vectors vector_cluster
        -- 各ベクトルのラベルの再割当て
        label_vectors :: [Label]
        label_vectors = map (\vector -> near vector center_vectors) vectors

main :: IO ()
main = do
    let datasize = 100
    let label_count = 3

    x <- replicateM datasize $ (getStdRandom $ randomR (0, 10) :: IO Float)
    y <- replicateM datasize $ (getStdRandom $ randomR (0, 10) :: IO Float)
    let vectors = zipWith (\x y -> [x, y]) x y

    init_label_vectors <- replicateM (length vectors) $ (getStdRandom $ randomR (0, label_count - 1) :: IO Int)
    let init_center_vectors = replicate label_count $ replicate (length $ vectors !! 0) 0 

    let centers = clustering vectors init_center_vectors init_label_vectors 100
    let labels = map (\v -> near v centers) vectors
    print $ zip vectors labels

