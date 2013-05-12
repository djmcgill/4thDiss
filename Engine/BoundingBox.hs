{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}

module Engine.BoundingBox where

import           Control.Applicative ((<$>))
import           Control.Monad (join)
import           Control.Lens
import           Data.List
import qualified Data.IntMap as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Numeric.LinearAlgebra hiding ((<>))

import Engine.RigidBody
import Engine.Objects

type ID = Int
type Interval      = (ID, Double, Double)
type IntervalAxis  = [(Double, IntervalPoint)]
data IntervalPoint = Start ID | End ID
    deriving (Eq, Ord)

-- | Given a list of objects, return the pairs of collisions and
--   and remaining non-collided ones.
onBoxCollisions :: (Object -> Object -> (Object,Object)) -> [Object] -> [Object]
onBoxCollisions processCollision objects = M.elems $ foldl' actOnCollision objectIDs collisions
    where
    objectIDs = M.fromList $ zip [1 ..] objects
    (xIntervals, yIntervals, zIntervals) = splitIntervals . M.elems $ M.mapWithKey getObjectIntervals objectIDs

    splitIntervals :: [(Interval, Interval, Interval)] -> ([Interval], [Interval], [Interval])
    splitIntervals = foldl' (\(xs,ys,zs) (x,y,z) -> (x:xs,y:ys,z:zs)) ([],[],[])

    collisions = find1DCollisions xIntervals
              <> find1DCollisions yIntervals
              <> find1DCollisions zIntervals
    actOnCollision :: M.IntMap Object -> (ID,ID) -> M.IntMap Object
    actOnCollision objects' (id1, id2) = M.insert id1 obj1' . M.insert id2 obj2' $ objects'
        where
        (obj1', obj2') = processCollision (objectIDs M.! id1) (objectIDs M.! id2)

    getObjectIntervals :: ID -> Object -> (Interval, Interval, Interval)
    getObjectIntervals id object
        | isSphere object = let Sphere r0       = object^.basicObject in makeCuboidIntervals r0 r0 r0
        | isCuboid object = let Cuboid x0 y0 z0 = object^.basicObject in makeCuboidIntervals x0 y0 z0
        where
        makeCuboidIntervals :: Double -> Double -> Double -> (Interval, Interval, Interval)
        makeCuboidIntervals x0 y0 z0 =
            let centre    = object^.body.x
                xInterval = (id, centre@>0 - (x0/2), centre@>0 - (x0/2))
                yInterval = (id, centre@>1 - (y0/2), centre@>1 - (y0/2))
                zInterval = (id, centre@>2 - (z0/2), centre@>2 - (z0/2))
            in (xInterval, yInterval, zInterval)

-- note that 'all (\(a,b) -> a < b) == True' for the output
find1DCollisions :: [Interval] -> [(ID, ID)]
find1DCollisions = findOverlaps . combineIntervals

-- | Given a list of interval start and end positions, return them in a sorted list
combineIntervals :: [Interval] -> IntervalAxis
combineIntervals = sort . concatMap (\(id, start, end) -> [(start, Start id), (end, End id)])

-- | Given an IntervalAxis, return the IDs of the intervals that overlap on it
findOverlaps :: IntervalAxis -> [(ID,ID)]
findOverlaps = snd . foldl' processInterval (S.empty, [])
    where
    -- | Maintains a set of the currently active intervals and a list of the prevously found collisions
    processInterval :: (S.Set ID, [(ID,ID)]) -> (Double, IntervalPoint) -> (S.Set ID, [(ID,ID)])
    -- an interval starts, so it collides with all of the currently active intervals then gets added to the active set
    processInterval (active, found) (_, Start id) = (S.insert id active, map (,id) (S.toList active) ++ found) -- collide them here
    -- an interval ends, so remove it from the active set
    processInterval (active, found) (_, End   id) = (S.delete id active, found)
