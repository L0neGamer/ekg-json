{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Encoding of ekg metrics as JSON. The encoding defined by the
-- functions in this module are standardized and used by the ekg web
-- UI. The purpose of this module is to let other web servers and
-- frameworks than the one used by the ekg package expose ekg metrics.
module System.Metrics.Json
    ( -- * Converting metrics to JSON values
      sampleToJson
    , valueToJson

      -- ** Newtype wrappers with instances
    , Sample(..)
    , Value(..)
    ) where

import qualified Data.Vector as Vec
import Data.Aeson ((.=))
import qualified Data.Aeson.Encode as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import qualified Data.Text as T
import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution as Distribution

------------------------------------------------------------------------
-- * Converting metrics to JSON values


-- | Encode metrics as nested JSON objects. Each "." in the metric
-- name (i.e., the first tag) introduces a new level of nesting. For example,
-- the metrics @[("foo.bar":[], 10), ("foo.baz":["dim0:1","dim1:hello"], "label-abc"), ("foo.baz":["dim0:1","dim1:world"], "label-def") ]@ are
-- encoded as
--
-- > {
-- >   "foo": {
-- >     "bar": [
-- >       {
-- >         "type": "c",
-- >         "val": 10,
-- >         "dims": []
-- >       },
-- >     ],
-- >     "baz": [
-- >       {
-- >         "type": "l",
-- >         "val": "label-abc",
-- >         "dims": [
-- >           "dim0:1",
-- >           "dim1:hello"
-- >         ]
-- >       },
-- >       {
-- >         "type": "l",
-- >         "val": "label-def",
-- >         "dims": [
-- >           "dim0:1",
-- >           "dim1:world"
-- >         ]
-- >       }
-- >     ]
-- >   }
-- > }
--
sampleToJson :: Metrics.Sample -> A.Value
sampleToJson metrics =
    buildOne metrics $ A.emptyObject
  where
    buildOne :: M.HashMap Metrics.Tags Metrics.Value -> A.Value -> A.Value
    buildOne m o = M.foldlWithKey' build o m -- need to "group by" first tag

    build :: A.Value -> Metrics.Tags -> Metrics.Value -> A.Value
    build m tags val =
        go m (T.splitOn "." $ Metrics.name tags) (Metrics.dimensions tags) val

    go :: A.Value -> [T.Text] -> [T.Text] -> Metrics.Value -> A.Value
    go v@(A.Object m) [str] dims val    =
        A.Object $ M.alter f str m
      where
        f Nothing              = Just . A.Array $ Vec.fromList [valueToJson dims val]
        f (Just (A.Array arr)) = Just . A.Array $ Vec.cons (valueToJson dims val) arr

    go (A.Object m) (str:rest) dims val =
        A.Object $ M.alter f str m
      where
        f Nothing    = Just $ go A.emptyObject rest dims val
        f (Just obj) = Just $ go obj rest dims val

    go v _ _ _                      = typeMismatch "Object" v

typeMismatch :: String   -- ^ The expected type
             -> A.Value  -- ^ The actual value encountered
             -> a
typeMismatch expected actual =
    error $ "when expecting a " ++ expected ++ ", encountered " ++ name ++
    " instead"
  where
    name = case actual of
        A.Object _ -> "Object"
        A.Array _  -> "Array"
        A.String _ -> "String"
        A.Number _ -> "Number"
        A.Bool _   -> "Boolean"
        A.Null     -> "Null"

-- | Encodes a single metric as a JSON object. Example:
--
-- > {
-- >   "type": "c",
-- >   "val": 89460,
-- >   "dims": [
-- >     "some-dim"
-- >   ]
-- > }
--
valueToJson :: [T.Text] -> Metrics.Value -> A.Value
valueToJson dims = \case
    (Metrics.Counter n)      -> scalarToJson dims n CounterType
    (Metrics.Gauge n)        -> scalarToJson dims n GaugeType
    (Metrics.Label l)        -> scalarToJson dims l LabelType
    (Metrics.Distribution l) -> distrubtionToJson dims l

-- | Convert a scalar metric (i.e. counter, gauge, or label) to a JSON
-- value.
scalarToJson :: A.ToJSON a => [T.Text] -> a -> MetricType -> A.Value
scalarToJson dims val ty = A.object
    ["val" .= val, "type" .= metricType ty, "dims" .= dims]
{-# SPECIALIZE scalarToJson :: [T.Text] -> Int64 -> MetricType -> A.Value #-}
{-# SPECIALIZE scalarToJson :: [T.Text] -> T.Text -> MetricType -> A.Value #-}

data MetricType =
      CounterType
    | GaugeType
    | LabelType
    | DistributionType

metricType :: MetricType -> T.Text
metricType CounterType      = "c"
metricType GaugeType        = "g"
metricType LabelType        = "l"
metricType DistributionType = "d"

-- | Convert a distribution to a JSON value.
distrubtionToJson :: [T.Text] -> Distribution.Stats -> A.Value
distrubtionToJson dims stats = A.object
    [ "mean" .= Distribution.mean stats
    , "variance" .= Distribution.variance stats
    , "count" .= Distribution.count stats
    , "sum" .= Distribution.sum stats
    , "min" .= Distribution.min stats
    , "max" .= Distribution.max stats
    , "type" .= metricType DistributionType
    , "dims" .= dims
    ]

------------------------------------------------------------------------
-- ** Newtype wrappers with instances

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Sample' without creating an orphan instance.
newtype Sample = Sample Metrics.Sample
    deriving Show

-- | Uses 'sampleToJson'.
instance A.ToJSON Sample where
    toJSON (Sample s) = sampleToJson s

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Value' without creating an orphan instance.
newtype Value = Value Metrics.Value
    deriving Show

-- | Uses 'valueToJson'.
instance A.ToJSON Value where
    toJSON (Value v) = valueToJson [] v
