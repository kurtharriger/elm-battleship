
import Html
import Random
import Time
import Signal.Extra exposing (foldp', fairMerge)
import Mouse

initialSeed : Signal Random.Seed
initialSeed =
  Signal.map
    (\(time, _) -> Random.initialSeed (round time))
    (Time.timestamp (Signal.constant ()))


type Action
  = Initialize Random.Seed
  | Mouse Bool

--
-- main =
--   Signal.map
--     (Html.text << toString)
--     initialSeed
--
-- main =
--    Signal.map
--      (Html.text << toString)
--     (Signal.foldp (\a s -> Just (Debug.log "action" a))
--         Nothing
--         initialSeed)
--
-- main =
--   Signal.map
--     (Html.text << toString)
--     (foldp' (\a s -> Just (Debug.log "action" a))
--       (Debug.log "initialize" >> Just)
--       (Signal.mergeMany [(Signal.map Mouse Mouse.isDown), (Signal.map Initialize initialSeed)]))
--
-- main =
--   Signal.map
--     (Html.text << toString)
--     (foldp' (\a s -> Just (Debug.log "action" a))
--       (Debug.log "initialize" >> Just)
--       (Signal.mergeMany [(Signal.map Initialize initialSeed), (Signal.map Mouse Mouse.isDown)]))
--
--
main =
  Signal.map
    (Html.text << toString)
    (foldp' (\a s -> Just (Debug.log "action" a))
      (Debug.log "initialize" >> Just)
      (Signal.map2 (,) (Signal.map Mouse Mouse.isDown)  (Signal.map Initialize initialSeed)))
