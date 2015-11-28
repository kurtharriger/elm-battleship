
import Html
import Random
import Time

initialSeed : Signal Random.Seed
initialSeed =
  Signal.map
    (\(time, _) -> Random.initialSeed (round time))
    (Time.timestamp (Signal.constant ()))


main : Signal Html.Html

-- main =
--   Signal.map
--     (Html.text << toString)
--     initialSeed

main =
  Signal.map
    (Html.text << toString)
    (Signal.foldp (\a s -> Just (Debug.log "action" a))
      Nothing
      initialSeed)
