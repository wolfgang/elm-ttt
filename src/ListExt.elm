module ListExt exposing (nth)

nth : Int -> List a -> a
nth n list default = 
    let rest = List.drop n list 
    in
        case List.head rest of
            Nothing -> default
            Just x -> x
