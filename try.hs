setList (a:b) 0 value=
    value:b
setList (a:b) n value=
    a:setList b (n-1) value 

delete :: [a]->Int->[a]
delete xs n = 
    let (ys,zs) = splitAt n xs  in   ys ++ (tail zs)

data DoodleTimeSlot timeType = DoodleTimeSlot
                            { begin :: timeType
                            , end   :: timeType
                            , names :: [String]
                            }

addName::String -> DoodleTimeSlot t -> DoodleTimeSlot t
addName name (DoodleTimeSlot startTime endTime names) = DoodleTimeSlot startTime endTime (name:names)