import Data.Map
import Data.Sequence
--import Data.Time.Clock

import Doodle
import Basic
import DoodleList


data DoodlePool keyType doodle = DoodlePool keyType (Map keyType doodle) 

instance Pool DoodlePool where
    freshKey (DoodlePool lastKey doodles) = succ $ lastKey
    get indx (DoodlePool lastKey doodles) = Data.Map.lookup indx doodles
    set indx doodle (DoodlePool lastKey doodles) = DoodlePool lastKey $ Data.Map.insert indx doodle doodles

emptyDoodle = (DoodlePool 0 Data.Map.empty)::(DoodlePool Int (DoodleList Int)) 

main :: IO()
main = run emptyDoodle