import Data.Map
import Data.Time

import Doodle
import Basic
import DoodleList


data DoodlePool keyType doodle = DoodlePool keyType (Map keyType doodle) 

instance Pool DoodlePool where
    freshKey (DoodlePool lastKey doodles) = succ $ lastKey
    get indx (DoodlePool lastKey doodles) = Data.Map.lookup indx doodles
    set indx doodle (DoodlePool lastKey doodles) = DoodlePool lastKey $ Data.Map.insert indx doodle doodles

emptyDoodle = (DoodlePool 0 Data.Map.empty)::(DoodlePool Int (DoodleList Data.Time.UTCTime)) 

main :: IO()
main = run emptyDoodle

--create => Left "name"
--add slot => Just (Right (2015-12-25 02:00:00, 2015-12-25 10:00:00))
--stop slots => Nothing
--toevoegen aan doodle => Right indx
--naam toevoegen = Just idx of slot
--klaar = Nothing