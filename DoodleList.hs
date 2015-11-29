module DoodleList (DoodleList, DoodleTimeSlot) where

import Data.Sequence

import Doodle
import Basic

data DoodleTimeSlot timeType = DoodleTimeSlot timeType timeType [String]

--if exists remove otherwise insert
toggleNameInNames::String->[String]->[String]
toggleNameInNames toToggle [] = [toToggle]
toggleNameInNames toToggle namesList@(name:names)
    | toToggle == name = names
    | name < toToggle = name : toggleNameInNames toToggle names
    | otherwise = toToggle : namesList

toggleNameInSlot::String -> DoodleTimeSlot t -> DoodleTimeSlot t
toggleNameInSlot name (DoodleTimeSlot startTime endTime names) = 
    DoodleTimeSlot startTime endTime $ toggleNameInNames name names

instance Show (DoodleTimeSlot t) where
    show (DoodleTimeSlot begin end names) =
        "| "++ show begin ++ " | " ++ show end ++ " | " ++ unwords names++" |\n"

data DoodleList timeType = DoodleList String [DoodleTimeSlot timeType] --deriving (Show)

addTimeSlot::Ord t=>(t,t)->[DoodleTimeSlot t]->[DoodleTimeSlot t]    
addTimeSlot (startTime, endTime) [] = [DoodleTimeSlot startTime endTime []]
addTimeSlot (newStartTime, newEndTime) slots@(slot@(DoodleTimeSlot startTime endTime names):restSlots)
    | endTime <= newStartTime = slot : (addTimeSlot (newStartTime, newEndTime) restSlots)
    | newEndTime <= endTime = DoodleTimeSlot newStartTime newEndTime [] : slots
    | otherwise = error "overlapping time slots"

toggleName :: String -> Int -> [DoodleTimeSlot t] -> [DoodleTimeSlot t]
toggleName name 0 (timeSlot:timeSlots) = toggleNameInSlot name timeSlot : timeSlots 
toggleName name n (timeSlot:timeSlots) = timeSlot : toggleName name (n-1) timeSlots

--calculates the amount of space necesairy to print all names
length :: Show t => DoodleList t-> Int
length (DoodleList name slots) = 
    foldr max 0 [Prelude.length $ unwords names|DoodleTimeSlot _ _ names <-slots]

instance Doodle DoodleList where
    initialize name = DoodleList name []
    add (startTime, endTime) (DoodleList name slots) = DoodleList name $ addTimeSlot (startTime, endTime) slots
    remove n (DoodleList name slots) = DoodleList name $ delete slots n
    toogle name n (DoodleList doodleName slots) = DoodleList doodleName $ toggleName name n slots

{-
instance Show (DoodleList a) where
    --show needs to return one large string
    --unlines [String]->String (seperated with newline characters)
    --UTFTime length = 26 characters

    show doodle@(DoodleList name slots)=
        let size = Prelude.length name + 2 in
        unlines $ [makeLine '+' '-' [size],
                   "| " ++ name ++" |", --print doodle tittle
                   makeLine '+' '-' [size]
                  ] ++ testprint(doodle)
-}

createHeader::DoodleList t -> String
createHeader (DoodleList name slots) = 
    let size = Prelude.length name + 2 in
    unlines $   [   makeLine '+' '-' [size]
                ,   "| " ++ name ++" |" --print doodle tittle
                ]
{-
testprint :: (Show t)=>DoodleList t -> Int-> [String]
testprint (DoodleList name slots) size = do
    xs<-map (\slot-> show slot) slots
    ys<-map (\x -> makeLine '+' '-' [size] ++ "\n" ++ x) [xs]
    zs<-makeLine '+' '-' [size]
    return [ys]
-}

testSlot = [(DoodleTimeSlot 1 3 ["John", "Marry", "Alice"]), (DoodleTimeSlot 7 9 ["John", "Marry"])]
testDoodle = initialize "MyDoodle" :: DoodleList Int
filledDoodle = toogle "John" 1 $ toogle "Mary" 0 $ toogle "John" 0 $ add (7, 9) $ add(1,5) testDoodle