module DoodleList (DoodleList, DoodleTimeSlot) where

import Data.Sequence
import Data.Time.Clock

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

data DoodleList timeType = DoodleList String [DoodleTimeSlot timeType]

--add time slot is fout
addTimeSlot::Ord t=>(t,t)->[DoodleTimeSlot t]->[DoodleTimeSlot t]    
addTimeSlot (startTime, endTime) [] = [DoodleTimeSlot startTime endTime []]
addTimeSlot (newStartTime, newEndTime) slots@(slot@(DoodleTimeSlot startTime endTime names):restSlots)
    | endTime <= newStartTime = slot : (addTimeSlot (newStartTime, newEndTime) restSlots)
    | newEndTime < startTime = DoodleTimeSlot newStartTime newEndTime [] : slots
    | otherwise = slots

toggleName :: String -> Int -> [DoodleTimeSlot t] -> [DoodleTimeSlot t]
toggleName name 0 (timeSlot:timeSlots) = toggleNameInSlot name timeSlot : timeSlots 
toggleName name n (timeSlot:timeSlots) = timeSlot : toggleName name (n-1) timeSlots

instance Doodle DoodleList where
    initialize name = DoodleList name []
    add (startTime, endTime) (DoodleList name slots) = DoodleList name $ addTimeSlot (startTime, endTime) slots
    remove n (DoodleList name slots) = DoodleList name $ delete slots n
    toogle name n (DoodleList doodleName slots) = DoodleList doodleName $ toggleName name n slots

instance (Show a)=>Show (DoodleList a) where
    show doodle@(DoodleList name slots)=
        let size = DoodleList.length doodle in
            makeSimpleLine size ++ "\n" ++ makeTitle name size ++
            foldr (++) (makeLine size) (map (\x->makeLine size ++ "\n" ++ createSlotString x size) slots)

createSlotString :: (Show t)=>(DoodleTimeSlot t)->Int -> String
createSlotString (DoodleTimeSlot begin end _) 0 =
   "| "++ show begin ++ " | " ++ show end ++ " |\n"
createSlotString (DoodleTimeSlot begin end names) size =
   let sentence = unwords names in
   "| "++ show begin ++ " | " ++ show end ++ " | " ++ sentence ++ Prelude.replicate (size - Prelude.length sentence) ' ' ++" |\n"  

--calculates the amount of space necesairy to print all names
length :: (Show t) => DoodleList t-> Int
length (DoodleList name slots) = 
    foldr max 0 [Prelude.length $ unwords names|DoodleTimeSlot _ _ names <-slots]