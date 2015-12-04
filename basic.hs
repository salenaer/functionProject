module Basic (delete, setList, makeSimpleLine, makeLine, makeTitle, insertComma) where

delete :: [a]->Int->[a]
delete xs n = 
    let (ys,zs) = splitAt n xs  in   ys ++ (tail zs)

setList::[a]->Int->a->[a]
setList (a:as) 0 value=
    value:as
setList (a:b) n value=
    a:(setList b (n-1) value)

dateLength :: Int
dateLength = 25
timeSlotLength :: Int
timeSlotLength = dateLength*2

--simple line is a line with only minus in the middle
makeSimpleLine::Int->String
makeSimpleLine 0 = 
    createPlusAndMinus [dateLength*2+1] --2 time slots plus one -
makeSimpleLine size = 
    createPlusAndMinus [timeSlotLength+size+4] --time slot '+' time slot '+' name + 2 whitespaces 

makeLine::Int->String
makeLine 0 =
    createPlusAndMinus [dateLength,dateLength]
makeLine x =
    createPlusAndMinus [dateLength,dateLength,x+2]

makeTitle::String->Int->String
makeTitle name 0 =
    "| " ++ name ++ Prelude.replicate (timeSlotLength - Prelude.length name - 1) ' ' ++" |\n" --print doodle tittle
makeTitle name x =
    "| " ++ name ++ Prelude.replicate (timeSlotLength - Prelude.length name + x + 2) ' ' ++" |\n" --print doodle tittle

createPlusAndMinus::[Int]->String
createPlusAndMinus list = 
    '+':foldr (\x y->x++'+':y) [] [Prelude.replicate x '-' | x <- list]

insertComma :: String->String->String
insertComma a [] = a
insertComma a b = a ++ ", " ++ b