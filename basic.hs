module Basic (delete, setList, makeSimpleLine, makeLine, makeTitle) where

delete :: [a]->Int->[a]
delete xs n = 
    let (ys,zs) = splitAt n xs  in   ys ++ (tail zs)

setList::[a]->Int->a->[a]
setList (a:as) 0 value=
    value:as
setList (a:b) n value=
    a:(setList b (n-1) value)

makeSimpleLine::Int->String
makeSimpleLine 0 = 
    createPlusAndMinus [51]
makeSimpleLine size = 
    createPlusAndMinus [54+size]
makeLine::Int->String
makeLine 0 =
    createPlusAndMinus [25,25]
makeLine x =
    createPlusAndMinus [25,25,x+2]
makeTitle::String->Int->String
makeTitle name 0 =
    "| " ++ name ++ Prelude.replicate (49 - Prelude.length name) ' ' ++" |\n" --print doodle tittle
makeTitle name x =
    "| " ++ name ++ Prelude.replicate (52 - Prelude.length name + x) ' ' ++" |\n" --print doodle tittle
createPlusAndMinus::[Int]->String
createPlusAndMinus list = 
    '+':foldr (\x y->x++'+':y) [] [Prelude.replicate x '-' | x <- list]