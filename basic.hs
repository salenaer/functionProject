module Basic (delete, setList, makeLine) where

delete :: [a]->Int->[a]
delete xs n = 
    let (ys,zs) = splitAt n xs  in   ys ++ (tail zs)

setList::[a]->Int->a->[a]
setList (a:as) 0 value=
    value:as
setList (a:b) n value=
    a:(setList b (n-1) value)

--List of ints are the number of fillchars between two stopChars
makeLine::Char->Char->[Int]->String
makeLine stopChar fillChar fillers =
    '+':foldr (\x y->x++'+':y) [] [Prelude.replicate x fillChar | x <-fillers]