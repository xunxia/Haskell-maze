module Main where

import System.IO
import Data.List
import Data.Char

type Coord = (Int,Int,[Char])
type Map = [String]
type Path = [Coord]

inMap :: Map->Path->Path
inMap mapTR path@((x,y,_):_) = if ((x>=1 && x<=(length (mapTR !! y)-2))&&(y>=1 && y<=(length (mapTR)-2))) then
                                path
                                else delete (path!!0) path
                                       
isGoal :: Map->Path->Bool                          
isGoal mapTR (n:_) =  getSpot mapTR n == '@'                                
                            
getSpot :: Map->Coord->Char
getSpot mapTR (x,y,_) = (mapTR!!y)!!x

isWay:: Map-> Path->Path
isWay mapTR path@(n:_) = if getSpot mapTR n /='#' then
                                path
                                else delete (path!!0) path
                       
findN :: Map->Coord->Char
findN mapTR (x,y,_) = (mapTR!!(y-1))!!x

findE :: Map->Coord->Char
findE mapTR (x,y,_) = (mapTR!!y)!!(x+1)

findS :: Map->Coord->Char
findS mapTR (x,y,_) = (mapTR!!(y+1))!!x

findW :: Map->Coord->Char
findW mapTR (x,y,_) = (mapTR!!y)!!(x-1)

findPath :: Map->Path->Path
findPath mapTR path@(n@(x,y,_):_)  
        | (findS mapTR n /='#')&&(null (filter (getPathDot x (y+1)) path)) = (x,y+1," "):path        
        | (findE mapTR n /='#')&&(null (filter (getPathDot (x+1) y) path)) = (x+1,y," "):path                
        | (findW mapTR n /='#')&&(null (filter (getPathDot (x-1) y) path)) = (x-1,y," "):path
        | (findN mapTR n /='#')&&(null (filter (getPathDot x (y-1)) path)) = (x,y-1," "):path        
        | otherwise           = (tail path)++[(x,y,"DE")]

solve :: Map->Path->Path
solve mapTR p
        | isGoal mapTR p = p
        | getThird (filter (getPathDot 1 1) p) =="DE" = p
solve mapTR p = solve mapTR (findPath mapTR (isWay mapTR (inMap mapTR p)))
                             
--add a boundary of '#' for the map so that the index will not out of boundary
addBoundary :: Map->Map
addBoundary mapTR = 
        let newLine = replicate (length(mapTR!!0)+2) '#'
        in newLine:(map (\s->'#':s++"#") mapTR )++[newLine]


removeBoundary :: Map->Map
removeBoundary mapFI = 
        map (\s-> init(tail s)) (init(tail mapFI))

getThird [(_,_,z)] = z

getPathDot a b (x,y,_) = a==x&&b==y 
   
toShow mapTR path = lines (coorY mapTR 0)
        where
                coorY (yL:ys) y = coorX yL 0 y++"\n"++coorY ys (y+1)
                coorY [] y = []
                coorX (ch:chs) x y = showCh ch x y : coorX chs (x+1) y 
                coorX [] _ _ =[]
                showCh ch x y = 
                       if null (filter (getPathDot x y) path) 
                                then ch
                                else if getThird (filter (getPathDot x y) path) =="DE" 
                                        then '!'
                                        else if ((mapTR!!y)!!x) =='@'
                                                then '@'
                                                else '+'

showMap mapTR path = unlines (removeBoundary (toShow mapTR path))

warnShow ::Map->Path->String                                
warnShow mapTR path = 
        if (isGoal mapTR path) 
                then "Woo hoo, I found the treasure :-)\n"++showMap mapTR path
                else "Uh oh, I could not find the treasure :-(\n"++showMap mapTR path

checkColuLen mapTR = null(filter (\x -> length x /= length (mapTR!!0)) mapTR)

findTreasure mapTR = warnShow mapWithEdge (solve mapWithEdge [(1,1," ")])
        where mapWithEdge = addBoundary mapTR      
        
validCh mapTR = null (filter (\xs -> notValid xs> 0) mapTR)
        where notValid xs = foldl (\acc x-> if x `elem` ['#', '-', '@'] then acc else acc+1 ) 0 xs                
                                                                 
main = do
        putStrLn "This is my challenge:"
        handle <- openFile "map.txt" ReadMode
        contents <-hGetContents handle
        --the next line is used for delete the empty line in front of the map
        let m = dropWhile (\x-> (null x)||(all isSpace x)) (lines contents)
        putStrLn $ unlines m
        if null m
                then putStrLn "It is an empty map!!!" 
                else if not (checkColuLen m)
                        then putStrLn "The map is not valid: not every row has same number of column!!!"
                        else if m!!0!!0 == '#'
                                then putStrLn "The top left corner of the map is \'#\' , I cannot search from there!!!"
                                else if not (validCh m)
                                        then putStrLn "The map contains invalid characters!!!"
                                        else
                                                putStrLn $ findTreasure m 
        hClose handle