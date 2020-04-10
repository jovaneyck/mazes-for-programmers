#load "draw.fsx"
#load "random.fsx"
open Maze
open Random

module Rand = 
    let Edge : Rand<Edge> = Rand.Bool |> State.map (function true -> Wall | _ -> NoWall)

let x =
    random {
        let! addend = Rand.IntRange 1 3 
        let! xs = Rand.listOf 10 (Rand.IntRange 2 10)
        let adds = xs |> List.map (fun x -> x + addend)
        return adds
    } 
    |> Rand.runSeed 1337

let locationsIn (m : Maze) = 
    m |> Map.toSeq |> Seq.map fst

let boundaries (m : Maze) = 
    let locs = locationsIn m
    (locs |> Seq.map fst |> Seq.max,locs |> Seq.map snd |> Seq.max)

let binaryTreeCell boundary location : Rand<Cell> =   
    let (bx,by) = boundary
    let (x,y) = location
        
    if x = bx && y = by then 
        Rand.unit <| Map.ofList [ (East, Wall); (South, Wall)] 
    elif x = bx then 
        Rand.unit <| Map.ofList [ (East, Wall); (South, NoWall)] 
    elif y = by then
        Rand.unit <| Map.ofList [ (East, NoWall); (South, Wall)] 
    else
        random {
            let! openEast = Rand.Bool
            let east = if openEast then Wall else NoWall
            let south = if openEast then NoWall else Wall
            let cell : Cell = mkCell [(East, east); (South, south)]
            return cell
        }

let binaryTree m : Rand<Maze> = 
    let locations = locationsIn m
    let boundaries = boundaries m

    random {    
        let! cells = 
            locations 
            |> Seq.map (fun loc -> binaryTreeCell boundaries loc |> State.map (fun c -> loc,c)) 
            |> Seq.toList 
            |> State.sequenceList
        return Map.ofList cells
    }

Maze.initMaze 100 100
|> binaryTree 
|> Rand.run
|> Draw.toLines
|> Draw.draw