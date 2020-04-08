#load "draw.fsx"
#load "random.fsx"
open Maze
open Random

let x =
    random {
        let! addend = Rand.IntRange 1 3 
        let! xs = Rand.listOf 10 (Rand.IntRange 2 10)
        let adds = xs |> List.map (fun x -> x + addend)
        return adds
    } 
    |> Rand.runSeed 1337

let m = 
   Maze.initMaze 10 10
   |> Maze.update (4,4) East NoWall
   |> Maze.update (4,4) South NoWall

m
|> Draw.toLines
|> Draw.draw