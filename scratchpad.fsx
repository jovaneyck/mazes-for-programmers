#load "draw.fsx"
open Maze

let m = 
   Maze.initMaze 10 10
   |> Maze.update (4,4) East NoWall
   |> Maze.update (4,4) South NoWall

m
|> Draw.toLines
|> Draw.draw