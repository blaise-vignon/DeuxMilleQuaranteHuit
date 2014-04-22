// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.



#if INTERACTIVE
#r @"C:\Users\blaisev\Documents\Visual Studio 2013\Projects\deuxmille\Grid\bin\Debug\Grid.dll"
#endif

open System
open System.IO

open Grid           

//let actionworked = ref false
//let test = [0;0;0;2]
//Grid.collapse actionworked test

let g2 = Grid.create

printgrid g2



let dirArray = [| Left  ; Right ; Up ; Down  ;|]
    
let possibilities = [for d in dirArray do 
                        let gout = collapsegrid g2 d
                        yield gout]
//
//let g3 =  Grid.collapsegrid  g2 Grid.Right |> snd
//
//let l = Grid.enumeratenext g3 2
//
//let rec power n =
//    match n with
//        | 0 -> 1
//        | n -> 2 * (power (n-1)) // no safeguard for neg numbers
//
//
//let gfinished = Array2D.init 4 4 (fun i j -> 2*((i+j)%2)+2)
//let gcomplex = Array2D.init 4 4 (fun i j -> 
//                                    let pow = ((i+j)/2)+1
//                                    power pow)
//
//
//Grid.collapsegrid  gcomplex Grid.Down
//
//gcomplex

               

[<EntryPoint>]
let main argv = 

    let g = Grid.create
    Grid.play g
    printfn "Press enter to exit"
    Console.ReadLine() |> ignore
    0 // return an integer exit code
