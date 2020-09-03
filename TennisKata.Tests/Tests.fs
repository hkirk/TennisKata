module Tests

open TennisKata.Tennis
open Xunit
open Swensen.Unquote

[<Fact>]
let ``playerWithScore should increase winners score with one`` () =
    test <@ increasePoint {id = 1; point = 0} = {id = 1; point = 0} @>

[<Fact>]   
let ``addPoint should find the correct player and add point`` () =
    test <@ addPoint ({id = 1; point = 0}, {id = 2; point = 0}) 2 = ({id = 1; point = 0}, {id = 2; point = 1}) @>

