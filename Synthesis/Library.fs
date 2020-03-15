module Synthesis

let abelar x =
    ((x > 12) && (x < 3097) && (x % 12) = 0)

let area b h =
    match ((b >= 0.0) && (h >= 0.0)) with
    | true -> (0.5 * b * h)
    | false -> failwith "MatchFailException"


let zollo x =
    match (x < 0) with
    | true -> x * -1
    | false -> x * 2

let min x y =
    match (x < y) with
    | true -> x
    | false -> y

let max x y =
    match (x > y) with
    | true -> x
    | false -> y

let ofTime h m s =
    ((h * 3600) + (m * 60) + s)

let toTime timeM =
    match timeM >= 0 with
    | true -> ((timeM / 3600), (timeM % 3600)/60, (timeM % 3600)%60)
    | false-> (0, 0, 0)


let digits num =
         let rec dig num count =
             match ((num/10) = 0) with
             |true -> count
             |false -> (dig (num/10) (count + 1))
         dig num 1

let minmax n =
    let a, b, c, d = n
    min(min a b)(min c d), max(max a b)(max c d)

let isLeap y =
    match y < 1582 with
    |true -> failwith "fail"
    |false -> match (y % 4 = 0 && y % 100 <> 0) || (y % 400 = 0) with
              | true -> true
              | false -> false
   

let month num =
    match num with
    |1 -> ("January", 31)
    |2 -> ("February", 28)
    |3 -> ("March", 31)
    |4 -> ("April", 30)
    |5 -> ("May", 31)
    |6 -> ("June", 30)
    |7 -> ("July", 31)
    |8 -> ("August", 31)
    |9 -> ("September", 30)
    |10 -> ("October", 31)
    |11 -> ("November", 30)
    |12 -> ("December", 31)
    |_ -> failwith "fail"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz n =
    failwith "Not implemented"

let monthDay d y =
    match isLeap(y) && d > 0 && d <= 366 with
    |true -> match d <= 31 with
             |true -> "January"
             |false -> match d > 31 && d <= 59  with
                       | true -> "February"
                       | false -> match d > 59 && d <= 90 with
                                  | true -> "March"
                                  | false -> match d > 90 && d <= 120  with
                                             | true -> "April"
                                             | false -> match d > 334 && d <= 59 with
                                                        | true -> "November"
                                                        | false -> "December"
    | false -> failwith "fail"
    
             

let coord _ =
    failwith "Not implemented"
  