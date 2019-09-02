// 23.4.1
let silverInGold = 20
let copperInsilver = 12

let rec helperMinimize = function
 | (gold, silver, copper) when copper >= copperInsilver -> helperMinimize(gold, silver + copper/copperInsilver, copper % copperInsilver)
 | (gold, silver, copper) when silver >= silverInGold -> helperMinimize(gold + silver / silverInGold, silver % silverInGold, copper)
 | (gold, silver, copper) when silver >= silverInGold -> helperMinimize(gold + silver / silverInGold, silver % silverInGold, copper)
 | x -> x

let (.+.) (x: int * int * int) (y: int * int * int) =
 let xCopper, xSilver, xGold = x
 let yCopper, ySilver, yGold = y
 helperMinimize(xCopper+yCopper, xSilver+ySilver, xGold+yGold)

let (.-.) (x: int * int * int) (y: int * int * int) =
 let xCopper, xSilver, xGold = x
 let yCopper, ySilver, yGold = y
 helperMinimize(xCopper-yCopper, xSilver-ySilver, xGold-yGold)

// 23.4.2
let (.+) (x:  float * float) (y:  float * float) = 
 let re1, im1 = x
 let re2, im2 = y
 (re1 + re2, im1 + im2)

let (.-) (x:  float * float) (y:  float * float) =
 let re2, im2 = y
 x .+ (-re2, -im2)


let (.*) (x:  float * float) (y:  float * float) = 
 let re1, im1 = x
 let re2, im2 = y
 (re1 * re2 - im1 * im2, re1 * im2 + im1 * re2)

let (./) (x:  float * float) (y:  float * float) =
 let re2, im2 = y
 x .* (re2 / (re2 * re2 + im2 * im2), -im2 / (re2 * re2 + im2 * im2))