let evaluate mathExp =
  let regex = Str.regexp " +" in
  let operations : string list = Str.split regex mathExp in
  let expStack = Stack.create() in 
  ignore(Stack.push "" expStack);
  ignore(Stack.pop expStack);
  let regexNum = Str.regexp "[0-9]+.[0-9]*" in

  for i = 0 to ((List.length operations) - 1) do
    let op = ref (List.nth operations i) in
    let isNum = Str.string_match regexNum !op 0 in
    if isNum then Stack.push !op expStack
    else begin
      if ((Stack.length expStack) < 2) then (
        print_string "Invalid input syntax\n";
      ) else (
        let o2 = Stack.pop expStack in
        let o1 = Stack.pop expStack in
        let operand2 = float_of_string o2 in
        let operand1 = float_of_string o1 in
        let temp = ref 0.0 in

        if !op = "+" then temp := operand1 +. operand2
        else if !op = "-" then temp := operand1 -. operand2
        else if !op = "*" then temp := operand1 *. operand2      
        else if !op = "/" then temp := operand1 /. operand2
        else if !op = "^" then temp := operand1 ** operand2
        else temp := 0.0;
        let s = string_of_float !temp in
        Stack.push s expStack
      );
    end;
  done;

  let sol = ref 0.0 in
  if Stack.is_empty expStack then (
    print_string "Something went wrong!\n";
  ) else (
    let s = Stack.pop expStack in
    if (Stack.is_empty expStack) then (
      sol := float_of_string s;
      print_string "Valid!\t";
    ) else (
      print_string "The stack was not empty at the end.\n";
    );
  );
  !sol;;

(* Test 1*)
let numCorrect = ref 0;;
let exp = ref "3. 3. +";;
let ans = evaluate !exp;;
let expected = ref 6.;;
Printf.printf "Exp: %s should return %f\n" !exp !expected;; 
let pass = string_of_bool (ans = !expected);;
if (ans = !expected) then
  numCorrect := !numCorrect + 1;;
Printf.printf "%s\n" pass;;

(* Test 2 *)
let exp2 = "3. 3. 4. + ^";;
let ans2 = evaluate exp2;;
let expected2 = 2187.;;
Printf.printf "Exp: %s should return %f\n" exp2 expected2;; 
let pass2 = string_of_bool (ans2 = expected2);;
if (ans2 = expected2) then
  numCorrect := !numCorrect + 1;;
Printf.printf "%s\n" pass2;;

(* Test 3 *)
let exp3 = "3. 3. 4. + ^ 2000 -";;
let ans3 = evaluate exp3;;
let expected3 = 187.;;
Printf.printf "Exp: %s should return %f\n" exp3 expected3;; 
let pass3 = string_of_bool (ans3 = expected3);;
if (ans3 = expected3) then
  numCorrect := !numCorrect + 1;;
Printf.printf "%s\n" pass3;;

(* Test 4 *)
let exp4 = "3. 3. ^ 2000 3000 + -";;
let ans4 = evaluate exp4;;
let expected4 = -4973.;;
Printf.printf "Exp: %s should return %f\n" exp4 expected4;; 
let pass4 = string_of_bool (ans4 = expected4);;
if (ans4 = expected4) then
  numCorrect := !numCorrect + 1;;
Printf.printf "%s\n" pass4;;

(* Test 5 *)
let exp5 = "3. 3. / 2000 3000 + -";;
let ans5 = evaluate exp5;;
let expected5 = -4999.;;
Printf.printf "Exp: %s should return %f\n" exp5 expected5;; 
let pass5 = string_of_bool (ans5 = expected5);;
if (ans5 = expected5) then
  numCorrect := !numCorrect + 1;;
Printf.printf "%s\n" pass5;;


(* Test 6*)
let exp6 = "3.     3.        +";;
let ans6 = evaluate exp6;;
let expected6 = 6.;;
Printf.printf "Exp: %s should return %f\n" exp6 expected6;; 
let pass6 = string_of_bool (ans6 = expected6);;
if (ans6 = expected6) then
  numCorrect := !numCorrect + 1;;
Printf.printf "%s\n" pass6;;

(* Test 7: Bad cases. Should stop elegantly and return 0*)
(* Forgot decimal points for floats *)
let exp7 = "3 + 3";;
let ans7 = evaluate exp7;;
let expected7 = 0.;;
Printf.printf "Exp: %s should return %f\n" exp7 expected7;; 
let pass7 = string_of_bool (ans7 = expected7);;
if (ans7 = expected7) then
  numCorrect := !numCorrect + 1;;
Printf.printf "%s\n" pass7;;

(* Test 8: invalid operations *)
let exp8 = "3 3 log";;
let ans8 = evaluate exp8;;
let expected8 = 0.;;
Printf.printf "Exp: %s should return %f\n" exp8 expected8;; 
let pass8 = string_of_bool (ans8 = expected8);;
if (ans8 = expected8) then
  numCorrect := !numCorrect + 1;;
Printf.printf "%s\n" pass8;;

(* Test 9: Bad order *)
let exp9 = "3. + 3.";;
let ans9 = evaluate exp9;;
let expected9 = 0.;;
Printf.printf "Exp: %s should return %f\n" exp9 expected9;; 
let pass9 = string_of_bool (ans9 = expected9);;
if (ans9 = expected9) then
  numCorrect := !numCorrect + 1;;
Printf.printf "%s\n" pass9;;


(* Test 10: Stack still contains values at end of operations *)
let exp10 = "3. 3. + 5.";;
let ans10 = evaluate exp10;;
let expected10 = 0.;;
Printf.printf "Exp: %s should return %f\n" exp10 expected10;; 
let pass10 = string_of_bool (ans10 = expected10);;
if (ans10 = expected10) then
  numCorrect := !numCorrect + 1;;
Printf.printf "%s\n" pass10;;

Printf.printf "Num Tests: 10 | Correct: %d\n" !numCorrect