open System

//Regular alphabet for the Ceasar Cipher
let alphabet = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']

//Frequency Alphabet that is sorted on the most frequently used letters in the english alphabet
let freqAlpha = [' '; 'E'; 'T'; 'O'; 'A'; 'I'; 'N'; 'H'; 'S'; 'R'; 'D'; 'L'; 'U'; 'M'; 'C'; 'W'; 'G'; 'F'; 'Y'; 'P'; ','; '.'; 'B'; 'K'; 'V'; '"'; '''; '-'; '?'; 'X'; 'J'; ';'; '!'; 'Q'; 'Z'; ')'; '(';]

//The alphabet that is sorted on the most frequently used letters in the text in the file TEXT_2.txt, this was the alphabet i 
//figured out myself after running the program
let decryptFreqAlpha = [' '; 'E'; 'T'; 'O'; 'A'; 'S'; 'N'; 'H'; 'I'; 'D'; 'L'; 'R'; 'P'; 'U'; 'F'; 'W'; '.'; 'G'; 'Y'; 'C'; 'M'; 'B'; ','; 'K'; '''; 'V'; 'X'; '?'; 'J'; '-'; 'Z'; 'Q'; '!']

//Function for shifting num steps to the left for a given letter in the Alphabet, 
//returns the shifted char
//Using the int values of the chars to see how they can be shifted
let ShiftLeft num (letter : char) = 
    let lValue = ((int(letter))-65-num)
    if lValue < 0 then
        alphabet.Item(26+lValue)
    else
        alphabet.Item(lValue)

//Function that uses the ShiftLeft function to shift letters and returning a list of the shifted Alphabet
//Takes a number thats an argument for ShiftLeft and also the String that will be shifted
let rec DecryptCeasar num l =
    match l with 
        | head::tail ->
            if Char.IsLetter head then
                (ShiftLeft num head)::(DecryptCeasar num tail)
            else
                head::(DecryptCeasar num tail)
        | _ -> ['\n']


//The function to execute the Ceasar Decryption, the first parameter is the key to start trying from, usually 0.
let rec ShowCeasar num l =
    if num >= 26 then
        printfn "Thats all"
    else
        printfn "Key %d: %s" num (System.String.Concat(DecryptCeasar num l))
        ShowCeasar (num+1) l

//Function recursively goes through the text and returns the number of occurences a given letter has. 
let rec LoopText letter s =
    match s with 
        | head::tail ->
            if head = letter then
                1 + (LoopText letter tail)
            else
                LoopText letter tail
        | _ -> 0

//Function that recursively goes through the Frequency sorted alphabet and returns a list with tuples of (occurenses, letter)
let rec FindOcc l s =
    match l with
        | head::tail ->
            ((LoopText head s), head)::(FindOcc tail s)
        | _ -> []

//Function that swaps the letter (letter) with the corresponding letter in the list of letter (a)
//returns the letter thats suppose to be on that position in the text
let rec SwapLetter (a : char list) l (letter : char) =
    match a, l with
        | head1::tail1, head2::tail2 ->
            match head2 with
                | (_, current) ->
                    if current = letter then
                        head1
                    else
                        SwapLetter tail1 tail2 letter
        | _ -> '\n'

//function that takes the Frequency sorted alphabet, the sorted alphabet on occurences and a list of chars of the string that will be decrypted
//Returns a list with characters with the letters replaced depending on the frequency sorted alphabet            
let rec FindLetters (a : char list) l (s : char list) =
    match s with
        | head::tail ->
            (SwapLetter a l head)::(FindLetters a l tail)
        | _ -> []
                    
                    

//Function that converts a string to a list of chars
let StringToCharList s =
    [for c in s -> c]

[<EntryPoint>]
let main argv = 
    
    printfn "Press 1 if u want to run the Ceasar Decryption and 2 if you want to run the Mixed Alphabet Decryption!"
    let choice = System.Console.ReadLine()
    if choice = "1" then
        ShowCeasar 0 (StringToCharList (IO.File.ReadAllText "TEXT_1.txt"))
        //Read the string from the file
    else
        let s = StringToCharList (IO.File.ReadAllText "TEXT_2.txt")
        //Create a list sorted on number of occurences the characters has
        let sortedList = List.sortDescending (FindOcc freqAlpha s)
        //Create the new string with the sortedList and the frequency sorted alphabet
        //change freqAlpha the decryptedFreqAlpha to get the right result immediately
        let str = (FindLetters freqAlpha sortedList s) |> Array.ofList |> String
        printfn "%A\n------------------------------\n" str
        printfn "%A" (freqAlpha |> Array.ofList |> String)
        //While loop to let the user continue changing the frequency alphabet to come up with a solution
        while true do
            printfn "Is this the correct Ciphered Alphabet? if not copy the alphabet above and change the letters u want and try again"
            let newAlpha = StringToCharList (System.Console.ReadLine())
            Console.Clear()
            printfn "\n%A\n-----------------------------\n" ((FindLetters newAlpha sortedList s) |> Array.ofList |> String)
            printfn "%A" (newAlpha |> Array.ofList |> String)

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
