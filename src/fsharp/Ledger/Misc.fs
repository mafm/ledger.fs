/// Stuff that should be built into the language, but I couldn't find.

module Misc

/// Remove occurences of chars from s
//  http://rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#F.23
let stripChars (chars:string) (s:string)  =
    Array.fold (fun (s:string) c -> s.Replace(c.ToString(),""))
                s (chars.ToCharArray())

/// Does list a start with list b?
let rec startsWith<'X when 'X: equality> (a :'X list) (b :'X list) =
    match b with
        | [] -> true
        | B::Bs -> match a with
                   | A::As -> (A=B) && (startsWith As Bs)
                   | _ -> false
