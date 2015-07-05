module Misc

/// Remove occurences of chars from s
//  http://rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#F.23
let stripChars (chars:string) (s:string)  =
    Array.fold (fun (s:string) c -> s.Replace(c.ToString(),""))
                s (chars.ToCharArray())
