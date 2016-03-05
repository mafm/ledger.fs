module FormatExceptionForDisplay

// https://sergeytihon.wordpress.com/2013/04/08/f-exception-formatter/

open System
open System.Reflection
open System.Text
open Microsoft.FSharp.Core.Printf
 
let formatDisplayMessage (e:Exception) =
    let sb = StringBuilder()
    let delimeter = String.replicate 50 "*"
    let nl = Environment.NewLine
    let rec printException (e:Exception) count =
        let indent = String.replicate (count * 5) " "
        let lessIndent = String.replicate ((count-1) * 5) " "
        if (e :? TargetException && e.InnerException <> null)
        then printException (e.InnerException) count
        else
            if (count = 1) then bprintf sb "%s%s%s%s%s" lessIndent e.Message nl indent delimeter
            else bprintf sb "%s%s%s%d)%s%s%s%s" nl nl lessIndent count e.Message nl indent delimeter
            bprintf sb "%s%sType: %s" nl indent (e.GetType().FullName)
            // Loop through the public properties of the exception object
            // and record their values.
            e.GetType().GetProperties()
            |> Array.iter (fun p ->
                // Do not log information for the InnerException or StackTrace.
                // This information is captured later in the process.
                if (p.Name <> "InnerException" && p.Name <> "StackTrace" &&
                    p.Name <> "Message" && p.Name <> "Data") then
                    try
                        let value = p.GetValue(e, null)
                        if (value <> null)
                        then bprintf sb "%s%s%s: %s" nl indent p.Name (value.ToString())
                    with
                    | e2 -> bprintf sb "%s%s%s: %s" nl indent p.Name e2.Message
            )
            if (e.StackTrace <> null) then
                let lines = e.StackTrace.Split '\n'
                bprintf sb "\n%s%s\n%sStackTrace\n" indent delimeter indent 
                for line in lines do
                    bprintf sb "%s%s%s" nl indent line
                //printf "^%s^\n" e.StackTrace                
                //bprintf sb "%s%s" nl e.StackTrace
            if (e.InnerException <> null)
            then printException e.InnerException (count+1)
    printException e 1
    sb.ToString()