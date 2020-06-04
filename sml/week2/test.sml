fun assert condition message = 
    if condition 
    then
        print (message ^ "\n")
    else 
        let
            val () = print ("Assert error: " ^ message ^ "\n")
        in
            OS.Process.exit OS.Process.failure
        end

fun complete () =
    let
        val () = print "All tests passed!"
    in
        OS.Process.exit OS.Process.success
    end