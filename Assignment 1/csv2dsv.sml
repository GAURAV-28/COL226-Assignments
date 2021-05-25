exception emptyInputFile
exception UnevenFields of string
exception unBalancedDoublequotes
exception doubleQuoteNOTenclosed
exception noNewlineatEnd

(*-------------------------------------------------------------------
This function is used to read the input file in a single string using 
TextIO
-------------------------------------------------------------------*)
fun read (infile:string) =
   let 
        val instream = TextIO.openIn infile
	fun loop instream =
	    String.implode(String.explode(TextIO.inputAll instream))

    in
	    loop instream before TextIO.closeIn instream
    end
(*-------------------------------------------------------------------
This function is used to write the output file using a single string 
using TextIO
-------------------------------------------------------------------*)
fun write(outfile: string, s: string) = 
  let
        val fd = TextIO.openOut outfile
  fun loop(s) = 
        TextIO.output(fd, s)
    
  in
        loop(s) before TextIO.closeOut fd 
  end
(*-------------------------------------------------------------------
This function is used to count the number of occurances of delim1 
outside the double quotes i.e. the actual delimiters. 
k represents the state of the string. 0-> number of double quotes are 
even, 1-> number of double quotes are odd(delimiter to be skipped).
-------------------------------------------------------------------*)
fun count(delim1: char, s: string) = 
  let fun cc_helper(j,c,n,k,delim1,s) = if c=n then j
                                    else if k = 0 then if String.sub(s,c) = delim1 then cc_helper(j+1,c+1,n,0,delim1,s)
                                                      else if String.sub(s,c) = #"\"" then cc_helper(j,c+1,n,1,delim1,s)
                                                      else cc_helper(j,c+1,n,0,delim1,s)
                                          else if String.sub(s,c) = #"\"" then cc_helper(j,c+1,n,0,delim1,s)
                                               else cc_helper(j,c+1,n,1,delim1,s);
  in
    cc_helper(0,0,size s,0,delim1,s)+1
  end

(*-------------------------------------------------------------------
This function is used to break the input string according to new line
(\n) present outside the double quotes i.e. breaking the string
in lines present in the input file.
k represents the state of the string. 0-> number of double quotes are 
even, 1-> number of double quotes are odd.
-------------------------------------------------------------------*)
fun break(s: string) = 
    let fun bb_helper(j,c,n,d,k,s) = if c = n then if d = n then j
                                                   else j @ [String.substring(s,d,n-d)]
                                     else if k = 0 then if String.sub(s,c) = #"\n" then bb_helper( j @ [String.substring(s,d,c-d + 1)], c+1, n, c+1, 0 , s)
                                                        else if String.sub(s,c)= #"\"" then bb_helper(j,c+1,n,d,1,s)
                                                        else bb_helper(j,c+1,n,d,0,s)
                                          else if String.sub(s,c)= #"\"" then bb_helper(j,c+1,n,d,0,s)
                                               else bb_helper(j,c+1,n,d,1,s);
    in bb_helper([],0,size s,0,0,s) end
(*-------------------------------------------------------------------
This function is used to count the number of delimiters in a string 
list using break and count functions.
-------------------------------------------------------------------*)
fun list_count(s: string, delim1: char) = 
    let
        val x = break(s);
        val y = map (fn x => count(delim1,x)) x; 
    in
        y
    end
(*-------------------------------------------------------------------
check_err function is used to traverse through the list containing the 
count of delimiters for each line and raise error whenever consecutive 
fields are not equal.
handle_err just handles the exception message generated in the check_err
function.
-------------------------------------------------------------------*)

fun check_err l =
    let fun cerr_helper(c,n,l) = if c = n-1 then ()
                                 else if List.nth(l,c) = List.nth(l,c+1) then cerr_helper(c+1,n,l)
                                      else raise UnevenFields ("Exception UnevenFields\nExpected: "^Int.toString(List.nth(l,c))^" fields, Present: "^Int.toString(List.nth(l,c+1))^" fields on Line "^Int.toString(c+2)^"\n");
    in cerr_helper(0,length l,l) end

fun handle_err l = check_err l handle UnevenFields i => print(i)
(*-------------------------------------------------------------------
This function is the core of the code. This function is used to
iterate through the input string and change or add characters 
according to the given no of double quotes - odd/even denoted by k.
To handle the corner cases, the first and last characters of the
string are written seperately.
If we encounter delim1 we replace it by delim2 and we check the 
neighbourhood of this position and make changes accordingly.
We keep updating the value of k whenever we encounter double quotes.
We are trying to enclose all strings that are not enclosed before so we 
add double quotes to the out string. Suitable exceptions are raised.
-------------------------------------------------------------------*)
fun string_iterss(delim1: char, delim2: char, s: string) =
    let fun helper(out,c,n,k,delim1,delim2,s) = 
    if c >= n-1 then if k=1 then raise unBalancedDoublequotes
                    else if String.sub(s,n-1) = #"\n" then if String.sub(s,n-2) = #"\"" then out^str(String.sub(s,n-1))
                                                            else out^"\""^str(String.sub(s,n-1))
                         else raise noNewlineatEnd
    else
        if k = 0 then if String.sub(s,c) = delim1 then if String.sub(s,c-1) = #"\"" then if String.sub(s,c+1) = #"\"" then helper(out^str(delim2),c+1,n,0,delim1,delim2,s)
                                                                                         else helper(out^str(delim2)^"\"",c+1,n,0,delim1,delim2,s)
                                                       else if String.sub(s,c+1) = #"\"" then helper(out^"\""^str(delim2),c+1,n,0,delim1,delim2,s)
                                                            else helper(out^"\""^str(delim2)^"\"",c+1,n,0,delim1,delim2,s)
                                                                    
                       else if String.sub(s,c) = #"\"" then if String.sub(s,c-1) = #"\"" then helper(out^"\"",c+1,n,1,delim1,delim2,s)
                                                            else if String.sub(s,c-1) = delim1 then helper(out^"\"",c+1,n,1,delim1,delim2,s)
                                                            else if String.sub(s,c-1) = #"\n" then helper(out^"\"",c+1,n,1,delim1,delim2,s)
                                                            else raise doubleQuoteNOTenclosed
                       else if String.sub(s,c) = #"\n" then if String.sub(s,c-1) = #"\"" then if String.sub(s,c+1) = #"\"" then helper(out^"\n",c+1,n,0,delim1,delim2,s)
                                                                                              else helper(out^"\n\"",c+1,n,0,delim1,delim2,s)
                                                                    else if String.sub(s,c+1) = #"\"" then helper(out^"\"\n",c+1,n,0,delim1,delim2,s)
                                                                         else helper(out^"\"\n\"",c+1,n,0,delim1,delim2,s)
                       else helper(out^str(String.sub(s,c)),c+1,n,0,delim1,delim2,s)
            
        else if String.sub(s,c)= #"\"" then if String.sub(s,c+1) = #"\"" then helper(out^"\"",c+1,n,0,delim1,delim2,s)
                                            else if String.sub(s,c+1) = delim1 then helper(out^"\"",c+1,n,0,delim1,delim2,s)
                                            else if String.sub(s,c+1) = #"\n" then helper(out^"\"",c+1,n,0,delim1,delim2,s)
                                            else raise doubleQuoteNOTenclosed 
             else helper(out^str(String.sub(s,c)),c+1,n,1,delim1,delim2,s);



    in
        if size s = 0 then raise emptyInputFile
        else if String.sub(s,0) = #"\"" then helper("\"",1,size s,1,delim1,delim2,s)
             else if String.sub(s,0) = delim1 then helper("\"\""^str(delim2)^"\"",1,size s,0,delim1,delim2,s)
             else helper("\""^str(String.sub(s,0)),1,size s,0,delim1,delim2,s)
    end

(*-------------------------------------------------------------------
Main function: call this function to change the delimiters and store
the new file as outfile. 
This function also gives error if input file is empty.
This function uses all the above defined function.
-------------------------------------------------------------------*)

fun convertDelimiters(infile: string, delim1: char, outfile: string, delim2:char) = 
    let
        val l = read(infile)
        val x = list_count(l, delim1)
        val m = string_iterss(delim1, delim2, l)
    in
        handle_err(x);
        if l = "" then raise emptyInputFile 
        else write(outfile,m)
    end 

(*-------------------------------------------------------------------
csv2tsv converts files with comma as delimiter to files with tab as 
delimiter.
tsv2csv converts files with tab as delimiter to files with comma as 
delimiter.
Both function uses convertDelimiters function.
-------------------------------------------------------------------*)


fun csv2tsv(infile: string, outfile: string) = convertDelimiters(infile, #",", outfile, #"\t");
fun tsv2csv(infile: string, outfile: string) = convertDelimiters(infile, #"\t", outfile, #",");
