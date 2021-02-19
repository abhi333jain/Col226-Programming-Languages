open TextIO;
exception E of int*int*int;


(* add exception for '\' used as delimeter *)
fun convertDelimiters1(infilename, delim1, outfilename, delim2) = 
	let

		val infile=openIn(infilename)
		val outfile=openOut(outfilename)

		fun isnextdelim1(infile,delim1) = 
			let 
				val x = lookahead(infile)
			in
				if(isSome(x)=true) then (str(valOf(x))=str(delim1))
				else false
			end;

		fun iter(infile,outfile,cnt,fields,delim1,delim2,record_no) = 
			if(endOfStream(infile)) then
				if(cnt=fields orelse record_no=1) then ( output(outfile,""); closeOut(outfile))
				else raise E(fields,cnt,record_no) 
				(*line number*)
			else
				let
					val c =inputN(infile,1)
				in
					if(c=str(delim2)) then 
						( output(outfile,"\\");	output(outfile,str(delim2));	iter(infile,outfile,cnt,fields,delim1,delim2,record_no))

					else if(c="\\" andalso isnextdelim1(infile,delim1)=true) then 
						( output(outfile,str(delim1));  inputN(infile,1);   (* leave the next char *)
							iter(infile,outfile,cnt,fields,delim1,delim2,record_no)) 

					else if(c=str(delim1)) then 
						( output(outfile,str(delim2)); iter(infile,outfile,cnt+1,fields,delim1,delim2,record_no))

	 (*check EOL *) else if(c="\n") then
						if(record_no=1) then ( output(outfile,c);	iter(infile,outfile,1,cnt,delim1,delim2,record_no+1))
						else
							if(fields=cnt) then  (output(outfile,c);	iter(infile,outfile,1,fields,delim1,delim2,record_no+1))
							else raise E(fields,cnt,record_no)

					else
						( output(outfile,c);	iter(infile,outfile,cnt,fields,delim1,delim2,record_no) )
				end ; 
	in
		iter(infile,outfile,1,1,delim1,delim2,1)
	end;

fun convertDelimiters(infilename, delim1, outfilename, delim2) = convertDelimiters1(infilename, delim1, outfilename, delim2) handle
													E(fields,cnt,record_no) => (
														print("exception UnevenFields of string \n");
														print("Expected: ");
														print(Int.toString(fields));
														print(" fields,");
														print("Present: ");
														print(Int.toString(cnt));
														print(" fields on Line ");	
														print(Int.toString(record_no));
														print("\n")													
														)
												|	IO => (
														print("exception emptyInputFile \n")
														);

fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",", outfilename, #"\t") 

fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename, #"\t" , outfilename, #",") 




fun convertNewlines1(infilename, newline1, outfilename, newline2) = 
	let

		val infile=openIn(infilename)
		val outfile=openOut(outfilename)

		val sz1= size newline1
		val sz2= size newline2
		
		fun iter(infile,outfile,cnt,str,sz1,nl1,nl2) = 
			if(endOfStream(infile)) then ( output(outfile,str) ; closeOut(outfile))
			else
				let
					val c =inputN(infile,1)
					val str1=str^c
				in
					if(cnt=sz1-1) then
						if(str1<=newline1 andalso str1>=newline1)  then ( output(outfile,newline2); iter(infile,outfile,0,"",sz1,nl1,nl2))
						else ( output(outfile,str1); iter(infile,outfile,0,"",sz1,nl1,nl2))
					else
						iter(infile,outfile,cnt+1,str1,sz1,nl1,nl2)
				end 
	in
		iter(infile,outfile,0,"",sz1,newline1,newline2)
	end;

fun convertNewlines(infilename, newline1, outfilename, newline2) = convertNewlines1(infilename, newline1, outfilename, newline2) handle
													IO => (
														print("exception emptyInputFile \n")
														);

fun unix2dos(infilename, outfilename) = convertNewlines(infilename, "\r\n", outfilename, "\n") 

fun dos2unix(infilename, outfilename) = convertNewlines(infilename, "\n", outfilename, "\r\n") 