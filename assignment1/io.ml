open TextIO;
exception E of int*int*int;	
exception E1;

fun printlist([]) = print(" ")
| printlist(hd::tl) =( 
	print(Int.toString(hd));
	print("\n");
	printlist(tl)
	);

fun printX(n) = 
	let
		fun strX(n) = 
			if(n=0) then "X"
			else
				let 
					val c = strX(n-1)
				in
					c^c
				end
	in
		(print(strX(n)); print("\n"))
	end;

fun readlist(infile) = 
	if(endOfStream(infile)) then []
	else inputN(infile,1)::readlist(infile)

fun write1(infile,outfile) = 
	if(endOfStream(infile)) then (output(outfile,"") ; closeOut(outfile))
	else ( output(outfile,inputN(infile,1));  write1(infile,outfile));



fun readline(infile,c) = 
	if(isSome(c)) then 
		(print(valOf(c)); readline(infile,inputLine(infile)))
	else print("");

fun readword(infile) = 
	let
		fun iter(infile,w,ls) = 
			if(endOfStream(infile)) then w::ls
			else
				let
					val c = inputN(infile,1)
				in
				if(c=" " orelse c="\n") then iter(infile,"",w::ls)
				else iter(infile,w^c,ls)
				end;
	in
		iter(infile,"",[])
	end;


fun readword1(infile) = 
	let
		fun iter(infile,w,cnt) = 
			if(endOfStream(infile)) then (print(w); print(" "); print(Int.toString(cnt+1)); print("\n"))
			else
				let
					val c = inputN(infile,1)
				in
				if(c=" " orelse(c="\t")) then (print(w); print("\n"); iter(infile,"",cnt+1))
				else if(c="\n") then (print(w); print(" "); print(Int.toString(cnt+1)); print("\n"); iter(infile,"",0))
				else iter(infile,w^c,cnt)
				end
	in
		iter(infile,"",0)
	end; 

fun readword2(infile,outfile) = 
	let 
		fun check_char(" ") = 1
		|	check_char("\t") = 1
		|	check_char("\n") = 2
		|	check_char(_) = 3

		fun getfield(infile,isend) = 
			if(endOfStream(infile)) then ("",true)
			else 
				let
					val c = inputN(infile,1)
					val typ= check_char(c)
				in
					if(typ=1) then ("",false)
					else if(typ=2) then ("",true)
					else 
						let 
							val (x,y) = getfield(infile,isend);
						in
							(c^x,y)
						end
				end;
		fun printword(infile,c) = 
				if(endOfStream(infile)) then print("")
				else
					let
						val (x,y) = getfield(infile,false);
					in
						( print(x); print(" ");
						  if(y=true) then 
						  	( print(Int.toString(c+1)); print("\n"); printword(infile,0))
						  else
						  	(print("\n"); printword(infile,c+1))
						  )
					end;
		fun writeword(infile,outfile,c) = 
				if(endOfStream(infile)) then (output(outfile,"") ; closeOut(outfile))
				else
					let
						val (x,y) = getfield(infile,false);
					in
						( output(outfile,x); output(outfile," ");
						  if(y=true) then 
						  	( output(outfile,Int.toString(c+1)); output(outfile,"\n"); writeword(infile,outfile,0))
						  else
						  	(output(outfile,"\n"); writeword(infile,outfile,c+1))
						  )
					end;
	in
		writeword(infile,outfile,0)
	end;


fun convertDelimiters1(infilename, delim1, outfilename, delim2) = 
	let

		fun isnextdelim1(infile,delim1) = 
			let 
				val x = lookahead(infile)
			in
				if(isSome(x)=true) then (str(valOf(x))=str(delim1))
				else false
			end;

		fun iter(infile,outfile,cnt,fields,delim1,delim2,record_no) = 
			if(endOfStream(infile)) then
				if(cnt=fields orelse record_no=1) then ( output(outfile,"") ; closeOut(outfile))
				else raise E(fields,cnt,record_no)
			else
				let
					val c =inputN(infile,1)
				in
					if(c=str(delim2)) then 
						( output(outfile,"\\"); output(outfile,str(delim2)); iter(infile,outfile,cnt,fields,delim1,delim2,record_no))

				else if(c="\\" andalso isnextdelim1(infile,delim1)=true) then 
						( output(outfile,str(delim1));  inputN(infile,1);   (* leave the next char *)
							iter(infile,outfile,cnt,fields,delim1,delim2,record_no)) 

					else if(c=str(delim1)) then 
						( output(outfile,str(delim2)); iter(infile,outfile,cnt+1,fields,delim1,delim2,record_no))

	 (*check EOL *) 	else if(c="\n") then
						if(record_no=1) then ( output(outfile,c); iter(infile,outfile,1,cnt,delim1,delim2,record_no+1))
						else
							if(fields=cnt) then  (output(outfile,c); iter(infile,outfile,1,fields,delim1,delim2,record_no+1))
							else raise E(fields,cnt,record_no)
					else
						( output(outfile,c); iter(infile,outfile,cnt,fields,delim1,delim2,record_no))
				end ; 
	in
		iter(infilename,outfilename,1,1,delim1,delim2,1)
	end;

fun convertDelimiters(infilename, delim1, outfilename, delim2) = convertDelimiters1(infilename, delim1, outfilename, delim2) handle
													E(fields,cnt,record_no) => (
														print("Expected: ");
														print(Int.toString(fields));
														print(" fields,");
														print("Present: ");
														print(Int.toString(cnt));
														print(" fields on Line ");	
														print(Int.toString(record_no));
														print("\n")													
														);

fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",", outfilename, #"\t") 
fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename, #"\t" , outfilename, #",") 

fun convertNewlines(infilename, newline1, outfilename, newline2) = 
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









