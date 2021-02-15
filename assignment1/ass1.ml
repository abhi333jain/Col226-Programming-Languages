open TextIO;
exception E; (*Add line number*)

fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
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
				if(cnt=fields orelse record_no=1) then ( output(outfile,"") ; closeOut(outfile))
				else raise E 
				(*line number*)
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
							else raise E
					else
						( output(outfile,c); iter(infile,outfile,cnt,fields,delim1,delim2,record_no))
				end ; 
	in
		iter(infile,outfile,1,1,delim1,delim2,1)
	end;

fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",", outfilename, #"\t") 

fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename, #"\t" , outfilename, #",") 