open TextIO;
exception UnevenFields of int*int*int;
exception emptyInputFile
exception NotTerminatedByEOL
exception FieldNotEnclosedinDq
exception DqNotInPair

(* add exception for '\' used as delimeter *)

(*check for empty field followed by eol *)

fun convertDelimiters1(infilename, delim1, outfilename, delim2) = 
	let

		val infile=openIn(infilename)
		val outfile=openOut(outfilename)

		fun isnextchar(c) =       (*checks if next char in the stream is c or not *)
			let 
				val x = lookahead(infile)
			in
				if(isSome(x)=true) then (str(valOf(x))=str(c))
				else false
			end;

		fun isempty(infile) = 	(*checks if the stream is empty *)
			let 
				val x = lookahead(infile)
			in
				not (isSome(x))
			end;			

(* cnt => stores number of fields iterated upon in the current record *)
(* fields =>  number of fields in the first record *)
(* record_no => stores the current line number *)
(* hasdq => stores whether the field being iterated upon is enclosed in double quotes or not *)
(* st => stores whether the current char read is the first char of the field or not *)
(* isEOL => stores whether the prev char read is "\n" or not *)

		fun iter(cnt,fields,record_no,hasdq,st,isEOL) = 	(* main function *)
			if(endOfStream(infile)) then 
				if(isEOL=1) then closeOut(outfile) 
				else raise NotTerminatedByEOL (* file is not terminated by EOL *)
			else
				let
					val c =inputN(infile,1)
				in
					if(st=1) then (* starting of a new field *)

						if(c=str(delim1)) then(* empty field *)
							( output(outfile,str(delim2)); iter(cnt+1,fields,record_no,0,1,0))
	
						else if(c="\n") then (*check for empty field followed by eol i.e end of record *)
							if(record_no=1 orelse fields=cnt) then (output(outfile,"\n"); iter(1,cnt,record_no+1,0,1,1))
							else raise UnevenFields(fields,cnt,record_no)

						else if(c="\"") then  (*field is enclosed in dq *)
							( output(outfile,c); iter(cnt,fields,record_no,1,0,0))
							
						else (*field is not enclosed in dq, so we explicitly enclose the field in dq and write it in outfile *)
							( output(outfile,"\""); output(outfile,c); iter(cnt,fields,record_no,0,0,0))

					else 	(* c is in middle/end of field *)


						if(hasdq=0)	then(*  field is not enclosed in dq , but in output file we will enclose it in dq*)

							if(c=str(delim1)) then 	(* end of a field *)
								( output(outfile,"\""); output(outfile,str(delim2)); 
								iter(cnt+1,fields,record_no,0,1,0))

							else if(c="\n") then	(*end of record *)
								if(record_no=1 orelse fields=cnt) then ( output(outfile,"\""); output(outfile,"\n"); 
										iter(1,cnt,record_no+1,0,1,1))
								else raise UnevenFields(fields,cnt,record_no)

							else if(c="\"") then raise FieldNotEnclosedinDq(*raise error as field is not enclosed in " "*)
							else	
								( output(outfile,c); iter(cnt,fields,record_no,hasdq,st,0))

						else if(c="\"") then	(*  field is enclosed in dq and c=" *)

							if(isnextchar(delim1)=true) then 	(* end of a field *)
								( output(outfile,c); output(outfile,str(delim2)); 
								inputN(infile,1);  (* leave the next char i.e delimeter *)	iter(cnt+1,fields,record_no,0,1,0))

							else if(isnextchar(#"\n")=true) then	  (* end of record *)
								if(record_no=1 orelse fields=cnt) then ( output(outfile,c); output(outfile,"\n"); 
								inputN(infile,1);   (* leave the next char i.e EOL *)	iter(1,cnt,record_no+1,0,1,1))
								else raise UnevenFields(fields,cnt,record_no)

							else if(isnextchar(#"\"")=true) then 	(* dq inside a field occur in pair i.e "" *)
								( output(outfile,c); output(outfile,c); 
								inputN(infile,1);   (* leave the next char i.e " *)	iter(cnt,fields,record_no,hasdq,st,0))

							else if(isempty(infile)=true) then raise NotTerminatedByEOL (* file is not terminated by EOL *)

							else (*raise error as dq must in pair inside a field*) raise DqNotInPair

						else
							( output(outfile,c); iter(cnt,fields,record_no,hasdq,st,0))
						end;
	in
		if(isempty(infile)=true) then raise emptyInputFile
		else iter(1,1,1,0,1,0)
	end;

fun convertDelimiters(infilename, delim1, outfilename, delim2) = convertDelimiters1(infilename, delim1, outfilename, delim2) handle
													UnevenFields(fields,cnt,record_no) => (
													(*	print("exception UnevenFields of string \n"); *)
														print("Expected: ");
														print(Int.toString(fields));
														print(" fields, ");
														print("Present: ");
														print(Int.toString(cnt));
														print(" fields on Line ");	
														print(Int.toString(record_no));
														print("\n")													
														)




fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",", outfilename, #"\t") 

fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename, #"\t" , outfilename, #",") 








