title BasicCal.asm		;DOS file name of program

.586			        ;enable all pentium instructions
.model flat, stdcall		;memory model & calling convention
.stack 8192			;allocate 8k for stack

INCLUDELIB kernel32.lib	 	;Include the kernel 32 library

;----------------------------------------------------------
; Constant Definitions
;----------------------------------------------------------

STD_INPUT  equ -10d                     ;Function number for keyboard input
STD_OUTPUT equ -11d                     ;Function number for monitor output

LF equ 10d                              ;Line feed ascii constant
CR equ 13d                              ;Carriage return constant
NEWLINE equ CR,LF                       ;Combine CR and LF for carriage return

ENABLE_PROCESSED_INPUT  equ 1           ;Flag to turn off line buffering
ENABLE_PROCESSED_OUTPUT equ 1           ;Flag to turn off line bufferin
ENABLE_LINE_WRAP        equ 3           ;Flag to trun line wrap on
DISABLE_PROCESSED_INPUT equ 7           ;Flag to turn on line buffering

; -----------------------------------------------------------------------
; Library Imports -> Prototype Declarations Section
; -----------------------------------------------------------------------
ExitProcess proto,
    dwExitCode:dword			;The exit code for the process 

GetStdHandle proto, 
	nStdHandle: dword               ;The standard device. -10=INPUT, -11=OUTPUT, -13=ERROR

SetConsoleMode proto,                  
    hConsoleHandle:dword,       	;A handle to the console input buffer or a console screen buffer
	dwMode:dword                    ;The input or output mode to be set. 

ReadFile proto,	
    hFile:dword,                       	;A handle to the device
	lpBuffer:near32,                ;A pointer to the buffer that receives the data read 
    nNumberOfCharsToRead:dword,         ;The maximum number of bytes to be read.
    lpNumberOfbytesRead:near32,         ;A pointer to the variable that receives the number of bytes read
    lpOverlapped:near32                 ;A pointer to an OVERLAPPED structure is required if the hFile parameter 
	                                ;was opened with FILE_FLAG_OVERLAPPED, otherwise it can be NULL.

WriteFile proto,                  
    hFile:dword, lpBuffer:near32,       ;A handle to the device
    nNumberOfCharsToWrite:dword,        ;The maximum number of bytes to be written.
    lpNumberOfbytesWritten:near32,      ;A pointer to the variable that receives the number of bytes written
    lpOverlapped:near32                 ;A pointer to an OVERLAPPED structure is required if the hFile parameter 
	                                ;was opened with FILE_FLAG_OVERLAPPED, otherwise it can be NULL.

; -----------------------------------------------------------------------
; .data -> Global Variables Section
; -----------------------------------------------------------------------
.data

	strAddr			dd  ?
	strLength		dd  ?
	hStdOut			dd  ?
	hStdIn			dd  ?
	read			dd  ?
	written			dd  ?	

	; String variables for output formatting
    	starStr db 50 DUP('*'), NEWLINE, 0
	promptMsg db "Basic calculator. One operation allowed", NEWLINE,0
	promptMsg1 db "Input a number base and the calculation line. Input 'Q' or 'q' to quit.", NEWLINE,0
	promptMsg2 db "Enter calculation in form of space delimited numbers, then operators.", NEWLINE,0
	baseStr db "Enter number base or 'Q' to quit: ", 0
	listBase db "A=10, B=11, C=12, D=13, E=14, F=15, G=16", NEWLINE,0 
	newLn db NEWLINE, 0
	colon db ": ", 0			
	
	; Variables for string and numbers
	baseNum WORD ?				; variable used to store base of number entered
	usrString db 40 DUP (?), 0		; variable used to parse input and store strings from user input. NULL terminated

	;Variables to parse input
	num1		WORD 0
	num2		WORD 0
	num		WORD 0
	num3		db ?
	operand		db ?
	numCount	WORD 0
	resultCal	WORD 0

	; Conversion variables. binary, decimal, hex, and base given
	binaryValue	db 50 dup(?)
	decimalValue	db 50 dup(?)
	hexValue	db 40 dup(?)	
	baseValue	db 50 dup(?)

	quotient	WORD ?
	remainder	WORD ?
	rmndrCntr	WORD 0			;counter to number of remainders pushed into stack 

	Num2StrCount dw 0				; counter for num 2 string
	
	; Variables for Num2Str
	usrStringResult db 40 DUP(?)			; variable used to store string result of number conversion
	num2Convert word ?				

	; error messages
	errorResult db "Result of calculation greater than word sized variable. Exiting program.", NEWLINE, 0
	
	; Variables to print results to screen
	binaryPrompt	 db "Binary: ", 0
	hexPrompt	 db "Hex: ", 0
	BaseGivenPrompt	 db "BaseGiven: ", 0
	decimalPrompt	 db "Decimal: ", 0
	spaces		 db "   ", 0

; -----------------------------------------------------------------------
; main -> Code Segment
; -----------------------------------------------------------------------
.code
main proc

   xor eax, eax
   xor ebx, ebx
   xor ecx, ecx
   xor edx, edx

   call printPrompt				; Print welcome prompt
   
   ;loop until user enters 'q' or 'Q'
   calLoop:
		call numBasePrompt		; Prompt user to enter base or quit
		call getUsrString		; get the string from prompt to store in usrString variable
		call compString			; compares string in usrString, if 'q' or 'Q' exits program
		call Str2Num		        ; function to convert usrString to number, stores result in baseNum 				
		call printCalcPrompt		; prints prompt for user calculation input, Display ": " prompt for calculation line
		call getUsrString		; get calculation line from user
		call parseString		; parse the line, and calculate answer
		call convertResults		; convert results of calculations into binary, decimal, Hex, and Base given
		call printResult		; print result of operation to screen
		jmp calLoop			; go back to mainLoop								

   invoke ExitProcess, 0			
main endp

;______________________________________________________________________________
; Procedures Section -> For functions used in main as well as other f(x)'s
;______________________________________________________________________________


; -----------------------------------------------------------------------
;	printResult -> Prints results of calculations into the terminal
;	Inputs -> None
;	Outputs -> Written to terminal
; -----------------------------------------------------------------------

printResult proc
	pushad		; save registers
	pushfd		; save flags 

	; print binary
	lea esi, binaryPrompt	
	call PrintString
	lea esi, binaryValue		; print binary value
	call PrintString

	; print spaces
	lea esi, spaces
	call PrintString

	; print hex
	lea esi, hexPrompt
	call PrintString
	lea esi, hexValue		; print hex value
	call PrintString

	; print spaces
	lea esi, spaces
	call PrintString
	
	; print base given
	lea esi, BaseGivenPrompt
	call PrintString
	lea esi, baseValue		; print base given balue
	call PrintString
	
	; print spaces
	lea esi, spaces
	call PrintString
	
	; print decimal
	lea esi, decimalPrompt
	call PrintString
	lea esi, decimalValue		; print result in decimal
	call PrintString
	
	; print newline
	lea esi, newLn
	call PrintString

	popad		; return registers
	popfd		; return flags
	ret		; return to caller
printResult endp

; -----------------------------------------------------------------------
; convertResults -> Converts result of calculation into binary, decimal, hex, and base given
; Input: result of calculations into resultCal
; Output: results of binary, decimal, base given, and hex values stored into variables
; -----------------------------------------------------------------------
convertResults proc
	pushad				; save registers
    	pushfd				; save flags
	
	; Convert results 
	lea esi, resultCal
	call convertNumToHex			; converts number to hex	
	call convertToBinary			; converts number to binary
	call convertNumToBaseGiven		; converts number to base provided by user	
	call convertToDecimal			; converts number to decimal				

	popfd		; restore flags
	popad	        ; restore registers
	ret		; return to caller
convertResults endp

; -----------------------------------------------------------------------
;	convertToDecimal -> Convert number to decimal
;	Input = ESI contains address of variable to convert 
;	Output = EDI contains address of word sized variable in decimal
; -----------------------------------------------------------------------
convertToDecimal proc
	pushad		; save registers
	pushfd		; save flags

	xor eax, eax
	xor ebx, ebx
	xor ecx, ecx
	xor edx, edx

	mov ax, [esi]			; move into ax the number contained in esi
	mov bx, 10d			; move 10d into bx to convert to decimal

	convert_convertToDecimal:
		div bx			; ax = ax / bx
		;mov quotient, ax	; move ax into quotient

		push dx			; push remainder into stack
		inc rmndrCntr		; increment number of items to pop
		
		cmp al, 0			; if quotient is greater than zero
		jg convert_convertToDecimal	; jump back to procedure
		
		lea esi, decimalValue		; load address of decimal value
		lea edi, num3			; load address of num3 to use in Num2Str
		
		cmp ax, 0			; compare if zero
		jz store_convertToDecimal	; jump to store number

	store_convertToDecimal:
		pop ax				; pop value into num 
		call Num2Str			; convert num to string ASCII
		mov bl, num3
		mov [esi], bx			; move number into esi
		inc esi				; move to next slot of array

		dec rmndrCntr			; decrement counter
		cmp rmndrCntr, 0		; compare counter to 0
		jnz store_convertToDecimal	; jump if not zero 

	done_convertToDecimal:
		popad		; restore registers
		popfd		; restore flags
		ret		; return to caller
convertToDecimal endp


; -----------------------------------------------------------------------
; convertNumToHex -> Converts a number to hex
;	Input: ESI = Contains address of result of calculation. Number to convert to Hex.
;	
;	Output: EDI = contains address of word sized variable in hex
;	
; -----------------------------------------------------------------------
convertNumToHex proc
	pushad			; save registers
        pushfd			; save flags
	
	xor eax, eax		; clear EAX register
	xor ebx, ebx		; clear EBX register
	xor ecx, ecx		; clear ECX register
	xor edx, edx		; clear EDX	register
	
	mov ax, [esi]		; move into ax the number contained in esi
	mov bx, 16d		; divide by 16d
	
	convert_NumToHex:	
		div bx				; div ax = ax / bx
		mov quotient, ax		; move ax to quotient

		push dx				; push remainder into stack
		inc rmndrCntr			; increment number of items to pop
		
		cmp al, 0			; compare if quotient is greater than zero
		jg convert_NumToHex		; jump back to procedure if still greater than 0

		lea esi, hexValue		; load address of hexValue
		lea edi, num3			; load address of num3 to use in Num2Str
		
		cmp quotient, 0			; compare if quotient IS zero
		jz storeHexResult		; continue to process to other bases
	
	storeHexResult:
		pop ax				; pop value into num 
		call Num2Str			; convert num to string ASCII
		mov bl, num3
		mov [esi], bx			; move number into esi
		inc esi				; move to next slot of array

		dec rmndrCntr			; decrement counter
		cmp rmndrCntr, 0		; compare counter to 0
		jnz storeHexResult		; jump if not zero

	done_convertNumToHex:
		; test
		;lea esi, hexValue
		;call PrintString
		popfd                   ; restore flags
		popad                   ; restore registers
		ret                     ; return to caller
convertNumToHex endp

; -----------------------------------------------------------------------
; convertToBinary -> Convert Number to Binary
;
; -----------------------------------------------------------------------
convertToBinary proc
	pushad			; save registers
        pushfd			; save flags
	
	xor eax, eax		; clear EAX register
	xor ebx, ebx		; clear EBX register
	xor ecx, ecx		; clear ECX register
	xor edx, edx		; clear EDX register
	
	mov ax, [esi]		; move into ax the number contained in esi
	mov bx, 2d		; divide by 2d
	
	convert_convertToBinary:	
		div bx			; div ax = ax / bx
		mov quotient, ax	; compare to quotient

		push dx			; push remainder into stack
		inc rmndrCntr		; increment number of items to pop
		
		cmp al, 0				; compare if quotient is greater than zero
		jg convert_convertToBinary		; jump back to procedure if still greater than 0

		lea esi, binaryValue	; load address of binaryValue
		lea edi, num3		; load address of num3 to use in Num2Str
		
		cmp quotient, 0			; compare if quotient IS zero
		jz convert_ToBinary		; continue to process to other bases
	
	convert_ToBinary:
		pop ax				; pop value into num 
		call Num2Str			; convert num to string ASCII
		mov bl, num3
		mov [esi], bx			; move number into esi
		inc esi				; move to next slot of array

		dec rmndrCntr			; decrement counter
		cmp rmndrCntr, 0		; compare counter to 0
		jnz convert_ToBinary		; jump if not zero

	done_convertToBinary:
		; test
		;lea esi, hexValue
		;call PrintString
		popfd		; restore flags
		popad		; restore registers
		ret		; return to caller
convertToBinary endp

; -----------------------------------------------------------------------
; convertNumToBaseGiven -> Convert to base provided
;		Input: baseNum to use to convert to base provided by user
;		Output: baseValue contains string of anwser to print out to terminal
; -----------------------------------------------------------------------
convertNumToBaseGiven proc
	pushad						; save registers
	pushfd						; save flags
	
	xor eax, eax				; clear EAX register
	xor ebx, ebx				; clear EBX register
	xor ecx, ecx				; clear ECX register
	xor edx, edx				; clear EDX register

	mov quotient, 0				; reset quotient

	add ax, [esi]				; move into ax the number contained in esi
	mov bx, baseNum				; divide by baseNum
	
	convert_convertNumToBaseGiven:	
		div bx				; div ax = ax / bx
		push dx				; push remainder into stack
		inc rmndrCntr			; increment number of items to pop
		
		cmp al, 0				; compare if quotient is greater than zero
		jg convert_convertNumToBaseGiven	; jump back to procedure if still greater than 0

		lea esi, baseValue			; load address of binaryValue
		lea edi, num3				; load address of num3 to use in Num2Str
		
		cmp ax, 0				; compare if quotient IS zero
		jz convert_NumToBaseGiven		; continue to process to other bases
	
	convert_NumToBaseGiven:
		pop ax					; pop value into num 
		call Num2Str				; convert num to string ASCII
		mov bl, num3
		mov [esi], bx				; move number into esi
		inc esi					; move to next slot of array

		dec rmndrCntr				; decrement counter
		cmp rmndrCntr, 0			; compare counter to 0
		jnz convert_NumToBaseGiven		; jump if not zero

	done_convertNumToBaseGiven:
		; test
		;lea esi, hexValue
		;call PrintString
		popfd		; restore flags
		popad		; restore registers
		ret		; return to caller
convertNumToBaseGiven endp

;------------------------------------------------------------------------------
; Num2Str -> Procedure to convert a number to a string
; Input : Give adress of number variable as well as number to convert into ECX and ESI
;			ax = contains number to convert
;			EDI = CONTAINS ADDRESS OF byte value to store string
; Output: EDI = Variable in EDI now contains number represented as a string 
;------------------------------------------------------------------------------
Num2Str proc
	pushad			; save registers
	pushfd			; save flags
	
	xor bx, bx		; clear bx register
	xor cx, cx		; clear cx register
	xor dx, dx		; clear dx register

	;mov ax, [esi]		; move into ax the esi value to produce string
	mov bx, 10h		; move into bx the number ten to use in ascii conversion

	cmp ax, 9					; compare number to greater than 9
	jg  processGreater_Num2Str			; jump if number is greater than 9

	add10_Num2Str:
		add ax, 30h				; add 30h to convert to ascii
		mov [edi], ax				; move ascii value to address in edi
		jmp done_Num2Str			; done converting value to ascii

	; number is greater than 9. Hence it is A, B, C, etc...
	; number A=41h B =42h C=43
	; subtract number from 10 and add 40h to convert to ASCII letter 
	processGreater_Num2Str:
		sub ax, 10				; subtract 10h
		add ax, 41h				; add 40h
		mov [edi], ax				; move result into AX

	done_Num2Str:
		; esi has anwser now 
		; test output here
		;mov edi, 0				; move null terminator into buffer
		;lea esi, usrStringResult		; load into esi userStringResult
		;call printString			; print result
		mov Num2StrCount, 0			; reset counter for next call

	popfd			; restore flags
	popad			; restore registers
	ret			; return to caller
Num2Str endp

;------------------------------------------------------------------------------
; Procedure to convert a null-terminated string to a unsigned word sized number
; Given  : address of null-terminated string to convert in ESI
;        : address of location to write word-sized numeric result in EDI
;------------------------------------------------------------------------------
Str2Num proc                       ; Define procedure
        pushad                     ; save registers
        pushfd                     ; save flags

	; initialize any variables and registers
	xor ax, ax					; using AX for total
	xor cx, cx					; zero out cx... used for processing characters
	lea esi, usrString
	; loop through each character
	foreachChar_str2num:
		cmp byte ptr [esi], 0			; compare character to null terminator
		jz done_str2num				; if null found, done with loop
		; total = total * base + digit
		mul bx					; multiply total (AX) times number base (BX)
		mov cl, [esi]				; move character into register
		cmp cl, '9'				; check if character is digit				
		jg isAlpha_str2num			; if not digit, jump to alpha section
		
	isDigit_str2num:
		sub cl, '0'			; convert ascii character into numeric value, i.e. '1' => 1
		jmp addToTotal_str2num  	; jump over alpha processing
	
	isAlpha_str2num:
		and cl, 0dfh			; ensure uppercase; 1101 1111
		sub cl, 'A'			; 'A'=0, 'B'=1, 'C'=2, ...
		add cl, 10			; 'A'=10, 'B'=11, 'C'=12, ...

	addToTotal_str2num:
		add ax, cx		; add number to total
		inc esi			; move pointer to next character in string
		jmp foreachChar_str2num ; continue with loop
			
	exit_str2num:
		invoke ExitProcess, 0		; exit program

	done_str2num:
	; at this point AX contains answer
	; let's store it
		lea edi, baseNum
		mov [edi], ax

		cmp baseNum, 35		   ; compare base to 'Z'
		jg exit_str2num		   ; jump if greater

            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
Str2Num endp

; -----------------------------------------------------------------------
; parseString -> Parses string 
; Input: Takes variable stored in ESI register and parses it
; Output: Result number in numeric form stored into resultCal variable
; -----------------------------------------------------------------------
parseString proc
	pushad			
    	pushfd				
	
	; initialize any variables and clear any registers
	xor ax, ax				; using AX for total
	xor bx, bx				; zero out bx if needed
	xor cx, cx				; zero out cx... used for processing characters
	
	xor esi, esi				; clear esi register
	
	call Str2NumSpace			; parse usrString

	cmp operand, '+'			; compare operand to '+' 
	jz plusOp_parseString			; jump to plus operand calculation

	cmp operand, '-'			; compare operand to '-'
	jz minusOp_parseString			; jump to minus operand calculation

	cmp operand, '/'			; compare operand to '/'
	jz divOp_parseString			; jump to division operand calculation

	cmp operand, '*'			; compare operand to '*'
	jz mulOp_parseString			; jump to division operand calculation
	
	plusOp_parseString:
		mov ax, num1			; move into ax num1
		add ax, num2			; add num1 to num2, store into ax
		mov resultCal, ax		; store result in variable
		jmp done_parseString		; jump to done
	
	minusOp_parseString:
		mov ax, num1			; move into ax num1
		sub ax, num2			; subtract ax - num1
		mov resultCal, ax		; store result
		jmp done_parseString		; jump to done
		
	divOp_parseString:
		mov ax, num1			; move num1 into ax
		mov bx, num2			; move num2 into bx
		div bx				; div ax = ax / bx
		mov resultCal, ax		; move result into variable
		jmp done_parseString		; jump to done
	
	mulOp_parseString:
		mov ax, num1			; move num1 into ax
		mov bx, num2			; move num2 into bx
		mul bx				; ax = ax * bx
		mov resultCal, ax		; store result
		jmp done_parseString		; jump to done
	
	done_parseString:
		popfd                      ; restore flags
		popad                      ; restore registers
		ret                        ; return to caller
parseString endp

;------------------------------------------------------------------------------
; Procedure to convert a space-terminated string to a unsigned word sized number
; Given  : address of space-terminated string to convert in ESI
;        : address of location to write word-sized numeric result in EDI
;------------------------------------------------------------------------------
Str2NumSpace proc                      ; Define procedure
            pushad                     ; save registers
            pushfd                     ; save flags

	; initialize any variables and registers
	xor ax, ax				; using AX for total
	xor cx, cx				; zero out cx... used for processing characters
	lea esi, usrString			; load address of string
	mov bx, baseNum				; move into bx baseNum

	; loop through each character
	foreachChar_Str2NumSpace:
					
		cmp byte ptr [esi], " "			; compare character to null terminator
		jz done_Str2NumSpace			; if null found, done with loop

		; total = total * base + digit
		mul bx					; multiply total (AX) times number base (BX)
		mov cl, [esi]				; move character into register
		cmp cl, '9'				; check if character is digit				
		jg isAlpha_Str2NumSpace			; if not digit, jump to alpha section

	isDigit_Str2NumSpace:
		sub cl, '0'				; convert ascii character into numeric value, i.e. '1' => 1
		jmp addToTotal_Str2NumSpace		; jump over alpha processing

	isAlpha_Str2NumSpace:
		and cl, 0dfh				; ensure uppercase; 1101 1111
		sub cl, 'A'				; 'A'=0, 'B'=1, 'C'=2, ...
		add cl, 10				; 'A'=10, 'B'=11, 'C'=12, ...
												
	addToTotal_Str2NumSpace:			
		add ax, cx				; add number to total
		inc esi					; move pointer to next character in string
		jmp foreachChar_Str2NumSpace		; continue with loop
			
	done_Str2NumSpace:
		; at this point AX contains answer
		; let's store it
		lea edi, num1
		mov [edi], ax
			
	inc esi;			; continue processing string, increment to operator
	mov cl, [esi]			; load operator into operator variable
	mov operand, cl			; copy cl operand into operand variable
	inc esi;			; this is a space
	inc esi;			; this is the second number, repeat procedure above but store result into num2 var
	;mov bx, baseNum	        ; move into bx baseNum
	xor eax, eax			; clear eax for next number

	; loop through each character
	foreachChar_Str2NumSpace2:
			
		cmp byte ptr [esi], 0			; compare character to null terminator
		jz done_Str2NumSpace2			; if null found, done with loop

		; total = total * base + digit
		mul bx							; multiply total (AX) times number base (BX)
		mov cl, [esi]					; move character into register
		cmp cl, '9'						; check if character is digit				
		jg isAlpha_Str2NumSpace2		; if not digit, jump to alpha section

	isDigit_Str2NumSpace2:
		sub cl, '0'						; convert ascii character into numeric value, i.e. '1' => 1
		jmp addToTotal_Str2NumSpace2	; jump over alpha processing

	isAlpha_Str2NumSpace2:
		and cl, 0dfh					; ensure uppercase; 1101 1111
		sub cl, 'A'						; 'A'=0, 'B'=1, 'C'=2, ...
		add cl, 10						; 'A'=10, 'B'=11, 'C'=12, ...

	addToTotal_Str2NumSpace2:
		add ax, cx						; add number to total
		inc esi							; move pointer to next character in string
		jmp foreachChar_Str2NumSpace2	; continue with loop
			
	done_Str2NumSpace2:
		; at this point AX contains answer
		; let's store it
		lea edi, num2
		mov [edi], ax

            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
Str2NumSpace endp

; -----------------------------------------------------------------------
; printCalcPrompt -> Prints colon out to screen
; -----------------------------------------------------------------------
printCalcPrompt proc
	pushad                     ; save registers
        pushfd                     ; save flags
			
	lea esi, colon			   ; load address of colon into esi
	call PrintString		   ; print colon to screen
 
	popfd                      ; restore flags
	popad                      ; restore registers
	ret                        ; return to caller
printCalcPrompt endp

; -----------------------------------------------------------------------
; compString -> Exits program if string in usrString is 'q' or 'Q'
;	'Q' = 81
;	'q' = 113
;	Input: Takes address of user string stored in esi
;	Output: returns to caller if not 'q' or 'Q', otherwise exits program
; -----------------------------------------------------------------------
compString proc
	pushad                     ; save registers
        pushfd                     ; save flags

	;load address of user string
	lea esi, usrString

	;if q, compare 
	;compare first character to 81 and 113
	cmp byte ptr [esi], "Q"			; compare first character of esi to Q
	jz exitProgram					; exit if "Q" - "Q" = 0, register ZR = 1
	cmp byte ptr [esi], "q"			; compare first character of esi to q
	jz exitProgram					; exit if "q" - "q" = 0, register ZR = 1
			
	;else continueProgram since string isn't "q" or "Q"
	jmp continueProgram

	exitProgram:
		invoke ExitProcess, 0		; Exits program if 'q' or 'Q'			
            
	continueProgram:
		popfd                      ; restore flags
		popad                      ; restore registers
		ret                        ; return to caller
compString endp

; -----------------------------------------------------------------------
; getUsrString -> gets string from the prompt and stores it in usrString 
; -----------------------------------------------------------------------
getUsrString proc
	pushad					; save all registers
	pushfd					; save flags
	
	xor esi, esi			; clear register
	lea esi, usrString		; get address of usrString to store in
	call getString			; get base/quit string from user							
		
	popfd
	popad
	ret
getUsrString endp

; -----------------------------------------------------------------------
; printPrompt -> Prints a welcome message to the program
; -----------------------------------------------------------------------
printPrompt proc
    pushad						; save all registers
    pushfd						; save flags
	
	lea esi, starStr			; Load address of starStr variable into esi 
	call PrintString			; Print stars into terminal
	
	lea esi, promptMsg			; Load address of promptMsg variable into esi 	
	call PrintString			; Print first prompt message line

	lea esi, promptMsg1			; Load address of promptMsg1 variable into esi
	call PrintString			; Print second prompt message line

	lea esi, promptMsg2			; Load address of promptMsg2 variable into esi
	call PrintString			; Print third prompt message line
   
	lea esi, listBase			; load address of listBase var into esi
	call PrintString			; print list of hex values

	lea esi, starStr			; Load address of starStr variable into esi
 	call PrintString			; Print stars into terminal
    
	popfd                      ; restore flags
	popad                      ; restore registers
	ret                        ; return to caller
printPrompt endp

; -----------------------------------------------------------------------
; numBasePrompt -> Prints prompt to enter number base or quit
; -----------------------------------------------------------------------
numBasePrompt proc
	pushad				; save all registers
	pushfd				; save flags
	
	lea esi, newLn
	call PrintString
	lea esi, baseStr	
	call PrintString

	popfd				; restore flags
	popad				; restore registers
	ret				; return to caller
numBasePrompt endp

;------------------------------------------------------------------------------
; Procedure to print a string to stdout
;
; Given   :  The Address of Null (0) terminated String to print in ESI register
; process :  Print the String using the kernel32.lib WriteFile to
;         :  Standard_Output function call.  No registers are changed and the
;         :  flags are not affected.
; Return  :  Nothing
;------------------------------------------------------------------------------
PrintString proc                       ; Define procedure
            pushad                     ; save registers
            pushfd                     ; save flags
            mov    strAddr, esi        ; copy string address
                                       ; find string length
            mov    strLength, 0        ; initialize string length
WhileChar:  cmp    byte ptr [esi], 0   ; character = null?
            jz     EndWhileChar        ; exit if so
            inc    strLength           ; increment character count
            inc    esi                 ; point at next character
            jmp    WhileChar           ; while more characters exist
EndWhileChar:
            invoke GetStdHandle,STD_OUTPUT ; get handle for console output
            mov    hStdOut, eax        ; copy file handle for screen
            invoke WriteFile,          ; invoke standard WriteFile with
              hStdOut,                 ;   file handle for screen
              strAddr,                 ;   address of string
              strLength,               ;   length of string
              near32 ptr written,      ;   variable for # bytes written
              0                        ;   overlapped mode
            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
PrintString endp

;------------------------------------------------------------------------------
; Procedure to get a string from stdin
;
; Given   :  The Address of the String to fill in ESI register
; process :  Input the String using the kernel32.lib ReadFile from the
;         :  Standard_Input function call.  No registers are changed and the
;         :  flags are not affected.
; Return  :  The input string in the data segment
;------------------------------------------------------------------------------
GetString proc                         ; Define procedure
            pushad                     ; save all registers
            pushfd                     ; save flags

            invoke GetStdHandle,STD_INPUT  ; get handle for console
            mov    hStdIn, eax         ; save the handle
            invoke SetConsoleMode,     ; invoke standard console with
              hStdIn,                  ;   file handle for keyboard
              DISABLE_PROCESSED_INPUT  ;   turn line buffering on

            mov    ecx, 255d;MAXSTR    ; string length
            mov    strLength, ecx      ; maximum string to accept
            mov    strAddr, esi        ; save pointer to input string
            invoke ReadFile,           ; invoke standard ReadFile with
              hStdIn,                  ;   file handle for keyboard
              strAddr,                 ;   address of string
              strLength,               ;   length of string
              near32 ptr read,         ;   variable for # bytes read
              0                        ;   overlapped mode
            mov ecx, read              ; number of bytes read
            mov byte ptr [esi+ecx-2],0 ; replace CR/LF by trailing null

            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
GetString   endp

end  ; end directive to compiler
