TITLE 5_numerical_string_validation     (5_numerical_string_validation.asm)

; ------------------------------------------------------------------------
; Author: Zachary Bochanski
; Last Modified: 2021.08.06
; Description: This program explores the use of macros by reading a user entered
; String, the string is validated by converting it to numerical values and throws
; an exception if the values are incorrect according to the requirements. (The focus of this assignment is 
; employing the use of MACROS, further refineing assembly skills, and
; on using STOSB/LODSB etc.) 
; 
; The flow of this program, based on the requriments, is as follows:
; 
; 1. Get string input
; 2. Convert to integer
; 3. Store in array
; 4. Convert to hex string
; 5. Display
; 
; To summarize, this program reads an writes strings to the console however,
; it stores and manupulates the actual numerical value.
; ------------------------------------------------------------------------


INCLUDE Irvine32.inc

; ------------------------------------------------------------------------
; DECLARE CONSTANTS - values to easily modify program
; ------------------------------------------------------------------------
ARRAY_SIZE EQU 10
NUM_BASE EQU 16


; ------------------------------------------------------------------------
; DATA SECTION - declare variable definitions
; ------------------------------------------------------------------------
.data
    ; introduction
    intro1      BYTE    "PROGRAMMING ASSIGNMENT 5: Designing low-level I/O procedures!",13,10
                BYTE    "Written by: Zach Bochanski",13,10,13,10,0
    intro2      BYTE    "Please provide 10 unsigned hexadecimal integers.",13,10
                BYTE    "Each number needs to be small enough to fit inside a 32 bit register.",13,10
                BYTE    "After you have finished inputting the raw numbers I will display a list of the integers,",13,10
                BYTE    "their sum, and their average value.",13,10,13,10,0
    
    ; get inputs
    hexPrompt   BYTE    "Please enter and unsigned hexadecimal number: ",0
    errorMsg    BYTE    "ERROR: You did not enter and unsigned hex  or your number was too large.",13,10
                BYTE    "Please try again: ",0
    numArray    DWORD   ARRAY_SIZE DUP(?)   ; array of size to store numbers.
    value       DWORD   ?
    outString   BYTE    31 DUP(?)

    ; results
    youEntered  BYTE    13,10,"You entered the following numbers:",13,10,0
    theSumIs    BYTE    13,10,"The sum of these numbers is: ",0
    theAvgIs    BYTE    13,10,"The rounded average is: ",0 
    

    ; outro message
    outro       BYTE    13,10,13,10,"Goodbye, and thanks for using this program!",13,10,0


; ------------------------------------------------------------------------
; MACROS SECTION - define macros
; ------------------------------------------------------------------------

; ------------------------------------------------------------------------
; Name: mGetString
; 
; Description: displays prompt and reads string from user
;
; Preconditons: none
; 
; Postconditions: none
; 
; Receives: array address, size, prompt address
;
; Returns: string length EAX  
; 
; ------------------------------------------------------------------------
mGetString MACRO array:REQ, arraySize:REQ, promptRef:REQ

    PUSH    ECX
    PUSH    EDX

    MOV     EDX, promptRef
    CALL    WriteString
    MOV     EDX, array      ; OFFSET where to store string
    MOV     ECX, arraySize  ; max characters to read
    CALL    ReadString

    POP     EDX
    POP     ECX

ENDM

; ------------------------------------------------------------------------
; Name: mDisplayString
; 
; Description: displays values as a string
;
; Preconditons: none
; 
; Postconditions: input must be offset
; 
; Receives: string address by reference
;
; Returns: none 
; 
; ------------------------------------------------------------------------
mDisplayString MACRO stringAddress:REQ

PUSH    EDX
MOV     EDX, stringAddress
CALL    WriteString
POP     EDX

ENDM


; ------------------------------------------------------------------------
; CODE SECTION - instructions for logic here
; ------------------------------------------------------------------------
.code
main PROC
 
    ; introduction message
    PUSH    OFFSET intro1    ; take note of bytes pushed on call stack and add n bytes to RET
    PUSH    OFFSET intro2
    CALL    introduction

    ; get user input
    MOV     EDI, OFFSET numArray
    MOV     ECX, ARRAY_SIZE
_ReadUserInput:
    
    ; read value
    PUSH    OFFSET errorMsg
    PUSH    OFFSET hexPrompt
    CALL    readVal
    MOV     value, EAX

    ; add returned int to list
    MOV     EAX, value
    MOV     [EDI], EAX          ; overwrite value in memory pointed to by EDI
    ADD     EDI, TYPE numArray  ; increment pointer by TYPE size

    LOOP    _ReadUserInput

    ; display list title
    MOV     EDX, OFFSET youEntered
    CALL    WriteString

    ; loop through num array to display values
    MOV     EDI, OFFSET numArray
    MOV     ECX, ARRAY_SIZE
    MOV     EBX, 0
_WriteValues:
    MOV     EAX, [EDI]
    MOV     value, EAX
    ADD     EDI, TYPE numArray
    ADD     EBX, EAX

    ; writeVal takes value to convert and array to store string.
    PUSH    OFFSET outString
    PUSH    value               ; pass value not reference
    CALL    writeVal

    ; handle whitespace and commas
    CMP     ECX, 1
    JE      _FinishLoop
    PUSH    EAX
    MOV     EAX, ','
    CALL    WriteChar
    MOV     EAX, ' '
    CALL    WriteChar
    POP     EAX

    _FinishLoop:
    LOOP    _WriteValues

    ; display sum title
    MOV     EDX, OFFSET theSumIs
    CALL    WriteString
    PUSH    OFFSET  outString
    PUSH    EBX
    CALL    writeVal

    ; display average
    MOV     EDX, OFFSET theAvgIs
    CALL    WriteString
    MOV     EAX, EBX
    MOV     EBX, ARRAY_SIZE
    MOV     EDX, 0
    DIV     EBX
    PUSH    OFFSET outString
    PUSH    EAX
    CALL    writeVal

    ; outro message
    PUSH    OFFSET outro
    CALL    goodBye
   
  
    Invoke ExitProcess,0    ; exit to operating system
main ENDP


; ------------------------------------------------------------------------
; Name: introduction
; 
; Description: Shows the programmer informatoin, program purpose, and directions.
;
; Preconditons: USE of EDX
; 
; Postconditions: none
; 
; Receives: intro1, 2
;
; Returns: none  
; 
; ------------------------------------------------------------------------
introduction PROC
    
    PUSH    EBP             
    MOV     EBP, ESP
    PUSH    EDX

    MOV     EDX, [EBP + 12]  ; intro1 [register + constant] to access intro variable on the stack
    CALL    WriteString
    MOV     EDX, [EBP + 8]   ; size 
    CALL    WriteString

    POP     EDX
    POP     EBP
    RET     8

introduction ENDP


; ------------------------------------------------------------------------
; Name: readVal
; 
; Description: reads "hex string" from user ana converts to numeric value, this
;               procedure used decimal notation to handle the ASCII conversion,
;               it could just as easliy us hex representation however decimal 
;               is easiest for the eyes and brain to digest.
;
; Preconditons: message strings passed in
; 
; Postconditions: must return a valid value
; 
; Receives: prompt, error message, empty memory location reference
;
; Returns: Validated numerica value
; 
; ------------------------------------------------------------------------
readVal PROC
    
    LOCAL   inputsArray[ 20 ]: BYTE, base: DWORD, decimalValue: DWORD, powerTotal: DWORD     ; LOCAL myarray[ ARRAY_SIZE ] : BYTE (room for null byte in 32 bit reg)
    PUSH    EAX
    PUSH    EBX
    PUSH    ECX
    PUSH    EDX
    PUSH    ESI

    ; initialize local variables
    MOV     EBX, 1
    MOV     base, EBX
    MOV     EBX, 0
    MOV     decimalValue, EBX

    LEA     EAX, inputsArray            ; load exact address of local array
    MOV     EBX, SIZEOF inputsArray
    MOV     ECX, [EBP + 8]              ; prompt
    mGetString  EAX, EBX, ECX
    LEA     ESI, inputsArray
    CMP     EAX, 10
    JG      _InvalidDigit
    MOV     ECX, EAX                    ; ECX = size of input string that mGetString returns, initialize loop with size of input
    CMP     ECX, 2
    JLE     _InvalidDigit
    JMP     _InitPower

_InvalidDigit:
    LEA     EAX, inputsArray            ; load exact address of local array
    MOV     EBX, SIZEOF inputsArray
    MOV     ECX, [EBP + 12]              ; error prompt
    mGetString  EAX, EBX, ECX
    LEA     ESI, inputsArray
    CMP     EAX, 10
    JG      _InvalidDigit
    MOV     ECX, EAX
    CMP     ECX, 2
    JLE     _InvalidDigit
    JMP     _InitPower

_InitPower:
    MOV     EBX, EAX
    MOV     EAX, NUM_BASE
    MOV     powerTotal, 1
    SUB     ECX, 2                      ; 1 digit = x^0, 2 digit = x^1, 3 digit = x^2... n digit = x^n-3
_CountPower:
    CMP     ECX, 2
    JE      _PowerOf1
    CMP     ECX, 1
    JE      _PowerOf0

    MOV     EDX, NUM_BASE
    MUL     EDX
    MOV     powerTotal, EAX
    JMP     _EndLoop
    _PowerOf1:
    MOV     EDX, 1
    MUL     EDX
    MOV     powerTotal, EAX
    JMP     _EndLoop
    _PowerOf0:

    _EndLoop:
    LOOP    _CountPower
    MOV     ECX, EBX

    MOV     EDX, 0
_VerifyHexLoop:
    MOV     EAX, 0
    LODSB               ; load AL with BYTE in source (ESI)

    ; check if first char is '0'
    CMP     EDX, 0
    JE      _CheckPrefix1

    ; check if second char BYTE is 'x'
    CMP     EDX, 1
    JE      _CheckPrefix2

    ; check if index is 0-9
    CMP     AL, 48      ; 0 in ASCII
    JL      _InvalidDigit
    CMP     AL, 57      ; 9 in ASCII
    JLE     _ValidNum

    ; check if index is A-F
    CMP     AL, 65      ; 65 is A in ASCII
    JL      _InvalidDigit   
    CMP     AL, 70      ; 70 is F in ASCII
    JLE      _ValidUpper

    ; check if index is a-f
    CMP     AL, 97      ; 97 is a in ASCII
    JL      _InvalidDigit
    CMP     AL, 102     ; 102 is f in ASCII
    JG      _InvalidDigit
    JMP     _ValidLower

    _CheckPrefix1:
    CMP     AL, 48
    JNE     _InvalidDigit
    JMP     _EndOfLoop

    _CheckPrefix2:
    CMP     AL, 120
    JNE     _InvalidDigit
    JMP     _EndOfLoop

    ; if valid then calculate decimal value: decValue  = decValue + valueAtIndex * base, where base = base * 16
    _ValidNum:
    SUB     AL, 48              ; index - 48
    JMP     _Valid

    _ValidLower:
    SUB     AL, 87
    JMP     _Valid

    _ValidUpper:
    SUB     AL, 55
    JMP     _Valid

    _Valid:
    PUSH    EDX
    MOV     EDX, powerTotal
    MUL     EDX
    ADD     decimalValue, EAX

    ; decrement powerTotal by factor of numeric base i.e. hex=16, dec=10 ...
    MOV     EAX, powerTotal
    MOV     EBX, NUM_BASE
    DIV     EBX
    MOV     powerTotal, EAX
    POP     EDX             ; restore edx after all multiplicaiton is complete
     
    _EndOfLoop:
    INC     EDX
    LOOP    _VerifyHexLoop

    POP     ESI
    POP     EDX
    POP     ECX
    POP     EBX
    POP     EAX
    ; return value in EAX
    MOV     EAX, decimalValue
    RET     8

readVal ENDP


; ------------------------------------------------------------------------
; Name: writeVal
; 
; Description: converts numeric value to "hex String" representation and writes to console
;
; Preconditons: integer value in parameter
; 
; Postconditions: none
; 
; Receives: int value
;
; Returns: none
; 
; ------------------------------------------------------------------------
writeVal PROC
    
    PUSH    EBP
    MOV     EBP, ESP
    PUSH    EAX
    PUSH    EBX
    PUSH    ECX
    PUSH    EDX
    PUSH    ESI
    PUSH    EDI

    ; get value
    ;MOV     EAX, [EBP + 8]  ; value passed into procedure
    
    ; set up loop counter: convert numeric value to string by dividing by desired base and summing the remainder.
    MOV     EDI, [ebp + 12]; outString = ebp +12
    CLD
    MOV     AL, '0'
    STOSB   
    MOV     AL, 'x'
    STOSB
    MOV     EAX, 0
    MOV     EAX, [EBP + 8] ; value + 8
    MOV     ECX, 0
_ConvertByte:
    MOV     EBX, NUM_BASE   ; divide value by desired base, quotient stored in EAX
    MOV     EDX, 0
    DIV     EBX

    ; convert remainder (EDX) to ASCII
    CMP     EDX, 9      ; 0 in 48 ASCII
    JLE      _IsNum
    CMP     EDX, 15     ; a-f in ASCII
    JLE     _IsLetter
    
    _IsNum:
    ADD     EDX, 48
    JMP     _AddToStack
    _IsLetter:
    ADD     EDX, 87     ; lower case
    JMP     _AddToStack
    
    _AddToStack:
    PUSH    EDX     ; store EDX on stack
    INC     ECX     ; keep track of number of loop cycles to ensure EDX is popped off the stack in the next loop
    CMP     EAX, 0  ; when quotient is zero, number is done.
    JG    _ConvertByte

_BuildString:
    POP     EDX
    MOV     EAX, EDX
    STOSB
    LOOP    _BuildString
    
    ; dispaly string and zero out memory
    mDisplayString  [EBP + 12]
    MOV     EAX, [EBP + 12]
    MOV     DWORD PTR [EAX], 0

    POP     EDI
    POP     ESI
    POP     EDX
    POP     ECX
    POP     EBX
    POP     EAX
    POP     EBP
    RET     8

writeVal ENDP


; ------------------------------------------------------------------------
; Name: goodBye
; 
; Description: Displays the goodby message.
;
; Preconditons: edx = outro message
; 
; Postconditions: none
; 
; Receives: outro
;
; Returns: none
; 
; ------------------------------------------------------------------------
goodBye PROC
    PUSH    EBP
    MOV     EBP, ESP
    MOV     EDX, [EBP + 8]
    CALL    WriteString
    POP     EBP
    RET     4
goodBye ENDP


END main
