;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               S y m C a l c                                @
;@                         (cell calculation engine)                          @
;@                                                                            @
;@             (c) 2024-2024 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- CALCULATION STACK ROUTINES -----------------------------------------------
;### STKPSH -> Adds a new entry to the stack
;### STKLST -> Get pointer to last stack entry

;--- FORMULA ROUTINES ---------------------------------------------------------
;### FORCLC -> calculates a formula
;### FOROPR -> does an operation with two values
;### FORDAT -> converts text into formula data
;### FORTXT -> converts formula data into text
;### FORREF -> converts cell reference to text
;### FORSPU -> pushes new element on stack
;### FORSPO -> pops element from stack
;### FORSLA -> copies last element from stack

;--- FUNCTION SUB ROUTINES ----------------------------------------------------
;### FNCGET -> puts single value parameter to (forclcval)
;### FNCNOL -> checks, if correct number of single parameters (no lists allowed)
;### FNCLST1 -> copies (fnclstv) to result and returns
;### FNCLST0 -> inits list function
;### FNCLST -> returns next element from list
;### FNCTGT -> reads datetime value and convert it to time values
;### FNCWPT -> converts word to FP value
;### FNC16B -> get signed 16bit value from function parameter
;### FNC08B -> get positive 8bit value from function parameter
;### FNCFOR -> calculates a formula with rules

;--- FUNCTIONS ----------------------------------------------------------------
;### FNCxxx -> executes a function
;### FNCSQR/LOG/L10/SIN/COS/TAN/ATN/EXP -> one parameter standard functions
;### FNCSGN -> returns the sign of a value
;### FNCABS -> returns the value always positive
;### FNCRND -> returns the rounded value
;### FNCFIX -> returns the fix value
;### FNCINT -> returns the int value
;### FNCPI -> returns PI
;### FNCCLM -> returns column
;### FNCROW -> returns row
;### FNCTRU -> returns TRUE
;### FNCFLS -> returns FALSE
;### FNCIF  -> returns second parameter, if first is true, otherwise returns third parameter
;### FNCAND -> returns TRUE, if all values are TRUE
;### FNCOR  -> returns FALSE, if all values are FALSE
;### FNCMIN -> returns minimum value
;### FNCMAX -> returns maximum value
;### FNCSUM -> returns sum of all values
;### FNCCNT -> returns the count of all values
;### FNCAVG -> returns the average of all values
;### FNCSTD -> returns the sample standard deviation
;### FNCDAT -> returns datetime of year,month,day
;### FNCTIM -> returns datetime of hour,minute,second
;### FNCDTM -> returns datetime of year,month,day,hour,minute,second
;### FNCDAD -> adds date to datetime
;### FNCTAD -> adds time to datetime
;### FNCMOS -> returns months between two datetimes
;### FNCYRS -> returns years between two datetimes
;### FNCDYS -> returns days between two datetimes
;### FNCHRS -> returns hours between two datetimes
;### FNCMIS -> returns minutes between two datetimes
;### FNCSCS -> returns seconds between two datetimes
;### FNCYEA/MON/DAY/HOR/MNU/SEC/WKY -> returns component (year, month, day, weekday, hour, minute, second) from datetime
;### FNCNPV -> net present value
;### FNCPV -> present value
;### FNCFV -> future value
;### FNCPMT -> periodic payment for an annuity
;### FNCRAT -> rate
;### FNCNPR -> nper
;### FNCFAD -> factorial doublestep
;### FNCFAC -> factorial

;--- COMPONENT CHECK ROUTINES -------------------------------------------------
;### CMPNUM -> tries to convert a number
;### CMPOPR -> tries to convert an operator
;### CMPFNC -> tries to convert a function
;### CMPREF -> tries to convert a reference
;### CMPRSB -> check for reference sub component (column or row)
;### CMPWRD -> check for keywords

;--- CHAR CHECK ROUTINES ------------------------------------------------------
;### CHRWHT -> skip whitespaces
;### CHRTRM -> check for terminator
;### CHRALP -> check for alphanumeric (A-Z, a-z, 0-9)
;### CHRNUM -> check for number (0-9)
;### CHRLET -> check for letter (A-Z,a-z)
;### CHRUCS -> uppercase

;--- RECALCULATION ROUTINES ---------------------------------------------------
;### RECUPD -> updates dependant cells after user changed cell
;### RECDRW -> regenerate texts and redraw all marked cells
;### RECCEL -> recalculate all marked cells
;### RECREF -> find and mark other cells, which have a reference to a cell

;---

celforval   equ 3+0
celforref   equ 3+5


fortokend   equ 0   ;end
fortokflt   equ 1   ;float
fortokdat   equ 2   ;datetime
fortokbin   equ 4   ;binary
fortokhex   equ 5   ;hexadecimal
fortokref   equ 8   ;references (8-15)
fortokbop   equ 16  ;bracket open
fortokbcl   equ 17  ;bracket close
fortoksep   equ 18  ;semicolon (function list separator)
fortokopr   equ 20  ;operators (20-47)
fortokfnc   equ 48  ;functions (48-..)

;Data header
;1B   record length (including header); <3=unused
;1W   Who points here (only if len>2), 0=unused
;     pointers will be corrected if a garbage collection takes place
;
;Number Formula
;5B   value (actual)
;4B*x Single references (column, row, 0,      0   )
;4B*y Range  references (column1,row1,column2,row2)
;xB   Formula Data
;       000     = End
;       001     = float [5B value]
;       002     = date  [5B value]
;       003     = time  [5B value]
;       004     = bin   [5B value]
;       005     = hex   [5B value]
;       006,007 = *res*
;       008     = Ref Single ColRel,RowRel  [1 byte reference index]
;       009     = Ref Single ColAbs,RowRel  [1 byte reference index]
;       010     = Ref Single ColRel,RowAbs  [1 byte reference index]
;       011     = Ref Single ColAbs,RowAbs  [1 byte reference index]
;       012     = Ref Range ColsRel,RowsRel [1 byte reference index]
;       013     = Ref Range ColsAbs,RowsRel [1 byte reference index]
;       014     = Ref Range ColsRel,RowsAbs [1 byte reference index]
;       015     = Ref Range ColsAbs,RowsAbs [1 byte reference index]
;       016,017 = Bracket Open/Close
;       018     = Semicolon (for function calls)
;       019     = *res* (named cell reference?)
;       020-047 = Operators
;       048-255 = Functions
;
;Const
;5B   value
;
;
;1.) Zelle A wurde geändert
;    - bei Formel mit alten vorhandenen Werten berechnen
;2.) alle Zellen markieren, die sich auf A direkt oder indirekt beziehen (rekursiv)
;       a) find unmarked cell B, which points to A
;       b) if nothing found, finished
;       c) call a) with B
;       d) mark cell B
;       e) goto a)
;    - falls A darunter ist -> Error, Zirkelbezug

;3.) markierte Zelle suchen mit Bezügen auf ausschließlich unmarkierte Zellen
;    - wenn es keine solche markierte Zelle gibt -> Error, Zirkelbezug
;4.) Zelle ausrechnen und demarkieren
;    - falls neuer wert -> mark for redraw
;    - wenn keine Zelle mehr markiert -> Ende, redraw cells
;5.) Goto [3]


celcntall   dw 0    ;number of all used cells
celcntrec   dw 0    ;number of marked cells for recalculation
celcntdrw   dw 0    ;number of marked cells for redraw


;==============================================================================
;### CALCULATION STACK ROUTINES ###############################################
;==============================================================================

;formula calculation stack
stklen  equ 6
stkmax  equ 42
stkmem  ds stkmax*stklen    ;1-28 operator, 29=seperator after value, 30=seperator after range, 31=(, 32-255 functions, FP value (if operator<=19)
stkpos  db 0

stktoksep   equ 29  ;separator after value
stktokser   equ 30  ;seperator after range
stktokbrk   equ 31  ;bracket open
stktokfnc   equ 32  ;start of functions

;### STKPSH -> Adds a new entry to the stack
;### Output     HL=address in stack (6 bytes)
;### Destroyed  AF,BC
stkpsh  ld hl,stkpos
        ld a,stkmax-1
        cp (hl)
        jp z,forclck
        inc (hl)
;### STKLST -> Get pointer to last stack entry
;### Output     CF=0 -> HL=pointer
;###            CF=1 -> stack empty
;### Destroyed  AF,BC
stklst  ld a,(stkpos)
        add a
        scf
        ret z
        ld c,a
        add a
        add c
        ld c,a
        ld b,0
        ld hl,stkmem-6
        add hl,bc
        ret


;==============================================================================
;### FORMULA ROUTINES #########################################################
;==============================================================================

;F O R M U L A S
;
;always start with =
;
;VALUES
;   flt start 0-9
;   bin start % then b..
;   hex start # then x..
;   dat @yyyy.m.d
;   tim _h;m;s (;=dp)
;   dtm @yyyy.m.d_h;m;s (;=dp)
;   ref start A-Z or $ (absolute reference)
;       single [$]a[a]n[n[n]]
;       range  [$]a[a]n[n[n]];[$]a[a]n[n[n]] (;=dp)
;     nxt spc/ops short/)/;/0
;
;OPERANTS
;   ops short +-*/^\ (add,sub,mul,div,pot,mod)
;       long  and,or,xor
;     nxt spc/(
;
;FUNCTIONS
;   fnc start A-Z then a../n.. then (
;      normal
;       sin,cos,tan,arc,sqrt,ln,log,abs,sgn,fac...
;      range
;       sum,average,min,max,...


errforinv   equ 0   ;invalid (component not detected)
errforsyn   equ 1   ;general syntax error
errfornum   equ 2   ;number error
errforovf   equ 3   ;number overflow
errforref   equ 4   ;reference error
errforbrk   equ 5   ;bracket error
errforlst   equ 6   ;list outside function
errforexp   equ 7   ;expression error

errclcbeg   equ 8
errclczer   equ 8   ;division/0
errclcovf   equ 9   ;overflow
errclcnum   equ 10  ;number
errclcref   equ 11  ;wrong reference
errclcstk   equ 12  ;stack overflow
errclcpar   equ 13  ;wrong number of parameters
errclcrng   equ 14  ;range can't be used here

;references
forcnvrsd   ds 16*4     ;single data (CR,0)
forcnvrrd   ds 16*4     ;range  data (C1R1,C2R2)
forcnvrsc   db 0        ;single count
forcnvrrc   db 0        ;range  count


;### FORCLC -> calculates a formula
;### Input      DE=data, HL=5B buffer, (forcnvrXX)=references
;### Output     CF=0 -> ok
;###            CF=1 -> error (A=error code 8-)
;### Destroyed  AF,BC,DE,HL,IX,IY

forclctyp   db 0    ;0=value, 1=reference
forclcval   ds 5    ;last value
forclczer   ds 5    ;null (for undefined/text cells)

forclc  ld (forclcg+1),sp
        xor a                   ;reset stack
        ld (stkpos),a
        ld (forclcc+1),hl
forclc1 ld a,(de)                       ;*** NEW EXPRESSION
        inc de
        cp fortokref
        jr nc,forclc2
        ex de,hl
        ld de,forclcval             ;** value
        ld bc,5
        ldir
        xor a
        ld (forclctyp),a
        ex de,hl
        jp forclc9
forclc2 cp fortokbop
        jr z,forclc6
        jp nc,forclc8
        bit 2,a                     ;** reference
        jr z,forclc3
        ld hl,forcnvrrd             ;* range reference
        ld a,(de)
        inc de
        add a:add a
        ld c,a
        ld b,0
        add hl,bc
        push de
        ld de,forclcval
        ld bc,4
        ldir
        ld a,1
        ld (forclctyp),a
        pop de
        ld a,(de)               ;next must be...
        cp fortokbcl            ;- bracket close
        jr z,forclcn
        cp fortoksep            ;- seperator
        jp nz,forclco
forclcn call stklst             ;previous
        jp c,forclco
        ld a,(hl)               ;previous must be...
        cp stktokfnc            ;- function start
        jr nc,forclc9
        cp stktoksep            ;- seperator value
        jr z,forclc9
        cp stktokser            ;- seperator range
        jr z,forclc9
        jp forclco
forclc3 ld hl,forcnvrsd             ;* single reference
        ld a,(de)
        inc de
        add a:add a
        ld c,a
        ld b,0
        add hl,bc
        ld c,(hl):inc hl
        ld h,(hl):ld l,c        ;l,h=cell
        push de
        call fldcel
        jr c,forclc5            ;no cell -> use zero
        ld a,(hl)
        and 63
        cp celtyptxt
        ld a,errclcref
        jp nc,forclcg           ;reference is text or error -> error
        ld bc,celrecdat
        add hl,bc
        ld c,(hl):inc hl
        ld h,(hl):ld l,c        ;get cell data
        inc hl:inc hl:inc hl
forclc4 ld de,forclcval         ;copy cell value
        ld bc,5
        ldir
        xor a
        ld (forclctyp),a
        pop de
        jr forclc9
forclc5 ld hl,forclczer
        jr forclc4
forclc6 call stkpsh                 ;** bracked open
        ld a,stktokbrk
forclc7 ld (hl),a
        jp forclc1
forclc8 push af                     ;** function
        call stkpsh
        pop af
        add stktokfnc-fortokfnc
        ld (hl),a
        ld a,(de)               ;special case -> check if function without parameters
        cp fortokbcl
        jp nz,forclc1
        inc de
        push de                 ;yes, execute it
        jp forclcq
forclc9 ld a,(de)                       ;*** BEHIND EXPRESSION
        inc de
        cp fortokopr
        jr c,forclcb
        add 1-fortokopr             ;** operator
        ld b,a
        call forclca            ;calculate until end/low/sep/brk/fnc
        push bc                 ;push operator + last value
        call stkpsh
        pop bc
        ld (hl),b
forclcf push de
        call forclcj
        pop de
        jp forclc1
forclcb or a
        jr nz,forclcd
        ld b,0                      ;** end reached
        call forclca
        ld hl,forclcval
forclcc ld de,0
        ld bc,5
        ldir
        or a
        ret
forclcd cp fortokbcl
        jr nz,forclce
        ld b,0                      ;** bracket close
        call forclca
        cp stktokbrk
        jr nz,forclci           ;before is function or seperator -> execute function
        ld hl,stkpos
        dec (hl)
        jr forclc9
forclce ld b,0                      ;** separator
        call forclca
        call stkpsh
        ld a,(forclctyp)
        add stktoksep
        ld (hl),a
        jr forclcf
forclcj inc hl
        ex de,hl
        ld hl,forclcval
        ld bc,5
        ldir
        ret
;(forclcval),stack until function -> function parameter
forclci call stkpsh
        push de
        ld a,(forclctyp)
        add stktoksep
        ld (hl),a
        call forclcj
forclcq ld e,-1
forclcl inc e
        call stklst
        ld a,(hl)
        push hl
        ld hl,stkpos
        dec (hl)
        pop hl
        sub stktokfnc
        jr c,forclcl
        ld bc,6
        add hl,bc
        add a               ;hl=stack with first parameter, e=number of parameter, a=function (0-x)
        ld c,a
        ld b,0
        ld a,e              ;a=number of parameters (1-x)
        ex de,hl            ;de=first parameter on stack
        ld hl,fnctab
        add hl,bc
        ld c,(hl):inc hl
        ld h,(hl):ld l,c
        ld bc,forclcm
        push bc
        jp (hl)
forclcm pop de
        jp c,forclc9
forclch ld a,errclczer:jr z,forclcg     ;** float error
        ld a,errclcovf:jp pe,forclcg
forclcp ld a,errclcnum
forclcg ld sp,0
        scf
        ret
forclck ld a,errclcstk:jr forclcg
forclco ld a,errclcrng:jr forclcg
;b=new operator -> calculate with stack entries until emp/sep/bop/fnc reached -> forclcval=result
forclca push bc
        call stklst
        pop bc                  ;b=new operator (0-)
        ret c                   ;-> stack empty
        ld a,(hl)               ;a=last operator
        cp stktoksep
        ret nc                  ;-> stack is sep val/sep rng/brkopn/fnc
        cp b
        ret c                   ;-> stack has lower operator
        push bc
        push de
        call foropr
        jr nc,forclch
        pop de
        pop bc
        jr forclca

;### FOROPR -> does an operation with two values
;### Input      (HL+1)=value1, forclcval=value2, (HL)=operator (1-28)
;### Output     forclcval=result, CF=0 error (CPC float error system), (stkpos)--
;### Destroyed  AF,BC,DE,HL,IX,IY
foroprjmp
dw fopbor,fopban                                ;01-02
dw foplwr,foplwe,fopgrt,fopgre,fopequ,fopunl    ;03-08
dw fopor ,fopxor,fopand,fopnan                  ;09-12
dw fopadd,fopsub,fopmul,fopdiv,fopmod,foppot    ;13-18

foropr  ld a,(hl)
        dec a
        inc hl
        ld de,forclcval
        add a
        ld c,a:ld b,0
        push hl
        ld hl,foroprjmp
        add hl,bc
        ld c,(hl):inc hl
        ld b,(hl)
        pop hl
        push bc
        ret

;*** COMPARISON OPERATORS *****************************************************

foplwr  call FLO_VGL        ;<
        jr c,fopcmp1
        jr fopcmp0
foplwe  call FLO_VGL        ;<=
        jr z,fopcmp1
        jr c,fopcmp1
        jr fopcmp0
fopgrt  call FLO_VGL        ;>
        jr z,fopcmp0
        jr nc,fopcmp1
        jr fopcmp0
fopgre  call FLO_VGL        ;>=
        jr nc,fopcmp1
        jr fopcmp0
fopequ  call FLO_VGL        ;=
        jr z,fopcmp1
        jr fopcmp0
fopunl  call FLO_VGL        ;<>
        jr nz,fopcmp1
        jr fopcmp0

fopcmp1 ld hl,FLO_CONST_1:jp foropr0
fopcmp0 ld hl,FLO_CONST_0:jp foropr0

;*** BOOLEAN OPERATORS ********************************************************

fopban  call fopbor1        ;&&
        cp 1
        sbc a
        cpl
        and (hl)
        jr fopbor0
fopbor  call fopbor1        ;||
        or (hl)
fopbor0 jr z,fopcmp0
        jr fopcmp1
fopbor1 ld bc,4
        add hl,bc
        ex de,hl
        add hl,bc
        ld a,(de)
        ret

;*** BITWISE OPERATORS ********************************************************

fopbita ds 5
fopbitb ds 4

fopor   db #3e:or  (hl):call fopbit:jr fopbit0      ;OR
fopxor  db #3e:xor (hl):call fopbit:jr fopbit0      ;XOR
fopand  db #3e:and (hl):call fopbit:jr fopbit0      ;AND
fopnan  db #3e:and (hl):call fopbit                 ;NAND
        ld hl,fopbita
        ld b,4
fopnan1 ld a,(hl)
        cpl
        ld (hl),a
        inc hl
        djnz fopnan1

fopbit0 ld a,(fopbita+3)        ;convert LW+sign to FLT
        add a
        sbc a
        jr z,fopbit3
        ld hl,(fopbita+0)
        ld de,(fopbita+2)
        call cnvbin2
        ld (fopbita+0),hl
        ld (fopbita+2),de
        ld a,-1
fopbit3 ld hl,fopbita
        call FLO_KONV_LW_TO_FLO
        jr foropr0

fopbit  ld (fopbit2+1),a        ;get both parameters and do bit-operations
        push hl
        call fopbit1
        ld (fopbita+0),hl
        ld (fopbita+2),de
        pop de
        call fopbit1
        ld (fopbitb+0),hl
        ld (fopbitb+2),de
        ld hl,fopbita
        ld de,fopbitb
        ld b,4
fopbit2 ld a,(de)
        or (hl)
        ld (hl),a
        inc hl
        inc de
        djnz fopbit2
        ret

fopbit1 ld hl,cnvfltb           ;convert FLT to LW+sign
        call FLO_MOVE
        call FLO_FIX_FLO_TO_LW
        ld hl,(cnvfltb+0)
        ld de,(cnvfltb+2)
        bit 7,b
        ret z
        jp cnvbin2

;*** MATH OPERATORS ***********************************************************

fopmod  ;...         :jr foropr1    ;\ (modulo)

fopadd  call FLO_ADD :jr foropr1    ;+
fopsub  call FLO_SUB :jr foropr1    ;-
fopmul  call FLO_MULT:jr foropr1    ;*
fopdiv  call FLO_DIV :jr foropr1    ;/
foppot  call FLO_POT                ;^

foropr0 scf
foropr1 push af
        ld de,forclcval
        ld bc,5
        ldir
        ld hl,stkpos
        dec (hl)
        pop af
        ret

;### FORDAT -> converts text into formula data
;### Input      IY=text, IX=data, (forcnvrXX)=reference buffer
;### Output     CF=0 -> A=data length, (forcnvrXX) filled
;###            CF=1 -> error (A=error code 0-7)
;### Destroyed  F,BC,DE,HL,IX,IY
forstkbrk   equ 1
forstkfnc   equ 2

forstkmax   equ 32
forstkmem   ds forstkmax
forstkpos   db 0,0

fordatfmt   dw 0

fordat  xor a
        ld (forstkpos),a
        ld (forcnvrsc),a
        ld (forcnvrrc),a
        ld (fordata+1),ix
        ld hl,fmtnfl
        ld (fordatfmt),hl

fordat1 call chrwht                 ;*** NEW EXPRESSION
fordatb cp "("                  ;bracket open?
        jr nz,fordat3
        ld (ix+0),fortokbop
        inc ix
        inc iy
        ld a,forstkbrk
fordat2 call forspu
        jr fordat1
fordat3 call cmpnum             ;number?
        jr nc,fordat4
        cp errforovf:scf:ret z
        cp errfornum:scf:ret z
        call cmpref             ;reference?
        jr nc,fordat4
        cp errforref:scf:ret z
        call cmpfnc             ;function?
        ld a,errforsyn
        ret c
        ld a,forstkfnc
        call forspu
        call chrwht             ;special case -> check if function without parameters
        cp ")"
        jr nz,fordatb
        jr fordat5
fordat4 call chrwht                 ;*** BEHIND EXPRESSION
        cp ")"
        jr z,fordat5
        cp ";"
        jr z,fordat6
        or a
        jr z,fordat8
        call cmpopr             ;operator?
        jr nc,fordat1
        ld a,errforexp
        ret
fordat5 call forspo             ;bracket closed
        jr c,fordat9
        ld (ix+0),fortokbcl
        inc ix
        inc iy
        jr fordat4
fordat9 ld a,errforbrk
        ret
fordat6 call forsla             ;next function list element
        jr c,fordat7
        cp forstkfnc
        jr nz,fordat7
        ld (ix+0),fortoksep
        inc ix
        inc iy
        jr fordat1
fordat7 scf
        ld a,errforlst
        ret
fordat8 call forsla             ;end reached
        ccf
        jr c,fordat9
        push ix
fordata ld bc,0
        pop hl
        ld (hl),0
        inc hl
        sbc hl,bc
        ld a,l
        ld hl,(fordatfmt)
        ret

;### FORTXT -> converts formula data into text
;### Input      IX=data, IY=text, (forcnvrXX)=references
;### Output     A=text length (without 0term)
;### Destroyed  F,BC,DE,HL,IX,IY
fortxtchr   db "();"

fortxt  ld (fordata+1),iy
fortxt1 ld a,(ix+0)
        or a
        jr nz,fortxt2
        push iy                 ;end reached
        jr fordata
fortxt2 inc ix
        cp fortokflt
        jr nz,fortxt3
        push ix:pop de          ;float
        ld hl,256*254+0
        push de
        push iy
        call cnvstr
fortxtb pop de
        ld hl,cnvstrtxt
        dec c
fortxtd ldir
        push de:pop iy
        pop ix
        ld c,5
        add ix,bc
        jr fortxt1
fortxt3 cp fortokdat
        jr nz,fortxt4
        push ix:pop de          ;datetime
        push de
        push iy
        call timedt
        jr fortxtb
fortxt4 cp fortokhex
        ld bc,"#"*256+#e0
        jr z,fortxta
        cp fortokbin
        jr nz,fortxtc
        ld bc,"%"*256+#c0
fortxta push ix:pop de          ;bin/hex
        push de
        push iy
        ld a,c
        ld h,7
        push bc
        call cnvbin
        pop af
        pop de
        ld hl,cnvstrtxtp
        ld (hl),a
        jr fortxtd
fortxtc cp fortokref+8
        jr nc,fortxt5
        sub fortokref           ;reference
        ld c,a
        and 3
        ld (forref+1),a
        ld a,(ix+0)
        inc ix
        add a:add a
        bit 2,c
        ld c,a
        ld b,0
        ld hl,forcnvrsd
        jr z,fortxt9
        ld hl,forcnvrrd
fortxt9 add hl,bc
        push af
        call forref
        pop af
        jp z,fortxt1
        ld (iy+0),":"
        inc iy
        call forref
        jp fortxt1
fortxt5 cp fortokopr
        jr nc,fortxt6
        ld l,a                  ;bracket, seperator
        ld h,0
        ld bc,fortxtchr-fortokbop
        add hl,bc
        ld a,(hl)
        ld (iy+0),a
        inc iy
        jp fortxt1
fortxt6 cp fortokfnc
        ld hl,cmpoprwrd         ;operator
        jr c,fortxt7
        ld hl,fncwrd            ;function
fortxt7 ld c,a
fortxt8 ld e,(hl):inc hl
        ld d,(hl):inc hl
        ld a,e:or d
        jp z,fortxt1
        ld a,(de)
        cp c
        jr nz,fortxt8
        ex de,hl
        inc hl
        ld c,(hl)
        ld b,0
        inc hl
        push iy:pop de
        ldir
        push de:pop iy
        jp fortxt1

;### FORREF -> converts cell reference to text
;### Input      (HL)=reference, (forref+1)=abs flags, IY=text
;### Output     IY=next, HL+=2
;### Destroyed  AF,BC,DE
forref  ld a,0
        ld e,(hl):inc hl
        ld d,(hl):inc hl
        push hl
        push iy:pop hl
        call forref1
        push af
        ld a,e
        inc a
        call fldcor6
        pop af
        call forref1
        ld a,d
        inc a
        call cnv08s
        push hl:pop iy
        pop hl
        ret
forref1 rra
        ret nc
        ld (hl),"$"
        inc hl
        ret

;### FORSPU -> pushes new element on stack
;### Input      A=element
;### Destroyed  AF,BC,HL
forspu  ld hl,forstkpos
        ld c,(hl)
        inc (hl)
        ld b,0
        ld hl,forstkmem
        add hl,bc
        ld (hl),a
        ld a,c
        cp forstkmax
        ;jr z,...error stack overflow ##!!##
        ret

;### FORSPO -> pops element from stack
;### Output     CF=0 -> A=element
;###            CF=1 -> nothing on stack
;### Destroyed  F,BC,HL
forspo  call forsla
        ret c
        ld hl,forstkpos
        dec (hl)
        ret

;### FORSLA -> copies last element from stack
;### Output     CF=0 -> A=element
;###            CF=1 -> nothing on stack
;### Destroyed  F,BC,HL
forsla  ld hl,(forstkpos)
        inc l:dec l
        scf
        ret z
        ld bc,forstkmem-1
        add hl,bc
        ld a,(hl)
        ret


;==============================================================================
;### FUNCTION SUB ROUTINES ####################################################
;==============================================================================

fncwrd
dw fncwrd00,fncwrd01,fncwrd02,fncwrd03,fncwrd04,fncwrd05,fncwrd06,fncwrd07,fncwrd08,fncwrd09
dw fncwrd10,fncwrd11,fncwrd12,fncwrd13,fncwrd14,fncwrd15,fncwrd16,fncwrd17,fncwrd18,fncwrd19
dw fncwrd20,fncwrd21,fncwrd22,fncwrd23,fncwrd24,fncwrd25,fncwrd26,fncwrd27,fncwrd28,fncwrd29
dw fncwrd30,fncwrd31,fncwrd32,fncwrd33,fncwrd34,fncwrd35,fncwrd36,fncwrd37,fncwrd38,fncwrd39
dw fncwrd40,fncwrd41,fncwrd42,fncwrd43,fncwrd44,fncwrd45,fncwrd46,fncwrd47,fncwrd48,fncwrd49
dw fncwrd50,fncwrd51,fncwrd52,fncwrd53,fncwrd54
dw 0

fncwrd00    db fortokfnc+00,5,"SQRT("
fncwrd01    db fortokfnc+01,3,"LN("
fncwrd02    db fortokfnc+02,6,"LOG10("
fncwrd03    db fortokfnc+03,4,"SIN("
fncwrd04    db fortokfnc+04,4,"COS("
fncwrd05    db fortokfnc+05,4,"TAN("
fncwrd06    db fortokfnc+06,5,"ATAN("
fncwrd07    db fortokfnc+07,4,"EXP("
fncwrd08    db fortokfnc+08,3,"PI("
fncwrd09    db fortokfnc+09,5,"SIGN("
fncwrd10    db fortokfnc+10,4,"ABS("

fncwrd11    db fortokfnc+11,7,"COLUMN("
fncwrd12    db fortokfnc+12,4,"ROW("

fncwrd13    db fortokfnc+13,4,"MIN("
fncwrd14    db fortokfnc+14,4,"MAX("
fncwrd15    db fortokfnc+15,4,"SUM("
fncwrd16    db fortokfnc+16,8,"AVERAGE("
fncwrd17    db fortokfnc+17,6,"COUNT("
fncwrd18    db fortokfnc+18,6,"STDEV("

fncwrd19    db fortokfnc+19,4,"INT("
fncwrd20    db fortokfnc+20,4,"FIX("
fncwrd21    db fortokfnc+21,6,"ROUND("

fncwrd22    db fortokfnc+22,4,"AND("
fncwrd23    db fortokfnc+23,3,"OR("
fncwrd24    db fortokfnc+24,5,"TRUE("
fncwrd25    db fortokfnc+25,6,"FALSE("
fncwrd26    db fortokfnc+26,3,"IF("

fncwrd27    db fortokfnc+27,5,"DATE("
fncwrd28    db fortokfnc+28,5,"TIME("
fncwrd29    db fortokfnc+29,9,"DATETIME("
fncwrd30    db fortokfnc+30,5,"YEAR("
fncwrd31    db fortokfnc+31,6,"MONTH("
fncwrd32    db fortokfnc+32,4,"DAY("
fncwrd33    db fortokfnc+33,8,"WEEKDAY("
fncwrd34    db fortokfnc+34,8,"WEEKNUM("    ;##!!## todo
fncwrd35    db fortokfnc+35,5,"HOUR("
fncwrd36    db fortokfnc+36,7,"MINUTE("
fncwrd37    db fortokfnc+37,7,"SECOND("
fncwrd38    db fortokfnc+38,9,"DATE_ADD("
fncwrd39    db fortokfnc+39,9,"TIME_ADD("
fncwrd40    db fortokfnc+40,6,"YEARS("
fncwrd41    db fortokfnc+41,7,"MONTHS("
fncwrd42    db fortokfnc+42,5,"DAYS("
fncwrd43    db fortokfnc+43,6,"HOURS("
fncwrd44    db fortokfnc+44,8,"MINUTES("
fncwrd45    db fortokfnc+45,8,"SECONDS("

fncwrd46    db fortokfnc+46,3,"PV("
fncwrd47    db fortokfnc+47,4,"PMT("
fncwrd48    db fortokfnc+48,3,"FV("
fncwrd49    db fortokfnc+49,5,"RATE("
fncwrd50    db fortokfnc+50,5,"NPER("
fncwrd51    db fortokfnc+51,4,"NPV("

fncwrd52    db fortokfnc+52,5,"FACT("
fncwrd53    db fortokfnc+53,11,"FACTDOUBLE("
fncwrd54    db fortokfnc+54,4,"NOT("

fnctab
dw fncsqr,fnclog,fncl10,fncsin,fnccos,fnctan,fncatn,fncexp,fncpi ,fncsgn,fncabs
dw fncclm,fncrow
dw fncmin,fncmax,fncsum,fncavg,fnccnt,fncstd
dw fncint,fncfix,fncrnd
dw fncand,fncor ,fnctru,fncfls,fncif
dw fncdat,fnctim,fncdtm,fncyea,fncmon,fncday,fncwkd,fncwkn,fnchor,fncmnu,fncsec,fncdad,fnctad
dw fncyrs,fncmos,fncdys,fnchrs,fncmis,fncscs
dw fncpv ,fncpmt,fncfv ,fncrat,fncnpr,fncnpv
dw fncfac,fncfad
dw fncnot

fncwkn  ;...WEEKNUM
        jp forclcp

fncpos      db 0,0          ;column, row

;### FNCGET -> puts single value parameter to (forclcval)
;### Input      (DE)=stack, A=number of values
;### Output     (forclcval)=value, HL=forclcval
fncget  cp 1
fncget1 ld a,errclcpar
        jp nz,forclcg
        ld a,(de)
        cp stktoksep
        jp nz,forclco
        inc de
fncget0 ld hl,forclcval
        jp FLO_MOVE

;### FNCNOL -> checks, if correct number of single parameters (no lists allowed)
;### Input      IXL=required number of parameters, (DE)=stack
;### Output     (DE)=stack
;### Destroyed  AF,BC,HL,IXL
fncnol  cp ixl:jr nz,fncget1
        ld l,e:ld h,d
        ld bc,6
        ld a,stktoksep
fncnol1 cp (hl)
        jp nz,forclco       ;no sep -> range not allowed here
        add hl,bc
        dec ixl
        jr nz,fncnol1
        ret

;### FNCLST1 -> copies (fnclstv) to result and returns
fnclst1 ld de,fnclstv
        jp fncget0

;### FNCLST0 -> inits list function
;### Input      A=number of parameters, DE=first parameter on stack
;### Output     (fnclstv)=first value, HL=fnclstv
fnclstv ds 5
fnclstc db 0
fnclst0 inc a
        ld (fnclstc),a
        dec a
        ld a,errclcpar
        jp z,forclcg
        xor a
        ld (fnclstmod),a
        ld hl,-6
        add hl,de
        ld (fnclstadr),hl
        call fnclst
        jp z,forclcp
        jp FLO_MOVE

;### FNCLST -> returns next element from list
;### Output     ZF=0 -> (DE)=next element, HL=fnclstv
;###            ZF=1 -> finished
fnclstadr   dw 0
fnclstmod   db 0    ;0=single, 1=range
fnclstbeg   db 0    ;first   column
fnclstcur   db 0,0  ;current range cell
fnclstend   db 0,0  ;last    range cell

fnclst  ld a,(fnclstmod)
        or a
        jr nz,fnclst2
fnclst6 ld hl,fnclstc       ;** single value
        dec (hl)
        ret z
        ld hl,(fnclstadr)
        ld de,6
        add hl,de
        ld (fnclstadr),hl
        ld a,(hl)
        inc hl
        sub stktokser
        jr z,fnclst7
        ex de,hl                ;zf=0
fnclst4 ld hl,fnclstv
        ret
fnclst7 inc a                   ;switch to range
        ld (fnclstmod),a
        ld a,(hl)
        ld (fnclstbeg),a
        ld de,fnclstcur
        ld bc,4
        ldir
        ld hl,(fnclstcur)
        jr fnclst5
fnclst2 ld hl,(fnclstcur)   ;** range value
        inc l                   ;increase range cell
        ld a,(fnclstend+0)
        cp l
        jr nc,fnclst3
        ld a,(fnclstbeg)
        ld l,a
        inc h
        ld a,(fnclstend+1)
        cp h
        jr nc,fnclst3
        xor a                   ;switch back to single
        ld (fnclstmod),a
        jr fnclst6
fnclst3 ld (fnclstcur),hl
fnclst5 call fldcel             ;try next cell
        jr c,fnclst2
        ld a,(hl)
        and 63
        cp celtyptxt
        jr nc,fnclst2
        ld de,celrecdat         ;zf=0
        add hl,de
        ld e,(hl):inc hl
        ld d,(hl)
        inc de:inc de:inc de
        jr fnclst4

;### FNCTGT -> reads datetime value and convert it to time values
;### Input      (DE)=stack, A=number of values
;### Output     A=second, B=minute, C=hour, D=day (starting from 1), E=month (starting from 1), HL=year
fnctgt  call fncget
fnctgt1 call FLO_FIX_FLO_TO_LW
        ld hl,(forclcval+0)
        ld de,(forclcval+2)
        jp timget

;### FNCWPT -> converts word to FP value
;### Input      HL=unsigned value
;### Output     (forclcval)=FP value
fncwpt0 ld h,0
fncwpt  xor a
fncwpt1 ld de,forclcval
        call FLO_KONV_HLA_TO_FLO
        scf
        ret

;### FNC16B -> get signed 16bit value from function parameter
;### Input      (DE)=stack
;### Output     (DE)=next, HL=value
fnc16bb ds 5

fnc16b  inc de
        push de
        ld hl,fnc16bb
        call FLO_MOVE
        call FLO_FIX_FLO_TO_LW:jp nc,forclch
        ld hl,(fnc16bb+0)
        ld de,(fnc16bb+2)
        ld a,e:or d
        pop de
        jr nz,fnc16b3
        bit 7,b
        jr z,fnc16b1
        xor a:sub l:ld l,a
        sbc a:sub h:ld h,a
fnc16b1 ex de,hl
        ld bc,5
        add hl,bc
        ex de,hl
        ret
fnc16b2 pop de
fnc16b3 ld a,-1
        or a
        jp forclch

;### FNC08B -> get positive 8bit value from function parameter
;### Input      (DE)=stack, C=min, B=max+1
;### Output     (DE)=next, A=value
fnc08b  call fnc16b
        inc h:dec h
        ld a,l
        ret z
        jr fnc16b3

;### FNCFOR -> calculates a formula with rules
;### Input      A=parameter count, DE=function parameter stack, IY=rules
;### Rule       00xxxyyy         -> copy paraXXX to buffYYY
;###            10xxxyyy,FNC     -> calculate buffXXX (+ buffYYY) using funcFNC
;###            110??yyy,CST     -> copy cnstCST to buffYYY
;###            111??yyy         -> bufferYYY is result
fncforstk   dw 0
fncforbuf   ds 8*6

fncfor  ld ixl,a
        call fncnol
        inc de
        ld (fncforstk),de
fncfor1 ld a,(iy+0)
        inc iy
        call fncfor2        ;de=buffer
        bit 4,a                 ;*** parameter to buffer
        jr nz,fncfor4
        ld hl,(fncforstk)
        call fncfor3        ;hl=destination,de=source
fncfor7 call FLO_MOVE
        jr fncfor1
fncfor4 bit 3,a                 ;*** buffer1 with buffer2
        jr nz,fncfor6
        call fncfor2        ;hl=par2,de=par1
        ex de,hl            ;hl=par1,de=par2
        ld bc,fncfor5
        push bc
        ld c,(iy+0):inc iy
        ld b,(iy+0):inc iy
        ld (fncfor5+2),iy
        push bc
        ret
fncfor5 ld iy,0
        jp nc,forclch
        jr fncfor1
fncfor6 bit 2,a
        jp nz,fncget0           ;*** buffer is result
        ld l,(iy+0):inc iy      ;*** constant to buffer
        ld h,(iy+0):inc iy
        ex de,hl
        jr fncfor7
;a=?yyyyxxx -> de=buffer, a=xxx?yyyy
fncfor2 ld hl,fncforbuf
fncfor3 push af
        and #7
        add a
        ld c,a
        add a
        add c
        ld c,a
        ld b,0
        add hl,bc
        ex de,hl
        pop af
        rrca:rrca:rrca
        ret


;==============================================================================
;### FUNCTIONS ################################################################
;==============================================================================

;### FNCxxx -> executes a function
;### Input      (DE)=parameters on stack (1B type, 5B value), A=number of parameters
;### Output     (forclcval)=result, CF=0 error (CPC float error system)
;### Destroyed  AF,BC,DE,HL,IX,IY

fncbuf  ds 5
fncbuf1 ds 5
fncbuf2 ds 5
fncbuf3 ds 5


;*** ONE PARAMETER STANDARD FUNCTIONS *****************************************

;### FNCSQR/LOG/L10/SIN/COS/TAN/ATN/EXP -> one parameter standard functions
fncsqr  call fncget:jp FLO_SQR      ;### FNCSQR -> returns square root
fnclog  call fncget:jp FLO_LOG_NAT  ;### FNCLOG -> returns natural logarythm
fncl10  call fncget:jp FLO_LOG_DEC  ;### FNCL10 -> returns decimal logarythm
fncsin  call fncget:jp FLO_SIN      ;### FNCSIN -> returns sinus
fnccos  call fncget:jp FLO_COS      ;### FNCCOS -> returns cosinus
fnctan  call fncget:jp FLO_TAN      ;### FNCTAN -> returns tangent
fncatn  call fncget:jp FLO_ARC_TAN  ;### FNCATN -> returns arcus tangent
fncexp  call fncget:jp FLO_POT_E    ;### FNCEXP -> returns e^value

;### FNCSGN -> returns the sign of a value
;### Parameter  1=value
;### Output     value=0 -> 0, value>0 -> 1, value<0 -> -1
fncsgn  call fncget
        call FLO_SGN
        ld de,FLO_CONST_0
        jp z,fncget0
        ld de,FLO_CONST_1NEG
        jp c,fncget0
        ld de,FLO_CONST_1
        jp fncget0

;### FNCABS -> returns the value always positive
;### Parameter  1=value
;### Output     value>=0 -> value, value<0 -> -value
fncabs  call fncget
        call FLO_SGN
        jr z,fncabs1
        jr nc,fncabs1
        call FLO_VZW
fncabs1 scf
        ret

;### FNCRND -> returns the rounded value
;### Parameter  1=value
;### Output     round(value)
fncrnd  call fncget
        call FLO_ROUND_FLO_TO_LW
fncrnd1 ret nc
        ld a,b
        jp FLO_KONV_LW_TO_FLO

;### FNCFIX -> returns the fix value
;### Parameter  1=value
;### Output     fix(value)
fncfix  call fncget
        call FLO_FIX_FLO_TO_LW
        jr fncrnd1

;### FNCINT -> returns the int value
;### Parameter  1=value
;### Output     int(value)
fncint  call fncget
        call FLO_INT_FLO_TO_LW
        jr fncrnd1


;*** CONSTANTS ****************************************************************

;### FNCPI -> returns PI
;### Parameter  -
;### Output     PI
fncpi   or a:jp nz,fncget1
        ld de,FLO_CONST_PI
        jp fncget0

;### FNCCLM -> returns column
;### Parameter  -
fncclm  or a:jp nz,fncget1
        ld a,(fncpos+0)
        jr fncrow1

;### FNCROW -> returns row
;### Parameter  -
fncrow  or a:jp nz,fncget1
        ld a,(fncpos+1)
fncrow1 ld l,a
        ld h,0
        inc hl
        ld de,forclcval
        call FLO_KONV_HLA_TO_FLO
        scf
        ret


;*** BOOLEAN ******************************************************************

;### FNCTRU -> returns TRUE
;### Parameter  -
;### Output     1
fnctru  or a:jp nz,fncget1
fnctru0 ld de,FLO_CONST_1
        jp fncget0

;### FNCFLS -> returns FALSE
;### Parameter  -
;### Output     0
fncfls  or a:jp nz,fncget1
fncfls0 ld de,FLO_CONST_0
        jp fncget0

;### FNCNOT  -> returns FALSE, if condition is true, otherwise TRUE
;### Parameter  1=condition
;### Output     !condition
fncnot  call fncget
        ld de,4
        add hl,de
        ld a,(hl)
        or a
        jr nz,fncfls0
        jr fnctru0

;### FNCIF  -> returns second parameter, if first is true, otherwise returns third parameter
;### Parameter  1=condition, 2=value for true, 3=value for false
fncif   ld ixl,3
        call fncnol
        ld hl,5
        add hl,de
        ld a,(hl)
        inc hl:inc hl
        ex de,hl
        or a
        jp nz,fncget0
        ld hl,6
        add hl,de
        ex de,hl
        jp fncget0

;### FNCAND -> returns TRUE, if all values are TRUE
;### Parameter  [list]
fncand  call fnclst0
        ld de,fnclstv
fncand1 ld hl,4
        add hl,de
        ld a,(hl)
        or a
        jr z,fncfls0
        call fnclst
        jr nz,fncand1
        jr fnctru0

;### FNCOR  -> returns FALSE, if all values are FALSE
;### Parameter  [list]
fncor   call fnclst0
        ld de,fnclstv
fncor1  ld hl,4
        add hl,de
        ld a,(hl)
        or a
        jr nz,fnctru0
        call fnclst
        jr nz,fncor1
        jr fncfls0


;*** LIST FUNCTIONS ***********************************************************

;### FNCMIN -> returns minimum value
;### Parameter  [list]
fncmin  call fnclst0
fncmin1 call fnclst
        jp z,fnclst1
        call FLO_VGL
        dec a
        call z,FLO_MOVE
        jr fncmin1

;### FNCMAX -> returns maximum value
;### Parameter  [list]
fncmax  call fnclst0
fncmax1 call fnclst
        jp z,fnclst1
        call FLO_VGL
        inc a
        call z,FLO_MOVE
        jr fncmax1

;### FNCSUM -> returns sum of all values
;### Parameter  [list]
fncsum  call fnclst0
fncsum1 call fnclst
        jp z,fnclst1
        call FLO_ADD
        jr c,fncsum1
        ret

;### FNCCNT -> returns the count of all values
;### Parameter  [list]
fnccnt  call fnclst0
        ld hl,0
fnccnt1 push hl
        call fnclst
        pop hl
        inc hl
        jr nz,fnccnt1
        xor a
        ld de,fnclstv
        call FLO_KONV_HLA_TO_FLO
        jp fnclst1

;### FNCAVG -> returns the average of all values
;### Parameter  [list]
fncavgc dw 0

fncavg  ld hl,1
        ld (fncavgc),hl
        call fnclst0
fncavg1 call fnclst
        jr z,fncavg2
        call FLO_ADD
        ld hl,(fncavgc)
        inc hl
        ld (fncavgc),hl
        jr c,fncavg1
        ret
fncavg2 ld hl,(fncavgc)
        xor a
        ld de,fncbuf
        push de
        call FLO_KONV_HLA_TO_FLO
        pop de
        ld hl,fnclstv
        call FLO_DIV
        jp fnclst1

;### FNCSTD -> returns the sample standard deviation
;### Parameter  [list]
fncstds equ fncbuf1         ;sum x
fncstdp equ fncbuf2         ;sum x^2
fncstdc equ fncbuf3

fncstd  call fnclst0
        ex de,hl
        ld hl,fncstds               ;(fncstds)=first x
        call FLO_MOVE
        ld hl,fncstdp
        call FLO_MOVE
        call FLO_MULT:jp nc,forclch ;(fncstdp)=first x^2
        ld a,1
        ld (fncstdc),a
fncstd1 call fnclst
        jr z,fncstd2
        ld hl,fncstds
        push de
        call FLO_ADD:jp nc,forclch  ;(fncstds)=sum x
        pop de
        call fncstd3                ;(fncbuf)=x^2, hl=fncbuf
        ex de,hl
        ld hl,fncstdp
        call FLO_ADD:jp nc,forclch  ;(fncstdp)=sum x^2
        ld hl,fncstdc
        inc (hl)
        jr fncstd1
fncstd2 ld hl,(fncstdc)
        call fncwpt         ;(forclcval)=n
        ld de,fncstds
        call fncstd3        ;(fncbuf)=(sum x)^2, hl=fncbuf
        ld de,forclcval
        call FLO_DIV        ;(fncbuf)=(sum x)^2/n
        ex de,hl
        ld hl,fncstdp
        call FLO_SUB        ;(fncstdp)=sum x^2 - (sum x)^2/n
        push hl
        ld hl,forclcval
        ld de,FLO_CONST_1NEG
        call FLO_ADD        ;(forclcval)=n-1
        ex de,hl
        pop hl
        call FLO_DIV:jp nc,forclch  ;(fncstdp)=(sum x^2 - (sum x)^2/n)/n-1
        call FLO_SQR:jp nc,forclch  ;(fncstdp)=sqr((sum x^2 - (sum x)^2/n)/n-1)
        ex de,hl
        jp fncget0
;de=source -> fncbuf=source^2, HL=fncbuf
fncstd3 ld hl,fncbuf
        call FLO_MOVE
        call FLO_MULT:jp nc,forclch
        ret


;*** DATETIME FUNCTIONS *******************************************************

.FLO_CONST_DAY
        db #00,#00,#C0,#28,#91  ;24*60*60   -> day
.FLO_CONST_3600
        db #00,#00,#00,#61,#8c  ;60*60      -> hour
.FLO_CONST_60
        db #00,#00,#00,#70,#86  ;60         -> minute

;### FNCDAT -> returns datetime of year,month,day
;### Parameter  1=year, 2=month, 3=day
;### Output     datetime
fncdat  ld ixl,3:call fncnol
        call fnc16b:push hl ;(sp)=year
        call fnc08b:push af ;(sph)=month
        call fnc08b         ;a=day
        pop de
        ld e,d              ;e=month
        ld d,a              ;d=day
        pop hl              ;hl=year
        xor a:ld c,a:ld b,a ;a,b,c=time
        jr fnctim0

;### FNCTIM -> returns datetime of hour,minute,second
;### Parameter  1=hour, 2=minute, 3=second
;### Output     datetime
fnctim  ld ixl,3:call fncnol
        call fnc08b:push af ;(sph)=hour
        call fnc08b:push af ;(sph)=minute
        call fnc08b         ;a=second
        pop bc              ;b=minute
        pop de
        ld c,d              ;c=hour
        ld hl,1980          ;hl=year
        ld de,#0101         ;e=month,d=day
fnctim0 call timput
        jr c,fnctime
fnctim1 ld (forclcval+0),hl
        ld (forclcval+2),de
        ld hl,forclcval
        xor a
        call FLO_KONV_LW_TO_FLO
        scf
        ret
fnctime ld a,-1
        or a
        jp forclch

;### FNCDTM -> returns datetime of year,month,day,hour,minute,second
;### Parameter  1=year, 2=month, 3=day, 4=hour, 5=minute, 6=second
;### Output     datetime
fncdtm  ld ixl,6:call fncnol
        call fnc16b:push hl ;(sp)=year
        call fnc08b:push af ;(sph)=month
        call fnc08b:push af ;(sph)=day
        call fnc08b:push af ;(sph)=hour
        call fnc08b:push af ;(sph)=minute
        call fnc08b         ;a=second
        pop bc              ;b=minute
        pop de
        ld c,d              ;c=hour
        pop de              ;d=day
        pop hl
        ld e,h              ;e=month
        pop hl              ;hl=year
        jr fnctim0

;### FNCDAD -> adds date to datetime
;### Parameter  1=t, 2=year, 3=month, 4=day
;### Output     datetime
fncdad  ld ixl,4:call fncnol
        push de
        ld hl,6
        add hl,de
        ex de,hl
        call fnc16b
        ld (fncdad1+1),hl       ;year  dif
        call fnc16b
        ld (fncdad2+1),hl       ;month dif
        ex de,hl
        ex (sp),hl
        inc hl
        ex de,hl
        call fncget0
        call fnctgt1            ;A=second, B=minute, C=hour, D=day (starting from 1), E=month (starting from 1), HL=year
        push af
        push bc
fncdad1 ld bc,0
        add hl,bc
        ex de,hl                ;de=year+dif
        ld a,h
        ld h,0
        dec l                   ;hl=month(0-11)
fncdad2 ld bc,0
        add hl,bc               ;hl=month+dif
        ld bc,12
        or a
        bit 7,h
        jr nz,fncdad4
fncdad3 sbc hl,bc               ;month=>0 -> year++, month-=12 while month>12
        inc de
        jr nc,fncdad3
        add hl,bc
        dec de
        jr fncdad5
fncdad4 add hl,bc               ;month<0  -> year--, month+=12 while month<0
        dec de
        jr nc,fncdad4
fncdad5 ld h,a
        inc l                   ;l=new month(1-12)
        ex de,hl                ;hl=new year, e=new month, d=old day
        pop bc
        pop af
        call fnctim0            ;(forclcval)=new date without daydif
        pop hl
        inc hl
        ld de,FLO_CONST_DAY
        call FLO_MULT:jp nc,forclch
        ex de,hl
        ld hl,forclcval
        jp FLO_ADD

;### FNCTAD -> adds time to datetime
;### Parameter  1=t, 2=hour, 3=minute, 4=second
;### Output     datetime
fnctad  ld ixl,4:call fncnol
        inc de
        push de         ;(sp)=par1
        ld hl,6
        add hl,de
        call fnctad1    ;par2=par2*60
        call fnctad2    ;par3=par2*60+par3
        call fnctad1    ;par3=(par2*60+par3)*60=par2*3600+par3*60
        call fnctad2    ;par4=par2*3600+par3*60+par4
        pop de
        call fnctad3
        ex de,hl
        jp fncget0
fnctad1 ld de,FLO_CONST_60  ;(hl)*60
        call FLO_MULT
        ret c
        jp forclch
fnctad2 ld e,l              ;next=next+act
        ld d,h
        ld bc,6
        add hl,bc
fnctad3 call FLO_ADD
        ret c
        jp forclch

;### FNCMOS -> returns months between two datetimes
;### Parameter  1=t2, 2=t1
;### Output     months
fncmos  call fncyrs0            ;hl=years, d=new day, e=new month, (fncyrso+0)=old month, (fncyrso+1)=old day
        add hl,hl:add hl,hl
        ld c,l:ld b,h
        add hl,hl:add hl,bc     ;hl*=12=monthdif between years
        ld a,(fncyrso+0)
        neg
        add e
        ld c,a
        rla
        sbc a
        ld b,a                  ;bc=monthdif in year
        add hl,bc
        ld a,(fncyrso+1)        ;a=old day
        cp d
        jr z,fncyrs4            ;=new day -> fine
        jr c,fncyrs4            ;<new day -> fine
        dec hl                  ;>new day -> month-=1
        jr fncyrs4

;### FNCYRS -> returns years between two datetimes
;### Parameter  1=t2, 2=t1
;### Output     years
fncyrso equ fncbuf1
fncyrss equ fncbuf2 ;0=neg

fncyrs  call fncyrs0
        ld a,(fncyrso+0)
        cp e
        jr z,fncyrs2
fncyrs1 jr c,fncyrs4
        dec hl
        jr fncyrs4
fncyrs2 ld a,(fncyrso+1)
        cp d
        jr nz,fncyrs1
fncyrs4 ld a,(fncyrss)
        dec a
        jp fncwpt1
fncyrs0 ld ixl,2:call fncnol
        inc de
        push de
        ld hl,6
        add hl,de
        call FLO_VGL        ;compare first with second
        ld a,1
        jr c,fncyrs5        ;second smaller -> no swap
        inc (hl)
        ld b,5
fncyrs6 ld a,(de)           ;swap
        ld c,(hl)
        ld (hl),a
        ld a,c
        ld (de),a
        inc hl:inc de
        djnz fncyrs6
        xor a
fncyrs5 ld (fncyrss),a
        pop de
        push de
        ld hl,6
        add hl,de
        ex de,hl
        call fncyrs3
        ld (fncyrso+2),hl
        ld (fncyrso+0),de
        pop de
        call fncyrs3        ;hl=new year, d=new day, e=new month
        ld bc,(fncyrso+2)
        or a
        sbc hl,bc
        ret
fncyrs3 ld hl,forclcval
        call FLO_MOVE
        jp fnctgt1

;### FNCDYS -> returns days between two datetimes
;### Parameter  1=t2, 2=t1
;### Output     days
fncdys  ld hl,FLO_CONST_DAY
fncdys1 push hl
        ld ixl,2:call fncnol
        inc de
        ld l,e:ld h,d
        ld bc,6
        add hl,bc
        call FLO_SUBX
        pop de
        ret nc
        call FLO_DIV
        ret nc
        ex de,hl
        jp fncget0

;### FNCHRS -> returns hours between two datetimes
;### Parameter  1=t2, 2=t1
;### Output     hours
fnchrs  ld hl,FLO_CONST_3600
        jr fncdys1

;### FNCMIS -> returns minutes between two datetimes
;### Parameter  1=t2, 2=t1
;### Output     minutes
fncmis  ld hl,FLO_CONST_60
        jr fncdys1

;### FNCSCS -> returns seconds between two datetimes
;### Parameter  1=t2, 2=t1
;### Output     seconds
fncscs  ld hl,FLO_CONST_1
        jr fncdys1

;### FNCYEA/MON/DAY/HOR/MNU/SEC/WKY -> returns component (year, month, day, weekday, hour, minute, second) from datetime
fncyea  call fnctgt:      :jp fncwpt        ;### FNCYEA -> returns year
fncmon  call fnctgt:ld l,e:jp fncwpt0       ;### FNCMON -> returns month
fncday  call fnctgt:ld l,d:jp fncwpt0       ;### FNCDAY -> returns day
fncwkd  call fnctgt
        call timwkd:ld l,a:jp fncwpt0       ;### FNCWKD -> returns weekday
fnchor  call fnctgt:ld l,c:jp fncwpt0       ;### FNCHOR -> returns hour
fncmnu  call fnctgt:ld l,b:jp fncwpt0       ;### FNCMNU -> returns minute
fncsec  call fnctgt:ld l,a:jp fncwpt0       ;### FNCSEC -> returns second


;*** FINANCIAL FUNCTIONS ******************************************************

;### FNCNPV -> net present value
;### Parameter  0=rate, 1-n=list
;### Formula    NPV=sum i[1-n](list[i]/(1+rate)^i)
fncnpvr equ fncbuf1
fncnpvs equ fncbuf2

fncnpv  call fnclst0                ;(fnclstv)=rate
        ld hl,fnclstv
        ld de,FLO_CONST_1
        push de
        call FLO_ADD:jp nc,forclch  ;(fnclstv)=1+rate
        pop de
        ld hl,fncnpvr
        call FLO_MOVE               ;fncnpvr=(1+rate)^i
        ld de,FLO_CONST_0
        ld hl,fncnpvs
        call FLO_MOVE               ;fncnpvs=0
fncnpv1 call fnclst
        jr z,fncnpv2
        push hl
        ld hl,fncbuf
        call FLO_MOVE               ;copy list to buffer
        pop de
        ld hl,fncnpvr
        call FLO_MULT:jp nc,forclch ;update (1+rate)^i
        ex de,hl
        ld hl,fncbuf
        call FLO_DIV:jp nc,forclch  ;buffer=list/(1+rate)^i
        ex de,hl
        ld hl,fncnpvs
        call FLO_ADD:jp nc,forclch
        jr fncnpv1
fncnpv2 ld de,fncnpvs
        jp fncget0

;### FNCPV -> present value
;### Parameter  0=rate, 1=nper, 2=payment
;### Formula    PV=payment*((1-(1+rate)^nper)/rate)
fncpv   ld a,3:ld iy,fncpvf:jp fncfor
fncpvf  db 8*0+0+#00                ;rate -> buf0
        db     3+#c0:dw FLO_CONST_1 ;1    -> buf3
        db 8*0+3+#80:dw FLO_ADD     ;buf0+buf3      buf0 = 1+rate
        db 8*1+1+#00                ;nper -> buf1
        db     2+#c0:dw FLO_CONST_0 ;0    -> buf2
        db 8*2+1+#80:dw FLO_SUB     ;buf2-buf1      buf2 = 0-nper
        db 8*0+2+#80:dw FLO_POT     ;buf0^buf2      buf0 = (1+rate)^(0-nper)
        db 8*3+0+#80:dw FLO_SUB     ;buf3-buf0      buf3 = 1-(1+rate)^(0-nper)
        db 8*0+0+#00                ;rate -> buf0
        db 8*3+0+#80:dw FLO_DIV     ;buf3/buf0      buf1 = (1-(1+rate)^(0-nper))/rate
        db 8*2+0+#00                ;paym -> buf0
        db 8*0+3+#80:dw FLO_MULT    ;buf0*buf3      buf0 = paym*(1-(1+rate)^(0-nper))/rate
        db     0+#e0                ;RET buf0

;### FNCFV -> future value
;### Parameter  0=interest rate, 1=nper, 2=payment
;### Formula    FV=payment*(((1+rate)^nper-1)/rate)
fncfv   ld a,3:ld iy,fncfvf:jp fncfor
fncfvf  db 8*0+0+#00                ;rate -> buf0
        db     1+#c0:dw FLO_CONST_1 ;1    -> buf1
        db 8*0+1+#80:dw FLO_ADD     ;buf0+buf1      buf0 = 1+rate
        db 8*1+2+#00                ;nper -> buf2
        db 8*0+2+#80:dw FLO_POT     ;buf0^buf2      buf0 = (1+rate)^nper
        db 8*0+1+#80:dw FLO_SUB     ;buf0-buf1      buf0 = (1+rate)^nper-1
        db 8*0+1+#00                ;rate -> buf1
        db 8*0+1+#80:dw FLO_DIV     ;buf0/buf1      buf0 = ((1+rate)^nper-1)/rate
        db 8*2+1+#00                ;paym -> buf1
        db 8*0+1+#80:dw FLO_MULT    ;buf0*buf1      buf0 = paym*(((1+rate)^nper-1)/rate)
        db     0+#e0                ;RET buf0

;### FNCPMT -> periodic payment for an annuity
;### Parameter  0=interest rate, 1=nper, 2=present value
;### Formula    PMT=value*(rate/(1-(1+rate)^(0-nper))
fncpmt  ld a,3:ld iy,fncpmtf:jp fncfor
fncpmtf db     3+#c0:dw FLO_CONST_1 ;1    -> buf3
        db 8*0+0+#00                ;rate -> buf0
        db 8*0+3+#80:dw FLO_ADD     ;buf0+buf3      buf0 = 1+rate
        db 8*1+1+#00                ;nper -> buf1
        db     2+#c0:dw FLO_CONST_0 ;0    -> buf2
        db 8*2+1+#80:dw FLO_SUB     ;buf2-buf1      buf2 = 0-nper
        db 8*0+2+#80:dw FLO_POT     ;buf0^buf2      buf0 = (1+rate)^(0-nper)
        db 8*3+0+#80:dw FLO_SUB     ;buf3-buf0      buf3 = 1-(1+rate)^(0-nper)
        db 8*0+0+#00                ;rate -> buf0
        db 8*0+3+#80:dw FLO_DIV     ;buf0/buf3      buf0 = rate/(1-(1+rate)^(0-nper))
        db 8*2+1+#00                ;value-> buf1
        db 8*0+1+#80:dw FLO_MULT    ;buf0*buf1      buf0 = value*(rate/(1-(1+rate)^(0-nper)))
        db     0+#e0                ;RET buf0

;### FNCRAT -> rate
;### Parameter  0=nper, 1=present value, 2=future value
;### Formula    RATE=((future/present)^(1/nper))-1
fncrat  ld a,3:ld iy,fncratf:jp fncfor
fncratf db 8*2+0+#00                ;futu -> buf0
        db 8*1+1+#00                ;pres -> buf1
        db 8*0+1+#80:dw FLO_DIV     ;buf0/buf1      buf0 = future/present
        db     1+#c0:dw FLO_CONST_1 ;1    -> buf1
        db 8*0+2+#00                ;nper -> buf2
        db 8*1+2+#80:dw FLO_DIV     ;buf1/buf2      buf1 = 1/nper
        db 8*0+1+#80:dw FLO_POT     ;buf0^buf1      buf0 = (future/present)^(1/nper)
        db     1+#c0:dw FLO_CONST_1 ;1    -> buf1
        db 8*0+1+#80:dw FLO_SUB     ;buf0-buf1      buf0 = (future/present)^(1/nper)-1
        db     0+#e0                ;RET buf0

;### FNCNPR -> nper
;### Parameter  0=interest rate, 1=payment, 2=future value
;### Formula    NPER=(ln(1+(value*(rate/payment))))/ln(1+rate)
fncnpr  ld a,3:ld iy,fncnprf:jp fncfor
fncnprf db     2+#c0:dw FLO_CONST_1 ;1    -> buf2
        db 8*0+0+#00                ;rate -> buf0
        db 8*1+1+#00                ;paym -> buf1
        db 8*0+1+#80:dw FLO_DIV     ;buf0/buf1      buf0 = rate/paym
        db 8*2+1+#00                ;valu -> buf1
        db 8*0+1+#80:dw FLO_MULT    ;buf0*buf1      buf0 = value*rate/paym
        db 8*0+2+#80:dw FLO_ADD     ;buf0+buf2      buf0 = 1+value*rate/paym
        db 8*0  +#80:dw FLO_LOG_NAT ;ln(buf0)       buf0 = ln(1+value*rate/paym)
        db 8*0+1+#00                ;rate -> buf1
        db 8*1+2+#80:dw FLO_ADD     ;buf1+buf2      buf1 = 1+rate
        db 8*1  +#80:dw FLO_LOG_NAT ;ln(buf1)       buf1 = ln(1+rate)
        db 8*0+1+#80:dw FLO_DIV     ;buf0/buf1      buf0 = ln(1+value*rate/paym)/ln(1+rate)
        db     0+#e0                ;RET buf0


;*** ADVANCED MATH FUNCTIONS **************************************************

;### FNCFAD -> factorial doublestep
;### Parameter  n
;### Formula    FACTDOUBLE=n*(n-2)*(n-4)*...
fncfad  ld b,2
        ld hl,FLO_CONST_2
        jr fncfac0

;### FNCFAC -> factorial
;### Parameter  n
;### Formula    FACT=n!
fncfac  ld b,1
        ld hl,FLO_CONST_1
fncfac0 push bc
        ld (fncfac2+1),hl
        call fncget
        pop af
        ld (fncfac1+1),a
        push hl
        ex de,hl
        ld hl,fncbuf1
        call FLO_MOVE
        pop hl
        call FLO_FIX_FLO_TO_LW:ret nc
        bit 7,b:jp nz,forclcp           ;negative -> error
        ld hl,(forclcval+2)
        ld a,l:or h
        ld a,errclcovf
        jp nz,forclcg
        ld hl,(forclcval+0)
        inc h:dec h
        jp nz,forclcg
        ld h,l
        push hl
        ld hl,forclcval
        xor a
        call FLO_KONV_LW_TO_FLO
        ld de,fncbuf1
        call FLO_VGL:jp nz,forclcp      ;not natural -> error
        ld hl,forclcval
        ld de,FLO_CONST_1
        call FLO_MOVE
        pop af
fncfac1 sub 0
        ret c
        push af
        ld hl,forclcval
        ld de,fncbuf1
        call FLO_MULT:jp nc,forclch
        ld hl,fncbuf1
fncfac2 ld de,FLO_CONST_1
        call FLO_SUB
        pop af
        jr fncfac1


;==============================================================================
;### COMPONENT CHECK ROUTINES #################################################
;==============================================================================

;### CMPNUM -> tries to convert a number
;### Input      IX=data, IY=text
;### Output     CF=0 -> (IX-6)=number [type,value], IX,IY=next
;###            CF=1 -> invalid (A=error code), IX,IY=unchanged
;### Destroyed  AF,BC,DE,HL
cmpnumbuf   ds 5
cmpnumtrm   db cmpnumtrm0-cmpnumtrm-1:db "+-*/\^);<>=&| ",0:cmpnumtrm0

cmpnum  ld a,(iy+0)
        cp "@"
        jr z,cmpnum3
        cp "_"
        jr z,cmpnum3
        cp "%"
        jr z,cmpnum6
        cp "#"
        jr z,cmpnum7
        push iy                 ;** float
        push ix
        ld hl,cmpnumbuf
        ld (cnvflt1+1),hl
        call cnvnum
        jr c,cmpnum1
        call cnvflt0
        jr c,cmpnum1
        ld c,fortokflt
cmpnum5 ld hl,cmpnumtrm
        call chrtrm
        jr c,cmpnum4
        pop hl
        ld (hl),c
        inc hl
        ex de,hl
        ld hl,cmpnumbuf
        ld bc,5
        ldir
        push de:pop ix
        pop hl
        ret
cmpnum1 cp 1:ld c,errforovf:jr z,cmpnum2
        cp 4:ld c,errfornum:jr c,cmpnum2
cmpnum4 ld c,errforinv
cmpnum2 pop ix
        pop iy
        ld a,c
        scf
        ret
cmpnum3 ld hl,timred            ;** datetime
        ld c,fortokdat
cmpnum8 ld (cmpnum9+1),hl
        push iy
        push ix
        ld hl,cmpnumbuf
        push bc
cmpnum9 call timred
        pop bc
        jr c,cmpnum4
        ld (fordatfmt),hl
        jr cmpnum5
cmpnum6 ld hl,cnvbnr            ;** binary
        ld c,fortokbin
        jr cmpnum8
cmpnum7 ld hl,cnvhxr            ;** hexadecimal
        ld c,fortokhex
        jr cmpnum8

;### CMPOPR -> tries to convert an operator
;### Input      IX=data, IY=text
;### Output     CF=0 -> (IX-1)=operator [type], IX,IY=next
;###            CF=1 -> not detected, IX,IY=unchanged
;### Destroyed  AF,BC,DE,HL
cmpoprwrd
dw cmpopr00,cmpopr01
dw cmpopr03,cmpopr05,cmpopr07,cmpopr02,cmpopr04,cmpopr06
dw cmpopr08,cmpopr09,cmpopr10,cmpopr11
dw cmpopr12,cmpopr13,cmpopr14,cmpopr15,cmpopr16,cmpopr17
dw 0

cmpopr00    db fortokopr+00,2,"||"
cmpopr01    db fortokopr+01,2,"&&"

cmpopr02    db fortokopr+02,1,"<"
cmpopr03    db fortokopr+03,2,"<="
cmpopr04    db fortokopr+04,1,">"
cmpopr05    db fortokopr+05,2,">="
cmpopr06    db fortokopr+06,1,"="
cmpopr07    db fortokopr+07,2,"<>"

cmpopr08    db fortokopr+08,4," OR "
cmpopr09    db fortokopr+09,5," XOR "
cmpopr10    db fortokopr+10,5," AND "
cmpopr11    db fortokopr+11,6," NAND "

cmpopr12    db fortokopr+12,1,"+"
cmpopr13    db fortokopr+13,1,"-"
cmpopr14    db fortokopr+14,1,"*"
cmpopr15    db fortokopr+15,1,"/"
cmpopr16    db fortokopr+16,1,"\"
cmpopr17    db fortokopr+17,1,"^"

cmpopr  ld hl,cmpoprwrd
cmpopr1 call cmpwrd
        ret c
        ld (ix+0),a
        inc ix
        ret

;### CMPFNC -> tries to convert a function
;### Input      IX=data, IY=text
;### Output     CF=0 -> (IX-1)=function [type], IX,IY=next
;###            CF=1 -> not detected, IX,IY=unchanged
;### Destroyed  AF,BC,DE,HL
cmpfnc  ld hl,fncwrd
        jr cmpopr1

;### CMPREF -> tries to convert a reference
;### Input      IX=data, IY=text
;### Output     CF=0 -> (IX-2)=reference [type,index], IX,IY=next, forcnvrXX updated
;###            CF=1 -> invalid (A=error code), IX,IY=unchanged
;### Destroyed  AF,BC,DE,HL
cmprefbuf   ds 3
cmprefabf   db 0    ;+1=column absolute, +2=row absolute
cmprefcol   db 0    ;column
cmprefrow   db 0    ;row

cmpref  push iy
        call cmpref1        ;check for reference
        ld a,errforinv
        jr c,cmpref5        ;-> invalid
        ld a,c
        cp ":"
        jr nz,cmpref6
        ld hl,cmprefabf         ;** range reference
        ld de,cmprefbuf
        ld bc,3
        ldir
        inc iy
        call cmpref1        ;check second reference
        jr c,cmprefa        ;-> invalid
        ld a,(cmprefabf)
        ld hl,cmprefbuf+0
        cp (hl)
        scf
        jr nz,cmprefa       ;abs/rel not equal -> invalid
        add fortokref+4
        ld (ix+0),a         ;store reference type
        push iy
        ld iy,cmprefbuf+1   ;sort references
        call cmpref7
        inc iy
        call cmpref7
        pop iy
        ld hl,forcnvrrc
        ld de,forcnvrrd
        call cmpref9
        ld hl,cmprefbuf+1
        ldi:ldi
cmpref8 inc hl
        ldi:ldi
        inc ix:inc ix
        pop hl
        or a
        ret
cmprefa ld a,errforref
cmpref5 pop iy              ;invalid
        ret
cmpref6 ld hl,forcnvrsc         ;** single reference
        ld de,forcnvrsd
        call cmpref9
        ld hl,cmprefabf
        ld a,(hl)
        add fortokref+0
        ld (ix+0),a
        jr cmpref8
;hl=count, de=data
cmpref9 ld a,(hl)
        cp 16
        ;jr z,...overflow
        ld (ix+1),a
        inc (hl)
        add a:add a
        ld l,a
        ld h,0
        add hl,de
        ex de,hl
        ret
;(iy+0),(iy+3)=ref1,ref2 -> sort ref1/ref2
cmpref7 ld a,(iy+3)
        ld c,(iy+0)
        cp c
        ret nc
        ld (iy+0),a
        ld (iy+3),c
        ret
;iy=text -> cf=0 ok, (cmprefabf),(cmprefcol),(cmprefrow)=reference, C=terminator
cmpref1 xor a
        ld (cmprefabf),a
        push iy
        ld bc,256*26+2
        ld de,256*fldmaxx+"A"
        ld hl,chrlet
        call cmprsb
        jr c,cmpref4
        ld (cmprefcol),a
        jr nz,cmpref2
        set 0,(hl)
cmpref2 ld bc,256*10+3
        ld de,256*fldmaxy+256+"0"
        ld hl,chrnum
        call cmprsb
        jr c,cmpref4
        jr nz,cmpref3
        set 1,(hl)
cmpref3 dec a
        ld (cmprefrow),a
        pop hl
        ret
cmpref4 pop iy
        ret

;### CMPRSB -> check for reference sub component (column or row)
;### Input      IY=text, C=max number of chars (2/3), B=base (26/10), E=startchar ("A"/"0"), D=max value (64/125+1), HL=checkfunction (chrlet/chrnum)
;### Output     CF=0 -> is component, ZF=1 absolute, A=value (+1 for numbers), C=terminator, IY=next, HL=cmprefabf
;###            CF=1 -> not, IY destroyed
;### Destroyed  B,DE,HL
cmprsbabs   db 0    ;flag, if absolute
cmprsbtlt   db cmprsbtlt0-cmprsbtlt-1:db "0123456789$":cmprsbtlt0         ;terminators behind column component
cmprsbtnm   db cmprsbtnm0-cmprsbtnm-1:db ": +-*/\^);<>=|&",0:cmprsbtnm0   ;terminators behind row    component

; a- z 00-25
;aa-az 26-51
;ba-bk 52-63

cmprsb  ld a,d
        ld (cmprsb6+1),a
        xor a
        ld (cmprsbabs),a
        ld (cmprsb3+1),hl
        ld (cmprsb5+1),hl
        ld a,(iy+0)
        sub "$"
        jr nz,cmprsb3
        inc a
        ld (cmprsbabs),a
        inc iy
cmprsb3 call 0
        ret c
        sub e
        ld h,a
cmprsb4 inc iy
        dec c
        jr z,cmprsb6
cmprsb5 call 0
        jr c,cmprsb6
        sub e
        bit 4,b
        jr z,cmprsb1
        add 26
cmprsb1 ld d,a
        push bc
        push de
        ld l,b
        call clcmu8
        pop de
        pop bc
        inc h:dec h
        scf
        ret nz
        ld a,l
        add d
        ret c
        ld h,a
        jr cmprsb4
cmprsb6 ld a,0
        cp h
        ret c
        ld e,h
        ld a,b
        cp 10
        ld hl,cmprsbtnm
        jr z,cmprsb7
        ld hl,cmprsbtlt
cmprsb7 call chrtrm
        ret c
        ld hl,cmprsbabs
        dec (hl)
        ld c,a
        ld a,e
        ld hl,cmprefabf
        ret

;### CMPWRD -> check for keywords
;### Input      IY=text, HL=keyword list structure
;### Output     CF=0 -> A=ID, IY=next
;###            CF=1 -> not detected, IY=unchanged
;### Destroyed  F,BC,DE,HL
cmpwrd  ld e,(hl):inc hl
        ld d,(hl)
        ld a,e
        or d
        scf
        ret z
        inc hl
        ex de,hl
        ld c,(hl):inc hl
        ld b,(hl)
        ld a,32
        push iy
cmpwrd1 inc hl              ;skip leading spaces
        cp (hl)
        jr nz,cmpwrd2
        djnz cmpwrd1
cmpwrd2 ld a,(iy+0)
        call chrucs
        cp (hl)
        jr nz,cmpwrd3
        inc hl
        inc iy
        djnz cmpwrd2
        pop hl
        ld a,c
        ret
cmpwrd3 pop iy
        ex de,hl
        inc c
        jr cmpwrd


;==============================================================================
;### CHAR CHECK ROUTINES ######################################################
;==============================================================================

;### CHRWHT -> skip whitespaces
;### Input      IY=text
;### Output     IY=next, A=next char
;### Destroyed  AF
chrwht  ld a,(iy+0)
        cp " "
        ret nz
        inc iy
        jr chrwht

;### CHRTRM -> check for terminator
;### Input      (IY+0)=char, HL+1=terminator list, (HL)=list count
;### Output     A=char, CF=0 -> is terminator, (HL)=terminator in list
;### Destroyed  F,B,HL
chrtrm  ld a,(iy+0)
chrtrm0 ld b,(hl)
chrtrm1 inc hl
        cp (hl)
        ret z
        djnz chrtrm1
        scf
        ret

;### CHRALP -> check for alphanumeric (A-Z, a-z, 0-9)
;### Input      (IY+0)=char
;### Output     A=ucase(char), CF=0 -> is alphanumeric
;### Destroyed  F
chralp  ld a,(iy+0)
        call chrlet
        ret nc
;### CHRNUM -> check for number (0-9)
;### Input      (IY+0)=char
;### Output     A=char,        CF=0 -> is number
;### Destroyed  F
chrnum  ld a,(iy+0)
        cp "0"
        ret c
        cp "9"+1
        ccf
        ret

;### CHRLET -> check for letter (A-Z,a-z)
;### Input      (IY+0)=char
;### Output     A=ucase(char), CF=0 -> is letter
;### Destroyed  F
chrlet  ld a,(iy+0)
        call chrucs
        cp "A"
        ret c
        cp "Z"+1
        ccf
        ret

;### CHRUCS -> uppercase
;### Input      A=char
;### Output     A=ucase(char)
;### Destroyed  F
chrucs  cp "a"
        ret c
        cp "z"+1
        ret nc
        add "A"-"a"
        ret


;==============================================================================
;### RECALCULATION ROUTINES ###################################################
;==============================================================================

;### RECUPD -> updates dependant cells after user changed cell
;### Input      L,H=changed cell
;### Output     CF=0 -> ok, CF=1 -> circular reference found
;### Destroyed  AF,BC,DE,HL,IX,IY
recupd  ld de,0
        ld (celcntrec),de   ;110        330         330 (early) 110 neu     330 neu     330 hsh
        call recref         ; 808888    3203388     3306252      698636     3003392     2997080
recupd1 call reccel         ;4016513    6865066     3983625                             3910503
        jr c,recupd2
recupd5 call recdrw         ; 852275     985610     1222172                              848265
recupd4 ret nc
        ld a,errmeminfd
        call prgalr
        or a
        ret
recupd2 call recupd0
        ld a,errforinfa
        call prgalr
        scf
        ret
recupd0 ld bc,(celcntall)           ;remove recalc/redraw marking (after error)
        ld hl,celrecmem+celrectyp
        ld de,celreclen
recupd3 ld a,(hl)
        and 63
        ld (hl),a
        add hl,de
        dec bc
        ld a,c
        or b
        jr nz,recupd3
        ret

;### RECDRW -> regenerate texts and redraw all marked cells
;### Input      (celcntdrw)=number of marked cells for redraw
;### Output     CF=1 -> memory full
;### Destroyed  AF,BC,DE,HL,IX,IY
recdrwlim   equ 10  ;max number of cells+1 for single updates

recdrwmem   db 0    ;flag, if memory full
recdrwmin   db 0,0
recdrwmax   db 0,0

recdrw  xor a
        ld (recdrwmem),a
        ld ix,celrecmem-celreclen
        ld hl,(celcntdrw)       ;hl=cell counter
        ld de,celdrw
        inc h:dec h             ;check, if too many for single updates
        jr nz,recdrw7
        ld a,l
        cp recdrwlim
        jr c,recdrw5

recdrw7 ld de,-1                ;too many cells -> update display at once
        ld (recdrwmin),de
        inc de
        ld (recdrwmax),de
recdrwd ld de,recdrw8
        call recdrw5
        push af
        ld hl,(recdrwmin)
        ld bc,(recdrwmax)
        call flddrw
        pop af
        ret

recdrw5 ld (recdrw6+1),de
recdrw1 ld a,l
        or h
        jr nz,recdrw4
        ld a,(recdrwmem)
        add 1
        ret
recdrw4 push hl
        ld de,celreclen
recdrw2 add ix,de
        bit 7,(ix+celrectyp)
        jr z,recdrw2
        call recdrw0            ;rebuild text and unmark redraw
        jr nc,recdrw3
        ld a,-1
        ld (recdrwmem),a
recdrw3 ld a,(ix+celrecctr)
        push ix
        or a
recdrw6 call nz,celdrw          ;redraw control, if existing / find min-max cells
        pop ix
        pop hl
        dec hl
        jr recdrw1

recdrw8 ld a,(ix+celrecclm)
        ld hl,recdrwmin
        call recdrw9
        ld a,(ix+celrecrow)
        dec hl
recdrw9 cp (hl)
        jr nc,recdrwa
        ld (hl),a               ;a<min -> newmin=a
recdrwa inc hl:inc hl
        cp (hl)
        ret c
        ret z
        ld (hl),a               ;a>max -> newmax=a
        ret

;ix=cell record -> rebuild text -> cf=1 memory full
recdrw0 res 7,(ix+celrectyp)
        ld (celcnvrec),ix
        call fmtnfl             ;l,h=dsp/fmt
        ld e,(ix+celrecdat+0)
        ld d,(ix+celrecdat+1)
        inc de:inc de:inc de
        push ix
        ld a,(ix+celrectyp)
        cp celtyperv
        jr z,recdrwb
        call celbld             ;(celbldbuf)=text, BC=length (with 0term)
recdrwc pop hl
        push hl
        ld de,celbldbuf
        call celutx
        pop ix
        ret
recdrwb ld a,(de)               ;error cell -> copy error type to cell text
        sub errclcbeg
        add a
        ld c,a
        ld b,0
        ld hl,celerrtab+2
        add hl,bc
        ld a,(hl):inc hl
        ld h,(hl):ld l,a
        ld c,(hl)
        inc hl
        ld b,0
        push bc
        ld de,celbldbuf
        ldir
        pop bc
        jr recdrwc

;### RECCEL -> recalculate all marked cells
;### Input      (celcntrec)=number of marked cells for recalculation
;### Output     CF=0 -> ok, (celcntdrw)=number of marked cells for redraw
;###            CF=1 -> circular reference found
;### Destroyed  AF,BC,DE,HL,IX,IY
reccelb ds 5

reccel  ld hl,0
        ld (celcntdrw),hl
reccel0 ld hl,(celcntrec)
        ld a,l
        or h
        ret z
        dec hl
        ld (celcntrec),hl
        ld ix,celrecmem         ;walk through all existing cells
        ld iy,(celcntall)
reccel1 bit 6,(ix+celrectyp)    ;test, if marked
        jr nz,reccel3
reccel2 ld de,celreclen
        add ix,de
        dec iy
        ld a,iyl:or iyh
        jr nz,reccel1
        scf                     ;no marked cell with unmarked references found -> must be circular reference error
        ret
reccelh pop hl:pop hl
        pop ix
        jr reccel2
reccel3 ld l,(ix+celrecdat+0)       ;** marked cell found
        ld h,(ix+celrecdat+1)
        ld de,celforref
        add hl,de               ;hl=references
        ld c,(ix+celrecref)
        ld a,c
        and #0f
        jr z,reccel6
        ld b,a                  ;* check single references
reccel4 ld e,(hl):inc hl
        ld d,(hl):inc hl
        push hl
        ex de,hl
        push ix
        push bc
        call fldcel
        bit 6,(hl)
        pop bc
        pop ix
        pop hl
        jr c,reccel5            ;reference cell is empty -> dont skip
        jr nz,reccel2           ;reference cell is marked -> skip
reccel5 inc hl:inc hl
        djnz reccel4
reccel6 ld a,c
        and #f0
        jr z,reccel7
        push ix                 ;* check range references
reccele push af                 ;a=cnt*16
        ld e,(hl):inc hl
        ld d,(hl):inc hl
        ld c,(hl):inc hl
        ld b,(hl):inc hl
        push hl
        ld l,e:ld h,d           ;hl=cur,de=min,bc=max
reccelf push hl
        push bc
        push de
        ld e,(hl):inc hl
        ld d,(hl)
        ex de,hl
        call fldcel
        bit 6,(hl)
        pop de
        pop bc
        pop hl
        jr c,reccelg
        jr nz,reccelh           ;cell is marked -> skip
reccelg inc l
        ld a,c
        cp l
        jr nc,reccelf
        ld l,e
        inc h
        ld a,b
        cp h
        jr nc,reccelf
        pop hl                  ;all cells in range are unmarked -> check next
        pop af
        sub 16
        jr nz,reccele
        pop ix
reccel7 res 6,(ix+celrectyp)        ;** cell has only unmarked references -> so it can be recalulated
        ld l,(ix+celrecclm)
        ld h,(ix+celrecrow)
        ld (fncpos),hl
        call celref             ;hl=data behind references
        ex de,hl
        ld hl,reccelb
        push hl
        push ix
        push iy
        call forclc
        pop iy
        pop ix
        pop hl
        ld e,(ix+celrecdat+0)   ;copy result to cell value and compare with old
        ld d,(ix+celrecdat+1)
        inc de:inc de:inc de
        jr c,reccelc            ;error
        ld bc,5*256+5+1
reccel8 ld a,(de)
        cp (hl)
        jr z,reccel9
        inc c                   ;value changed...
reccel9 ldi
        djnz reccel8
        ld a,celtypfrn
        cp (ix+celrectyp)       ;was error before?
        set 7,a
        jr nz,recceld           ;yes, mark for redraw
        dec c                   ;changed?
        jp z,reccel0
recceld ld (ix+celrectyp),a     ;yes, mark for redraw
        ld hl,(celcntdrw)
        inc hl
        ld (celcntdrw),hl
        jp reccel0

reccelc ld c,a                  ;error -> a=error, de=celdat
        ld a,celtyperv
        cp (ix+celrectyp)
        jr nz,reccela
        ld a,(de)               ;was already error before
        cp c
        jp z,reccel0            ;same error -> value not changed
reccela ld a,c
        ld (de),a               ;store error type
        ld a,celtyperv+128
        jr recceld              ;mark for redraw

;### RECREF -> find and mark other cells, which have a reference to a cell
;### Input      L,H=cell, (celcntrec)=current number of marked cells for recalculation
;### Output     (celcntrec) updated, CF=0
;### Destroyed  AF,BC,DE,HL,IX,IY
recstkmax   equ 128
recstkpos   db 0
recstkmem   ds 2*recstkmax

recref  ld a,1
        ld (recstkpos),a
        ld (recstkmem),hl
recrefc ld hl,recstkpos
        ld a,(hl)
        or a
        ret z
        dec (hl)
        ld l,a
        ld h,0
        add hl,hl
        ld bc,recstkmem-2
        add hl,bc
        ld c,(hl):inc hl
        ld h,(hl):ld l,c
        call recref0
        jr recrefc

recref0 ld (recrefa+1),hl       ;store root cell
        ld iy,(celcntall)
        iy_counter
        ld a,iyl:or iyh
        ret z
        ld ix,celrecmem         ;walk through all existing cells
        ld de,celreclen
recref1 ld a,(ix+celrectyp) ;5
        cp celtyperv        ;2  ;unmarked formula with wrong value?
        jr z,recrefd        ;2
        cp celtypfrn        ;2  ;unmarked formula?
        jr nz,recref2       ;3
recrefd ld a,(ix+celrecref)
        or a                    ;has references?
        jr nz,recref3
recref2 add ix,de           ;4  ;next one
        dec iyh             ;2
        jr nz,recref1       ;3 23
        dec iyl
        jr nz,recref1
        ret
recrefe ld de,celreclen
        jr recref2

recref3 ld l,(ix+celrecdat+0)       ;** cell is formula with references
        ld h,(ix+celrecdat+1)
        ld de,celforref
        add hl,de               ;hl=references
recrefa ld de,0
        ld c,a                  ;c=references ([b0-3]=single, [b4-7]=range)
        and #0f
        jr z,recref6
        ld b,a                  ;* check single references
recref4 ld a,e
        cp (hl)
        inc hl
        ld a,(hl)
        inc hl
        jr nz,recref5
        cp d
        jr z,recrefb
recref5 inc hl:inc hl
        djnz recref4
recref6 ld a,c
        and #f0
        jr z,recrefe
        rrca:rrca:rrca:rrca
        ld b,a                  ;* check range references
recref7 ld a,e
        cp (hl)
        inc hl:inc hl
        jr c,recref8
        dec a
        cp (hl)
        jr nc,recref8
        dec hl
        ld a,d
        cp (hl)
        inc hl:inc hl
        jr c,recref9
        dec a
        cp (hl)
        jr nc,recref9
        jr recrefb
recref8 inc hl
recref9 inc hl
        djnz recref7
        jr recrefe

recrefb ld hl,recstkpos             ;** reference found
        ld a,(hl)
        cp recstkmax
        ;jr nc,...stack overflow
        inc (hl)
        add a
        ld c,a
        ld b,0
        ld hl,recstkmem
        add hl,bc
        ld a,(ix+celrecclm)     ;push onto stack
        ld (hl),a:inc hl
        ld a,(ix+celrecrow)
        ld (hl),a
        set 6,(ix+celrectyp)    ;mark cell
        ld hl,(celcntrec)
        inc hl
        ld (celcntrec),hl       ;increase counter
        jr recrefe
