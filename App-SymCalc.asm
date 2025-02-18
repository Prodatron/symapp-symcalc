;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               S y m C a l c                                @
;@                                                                            @
;@             (c) 2024-2025 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;select all
;- sum/avg needs long time, no area optimization?

;cell texts -> 2nd bank
;- cellod3 -> copy from 2nd
;- celsav  -> copy to 2nd, compare old/new
;- celrem  -> just delete
;- celutx  -> resize mem, copy to 2nd
;- mempoi  -> adjust celrec pointer in 1st, celctr pointer in 2nd
;- move converters to 2nd
;- recdrw, celbld -> execute in 2nd bank, copy a larger amount of cell data first!

;glitches
;- overlapping cells
;- komma aufrunden
;- dynamic cell controls
;  - scrolling too fast -> forgot to plot some areas
;  - bar scroll sometimes messy
;- bold/italics no full paper -> missing OS feature

;functions
;- multiplan lookup (cell range, min number -> result is last cell in found row/column)
;- multiplan index (x, y, cell range -> result is cell xy in range)
;- celltype (text, number or error type)

;bugs
;- missing memful/stackoverflow checks in engine etc
;- memful during cut&paste - alert hanging?
;- load -> test, if new memory is<= max available
;- binary 32bit overflow

;nice to have
;- percent as formula token/auto format
;- valignment
;- splitscreen (!)
;- names for cells and cell ranges (maybe additional token in front of 8-15?)
;- date with full 32bit, using negative values for low dates
;- SymChart
;- bitmaps as cell content

;optimizing
;- optimize timget
;- FLO_PREPARE looses last decimal digit, try using FIX_FLO_TO_LW, if integer

;help ideas
;- more examples
;- pdf generator
;- screenshots



;--- PROGRAM ------------------------------------------------------------------
;### PRGPRZ -> Application process
;### PRGKEY -> key pressed
;### PRGEND -> End program
;### PRGHLP -> show help
;### PRGINF -> show info-box
;### PRGALR -> show alert window
;### PRGFIL -> Generates extended file path
;### DOCPAR -> get document from command line parameter
;### DOCAUT -> loads document automatically, if attached as command line parameter

;--- FILE ROUTINES ------------------------------------------------------------
;### FILMOD -> Ask for file-saving, if document has been modified
;### FILNEW -> New document
;### FILOPN -> Open document
;### FILBRW -> Browse file
;### FILSAS -> Save document as
;### FILSAV -> Save document
;### FILTIT -> Refreshes window title with filename
;### FILLOD -> load document
;### FILMTA -> load meta data
;### FILCFG -> load config data
;### FILSHT -> load sheet data
;### FILCRC/CTX/CDT -> load cell records/text/data
;### FILEND -> file end reached
;### FILSTO -> save actual spreadsheet
;### FILERR -> show error message
;### FILIMC -> import CSV
;### FILEXC -> export CSV
;### FILIMS -> import SYLK
;### FILEXS -> export SYLK

;--- DOCUMENT ROUTINES --------------------------------------------------------
;### DOCMOD0 -> marks doc for not modified
;### DOCLOD -> loads primary spreadsheet state
;### DOCSAV -> saves primary spreadsheet state
;### DOCCLR -> clears primary spreadsheet
;### DOCHSH -> generates hash table for all cell records
;### DOCSTO -> prepares the primary spreadsheet for storing
;### DOCDRW -> redraws the primary spreadsheet

;--- MARKING ROUTINES ---------------------------------------------------------
;### MRKINI -> inits marking
;### MRKSHF -> mark fields with cursor+shift
;### MRKFLD -> marks fields
;### MRKREM -> unmark, if marking was active
;### MRKESC -> Esc, cancle marking
;### MRKSRT -> returns sorted marked area
;### MRKACT -> check if marking is active
;### MRKALL -> marks whole field
;### MRKCLM -> marks whole column(s)
;### MRKROW -> marks whole row(s)
;### MRKCHG -> check, if marking changed
;### MRKSUM -> show sum/average in status line

;--- MENU LINKS ---------------------------------------------------------------
;### MENCSZ -> set column size
;### MENRSZ -> set row size
;### MENCSL -> selects column
;### MENRSL -> selects row
;### MENAGN/LF/RG/CN -> alignment
;### MENSNR/BL/IT -> style
;### MENNFL/EX/PR/DT/TM/BL/BN/HX -> number format
;### MENSEP -> 1000 separator
;### MENTOL -> show/hide toolbar
;### MENSTA -> show/hide statusbar

;--- BAR ROUTINES -------------------------------------------------------------
;### BARINI -> inits both bars
;### BARSIZ -> check, if click for bar resizing
;### BARSCL -> set column size with dialogue window
;### BARSRW -> set row size with dialogue window
;### BARSST -> opens size set window
;### BARMCL -> context menu for column bar
;### BARMRW -> context menu for row bar
;### BARCCL -> column bar clicked
;### BARCRW -> row bar clicked
;### BARCMR -> update cursor/marking sizes after cell size update
;### BARADJ -> adjust cell controls after column/row size update

;--- FIELD ROUTINES -----------------------------------------------------------
;### FLDFCF/FCE -> sets focus to field/formula editor
;### FLDMLR -> release left mouse click
;### FLDEDT -> copies coordinates to editor, if editor-click-mode active
;### FLDCLK -> field mouse click
;### FLDMOS -> returns clicked cell
;### FLDKEY -> field key event
;### FLDCLR -> clears field(s), if existing
;### FLDMUL -> manipulate current or marked fields
;### FLDDRW -> redraw field area
;### FLDCxx -> move cursor
;### FLDPOS -> calculates cell position and size
;### FLDDIM -> calculates cell begin and width
;### FLDSCL -> scroll x/y if necessary
;### FLDCEL -> check if there is a cell at field position
;### FLDCOR -> shows current cell or selection coordinates

;--- VISIBLE CELL CONTROL HANDLING --------------------------------------------
;### VISDRW -> rebuild and redraw new visible area
;### VISSCL -> check, if visible field has been changed, recalculates cell range and returns changed area
;### VISRNG(X/Y) -> calculates the visible cell range
;### VISREM -> removes invisible cell controls
;### VISADD -> tries to adds cell controls, which are now visible

;--- CELL HANDLING ------------------------------------------------------------
;### CELIMP -> prepares/finishes cell import
;### CELPUT -> parses string and copies result into cell
;### CELGET -> gets cell content as string (for export)
;### CELLOD -> loads current cell into formula input
;### CELSAV -> saves formula input to cell
;### CELMEMx -> show message and optionally removes cell when memory full (coming from CELSAV)
;### CELDRW -> redraw cell control
;### CELUPD -> updates cell data/text
;### CELUDA -> updates cell data
;### CELUTX -> updates cell text
;### CELURF -> updates cell reference counter
;### CELRSZ -> adds/resizes/removes cell data/text
;### CELCAD -> gets control addresses
;### CELCNW -> adds a new control
;### CELNEW -> adds a new cell
;### CELREM -> removes a cell record
;### CELCRM -> removes a cell control
;### CELREF -> get reference data from cell
;### CELCNV -> converts formula input into cell data
;### CELERR -> prepares an error cell
;### CELBLD -> builds a cell display text
;### CELUNI -> add unit to cell text

;--- CELL FORMAT DIALOGUE -----------------------------------------------------
;### CFDOPN -> open cell format window
;### CFDOKY -> "ok" clicked

;--- FORMAT HANDLING ----------------------------------------------------------
;### FMTCOL -> open colour dropdown
;### FMTCUR -> try to get current cell or first cell in range
;### FMTALF/RG/CN -> set/reset alignment
;### FMTFBL/IT/DF -> set font
;### FMTDSP -> displays format of current cell(s)
;### FMTNxx -> returns cell or default formatting for number types
;### FMTDFT -> set default format for cell and control, if new or type changed
;### FMTCTR -> convert cell format to control

;--- TIME CONVERSION ----------------------------------------------------------
;### TIMEDT -> converts datetime to edit string
;### TIMRED -> reads datetime from string
;### TIMWRT -> writes datetime as string with mask
;### TIMPUT -> converts time to timestamp value
;### TIMGET -> reads time from timestamp value ##!!## NOT OPTIMIZED
;### TIMWKD -> weekday calculation
;### CLCM60 -> multiply 32bit value*60

;--- NUMBER CONVERSION --------------------------------------------------------
;### CNVSTR -> converts FP number to string
;### CNVDIG -> adds a digit and inserts a 1000er point, if needed
;### CNVFLT -> converts string to FP number
;### CNVNUM -> converts string into number, exponent, sign
;### CNV32S -> Converts 32Bit-number (unsigned) to string (terminated by 0)
;### CNV08S -> converts 8bit to decimal string
;### CNVS16 -> converts string into number
;### CNVDEC -> converts byte into two decimal digits
;### CNVBOL -> convert number to boolean
;### CNVBIN -> convert number to hex/bin
;### CNVHXR -> reads hex from string
;### CNVBNR -> reads bin from string

;--- CUT, COPY AND PASTE ROUTINES ---------------------------------------------
;### COPCOP -> copy
;### COPCUT -> cut
;### COPPST -> paste
;### COPDEL -> removes cell, if not in destination range
;### COPRNG -> paste from range
;### COPTXT -> rebuilds text for cell in range, if it has been updated
;### COPMUL -> does operation on multiple cells
;### COPGET -> copies a single cell to the buffer
;### COPPUT -> copies from buffer to single cell, relocate references, mark for recalc if formula
;### COPRR1 -> activate reference in range check, if "cut" mode
;### COPRR0 -> deactivate reference in range check, if "cut" mode
;### COPRFR -> check, if reference completely points to range
;### COPREF -> adjust references, if relative

;--- MOVE CELLS/ROWS/COLUMNS --------------------------------------------------
;### MOVCID -> insert cells down
;### MOVCIR -> insert cells right
;### MOVCRU -> remove cells up
;### MOVCRL -> remove cells left
;### MOVRSI -> insert single row
;### MOVRWI -> insert rows
;### MOVCSI -> insert single column
;### MOVCLI -> insert columns
;### MOVRSR -> remove single row
;### MOVRWR -> remove rows
;### MOVCSR -> remove single column
;### MOVCLR -> remove columns
;### MOVMOV -> moves cells using cut&paste
;### MOVVAL -> prepares values for cells movement
;### MOVVLB -> prepares values for bar-clicked single row/column movement
;### MOVOPT -> optimize source range

;--- CONFIG -------------------------------------------------------------------
;### CFGINI -> init config

;--- DOCUMENT PROPERTIES ------------------------------------------------------
;### PRPOPN -> open document properties window
;### PRPOKY -> document properties has been saved

;--- INTERBANK HANDLING -------------------------------------------------------
;### IBKINI -> init extended modules
;### IBKLOC -> calls local routine from external module with full register transfer

;--- SUB ROUTINES -------------------------------------------------------------
;### KEYCHK -> checks key
;### CLCRNG -> check, if value in range
;### CLCM86 -> 8*16bit multiplication
;### CLCMU8 -> 8bit unsigned multiplication
;### CLCS32 -> substract 32bit values
;### STRCMP -> string compare
;### STRLEN -> get string length
;### STRINI -> inits string input control
;### WINPOS -> gets main window position
;### WINBLR -> dropdown window lost focus -> close it
;### WINCLS -> closes dialogue window
;### WINBOT -> shows booting window
;### MEMINI -> inits memory stuff

;---


macro   bc_counter
        ld a,c
        dec bc
        inc b
        ld c,b
        ld b,a
endm

macro   ix_counter
        ld a,ixl
        dec ix
        inc ixh
        ld ixl,ixh
        ld ixh,a
endm

macro   iy_counter
        ld a,iyl
        dec iy
        inc iyh
        ld iyl,iyh
        ld iyh,a
endm


;MAX DEFINITIONS
celrecmax   equ 1024                ;cell records
celdatmax   equ 8000                ;cell data memory
;celtxtmax                          ;cell text memory
ctrcelmax   equ 120                 ;cell gui controls (max=234=240-6)

celreclen   equ 12


_cfdopn  equ 01
_cfdtab  equ 02
_cfdoky  equ 03
_cfdmod  equ 04
_cfdprv  equ 05
_cfdpen  equ 06
_cfdpap  equ 07
_wincnc  equ 08
_barsdia equ 09
_barsdib equ 10
        ;equ 11
        ;equ 12
;*res*   equ 13
_fmtcol  equ 14
_fmtctb  equ 15
_fmtc00  equ 16
_fmtc01  equ 17
_fmtc02  equ 18
_fmtc03  equ 19
_fmtc04  equ 20
_fmtc05  equ 21
_fmtc06  equ 22
_fmtc07  equ 23
_fmtc08  equ 24
_fmtc09  equ 25
_fmtc10  equ 26
_fmtc11  equ 27
_fmtc12  equ 28
_fmtc13  equ 29
_fmtc14  equ 30
_fmtc15  equ 31
_prpopn  equ 32
_prptab  equ 33
_prpoky  equ 34
_prpcat  equ 35
_prptyp  equ 36
_prpup   equ 37
_prpdwn  equ 38
_xchdec  equ 39
_xchdic  equ 40
_xchbrw  equ 41
_xchexc  equ 42
_xchimc  equ 43
_xchipc  equ 44
_xchexs  equ 45
_xchims  equ 46
_cfgopn  equ 47
_cfgoky  equ 48
_cfgtab  equ 49
_cfgreg  equ 50
_cfgctp  equ 51
_cfgcol  equ 52
_prpcol  equ 53
_prpctp  equ 54


tmpbuf  ds 256      ;temp buffer (fillod, fldedt, celput)


;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;### CODE AREA ################################################################
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


;==============================================================================
;### PROGRAM ##################################################################
;==============================================================================

;### PRGPRZ -> Application process
winmainum   db 0    ;main     window ID
windianum   db 0    ;dialogue window ID
winmaifoc   db 1    ;flag, if main window has focus
ctroldfoc   db 0    ;last control with focus
ctroldedt   db -1   ;neg-flag, if edit control lost focus (=0 -> formula editor lost focus, <>0 -> did not loose focus)

prgprzdly   db 0

prgprz  call winbot             ;** booting beg

        call memini
        call prgfil
        call ibkini
        call cfgini
        call hshini_b
        call barmem_b
        ld (winmairec2+00+4),de
        ld (winmairec2+16+4),hl
        call docclr
        call SySystem_HLPINI

        call wincls             ;** booting end

        ld de,winmaidat
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN       ;open main window
        jp c,prgend                 ;memory full -> quit process
        ld (winmainum),a            ;window has been opened -> store ID

        call fldfcf
        call fldcor
        call docaut

prgprzx ld de,(supcelctr+6)         ;** bar scrolling
        ld hl,(supcelctr+8)
        call barmov_b
        dec d
        jr z,prgprzb
        ld hl,ctrgrdver+2
        dec d
        jr z,prgprza
        ld hl,ctrgrdhor+2
prgprza call barset_b
        ld a,h
        ld (winmaigrp+14),a
        ld a,l
        rst #20:dw jmp_keyput
        rst #30
        call fldfcf
        jr prgprzx

prgprzb ld a,(fmtdspf)              ;** format of current cell
        or a
        jp nz,fmtdsp
        call visscl                 ;** visible cells
        jp c,visdrw
        call mrkchg                 ;** sum/average in status line
        jp nz,mrksum
        call bsyend

        rst #30
        ld hl,prgprzdly
        dec (hl)
        jr nz,prgprzz
jr prgprzz  ;##!!## mouse wheel on cellfield wont be recognized
        ld ix,(App_PrcID)           ;check for messages (sleep)
        ld ixh,-1
        ld iy,App_MsgBuf
        rst #08
        ld a,3
        ld (prgprzdly),a
        jr prgprzw

prgprz0 ld a,3
        ld (prgprzdly),a
prgprzz ld ix,(App_PrcID)           ;check for messages (idle)
        ld ixh,-1
        ld iy,App_MsgBuf
        rst #18
prgprzw dec ixl
        jr nz,prgprzx
        ld a,(App_MsgBuf+0)
        or a
        jp z,prgend0
        ld hl,(App_MsgBuf+1)
        cp MSR_DSK_CFOCUS
        jr z,prgprz8
        cp MSR_DSK_WFOCUS
        jr z,prgprz2
        cp MSR_DSK_WMODAL
        jr z,prgprz5
        cp MSR_DSK_WCLICK           ;click
        jr nz,prgprz0
        ld a,h                      ;main/dialogue click
        cp DSK_ACT_CLOSE
        jr z,prgprz7
        ld hl,(App_MsgBuf+8)
        cp DSK_ACT_MENU
        jp z,prgmen
        cp DSK_ACT_TOOLBAR
        jr z,prgprz1
        cp DSK_ACT_KEY
        jp z,prgkey
        cp DSK_ACT_CONTENT
        jr nz,prgprz0
prgprz1 ld a,h
        or l
        jr z,prgprz0
        inc h:dec h
        jp z,guijmp1
        jp (hl)
prgprz2 ld hl,(App_MsgBuf+1)        ;focus
        ld a,(winmainum)
        cp l
        jr z,prgprz4
        inc h:dec h
        jr nz,prgprz0
prgprz6 ld a,(windrpnum)
        cp l
        jp z,winblr
        jr prgprz0
prgprz4 ld a,h
        ld (winmaifoc),a
        jp prgprz0
prgprz5 ld hl,(App_MsgBuf+1)        ;user tried to escape from modal -> if dropdown, close it
        jr prgprz6
prgprz7 ld a,l                      ;user closes window
        ld hl,winmainum
        cp (hl)
        jp z,prgend0
        ;...
        jp prgprz0
prgprz8 ld a,(winmainum)            ;** control focus changed
        cp (iy+1)
        jp nz,prgprz0
        ld a,(App_MsgBuf+2)
        cp winmairec_next+1
        jr z,keytbf
        cp winmairec_formu+1
        jr nz,prgprz9
        ld (ctroldfoc),a            ;new is editor -> store, "lost focus"=no (<>0)
        ld (ctroldedt),a            ;store control ID
        jp prgprz0
prgprz9 ld hl,winmaigrp+14
        cp (hl)
        jp nz,prgprz0               ;new is sub-window -> ignore
        ld hl,ctroldfoc
        ld c,(hl)                   ;c=old control
        ld (hl),a                   ;store new control
        cp c
        jp z,prgprz0                ;new=old -> ignore
        ld a,c
        sub winmairec_formu+1       ;a=0, if old was editor
        ld (ctroldedt),a            ;if old was editor -> "was before"=yes (0)
        jp prgprz0

;### PRGKEY -> key pressed
;### Input      HL=control ID
keyglbt db "N"-64:dw filnew         ;global
        db "O"-64:dw filopn
        db "S"-64:dw filsav
        db "Q"-64:dw prgend0
keyglbt0
keyglbc equ keyglbt0-keyglbt/3

prgkeyt db 13:dw keyret             ;outside spreadsheet
        db 27:dw keyesc
prgkeyt0
prgkeyc equ prgkeyt0-prgkeyt/3

prgkey  push hl
        ld hl,keyglbt       ;global keys
        ld b,keyglbc
        call keychk
        pop de
        jr z,prgkey2
        ex de,hl
        ld bc,fldclk
        or a
        sbc hl,bc
        jp z,fldkey
prgkey1 ld hl,prgkeyt       ;outside spreadsheet
        ld b,prgkeyc
        call keychk
        jp nz,prgprz0
prgkey2 jp (hl)

keytbf  call fldfcf
        jp fldcrg

keyret  ld a,(winmaigrp+14)
        cp winmairec_formu+1
        jr nz,keyret1
        call fldfcf
        jp fldcdw
keyret1 cp winmairec_fpos+1
        jp z,fldset
        ;...
        jp prgprz0

keyesc  ld a,(winmaigrp+14)
        cp winmairec_formu+1
        jr nz,keyesc1
        call cellod
keyesc1 call fldfcf
        jp prgprz0

;### PRGMEN -> item in main menu selected
;### Input      HL=menu item ID
prgmen  ld bc,prgmentab-2
        jp jmptab
prgmentab
dw filnew  ;mai_filnew  equ 01
dw filopn  ;mai_filopn  equ 02
dw filsav  ;mai_filsav  equ 03
dw filsas  ;mai_filsas  equ 04
dw filimc  ;mai_filimc  equ 05
dw filims  ;mai_filims  equ 06
dw filexc  ;mai_filexc  equ 07
dw filexs  ;mai_filexs  equ 08
dw prpopn  ;mai_prpopn  equ 09
dw prgend0 ;mai_prgend0 equ 10
dw copcut  ;mai_copcut  equ 11
dw copcop  ;mai_copcop  equ 12
dw coppst  ;mai_coppst  equ 13
dw fldclr  ;mai_fldclr  equ 14
dw mrkall  ;mai_mrkall  equ 15
dw mencsl  ;mai_mencsl  equ 16
dw menrsl  ;mai_menrsl  equ 17
dw mentol  ;mai_mentol  equ 18
dw mensta  ;mai_mensta  equ 19
dw cfgopn  ;mai_cfgopn  equ 20
dw cfdopn  ;mai_cfdopn  equ 21
dw mensnr  ;mai_mensnr  equ 22
dw mensbl  ;mai_mensbl  equ 23
dw mensit  ;mai_mensit  equ 24
dw menagn  ;mai_menagn  equ 25
dw menalf  ;mai_menalf  equ 26
dw menacn  ;mai_menacn  equ 27
dw menarg  ;mai_menarg  equ 28
dw mennfl  ;mai_mennfl  equ 29
dw menndt  ;mai_menndt  equ 30
dw menntm  ;mai_menntm  equ 31
dw mennpr  ;mai_mennpr  equ 32
dw mennex  ;mai_mennex  equ 33
dw mennbl  ;mai_mennbl  equ 34
dw mennbn  ;mai_mennbn  equ 35
dw mennhx  ;mai_mennhx  equ 36
dw mensep  ;mai_mensep  equ 37
dw mencsz  ;mai_mencsz  equ 38
dw menrsz  ;mai_menrsz  equ 39
dw movrwi  ;mai_movrwi  equ 40
dw movcli  ;mai_movcli  equ 41
dw movrwr  ;mai_movrwr  equ 42
dw movclr  ;mai_movclr  equ 43
dw movcid  ;mai_movcid  equ 44
dw movcir  ;mai_movcir  equ 45
dw movcru  ;mai_movcru  equ 46
dw movcrl  ;mai_movcrl  equ 47
dw prghlp  ;mai_prghlp  equ 48
dw prginf  ;mai_prginf  equ 49

;### PRGEND -> End program
prgend0 call filmod
        jp c,prgprz0
        call cfgsav_b
prgend  ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend1 rst #30
        jr prgend1

;### PRGHLP -> show help
prghlp  call SySystem_HLPOPN
        jp prgprz0

;### PRGINF -> show info-box
prginf  ld a,prgmsginf          ;*** info box
        ld b,1+128+64
        call prgalr0
        jp prgprz0

;### PRGALR -> show alert window
;### Input      A=alert message ID
prgalr  ld b,1+64
prgalr0 ld l,a
        ld h,0
        add hl,hl:add hl,hl
        ld e,l:ld d,h
        add hl,hl:add hl,de
prgerrinf   equ $+1
        ld de,prgerrinf
        add hl,de
prgalr2 ld a,0
        ld de,winmaidat
        jp SySystem_SYSWRN

;### PRGFIL -> Generates extended file path
prgpth  dw 0
prgfex  dw 0    ;address of file extension

prgfil  ld hl,(App_BegCode)
        ld de,App_BegCode
        dec h
        add hl,de           ;HL = CodeEnd = path
        ld (prgpth),hl
        ld b,255
prgfil1 ld a,(hl)           ;search end of path
        or a
        jr z,prgfil3
        cp " "
        jr z,prgfil2
        inc hl
        djnz prgfil1
        jp prgend
prgfil2 push hl
        inc hl
        call docpar
        pop hl
        ld (hl),0
prgfil3 ld bc,-3
        add hl,bc
        ld (prgfex),hl
        ret
prgfil0 ld de,(prgfex)
        ld bc,4
        ldir
        ret

;### DOCPAR -> get document from command line parameter
;### Input      HL=path
docpar  ld de,docpth
docpar1 ld a,(hl)
        call chrucs
        ld (hl),a
        ldi
        or a
        jr nz,docpar1
        dec hl:dec hl:dec hl:dec hl
        ex de,hl
        ld hl,filbrwe
        ld c,3
docpar2 push de
        ld b,3
docpar3 ld a,(de)
        cp (hl)
        jr nz,docpar4
        inc hl
        inc de
        djnz docpar3
        pop de
        ld a,4
        sub c
        jr docpar5
docpar4 pop de
        inc hl:inc hl:inc hl
        dec c
        jr nz,docpar2
        ld a,1
docpar5 ld (docaut+1),a
        ret

;### DOCAUT -> loads document automatically, if attached as command line parameter
docaut  ld a,0          ;0=no attached file, 1=SCS, 2=CSV, 3=SLK
        or a
        ret z           ;nothing
        cp 2
        jp c,fillod     ;SCS
        ld hl,_xchipc
        jr z,docaut1    ;CSV
        ld hl,_xchims
docaut1 jp guijmp_c     ;SLK


;==============================================================================
;### FILE ROUTINES ############################################################
;==============================================================================

;### FILMOD -> Ask for file-saving, if document has been modified
;### Output     CF=0 ok, CF=1 cancel
filmod  ld a,(docmodf)
        or a
        ret z
        ld a,prgmsgsav
        ld b,8*4+3+64
        call prgalr0
        cp 1
        ret c
        sub 4
        ret z
        ccf
        ret c
        jp filsav0

;### FILNEW -> New document
filnew  call filnew0
        call docdrw
        jp prgprz0
filnew0 call filmod
        ret c
        call prpdef_b
        ld de,filtits
        ld a,filtits0-filtits
        call filtit3
        call docclr
        call docmod0
        or a
        ret

;### FILOPN -> Open document
filopn  call filmod
        jp c,prgprz0
        xor a
        call filopn0
        or a
        call z,fillod
        jp prgprz0
filopn0 ld l,0
        jr filbrw

;### FILBRW -> Browse file
;### Input      L=extension type (-1=just get address, 0=SCS, 3=CSV, 6=SLK), A=load(0)/save(64)
;### Output     HL=docpth
filbrwe db "SCS"
        db "CSV"
        db "SLK"

filbrw  ld h,0
        inc l
        jr z,filbrw1
        ld bc,filbrwe-1
        add hl,bc
        ld de,docmsk
        ld bc,3
        ldir
        ld hl,App_BnkNum
        add (hl)
        ld hl,docmsk
        ld c,8
        ld ix,100
        ld iy,5000
        ld de,winmaidat
        call SySystem_SELOPN
filbrw1 ld hl,docpth
        ret

;### FILSAS -> Save document as
filsas  call filsav1
        call filtit
        jp prgprz0

;### FILSAV -> Save document
filsav  call filsav0
        jp prgprz0
;-> CF=1 cancel
filsav0 ld a,(docpth)
        or a
        jr nz,filsav2
filsav1 ld a,64
        call filopn0
        or a
        scf
        ret nz
filsav2 call filsto
        call filtit
        or a
        ret

;### FILTIT -> Refreshes window title with filename
filtitt db " - SymCalc",0
filtits db "untitled":filtits0
filtit0 db 0

filtit  ld bc,filtit0
        ld hl,docpth
        ld e,l:ld d,h
filtit1 call filtit4
        inc hl
        jr nz,filtit2
        ld e,l:ld d,h       ;de=behind last /\:
filtit2 cp "."
        jr nz,filtit5
        ld c,l:ld b,h
        dec bc              ;bc=at last .
filtit5 or a
        jr nz,filtit1
        ld (bc),a
        ld (filtit6+3),bc
        dec hl
        sbc hl,de
        jr z,filtit6
        ld a,l
        cp 13
        jr c,filtit3
        ld a,12
;a=len,de=source
filtit3 ld c,a
        ld b,0
        ex de,hl
        ld de,maitittxt
filtit8 ld a,(hl)
        or a
        jr z,filtit7
        ldi
        inc c:dec c
        jr nz,filtit8
filtit7 ld hl,filtitt
        ld bc,11
        ldir
        ld a,(winmainum)
        call SyDesktop_WINTIT
filtit6 ld a,"."
        ld (filtit0),a
        ret
filtit4 ld a,(hl)
        cp "/"
        ret z
        cp "\"
        ret z
        cp ":"
        ret

chuhed  db "HEAD":ds 4              ;application, document, version, timestamp infos
chutab
chumta  db "META":ds 4:dw filmta    ;meta data (author etc)
chucfg  db "CNFG":ds 4:dw filcfg    ;config data (format strings etc)
chusht  db "SHET":ds 4:dw filsht    ;sheet data (number of elements, memory sizes)
chucrc  db "CREC":ds 4:dw filcrc    ;cell records (12*x)
chuctx  db "CTXT":ds 4:dw filctx    ;cell texts
chucdt  db "CDAT":ds 4:dw filcdt    ;cell data
chuend  db "ENDF":ds 4:dw filend    ;end of file
chunum  equ 7

filhnd  db 0

filerrlod   equ 0   ;while loading
filerrsav   equ 5   ;while saving
filerrfrm   equ 128 ;wrong format
filerrver   equ 129 ;unsupported new version
filerrcor   equ 130 ;corrupt file
filerrmem   equ 131 ;memory full

;### FILLOD -> load document
fillodf db -1,-1,-1,-1              ;neg-flags, if all important parts loaded
fillodb equ tmpbuf
fillodi db "HEAD",56,0,0,0,"SCS",0

fillod  ld (fillod3+1),sp
        ld hl,-1
        ld (fillodf+0),hl
        ld (fillodf+2),hl           ;reset part flags
        ld ix,(App_BnkNum-1)
        ld hl,docpth
        call SyFile_FILOPN          ;open file
        jp c,fillod4
        ld (filhnd),a
        ld bc,64
        call fillod1                ;load header
        ld hl,fillodi
        ld de,fillodb
        ld b,12
        call strcmp                 ;check, if correct format
        ld a,filerrfrm
        jp nz,fillode
        ld de,(fillodb+12)
        ld hl,256*1+0
        or a
        sbc hl,de                   ;check, if supported version
        ld a,filerrver
        jp c,fillode
        call prpdef_b               ;yes, clear document and start loading
        call docclr
        call docmod0
fillod6 ld bc,8                 ;** RETURN from chunk handler
        call fillod1                ;load next chunk header
        ld hl,chutab
        ld b,chunum
fillod7 push hl
        push bc
        ld de,fillodb
        ld b,4
        call strcmp                 ;detect chunk
        pop bc
        pop hl
        jr z,fillod9
        ld de,10
        add hl,de
        djnz fillod7
        ld ix,(fillodb+4)           ;unknown chunk -> skip
        ld iy,(fillodb+6)
        jr fillodg
fillodd ld iy,0
fillodg ld c,1
        ld a,(filhnd)
        call SyFile_FILPOI
        jr nc,fillod6
        jr fillod8
fillod9 ld de,8                     ;known chunk -> jump to handler function
        add hl,de
        ld e,(hl):inc hl
        ld d,(hl)
        ex de,hl
        jp (hl)
;bc=loaded chunk length, (fillodb+4)=full chunk length -> skip additional bytes
filloda ld hl,(fillodb+4)
        or a
        sbc hl,bc
        jr z,fillod6
        push hl:pop ix
        jr fillodd
;[hl=dest,] bc=length -> load data
fillod1 ld hl,fillodb
fillod2 ld a,(App_BnkNum)
        ld e,a
        ld a,(filhnd)
        call SyFile_FILINP
        jr c,fillod3
        ret z
        ld a,filerrcor              ;couldn't load all -> file corrupt
fillod3 ld sp,0
fillod8 push af
        call filsto3
        pop af
fillodc push af
        call docclr
        call fillod5
        pop af
fillod4 ld e,filerrlod
        jp filerr
fillod5 call filtit
        jp docdrw
fillode push af
        call filsto3
        pop af
        jr fillod4

;### FILMTA -> load meta data
filmta  ld de,(fillodb+4)
        ld hl,(filhnd-1)
        call prplod_b
        push hl:pop af
        jr c,fillod3
        jp fillod6

;### FILCFG -> load config data
filcfg  ld bc,cfgdocend-cfgdocbeg
        ld l,c:ld h,b
        ld de,(fillodb+4)
        or a
        sbc hl,de
        jr c,filcfg1
        ld c,e:ld b,d
filcfg1 ld hl,cfgdocbeg
        call filsht5
        jp filloda

;### FILSHT -> load sheet data
filsht  ld hl,docshtdat
        ld bc,29+docstalen
        push bc
        ld de,fillodf+0
        call filsht2
        ld hl,(docshtdat+18+docstalen)      ;l=cols,h=rows
        push hl
        ld de,ctrgrdver+2
        ld a,(cfgcelxln)
        ld c,fldmaxx
        call filsht3
        pop hl
        push hl
        ld l,h
        ld de,ctrgrdhor+2
        ld a,(cfgcelyln)
        ld c,fldmaxy
        call filsht3
        call barini
        pop hl
        pop bc
        ld e,h
        ld d,0
        ld h,d
        add hl,bc
        add hl,de
        ld c,l
        ld b,h
        jp filloda
;de=grid sizes,a=default,c=number-1,l=len in file
filsht3 push de
        push hl
        call filsht4
        pop bc
        ld b,0          ;##!!## check, if too long
        pop hl
        jp fillod2
filsht4 ld l,e:ld h,d
        inc de
        add a
        ld (hl),a
        ld b,0
        ldir
        ret

filsht2 xor a
        ld (de),a
filsht5 push bc
        call fillod2
        pop bc
        ret
filsht1 call filsht2
        jp filloda

;### FILCRC/CTX/CDT -> load cell records/text/data
filcrc  ld de,fillodf+1:ld hl,celrecmem    :ld bc,(doclenrec):jr filsht1    ;load cell records
filctx  ld de,fillodf+2:ld hl,(celtxtrec+0):ld bc,(doclentxt):jr filsht1    ;load cell text
filcdt  ld de,fillodf+3:ld hl,(celdatrec+0):ld bc,(doclendat):jr filsht1    ;load cell data

;### FILEND -> file end reached
filend  call filsto3            ;close file
        ld hl,fillodf           ;check, if all parts loaded
        ld b,4
        xor a
filend1 or (hl)
        inc hl
        djnz filend1
        jp nz,fillodc           ;no -> reset document, show error
        ld hl,celrecmem
        ld de,(docadrrec)
        or a
        sbc hl,de               ;hl=real-orig recadr
        ld iy,docnumtxt
        ld ix,celtxtrec
        push hl
        call filend2            ;init and relocate text
        pop hl
        ld iy,docnumdat
        ld ix,celdatrec
        call filend2            ;init and relocate data
        ld hl,(celtxtrec+memstrbeg)
        ld de,(docadrtxt)
        or a
        sbc hl,de
        ld (filend5+1),hl       ;prepare txtadr dif
        ld hl,(celdatrec+memstrbeg)
        ld de,(docadrdat)
        or a
        sbc hl,de
        ld (filend6+1),hl       ;prepare datadr dif
        ld bc,(docnumrec)
        ld (celcntall),bc
        ld a,c
        or b
        jr z,filend8
        ld ix,celrecmem         ;relocate text and data pointer in cell records, generate controls
filend4 push bc
        ld l,(ix+celrectxt+0)
        ld h,(ix+celrectxt+1)
filend5 ld bc,0
        add hl,bc
        ld (ix+celrectxt+0),l   ;relocate text pointer
        ld (ix+celrectxt+1),h
        ex de,hl
        ld l,(ix+celrecdat+0)
        ld h,(ix+celrecdat+1)
        ld a,l
        or h
        jr z,filend7
filend6 ld bc,0
        add hl,bc
        ld (ix+celrecdat+0),l   ;relocate data pointer, if existing
        ld (ix+celrecdat+1),h
filend7 push ix
        ld l,(ix+celrecclm)     ;try to generate a control
        ld h,(ix+celrecrow)
        inc de:inc de:inc de
        call celcnw
        pop ix
        ld (ix+celrecctr),a
        or a
        call nz,fmtctr
        ld bc,12
        add ix,bc
        pop bc
        dec bc
        ld a,c
        or b
        jr nz,filend4
filend8 call docmod0
        ld hl,docstadat
        call doclod
        call dochsh
        jp fillod5
;hl=recadr dif, iy=sheet data, ix=memory management record -> init memory record and relocate "who points here"
filend2 push hl
        ld l,(ix+memstrbeg+0)       ;init memory management
        ld h,(ix+memstrbeg+1)
        push hl
        ld c,(iy+4)
        ld b,(iy+5)
        add hl,bc
        ld (ix+memstrend+0),l       ;beginfree=start+len
        ld (ix+memstrend+1),h
        ld l,(ix+memstrmax+0)
        ld h,(ix+memstrmax+1)
        or a
        sbc hl,bc
        ld (ix+memstrfre+0),l       ;free=max-len
        ld (ix+memstrfre+1),h
        pop hl                      ;hl=memory start
        pop bc                      ;bc=recadr dif
        ld e,(iy+0):ld (ix+memstrnum+0),e
        ld d,(iy+1):ld (ix+memstrnum+1),d
        ld a,e
        or d
        ret z
        push de:pop iy              ;iy=number of elements
filend3 ld a,(hl)
        inc hl
        ld e,(hl):inc hl:ld d,(hl)  ;get old pointer
        ex de,hl
        add hl,bc
        ex de,hl
        ld (hl),d:dec hl:ld (hl),e  ;set relocated pointer
        dec a
        ld e,a:ld d,0
        add hl,de
        dec iy
        ld a,iyl:or iyh
        jr nz,filend3
        ret

;### FILSTO -> save actual spreadsheet
filsto  call docsto
        ld hl,fldmaxy*256+256+fldmaxx+1
        ld (docshtdim),hl
        ld ix,(App_BnkNum-1)
        ld hl,docpth
        xor a
        call SyFile_FILNEW
        jp c,filsto5
        ld (filhnd),a
        ld ix,chuhed:ld hl,docheddat  :ld bc,56                 :call filsto1  ;head
        ld hl,(filhnd-1)
        call prpsav_b                                                          ;meta
        push hl:pop af
        jr c,filsto4
        ld ix,chucfg:ld hl,cfgdocbeg  :ld bc,cfgdocend-cfgdocbeg:call filsto1  ;config
        ld hl,29+docstalen+fldmaxx+1+fldmaxy+1
        ld (chusht+4),hl
        ld a,(App_BnkNum)
        ld ix,chusht:ld hl,docshtdat  :ld bc,29+docstalen       :call filsto7  ;sheet
        ld hl,ctrgrdver+2
        ld bc,fldmaxx+1
        call filsto8
        jr c,filsto4
        ld hl,ctrgrdhor+2
        ld bc,fldmaxy+1
        call filsto8
        jr c,filsto4
        ld ix,chucrc:ld hl,(docadrrec):ld bc,(doclenrec)        :call filsto1  ;records
        ld ix,chuctx:ld hl,(docadrtxt):ld bc,(doclentxt)        :call filsto1  ;text
        ld ix,chucdt:ld hl,(docadrdat):ld bc,(doclendat)        :call filsto1  ;data
        ld ix,chuend:ld bc,0                                    :call filsto1  ;end
        call docmod0
filsto3 ld a,(filhnd)
        jp SyFile_FILCLO
filsto4 push af
        call filsto3
        pop af
filsto5 ld e,filerrsav
        jp filerr
;ix=chunk, hl=data, bc=length
filsto1 ld (ix+4),c
        ld (ix+5),b
filsto7 push bc
        push hl
        push ix:pop hl
        ld bc,8
        call filsto8
        pop hl
        pop bc
        jr c,filsto6
        ld a,c
        or b
        ret z
        call filsto8
        ret nc
filsto6 pop hl
        jr filsto4
filsto8 ld a,(App_BnkNum)
        ld e,a
        ld a,(filhnd)
        jp SyFile_FILOUT

;### FILERR -> show error message
;### Input      A=code, HL=type, E=load(0)/save(5)
filerr  cp 128
        jr nc,filerr1
        call cnvdec
        ;...set error code
        ld a,127
filerr1 add e
        add errlodinfb-128
        jp prgalr

;### FILIMC -> import CSV
filimc  ld hl,_xchdic
        jp guijmp_b

;### FILEXC -> export CSV
filexc  ld hl,_xchdec
        jp guijmp_b

;### FILIMS -> import SYLK
filims  ld hl,_xchims
        jp guijmp_b

;### FILEXS -> export SYLK
filexs  ld hl,_xchexs
        jp guijmp_b


;==============================================================================
;### DOCUMENT ROUTINES ########################################################
;==============================================================================

docstatab
dw supcelctr +6:db 4
dw fldcurp     :db 6
dw cfgcolpen   :db 3
dw 0
docstalen   equ 4+6+9

;*** HEAD
docheddat
db "SCS",0              ; 4B file type
db 0,1                  ; 2B file version
db "SymCalc":ds 16-7    ;16B application name
db 0,1                  ; 2B application version
ds 4                    ; 4B date
ds 28                   ;28B *unused*

;*** SHET
docshtdat
docnumrec   dw 0            ;1W number of cell records
docadrrec   dw 0            ;1W orgadr of cell records
doclenrec   dw 0            ;1W length of cell records
docnumdat   dw 0            ;1W number of cell data elements
docadrdat   dw 0            ;1W orgadr of cell data
doclendat   dw 0            ;1W length of cell data
docnumtxt   dw 0            ;1W number of cell text elements
docadrtxt   dw 0            ;1W orgadr of cell text
doclentxt   dw 0            ;1W length of cell text
docstadat   ds docstalen    ;   document state
                            ;1W scroll X
                            ;1W scroll Y
                            ;1B cursor new X
                            ;1B cursor new Y
                            ;4B mark first/last
                            ;1B colour default pen
                            ;1B colour default paper
                            ;1B colour grid
                            ;6B *reserved*
docshtdim   db fldmaxx+1    ;1B field xlen
            db fldmaxy+1    ;1B field ylen
docshtnam   ds 9            ;9B sheet name+0 (will be used later when supporting multiple sheets)

;### DOCMOD0 -> marks doc for not modified
docmodf db 0    ;flag, if document has been modified

docmod0 xor a
        ld (docmodf),a
        ret

;### DOCLOD -> loads primary spreadsheet state
;### Input      HL=state data source
;### Destroyed  AF,BC,DE,HL,IY
doclod  call doclod0
        ld (visxln),de
        call doccol
        call mrkact
        jr z,doclod4
        call mrkfld9            ;activate marking
        call fldcur0
        ld a,64
        call mrkrem2
        jr doclod5
doclod4 call mrkrem0            ;activate cursor
doclod5 ld hl,(fldcurp)
        ld (fldcuro),hl
        ld de,(supcelctr+6)
        ld hl,(supcelctr+8)
        call barofs_b
        ld a,1
        ld (fmtdspf),a
        jp barcmr
doclod0 ld ix,docstatab
        ld b,0
doclod1 ld e,(ix+0)
        ld d,(ix+1)
        ld a,e
        or d
        ret z
        ld c,(ix+2)
doclod2 db 0
        ldir
doclod3 db 0
        ld c,3
        add ix,bc
        jr doclod1

;### DOCSAV -> saves primary spreadsheet state
;### Input      HL=state data destination
;### Destroyed  AF,BC,DE,HL,IY
docsav  db #3e:ex de,hl
        call docsav1
        call doclod0
        xor a
docsav1 ld (doclod2),a
        ld (doclod3),a
        ret

;### DOCCLR -> clears primary spreadsheet
docclrb dw 0,0
        dw 0,0,-1
        db 1,8,2
        ds 6

docclr  call hshini_b           ;reset hashtable
        ld de,ctrgrdver+2       ;reset cell sizes
        ld a,(cfgcelxln)
        ld c,fldmaxx
        call filsht4
        ld de,ctrgrdhor+2
        ld a,(cfgcelyln)
        ld c,fldmaxy
        call filsht4
        ld hl,cfgapppen         ;prepare default state
        ld de,docclrb+4+6
        ld bc,3
        ldir
        ld hl,docclrb           ;reset state
        call doclod
        call barini
docclr0 ld hl,-16               ;reset buttons
        ld (wintolrec1+6),hl
        ld (wintolrec2+6),hl
        ld a,supcelrec_cell     ;reset marking, controls
        ld (mrkfldid),a
        inc a
        ld (supcelgrp),a
        ld hl,supcelrec2
        ld (mrkfldadr),hl
        inc hl:inc hl
        ld (hl),64
        xor a
        ld l,a:ld h,a
        ld (ctrcelnum),a
        ld (mrkflda),hl
        ld (celcntall),hl
        ld (coprngsta),a        ;reset copypaste
        add 16
menmaidat2a equ $+1
        ld hl,menmaidat2a:call ibkbyt
        ld hl,celdatrec         ;reset data
        call docclr1
        ld hl,celtxtrec         ;reset text
docclr1 call docclr2
        call docclr2
        ld (hl),a
        inc hl
        ld (hl),a
        ret
docclr2 ld e,(hl):inc hl
        ld d,(hl):inc hl
        ld (hl),e:inc hl
        ld (hl),d:inc hl
        ret

;### DOCHSH -> generates hash table for all cell records
dochsh  call hshini_b           ;reset hashtable
        ld bc,(celcntall)
        ld hl,celrecmem
dochsh1 ld a,c:or b
        ret z
        push bc
        push hl
        ld e,l:ld d,h           ;de=cell record
        inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                  ;hl=clm/row
        call hshnew_b           ;create hash
        pop hl
        ld bc,celreclen
        add hl,bc
        pop bc
        dec bc
        jr dochsh1

;### DOCSTO -> prepares the primary spreadsheet for storing
;### Output     (celdatrec),(celtxtrec)=packed memory, (docadr/len/cntXXX)=prepared
docsto  ld hl,docstadat
        call docsav
        ld hl,celrecmem         ;get cell record infos
        ld (docadrrec),hl
        ld hl,(celcntall)
        ld (docnumrec),hl
        add hl,hl:add hl,hl ;*4
        ld e,l:ld d,h
        add hl,hl:add hl,de ;*12
        ld (doclenrec),hl
        or a
        call mempoic
        ld ix,celdatrec
        ld de,docnumdat         ;pack and get cell data infos
        call docsto1
        scf
        call mempoic
        ld ix,celtxtrec
        ld de,docnumtxt         ;pack and get cell text infos
docsto1 push de
        call memgrb
        pop de
        ld a,(ix+memstrnum+0):ld (de),a:inc de
        ld a,(ix+memstrnum+1):ld (de),a:inc de  ;store number
        ld a,(ix+memstrbeg+0):ld (de),a:inc de
        ld a,(ix+memstrbeg+1):ld (de),a:inc de  ;store adr
        ld l,(ix+memstrmax+0)
        ld h,(ix+memstrmax+1)
        ld c,(ix+memstrfre+0)
        ld b,(ix+memstrfre+1)
        or a
        sbc hl,bc
        ex de,hl
        ld (hl),e:inc hl                        ;store len
        ld (hl),d
        ret

;### DOCDRW -> redraws the primary spreadsheet
docdrw  ld de,256*winmairec_field-512+256-3
        ld a,(winmainum)
        call SyDesktop_WININH
        ld a,(winmaidat+1)
        and 8
        ld de,256*wintolrec_font+256-8
        ld a,(winmainum)
        call nz,SyDesktop_WINTOL
        call cellod
        jp fldcor

;### DOCCOL -> sets default and field colours for primary spreadsheet
doccol  ld a,(cfgcolpen)        ;set cell default colours
        add a:add a:add a:add a
        ld hl,cfgcolpap
        or (hl)
        ld (cfgfmtnum),a
        ld (cfgfmttxt),a
        ld (celcnvcol),a
        ld a,(cfgcolgrd)
        ld c,a
        add a:add a:add c
        ld (16*0+supcelrec1+4),a
        ld a,c:xor 3:ld c,a
        add a:add a:add c
        ld (16*1+supcelrec1+4),a
        ld a,(cfgcolpap)        ;set field colours
        add 128
        ld (supcelrec+4),a
        ld a,(cfgcolgrd)
        ld (ctrgrdhor),a
        add 128
        ld (ctrgrdver),a
        ret


;==============================================================================
;### MARKING ROUTINES #########################################################
;==============================================================================

;### MRKINI -> inits marking
mrkini  ld hl,(fldcurp)
        ld (fldmrkp),hl
        ld (fldmrks),hl
mrkini1 push bc
        push de
        ld a,1
        ld (mrkflda),a
        ld (fmtdspf),a
        call mrkfld9            ;init control
        call fldcur0            ;remove cursor
        ld a,(winmainum)
        ld de,supcelrec_cur*256+winmairec_field
        call SyDesktop_WINSIN
        rst #30
        ld a,64
        call mrkrem2
        call mrkfld0            ;show initialized control (origin cell)
        pop de
        pop bc
        ret

;### MRKSHF -> mark fields with cursor+shift
;### Input      C=dif, B=max, E=type (0=x, 1=y)
mrkshf  call mrkact
        call z,mrkini
        ld hl,(fldmrks)
        dec e
        ld a,l
        jr nz,mrkshf1
        ld a,h
mrkshf1 add c
        cp b
        jp nc,prgprz0
        inc e
        jr nz,mrkshf2
        ld l,a
        jr mrkshf3
mrkshf2 ld h,a
mrkshf3 ex de,hl
        ld a,1
        ld (mrkfldk),a
        jr mrkfld1

;### MRKFLD -> marks fields
mrkflda     db 0                ;flag, if marking is active  \
mrkfldk     db 0                ;flag, if keyboard marking   /
mrkfldid    db supcelrec_cell
mrkfldadr   dw supcelrec2

mrkfld  call fldmos
        ;jr c,...outside, scroll if possible
        ex de,hl
        xor a
        ld (mrkfldk),a
        ld hl,mrkflda
        cp (hl)
        jr nz,mrkfld1
        ld hl,(fldcurp)         ;no mark in progress -> check, if still same cell
        sbc hl,de
        jp z,prgprz0            ;yes, ignore
        call mrkini

mrkfld1 ld hl,(fldmrks)         ;** proceed marking
        ld a,e
        sub l
        ld b,a
        jr nz,mrkfld4
        ld a,d
        cp h
        jp z,prgprz0
        jr mrkfld5

mrkfld4 ld d,h
mrkfld5 ld (fldmrks),de         ;b=0 -> ydif, b>0 -> xdif
        inc b:dec b
        ld bc,(fldmrkp)

        ld iy,(mrkfldadr)
        ld ix,ctrgrdver+2
        jr nz,mrkfld6
        ld e,d
        ld l,h
        ld c,b
        inc iy:inc iy
        ld ix,ctrgrdhor+2
;c=origin, e=new, l=old, iy=ctrrec, ix=sizes
mrkfld6 ld a,e                  ;check if c between e and l
        cp l
        jr c,mrkfldb
        ld e,l
        ld l,a      ;e=<,l=>
mrkfldb ld a,c
        cp e
        jr c,mrkfldc
        jr z,mrkfldc
        cp l
        jr nc,mrkfldc
        call mrkfld0            ;yes -> full redraw
        call mrkfld9
        call mrkfld0
        jr mrkfldd

mrkfldc ld a,l
        cp c
        ld b,0
        jr c,mrkfld8
        ld a,e
        cp c
        jr c,mrkfld8
        inc b
mrkfld8 push bc
        ld a,e
        add b
        push hl
        push ix
        call flddim             ;hl=newpos
        pop ix
        ex (sp),hl
        ld a,l
        add b
        call flddim             ;hl=oldpos
        pop de                  ;de=newpos
        sbc hl,de
        jr nc,mrkfld7
        add hl,de
        ex de,hl
        or a
        sbc hl,de               ;de=pos,hl=len
mrkfld7 pop bc
        ld (iy+6),e
        ld (iy+7),d
        ld (iy+10),l
        ld (iy+11),h
        call mrkfld0            ;plot mark dif
        call mrkfld9            ;calculate full control pos/sizes

mrkfldd ld a,(mrkfldk)
        or a
        jr z,mrkflde
        ld a,(fldmrks+0)
        ld ix,ctrgrdver+2
        call flddim                 ;calculate control xpos -> hl=xpos, de=xwidth
        push hl
        ld c,e
        ld b,d
        ld a,(fldmrks+1)
        ld ix,ctrgrdhor+2
        call flddim                 ;calculate control ypos -> hl=ypos, de=ywidth
        pop ix
        call fldscl

mrkflde call fldcor
        call fldedt
        jp prgprz0

mrkfld0 ld a,(mrkfldid)
        ld d,a
        ld e,winmairec_field
        ld a,(winmainum)
        call SyDesktop_WINSIN
        rst #30
        ret

mrkfld9 ld a,(fldmrkp+0)        ;calculate control pos/size
        ld hl,(fldmrks+0)
        ld iy,(mrkfldadr)
        ld (iy+2),0             ;activate mark control
        ld (iy+4),128+64+1
        ld ix,ctrgrdver+2
        call mrkfld3
        ld a,(fldmrkp+1)
        ld hl,(fldmrks+1)
        inc iy:inc iy
        ld ix,ctrgrdhor+2
;a=pos1,l=pos2,ix=width table,iy=control record
mrkfld3 ld h,l
        cp l
        jr c,mrkfld2
        ld h,a
        ld a,l
mrkfld2 push hl
        push ix
        call flddim
        pop ix
        ld (iy+6),l
        ld (iy+7),h
        pop af
        push hl
        inc a
        call flddim
        pop de
        sbc hl,de
        ld (iy+10),l
        ld (iy+11),h
        ret

;### MRKREM -> unmark, if marking was active
;### Destroyed  AF,DE,IX,IY
mrkrem  push hl
        call mrkact
        jr z,mrkrem1
        push bc
        ld hl,-1                ;** marking was active -> deactivate it
        ld (fldmrks),hl
        call mrkfld0            ;remove marking
        call mrkrem0
        
        ld hl,(fldcurp)
        ld (fldcuro),hl
        pop bc
mrkrem1 pop hl
        ret
mrkrem0 ld hl,(mrkfldadr)       ;deactive mark control
        inc hl:inc hl
        ld (hl),64
        ld a,2                  ;reactivate cursor controls
mrkrem2 ld (supcelrec1+00+2),a
        ld (supcelrec1+16+2),a
        ret

;### MRKESC -> Esc, cancle marking
mrkesc  call mrkrem
        call fldcur2
        call fldcor
        jp prgprz0

;### MRKSRT -> returns sorted marked area
;### Output     HL=upper left, BC=lower right
;### Destroyed  AF
mrksrt  ld hl,(fldmrkp)
        ld bc,(fldmrks)
        ld a,l
        cp c
        jr c,mrksrt1
        ld l,c
        ld c,a
mrksrt1 ld a,h
        cp b
        ret c
        ld h,b
        ld b,a
        ret

;### MRKACT -> check if marking is active
;### Output     ZF=0 marking active
;###            ZF=1 no marking (HL=0)
;### Destroyed  AF,HL
mrkact  ld hl,(fldmrks)
        inc hl
        ld a,l
        or h
        ret

;### MRKMOS -> mark fields with mouse+shift
mrkmos  call fldmos
        push hl
        call mrkrem
        pop hl
        ld (fldmrks),hl
        ld hl,(fldcurp)
        jr mrkall1

;### MRKALL -> marks whole field
mrkall  call mrkrem
        ld hl,256*fldmaxy+fldmaxx
        ld (fldmrks),hl
        ld hl,0
mrkall1 ld (fldmrkp),hl
        call mrkini1
        jr mrkclm2

;### MRKCLM -> marks whole column(s)
;### Input      A=column
mrkclm  push af
        ld hl,jmp_keysta:rst #28
        bit 0,e
        pop de
        ld e,d
        jr z,mrkclm4
;e,d=column
mrkclm0 call mrkact         ;shift -> mark multiple
        ld a,(fldcurp+0)
        jr z,mrkclm3
        ld a,(fldmrkp+0)
mrkclm3 ld e,a
mrkclm4 push de
        call mrkrem
        pop de
        ld h,fldmaxy
        ld l,d
        ld (fldmrks),hl
        ld h,0
        ld l,e
mrkclm1 ld (fldmrkp),hl
        push hl
        call mrkini1
        pop hl
        ld (fldcurp),hl
mrkclm2 call fldfcf
        call fldcor
        jp prgprz0

;### MRKROW -> marks whole row(s)
;### Input      A=row
mrkrow  push af
        ld hl,jmp_keysta:rst #28
        bit 0,e
        pop de
        ld e,d
        jr z,mrkrow4
;e,d=row
mrkrow0 call mrkact         ;shift -> mark multiple
        ld a,(fldcurp+1)
        jr z,mrkrow3
        ld a,(fldmrkp+1)
mrkrow3 ld e,a
mrkrow4 push de
        call mrkrem
        pop de
        ld h,d
        ld l,fldmaxx
        ld (fldmrks),hl
        ld h,e
        ld l,0
        jr mrkclm1

;### MRKCHG -> check, if marking changed
;### Output     ZF=0 -> changed
mrkchgl ds 6

mrkchg  ld a,(cfgviwsta)
        or a
        ret z
        ld hl,fldcurp
        ld de,mrkchgl
        ld b,6
mrkchg1 ld a,(de)
        cp (hl)
        jr nz,mrkchg2
        inc hl
        inc de
        djnz mrkchg1
        ret
mrkchg2 ld c,b
        ld b,0
        ldir
        ret

;### MRKSUM -> show sum/average in status line
mrksums ds 5
mrksumc dw 0
mrksumt db "; Avg ":mrksumt0

mrksum  ld hl,0
        ld (mrksums+3),hl
        ld (mrksumc+0),hl
        ld hl,mrksum1
        call fldmul
        call mrksum5
        ld hl,cnvstrtxt
        ld de,txtbotinf+4
        dec c
        ldir
        ld hl,mrksumt
        ld bc,mrksumt0-mrksumt
        ldir
        push de
        ld hl,(mrksumc)
        ld a,l:or h
        jr z,mrksum3
        call fncwpt
        ld de,forclcval
        ld hl,mrksums
        call FLO_DIV
mrksum3 call mrksum5
        pop de
        ld hl,cnvstrtxt
        ldir
        ld a,(winmainum)
        ld e,winmairec_sum
        call SyDesktop_WININH   ;update status summary
        jp prgprzx
mrksum1 ld a,(hl)
        cp celtypnum
        jr z,mrksum2
        cp celtypfrn
        ret nz
mrksum2 ld bc,celrecdat
        add hl,bc
        ld e,(hl):inc hl
        ld d,(hl)
        inc de:inc de:inc de
        ld hl,(mrksumc)
        inc hl
        ld (mrksumc),hl
        ld hl,mrksums
        jp FLO_ADD
mrksum5 ld de,mrksums
        ld hl,254*256
        jp cnvstr


;==============================================================================
;### MENU LINKS ###############################################################
;==============================================================================

;### MENCSZ -> set column size
mencsz  ld a,(fldcurp+0)
        ld (baradj3+1),a
        jp barscl

;### MENRSZ -> set row size
menrsz  ld a,(fldcurp+1)
        ld (baradj3+1),a
        jp barsrw

;### MENCSL -> selects column
mencsl  ld a,(fldcurp+0)
        ld e,a:ld d,a
        jp mrkclm4

;### MENRSL -> selects row
menrsl  ld a,(fldcurp+1)
        ld e,a:ld d,a
        jp mrkrow4

;### MENAGN/LF/RG/CN -> alignment
menagn  ld a,%00000000:jr menaln
menalf  ld a,%01000000:jr menaln
menarg  ld a,%10000000:jr menaln
menacn  ld a,%11000000
menaln  ld c,#3f
menaln1 ld hl,celrecdsp
menaln2 call fmtaln1
        call fmtdsp6
        jp prgprz0

;### MENSNR/BL/IT -> style
mensnr  ld a,%00000000:jr mensty
mensbl  ld a,%00000001:jr mensty
mensit  ld a,%00000010
mensty  ld c,#fc
        jr menaln1

;### MENNFL/EX/PR/DT/TM/BL/BN/HX -> number format
mennfl  ld a,%00001111:jr mennum
mennex  ld a,%00101111:jr mennum
mennpr  ld a,%01001111:jr mennum
menndt  ld a,%01101111:jr mennum
menntm  ld a,%10001111:jr mennum
mennbl  ld a,%10101111:jr mennum
mennbn  ld a,%11001111:jr mennum
mennhx  ld a,%11101111
mennum  ld (mennum2+1),a
        ld hl,mennum1
mennum0 call fmtaln5
        jp prgprz0
mennum1 ld a,(hl)
        cp celtyptxt
        ret nc
        push hl
        ld de,celrecfmt
        add hl,de
        ld a,(hl)
        and #10
mennum2 or 0
mennum3 ld (hl),a
        dec hl
        ld de,cfdfmtdsp
        ldi:ldi
        jp cfdoky4

;### MENSEP -> 1000 separator
mensep  ld hl,mensep1
        jr mennum0
mensep1 push hl
        ld de,celrecfmt
        add hl,de
        ld a,(hl)
        xor 8
        jr mennum3

;### MENTOL -> show/hide toolbar
mentol  ld hl,cfgviwtol
        ld a,(hl)
        xor 1
        ld (hl),a
        call cfginit
mentol1 ld a,(winmaidat+0)
        cp 2
        ld a,(winmainum)
        push af
        call z,SyDesktop_WINMAX
        pop af
        call nz,SyDesktop_WINMID
        jp prgprz0

;### MENSTA -> show/hide statusbar
mensta  ld hl,cfgviwsta
        ld a,(hl)
        xor 1
        ld (hl),a
        ld hl,mrkchgl
        inc (hl)
        call cfginis
        jr mentol1


;==============================================================================
;### BAR ROUTINES #############################################################
;==============================================================================

;### BARINI -> inits both bars
barini  call barini0_b
        call barini1_b
        ld a,fldmaxx+1      ;set field width
        ld ix,ctrgrdver+2
        call flddim
        ld (supcelctr+2),hl
        ld a,fldmaxy+1      ;set field width
        ld ix,ctrgrdhor+2
        call flddim
        ld (supcelctr+4),hl
        ret

;### BARSIZ -> check, if click for bar resizing
;### Input      A=column/row, HL+DE=pos in cell, B=sensibility
;### Output     CF=1 -> resize-click
;### Destroyed  F,C,HL
barsiz  ld c,a
        ld a,l
        cpl
        cp b
        jr c,barsiz1
        add hl,de
        ld a,l
        cp b
        ld a,c
        ret nc
        dec c
barsiz1 ld a,c
        ret

;### BARSCL -> set column size with dialogue window
;### Input      (baradj3+1)=column
barscl  ld a,(cfgcelxln)
        ld c,0
        ld de,barccl0           ;clm sng
        ld hl,barscl1           ;clm mul
        ld ix,ctrgrdver+2
        jr barsst
barscl1 ld (barscl3+1),de       ;set clm multi
        ld iy,fldmrkp+1
        ld c,fldmaxy
        call barsst5
        jp nz,barccl0
        ld a,c
        sub l
        inc a
        ld b,a
        ld c,l
barscl2 push bc
        ld a,c
        ld (baradj3+1),a
        ld ix,ctrgrdver+2
        ld b,0
        add ix,bc
barscl3 ld de,0
        call barccl9
        pop bc
        inc c
        djnz barscl2
        jp barccl8

;### BARSRW -> set row size with dialogue window
barsrw  ld a,(cfgcelyln)
        ld c,1
        ld de,barcrw0
        ld hl,barsrw1
        ld ix,ctrgrdhor+2
        jr barsst
barsrw1 ld (barsrw3+1),de
        ld iy,fldmrkp+0
        ld c,fldmaxx
        call barsst5
        jp nz,barcrw0
        ld a,b
        sub h
        inc a
        ld b,a
        ld c,h
barsrw2 push bc
        ld a,c
        ld (baradj3+1),a
        ld ix,ctrgrdhor+2
        ld b,0
        add ix,bc
barsrw3 ld de,0
        call barcrw9
        pop bc
        inc c
        djnz barsrw2
        jp barcrw8

;### BARSST -> opens size set window
;### Input      A=default, C=type, DE=execution for single, HL=execution for marking, IX=width table, (baradj3+1)=column/row
barsst  ld b,a              ;b=def,c=typ
        ld (barsst3+1),de
        ld (barsst4+1),hl
        ld a,(baradj3+1)
        call flddim
        ld (barsst2+2),ix   ;e=width
        ld d,c
        ld l,b
        call barsdi_b
        jp prgprz0

barsst1 inc de
        call mrkact
barsst2 ld ix,0
barsst3 jp z,0       ;single
barsst4 jp 0         ;multi
barsst5 ld a,(iy+0)
        or a
        ret nz
        ld a,(iy+2)
        cp c
        ret nz
        call mrksrt
        xor a
        ret

;### BARMCL -> context menu for column bar
barmcl  ld d,0
barmcl1 ld a,c
        ld (baradj3+1),a
        ld e,c
        call barmen_b       ;-> d=column/row
        ld bc,barmcl_jmp
        jp jmptab
barmcl_jmp 
        dw prgprz0
        dw mrkclm4  ;barmcs
        dw mrkclm0  ;barmcm
        dw mrkrow4  ;barmrs
        dw mrkrow0  ;barmrm
        dw barscl   ;barmsc
        dw barsrw   ;barmsr
        dw movcsi   ;barmci
        dw movcsr   ;barmcr
        dw movrsi   ;barmri
        dw movrsr   ;barmrr

;### BARMRW -> context menu for row bar
barmrw  ld d,1
        jr barmcl1

;### BARCCL -> column bar clicked
barccl  call fldmos3            ;a=column
        jp c,prgprz0
        ld c,a
        ld a,(App_MsgBuf+3)
        cp DSK_SUB_MRCLICK
        jr z,barmcl
        ld a,c
        ld b,2
        call barsiz
        jp nc,mrkclm
        ld (baradj3+1),a
        ld ix,ctrgrdver+2
        call flddim             ;HL=cell begin, DE=cell width
        push ix
        or a
        ld bc,(supcelctr+6)
        sbc hl,bc
        ld bc,(winmairec1+6)
        add hl,bc
        push de
        push hl
        call winpos
        pop bc
        add hl,bc
        pop bc
        inc bc                  ;bc=xlen
        ex de,hl                ;de=xpos
        ld hl,-1
        ld ix,2000
        call SyDesktop_CONSIZ   ;de=new cell width+1
        pop ix
        jp c,prgprz0
;de=new size+1, (ix)=old width
barccl0 call barccl9
        jp z,prgprz0
barccl8 call barini0_b          ;update column bar
        call barcmr             ;update cursor/marking
        ld a,(winmainum)
        ld e,winmairec_field-2
        push af
        call SyDesktop_WININH   ;draw column bar
        pop af
        ld e,winmairec_field
        call SyDesktop_WININH   ;draw cells
        call fldfcf
        jp prgprz0
;de=new size+1, (ix)=old width -> adjust one size -> zf=1 no changes
barccl9 ld c,14
        call barccl1            ;de=abs size, hl=-dif size
        ret z
        ld a,6
        ld b,celrecclm
        call baradj
        xor a
        inc a
        ret
;de=new size+1, c=min -> de=abs size, hl=-dif size, zf=1 no change
barccl1 dec de
        inc d:dec d
        jr nz,barccl3
        ld a,126
        cp e
        jr nc,barccl2
barccl3 ld de,126
barccl2 ld a,e
        cp c
        jr nc,barccl4
        ld e,c
barccl4 ld h,d
        ld l,(ix+0)
        srl l                   ;hl=old width
        sla e                   ;de=new width
        ld (ix+0),e
        srl e
        or a
        sbc hl,de               ;hl=old-new=-dif
        ret

;### BARCRW -> row bar clicked
barcrw  call fldmos4            ;a=row
        jp c,prgprz0
        ld c,a
        ld a,(App_MsgBuf+3)
        cp DSK_SUB_MRCLICK
        jp z,barmrw
        ld a,c
        ld b,1
        call barsiz
        jp nc,mrkrow
        ld (baradj3+1),a
        ld ix,ctrgrdhor+2
        call flddim             ;HL=cell begin, DE=cell width
        push ix
        inc de
        push de:pop ix          ;ix=ylen
        or a
        ld bc,(supcelctr+8)
        sbc hl,bc
        ld bc,(winmairec1+6)
        add hl,bc               ;hl=ypos
        ld bc,22
        add hl,bc
        ld a,(winmaidat+1)
        bit 3,a
        jr z,barcrw7
        ld bc,17
        add hl,bc
barcrw7 push hl
        call winpos
        pop hl
        add hl,de
        ld de,-1
        ld bc,2000
        call SyDesktop_CONSIZ   ;hl=new cell height+1
        pop ix
        jp c,prgprz0
        ex de,hl
;de=new size+1
barcrw0 call barcrw9
        jp z,prgprz0
barcrw8 call barini1_b          ;update row bar
        call barcmr             ;update cursor/marking
        ld a,(winmainum)
        ld de,winmairec_field-1*256+256-2
        call SyDesktop_WININH   ;draw row bar+cells
        call fldfcf
        jp prgprz0
barcrw9 ld c,9
        call barccl1
        ret z
        ld a,8
        ld b,celrecrow
        call baradj
        xor a
        inc a
        ret

;### BARCMR -> update cursor/marking sizes after cell size update
barcmr  call mrkact
        call nz,mrkfld9
        call fldcur3
        jp fldcur0

;### BARADJ -> adjust cell controls after column/row size update
;### Input      (baradj3+1)=row/column, A=x6/y8ofs, B=celrecclm/celrecrow, DE=new width, HL=-width dif
baradj  ld (baradj4+1),a
        add 4
        ld (baradj7+1),a
        ld a,b
        ld (baradj2+2),a
        dec de
        ld (baradj8+1),de
        ld (baradj5+1),hl
        ld hl,(celcntall)       ;hl=cell count
        ld a,l
        or h
        ret z
        ld ix,celrecmem         ;check all existing cells
baradj1 push hl
        ld a,(ix+celrecctr)
        or a
        jr z,baradj9
        dec a
        ld l,a
baradj2 ld a,(ix+0)         ;get celrecclm/celrecrow
baradj3 cp 0                ;same or larger than modified row/clm?
        jr z,baradj6
        jr c,baradj9
        call celcad             ;** remove cell position
baradj4 ld hl,0             ;adjust position
        add hl,de
        ld e,(hl):inc hl
        ld d,(hl)
        ex de,hl
baradj5 ld bc,0             ;pos+=dif
        or a
        sbc hl,bc
        ex de,hl
        ld (hl),d:dec hl
        ld (hl),e
        jr baradj9
baradj6 call celcad             ;** resize cell size
baradj7 ld hl,0             ;adjust size
        add hl,de
baradj8 ld de,0             ;set new size
        ld (hl),e:inc hl
        ld (hl),d
baradj9 ld de,celreclen         ;next
        add ix,de
        pop hl
        dec hl
        ld a,l
        or h
        jr nz,baradj1
        ret


;==============================================================================
;### FIELD ROUTINES ###########################################################
;==============================================================================

fldcuro db 0,0      ;cursor position old \
fldcurp db 0,0      ;cursor position new |
fldmrkp db 0,0      ;mark 1st position   |
fldmrks db -1,-1    ;mark 2nd position   / (-1=nothing marked)


;### FLDFCF/FCE -> sets focus to field/formula editor
fldfcf  ld a,winmairec_field+1
fldfcf1 ld (winmaigrp+14),a
        ret
fldfce  ld a,(ctrfrminp+8)
        ld (ctrfrminp+6),a
        push af
        xor a
        ld (ctrfrminp+4),a
        call cellod0
        rst #30
        pop af
fldfce1 ld (ctrfrminp+4),a
        xor a
        ld (ctrfrminp+6),a
        call cellod0
        ld a,winmairec_formu+1
        ld (ctroldfoc),a
        ld a,winmairec_formu+1
        jr fldfcf1

;### FLDMLR -> release left mouse click
fldmlr  xor a
        ld (fldclkd),a
        ld (mrkflda),a
        ld hl,fldclke
        bit 0,(hl)
        jp z,prgprz0
        dec (hl)            ;** complete editor-support
        ld hl,(fldclkf)
        ld (fldcurp),hl
        call fldcur0    ;actual to old
        call mrkrem
        call fldcur3    ;calc curobj pos
        call fldcur6    ;plot cursor
        call fldcor
        ld a,(ctrfrminp+4)
        ld hl,ctrfrminp+6
        add (hl)
        call fldfce1
        jp prgprz0

;### FLDEDT -> copies coordinates to editor, if editor-click-mode active
fldedtb equ tmpbuf

fldedt  ld a,(fldclke)
        or a
        ret z
        ld a,(ctrfrminp+8)      ;check, if too long
        ld hl,ctrcelinp+8
        add (hl)
        ret c
        cp 251+1
        ret nc
        ld de,(ctrfrminp+4)     ;de=curpos
        ld hl,(ctrfrminp+6)     ;hl=sellen (>0 -> ins=cur, nxt=cur+sel, <0 -> ins=cur+sel, nxt=cur)
        bit 7,h
        add hl,de
        jr nz,fldedt1
        ex de,hl                ;hl=ins, de=nxt
fldedt1 ld (fldedt2+1),hl
        ld bc,txtfrminp
        add hl,bc
        ex de,hl                ;de=insadr
        add hl,bc               ;hl=nxtadr
        push de
        ld de,fldedtb
        ld bc,251
        ldir
        pop de
        ld hl,txtcelinp
        ld bc,(ctrcelinp+8)
        ldir
        ld hl,txtfrminp+251
        or a
        sbc hl,de
        ret z
        ld c,l
        ld b,h
        ld hl,fldedtb
        ldir
        ld hl,txtfrminp
        call strlen
        ld (ctrfrminp+8),bc
        ld hl,(ctrcelinp+8)
        ld (ctrfrminp+6),hl
fldedt2 ld hl,0
        ld (ctrfrminp+4),hl
        jp cellod0

;### FLDCLK -> field mouse click
fldclkd db 0                    ;flag, if mouse button down (mark area mode)
fldclke db 0                    ;flag, if editor-click-mode
fldclkf dw 0                    ;editor-click-mode orig curpos

fldclk  ld a,(App_MsgBuf+3)
        cp DSK_SUB_MRCLICK      ;right click?
        jr z,fldclk2
        ld a,(fldclkd)          ;mark session?
        or a
        jp nz,mrkfld
        ld hl,jmp_keysta:rst #28
        bit 0,e                 ;shift pressed?
        jp nz,mrkmos

        call fldmos
        jp c,prgprz0
        ld a,(ctroldedt)        ;check, if editor lost focus (means, we are/go maybe in editor-click-mode)
        or a
        jr nz,fldclk9           ;no -> go on
        inc a
        ld (ctroldedt),a        ;yes -> activate editor-click-mode; first set "lost focus"=no (<>0)
        ld a,(txtfrminp)
        sub "="                 ;check, if we edit a formular
        jr nz,fldclk9           ;no -> ignore, go on
        inc a
        ld (fldclke),a          ;yes -> activate editor-click-mode
        ld de,(fldcurp)
        ld (fldclkf),de         ;remember orig curpos

fldclk9 call fldcurf            ;remove marking and set "update cursor anyway", if there was a marking
        ld (fldcurp),hl
        ld a,1
        ld (fldclkd),a
        ld hl,0
        ld (mrkflda),hl

        ld a,(fldclke)
        or a
        jr z,fldclk7

        db #3e:or a         ;** editor click mode
        ld (fldcurd),a          ;reset "update cursor anyway"
        call fldcur0            ;actual to old
        call fldcur3            ;calc curobj pos
        call fldcur6            ;plot cursor
        call fldcor             ;plot coordinate
        call fldedt             ;insert coords into editor
        jp prgprz0

fldclk7 call fldcur1        ;** normal mode; ignore, if same pos, otherwise celsav, fldcor, fldcur5
        ld a,(App_MsgBuf+3)
        cp DSK_SUB_MDCLICK
        jp nz,prgprz0

fldclk1 call fldfce             ;double click -> editor focus
        jp prgprz0

fldclk2 call fldmos             ;right click -> context menu
        jp c,prgprz0
        ex de,hl                ;e,d=clicked cell
        call mrkact
        jr z,fldclk4
        call mrksrt             ;hl,bc=edges
        ld a,e
        cp l
        jr c,fldclk3
        inc c
        cp c
        jr nc,fldclk3
        ld a,d
        cp h
        jr c,fldclk3
        inc b
        cp b
        jr c,fldclk5
fldclk3 push de
        call mrkrem
        pop de
fldclk4 ld (fldcurp),de
        call fldcur1
fldclk5 ld hl,(coprngsta)
        call celmen_b
        ld bc,fldclk_jmp
        jp jmptab
fldclk_jmp
        dw prgprz0
        dw copcut   ;cmncut
        dw copcop   ;cmncop
        dw coppst   ;cmnpst
        dw fldclr   ;cmnclr
        dw cfdopn   ;cmnfmt
        dw movcid   ;cmncid
        dw movcir   ;cmncir
        dw movrwi   ;cmnrwi
        dw movcli   ;cmncmi
        dw movcru   ;cmncru
        dw movcrl   ;cmncrl
        dw movrwr   ;cmnrwr
        dw movclr   ;cmncmr

;### FLDMOS -> returns clicked cell
;### Input      (App_MsgBuf+4/6)=mouse position
;### Output     CF=0 -> L,H=cell column/row
;###            CF=1 -> outside
fldmos  call fldmos4
        ret c
        push af
        call fldmos3
        pop hl
        ld l,a
        ret
fldmos3 ld hl,(App_MsgBuf+4)    ;mouse xpos
        ld iy,winmairec1+0      ;field xpos/len
        ld bc,(supcelctr+6)     ;field xofs
        ld ix,ctrgrdver+2       ;cell xlens
        jr fldmos1
fldmos4 ld hl,(App_MsgBuf+6)    ;mouse ypos
        ld iy,winmairec1+2      ;field ypos/len
        ld bc,(supcelctr+8)     ;field yofs
        ld ix,ctrgrdhor+2       ;cell ylens
fldmos1 ld e,(iy+6)
        ld d,(iy+7)
        xor a
        sbc hl,de
        push hl
        ld l,(iy+10)
        ld h,(iy+11)
        ld de,-8
        add hl,de
        pop de
        sbc hl,de
        ret c
        ex de,hl
        add hl,bc               ;hl=absolute pos in field
;ix=siztab,hl=pixpos,a=0 -> a=cellpos
fldmos0 ld d,a
fldmos2 ld e,(ix+0)     ;5
        srl e           ;2
        or a            ;1
        sbc hl,de       ;4      ;hl+de=pos in cell
        ccf             ;1
        ret nc          ;2
fldmos5 inc ix          ;3
        inc a           ;1
        jr fldmos2      ;3 22

;### FLDKEY -> field key event
fldkeyt db 136      :dw fldcup    ;cursor up      -> up
        db 137      :dw fldcdw    ;cursor down    -> down
        db 138      :dw fldclf    ;cursor left    -> left
        db 139      :dw fldcrg    ;cursor right   -> right
        db  13      :dw fldcdw    ;return         -> down
        db   9      :dw fldcrg    ;tab            -> right
        db  11      :dw fldclf0   ;tab+shift      -> left
        db 142      :dw fldclk1   ;F2             -> edit cell
        db 127      :dw fldclr    ;CLR            -> clear actual/marked cells
        db  27      :dw mrkesc    ;ESC            -> cancel marking
        db 178      :dw cfdopn    ;Alt+0          -> open cell format dialogue
        db 189      :dw fldmlr    ;mouse left release
        db "A"-64   :dw mrkall    ;Ctrl+A         -> mark all
        db "C"-64   :dw copcop    ;Ctrl+C         -> Copy
        db "X"-64   :dw copcut    ;Ctrl+X         -> Cut
        db "V"-64   :dw coppst    ;Ctrl+V         -> Paste

        db "B"+87   :dw fmtfbl    ;Alt+B          -> bold
        db "I"+87   :dw fmtfit    ;Alt+I          -> italics
        db "P"+87   :dw fmtfdf    ;Alt+P          -> default (plain)
        db "L"-64   :dw fmtalf    ;Ctrl+L         -> left aligned
        db "R"-64   :dw fmtarg    ;Ctrl+R         -> right aligned
        db "E"-64   :dw fmtacn    ;Ctrl+E         -> centered
        db "G"-64   :dw fmtadf    ;Ctrl+G         -> default (general)

        db 179      :dw mennfl    ;Alt+1          -> Number
        db 180      :dw menndt    ;Alt+2          -> Date
        db 181      :dw menntm    ;Alt+3          -> Time
        db 182      :dw mennpr    ;Alt+4          -> Percentage
        db 183      :dw mennex    ;Alt+5          -> Scientific
        db 184      :dw mennbl    ;Alt+6          -> Boolean
        db 185      :dw mennbn    ;Alt+7          -> Binary
        db 186      :dw mennhx    ;Alt+8          -> Hexadecimal
        db 187      :dw mensep    ;Alt+9          -> 1000 separator

        db #fe      :dw fldcpu    ;Home/Alt+,     -> document begin
        db #ff      :dw fldcpd    ;End /Alt+.     -> document end

fldkeyt0
fldkeyc equ fldkeyt0-fldkeyt/3

fldkey  ld a,(App_MsgBuf+4)
        cp 32
        jr c,fldkey4
        cp 127
        jr nc,fldkey4
        ld (txtfrminp),a        ;** ASCII char -> jump to editor
        xor a
        ld (txtfrminp+1),a
        ld (ctrfrminp+2),a
        ld (ctrfrminp+6),a
        inc a
        ld (ctrfrminp+4),a
        ld (ctrfrminp+8),a
        ld hl,ctrfrminp+12
        set 7,(hl)
        call fldfce
        call cellod0
        jp prgprz0

fldkey4 ld hl,fldkeyt           ;** general field char
        ld b,fldkeyc
        call keychk
        jp nz,prgkey1
        jp (hl)

;### FLDCLR -> clears field(s), if existing
fldclr  ld hl,0
        ld (celcntrec),hl
        ld hl,fldclr1
        call fldmul
        jp c,prgprz0
        call nc,flddrw          ;redraw
        call nc,cellod6         ;clear input
        call recupd1
        jp prgprz0
fldclr1 push hl
        inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ex (sp),hl
        bit 6,(hl)
        jr z,flrclr2
        ld de,(celcntrec)
        dec de
        ld (celcntrec),de
flrclr2 call celrem
        pop hl
        jp recref

;### FLDMUL -> manipulate current or marked fields
;### Input      HL=manipulation routine (gets cell record in HL)
;### Output     CF=0 -> done, HL=upper left, BC=lower right
;###            CF=1 -> nothing happend
;### Destroyed  AF,BC,DE,HL,IX,IY
fldmul  ld (fldmul4+1),hl
        call mrkact
        jr nz,fldmul1
        ld hl,(fldcurp)     ;** single
        push hl
        call fldmul5
        pop hl
        ret c                   ;was already empty
        ld c,l:ld b,h
        ret
fldmul1 call mrksrt         ;** multi
fldmul2 push hl                 ;HL=upper left, BC=lower right
fldmul3 push hl
        push bc
        call fldmul5
        pop bc
        pop hl
        inc l
        ld a,c
        cp l
        jr nc,fldmul3
        pop hl
        inc h
        ld a,b
        cp h
        jr nc,fldmul2
        call mrksrt
        or a
        ret
fldmul5 call fldcel         ;** manipulate field, if existing
        ret c
fldmul4 call 0
        or a
fldmul6 ret
fldmul0 ld de,fldmul6       ;** error exception -> deactivate and show message
        ld (fldmul4+1),de
        jp prgalr

;### FLDDRW -> redraw field area
;### Input      HL=upper left, BC=lower right
;### Destroyed  AF,BC,DE,HL,IX,IY
flddrw  push bc
        call fldpos             ;IX=xpos left,HL=ypos up
        pop de
        push ix
        push hl                 ;(sp)=ypos up,xpos left
        ex de,hl
        call fldpos             ;IX+BC=xpos right,HL+DE=ypos down
        add hl,de
        ex de,hl                ;DE=ypos down
        push ix:pop hl
        add hl,bc               ;hl=xpos right, de=ypos down
        ex (sp),hl              ;hl=ypos up, de=ypos down, (sp)=xpos right, xpos left
        ld ix,(winmairec1+12)   ;field ylen
        ld bc,(supcelctr+8)     ;field yofs
        call flddrw1            ;de=ypos, hl=ylen
        push hl:pop iy          ;iy=ylen
        pop hl                  ;hl=xpos right
        jr c,flddrw4
        ex de,hl                ;de=xpos right, hl=ypos
        ld bc,(winmairec1+8)    ;..ofs correction
        add hl,bc
        ex (sp),hl              ;hl=xpos left, (sp)=ypos
        ld ix,(winmairec1+10)   ;field xlen
        ld bc,(supcelctr+6)     ;field xofs
        call flddrw1            ;de=xpos, hl=xlen
        jr c,flddrw4
        push hl:pop ix          ;ix=xlen
        ex de,hl                ;hl=xpos
        ld bc,(winmairec1+6)    ;..ofs correction
        add hl,bc
        pop bc                  ;bc=ypos
        ld e,winmairec_field
        ld a,(winmainum)
        jp SyDesktop_WINPIN
flddrw4 pop hl
        ret
;ix=len, bc=scroll ofs, hl=pos low, de=pos high -> de=pos, hl=len, cf=1 outside
flddrw1 or a
        sbc hl,bc
        jr nc,flddrw2
        ld hl,0
flddrw2 ex de,hl            ;de=low scroll-corrected
        or a
        sbc hl,bc           ;hl=high scroll-corrected
        ret c
        ld bc,-8
        add ix,bc
        push ix:pop bc      ;bc=len
        or a
        sbc hl,bc
        add hl,bc
        jr c,flddrw3
        ld l,c:ld h,b
flddrw3 ex de,hl            ;de=high len corrected
        or a
        sbc hl,bc
        ccf
        ret c
        add hl,bc           ;hl=low len checked
        ex de,hl
        or a
        sbc hl,de
        inc hl
        inc de              ;de=pos, hl=len
        ret

;### FLDCxx -> move cursor
fldcpu  ld hl,0                     ;home
        jr fldcpd1
fldcpd  ld de,256*fldmaxy+fldmaxx   ;end
        ld hl,0
        call movopt
        jr z,fldcpu
        ld l,c:ld h,b
fldcpd1 ld (fldcurp),hl
        call fldcurf
        jr fldcurc

fldclf0 ld bc,256*fldmaxx+256+255   ;shift+tab=left (ignore shift)
        ld hl,fldcurp+0
        ld e,0
        jr fldcur4

fldclf  ld c,-1                     ;left
        jr fldcurx
fldcrg  ld c,1                      ;right
fldcurx ld b,fldmaxx+1
        ld hl,fldcurp+0
        ld e,0
        jr fldcur

fldcup  ld c,-1                     ;up
        jr fldcury
fldcdw  ld c,1                      ;down
fldcury ld b,fldmaxy+1
        ld hl,fldcurp+1
        ld e,1

;c=difference, (hl)=position, b=max
fldcur  push bc                     ;check, if shift+cursor
        push de
        push hl
        ld hl,jmp_keysta:rst #28
        rr e
        bit 0,e
        pop hl
        pop de
        pop bc
        jp c,mrkshf
        jr z,fldcur4
        sla c:sla c:sla c           ;cursor+control -> skip 8 rows/columns

fldcur4 call fldcurf
        ld a,(hl)                   ;move cursor
        add c
        bit 7,c
        jr nz,fldcura
        jr c,fldcur8
fldcurb cp b
        jr c,fldcur7
        bit 7,c
        jr z,fldcur8
        jr fldcur9
fldcura jr nc,fldcur9
        jr fldcurb
fldcur9 xor a                       ;keep at beg
        jr fldcur7
fldcur8 ld a,b                      ;keep at end
        dec a
fldcur7 ld (hl),a
fldcurc call fldcur1
        jp prgprz0

fldcurf push hl                     ;if existing, remove marking and set "always update cursor" flag
        call mrkact
        pop hl
        ret z
        call mrkrem
        db #3e:scf
        ld (fldcurd),a
        ret

fldcur1 ld de,(fldcurp)             ;place cursor
        ld hl,(fldcuro)
fldcurd or a
        jr c,fldcure
        sbc hl,de
        ret z                       ;-> didn't change
fldcure db #3e:or a
        ld (fldcurd),a
        push de
        call celsav
        call fldcor                 ;show cell position
        pop hl
        ld (fldcuro),hl

fldcur5 call fldcur0                ;put actual to old cursor control coordinates
fldcur2 call fldcur3                ;set new cursor control coordinates
        call fldscl                 ;scroll field if necessary
        call fldcur6                ;redraw cursor
        ld a,1
        ld (fmtdspf),a              ;trigger format display update
        jp cellod                   ;load new cell under cursor

fldcur6 ld a,(winmainum)            ;redraw cursor
        ld de,winmairec_field+65536-512
        ld l,supcelrec_cur
        call SyDesktop_WINSIN
        rst #30
        ret

fldcur0 ld hl,supcelrec1+16+6       ;put actual to old cursor control coordinates
        ld de,supcelrec1+00+6
        ld bc,4*2
        ldir
        ret

fldcur3 ld hl,(fldcurp)
        call fldpos                 ;IX=xpos,BC=xsiz,HL=ypos,DE=ysiz
        ld (supcelrec1+16+06),ix
        ld (supcelrec1+16+08),hl
        inc bc
        ld (supcelrec1+16+10),bc
        dec bc
        inc de
        ld (supcelrec1+16+12),de
        dec de
        ret

;### FLDPOS -> calculates cell position and size
;### Input      L=column, H=row
;### Output     IX=xpos,BC=xsiz,HL=ypos,DE=ysiz
;### Destroyed  AF
fldpos  push hl
        ld a,l
        ld ix,ctrgrdver+2
        call flddim         ;hl=xpos, de=xlen
        ld c,e
        ld b,d              ;bc=xlen
        pop af              ;a=row
        push hl
        ld ix,ctrgrdhor+2
        call flddim         ;hl=ypos,de=ylen
        pop ix              ;ix=xpos
        ret

;### FLDDIM -> calculates cell begin and width
;### Input      IX=width table, A=position
;### Output     HL=cell begin, DE=cell width
;### Destroyed  AF,IX
flddim  ld hl,0
        ld d,l
flddim1 ld e,(ix+0)
        srl e
        or a
        ret z
        add hl,de
        inc ix
        dec a
        jr flddim1

;### FLDSCL -> scroll x/y if necessary
;### Input      ix=xpos,bc=xsiz,hl=ypos,de=ysiz
fldscl  push ix
        push bc
        ld ix,supcelctr+8           ;check for y-scrolling
        ld iy,winmairec1+12
        ld a,31
        call fldscl0
        pop de
        pop hl
        ld ix,supcelctr+6           ;check for x-scrolling
        ld iy,winmairec1+10
        ld a,30
;HL=new position, DE=cell width*2, (IX)=current offset, (IY)=field width, A=type (30=scroll X, 31=scroll Y)
fldscl0 ld c,(ix+0)
        ld b,(ix+1)
        or a
        sbc hl,bc
        jr c,fldscl1    ;hl+bc=new pos
        add hl,bc
        add hl,de
        ex de,hl        ;de=behind right cell
        ld l,(iy+0)
        ld h,(iy+1)
        push bc
        ld bc,-8
        add hl,bc       ;hl=field width
        pop bc
        push hl
        add hl,bc       ;hl=behind right field
        sbc hl,de
        pop hl
        ret nc
        ex de,hl        ;hl=behind right cell, de=field width
        or a
        sbc hl,de       ;hl=new pos
        jr fldscl2
fldscl1 add hl,bc
fldscl2 ld (supcelctr+12),hl
        ld c,a
        ld a,3+8
        ld (supcelctr+10),a
        call fldfcf
        ld a,c
        rst #20:dw jmp_keyput
        rst #30
        ld a,3+12
        ld (supcelctr+10),a
        ret

;### FLDCEL -> check if there is a cell at field position
;### Input      L,H=position
;### Output     CF=1 -> no cell, HL=0
;###            CF=0 -> HL=cell data record
;### Destroyed  AF,BC,DE,IX
fldcel  call hshfnd_b
        ld a,e
        sub 1
        ret

;### FLDCOR -> shows current cell or selection coordinates
;### Destroyed  AF,BC,DE,HL,IX,IY
fldcor  ld hl,(fldmrks)
        inc hl
        ld a,l
        or h
        ld de,txtcelinp
        jr nz,fldcor2
        ld hl,(fldcurp)         ;** single position
        ld b,0
        call fldcor3
fldcor1 ld a,b                  ;update control
        ld (ctrcelinp+4),a
        ld (ctrcelinp+8),a
        xor a
        ld (ctrcelinp+2),a
        ld (ctrcelinp+6),a
        ld a,(winmainum)
        ld e,winmairec_fpos
        jp SyDesktop_WININH
fldcor2 call mrksrt             ;** marked area
        push bc             ;hl=upper left, bc=lower right
        ld b,0
        call fldcor3
        ld (hl),":"
        inc hl
        inc b
        ex de,hl
        pop hl
        call fldcor3
        jr fldcor1
;hl=pos, de=str, b=cnt -> convert pos to string -> b,hl=de  updated
fldcor3 ex de,hl
        ld a,e
        inc a
        call fldcor6
        ld a,d
        inc a
        call cnv08s
        ld (hl),0
        ret
fldcor6 ld c,"A"-3      ;a=pos (1-), hl=str -> generate "AA" position text -> hl=behind
        dec a
fldcor4 inc c
        sub 26
        jr nc,fldcor4
        bit 6,c
        jr z,fldcor5
        inc c
        ld (hl),c
        inc hl
        inc b
fldcor5 add 26+"A"
        ld (hl),a
        inc hl
        inc b
        ret

;### FLDSET -> tries to set cursor/marking to entered cell/cell range
fldsetd ds 2
fldset  ld hl,0
        ld (forcnvrsc),hl
        ld iy,txtcelinp
        call chrwht
        ld ix,fldsetd
        call cmpref
        jr c,fldset2
        call mrkrem
        ld a,(fldsetd)
        cp fortokref+4
        jr nc,fldset1
        ld hl,(forcnvrsd)       ;single cell -> set cursor
        ld (fldcurp),hl
        call fldcur1
        call fldfcf
        jp prgprz0
fldset1 ld hl,(forcnvrrd+2)     ;cell range -> set marking
        ld (fldmrks),hl
        ld hl,(forcnvrrd+0)
        jp mrkclm1
fldset2 ld a,errforinf4         ;wrong reference
        call prgalr
        jp prgprz0


;==============================================================================
;### VISIBLE CELL CONTROL HANDLING ############################################
;==============================================================================

visxps  dw 0    ;old xpos
visxln  dw 0    ;old xlen
visyps  dw 0    ;old ypos
visyln  dw 0    ;old ylen

;### VISDRW -> rebuild and redraw new visible area
;### Input      HL,BC,IX,DE redraw area
;### Destroyed  AF,BC,DE,HL,IX,IY
visdrw  push hl
        push bc
        push ix
        push de
        call visadd
        pop iy
        pop ix
        pop bc
        pop hl
        jr nc,visdrw1
        ld e,winmairec_field
        ld a,(winmainum)
        call SyDesktop_WINPIN
visdrw1 jp prgprzx

;### VISSCL -> check, if visible field has been changed, recalculates cell range and returns changed area
;### Output     (vis???)=updated, CF=0 -> nothing changed, CF=1 -> changed, redraw HL,BC,IX,DE area
;### Destroyed  AF,BC,DE,HL,IX,IY
visscl  ld hl,(visxln)
        ld de,(winmairec1+10)
        ld (visxln),de
        or a
        sbc hl,de
        ld hl,(visyln)
        ld de,(winmairec1+12)
        ld (visyln),de
        jr c,visscl0            ;xlen increased -> redraw all
        sbc hl,de
        jr c,visscl0            ;ylen increased -> redraw all

        ld bc,(winmairec1+10)
        ld de,(supcelctr+6)
        ld ix,visxps
        call visscl3
        jr nc,visscl6
        push hl
        push bc
        call visscl5
        pop ix
        pop hl
        jr c,visscl1
        ld de,(winmairec1+6)
        add hl,de
        jr c,visscl1
        ld iy,visrngx
        jr visscl2
visscl0 ld hl,(supcelctr+6)
        ld (visxps),hl
        ld hl,(supcelctr+8)
        ld (visyps),hl
visscl1 ld hl,(winmairec1+6)
        ld ix,(winmairec1+10)
        ld iy,visrng
visscl2 ld bc,(winmairec1+8)
        ld de,(winmairec1+12)
visscl8 push bc
        push de
        push hl
        push ix
        ld de,visscl7
        push de
        jp (iy)
visscl7 pop ix
        pop hl
        pop de
        pop bc
        scf
        ret
visscl6 call visscl5
        ret nc
        ld de,(winmairec1+8)
        add hl,de
        jr c,visscl1
        ld e,c:ld d,b
        ld c,l:ld b,h
        ld hl,(winmairec1+6)
        ld ix,(winmairec1+10)
        ld iy,visrngy
        jr visscl8

visscl5 ld bc,(winmairec1+12)
        ld de,(supcelctr+8)
        ld ix,visyps
;bc=totlen, de=newpos, (ix)=oldpos -> cf=1 -> hl,bc=redraw pos,len (de=destroyed)
visscl3 ld l,(ix+0)
        ld h,(ix+1)
        or a
        sbc hl,de           ;hl=old-new
        ld (ix+0),e
        ld (ix+1),d
        ret z
        push af
        ex de,hl            ;de=old-new
        ld hl,-8
        add hl,bc
        ex de,hl            ;hl=old-new, de=total len
        pop af
        jr c,visscl4
        ld c,l:ld b,h       ;old>new -> refxps=0, refxln=old-new
        sbc hl,de
        ld hl,0
        ret c
        dec hl
        scf
        ret
visscl4 xor a:sub l:ld c,a  ;old<new -> refxps=len+(old-new), refxln=-(old-new)
        sbc a:sub h:ld b,a
        add hl,de
        ret c
        ld hl,-1
        scf
        ret

;### VISRNG(X/Y) -> calculates the visible cell range
;### Destryoed  AF,BC,DE,HL,IX
visrng  call visrngy
visrngx ld hl,(supcelctr+6)
        ld ix,ctrgrdver+2
        xor a
        call fldmos0
        ld (visrmxl+1),a
        ld (visadxl+1),a
        ex de,hl
        ld hl,(winmairec1+10)
        ld bc,-8
        add hl,bc
        ex de,hl
        add hl,de
        jr nc,visrng1
        ld d,0
        call fldmos5
visrng1 inc a
        ld (visrmxh+1),a
        ld (visadxh+1),a
        ret
visrngy ld hl,(supcelctr+8)
        ld ix,ctrgrdhor+2
        xor a
        call fldmos0
        ld (visrmyl+1),a
        ld (visadyl+1),a
        ex de,hl
        ld hl,(winmairec1+12)
        ld bc,-8
        add hl,bc
        ex de,hl
        add hl,de
        jr nc,visrng2
        ld d,0
        call fldmos5
visrng2 inc a
        ld (visrmyh+1),a
        ld (visadyh+1),a
        ret

;### VISREM -> removes invisible cell controls
;### Input      L=number of cell controls
;### Output     CF=1 -> not all cell controls have been removed, L=remaining cell controls
;###            CF=0 -> all cell controls have been removed, L=0, IX=last cell with removed control, BC=remaining counters
;### Destroyed  AF,BC,DE,IX,IY
visrem  ld ix,celrecmem
        ld bc,(celcntall)
visrem4 ld de,celreclen
visrem1 ld a,c:or b             ;2
        scf                     ;1
        ret z                   ;2
        ld a,(ix+celrecctr)     ;5
        or a                    ;1
        jr z,visrem2            ;2
        ld a,(ix+celrecrow)     ;5
visrmyl cp 0                    ;2
        jr c,visrem3            ;2
visrmyh cp 0                    ;2
        jr nc,visrem3           ;2
        ld a,(ix+celrecclm)     ;5
visrmxl cp 0                    ;2
        jr c,visrem3            ;2
visrmxh cp 0                    ;2
        jr nc,visrem3           ;2
visrem2 add ix,de               ;4
        dec bc                  ;2
        jr visrem1              ;3 48max
visrem3 ld a,(ix+celrecctr)     ;invisible -> remove control
        ld (ix+celrecctr),0
        push bc
        push hl
        push ix
        dec a
        call celcrm
        pop ix
        pop hl
        pop bc
        ld de,celreclen
        or a
        dec l
        ret z
        jr visrem2

;### VISADD -> tries to adds cell controls, which are now visible
;### Output     CF=1 -> new controls have been added
;### Destroyed  AF,BC,DE,HL,IX,IY
visadd  ld bc,(celcntall)
        inc b:dec b
        jr nz,visadd8
        ld a,(ctrcelnum)        ;check, if all cells have controls
        cp c
        ret z                   ;yes, finished
visadd8 ld (visadd6+1),bc       ;prepare pointer/counter for both remove/add
        bc_counter
        ld ix,celrecmem
        ld (visadd6+5),ix
        ld iyl,0

        ld de,celreclen
visadd1 ld a,(ix+celrecctr)     ;5
        or a                    ;1
        jr nz,visadd2           ;2
        ld a,(ix+celrecrow)     ;5
visadyl cp 0                    ;2
        jr c,visadd2            ;2
visadyh cp 0                    ;2
        jr nc,visadd2           ;2
        ld a,(ix+celrecclm)     ;5
visadxl cp 0                    ;2
        jr c,visadd2            ;2
visadxh cp 0                    ;2
        jr c,visadd3            ;2
visadd2 add ix,de               ;4
        djnz visadd1            ;4 42max
        dec c
        jr nz,visadd1
        jr visadd9

visadd3 ld hl,ctrcelnum         ;visible -> try to add control
        ld a,ctrcelmax
        cp (hl)
        jr z,visadd5
visadd4 ld e,(ix+celrectxt+0)
        ld d,(ix+celrectxt+1)
        inc de:inc de:inc de
        ld l,(ix+celrecclm)
        ld h,(ix+celrecrow)
        push bc
        push ix
        call celcnw
        pop ix
        ld (ix+celrecctr),a
        call fmtctr6            ;adjust control
        pop bc
        ld de,celreclen
        ld iyl,1
        jr visadd2
visadd5 push bc                 ;no free control -> try to remove invisible
        push ix
visadd6 ld bc,0
        ld ix,0
        ld l,1
        push iy
        call visrem4
        pop iy
        jr c,visadd7
        add ix,de
        ld (visadd6+5),ix
        dec bc
        ld (visadd6+1),bc
        pop ix
        pop bc
        jr visadd4
visadd7 pop hl                  ;no invisible found -> abort
        pop hl
visadd9 or a
        dec iyl
        ret nz
        scf
        ret


;==============================================================================
;### CELL HANDLING ############################################################
;==============================================================================

;### CELIMP -> prepares/finishes cell import
;### Input      A=type (0=prepare, 1=finish; redraw cells)
;### Output     prepare -> CF=1 -> cancel, CF=0 -> HL=tmpbuf
celimp  or a
        jr nz,celimp1
        call coppst0
        call filnew0
        ret c
        ld hl,tmpbuf
        ret
celimp1 call recupd1
        jp docdrw

;### CELPUT -> parses string and copies result into cell
;### Input      L,H=cell, tmpbuf=string, BC=length (with 0term)
;### Output     CF=1 -> memory full
celput  ld (copcelrec+celrecclm),hl
        push hl
        ld hl,tmpbuf
        dec c
        call celcnv0
        ld hl,celcnvtln
        ld de,copceltxl
        ld bc,252*2+4
        ldir
        ld a,(celcnvtyp)
        ld (copcelrec+celrectyp),a
        ld hl,celcnvref
        ld de,copcelrec+celrecref
        ld bc,4
        ldir
        pop hl
        jp copput

;### CELGET -> gets cell content as string (for export)
;### Input      L,H=cell, E=type (+1=as displayed, +2=formula)
;### Output     HL=text, BC=length (only if A>0; without 0term), A=type
;### Destroyed  AF,DE,IX,IY
celget  xor a
        ld (celcnvtxt),a
        push de
        call fldcel
        pop de
        jr c,celget0
        ld a,(hl)
        ld (celget5+1),a
        cp celtyptxt
        jr z,celget2        ;cell is text -> just take text
        cp celtypnum
        jr z,celget3        ;cell is value -> take it with/without format
        cp celtyperf
        jr z,celget2        ;cell is formula with syntax error -> take text
        cp celtyperv
        jr z,celget2        ;cell is formula with value error -> take formula
        bit 1,e
        jr z,celget3        ;cell is formula -> take number if no formula required
celget4 push hl:pop ix      ;** take formula
        call celref
        push hl:pop ix
        ld iy,celcnvtxt+1
        ld (iy-1),"="
        call fortxt
        ld c,a
        ld b,0
        ld hl,celcnvtxt
        jr celget5
celget0 ld hl,celcnvtxt     ;** empty cell
        xor a
        ret
celget2 ld de,celrectxt     ;** take displayed text
        add hl,de
        ld a,(hl):inc hl
        ld h,(hl):ld l,a
        ld a,(hl)
        inc hl:inc hl:inc hl
        sub 4
        ld c,a
        ld b,0
        ld de,celcnvtxt
        push de
        push bc
        ldir
        pop bc
        pop hl
celget5 ld a,0
        ret
celget3 bit 0,e             ;*** take number with/without format
        jr nz,celget2
        push hl:pop ix
        call cellodc
        push hl
        call strlen
        pop hl
        jr celget5

;### CELLOD -> loads current cell into formula input
;### Input      (fldcurp)=field position
cellod  ld hl,ctrfrminp+12
        res 7,(hl)
        ld hl,(fldcurp)             ;get cell content
        call fldcel
        jr nc,cellod1
cellod6 ld a,(ctrfrminp+8)          ;no cell -> empty input
        or a
        ret z
        ld hl,ctrfrminp+2
        ld de,ctrfrminp+3
        ld bc,8-1
        ld (hl),b
        ldir                        ;clear input
        xor a
        ld (txtfrminp),a
        jr cellod0
cellod1 ld a,(hl)                   ;check, what cell type
        push hl:pop ix
        cp celtyptxt
        jr z,cellod3
        cp celtypfrn
        jr z,cellod5
        cp celtyperv
        jr z,cellod5
        cp celtyperf
        jr z,cellod7
        cp celtypnum
        ;jr nz,cellod...other
        call cellodc
        jr cellod2

cellod7 ld l,(ix+celrecdat+0)   ;** formula with syntax error
        ld h,(ix+celrecdat+1)
        jr cellod8
        
cellod3 ld l,(ix+celrectxt+0)   ;** text
        ld h,(ix+celrectxt+1)
cellod8 ld a,(hl)
        inc hl:inc hl:inc hl
        sub 4
        ld c,a
cellod2 ld ix,ctrfrminp
        ld b,0
        call strini1
        res 7,(ix+12)
        ld de,txtfrminp
        inc c
        ldir
cellod0 ld a,(winmainum)
        ld e,winmairec_formu
        jp SyDesktop_WININH

cellod5 call celref             ;** formula
        push hl:pop ix
        ld iy,txtfrminp+1
        ld (iy-1),"="
        call fortxt
        ld ix,ctrfrminp
        call strini
        jr cellod0

cellodc ld e,(ix+celrecdat+0)   ;** number
        ld d,(ix+celrecdat+1)
        inc de:inc de:inc de
        ld a,(ix+celrecfmt)
        and #e0
        cp 32*3
        jr c,cellod9                ;float/exp/percent
        cp 32*5
        jr z,cellod9                ;boolean (=float)
        jr nc,cellod4
        call timedt                 ;date/time
        jr celloda
cellod9 ld hl,256*254+0             ;float
        call cnvstr
celloda dec c
        ld hl,cnvstrtxt
        ret
cellod4 ld h,7                      ;bin/hex
        push af
        call cnvbin
        pop af
        cp 32*6
        ld a,"%"
        jr z,cellodb
        ld a,"#"
cellodb ld hl,cnvstrtxtp
        ld (hl),a
        ret

;### CELSAV -> saves formula input to cell
;### Input      (fldcuro)=field position
celsav  ld hl,ctrfrminp+12          ;check, if modified
        bit 7,(hl)
        ret z
        ld hl,(fldcuro)
        ld a,l
        and h
        inc a
        ret z                       ;no old cell selected -> reject
        ld (fncpos),hl
        call fldcel                 ;check, if existing cell
        ld (celcnvrec),hl
        push af
        push hl
        call celcnv                 ;convert input to cell data
        pop hl
        pop af
        ld a,(celcnvtyp)
        jr nc,celsav5
        or a                        ;cell was empty
        ret z                       ;still empty -> finished

        ld bc,(fldcuro)         ;** NEW CELL
        call celnew
        jp c,celmem1
        call celurf                 ;update reference counter
        ld bc,(celcnvdln)
        ld de,celcnvdat
        call celuda                 ;create data
        jp c,celmem0
        ld bc,(celcnvtln)
        ld de,celcnvtxt
        call celutx                 ;create text
        jp c,celmem0
        push hl
        ld hl,(fldcuro)             ;create new control, HL=col/row, DE=text
        call celcnw                 ;A=control (1-x; 0=no free control), IY=form record
        pop hl
        push hl
        inc hl:inc hl:inc hl
        ld (hl),a                   ;set control
        pop hl
        push af
        or a
        push hl:pop ix
        ld de,(celcnvcol)           ;e=col, d=dsp
        ld a,(celcnvfmt)
        ld c,a                      ;c=fmt
        call fmtdft2                ;set format for cell and control
        pop af
        call celdrw
        ld hl,(fldcuro)
        jp recupd

celsav5 or a                        ;cell was existing
        jr nz,celsav4
celsav1 call celrem             ;** REMOVE CELL
        ld hl,(fldcuro)
        ld c,l:ld b,h
        call flddrw
        ld hl,(fldcuro)
        jp recupd

celsav4 cp (hl)                 ;** UPDATE CELL
        ld (hl),a
        scf
        call nz,fmtdft
        call celurf                 ;update reference counter
        ld bc,(celcnvdln)
        ld de,celcnvdat
        call celuda                 ;update data
        jr c,celmem0
        ld (celmem3+1),hl
        ld de,celrecctr             ;update text
        add hl,de
        ld a,(hl)
        push af
        inc hl
        ld ix,celtxtrec
        ld bc,(celcnvtln)
        scf
        push bc
        call memrsz                 ;resize text, update cell text and text control pointer
        pop bc
        jr c,celmem3
        ex de,hl
        inc de:inc de:inc de
        ld hl,celcnvtxt
celsav2 ld a,(de)                   ;copy new text to old and search for differences
        cp (hl)
        jr z,celsav3
        inc b
celsav3 ldi
        inc c:dec c
        jr nz,celsav2
        pop af
        inc b:dec b
        ret z                       ;cell text didn't change -> finished
        call celdrw
        ld hl,(fldcuro)
        jp recupd

;### CELMEMx -> show message and optionally removes cell when memory full (coming from CELSAV)
;### Input      IX or HL=cell record
celmem3 ld hl,0
        pop af
celmem0 ld a,errmeminfb             ;hl=record
        call celmem2
        jr celsav1
celmem1 ld a,errmeminfa             ;was new cell try, no remove
celmem2 push hl                     ;DE=show message, clear edit
        call prgalr
        pop hl
        ret

;### CELDRW -> redraw cell control
;### Input      A=control (0 or 1-x)
;### Destroyed  AF,BC,DE,HL,IX,IY
celdrw  or a
        ret z                       ;no control -> finished
        ld l,a
        dec l
        call celcad                 ;hl=control record
        add supcelrec_cell-1
        ld d,a
        inc hl:inc hl:inc hl
        ld a,(hl)
        push af
        push hl
        set 6,(hl)                  ;always fill background on updates
        ld e,winmairec_field
        ld a,(winmainum)
        call SyDesktop_WINSIN       ;update display
        rst #30
        pop hl
        pop af
        ld (hl),a
        ret

;### CELUPD -> updates cell data/text
;### Input      HL=pointer, BC=source length, DE=source address, IX=memory management record, CF=flag, if textdata
;### Output     CF=0 -> DE=destination address
;###            CF=1 -> memory full
;### Destroyed  AF,BC,DE,HL,IY
celupd  push bc
        push de
        call celrsz
        ex de,hl
        pop hl
        pop bc
        ret c
        ret z
        inc de:inc de:inc de
        push de
        ldir
        pop de
        ret

;### CELUDA -> updates cell data
;### Input      HL=cell record, BC=source length, DE=source address
;### Output     CF=0 -> DE=destination address
;###            CF=1 -> memory full
;### Destroyed  AF,BC,DE,IX,IY
celuda  ld a,1
        ld (docmodf),a
        push hl
        push de
        ld de,celrecdat
        add hl,de
        pop de
        ld ix,celdatrec
        call celupd
        pop hl
        ret

;### CELUTX -> updates cell text
;### Input      HL=cell record, BC=source length, DE=source address
;### Output     CF=0 -> DE=destination address
;###            CF=1 -> memory full
;### Destroyed  AF,BC,DE,IX,IY
celutx  push hl
        push de
        ld de,celrectxt
        add hl,de
        pop de
        ld ix,celtxtrec
        scf
        call celupd
        pop hl
        ret

;### CELURF -> updates cell reference counter
;### Input      HL=cell record, (celcnvref)=counter
;### Desstroyed AF,BC
celurf  push hl
        ld bc,celrecref
        add hl,bc
        ld a,(celcnvref)
        ld (hl),a
        pop hl
        ret

;### CELRSZ -> adds/resizes/removes cell data/text
;### Input      IX=memory management record, HL=pointer to element, C=new payload length (0-252), CF=flag, if textdata
;### Output     CF=0 -> ok, ZF=0 -> (pointer),HL=address, ZF=1 -> was removed
;###            CF=1 -> memory full
;### Destroyed  F,BC,DE,HL,IY
celrsz  inc c:dec c
        jr z,celrsz3
        push af
        ld a,(hl)
        inc hl
        or (hl)
        dec hl
        jr nz,celrsz2
        pop af          ;** new (oldpoi=0)
        call memnew
celrsz1 ld a,0
        inc a
        ret
celrsz2 pop af          ;** resize
        call memrsz
        jr celrsz1
celrsz3 ld e,(hl)       ;** delete
        inc hl
        ld d,(hl)
        ld a,e:or d
        ret z
        xor a
        ld (hl),a
        dec hl
        ld (hl),a
        ex de,hl
        call memdel
        xor a
        ret

;### CELCAD -> gets control addresses
;### Input      L=control (0-x)
;### Output     DE=form record, HL=control record
;### Destroyed  F,BC
celcad  ld h,0
        add hl,hl           ;*2
        ld e,l
        ld d,h
        add hl,hl           ;*4
        add hl,hl:add hl,hl ;*16
        ld bc,supcelrec2
        add hl,bc
        ex de,hl                ;de=form record
        ld c,l
        ld b,h
        add hl,hl           ;*4
        add hl,bc           ;*6
        ld bc,ctrcelmem
        add hl,bc               ;hl=control record
        ret

;### CELCNW -> adds a new control
;### Input      L=column, H=row, DE=text
;### Output     A=control (1-x; 0=no free control), IY=form record
;### Destroyed  AF,BC,DE,HL,IX,IY
celcnw  ld (celcnw1+1),de
        ld (celcnw2+1),hl
        ld hl,ctrcelnum
        ld a,ctrcelmax
        sub (hl)
        jr nz,celcnw3
        push hl
        ld l,1
        call visrem             ;no free control -> try to remove invisible
        pop hl
        ld a,0
        ret c
celcnw3 push hl
        ld hl,(mrkfldadr)       ;move marker
        ld e,l
        ld d,h
        ld bc,16
        add hl,bc
        ld (mrkfldadr),hl
        ex de,hl
        ldir
        ld hl,mrkfldid
        inc (hl)
        ld hl,supcelgrp+0       ;increase control count
        inc (hl)
        pop hl

        inc (hl)
        ld l,(hl)
        dec l
        call celcad             ;de=form record, hl=control record
        push de:pop iy
        ld (iy+2),1
        ld (iy+3),255
        ld (iy+4),l
        ld (iy+5),h
celcnw1 ld de,0
        ld (hl),e:inc hl        ;set text
        ld (hl),d:inc hl
celcnw2 ld hl,0
        call fldpos             ;IX=xpos,BC=xsiz,HL=ypos,DE=ysiz
        inc hl
        ld (iy+08),l            ;set ypos
        ld (iy+09),h
        push ix:pop hl
        inc hl
        ld (iy+06),l            ;set xpos
        ld (iy+07),h
        dec bc
        ld (iy+10),c            ;set xlen
        ld (iy+11),b
        dec de
        ld (iy+12),e            ;set ylen
        ld (iy+13),d
        ld a,(ctrcelnum)
        ret

;### CELNEW -> adds a new cell
;### Input      A=type, C=column, B=row
;### Output     CF=0 -> HL=cell record
;###            CF=1 -> memory full
;### Destroyed  AF,BC,DE,HL,IX,IY
celnew  ld hl,(celcntall)
        ld de,celrecmax
        or a
        sbc hl,de
        scf
        ret z
        add hl,de
        inc hl
        ld (celcntall),hl
        add hl,hl:add hl,hl ;*4
        ld e,l:ld d,h
        add hl,hl:add hl,de ;*12
        ld de,celrecmem-celreclen
        add hl,de               ;hl=new cell record
        ld (hl),a               ;set type
        push hl
        inc hl
        push hl
        ld e,l
        ld d,h
        inc de
        ld (hl),0
        push bc
        ld bc,celreclen-2
        ldir                    ;reset cell record
        pop bc
        pop hl
        ld (hl),c               ;set column
        inc hl
        ld (hl),b               ;set row

        dec hl
        dec hl
        ex de,hl
        ld l,c:ld h,b
        call hshnew_b
        or a

        pop hl
        ret

;### CELREM -> removes a cell record
;### Input      HL=cell record
;### Destroyed  AF,BC,DE,HL,IX,IY
celrem  push hl                 ;** remove cell

        inc hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        push hl
        ex de,hl
        call hshrem_b        
        pop hl
        inc hl

        ld a,1
        ld (docmodf),a
        ld a,(hl)
        push af
        ld de,celrectxt-3
        add hl,de
        push hl
        ld ix,celtxtrec
        call celrsz3            ;** delete text
        pop hl
        inc hl:inc hl
        ld ix,celdatrec
        call celrsz3            ;** delete data
        pop af                      ;a=control
        pop de                      ;de=record
        ld hl,(celcntall)
        dec hl
        ld (celcntall),hl           ;decrease cell count
        add hl,hl
        add hl,hl       ;*4
        ld c,l:ld b,h
        add hl,hl       ;*8
        add hl,bc       ;*12
        ld bc,celrecmem
        add hl,bc                   ;hl=last record
        sbc hl,de
        jr z,celrem1
        add hl,de
        ld bc,12                ;** move last cell to removed cell
        push de
        ldir
        pop hl
        push af

        ld e,l:ld d,h               ;de=moved cell record adr
        inc hl
        ld a,(hl)
        inc hl
        push hl
        ld h,(hl)
        ld l,a                      ;hl=moved cell clm/row
        call hshupd_b
        pop hl

        call celrem2                ;update "who points here" for text and data
        call celrem2
        pop af
celrem1 sub 1
        ret c                       ;cell had no control -> finished
        jr celcrm               ;** remove control
celrem2 inc hl:inc hl
        ld e,(hl):inc hl
        ld d,(hl):dec hl
        ld a,e:or d
        ret z
        ex de,hl
        inc hl:ld (hl),e
        inc hl:ld (hl),d
        ex de,hl
        ret       

;### CELCRM -> removes a cell control
;### Input      A=control (0-x)
;### Destroyed  AF,BC,DE,HL,IX,IY
celcrm  ld hl,ctrcelnum
        dec (hl)
        cp (hl)
        jr z,celcrm4                ;last control -> no need to move something
        ld l,(hl)
        call celcad                 ;get old records
        push hl
        push de
        ld l,a
        call celcad                 ;get new records
        ex (sp),hl                  ;de=form new, hl=form old
        ld bc,16
        ldir                        ;move last form record to removed form record
        ld hl,-16+4
        add hl,de
        pop de
        ld (hl),e:inc hl
        ld (hl),d
        pop hl                      ;de=control new, hl=control old
        ld bc,6
        ldir                        ;move last control record to removed control record
        ld hl,celrecmem+3
        inc a
        ld ixl,a
        ld bc,(celcntall)
        bc_counter
        ld a,(ctrcelnum)
        inc a
        ld de,celreclen
celcrm2 cp (hl)                     ;search and replace control in celrecmem
        jr z,celcrm3
        add hl,de
        djnz celcrm2
        dec c
        jr nz,celcrm2
        jr celcrm4                  ;not found -> CORRUPT DATA!
celcrm3 ld a,ixl
        ld (hl),a
celcrm4 ld hl,(mrkfldadr)           ;move marker
        ld e,l
        ld d,h
        ld bc,16
        or a
        sbc hl,bc
        ld (mrkfldadr),hl
        ex de,hl
        ldir
        ld hl,mrkfldid
        dec (hl)
        ld hl,supcelgrp+0           ;decrease control count
        dec (hl)
        ret

;### CELREF -> get reference data from cell
;### Input      IX=cell record
;### Output     (forcnvXXX)=filled, HL=cell data behind last reference
;### Destroyed  AF,BC,DE
celref  ld l,(ix+celrecdat+0)
        ld h,(ix+celrecdat+1)
        ld bc,celforref
        add hl,bc
        ld a,(ix+celrecref)
        push af
        and 15
        ld (forcnvrsc),a
        ld de,forcnvrsd
        call nz,celref1
        pop af
        rrca:rrca:rrca:rrca
        and 15
        ld (forcnvrrc),a
        ret z
        ld de,forcnvrrd
celref1 add a:add a
        ld c,a
        ld b,0
        ldir
        ret

;### CELCNV -> converts formula input into cell data
;### Input      txtfrminp/HL=input, (ctrfrminp+8)/C=length (with 0term)
;### Output     celcnvXXX=prepared
celcnvrec   dw 0        ;cell data record or 0

celcnvtyp   db 0        ;type
celcnvtln   db 0,0      ;text length (0-terminator included)
celcnvtxt   ds 252      ;text
celcnvdln   db 0,0      ;data length
celcnvdat   ds 252      ;data
celcnvref   db 0        ;reference count [b0-3]=single, [b4-7]=range

celcnvcol   db 16*1+8   ;##!!## set
celcnvdsp   db 0
celcnvfmt   db 0

celcnv  ld bc,(ctrfrminp+8)
        ld hl,txtfrminp
celcnv0 ld (celcnve+1),hl
        xor a
        ld (celcnvref),a
        ld a,c
        ld (celcnvf+1),a
        or a
        jr nz,celcnv3
        ld (celcnvtyp),a    ;** cell is empty
        ld (celcnvtln),a
        ld (celcnvdln),a
        ret
celcnv3 ld a,(hl)
        cp "="
        jr z,celcnv4
        push hl:pop iy
        ld hl,celcnvdat
        cp "@"
        jr z,celcnv7
        cp "_"
        jr z,celcnv7
        cp "%"
        jr z,celcnvb
        cp "#"
        jr z,celcnvc
        call cnvflt
        jr c,celcnv1
        ld hl,fmtnfl        ;** cell is float

celcnv8 call chrwht
        or a
        jr nz,celcnv1           ;no 0 terminator -> text
        ld a,celtypnum
        ld (celcnvtyp),a
        ld a,5
        ld (celcnvdln),a
celcnv9 ld (celcnv6+1),hl
celcnv6 call fmtnfl             ;get cell dsp/fmt in l,h
        ld (celcnvdsp),hl
        ld de,celcnvdat
        call celbld             ;convert to text
        ld a,c
        ld hl,celbldbuf
        jr celcnv2

celcnvb call cnvbnr         ;** cell is binary
        jr celcnvd
celcnvc call cnvhxr         ;** cell is hexadecimal
        jr celcnvd
celcnv7 call timred         ;** cell is datetime
celcnvd jr nc,celcnv8

celcnv1 ld a,celtyptxt      ;** cell is text
        ld (celcnvtyp),a
        xor a
        ld (celcnvdln),a
celcnvf ld a,0
        inc a
celcnve ld hl,0
celcnv2 ld (celcnvtln),a
        ld de,celcnvtxt
        ld c,a
        ld b,0
        ldir
        ret

celcnv4 inc hl              ;** cell is formula
        push hl:pop iy
        ld ix,celcnvtxt         ;first store formula data in cell text
        call fordat
        jp c,celerr
        ld (celcnva+1),hl
        ld (celcnvdln),a
        ld de,celcnvdat+5       ;copy references to cell data
        ld b,0
        ld hl,(forcnvrsc)
        ld a,l
        push hl
        ld hl,forcnvrsd
        call celcnv5            ;copy single references
        ld hl,forcnvrrd
        pop af:push af
        call celcnv5            ;copy range references
        pop hl
        ld a,h
        add a:add a:add a:add a
        add l
        ld (celcnvref),a        ;store reference counts
        ld a,l                  ;calculate full data length
        add h
        add a:add a
        add 5
        ld hl,celcnvdln
        ld c,(hl)
        add (hl)
        ;...check for data overflow (252)
        ld (hl),a
        ld b,0
        ld hl,celcnvtxt
        push hl
        ldir                    ;copy formula data
        pop de
        ld hl,celcnvdat+0
        call forclc             ;calculate and store formula result
        jp c,celerr
        ld a,celtypfrn
        ld (celcnvtyp),a        ;type number formula
celcnva ld hl,fmtnfl
        jp celcnv9              ;generate text from value

celcnv5 or a
        ret z
        add a:add a
        ld c,a
        ld b,0
        ldir
        ret

;### CELERR -> prepares an error cell
;### Input      A=error code (, celncnvXXX=partially prepared)
;### Output     celncnvXXX=prepared
celerrtab   dw celerrtxt1,celerrtxt2,celerrtxt3,celerrtxt4,celerrtxt5,celerrtxt6,celerrtxt7,celerrtxt8
celerrtxt1  db 8,"#VALUE!",0    ;syntax error in formula

celerrtxt2  db 8,"#DIV/0!",0    ;division by zweo
celerrtxt3  db 7,"#OVFL!",0     ;overflow
celerrtxt4  db 6,"#NUM!",0      ;number (other calculation error)
celerrtxt5  db 6,"#REF!",0      ;reference is not a number
celerrtxt6  db 7,"#STCK!",0     ;stack overflow
celerrtxt7  db 7,"#PARA!",0     ;wrong number of parameters
celerrtxt8  db 8,"#RANGE!",0    ;range can't be used here

celerr  cp errclcbeg
        jr nc,celerr1
        push af                 ;syntax error
        add errforinf0
        call prgalr             ;show error type message
        ld a,(ctrfrminp+8)
        inc a
        ld (celcnvdln),a
        ld c,a
        ld b,0
        ld hl,txtfrminp         ;copy original formula text to data
        ld de,celcnvdat
        ldir
        pop af
celerr1 sub errclcbeg
        ld e,0
        ld c,celtyperf
        jr c,celerr2
        ld c,celtyperv
        inc a
        add a
        ld e,a
celerr2 ld a,c
        ld (celcnvtyp),a
        ld d,0
        ld hl,celerrtab         ;copy error label to text
        add hl,de
        ld e,(hl):inc hl
        ld d,(hl)
        ex de,hl
        ld a,(hl)
        inc hl
        jp celcnv2

;### CELBLD -> builds a cell display text
;### Input      DE=value, L=dsp, H=fmt
;### Output     (celbldbuf)=text, BC=length (with 0term), HL=celbldbuf
;### Destroyed  AF,DE,HL,IX,IY
celblddsp   db 0
celbldfmt   db 0
celbldbuf   ds 40+6         ;**tmpbuf** ???

celbldx ld (celbldbuf+0),bc
        ld (celbldbuf+2),de
        ld (celbldbuf+4),a
        ld de,celbldbuf

celbld  ld (celblddsp),hl
        ld a,h
        and #e0
        cp 32*3
        jr nc,celbld4
        push de
        sub 32*1
        ld de,256*255+0     ;exponent
        jr z,celbld1
        jr nc,celbld2
celbld3 ld a,h              ;decimal
        and 8
        rrca:rrca:rrca
        ld e,a
        ld a,h
        and 7
        cp 7
        ld d,a
        jr nz,celbld1
        ld d,-2
celbld1 ex de,hl
        pop de
        call cnvstr
celbld5 ld hl,(celblddsp)
        call celuni
        ld hl,celbldbuf
        ret
celbld2 pop de              ;percent
        push hl
        ld hl,celbldbuf
        call FLO_MOVE
        ld de,FLO_CONST_100
        call FLO_MULT
        ex (sp),hl
        jr celbld3
celbld4 jr z,celbld6
        cp 32*5
        jr c,celbld8
        jr nz,celbld7
        call cnvbol         ;boolean
        ld hl,celbldbuf
        ret
celbld7 call cnvbin
        jr celbld5
celbld6 ld bc,cfgmskdat     ;date
        jr celbld9
celbld8 ld bc,cfgmsktim     ;time
celbld9 ld a,l
        and 7*8
        add a
        ld l,a:ld h,0
        add hl,bc
        push hl
        call celblda
        pop iy
        ld ix,celbldbuf
        call timwrt
        ld hl,celbldbuf
        ld b,0
        ret
celblda ld hl,celbldbuf
        call FLO_MOVE
celbldb call FLO_FIX_FLO_TO_LW
        ld hl,(celbldbuf+0)
        ld de,(celbldbuf+2)
        bit 7,b
        call nz,clcn32
        jp timget

;### CELUNI -> add unit to cell text
;### Input      (cnvstrtxt)=text, BC=length (with 0term), L=dsp, H=fmt
;### Output     (celbldbuf)=text, BC=length (with 0term)
celuni  ld a,h
        and #e0
        cp 32*2
        ld de,cfguniper
        jr z,celuni2
        ld de,cfguninum
        jr c,celuni1        ;num/exp -> num units
        cp 32*6
        ld de,cfgunibin
        jr nc,celuni1       ;bin/hex -> bin units
        ld hl,cnvstrtxt     ;date/time/boolean -> no unit
        ld de,celbldbuf
        push bc
        ldir
        pop bc
        ret
celuni1 ld a,l
        and #38
        ld l,a
        ld h,0
        add hl,de
        ex de,hl
celuni2 push bc:pop ix
        ex de,hl
        ld a,(hl)           ;hl=uniline
        inc hl
        ld de,celbldbuf
        or a
        call z,celuni3
        push hl
        ld hl,cnvstrtxt
        ldir
        dec de
        pop hl
        dec a
        call z,celuni3
        xor a
        ld (de),a
        push ix:pop bc
        ret
celuni3 inc (hl):dec (hl)
        ret z
        inc bc
        ldi
        inc ix
        jr celuni3


;==============================================================================
;### CELL FORMAT DIALOGUE #####################################################
;==============================================================================

;### CFDOPN -> open cell format window
cfdopn  call fmtcur
        ex de,hl
        ld hl,_cfdopn
        jp guijmp_b

;### CFDOKY -> "ok" clicked
cfdfmttyp   db 0
cfdfmtcol   db 0
cfdfmtdsp   db 0
cfdfmtfmt   db 0

cfdoky  ld (cfdfmttyp),ix
        ld (cfdfmtdsp),de
        ld a,1
        ld (fmtdspf),a
        ld hl,cfdoky1
        call fldmul
        call nc,flddrw          ;redraw, if changed
        jp prgprz0
cfdoky1 ld a,(hl)           ;** reformat one cell
        cp celtyptxt
        ld b,1
        jr c,cfdoky2
        ld b,3
cfdoky2 ld a,(cfdfmttyp)
        cp b
        ret nz                  ;maintype changed -> ignore ##!!##
        push hl
        push bc
        ld de,celreccol
        add hl,de
        ex de,hl
        ld hl,cfdfmtcol
        ldi:ldi:ldi             ;update format
        pop af
        cp celtyptxt
        jr nc,cfdoky3           ;cell was text -> no additional changes
cfdoky4 pop hl
        push hl
        ld de,celrecdat
        add hl,de
        ld e,(hl):inc hl
        ld d,(hl)
        inc de:inc de:inc de
        ld hl,(cfdfmtdsp)
        call celbld             ;regenerate text
        ld de,celbldbuf
        pop hl
        push hl
        call celutx             ;update text
        jr c,cfdoky5
cfdoky3 pop ix
        jp fmtctr
cfdoky5 pop hl              ;** memful exception
        call celrem
        ld a,errmeminfb
        jp fldmul0


;==============================================================================
;### FORMAT HANDLING ##########################################################
;==============================================================================

;### FMTCOL -> open colour dropdown
fmtcol  call fmtcur
        ex de,hl
        ld hl,_fmtcol
        jp guijmp_b

;### FMTCUR -> try to get current cell or first cell in range
;### Output     HL=cell data record (0=no cell, DE=default colours)
fmtcur  call mrkact         ;multiple cells?
        jr z,fmtcur4        ;no -> try current cell
        db #3e:scf
        ld (fmtcur1),a
        xor a
        ld hl,fmtcur3
        ld (hl),a
        call fldmul         ;find first cell in multiple
fmtcur1 or a
        jr c,fmtcur4        ;no cell in multiple -> use default
fmtcur2 ld hl,0
        ret
fmtcur3 db 0                ;cell found -> use this
        ld a,#c9
        ld (fmtcur3),a
        db #3e:or a
        ld (fmtcur1),a
        ld (fmtcur2+1),hl
        ret
fmtcur4 ld hl,(fldcurp)
        jp fldcel           ;current cell?

;### FMTALF/RG/CN -> set/reset alignment
fmtadf  xor a
        jr fmtaln
fmtalf  ld a,1
        jr fmtaln
fmtacn  ld a,2
        jr fmtaln
fmtarg  ld a,3
fmtaln  push af
        call fmtdsp4
        pop af
        bit 1,a
        jr z,fmtaln4
        xor 1               ;back swap right/centered
fmtaln4 rrca:rrca
        and #c0
        ld c,#3f                ;c=cell mask

fmtaln7 ld hl,celrecdsp
;a=new value,c=mask,hl=record offset -> manipulate cells
fmtaln0 call fmtaln1
fmtaln9 jp prgprz0
;d=new value,e=mask -> manipulate cells (celreccol)
fmtaln8 inc e:dec e
        jr z,fmtaln9
        ld hl,celreccol
        ld a,d
        ld c,e
        jr fmtaln0

fmtaln1 ld (fmtaln6+1),hl
        ld (fmtaln3+1),a
        ld a,c
        ld (fmtaln3-1),a
        ld hl,fmtaln2
fmtaln5 call fldmul
        call nc,flddrw          ;redraw, if changed
        ret
fmtaln2 push hl:pop ix
fmtaln6 ld de,celrecdsp
        add hl,de
        ld a,(hl)
        and 0
fmtaln3 or 0
        ld (hl),a
        jp fmtctr

;### FMTFBL/IT/DF -> set font
fmtfdf  xor a
        jr fmtfnt
fmtfbl  ld a,1
        jr fmtfnt
fmtfit  ld a,2
fmtfnt  push af
        call fmtdsp5
        pop af
        ld c,#fc
        jr fmtaln7

;### FMTDSP -> displays format of current cell(s)
;### Input      (fldcurp)=cell
;### Destroyed  AF,BC,DE,HL,IX,IY
fmtdspf db 0        ;trigger flag
fmtdspc db 0,0      ;current format (font [0=normal, 1=bold, 2=italic], alignment [0=normal, 1=left, 2=center, 3=right])

fmtdsp  call fmtdsp0
        jp prgprz0

fmtdsp0 xor a
        ld (fmtdspf),a
        ld a,(winmaidat+1)
        and 8
        ret z
        call mrkact
        ld hl,0
        jr nz,fmtdsp1   ;range -> just deactivate buttons
fmtdsp6 ld hl,(fldcurp)
        call fldcel
        jr c,fmtdsp1
        ld de,celrecdsp
        add hl,de
        ld h,(hl)
        ld a,h
        and #03
        ld l,a          ;l=font
        ld a,h
        rlca:rlca
        and #03
        ld h,a          ;h=alignment

fmtdsp1 push hl
        ld a,l
        call fmtdsp5
        pop af
        bit 1,a
        jr z,fmtdsp4
        xor 1           ;swap right/centered
fmtdsp4 ld hl,fmtdspc+1
        ld de,256*wintolrec_algn+106-16
        ld ix,wintolrec2
fmtdsp2 cp (hl)
        ret z
        ld (hl),a
        ld bc,-16
        or a
        jr z,fmtdsp3
        add a:add a:add a:add a
        add e
        ld c,a
        ld b,0
fmtdsp3 ld (ix+6),c
        ld (ix+7),b
        ld a,(cfgviwtol)
        or a
        ret z
        ld a,(winmainum)
        ld e,-4
        jp SyDesktop_WINTOL
fmtdsp5 ld hl,fmtdspc+0
        ld de,256*wintolrec_font+70-16
        ld ix,wintolrec1
        jr fmtdsp2

;### FMTNxx -> returns cell or default formatting for number types
;### Input      (celcnvrec)=cell record or zero
;### Output     L=dsp, H=fmt, IX=(celcnvrec)
;### Destroyed  AF
fmtnfl  call fmtnum:ret nz:ld hl,(cfgfmtflt):ret   ;float
fmtndt  call fmtnum:ret nz:ld hl,(cfgfmtdat):ret   ;date
fmtntm  call fmtnum:ret nz:ld hl,(cfgfmttim):ret   ;time
fmtnbn  call fmtnum:ret nz:ld hl,(cfgfmtbin):ret   ;binary
fmtnhx  call fmtnum:ret nz:ld hl,(cfgfmthex):ret   ;hexadecimal
fmtnum  ld ix,(celcnvrec)
        ld a,ixl:or ixh
        ret z                   ;cell wasn't existing -> use default for new number type
        ld a,(ix+celrectyp)
        cp celtyptxt
        jr nc,fmtnum1           ;cell was text before -> use default for new number type
        ld l,(ix+celrecdsp)
        ld h,(ix+celrecfmt)
        xor a:inc a
        ret
fmtnum1 xor a
        ret

;### FMTDFT -> set default format for cell and control, if new or type changed
;### Input      HL=cell record, CF=0 -> new cell, CF=1 -> cell type changed, don't alter pen/paper/font, alignment if was>0
;### Destroyed  AF,BC,DE,IX
fmtdft  push af
        ld a,(hl)
        push hl:pop ix
        ld de,(cfgfmtnum)           ;e=pen, d=dsp
        ld bc,(cfgfmtflt+1)         ;c=fmt
        cp celtypnum:jr z,fmtdft1
        cp celtypfrn:jr z,fmtdft1
        cp celtyperv:jr z,fmtdft1
        cp celtyperf:jr z,fmtdft1
        ld de,(cfgfmttxt)
        ld c,0
        cp celtyptxt
        jr z,fmtdft1
        ;...
        pop af
        ret
fmtdft1 pop af
        jr nc,fmtdft2
        ld a,(ix+celrecdsp)
        ld b,a
        and #c0
        jr nz,fmtdft4
        ld a,d
        and #c0
        res 6,b
        res 7,b
        or b
        ld b,a
fmtdft4 ld d,b
        jr fmtdft3
fmtdft2 ld (ix+celreccol),e
fmtdft3 ld (ix+celrecfmt),c
        ld (ix+celrecdsp),d
;### FMTCTR -> convert cell format to control
;### Input      IX=cell record
;### Destroyed  AF,C,DE
fmtctr  ld a,1
        ld (docmodf),a
        ld a,(ix+celrecctr)
        or a
        ret z
fmtctr6 push hl
        ld l,a
        dec l
        call celcad
        call fmtctr0
        pop hl
        ret
;hl=control data, de=form record, ix=cell
fmtctr0 inc hl:inc hl
        ld a,(ix+celreccol)
        ld (hl),a
        inc hl
        and #0f
        ld c,a
        ld a,(cfgcolpap)
        cp c
        ld c,128            ;16 colour mode
        jr z,fmtctr1
        set 6,c             ;fill background, if paper not default
fmtctr1 ld a,(ix+celrecdsp)
        ld b,a
        rlca:rlca
        and #3
        sub 1
        jr nc,fmtctr2
        ld a,(ix+celrectyp)
        and 63
        cp celtyperf
        ccf
        jr c,fmtctr5
        cp celtyptxt
fmtctr5 ld a,0
        adc a
fmtctr2 or c
        ld (hl),a
        ld a,b              ;font
        and 3
        ld c,1
        jr z,fmtctr4
        dec a
        ld bc,fnt_bold
        jr z,fmtctr3
        ld bc,fnt_italic
fmtctr3 inc hl
        ld (hl),c
        inc hl
        ld (hl),b
        ld c,5
fmtctr4 ex de,hl
        inc hl:inc hl
        ld (hl),c
        ret


;==============================================================================
;### TIME CONVERSION ##########################################################
;==============================================================================

;### TIMEDT -> converts datetime to edit string
;### Input      DE=5byte FP number
;### Output     (cnvstrtxt)=string, BC=string length (2-20, including 0terminator)
;### Destroyed  AF,DE,HL,IX,IY
timedtt db "_H:m:s",0
timedtd db "@Y.M.D",0
timedtf db "@Y.M.D_H:m:s",0

timedt  call celblda        ;A=second (0-59), B=minute (0-59), C=hour (0-23), D=day (1-31), E=month (1-12), HL=year (date_year_min-date_year_max)
        push bc
        ld bc,date_year_min
        or a
        sbc hl,bc
        add hl,bc
        pop bc
        jr nz,timedt1
        push bc
        ld bc,#0101
        ex de,hl
        sbc hl,bc
        add hl,bc
        ex de,hl
        pop bc
        ld iy,timedtt
        jr z,timedt2
timedt1 ld iyl,a
        or c:or b
        ld a,iyl
        ld iy,timedtd
        jr z,timedt2
        ld iy,timedtf
timedt2 ld ix,cnvstrtxt
        call timwrt
        ld b,0
        ret

;### TIMRED -> reads datetime from string
;### Input      IY=string, HL=5byte FP destination
;### Output     CF=0 -> ok, IY=next, (IY+0)=terminator, HL=default format function
;###            CF=1 -> no datetime
;### Destroyed  AF,BC,DE,HL,IX
timredtdi   db 1,"."
timredtde   db timredtde0-timredtde-1:db "+-*/\^);<>=&|_ ",0:timredtde0
timredtti   db 1,":"
timredtte   db timredtte0-timredtte-1:db "+-*/\^);<>=&|: ",0:timredtte0

timredi dw date_year_min:db 1,1
        db 0,0,0

timredd dw 0000:db 0,0  ;year, day, month
timredt db 0,0,0        ;hour, minute, second

timred  call timred0
        ld hl,timredi
        ld de,timredd
        ld bc,7
        ldir
        ld a,(iy+0)
        inc iy
        cp "@"
        jr z,timred2
        ld hl,fmtntm
        ld (timred9+1),hl   ;is time
        cp "_"
        jr z,timred3
        scf
        ret
timred2 ld hl,fmtndt        ;is date
        ld (timred9+1),hl
        ld hl,timredtdi:ld bc,date_year_min:ld de,date_year_max:call cnvs16:ret c:       ld (timredd+0),hl  ;year
        ld hl,timredtdi:ld bc,            1:ld de,           12:call cnvs16:ret c:ld a,l:ld (timredd+2),a   ;month
        ld hl,timredtde:ld bc,            1:ld de,           31:call cnvs16:ret c:ld a,l:ld (timredd+3),a   ;day
        ld a,(iy-1)
        cp "_"
        jr nz,timred4
timred3 ld hl,timredtti:ld bc,  00:ld de,  23:call cnvs16:ret c:ld a,l:ld (timredt+0),a     ;hour
        ld hl,timredtte:ld bc,  00:ld de,  59:call cnvs16:ret c:ld a,l:ld (timredt+1),a     ;minute
        ld a,(iy-1)
        cp ":"
        jr nz,timred4
        ld hl,timredtte:ld bc,  00:ld de,  59:call cnvs16:ret c:ld a,l:jr timred5           ;second
timred4 ld a,(timredt+2)
timred5 ld bc,(timredt+0)
        ld de,(timredd+2)
        ld hl,(timredd+0)
        call timput
timred1 xor a
        bit 7,d
        call nz,clcn32
timred6 ld (0),hl
timred7 ld (0),de
timred8 ld hl,0
        call FLO_KONV_LW_TO_FLO
        dec iy
        or a
timred9 ld hl,0
        ret
timred0 ld (timred8+1),hl
        ld (timred6+1),hl
        inc hl:inc hl
        ld (timred7+2),hl
        ret

;### TIMWRT -> writes datetime as string with mask
;### Input      A=second (0-59), B=minute (0-59), C=hour (0-23), D=day (1-31), E=month (1-12), HL=year (date_year_min-date_year_max)
;###            IY=mask, IX=string
;### Mask       s=second,m=minute,H=hour,h=hour12,a="AM"/"PM",D=day,M=month,Y=year,y=year 2digits,w=weekday 3chars,W=weekday full,n=month 3chars,N=month full
;### Output     C=string length (0-terminated)
;### Destroyed  AF,BC,DE,HL,IX,IY
timwrtt
db "s":dw timwse
db "m":dw timwmi
db "H":dw timwh2
db "h":dw timwh1
db "D":dw timwdy
db "M":dw timwmo
db "Y":dw timwy4
db "y":dw timwy2
db "n":dw timwm3
db "N":dw timwmf
db "w":dw timww3
db "W":dw timwwf
db "a":dw timwxm
db "\":dw timwes
timwrtt0

timwrtmsh   dw timwrtmsh01,timwrtmsh02,timwrtmsh03,timwrtmsh04,timwrtmsh05,timwrtmsh06,timwrtmsh07,timwrtmsh08,timwrtmsh09,timwrtmsh10,timwrtmsh11,timwrtmsh12
timwrtmln   dw timwrtmln01,timwrtmln02,timwrtmln03,timwrtmln04,timwrtmln05,timwrtmln06,timwrtmln07,timwrtmln08,timwrtmln09,timwrtmln10,timwrtmln11,timwrtmln12

timwrtmsh01 db "Jan",0
timwrtmsh02 db "Feb",0
timwrtmsh03 db "Mar",0
timwrtmsh04 db "Apr",0
timwrtmsh05 db "May",0
timwrtmsh06 db "Jun",0
timwrtmsh07 db "Jul",0
timwrtmsh08 db "Aug",0
timwrtmsh09 db "Sep",0
timwrtmsh10 db "Oct",0
timwrtmsh11 db "Nov",0
timwrtmsh12 db "Dec",0

timwrtmln01 db "January",0
timwrtmln02 db "February",0
timwrtmln03 db "March",0
timwrtmln04 db "April",0
timwrtmln05 db "May",0
timwrtmln06 db "June",0
timwrtmln07 db "July",0
timwrtmln08 db "August",0
timwrtmln09 db "Septemper",0
timwrtmln10 db "October",0
timwrtmln11 db "November",0
timwrtmln12 db "December",0

timwrtdsh   dw timwrtdsh01,timwrtdsh02,timwrtdsh03,timwrtdsh04,timwrtdsh05,timwrtdsh06,timwrtdsh07
timwrtdln   dw timwrtdln01,timwrtdln02,timwrtdln03,timwrtdln04,timwrtdln05,timwrtdln06,timwrtdln07

timwrtdsh01 db "Mon",0
timwrtdsh02 db "Tue",0
timwrtdsh03 db "Wed",0
timwrtdsh04 db "Thu",0
timwrtdsh05 db "Fri",0
timwrtdsh06 db "Sat",0
timwrtdsh07 db "Sun",0

timwrtdln01 db "Monday",0
timwrtdln02 db "Tuesday",0
timwrtdln03 db "Wednesday",0
timwrtdln04 db "Thursday",0
timwrtdln05 db "Friday",0
timwrtdln06 db "Saturday",0
timwrtdln07 db "Sunday",0

timwrtd db 0    ;+0=second
        db 0,0  ;+1=hour,+2=minute
        db 0,0  ;+3=month,+4=day
        dw 0    ;+5=year

timwrt  ld (timwrtd+0),a
        ld (timwrtd+1),bc
        ld (timwrtd+3),de
        ld (timwrtd+5),hl
        ld c,0
timwrt1 ld a,(iy+0):inc iy
        ld b,timwrtt0-timwrtt/3
        ld hl,timwrtt
        ld de,3
timwrt2 cp (hl)
        jr z,timwrt3
        add hl,de
        djnz timwrt2
timwrt7 ld (ix+0),a:inc ix:inc c
        or a
        jr nz,timwrt1
        ret
timwrt3 inc hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        jp (hl)

timwrt4 call cnvdec                     ;a=value -> write 2dec digits
        ld (ix+0),l:inc ix:inc c
        ld (ix+0),h:inc ix:inc c
        jr timwrt1

timwse  ld a,(timwrtd+0):jr timwrt4                     ;** "s" second
timwmi  ld a,(timwrtd+2):jr timwrt4                     ;** "m" minute
timwh2  ld a,(timwrtd+1):jr timwrt4                     ;** "H" hour 24
timwh1  ld a,(timwrtd+1)                                ;** "h" hour 12
        sub 12
        jr z,timwh11
        jr nc,timwrt4
timwh11 add 12
        jr nz,timwrt4
        ld a,12
        jr timwrt4
timwdy  ld a,(timwrtd+4):jr timwrt4                     ;** "D" day
timwmo  ld a,(timwrtd+3):jr timwrt4                     ;** "M" month
timwy2  call timwy22                                    ;** "y" year 2digits
timwy21 sub 2000-date_year_min
        jr nc,timwrt4
        add 100
        jr timwrt4
timwy22 ld hl,(timwrtd+5)
        ld de,-date_year_min
        add hl,de
        ld a,l
        ret
timwy4  call timwy22                                    ;** "Y" year 4digits
        ld de,"19"
        cp 2000-date_year_min
        jr c,timwy41
        ld de,"20"
timwy41 call timwy42
        jr timwy21
timwy42 ld (ix+0),d:inc ix:inc c
        ld (ix+0),e:inc ix:inc c
        ret
timww3  ld hl,timwrtdsh                                 ;** "w" weekday short
        jr timwwf1
timwwf  ld hl,timwrtdln                                 ;** "W" weekday long
timwwf1 push hl
        ld de,(timwrtd+3)
        ld hl,(timwrtd+5)
        call timwkd
        pop hl
        jr timwrt5
timwm3  ld a,(timwrtd+3):ld hl,timwrtmsh-2:jr timwrt5   ;** "n" month short
timwmf  ld a,(timwrtd+3):ld hl,timwrtmln-2              ;** "N" month long
timwrt5 add a
        ld e,a:ld d,0
        add hl,de
        ld e,(hl):inc hl
        ld d,(hl)
timwrt6 ld a,(de)
        or a
        jp z,timwrt1
        ld (ix+0),a:inc ix:inc c
        inc de
        jr timwrt6
timwxm  ld a,(timwrtd+1)                                ;** "a" AM/PM
        cp 13
        ld de,"AM"
        jr c,timwxm1
        ld de,"PM"
timwxm1 call timwy42
        jp timwrt1
timwes  ld a,(iy+0)                                     ;** "\" Escape
        inc iy
        jp timwrt7

;### TIMPUT -> converts time to timestamp value
;### Input      A=second (0-59), B=minute (0-59), C=hour (0-23), D=day (1-31), E=month (1-12), HL=year (date_year_min-date_year_max)
;### Output     CF=0 -> DE,HL=timestamp value (seconds since date_year_min)
;###            CF=1 -> invalid date (<date_year_min or >date_year_max)
;### Destroyed  AF,BC,IX
;### Source     [24-07-03] https//de.wikipedia.org/wiki/Unixzeit#Beispiel-Implementierung

timputm dw 0,31,59,90,120,151,181,212,243,273,304,334

timput         cp 60:ccf:ret c:ld ixl,a     ;** time/day/month validation
        ld a,b:cp 60:ccf:ret c
        ld a,c:cp 24:ccf:ret c
        ld a,d:sub 1:    ret c
               cp 31:ccf:ret c
        ld a,e:sub 1:    ret c
               cp 12:ccf:ret c:ld a,ixl
timput0 push af         ;no time/day/month validation
        push bc
        push de
        ld a,e
        add a
        ld (timput2+0+2),a
        inc a
        ld (timput2+3+2),a
        ld de,-date_year_min
        add hl,de       ;hl=year-date_year_min
        jr nc,timput6
        ld a,l
        cp date_year_max+1-date_year_min    ;** year validation
        jr nc,timput6
        ld (timput3+1),a
        or a
        jr z,timput1
        dec a
        srl a:srl a
        inc a
timput1 push af         ;a=leap years since date_year_min (without current year)
        ld a,l
        ld de,365
        call clcm86     ;hl=year*365=days since date_year_min
        pop af
        ld c,a
        ld b,0
        add hl,bc
        ld ix,timputm-2
timput2 ld c,(ix+0)
        ld b,(ix+0)
        add hl,bc       ;hl+=days until month
        pop bc          ;c=month, b=day
        ld a,c
        ld c,b
        ld b,0
        dec c
        add hl,bc       ;hl+=day
        cp 3
        jr c,timput4
timput3 ld a,0
        and 3
        jr nz,timput4
        inc hl          ;hl+=leap day
timput4 ex de,hl
        ld a,24
        call clcm86     ;ahl*=24
        pop bc          ;c=hour, b=minute
        push bc
        ld b,0
        add hl,bc
        adc b           ;ahl=days*24+hour
        ld e,a:ld d,0
        call clc260     ;dehl=dehl*60=minutes since date_year_min
        pop bc          ;b=minute
        call timput5    ;dehl=minutes since date_year_min+minute
        call clc260     ;dehl=seconds since date_year_min
        pop bc          ;b=second
timput5 ld c,b
        ld b,0
        add hl,bc
        ret nc
        inc de
        or a
        ret
timput6 pop hl
        pop hl
        pop hl
        scf
        ret

;### TIMGET -> reads time from timestamp value ##!!## NOT OPTIMIZED
;### Input      DE,HL=timestamp value (seconds since date_year_min)
;### Output     A=second, B=minute, C=hour, D=day (starting from 1), E=month (starting from 1), HL=year
;### Destroyed  F,IX,IY

;seconds  = timestamp % seconds_per_day
;day      = timestamp / seconds_per_day + 1976_03_01
;temp     = 4*(day + 365 + 1)/days_per_4years-1
;year     = temp + 1976
;day      = day - 365 * temp - temp / 4
;month    = (5 * day + 2) / 153
;day      = day - (month * 153 + 2) / 5 + 1
;month    = month + 3
;month>12 -> month -= 12; year += 1
;hour/minute/second...

timgetyer   dw #3380,#01e1      ;normal year
timgetyel   dw #8500,#01e2      ;leap year
timgetmo8   dw #ea00,#0024      ;month 28 days
timgetmo9   dw #3b80,#0026      ;month 29 days
timgetmo0   dw #8d00,#0027      ;month 30 days
timgetmo1   dw #de80,#0028      ;month 31 days
timgetday   dw #5180,#0001      ;day
timgethor   dw #0e10,#0000      ;hour
timgetmin   dw #003c,#0000      ;minute

timgetmon   dw timgetmo1,timgetmo8,timgetmo1,timgetmo0,timgetmo1,timgetmo0,timgetmo1,timgetmo1,timgetmo0,timgetmo1,timgetmo0,timgetmo1

timget  xor a                   ;** calculate year starting from date_year_min
        ld iyl,a
timget2 ld ix,timgetyer
        jr nz,timget3
        ld ix,timgetyel         ;take leap years into account (year has 365 or 366 days)
timget3 push de
        push hl
        call clcs32
        jr c,timget4
        pop bc
        pop bc
        inc iyl                 ;iyl=year-date_year_min
        inc a
        and 3
        jr timget2
timget4 or a                    ;** calculate month
        ld hl,timgetmo8
        jr nz,timget5
        ld hl,timgetmo9         ;take leap year into account (february has 28 or 29 days)
timget5 ld (timgetmon+2),hl
        pop hl
        pop de
        push iy
        ld iy,timgetmon
        xor a
timget6 inc a                   ;a=month (1-12)
        ld c,(iy+0)
        ld b,(iy+1)
        push bc:pop ix
        push de
        push hl
        call clcs32
        jr c,timget7
        pop bc
        pop bc
        inc iy
        inc iy
        jr timget6
timget7 pop hl
        pop de
        pop bc
        ld b,a                  ;c=year-date_year_min, b=month (1-12)
        push bc
        ld iy,3                 ;** calculate day, hour, minute
        ld ix,timgetday
timget8 xor a
timget9 push de
        push hl
        call clcs32
        jr c,timgeta
        pop bc
        pop bc
        inc a
        jr timget9
timgeta pop hl
        pop de
        ld bc,4
        add ix,bc
        push af
        dec iyl
        jr nz,timget8
        ld a,l          ;a=second
        pop de
        ld b,d          ;b=minute
        pop de
        ld c,d          ;c=hour
        pop de
        inc d           ;d=day
        pop hl
        ld e,h          ;e=month
        push de
        ld h,0
        ld de,date_year_min
        add hl,de       ;hl=year
        pop de
        ret

;### TIMWKD -> weekday calculation
;### Input      D=day, E=month, HL=year
;### Output     A=weekday (0-6; mon-sun)
;### Destroyed  F,BC,DE,HL
;### Source     [24-07-03] https//de.wikipedia.org/wiki/Wochentagsberechnung#Formel
timwkdm db 02,05,07,10,12,15,17,20,23,25,28,30

timwkd  ld a,e
        sub 3
        jr nc,timwkd1
        dec hl
        add 12
timwkd1 ld e,a              ;e=month (0-11 = mar-feb)
        ld bc,-date_year_min
        add hl,bc
        ld a,l
        sub 2000-date_year_min
        ld h,20
        jr nc,timwkd2
        add 100
        ld h,19             ;h=year digits 1-2=c
timwkd2 ld l,a              ;l=year digits 3-4=y
        srl a:srl a         ;=int(y/4)
        add l               ;+y
        sub h:sub h         ;-2c
        srl h:srl h
        add h               ;+int(c/4)
        add d               ;+d
        ld hl,timwkdm
        ld d,0
        add hl,de
        add (hl)            ;+int(2,6m-0,2)
        cp -35
        jr nc,timwkd5       ;mod 7 (pos or neg)
timwkd3 sub 7
        jr nc,timwkd3
        add 7
timwkd4 sub 1               ;convert 0-sunday to 0-monday (0=mon, 6=sun)
        ret nc
        ld a,6
        ret
timwkd5 add 7
        jr c,timwkd4
        jr timwkd5

;### CLCM60 -> multiply 32bit value*60
;### Input      DEHL=value
;### Output     DEHL=value*60
;### Destroyed  F,BC
clc260  ld c,l:ld b,h           ;bc=low
        push de
        add hl,hl:rl e:rl d
        add hl,hl:rl e:rl d
        add hl,hl:rl e:rl d
        add hl,hl:rl e:rl d     ;dehl=value*16
        or a
        sbc hl,bc
        pop bc
        ex de,hl
        sbc hl,bc
        ex de,hl                ;dehl=value*16-value=value*15
        add hl,hl:rl e:rl d
        add hl,hl:rl e:rl d     ;dehl=value*15*4=value*60
        ret


;==============================================================================
;### NUMBER CONVERSION ########################################################
;==============================================================================

;### CNVSTR -> converts FP number to string
;### Input      DE=5byte FP number, H=comma (-1=exp, -2=all), L=flag, if 1000er dots
;### Output     (cnvstrtxt)=string, BC=string length (2-27, including 0terminator)
;### Destroyed  AF,DE,L,IX,IY
cnvstrdig   equ 10
cnvstrman   ds cnvstrdig+1              ;mantissa string
cnvstrmal   dw 0                        ;mantissa length
cnvstrtxtp  db 0
cnvstrtxt   ds 40:cnvstrtxt0
cnvstrdot   db 0
cnvstrcom   db 0

cnvstr  ld (cnvstrdot),hl
        push hl
        ld hl,cnvfltb
        call FLO_MOVE
        call FLO_PREPARE        ;(HL)=LW normed mantissa, B=sign of mantissa (0 -> FLO=0), E=exponent/comma position (decimal)
        ld d,b
        inc b:dec b
        jr nz,cnvstrs
        ld hl,"0"
        ld (cnvstrman),hl
        ld l,1
        jr cnvstr3
cnvstrs push de
        ld ix,(cnvfltb+0)
        ld de,(cnvfltb+2)
        ld iy,cnvstrman
        call cnv32s             ;cnvstrman=mantissa as string
        push iy:pop hl
        ld bc,cnvstrman-1
        or a
        sbc hl,bc               ;HL=manlen
        pop de                  ;D=mansgn, E=exp
cnvstr3 dec l                       ;** remove end-zeroes and correct exponent
        jr z,cnvstrq
        ld a,(iy+0)
        sub "0"
        jr nz,cnvstrq
        ld (iy+0),a
        dec iy
        inc e
        jr cnvstr3
cnvstrq inc l
        ld (cnvstrmal),hl
        pop af                  ;A=comma
        ld hl,cnvstrtxt
        push hl
        bit 7,d
        jr z,cnvstr1
        ld (hl),"-"             ;negative mantissa
        inc hl
cnvstr1 inc a
        jr nz,cnvstr7

cnvstr6 push de                     ;** exponent display
        ex de,hl
        ld hl,cnvstrman  
        ldi
        ld a,(cfgnumcom)
        ld (de),a
        inc de
cnvstr2 ld a,(hl)
        ldi
        or a
        jr nz,cnvstr2
        dec de
        ex de,hl
        ld (hl),"e"
        inc hl
        pop de
        ld a,(cnvstrmal)        ;exp display = exp + mantissa length - 1
        add e
        dec a
        ld (hl),"+"
        jp p,cnvstr4
        ld (hl),"-"
        neg
cnvstr4 inc hl
        call cnv08s
cnvstr5 xor a
        ld (hl),a
        inc hl
        pop de
        sbc hl,de
        ld c,l
        ld b,h
        ret
cnvstrp ex de,hl
        jr cnvstr5

cnvstr7 ld a,(cnvstrmal)            ;** try comma display
        bit 7,e
        jr nz,cnvstrd
        add e                       ;** exp is positive
        cp cnvstrdig+1          ;a=length of complete number
        jr nc,cnvstr6           ;exp too long -> exp display

        ex de,hl
        ld hl,cnvstrman
        call cnvstrj                ;** display mantissa without comma
        ex de,hl
        ld a,(cnvstrcom)        ;check if forced comma
        or a
        jr z,cnvstr5
        cp -2
        jr z,cnvstr5
        ld b,a                  ;yes, add comma and number of "0"
        inc b
        ld a,(cfgnumcom)
cnvstrc ld (hl),a
        inc hl
        ld a,"0"
        djnz cnvstrc
        jr cnvstr5

cnvstrd ld c,a                      ;** exp is negative
        add e
        jr c,cnvstri
        neg                     ;* comma outside mantissa
        ld b,a
        ld a,(cnvstrcom)
        or a
        jr z,cnvstre
        ld a,b
        add c
        cp cnvstrdig+1
        jr nc,cnvstr6           ;too large -> switch to exp display
cnvstre ex de,hl
        ld hl,cnvstrman
        call cnvstrf
        jr z,cnvstrp
        ld a,"0"
cnvstrg ld (de),a
        inc de
        call cnvstrh
        jr z,cnvstrp
        djnz cnvstrg
        jr cnvstrl
cnvstri ex de,hl                ;* comma inside mantissa
        ld hl,cnvstrman
        jr z,cnvstrk
        call cnvstrj            ;display mantissa part before comma
        ld a,(cnvstrcom)
        or a
        jr z,cnvstrp
        call cnvstrn
        jr cnvstrl
cnvstrk call cnvstrf
        jr z,cnvstrp
cnvstrl ldi                     ;display mantissa after comma
        call cnvstrh
        jr z,cnvstrp
        ld a,(hl)
        jr nc,cnvstrm
        or a
        jr nz,cnvstrl
        jr cnvstrp
cnvstrm or a
        jr nz,cnvstrl
        dec hl
        ld (hl),"0"
        jr cnvstrl

cnvstrj ld b,a                      ;** display mantissa until comma
cnvstr8 sub 3
        jr z,cnvstr9
        jr nc,cnvstr8
cnvstr9 add 3
        ld c,a                  ;c=1000 counter
cnvstra ld a,(hl)
        inc hl
        or a
        jr nz,cnvstrb
        ld a,"0"                ;end reached, fill with "0"
        dec hl
cnvstrb call cnvdig             ;add digit
        jr nz,cnvstra
        ret

cnvstrf ld a,"0"                ;start with "0," -> zf=1 -> end reached
        ld (de),a
        inc de
        ld a,(cnvstrcom)
        or a
        ret z
cnvstrn ld a,(cfgnumcom)
        ld (de),a
        inc de
        ret

cnvstrh push hl                 ;check if max comma reached -> zf=1 -> yes, zf=0 -> no, cf=1 never
        ld hl,cnvstrcom
        bit 7,(hl)
        scf
        jr nz,cnvstro
        or a
        dec (hl)
cnvstro pop hl
        ret

;### CNVDIG -> adds a digit and inserts a 1000er point, if needed
;### Input      DE=string, A=char, C=point counter, B=length counter
cnvdig  ld (de),a
        inc de
        dec b
        ret z
        dec c
        ret nz
        ld a,(cnvstrdot)
        dec a
        ret nz
        ld a,(cfgnumpoi)
        ld (de),a
        inc de
        ld c,3+1
        dec c
        ret

;### CNVFLT -> converts string to FP number
;### Input      IY=string, HL=5byte FP destination
;### Output     CF=0 -> ok, IY=next, (IY+0)=terminator
;###            CF=1 -> error (A=type -> 1=overflow, 2=second comma, 3=wrong char/terminator, 4=no number)
;### Destroyed  AF,BC,DE,HL,IX,IY
cnvfltb ds 5

cnvflt  ld (cnvflt1+1),hl
        call cnvnum             ;hl,ix=num, c=exp, a[7]=sgn, b=terminator type
        ret c
cnvflt0 ld (cnvfltb+0),ix
        ld (cnvfltb+2),hl
        dec b
        jr nz,cnvflt3
        dec c
        dec c
        inc iy
cnvflt3 ld e,a
        ld a,ixl:or ixh         ;##!!## workaround for 0, compare with pocket calculator
        or l:or h
        ld (cnvfltb+4),a
        ld a,e
        jr z,cnvflt2

        push iy
        push bc
        ld hl,cnvfltb
        call FLO_KONV_LW_TO_FLO
        pop bc
        ld a,c
        or a
        scf
        push bc
        call nz,FLO_10A
        pop bc
        pop iy
        ccf
        jp c,cnvnum6
cnvflt2 ld de,cnvfltb
cnvflt1 ld hl,0
        call FLO_MOVE
        or a
        ret

;### CNVNUM -> converts string into number, exponent, sign
;### Input      IY=string
;### Output     CF=0 -> ok, HL,IX=number, C=exponent, A=sign (0=pos, -1=neg), B=terminator type, IY=next
;###            CF=1 -> error (A=type -> 1=overflow, 2=second comma, 3=wrong char/terminator, 4=no number; HL,IX,IY destroyed)
;### Destroyed  AF,BC,DE
cnvnumexp   db 0    ;comma position in number+1
cnvnumsgn   db 0    ;0=pos, -1=neg
cnvnumlen   db 0

cnvnumtrm   db "+-*/\^|&!);<>= ",0,"%":cnvnumtrm0

cnvnum  xor a               ;reset values
        ld (cnvnumexp),a
        ld (cnvnumsgn),a
        ld (cnvnumlen),a
        ld a,(cfgnumcom)
        ld (cnvnum7+1),a
        call chrwht
        cp "+"                  ;** sign
        jr z,cnvnum1
        cp "-"
        jr nz,cnvnum3
        ld hl,cnvnumsgn
        dec (hl)
cnvnum1 inc iy
cnvnum3 call chrnum
        ld a,4
        ret c               ;first digit (after whitespace/+/-) no number
        ld ix,0
        ld hl,0
cnvnum4 ld a,(iy+0)
        cp "0"
        jr c,cnvnum7
        cp "9"+1
        jr nc,cnvnum7
        ld c,ixl:ld b,ixh       ;** digit
        ld e,l:ld d,h
        add ix,ix:adc hl,hl:jr c,cnvnum6    ;*2
        add ix,ix:adc hl,hl:jr c,cnvnum6    ;*4
        add ix,bc:adc hl,de:jr c,cnvnum6    ;*5
        add ix,ix:adc hl,hl:jr c,cnvnum6    ;*10
        sub "0"
        ld c,a
        ld b,0
        add ix,bc
        ld c,b
        adc hl,bc           ;HL,IX+=digit
        jr c,cnvnum6
        ld a,(cnvnumlen)
        inc a
        ld (cnvnumlen),a
cnvnum5 inc iy
        jr cnvnum4
cnvnum6 ld a,1              ;error -> overflow
        ret
cnvnum7 cp ","
        jr nz,cnvnum9
        ld a,(cnvnumexp)        ;** comma
        or a
        jr nz,cnvnum8
        ld a,(cnvnumlen)
        inc a
        ld (cnvnumexp),a
        jr cnvnum5
cnvnum8 ld a,2
        scf
        ret
cnvnum9 cp "e"
        jr z,cnvnumd
        cp "E"
        jr z,cnvnumd
        call cnvnumt            ;** terminator
        ret c
        call cnvnumc            ;** finished without additional exponent
        jr c,cnvnumj
cnvnumh ld a,(cnvnumsgn)
        or a
        ret
cnvnumj ld a,4
        ret
cnvnumd inc iy                  ;** exponent
        ld a,(iy+0)
        ld c,0
        ld e,c              ;e=value
        ld d,c              ;d=counter
        cp "+"
        jr z,cnvnumk
        cp "-"
        jr nz,cnvnume
        dec c               ;c=expsgn (0=pos, -1=neg)
cnvnumk inc iy
cnvnume ld a,(iy+0)
        cp "0"
        jr c,cnvnumf
        cp "9"+1
        jr nc,cnvnumf
        sub "0"
        ld b,a              ;b=new digit
        ld a,e              ;a=old value
        add a:jr c,cnvnum6  ;*2
        add a:jr c,cnvnum6  ;*4
        add e:jr c,cnvnum6  ;*5
        add a:jr c,cnvnum6  ;*10
        add b:jr c,cnvnum6
        ld e,a
        inc iy
        inc d
        jr cnvnume
cnvnumf inc d:dec d
        scf
        jr z,cnvnumj
        push de
        call cnvnumt
        pop de
        ret c
        bit 7,e
        scf
        jr nz,cnvnum6
        ld a,e
        inc c
        jr nz,cnvnumg
        neg
cnvnumg ld e,a              ;e=exponent
        call cnvnumc
        ld a,c
        add e
        ;...##!!## check, if sign changed -> overflow
        ld c,a
        jr cnvnumh

cnvnumc ld a,(cnvnumlen)    ;-> c=comma-exponent, cf=1 -> no number
        or a
        scf
        ret z
        ld (cnvnumi+1),a
        ld a,(cnvnumexp)
        or a
        ld c,a
        ret z
cnvnumi sub 0
        dec a
        ld c,a
        or a
        ret

cnvnumt ex de,hl            ;check for terminator
        ld hl,cnvnumtrm
        ld b,cnvnumtrm0-cnvnumtrm
cnvnuma cp (hl)
        jr z,cnvnumb
        inc hl
        djnz cnvnuma
        ld a,3
        scf
        ret
cnvnumb ex de,hl
        ret

;### CNVDEC -> converts byte into two decimal digits
;### Input      A=value
;### Output     L=10 ascii digit, H=1 ascii digit
;### Destroyed  AF
cnvdec  ld l,"0"-1
cnvdec1 inc l
        sub 10
        jr nc,cnvdec1
        add "0"+10
        ld h,a
        ret

;### CNVBOL -> convert number to boolean
;### Input      (DE)=5b float number, L[b3-5]=expression type
;### Output     (celbldbuf)=text, BC=length (with 0-terminator)
;### Destroyed  AF,BC,DE,HL
cnvbolb ds 5
cnvbol  push hl
        ld hl,cnvbolb
        call FLO_MOVE
        call FLO_PREPARE        ;(HL)=LW normed mantissa, B=sign of mantissa (0 -> FLO=0), E=exponent/comma position (decimal)
        inc b:dec b
        ld bc,8
        jr z,cnvbol2
        ld c,b
cnvbol2 pop de
        ld a,e
        and #38
        ld l,a
        ld h,b          ;*8
        add hl,hl       ;*16
        add hl,bc       ;+0/8
        ld bc,cfgunibol
        add hl,bc
        ld bc,8
        ld de,celbldbuf
        push de
        ldir
        pop hl
        call strlen
        inc bc
        ret

;### CNVBIN -> convert number to hex/bin
;### Input      A=type (32*6=bin, 32*7=hex), (DE)=5b float number, H[b3]=0000 sep, H[b0-2]=digits
;### Output     (cnvstrtxt)=text, BC=length (including 0-terminator)
;### Destroyed  AF,BC,DE,HL,IX,IY
cnvbinb db  1, 4, 8,12,16,24,32,32
cnvbinh db  1, 2, 3, 4, 5, 6, 8, 8
cnvbint db 0,"0123456789ABCDEF "

cnvbin  push hl
        push af
        ld hl,cnvfltb
        call FLO_MOVE
        call FLO_FIX_FLO_TO_LW
        jr c,cnvbin1
        pop af
        pop hl
        ld hl,errtxtovf
        ld de,cnvstrtxt
        ld bc,errtxtovf0-errtxtovf
        push bc
        ldir
        pop bc
        ret
cnvbin1 ld hl,(cnvfltb+0)
        ld de,(cnvfltb+2)
        bit 7,b
        call nz,clcn32
        ld a,(cfgnumbin)
        ld (cnvbint+17),a
        pop af
        pop bc
        bit 3,b
        ld iyl,255
        jr z,cnvbin7
        ld iyl,4
cnvbin7 ld c,a
        ld a,b
        and 7
        push af
        ld (cnvbin3+2),a
        ld a,c
        cp 32*6
        ld a,#01
        ld b,a
        ld ix,cnvbinb
        jr z,cnvbin3
        ld a,#0f
        ld b,4
        ld ix,cnvbinh
cnvbin3 ld c,(ix+0)
        ld (cnvbin5+1),a
        ld a,b
        ld (cnvbin4+1),a
        ld ix,cnvstrtxt0-2
        ld (ix+1),0
cnvbin4 ld b,0
        ld a,l
cnvbin5 and 0
        inc a
        ld (ix+0),a
        dec c
        jr z,cnvbin9
        dec ix
        dec iyl
        jr nz,cnvbin6
        ld iyl,4
cnvbin8 ld (ix+0),17
        dec ix
cnvbin6 rr d:rr e:rr h:rr l
        djnz cnvbin6
        jr cnvbin4
;ix=first digit
cnvbin9 pop af
        push ix:pop hl
        sub 7
        jr nz,cnvbinc
cnvbina ld a,(hl)           ;skip leading 0/" "
        and #0f
        dec a
        inc hl
        jr z,cnvbina
        dec hl
        xor a
        cp (hl)
        jr nz,cnvbinc
        dec hl
cnvbinc ld de,cnvstrtxt
        ld ix,cnvbint
        ld c,0
cnvbind ld a,(hl)
        inc hl
        ld (cnvbine+2),a
cnvbine ld a,(ix+0)
        ld (de),a
        inc de
        inc c
        or a
        jr nz,cnvbind
        ld b,0
        ret

;### CNVHXR -> reads hex from string
;### Input      IY=string, HL=5byte FP destination
;### Output     CF=0 -> ok, IY=next, (IY+0)=terminator, HL=default format function
;###            CF=1 -> no binary
;### Destroyed  AF,BC,DE,HL,IX
cnvhxr  ld bc,cnvhxr1
        ld de,cnvhxr2
        ld ix,fmtnhx
        jr cnvbnr0
cnvhxr1 call chrucs
        sub "0"
        ret c
        cp 9+1
        ccf:ret nc
        sub "A"-"9"-1
        cp 10
        ret c
        cp 15+1
        ccf
        ret
cnvhxr2 call cnvbnr5:ret c
        call cnvbnr5:ret c
        call cnvbnr5:ret c
        jr cnvbnr5

;### CNVBNR -> reads bin from string
;### Input      IY=string, HL=5byte FP destination
;### Output     CF=0 -> ok, IY=next, (IY+0)=terminator, HL=default format function
;###            CF=1 -> no binary
;### Destroyed  AF,BC,DE,HL,IX
cnvbnrtrm   db cnvbnrtrm0-cnvbnrtrm-1:db "+-*/\^);<>=&| ",0:cnvbnrtrm0

cnvbnr  ld bc,cnvbnr4
        ld de,cnvbnr5
        ld ix,fmtnbn
cnvbnr0 ld (cnvbnr2+1),bc
        ld (cnvbnr3+1),de
        ld (timred9+1),ix
        call timred0
        inc iy
        ld hl,cnvbnrtrm
        call chrtrm
        ccf:ret c
        ld hl,0
        ld de,0
cnvbnr1 push hl
        ld hl,cnvbnrtrm
        call chrtrm
        pop hl
        inc iy
        jp nc,timred1
cnvbnr2 call cnvbnr4
        ret c
cnvbnr3 call cnvbnr5
        ret c
        ld c,a
        ld b,0
        add hl,bc
        jr cnvbnr1
cnvbnr4 sub "0"
        ret c
        cp 1+1
        ccf
        ret
cnvbnr5 add hl,hl
        ex de,hl
        adc hl,hl
        ex de,hl
        ret


;==============================================================================
;### CUT, COPY AND PASTE ROUTINES #############################################
;==============================================================================

coprngbeg   db 0,0          ;left  upper cell
coprngend   db 0,0          ;right lower cell (-1=single cell)
coprngsta   db 0            ;0=nothing, 1=copy, 2=cut

copdstbeg   db 0,0
copdstend   db 0,0

copcelrec   ds celreclen
copceltxl   db 0,0
copceltxt   ds 252          ;permamnent, can't be share
copceldtl   db 0,0
copceldat   ds 252          ;permamnent, can't be share

;### COPCOP -> copy
copcop  ld a,1
        ld (coprngsta),a
        call mrkact
        jr nz,copcop2
        ld hl,(fldcurp)         ;** single
        ld (coprngbeg),hl
        call copget                 ;copy cell to buffer
        ld hl,-1
        ld (coprngend),hl
copcop1 ;...show animation
        ld a,1+16
        ld hl,(menmaidat2a):call ibkbyt
        jp prgprz0
copcop2 call mrksrt             ;** range
copcop3 ld (coprngbeg),hl
        ld (coprngend),bc
        jr copcop1

;### COPCUT -> cut
copcut  ld a,2
        ld (coprngsta),a
        call mrkact
        jr nz,copcop2
        ld hl,(fldcurp)             ;cut -> source is always a range
        ld c,l:ld b,h
        jr copcop3

;### COPPST -> paste
coppst  call coppst0
        call bsyrng
        ld a,(coprngsta)
        or a
        jp z,prgprz0
        cp 2
        jr nz,coppst6
        call mrkact             ;** source = range cut
        ld hl,(fldcurp)
        jr z,coppst7
        call mrksrt         ;hl=min=destination
coppst7 call coppstd
        jp c,coppste
        ld a,#c9
        ld (copref),a       ;deactivate reference relocator
        jr coppst5
coppst6 ld hl,(coprngend)
        ld a,l
        and h
        inc a
        jr nz,coppst3
        call mrkact             ;** source = single copy
        ld hl,(fldcurp)
        ld e,l:ld d,h
        jr z,coppst1
        call mrksrt
        ld e,c:ld d,b       ;hl=min,de=max
coppst1 ld bc,copput        ;copy cells to range
        call copmul
coppst2 ld bc,recref        ;mark cells, who point to range
        call copmul

        push hl
        push de
        call reccel         ;recalculate all cells in range and who point there
        pop de
        pop hl
        ld bc,coptxt        ;rebuild texts in range
        call copmul
        ld c,e:ld b,d       ;redraw range
        call flddrw
        call recdrw         ;rebuild and redraw remaining cells (who point there)
coppstb ld a,(copmulm)
        or a
        ld a,errmeminfc
        call nz,prgalr
        jp prgprz0

coppst3 call mrkact             ;** source = range copy
        jr nz,coppst4
        ld hl,(fldcurp)     ;dst=single -> expand to range
        call coppstd
        jr nc,coppst5
coppste ld a,errcopinfa     ;error -> range outside field
        call prgalr
        jp prgprz0
coppst4 call mrksrt         ;hl=min dst, bc=max dst
coppst5 ld de,(coprngbeg)   ;de=min src
        ld ix,(coprngend)   ;ix=max src
        ld a,l
        ld (coprng1+1),a    ;set xdst base
        ld a,e
        ld (coprng3+1),a    ;set xsrc base
        ld a,ixl:sub e:inc a
        ld (coprng2+1),a    ;set xsrc len (=modulo)
        ld a,h
        ld (coprng4+1),a    ;set ydst base
        ld a,d
        ld (coprng6+1),a    ;set ysrc base
        ld a,ixh:sub d:inc a
        ld (coprng5+1),a    ;set ysrc len (=modulo)
        ld a,d:cp h
        ld d,0
        rl d                ;ysrc<ydst -> revers ycopy
        ld a,e:cp l
        ld a,d
        rla                 ;xsrc<xdst -> revers xcopy
        ld e,c:ld d,b
        ld bc,coprng
        call copmul
        ld a,(coprngsta)
        cp 2
        jp nz,coppst2
        ld (copdstbeg),hl       ;** cut
        push hl
        ld (copdstend),de
        xor a
        ld (copref),a       ;reactivate reference relocator
        ld hl,(coprngbeg)
        push hl
        ld de,(coprngend)
        ld bc,copdel
        call copmul         ;delete cells in source range, if not in destination
        call coprr1         ;activate reference-in-range check
        pop de
        pop hl
        ld a,l:sub e:ld (copref7+1),a   ;dif clm
        ld a,h:sub d:ld (copref9+1),a   ;dif row
        ld ix,celrecmem     ;relocate all cells with references, which point to source range
        ld bc,(celcntall)
coppst8 ld a,c
        or b
        jr z,coppstc
        push bc
        ld a,(ix+0)
        and 63
        cp celtypfrn
        jr nz,coppst9       ;no formula -> ignore
        ld e,(ix+celrecdat+0)   
        ld d,(ix+celrecdat+1)
        ld a,(ix+celrecref)
        ld hl,celforref
        call copref0
coppst9 ld bc,celreclen
        add ix,bc
        pop bc
        dec bc
        jr coppst8
coppstc call coprr0         ;deactivate reference-in-range check
        xor a
        ld (coprngsta),a    ;reset copypaste
        add 16
        ld hl,(menmaidat2a):call ibkbyt
        ld hl,(coprngbeg)   ;mark cells, who point to old range
        ld de,(coprngend)
        push de
        push hl
        ld bc,recref
        push bc
        call copmul
        pop bc
        ld hl,(copdstbeg)   ;mark cells, who point to new range
        ld de,(copdstend)
        push de
        push hl
        call copmul
        call reccel         ;recalculate cells
        pop hl              ;hl=dstbeg
        pop de              ;de=dstend
        pop bc              ;bc=srcbeg
        pop ix              ;ix=srcend
        jr c,coppsta
        ld a,l:cp   c:jr  c,$+3:ld l,c
        ld a,h:cp   b:jr  c,$+3:ld h,b      ;hl=min(dst,src)
        ld a,e:cp ixl:jr nc,$+4:ld e,ixl
        ld a,d:cp ixh:jr nc,$+4:ld d,ixh    ;de=max(dst,src)
        ld (recdrwmin),hl
        ld (recdrwmax),de
        xor a
        ld (recdrwmem),a
        ld ix,celrecmem-celreclen
        ld hl,(celcntdrw)
        call recdrwd        ;redraw all changed cells
        call recupd4
        jp coppstb
coppsta call recupd2
        jp coppstb

coppstd ld de,(coprngbeg)   ;hl=source beg -> test, if destination is outside field
        ld bc,(coprngend)
        ld a,c:sub e:add l
        ret c               ;-> outside x, error
        cp fldmaxx+1
        ccf:ret c
        ld c,a
        ld a,b:sub d:add h
        ret c               ;-> outside y, error
        cp fldmaxy+1
        ccf
        ld b,a              ;bc=source end
        ret

coppst0 xor a               ;init paste
        ld (copmulm),a
        ld l,a:ld h,a
        ld (celcntrec),hl
        ret

;### COPDEL -> removes cell, if not in destination range
;### Input      L,H=clm/row
;### Output     CF=0
copdel  ld de,copdstbeg
        ld a,l
        call clcrng         ;clm in range?
        jr c,copdel1        ;no -> delete
        ld a,h
        call clcrng         ;row in range?
        ret nc              ;yes -> keep
copdel1 call fldcel
        ccf
        ret nc              ;no cell -> return
        call celrem
        or a
        ret

;### COPRNG -> paste from range
;### Input      L,H=destination clm/row
;### Output     CF=0 -> ok, CF=1 -> memory full, HL=cell record
coprng  push hl
        ld a,l
coprng1 sub 0
coprng2 ld c,0
        call coprng7
coprng3 add 0              ;xsrc=((xdstpos-xdstbas) mod xsrclen)+xsrcbas
        ld l,a
        ld a,h
coprng4 sub 0
coprng5 ld c,0
        call coprng7
coprng6 add 0              ;ysrc=((ydstpos-ydstbas) mod ysrclen)+ysrcbas
        ld h,a
        call copget
        pop hl
        jp copput
;a=a mod c
coprng7 cp c
        ret c
        sub c
        jr coprng7

;### COPTXT -> rebuilds text for cell in range, if it has been updated
;### Input      L,H=clm/row
;### Output     CF=0 -> ok, CF=1 -> memory full, HL=cell record
coptxt  call fldcel
        ccf
        ret nc
        ld a,(hl)
        cp celtypfrn+128
        scf:ccf
        ret nz
        push hl:pop ix
        ld hl,(celcntdrw)
        dec hl
        ld (celcntdrw),hl
        jp recdrw0

;### COPMUL -> does operation on multiple cells
;### Input      BC=routine (will be called with L,H=clm/row; has to return CF=0 if not memory full),
;###            HL=upper left cell, DE=lower right cell, A=direction (bit0=x direction, bit1=y direction; 0=asc,1=desc)
;### Output     CF=1 -> memory full
;### Destroyed  AF,BC,IX,IY
copmulm db 0    ;memory full flag

copmul  ld (copmul2+1),bc
        push hl
        bit 1,a     ;zf=1 -> y-asc
        rra         ;cf=0 -> x-asc

        db #3e:ld l,c
        db #01:inc l:cp e
        jr nc,copmul7
        db #3e:ld l,e
        db #01:dec l:cp c
copmul7 ld (copmul0+0),a
        ld (copmul5),a
        ld (copmul4),bc

        db #3e:ld h,b
        db #01:inc h:cp d
        jr z,copmul8
        db #3e:ld h,d
        db #01:dec h:cp b
copmul8 ld (copmul0+1),a
        ld (copmul6),bc

        ld c,l:ld b,h       ;bc=min,de=max,hl=cur
copmul0 ld l,c              ;ld l,e [bit0=1]
        ld h,b              ;ld h,d [bit1=1]
copmul1 push hl
        push de
        push bc
copmul2 call 0
        jr nc,copmul3
        call celrem         ;memful -> delete current cell, prepare error message
        ld a,-1
        ld (copmulm),a
copmul3 pop bc
        pop de
        pop hl
        ld a,l
copmul4 inc l               ;dec l  [bit0=1]
        cp e                ;cp c   [bit0=1]
        jr nz,copmul1
copmul5 ld l,c              ;ld l,e [bit0=1]
        ld a,h
copmul6 inc h               ;dec h  [bit1=1]
        cp d                ;cp b   [bit1=1]
        jr nz,copmul1

        pop hl                      ;hl=first, de=last
        ld a,(copmulm)
        add 1
        ret

;### COPGET -> copies a single cell to the buffer
;### Input      L,H=source cell
;### Output     (copcelXXX)=cell data
;### Destroyed  AF,BC,DE,HL,IX
copget  call fldcel
        jr nc,copget1
        xor a
        ld (copcelrec+celrectyp),a
        ret
copget1 ld de,copcelrec
        ld bc,celreclen
        ldir
        ld bc,celrectxt-celreclen
        add hl,bc
        ld de,copceltxl
        call copget2
        ld de,copceldtl
copget2 ld c,(hl):inc hl
        ld b,(hl):inc hl
        ld a,(bc)
        or a
        jr z,copget3
        sub 3
        ld (de),a
        inc de:inc de
        push hl
        ld l,c:ld h,b
        ld c,a
        ld b,0
        inc hl:inc hl:inc hl
        ldir
        pop hl
        ret
copget3 ld (de),a
        ret

;### COPPUT -> copies from buffer to single cell, relocate references, mark for recalc if formula
;### Input      L,H=destination cell, (copcelXXX)=cell data
;### Output     CF=0 -> ok, CF=1 -> memory full, HL=cell record
;### Destroyed  AF,BC,DE,HL,IX,IY
copput  ld a,(copcelrec+celrectyp)
        or a
        jr z,copput3
        ld de,(copcelrec+celrecclm)     ;e=old clm, d=old row
        ld a,l:sub e:ld (copref7+1),a   ;dif clm
        ld a,h:sub d:ld (copref9+1),a   ;dif row
        push hl
        call fldcel
        jr c,copput2
        pop de
copput1 push hl                 ;** update existing
        ex de,hl
        ld hl,copcelrec
        ld a,(hl)
        cp celtypfrn
        jr nz,copput4
        set 6,a
        ld bc,(celcntrec)
        inc bc
        ld (celcntrec),bc
copput4 ld (de),a           ;copy type and mark for recalc
        ld bc,celrecref     ;skip clm/row/ctr/pointers
        add hl,bc
        ex de,hl
        add hl,bc
        ex de,hl
        ld bc,4
        ldir                ;copy refcnt,colours,display,format
        pop hl
        ld bc,(copceldtl)
        ld de,copceldat
        call celuda         ;update data, de=data
        ret c
        ld a,(copcelrec+celrectyp)
        cp celtypfrn
        push hl
        call z,copref
        pop hl
        ld bc,(copceltxl)
        ld de,copceltxt
        push hl
        call celutx         ;update text
        pop ix
        jp fmtctr           ;update control format
copput2 pop bc                  ;** create new
        push bc
        call celnew
        jr c,copput5        ;memory full without cell record -> set mem-flag instead of CF
        call copput1        ;de=text
        jr c,copput6
        ex (sp),hl          ;l,h=clm/row
        call celcnw
        pop ix
        ld (ix+celrecctr),a
        or a
        ret z
        call fmtctr
        or a
        ret
copput3 call fldcel             ;** delete cell, if existing
        ccf
        ret nc
        call celrem
        or a
        ret
copput5 ld a,-1
        ld (copmulm),a
        or a
copput6 pop bc
        ret

;### COPRR1 -> activate reference in range check, if "cut" mode
coprr1  ld hl,coprfr    ;only adjust, if in range
        ld c,#3e        ;LD A,x -> always adjust
        jr coprr

;### COPRR0 -> deactivate reference in range check, if "cut" mode
coprr0  ld hl,coprfr1   ;always adjust
        ld c,#20        ;JR NZ,x -> only adjust, if relative
coprr   ld a,(coprngsta)
        cp 2
        ret nz
        ld (coprefc+1),hl
        ld a,c
        ld (coprefd),a
        ld (coprefe),a
        ret

;### COPRFR -> check, if reference completely points to range
;### Input      (HL)=reference, C[2]=single/range, (coprngbeg)=range begin, (coprngend)=range end
;### Output     CF=0 -> completely inside range
;###            CF=1 -> (partially) outside range
;### Destroyed  AF,DE
coprfr  push hl
        call coprfr3
        jr c,coprfr2
        bit 2,c
        jr z,coprfr2
        call coprfr3
coprfr2 pop hl
        ret
coprfr3 ld de,coprngbeg
        call coprfr4
        ret c
coprfr4 ld a,(hl)
        inc hl
        jp clcrng
coprfr1 or a
        ret

;### COPREF -> adjust references, if relative
;### Input      (copref7+1)=clm dif, (copref9+1)=row dif, DE=cell data, (copcelrec+celrecref)=ref counter
;### Output     B=1 -> changed reference(s)
;### Destroyed  AF,C,DE,HL
copref  db 0
        ld a,(copcelrec+celrecref)
        ld hl,5
copref0 ld b,h
        add hl,de
        ld (copref3+1),hl       ;hl=single ref data
        ld c,a
        and 15
        add a:add a
        ld e,a
        ld d,0
        add hl,de
        ld (copref4+1),hl       ;hl=range ref data
        ld a,c
        rrca:rrca:rrca:rrca
        and 15
        add a:add a
        ld e,a
        ld d,0
        add hl,de               ;hl=formula
copref1 ld a,(hl)
        or a
        ret z                   ;0    -> end
        inc hl
        cp fortokref
        jr nc,copref2
        ld de,5                 ;1-7  -> 5byte value
        add hl,de
        jr copref1
copref2 cp fortokref+8
        jr nc,copref1           ;>16  -> 1byte token
        ld c,a                  ;8-15 -> reference
        ld a,(hl)               ;a=index
        inc hl
        push hl
        add a:add a
        ld l,a
        ld h,0
        bit 2,c                 ;single or range?
copref3 ld de,0
        jr z,copref5
copref4 ld de,0
copref5 add hl,de               ;hl=reference
coprefc call coprfr1            ;(optionally check, if ref in range)
        jr c,coprefb
        call copref6            ;adjust first cell
        bit 2,c
        call nz,copref6         ;adjust second cell, if range
coprefb pop hl
        jr copref1
copref6 bit 0,c                 ;(hl)=clm,row -> adjust, if relative -> hl+=2
coprefd jr nz,copref8
        ld a,(hl)
copref7 add 0
        ld (hl),a
        set 0,b
copref8 inc hl
        bit 1,c
coprefe jr nz,coprefa
        ld a,(hl)
copref9 add 0
        ld (hl),a
        set 0,b
coprefa inc hl
        ret


;==============================================================================
;### MOVE CELLS/ROWS/COLUMNS ##################################################
;==============================================================================

;### MOVCID -> insert cells down
movcid  call movval     ;HL=source begin, C=source xsize, B=source ysize
        ld a,c
        add l
        dec a
        ld e,a          ;E=source x end
        jr movrwi1

;### MOVCIR -> insert cells right
movcir  call movval     ;HL=source begin, C=mark xsize, B=mark ysize
        ld a,b
        add h
        dec a
        ld d,a          ;D=source y end
        jr movcli1

;### MOVCRU -> remove cells up
movcru  call movval
        ld d,fldmaxy
        ld a,c
        add l
        dec a
        ld e,a          ;E=source x end
        jr movrwr1

;### MOVCRL -> remove cells left
movcrl  call movval
        ld e,fldmaxx
        ld a,b
        add h
        dec a
        ld d,a          ;D=source y end
        jr movclr1

;### MOVRSI -> insert single row
movrsi  call movvlb
        jr movrwi0

;### MOVRWI -> insert rows
movrwi  call movval     ;HL=mark begin, C=mark xsize, B=mark ysize
movrwi0 ld l,0          ;L=first column -> HL=source beg
        ld e,fldmaxx    ;E=last column
movrwi1 ld a,fldmaxy
        sub b
        ld d,a          ;D=end row minus ysize -> DE=source end
        push bc
        call movopt     ;HL,BC=source beg/end
        jr z,movrwr2
        pop af          ;A=ysize
        add h
        ld d,a          ;D=ysize+source Y beg
        ld e,l          ;E=source X beg -> DE=below source
        jr movmov

;### MOVCSI -> insert single column
movcsi  call movvlb
        jr movcli0

;### MOVCLI -> insert columns
movcli  call movval     ;HL=mark begin, C=mark xsize, B=mark ysize
movcli0 ld h,0          ;H=first row -> HL=source beg
        ld d,fldmaxy    ;D=last row
movcli1 ld a,fldmaxx
        sub c
        ld e,a          ;E=end column minus xsize -> DE=source end
        ld b,c
        push bc
        call movopt     ;HL,BC=source beg/end
        jr z,movrwr2
        pop af          ;A=xsize
        add l
        ld e,a          ;E=xsize+source X beg
        ld d,h          ;E=source Y beg -> DE=below source
        jr movmov

;### MOVRSR -> remove single row
movrsr  call movvlb
        jr movrwr0

;### MOVRWR -> remove rows
movrwr  call movval     ;HL=mark begin, C=mark xsize, B=mark ysize
movrwr0 ld de,256*fldmaxy+fldmaxx   ;DE=source end
        ld l,0          ;L=first column -> HL=destination beg
movrwr1 push hl
        ld a,b
        add h
        ld h,a          ;HL=source beg
        push hl
        call movopt     ;HL,BC=source beg/end
        pop hl
movrwr2 pop de
        jr nz,movmov
        jp prgprz0

;### MOVCSR -> remove single column
movcsr  call movvlb
        jr movclr0

;### MOVCLR -> remove columns
movclr  call movval     ;HL=mark begin, C=mark xsize, B=mark ysize
movclr0 ld de,256*fldmaxy+fldmaxx   ;DE=source end
        ld h,0          ;H=first row -> HL=destination beg
movclr1 push hl
        ld a,c
        add l
        ld l,a          ;HL=source beg
        push hl
        call movopt     ;HL,BC=source beg/end
        pop hl
        pop de
        jr nz,movmov
        jp prgprz0

;### MOVMOV -> moves cells using cut&paste
;### Input      HL=source beg, BC=source end, DE=destination beg
movmov  ld (coprngbeg),hl
        ld (coprngend),bc
        ld a,2
        ld (coprngsta),a
        call coppst0
        push de
        call bsyrng
        pop hl
        jp coppst7

;### MOVVAL -> prepares values for cells movement
;### Output     HL=mark begin, C=mark xsize, B=mark ysize
movval  call mrkact
        ld hl,(fldcurp)
        ld c,l:ld b,h
        call nz,mrksrt      ;HL=upper left, BC=lower right
        ld a,c
        sub l
        inc a
        ld c,a              ;c=xsize
        ld a,b
        sub h
        inc a
        ld b,a              ;b=ysize
        ret

;### MOVVLB -> prepares values for bar-clicked single row/column movement
movvlb  ld l,d
        ld h,d
        ld bc,#0101
        ret

;### MOVOPT -> optimize source range
;### Input      HL=beg, DE=end
;### Output     HL=beg optimized, BC=end optimized, ZF=1 -> no cell in area
movopt  ld iy,(celcntall)
        ld a,iyl:or iyh
        ret z
        iy_counter
        ld a,e:inc a
        ld (movopt2+1),a
        ld a,d:inc a
        ld (movopt4+1),a
        ld a,l
        ld (movopt3+1),a
        ld a,h
        ld (movopt5+1),a
        ld hl,celrecmem+celrecclm
        ld de,celreclen
        ld ix,256*fldmaxy+fldmaxx   ;ix=min
        ld bc,0                     ;bc=max

movopt1 ld a,(hl)           ;a=column           ** check if inside area
movopt2 cp 0
        jr nc,movopt9       ;>max -> ignore
movopt3 cp 0
        jr c,movopt9        ;<min -> ignore
        inc hl
        ld a,(hl)           ;a=row
        dec hl
movopt4 cp 0
        jr nc,movopt9       ;>max -> ignore
movopt5 cp 0
        jr c,movopt9        ;<min -> ignore (24)

        cp ixh              ;a=row              ** check if new min/max row
        jr nc,movopt6       ;>=current min -> keep
        ld ixh,a            ;set new min row
movopt6 cp b
        jr c,movopt7        ;<current max -> keep
        ld b,a              ;set new max row

movopt7 ld a,(hl)
        cp ixl              ;a=column           ** check if new min/max column
        jr nc,movopt8       ;>=current min -> keep
        ld ixl,a            ;set new min column
movopt8 cp c
        jr c,movopt9        ;<current max -> keep
        ld c,a              ;set new max column (21)

movopt9 add hl,de
        dec iyh
        jr nz,movopt1       ;(8 -> 24+21+8=53 -> 376 cells/frame, 5frames for 2048)
        dec iyl
        jr nz,movopt1

        ld a,c
        sub ixl             ;a=max-min
        jr c,movopta        ;max<min -> no cells found
        push ix:pop hl
        inc a               ;found -> zf=0 (a was >=0, now still >0)
        ret
movopta xor a               ;not found ->  zf=1
        ret


;==============================================================================
;### CONFIG ###################################################################
;==============================================================================

read"App-SymCalc-Config.asm"

;### CFGINI -> init config
cfgini  ld hl,cfgprgbeg
        call cfgmem_b
        call cfginit
;init statusbar view
cfginis ld a,(cfgviwsta)
        add a
        inc a
        add 16
menmaidat31 equ $+1
        ld hl,menmaidat31:call ibkbyt
        bit 1,a
        ld hl,-40
        ld de,-32
        ld a,wincnt_all
        jr nz,cfgini1
        ld hl,-32
        ld de,-24
        ld a,wincnt_all-wincnt_sta
cfgini1 ld (winmaigrp),a
        ld (16*0+winmaiclc1+12),hl
        ld (16*1+winmaiclc1+12),de
        ret
;init toolbar view
cfginit ld a,(cfgviwtol)
        add a
        inc a
        add 16
menmaidat30 equ $+1
        ld hl,menmaidat30:call ibkbyt
        and 2
        add a:add a
        ld hl,winmaidat+1
        res 3,(hl)
        or (hl)
        ld (hl),a
        ret

;errors
errtxtovf   db "OVERFLOW",0:errtxtovf0

;### CFGOPN -> opens config dialogue
cfgopn  ld hl,_cfgopn
        jp guijmp_b


;==============================================================================
;### DOCUMENT PROPERTIES ######################################################
;==============================================================================

;### PRPOPN -> open document properties window
prpopnm ds 19

prpopn  ld hl, celrecmax           :ld (prpopnm+00),hl  ;cells
        ld hl,(celcntall)          :ld (prpopnm+02),hl
        ld hl,(celdatrec+memstrmax):ld (prpopnm+04),hl  ;data
        ld hl,(celdatrec+memstrfre):ld (prpopnm+06),hl
        ld hl,(celtxtrec+memstrmax):ld (prpopnm+08),hl  ;text
        ld hl,(celtxtrec+memstrfre):ld (prpopnm+10),hl
        ld  a, ctrcelmax           :ld (prpopnm+12),a   ;controls
        ld  a,(ctrcelnum)          :ld (prpopnm+14),a
        xor a:ld l,a:ld h,a        :ld (prpopnm+16),hl  ;external sheets
                                   :ld (prpopnm+18),a
        ld de,prpopnm
        ld hl,_prpopn
        jp guijmp_b

;### PRPOKY -> document properties has been saved
;### Input      E=type (0=nothing, +1=update all texts of number cells, +2=redraw field)
prpoky  ld a,1
        ld (docmodf),a
        push de
        call doccol
        pop de
        inc e:dec e
        jp z,prgprz0
        bit 0,e
        jr z,prpoky3
        ld hl,(celcntall)       ;hl=cell count
        ld a,l:or h
        jp z,prgprz0
        ld ix,celrecmem         ;rebuild all existing cells
prpoky1 push hl
        ld a,(ix+celrectyp)
        cp celtyptxt
        jr nc,prpoky2
        ld e,(ix+celrecdat+0)
        ld d,(ix+celrecdat+1)
        inc de:inc de:inc de
        ld l,(ix+celrecdsp)
        ld h,(ix+celrecfmt)
        push ix
        call celbld
        ld de,celbldbuf
        pop hl:push hl
        call celutx
        jr c,prpoky4
        pop ix
prpoky2 ld de,celreclen
        add ix,de
        pop hl
        dec hl
        ld a,l:or h
        jr nz,prpoky1
prpoky3 ld e,winmairec_field
        ld a,(winmainum)
        call SyDesktop_WINDIN   ;update display
        jp prgprz0
prpoky4 pop hl                  ;** memful exception
        pop de
        call celrem
        ld a,errmeminfb
        call prgalr
        jr prpoky3


;==============================================================================
;### INTERBANK HANDLING #######################################################
;==============================================================================

;### IBKINI -> init extended modules
ibkinif db "ex1",0:datnam0

ibkini  ld a,"1"
        ld hl,ib1msg
        call ibkini1
        ld a,"2"
        ld hl,ib2msg
ibkini1 push hl
        ld (ibkinif+2),a
        ld hl,ibkinif
        call prgfil0
        ld hl,(prgpth)
        ld a,(App_BnkNum)
        push af
        call SySystem_PRGRUN
        or a
        jp nz,prgend
        ld ix,(App_PrcID)
        ld a,h
        ld ixh,a
        pop af
        pop iy
        ld (iy+ib1msgb-ib1msg),a
        push ix
        rst #10
        pop ix
        rst #08
        ;...                    ;##!!## check if too long, if correct etc
        ret

;### IBKBYT -> sets byte in extended module
;### Input      A=value, HL=address
;### Output     HL=HL+1
;### Destroyed  BC
ibkbyt  push af
        ld b,a
ibkbyt1 ld a,0
        rst #20:dw jmp_bnkwbt
        pop af
        ret

;### IBKLOC -> calls local routine from external module with full register transfer
;### Input      HL=routine ID, (ibkreg)=AF,BC,DE,HL,IX,IY
;### Return     (ibkreg)=AF,BC,DE,HL,IX,IY
ibkreg  ds 12
ibkloc  ld bc,ibkloc1
        push bc
        ld bc,ibktab
        add hl,bc
        ld c,(hl):inc hl
        ld h,(hl):ld l,c
        push hl
        ld hl,(ibkreg+0)
        push hl:pop af
        ld bc,(ibkreg+2)
        ld de,(ibkreg+4)
        ld hl,(ibkreg+6)
        ld ix,(ibkreg+8)
        ld iy,(ibkreg+10)
        ret
ibkloc1 ld (ibkreg+2),bc
        ld (ibkreg+4),de
        ld (ibkreg+6),hl
        ld (ibkreg+8),ix
        ld (ibkreg+10),iy
        push af:pop hl
        ld (ibkreg+0),hl
        jp jmp_bnkret

ibktab
dw celbldx:celbld_  equ 00
dw filbrw :filbrw_  equ 02
dw movopt :movopt_  equ 04
dw celget :celget_  equ 06
dw celput :celput_  equ 08
dw celimp :celimp_  equ 10

guijmp1 ld a,l
        cp _cfgcol
        jr z,guijmp2
        cp _prpcol
        jr z,guijmp2
        cp _cfdpen
        jr z,guijmp2
        cp _cfdpap
        jr nz,guijmp_b
guijmp2 ld a,(App_MsgBuf+4):ld e,a
        ld a,(App_MsgBuf+6):ld d,a
guijmp_b
        call guijmp_c
        dec l:jp z,cfdoky   ;ret_cfdoky
        dec l:jp z,barsst1  ;ret_barsdi
        dec l:jp z,prpoky   ;ret_prpoky
        dec l:jp z,fmtaln8  ;ret_fmtaln
        ;...
        jp prgprz0
guijmp_c
guijmp equ $+2:ld ix,guijmp:jp ib1clx

cfgmem_b:cfgmem equ $+2:ld ix,cfgmem:jr ib1cll
cfgsav_b:cfgsav equ $+2:ld ix,cfgsav:jr ib1cll
prpdef_b:prpdef equ $+2:ld ix,prpdef:jr ib1cll
prpsav_b:prpsav equ $+2:ld ix,prpsav:jr ib1cll
prplod_b:prplod equ $+2:ld ix,prplod:jr ib1cll

barini0_b   ld hl,ctrgrdver+2:barini0 equ $+2:ld ix,barini0:jr ib1cll
barini1_b   ld hl,ctrgrdhor+2:barini1 equ $+2:ld ix,barini1:jr ib1cll
barmem_b:barmem equ $+2:ld ix,barmem:jr ib1cll
barmov_b:barmov equ $+2:ld ix,barmov:jr ib1cll
barset_b:barset equ $+2:ld ix,barset:jr ib1cll
barofs_b:barofs equ $+2:ld ix,barofs:jr ib1cll
barmen_b:barmen equ $+2:ld ix,barmen:jr ib1cll
barsdi_b:barsdi equ $+2:ld ix,barsdi:jr ib1cll
celmen_b:celmen equ $+2:ld ix,celmen:jr ib1cll

hshini_b:hshini equ $+2:ld ix,hshini:jr ib1cll
hshnew_b:hshnew equ $+2:ld ix,hshnew:jr ib1cll
hshrem_b:hshrem equ $+2:ld ix,hshrem:jr ib1cll
hshupd_b:hshupd equ $+2:ld ix,hshupd:jr ib1cll
hshfnd_b:hshfnd equ $+2:ld ix,hshfnd

ib1cll  ld b,0
prgstk2 equ $+2
        ld iy,prgstk2
        jp jmp_bnkcll

ib1clx  ld b,0
prgstk2a equ $+2
        ld iy,prgstk2a
        jp jmp_bnkcll


;==============================================================================
;### SUB ROUTINES #############################################################
;==============================================================================

;### JMPTAB -> jumps to routine specified by a value and a table
;### Input      L=value (0-x), BC=table with jump addresses
;### Destroyed  F,C,HL
jmptab  ld h,0
        add hl,hl
        add hl,bc
        ld c,(hl)
        inc hl
        ld h,(hl)
        ld l,c
        jp (hl)

;### KEYCHK -> checks key
;### Input      HL=key jump table, B=number of entries, (App_MsgBuf+4)=key
;### Output     ZF=1 -> HL=jump address, ZF=0 -> not found
keychk  ld a,(App_MsgBuf+4)
        ld de,3
keychk1 ld c,(hl)
        cp c
        jr z,keychk2
        add hl,de
        djnz keychk1
        inc b
        ret
keychk2 inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ret


;### BSYRNG -> starts showing busy message, if range is sizex+sizey>=12
;### Destroyed  AF,HL,DE
bsyrngm equ 12
bsyrng  ld hl,(coprngend)
        ld a,l:or h:inc a
        ret z
        ld de,(coprngbeg)
        ld a,h:sub d
        cp bsyrngm
        jr nc,bsybeg
        add l:sub e
        cp bsyrngm
        ret c
;### BSYBEG -> starts showing busy message
;### Destroyed  AF,HL,DE
bsybeg  db 0
        ld de,#c900
        ld hl,txtbotbsy
bsybeg1 push bc
        push ix
        push iy
        ld a,d
        ld (bsybeg),a
        ld a,e
        ld (bsyend),a
        ld (ctrbotinf),hl
        ld a,(winmainum)
        ld e,winmairec_sum
        call SyDesktop_WININH   ;show "busy"
        pop iy
        pop ix
        pop bc
        ret

;### BSYEND -> stops showing busy message
;### Destroyed  AF,HL,DE
bsyend  ret
        ld de,#00c9
        ld hl,txtbotinf
        jr bsybeg1

;### CLCRNG -> check, if value in range
;### Input      (DE+0)=range min, (DE+2)=range max, A=value
;### Output     CF=0 -> inside, DE+=1, CF=1 -> outside
;### Destroyed  (DE,)F
clcrng  ex de,hl:cp (hl):ex de,hl
        ret c           ;value<min -> outside
        inc de:inc de
        ex de,hl:cp (hl):ex de,hl
        dec de
        ret z           ;value=max -> inside
        ccf             ;value>max -> outside
        ret

;### CLCM86 -> 8*16bit multiplication
;### Input      A,DE=values
;### Output     AHL=A*DE
;### Destroyed  F,BC
clcm86  ld c,0
        ld h,c
        ld l,h
        add a           ;optimised 1st iteration
        jr nc,clcm861
        ld h,d
        ld l,e
clcm861 ld b,7
clcm862 add hl,hl
        rla
        jr nc,clcm863
        add hl,de
        adc c           ;yes this is actually adc a, 0 but since c is free we set it to zero and so we can save 1 byte and up to 3 T-states per iteration
clcm863 djnz clcm862
        ret

;### CLCMU8 -> 8bit unsigned multiplication
;### Input      L,H=values
;### Output     HL=L*H
;### Destroyed  F,B,DE
clcmu8  ld e,l
        ld d,0
        ld l,d
        ld b,8
clcmu81 add hl,hl
        jr nc,clcmu82
        add hl,de
clcmu82 djnz clcmu81
        ret

;### CLCS32 -> substract 32bit values
;### Input      DE,HL=variable, (IX)=value
;### Output     DE,HL=DE,HL-(IX), CF=1 overflow
;### Destroyed  F,BC
clcs32  ld c,(ix+0)
        ld b,(ix+1)
        or a
        sbc hl,bc
        ld c,(ix+2)
        ld b,(ix+3)
        ex de,hl
        sbc hl,bc
        ex de,hl
        ret

;### CLCN32 -> negative of 32bit value
;### Input      DE,HL=32bit value
;### Output     DE,HL=-32bit value, A=128
;### Destroyed  F
clcn32  xor a:sub l:ld l,a
        sbc a:sub h:ld h,a
        sbc a:sub e:ld e,a
        sbc a:sub d:ld d,a
        ld a,128
        ret

;### STRCMP -> string compare
;### Input      HL=string1, DE=string2, B=length
;### Output     ZF=1 -> same
;### Destroyed  AF,B,DE,HL
strcmp  ld a,(de)
        cp (hl)
        ret nz
        inc de
        inc hl
        djnz strcmp
        ret

;### WINPOS -> gets main window position
;### Output     HL=xpos, DE=ypos
;### Destroyed  AF
winpos  ld a,(winmaidat+0)
        ld hl,0
        ld de,0
        cp 2
        ret z
        ld hl,(winmaidat+4)
        inc hl
        ld de,(winmaidat+6)
        inc de
        ret

;### WINBLR -> dropdown window lost focus -> close it
winblr  call winblr0
        jp prgprz0
winblr0 ld hl,windrpnum
        ld a,(hl)
        ld (hl),0
        jp SyDesktop_WINCLS   ;close dropdown window

;### WINCLS -> closes dialogue window
wincls  ld a,(windianum)
        jp SyDesktop_WINCLS

;### WINBOT -> shows booting window
winbot  ld de,winbotdat
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN       ;open boot window
        jp c,prgend
        ld (windianum),a
        ret

;### MEMINI -> inits memory stuff
memini  ld hl,celrecmax*celreclen
        ld de,celrecmem
        add hl,de
        ld (celdatrec+0),hl
        ld (celdatrec+2),hl
        ret


;==============================================================================
;### MODULES ##################################################################
;==============================================================================

read"App-SymCalc-Engine.asm"
read"App-SymCalc-Memory.asm"
read"App-SymCalc-Float.asm"
read"App-SymCalc-Subs.asm"


;==============================================================================
;### CELL DATA ################################################################
;==============================================================================

celdatrec
dw 0            ;total memory start (=celrecmax*celreclen+celrecmem)
dw 0            ;free  memory start (=celrecmax*celreclen+celrecmem)
dw celdatmax    ;total memory length
dw celdatmax    ;free  memory length
dw 0            ;number of existing elements

celrecmem   ds celreclen    ;***LAST IN CODE AREA***

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;### DATA AREA ################################################################
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

App_BegData

read"App-SymCalc-Font.asm"

txtcfdprv   ds 40+6

;==============================================================================
;### CELL TEXT ################################################################
;==============================================================================

celtxtmax   equ 8000-$+App_BegData  ;cell text memory
celtxtrec
dw celtxtmem    ;total memory start
dw celtxtmem    ;free  memory start
dw celtxtmax    ;total memory length
dw celtxtmax    ;free  memory length
dw 0            ;number of existing elements

celtxtmem   db 0    ;***LAST IN DATA AREA***

botlog db 78,156,20:dw $+7,$+4,156*20:db 5
db #11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#66,#66,#66,#66,#66,#66
db #66,#66,#61,#11,#11,#11,#11,#11,#66,#66,#66,#11,#11,#11,#11,#11,#11,#66,#66,#66,#61,#11,#11,#11,#11,#11,#11,#11,#11,#11,#66,#66,#66,#66,#66,#66,#66,#66,#61
db #11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#66,#88,#88,#88,#88,#88,#88
db #88,#88,#61,#11,#11,#11,#11,#16,#88,#88,#88,#61,#11,#11,#11,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#11,#11,#66,#88,#88,#88,#88,#88,#88,#88,#88,#61
db #11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#66,#88,#88,#88,#88,#88,#88,#88
db #88,#86,#11,#11,#11,#11,#11,#16,#88,#88,#88,#61,#11,#11,#11,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#11,#66,#88,#88,#88,#88,#88,#88,#88,#88,#86,#11
db #11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#16,#88,#88,#88,#88,#88,#88,#88,#88
db #88,#61,#11,#11,#11,#11,#11,#68,#88,#88,#88,#86,#11,#11,#11,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#16,#88,#88,#88,#88,#88,#88,#88,#88,#88,#61,#11
db #11,#11,#11,#17,#66,#66,#66,#66,#66,#66,#71,#76,#66,#61,#11,#11,#17,#66,#67,#16,#66,#11,#11,#11,#11,#11,#11,#17,#66,#71,#16,#88,#88,#88,#88,#88,#88,#88,#88
db #86,#11,#11,#11,#11,#11,#16,#88,#88,#88,#88,#88,#61,#11,#11,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#16,#88,#88,#88,#88,#88,#88,#88,#88,#86,#11,#11
db #11,#11,#17,#66,#77,#11,#11,#11,#11,#11,#67,#67,#11,#76,#11,#11,#66,#71,#76,#76,#17,#61,#11,#11,#11,#11,#11,#76,#17,#61,#68,#88,#88,#86,#66,#66,#66,#66,#66
db #61,#11,#11,#11,#11,#11,#68,#88,#88,#88,#88,#88,#61,#11,#11,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#68,#88,#88,#86,#66,#66,#66,#66,#66,#61,#11,#11
db #11,#11,#76,#11,#11,#11,#11,#11,#11,#11,#67,#76,#11,#16,#71,#17,#61,#11,#67,#76,#11,#76,#11,#11,#11,#11,#17,#61,#17,#61,#68,#88,#88,#61,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#11,#11,#68,#88,#88,#88,#88,#88,#86,#11,#11,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#11,#11
db #11,#17,#61,#11,#11,#11,#11,#11,#11,#11,#67,#16,#11,#17,#61,#16,#67,#77,#61,#76,#11,#17,#61,#11,#11,#11,#76,#11,#17,#61,#68,#88,#86,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#11,#16,#88,#88,#88,#66,#88,#88,#88,#61,#11,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#68,#88,#86,#11,#11,#11,#11,#11,#11,#11,#11,#11
db #11,#76,#11,#11,#76,#66,#66,#66,#66,#66,#71,#16,#71,#11,#61,#16,#66,#66,#11,#76,#11,#11,#76,#11,#11,#17,#61,#11,#17,#61,#68,#88,#86,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#11,#68,#88,#88,#86,#11,#68,#88,#88,#61,#11,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#68,#88,#86,#11,#11,#11,#11,#11,#11,#11,#11,#11
db #11,#61,#11,#16,#61,#11,#11,#11,#11,#11,#11,#11,#61,#11,#76,#66,#11,#16,#11,#76,#11,#11,#17,#61,#11,#76,#11,#11,#17,#61,#68,#88,#86,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#11,#68,#88,#88,#61,#11,#16,#88,#88,#86,#11,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#68,#88,#86,#11,#11,#11,#11,#11,#11,#11,#11,#11
db #17,#61,#11,#67,#11,#11,#11,#11,#11,#11,#11,#11,#67,#11,#16,#61,#11,#76,#11,#76,#11,#11,#11,#76,#17,#61,#11,#11,#17,#61,#68,#88,#86,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#16,#88,#88,#88,#61,#11,#16,#88,#88,#88,#61,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#68,#88,#86,#11,#11,#11,#11,#11,#11,#11,#11,#11
db #16,#66,#66,#71,#11,#11,#11,#11,#76,#66,#67,#11,#76,#11,#11,#11,#11,#61,#11,#76,#11,#11,#11,#17,#66,#11,#11,#11,#17,#61,#68,#88,#86,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#68,#88,#88,#86,#11,#11,#11,#68,#88,#88,#61,#11,#11,#68,#88,#88,#61,#11,#11,#11,#11,#11,#11,#68,#88,#86,#11,#11,#11,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#11,#11,#11,#17,#61,#11,#67,#11,#16,#11,#11,#11,#17,#61,#11,#76,#11,#11,#11,#17,#66,#11,#11,#11,#17,#61,#66,#66,#66,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#66,#66,#66,#61,#11,#11,#11,#16,#88,#88,#86,#11,#11,#16,#66,#66,#61,#11,#11,#11,#11,#11,#11,#66,#66,#66,#11,#11,#11,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#11,#11,#11,#76,#11,#17,#61,#11,#16,#61,#11,#11,#16,#71,#11,#76,#11,#16,#66,#66,#76,#66,#66,#11,#17,#61,#11,#11,#11,#11,#11,#11,#11,#11,#11
db #11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#16,#88,#88,#86,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11
db #17,#66,#66,#66,#66,#66,#66,#61,#11,#16,#71,#11,#11,#61,#11,#11,#16,#11,#11,#76,#11,#16,#71,#11,#11,#11,#16,#11,#17,#61,#16,#66,#66,#66,#66,#66,#66,#66,#66
db #66,#66,#61,#16,#66,#66,#66,#66,#66,#66,#66,#66,#88,#88,#88,#61,#11,#16,#66,#66,#66,#66,#66,#66,#66,#66,#66,#16,#66,#66,#66,#66,#66,#66,#66,#66,#66,#66,#61
db #16,#67,#77,#77,#77,#77,#11,#11,#11,#67,#11,#11,#11,#67,#11,#11,#66,#11,#11,#76,#11,#16,#71,#11,#11,#11,#16,#11,#17,#61,#16,#88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#61,#68,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#86,#11,#68,#88,#88,#88,#88,#88,#88,#88,#88,#86,#16,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#61
db #16,#71,#11,#11,#11,#11,#11,#11,#16,#71,#11,#11,#11,#67,#11,#11,#66,#11,#11,#76,#11,#16,#71,#11,#11,#11,#16,#11,#17,#61,#11,#68,#88,#88,#88,#88,#88,#88,#88
db #88,#86,#16,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#86,#11,#68,#88,#88,#88,#88,#88,#88,#88,#88,#61,#11,#68,#88,#88,#88,#88,#88,#88,#88,#88,#86,#11
db #16,#71,#11,#11,#11,#11,#11,#76,#67,#11,#11,#11,#11,#67,#11,#11,#66,#11,#11,#76,#11,#16,#71,#11,#11,#11,#16,#11,#17,#61,#11,#16,#88,#88,#88,#88,#88,#88,#88
db #88,#86,#16,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#61,#68,#88,#88,#88,#88,#88,#88,#88,#88,#11,#11,#16,#88,#88,#88,#88,#88,#88,#88,#88,#86,#11
db #17,#66,#66,#66,#66,#66,#66,#67,#11,#11,#11,#11,#11,#76,#66,#66,#67,#11,#11,#16,#66,#66,#71,#11,#11,#11,#16,#66,#66,#61,#11,#11,#66,#88,#88,#88,#88,#88,#88
db #88,#61,#68,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#61,#68,#88,#88,#88,#88,#88,#88,#88,#86,#11,#11,#11,#66,#88,#88,#88,#88,#88,#88,#88,#61,#11
db #11,#77,#77,#77,#77,#77,#71,#11,#11,#11,#11,#11,#11,#17,#77,#77,#71,#11,#11,#17,#77,#77,#11,#11,#11,#11,#11,#77,#77,#11,#11,#11,#11,#66,#66,#66,#66,#66,#66
db #66,#11,#66,#66,#66,#66,#66,#66,#66,#66,#66,#66,#66,#66,#66,#66,#61,#66,#66,#66,#66,#66,#66,#66,#66,#11,#11,#11,#11,#11,#66,#66,#66,#66,#66,#66,#66,#11,#11

txtbot1 db "member of SymbOS office suite",0
txtbot2 db "the professional spreadsheet application",0
txtbot3 db "(c) 20":dw ver_app_year:db " SymbiosiS",0
txtbot4 db "Loading v",ver_app_maj+"0",".",ver_app_min+"0","...",0

botlog0


;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;### TRANSFER AREA ############################################################
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

App_BegTrns

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#1d,#1d,#1d,#11,#11,#11,#11,#11,#11,#11,#d1,#d1,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#1d,#dd,#dd,#dd,#dd,#dd,#dd,#dd,#dd,#dd,#dd,#d1
db #1d,#d1,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#1d,#df,#1f,#11,#ff,#11,#11,#1f,#f1,#11,#11,#11,#1d,#d1,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#1d,#d1,#11,#11,#1f,#11,#11,#11,#f1,#11,#11,#11
db #1d,#d1,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#1d,#d1,#18,#11,#88,#18,#18,#18,#81,#11,#88,#11,#1d,#d1,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#1d,#d3,#33,#33,#33,#33,#33,#33,#33,#33,#33,#11
db #1d,#d1,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#1d,#d1,#88,#11,#61,#61,#16,#16,#61,#11,#66,#11,#1d,#d1,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#1d,#d1,#88,#11,#61,#61,#17,#77,#71,#11,#16,#11
db #1d,#d1,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#1d,#d8,#88,#11,#61,#61,#16,#16,#71,#11,#11,#11,#1d,#d1,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#1d,#d8,#88,#11,#61,#61,#16,#16,#71,#11,#11,#11
db #1d,#d1,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#1d,#d1,#1a,#1a,#11,#11,#11,#11,#11,#1a,#1a,#a1,#1d,#d1,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11

;### PRGPRZS -> Stack for application process
        ds 128
prgstk  ds 6*2
        dw prgprz
App_PrcID  db 0
App_MsgBuf ds 14

ib1msg  dw #1234    ;message for extended module 1
        dw pt1tab
        dw pt1bnk
        db pt1bnk0-pt1bnk/2
ib1msgb db 0
        dw winmaidat
        dw ibkstk
        dw ibkloc

ib2msg  dw #1234    ;message for extended module 2
        dw pt2tab
        dw pt2bnk
        db pt2bnk0-pt2bnk/2
ib2msgb db 0
        dw winmaidat
        dw ibkstk
        dw ibkloc

        ds 64       ;stack for calls from an extended module
ibkstk  dw 0

read"App-SymCalc-Forms.asm"

list
memsumrec   equ celrecmax*celreclen
memsumtxt   equ celtxtmax
memsumdat   equ celdatmax
memsumctr   equ ctrcelmax*22

memsumall   equ memsumrec+memsumtxt+memsumdat+memsumctr
nolist

prgtrnend
