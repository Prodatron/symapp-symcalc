;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               S y m C a l c                                @
;@       (extend 1 - cell hash table, dialogues, config, import/export)       @
;@                                                                            @
;@             (c) 2024-2024 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- PROGRAM ------------------------------------------------------------------
;### PRGPTC -> patches pointer in main module

;--- INTERBANK HANDLING -------------------------------------------------------
;### IBKINI -> inits interbank calls
;### IBKCLL -> call routine in main module
;### IBKGET -> copies data from main module
;### IBKPUT -> copies data to main module

;--- HASH ROUTINES ------------------------------------------------------------
;### HSHINI -> inits hash table
;### HSHADR -> returns the hash table address
;### HSHNEW -> adds a hash entry for a cell
;### HSHREM -> removes a hash entry for a cell
;### HSHUPD -> changes cell address
;### HSHFND -> check if there is a cell at field position

;--- GUI ----------------------------------------------------------------------
;### WINPOS -> gets main window position
;### WINBLR0 -> close drowdown window
;### WINDRP -> opens dropdown window
;### WINDIA -> opens dialogue window and set it as modal
;### WINCNC -> cancels dialogue window
;### WINCLS -> closes dialogue window
;### WINTAB -> tab clicked
;### GUIRET -> returns from GUI routine
;### GUIJMP -> executes a GUI routine

;--- BAR ROUTINES -------------------------------------------------------------
;### BARMEM -> inits column/row bar memory and control data
;### BARINI -> init bars
;### BARGEN -> generates a row/column bar
;### BARTIT -> generate bar letter title
;### BARPOS -> calculates bar parameters for actual scroll position
;### BARMOV -> check, if cell field has a different scroll position
;### BAROFS -> sets scroll offset for both bars
;### BARSET -> scroll column or row bar
;### BARSDI -> opens clm/row size dialogue
;### BARMEN -> context menu for column bar
;### CELMEN -> context menu for cells

;--- CELL FORMAT DIALOGUE -----------------------------------------------------
;### CFDOPN -> open cell format window
;### CFGPEN/PAP -> pen/paper selection
;### CFDMOD -> modify num tab for different types
;### CFDPRV -> update preview
;### CFDFMT -> generate cell format data from dialogue
;### CFDOKY -> "ok" clicked
;### CFDTAB  -> main tab clicked
;### FMTVAL -> get cell format values
;### FMTCOL -> open colour dropdown
;### FMTCTB -> colour dropdown pen/paper tab clicked
;### FMTCTB -> colour dropdown colour clicked

;--- CONFIG ROUTINES ----------------------------------------------------------
;### CFGMEM -> loads and inits config
;### CFGSAV -> saves config to file
;### CFGGET -> get config copy from 1st bank
;### CFGPUT -> put config copy to 1st bank

;--- CONFIG LIST ROUTINES -----------------------------------------------------
;### LSTBLD -> builds unit/format list
;### LSTBOL -> builds boolean list from config
;### LSTUNI -> builds unit list from config
;### LSTTIM -> builds time format list from config
;### LSTMSK -> converts datetime mask to format item

;--- DOCUMENT PROPERTIES ROUTINES ---------------------------------------------
;### PRPSTA -> prepares stats
;### PRPOPN -> opens document properties dialogue
;### PRPTAB -> document properties tab pressed
;### PRPCAT -> category changed
;### PRPGET -> loads from list/config into editor
;### PRPPUT -> saves from editor into config
;### PRPTYP -> unit/format list clicked
;### PRPUP  -> move list entry up
;### PRPDWN -> move list entry down
;### PRPOKY -> saves document properties
;### PRPDEF -> sets default document config and clears meta data
;### PRPSAV -> saves meta data into opened file
;### PRPLOD -> loads meta data from opened file
;### PRPUNC -> unpacks meta data
;### PRPCPR -> packs meta data

;--- EXCHANGE (IMPORT/EXPORT) FILEFORMATS -------------------------------------
;### XCHBFR -> reset buffer
;### XCHDEC -> dialogue for CSV format export
;### XCHDIC -> dialogue for CSV format import
;### XCHBRW -> browses for a file
;### XCHEXC -> exports CSV
;### XCHLIM -> check, if cell content contains field limiter
;### XCHLMG -> get delimiters
;### XCHWRT -> writes into file output buffer
;### XCHERR -> error during import/export
;### XCHIPC -> imports CSV from command line parameter
;### XCHIMC -> imports CSV
;### XCHIMS -> imports SYLK
;### XCHRED -> read next field from file
;### XCHLFD -> (get char and) check for linefeed
;### XCHCHR -> read char from file
;### XCHEXS -> exports SYLK
;### XCHSFL -> starts new field in sylk record
;### XCHSNM -> adds decimal number to sylk record
;### XCHSEC -> escape sylk text (replaces ";" with ";;")
;### XCHSUC -> unescape sylk text (replaces ";;" with ";")

;--- SUB ROUTINES -------------------------------------------------------------
;### PRGFIL -> Generates config file path
;### STRLEN -> get string length
;### STRINI -> inits string input control
;### CHRTRM -> check for terminator
;### CHRUCS -> uppercase
;### CNV08S -> converts 8bit to decimal string
;### CNV32S -> Converts 32Bit-number (unsigned) to string (terminated by 0)
;### CNVS16 -> converts string into number
;### LSTPUT -> select list entry by value
;### LSTGET -> get value from list
;### FLDWDT -> get cell width table from 1st bank
;### FLDDIM -> calculates cell begin and width

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

winmairec_field equ 10

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;### CODE AREA ################################################################
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;--- MEMORY INFO --------------------------------------------------------------

ptctab
dw 0
dw 0
dw 0
dw App_BegCode  ;adr code
dw App_BegData  ;adr data
dw App_BegTrns  ;adr trns
read"App-SymCalc-Patch1.asm"
ptctab0


;==============================================================================
;### PROGRAM ##################################################################
;==============================================================================

prgprz  call prgfil
        ld ix,(App_PrcID)           ;check for messages
        ld ixh,-1
        ld iy,App_MsgBuf
        rst #08
        dec ixl
        jr nz,prgprz
        ld hl,(App_MsgBuf+0)        ;check, if correct message from main module
        ld a,l
        or a
        jp z,prgend0
        ld de,-#1234
        adc hl,de
        jr nz,prgprz

        push ix
        ld hl,App_BegCode           ;add len-infos to patch table
        ld de,ptctab
        ld bc,3*2
        ldir
        ld hl,(App_MsgBuf+2)        ;patch main module
        ld de,(App_MsgBuf+4)
        ld bc,(App_MsgBuf+6)
        ld a,b
        ld (winmaidat_bnk),a
        call prgptc
        ld hl,(App_MsgBuf+8)
        ld (winmaidat_adr),hl
        call ibkini
        pop ix

        ld hl,#4321                 ;send confirmation to main module
        ld (App_MsgBuf+0),hl
        ld a,(App_PrcID)
        ld ixl,a
        ld a,ixh
        ld (App_PrcID),a
        ld iy,App_MsgBuf
        rst #10
        rst #30                     ;wait, as message has to be received before removing this process
        rst #30

        ld hl,0
        ld (App_BegCode+0),hl
        ld (App_BegCode+2),hl
        ld (App_BegCode+4),hl

prgend0 ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend1 rst #30
        jr prgend1

;### PRGPTC -> patches pointer in main module
;### Input      HL=table of pointers, DE=table of banknumbers, C=banknumbers count, B=main module bank
;### Destroyed  AF,BC,DE,HL,IX,IY
prgptc  push bc
        push de
        ld ix,ptctab
        ld iyl,ptctab0-ptctab/2
        ld a,b
prgptc1 rst #20:dw jmp_bnkrwd
        push hl
        ld l,c:ld h,b
        ld c,(ix+0)
        ld b,(ix+1)
        rst #20:dw jmp_bnkwwd
        pop hl
        inc ix:inc ix
        dec iyl
        jr nz,prgptc1
        pop hl
        pop iy
prgptc2 rst #20:dw jmp_bnkrwd
        push hl
        ld l,c:ld h,b
        ld bc,(App_BnkNum-1)
        rst #20:dw jmp_bnkwbt
        pop hl
        dec iyl
        jr nz,prgptc2
        ret

;==============================================================================
;### INTERBANK HANDLING #######################################################
;==============================================================================

;### IBKINI -> inits interbank calls
;### Input      HL=winmaidat address
ibkini  ld bc,36
        add hl,bc
        ld a,(winmaidat_bnk)
        rst #20:dw jmp_bnkrwd
        ld l,c:ld h,b
        ld (winmaigrp_adr),hl
        inc hl:inc hl
        rst #20:dw jmp_bnkrwd
        ld (winmairec_adr),bc

        ld hl,(App_MsgBuf+10)
        ld (ibkclls+2),hl
        ld hl,(App_MsgBuf+12)
        ld (ibkclla+2),hl
        ld de,-12
        add hl,de
        ld (ibkcllr+1),hl
        ld a,(App_MsgBuf+7)
        ld (ibkcllb+1),a
        add a:add a:add a:add a
        ld hl,App_BnkNum
        add (hl)
        ld (ibkcllx+1),a
        ret

ibkreg  ds 12   ;AF,BC,DE,HL,IX,IY

;### IBKCLL -> call routine in main module
;### Input      ((SP))=routine ID, all registers
;### Output     all registers
ibkcll  ld (ibkreg+2),bc
        ld (ibkreg+4),de
        ld (ibkreg+6),hl
        ld (ibkreg+8),ix
        ld (ibkreg+10),iy
        push af:pop hl
        ld (ibkreg+0),hl
        ld hl,ibkreg
ibkcllr ld de,0
ibkcllx ld a,0
        ld bc,12
        rst #20:dw jmp_bnkcop
        pop hl
        ld c,(hl):inc hl
        ld b,(hl):inc hl
        push hl
        ld l,c:ld h,b
ibkclla ld ix,0
ibkcllb ld b,0
ibkclls ld iy,0
        call jmp_bnkcll
        ld hl,(ibkcllr+1)
        ld de,ibkreg
        ld bc,12
        call ibkget
        ld hl,(ibkreg+0)
        push hl:pop af
        ld bc,(ibkreg+2)
        ld de,(ibkreg+4)
        ld hl,(ibkreg+6)
        ld ix,(ibkreg+8)
        ld iy,(ibkreg+10)
        ret

;### IBKGET -> copies data from main module
;### Input      HL=source in main module, DE=local destination, BC=length
ibkget  ld a,(ibkcllx+1)
        rrca:rrca:rrca:rrca
ibkget1 rst #20:dw jmp_bnkcop
        ret

;### IBKPUT -> copies data to main module
;### Input      HL=local source, DE=destination in main module, BC=length
ibkput  ld a,(ibkcllx+1)
        jr ibkget1

;*** routines IDs ***
celbld_ equ 00
filbrw_ equ 02
movopt_ equ 04      ;HL=beg, DE=end -> HL=beg optimized, BC=end optimized
celget_ equ 06      ;L,H=cell, E=type (+1=as displayed, +2=formula) -> (celcnvtxt)=text, BC=length (with 0term), HL=celcnvtxt
celput_ equ 08      ;L,H=cell, tmpbuf=string, C=length (with 0term) -> CF=1 memory full
celimp_ equ 10      ;A=type (0=prepare, 1=finish; redraw cells) -> HL=tmpbuf


;==============================================================================
;### HASH ROUTINES ############################################################
;==============================================================================

hshfre  dw hshbkt+2 ;first entry+2 to search for a free one

;### HSHINI -> inits hash table
;### Destroyed  F,BC,DE,HL
hshini  ld hl,hshtab
        ld a,h
        ld (hshadr1+1),a
        ld bc,3*256-1
        call hshini1
        ld hl,hshbkt
        inc hl:inc hl
        ld (hshfre),hl
        dec hl:dec hl
        ld bc,2048*6-1
        call hshini1
        jp jmp_bnkret
hshini1 ld e,l:ld d,h
        inc de
        ld (hl),0
        ldir
        ret

;### HSHADR -> returns the hash table address
;### Input      L,H=column/row
;### Output     C,B=column/row, HL=hash table address
;### Destroyed  AF
hshadr  ld c,l:ld b,h
        ld a,l              ;1
        rrca:rrca:rrca:rrca ;4
        and #f0             ;2
        ld l,a              ;1
        ld a,h              ;1
        and #0f             ;2
        or l                ;1
        ld l,a              ;1  l=hash value
hshadr1 ld h,0              ;2
        ret

;### HSHNEW -> adds a hash entry for a cell
;### Input      L,H=column/row, DE=cell adr
;### Destroyed  AF,BC,DE,HL
hshnew  push de
        push hl
        ld hl,(hshfre)          ;hl=first position in buckets to search
        ld de,6-1
hshnew1 ld a,(hl)       ;2
        inc hl          ;2
        or (hl)         ;2
        jr z,hshnew2    ;2
        add hl,de       ;3
        jr hshnew1      ;3 14
hshnew2 dec hl:dec hl:dec hl    ;hl=free bucket slot
        ex (sp),hl
        call hshadr             ;hl=hash table address, bc=clm/row
        ld a,(hl)
        or a
        jr nz,hshnew4
        inc (hl)                ;bucket was empty -> store bucket slot as first entry
        pop de                  ;de=bucket slot
        inc h
        ld (hl),e
        inc h
hshnew3 ld (hl),d
        ex de,hl                ;hl=bucket slot
        ld (hl),c:inc hl
        ld (hl),b:inc hl
        pop de                  ;de=cell adr
        ld (hl),e:inc hl
        ld (hl),d               ;store cell adr in bucket slot
        ld de,6-3+2
        add hl,de
        ld (hshfre),hl
        jp jmp_bnkret
hshnew4 inc (hl)
        inc h:ld e,(hl)
        inc h:ld d,(hl)
hshnew5 ld hl,4
        add hl,de               ;hl=bucket entry pointer
        dec a
        jr z,hshnew6
        ld e,(hl):inc hl
        ld d,(hl)
        jr hshnew5
hshnew6 pop de
        ld (hl),e
        inc hl
        jr hshnew3

;### HSHREM -> removes a hash entry for a cell
;### Input      L,H=column/row
;### Destroyed  AF,BC,DE,HL
hshrem  call hshadr
        ld a,(hl)
        dec a
        ld (hl),a
        inc h
        ld e,(hl)
        inc h
        ld d,(hl)
        ex de,hl                    ;hl=first bucket entry
hshrem1 or a
        jr nz,hshrem3
        inc hl:inc hl               ;only one entry for this hash -> clear it, no pointer stuff
        ld (hl),a:inc hl
        ld (hl),a:dec hl
hshrem2 ex de,hl
        ld hl,(hshfre)
        sbc hl,de                   ;always cf=0 here
        jp c,jmp_bnkret
        ld (hshfre),de
        jp jmp_bnkret

hshrem3 call hshrem6
        jr nz,hshrem4
        ld c,(hl):inc hl            ;cell is first in bucket
        ld b,(hl)                   ;bc=pointer to next bucket entry
        ex de,hl
        ld (hl),b
        dec h
        ld (hl),c                   ;write pointer to hashtable
        ex de,hl
hshrem8 dec hl:dec hl
        ld (hl),0:dec hl            ;mark bucket entry as free
        ld (hl),0
        jr hshrem2                  ;adjust "search for free"

hshrem4 ld (hshrem5+2),hl           ;store last pointer address
        ld e,(hl):inc hl
        ld d,(hl)
        ex de,hl
        call hshrem6
        jr nz,hshrem4
        ld e,(hl):inc hl            ;bucket entry with cell found
        ld d,(hl)
hshrem5 ld (0),de                   ;copy its next-pointer to previous entry
        jr hshrem8

hshrem6 ld a,(hl):inc hl
        cp c
        ld a,(hl):inc hl
        inc hl:inc hl
        ret nz
        cp b
        ret

;### HSHUPD -> changes cell address
;### Input      L,H=column/row, DE=cell adr
;### Destroyed  AF,BC,DE,HL
hshupd  ld (hshupd3+1),de
        call hshadr
        inc h
        ld a,(hl)
        inc h
        ld h,(hl)
        ld l,a
        ld de,4-1
hshupd1 ld a,(hl):inc hl
        cp c
        jr nz,hshupd2
        ld a,(hl)
        cp b
        jr z,hshupd3
hshupd2 add hl,de
        ld a,(hl):inc hl
        ld h,(hl):ld l,a
        jr hshupd1
hshupd3 ld de,0
        inc hl:ld (hl),e
        inc hl:ld (hl),d
        jp jmp_bnkret

;### HSHFND -> check if there is a cell at field position
;### Input      L,H=column/row
;### Output     E=0 -> no cell, HL=0
;###            E=1 -> HL=cell data record
;### Destroyed  AF,BC,D,IX
hshfnd  call hshadr
        ld a,(hl)           ;2
        or a                ;1
        jr z,hshfnd4        ;2
        ld ixl,a            ;       ixl=bucket size (##!!## remove after tests)
        inc h               ;1
        ld a,(hl)           ;2
        inc h               ;1
        ld h,(hl)           ;2
        ld l,a              ;1      hl=adr of first bucket entry
        ld de,4-1           ;3 34

hshfnd1 ld a,(hl):inc hl    ;4
        cp c                ;1
        jr nz,hshfnd2       ;2/3 -> 7/8
        ld a,(hl)           ;2
        cp b                ;1
        jr z,hshfnd3        ;2/3 -> 5/6
hshfnd2 dec ixl             ;       (##!!## remove after tests)
        jr z,hshfnd4        ;       (##!!## exception call for integrity tests)
        add hl,de           ;3
        ld a,(hl):inc hl    ;4
        ld h,(hl):ld l,a    ;3
        jr hshfnd1          ;3 13+8/17+7+5->25/29-> 23 (not found)

hshfnd3 inc hl:ld e,(hl)    ;4
        inc hl:ld d,(hl)    ;4 13+8-> 21 (found) -> average 34+2.5*23 = ca 89 (1,5 rasterlines)
        ex de,hl
        ld e,1
        jp jmp_bnkret
hshfnd4 ld e,0
        ld l,e:ld h,e
        jp jmp_bnkret


;==============================================================================
;### GUI ######################################################################
;==============================================================================

windianum       db 0
winmaidat_adr   dw 0
winmaidat_bnk   db 0
winmaigrp_adr   dw 0
winmairec_adr   dw 0

winmaibuf       ds 8
windrpnum       db 0

;### WINPOS -> gets main window position
;### Output     HL=xpos, DE=ypos
;### Destroyed  AF,BC
winpos  ld hl,(winmaidat_adr)
        ld de,winmaibuf
        ld bc,8
        call ibkget
        ld a,(winmaibuf+0)
        ld hl,0
        ld de,0
        cp 2
        ret z
        ld hl,(winmaibuf+4)
        inc hl
        ld de,(winmaibuf+6)
        inc de
        ret

;### WINBLR0 -> close drowdown window
winblr0 ld de,-1
        xor a
        call windia2
        ld a,(windrpnum)
        jp SyDesktop_WINCLS

;### WINDRP -> opens dropdown window
;### Input      IX=window record, BC=xofs, DE=yofs
windrp  push de
        push bc
        call winpos
        pop bc
        add hl,bc
        ld (ix+4),l
        ld (ix+5),h
        pop hl
        add hl,de
        ld (ix+6),l
        ld (ix+7),h
        push ix:pop de
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN
        jp c,guiret0
        ld (windrpnum),a
        ld de,-1
        push af
        call windia2
        pop af
        jr windia1

;### WINDIA -> opens dialogue window and set it as modal
;### Input      DE=window record
windia  ld a,(App_BnkNum)
        call SyDesktop_WINOPN
        jp c,guiret0
        ld (windianum),a
windia1 ld de,51
        inc a
        call windia2
        jp guiret0
windia2 ld b,a
        ld hl,(winmaidat_adr)
        add hl,de
        ld a,(winmaidat_bnk)
        rst #20:dw jmp_bnkwbt
        ret

;### WINCNC -> cancels dialogue window
wincnc  call wincls
        jp guiret0

;### WINCLS -> closes dialogue window
wincls  ld a,(windianum)
        jp SyDesktop_WINCLS

;### WINTAB -> tab clicked
;### Input      A=new tab, HL=tab table, DE=group record
wintab  cp (hl)
        jp z,guiret0
        ld (hl),a
        ld c,a
        add a
        add c
        inc a
        ld c,a
        ld b,0
        add hl,bc
        ldi
        inc de
        ldi:ldi
        ld e,-1
        ld a,(windianum)
        call SyDesktop_WININH
        jp guiret0

;### GUIRET -> returns from GUI routine
;###  Input     HL=ret ID
guiret0 ld hl,0
guiret  jp jmp_bnkret

;### GUIJMP -> executes a GUI routine
;### Input      L=ID, DE,H=register
guijmp  ld a,h
        ld h,0
        add hl,hl
        ld bc,guitab-2
        add hl,bc
        ld c,(hl):inc hl
        ld h,(hl):ld l,c
        jp (hl)

guitab
dw cfdopn :_cfdopn  equ 01
dw cfdtab :_cfdtab  equ 02
dw cfdoky :_cfdoky  equ 03
dw cfdmod :_cfdmod  equ 04
dw cfdprv :_cfdprv  equ 05
dw cfdpen :_cfdpen  equ 06
dw cfdpap :_cfdpap  equ 07
dw wincnc :_wincnc  equ 08
dw barsdia:_barsdia equ 09
dw barsdib:_barsdib equ 10
dw 0                  ; 11
dw 0                  ; 12
dw fmtces :_fmtces  equ 13
dw fmtcol :_fmtcol  equ 14
dw fmtctb :_fmtctb  equ 15
dw fmtc00 :_fmtc00  equ 16
dw fmtc01 :_fmtc01  equ 17
dw fmtc02 :_fmtc02  equ 18
dw fmtc03 :_fmtc03  equ 19
dw fmtc04 :_fmtc04  equ 20
dw fmtc05 :_fmtc05  equ 21
dw fmtc06 :_fmtc06  equ 22
dw fmtc07 :_fmtc07  equ 23
dw fmtc08 :_fmtc08  equ 24
dw fmtc09 :_fmtc09  equ 25
dw fmtc10 :_fmtc10  equ 26
dw fmtc11 :_fmtc11  equ 27
dw fmtc12 :_fmtc12  equ 28
dw fmtc13 :_fmtc13  equ 29
dw fmtc14 :_fmtc14  equ 30
dw fmtc15 :_fmtc15  equ 31
dw prpopn :_prpopn  equ 32
dw prptab :_prptab  equ 33
dw prpoky :_prpoky  equ 34
dw prpcat :_prpcat  equ 35
dw prptyp :_prptyp  equ 36
dw prpup  :_prpup   equ 37
dw prpdwn :_prpdwn  equ 38
dw xchdec :_xchdec  equ 39
dw xchdic :_xchdic  equ 40
dw xchbrw :_xchbrw  equ 41
dw xchexc :_xchexc  equ 42
dw xchimc :_xchimc  equ 43
dw xchipc :_xchipc  equ 44
dw xchexs :_xchexs  equ 45
dw xchims :_xchims  equ 46
dw cfgopn :_cfgopn  equ 47
dw cfgoky :_cfgoky  equ 48
dw cfgtab :_cfgtab  equ 49
dw cfgreg :_cfgreg  equ 50
dw cfgctp :_cfgctp  equ 51
dw cfgcol :_cfgcol  equ 52
dw prpcol :_prpcol  equ 53
dw prpctp :_prpctp  equ 54


;==============================================================================
;### BAR ROUTINES #############################################################
;==============================================================================

barclmmax   equ 16
barrowmax   equ 32

barmemcnt   equ barclmmax+barrowmax
barmemadd   equ 40*barmemcnt+32     ;16+32*elements+2*4*elements for each

barmemclm   dw 0,0,0 ;adr supclmrec,ctrclm,txtclm
barmemrow   dw 0,0,0 ;adr suprowrec,ctrrow,txtrow

;### BARMEM -> inits column/row bar memory and control data
;### Output     DE=supclmctr, HL=suprowctr
barmemrec
dw     00,255*256+00,2,     0,0,10000,10000,0
dw     00,255*256+02,12+0      ,  0,0,40,10,0
dw     00,255*256+01,00000000  ,  1,1,38, 8,0

barmem  ld hl,bardat
        ld (supclmgrp+2),hl     ;supclmrec
        ld (barmemclm+0),hl
        ld bc,32*barclmmax+16
        add hl,bc
        ld (suprowgrp+2),hl     ;suprowrec
        ld (barmemrow+0),hl
        ld bc,32*barrowmax+16
        add hl,bc
        ld (barmemclm+2),hl     ;ctrclm
        ld bc,4*barclmmax
        add hl,bc
        ld (barmemclm+4),hl     ;txtclm
        add hl,bc
        ld (barmemrow+2),hl     ;ctrrow
        ld bc,4*barrowmax
        add hl,bc
        ld (barmemrow+4),hl     ;txtrow
        ld hl,0:ld (barmemrec+16+08),hl
        inc l  :ld (barmemrec+32+08),hl
        ld l,10:ld (barmemrec+16+12),hl
        ld l, 8:ld (barmemrec+32+12),hl
        ld ix,barmemclm
        ld iyl,barclmmax
        call barmem1
        ld hl,0:ld (barmemrec+16+06),hl
        inc l  :ld (barmemrec+32+06),hl
        ld l,20:ld (barmemrec+16+10),hl
        ld l,18:ld (barmemrec+32+10),hl
        ld ix,barmemrow
        ld iyl,barrowmax
        call barmem1
        ld de,supclmctr
        ld hl,suprowctr
        jp jmp_bnkret
barmem1 ld e,(ix+0)             ;copy background as first record
        ld d,(ix+1)
        ld hl,barmemrec
        ld bc,16
        ldir
        ld l,(ix+2)
        ld h,(ix+3)
        push hl
        ld iyh,iyl
barmem2 ld (barmemrec+32+4),hl  ;set control adr
        push hl
        ld hl,barmemrec+16
        ld c,32
        ldir                    ;copy frame/text records
        pop hl
        ld c,4
        add hl,bc               ;increase control adr
        dec iyh
        jr nz,barmem2
        pop hl
        ld e,(ix+4)
        ld d,(ix+5)
        ld c,4
barmem3 ld (hl),e  :inc hl      ;set text adr
        ld (hl),d  :inc hl
        ld (hl),2+4:inc hl      ;init control parameters
        ld (hl),34 :inc hl
        ex de,hl
        add hl,bc
        ex de,hl
        dec iyl
        jr nz,barmem3
        ret

;### BARINI -> init bars
;### Input      HL=width table
barini0 ld (flddimx),hl
        ld de,256*0
        call bargen
        jp jmp_bnkret
barini1 ld (flddimy),hl
        ld de,256*1
        call bargen
        jp jmp_bnkret

;### BARGEN -> generates a row/column bar
;### Input      D=type (0=column, 1=row), E=offset, HL=width table
bargentyp   db 0
bargenofs   db 0
bargencnt   db 0

barparbeg   equ 0   ;bar beg  in pixels
barparsiz   equ 2   ;bar size in pixels

barparlen   equ 4

barparclm   ds barparlen
barparrow   ds barparlen

bargen  push de
        call fldwdt
        pop bc
bargen0 ld a,b
        or a
        ld b,barclmmax
        ld hl,bartit
        ld iy,barmemclm
        ld de,supclmgrp
        ld ix,barparclm
        jr z,bargen5
        ld b,barrowmax
        ld hl,cnv08s
        ld iy,barmemrow
        ld de,suprowgrp
        ld ix,barparrow
bargen5 push ix                 ;A=type (0=cols, 1=rows), C=offset, B=number of controls (multiple of 8), HL=text routine, IY=memory areas (+0 form, +2 control data, +4 text), DE=super record, IX=bar parameters
        ld (bargentyp),a
        ld a,b
        add a
        inc a
        ld (de),a               ;set number of controls
        ld (bargenofs),bc
        ld (bargen2+1),hl
        ld l,(iy+4)
        ld h,(iy+5)
bargen1 inc c
        push bc
        push hl
        ld a,c
bargen2 call 0
        ld (hl),0
        pop hl
        ld bc,4
        add hl,bc
        pop bc
        djnz bargen1
        ld l,(iy+0)
        ld h,(iy+1)
        ld bc,16
        add hl,bc                   ;hl=first record
        ld a,(bargentyp)
        or a
        jr z,bargen3
        inc hl:inc hl
bargen3 ld a,(bargenofs)
        push hl
        call flddim                 ;ix=width table
        ex de,hl                    ;de=begin
        pop iy                      ;iy=records
        push de
        ld a,(bargencnt)
        ld bc,32
bargen4 ld (iy+06+00),e
        ld (iy+07+00),d             ;set beg frame
        inc de
        ld (iy+06+16),e
        ld (iy+07+16),d             ;set beg text
        dec de
        ld l,(ix+0)
        inc ix                      ;ix=next width
        ld h,0
        srl l                       ;hl=width
        ld (iy+10+00),l
        ld (iy+11+00),h             ;set width frame
        ex de,hl
        add hl,de                   ;hl=begin next
        dec de:dec de
        ld (iy+10+16),e
        ld (iy+11+16),d             ;set width text
        ex de,hl
        add iy,bc
        dec a
        jr nz,bargen4
        pop hl                      ;hl=beg first, de=begin next
        pop ix
        ld (ix+barparbeg+0),l
        ld (ix+barparbeg+1),h
        ex de,hl
        or a
        sbc hl,de                   ;de=size
        ld (ix+barparsiz+0),l
        ld (ix+barparsiz+1),h
        ret

;### BARTIT -> generate bar letter title
;### Input      A=pos (1-), HL=string
;### Output     (HL)="AA", HL=behind
bartit  ld c,"A"-3      ;a=pos (1-), hl=str -> generate "AA" position text -> hl=behind
        dec a
bartit1 inc c
        sub 26
        jr nc,bartit1
        bit 6,c
        jr z,bartit2
        inc c
        ld (hl),c
        inc hl
bartit2 add 26+"A"
        ld (hl),a
        inc hl
        ret

;### BARPOS -> calculates bar parameters for actual scroll position
;### Input      flddimt=size table, IY=barpar, A=number of controls (16/32), HL=scroll position, DE=winmairec offset for cell field width/height
;### Output     CF=0 -> ok, inside, CF=1 -> outside, C=new offset
;### Destroyed  AF,BC,DE,HL,IX,IY
barpos  ld (barpos3+1),a
        push hl
        ld hl,(winmairec_adr)
        add hl,de
        ld a,(winmaidat_bnk)
        rst #20:dw jmp_bnkrwd
        ld e,c:ld d,b
        pop hl
        push hl
        ld c,(iy+barparbeg+0)
        ld b,(iy+barparbeg+1)
        xor a
        sbc hl,bc
        jr c,barpos1            ;scroll < barbeg -> outside
        add hl,de
        ld c,(iy+barparsiz+0)
        ld b,(iy+barparsiz+1)
        sbc hl,bc
        jr nc,barpos1           ;scroll+field >= barbeg+barsiz -> outside
        pop hl
        or a
        ret
barpos1 pop hl
        push de
        ld ix,flddimt
        push ix
        call fldmos0            ;a=new offset
        pop ix
        pop hl
        ld c,a                  ;c=new 0-offset
        ld b,4                  ;b=try offset dif
        cp 4
        jr nc,barpos2
        ld b,a
barpos2 ld d,0
        ld e,a
        add ix,de
barpos3 ld a,0
        sub b
barpos4 ld e,(ix+0)
        srl d
        or a
        sbc hl,de
        jr c,barpos5            ;field width < cells width -> use dif
        inc ix
        dec a
        jr nz,barpos4
        scf
        ret                     ;field width >= cells width -> use 0-offset
barpos5 ld a,c
        sub b
        ld c,a
        scf
        ret

;### BARMOV -> check, if cell field has a different scroll position
;### Input      DE=xofs, HL=yofs
;### Output     D=status (1=nothing, 2=xscroll, 3=yscroll)
barmov  ld c,l:ld b,h
        ld hl,(supclmctr+6)
        or a
        sbc hl,de
        jr z,barmov1
        ld (barset1+1),de
        ld d,2
        jp jmp_bnkret
barmov1 ld hl,(suprowctr+8)
        sbc hl,bc
        ld d,1
        jp z,jmp_bnkret
        ld (barset1+1),bc
        ld d,3
        jp jmp_bnkret

;### BAROFS -> sets scroll offset for both bars
;### Input      DE=xofs column bar, HL=yofs row bar
barofs  ld (supclmctr+6),de
        ld (suprowctr+8),hl
        jp jmp_bnkret

;### BARSET -> scroll column or row bar
;### Input      D=type (0=column, 1=row), HL=width table
;### Output     H=focus object, L=key message
barset  push de
        call fldwdt
        pop af
barset1 ld hl,0        
        push hl
        dec a
        jr z,barset3

        ld iy,barparclm     ;** clm/x
        ld a,16
        ld de,16*winmairec_field+10     ;(winmairec1+10)
        call barpos
        jr nc,barset2
        ld b,0
        call bargen0
barset2 pop hl
        ld (supclmctr+12),hl
        ld hl,winmairec_field-1*256+30
        jp jmp_bnkret

barset3 ld iy,barparrow     ;** row/y
        ld a,32
        ld de,16*winmairec_field+12     ;(winmairec1+12)
        call barpos
        jr nc,barset4
        ld b,1
        call bargen0
barset4 pop hl
        ld (suprowctr+12),hl
        ld hl,winmairec_field-0*256+31
        jp jmp_bnkret

;### BARSDI -> opens clm/row size dialogue
;### Input      D=type (0=column, 1=row), E=current width, L=default
barsdit db 1,0  ;terminator

barsdi  ld a,l
        ld (barsdia+1),a
        ld a,e
        dec d
        ld hl,ctxclmtxt1
        ld de,txtsizclm
        jr nz,barsdi1
        ld hl,ctxrowtxt1
        ld de,txtsizclm
barsdi1 ld (winsizdat1),hl      ;open size dialogue
        ld (ctrsizlab),de
        call barsdi2
        ld de,winsizdat
        jp windia

barsdi2 ld hl,txtsizinp         ;init input
        call cnv08s
        ld (hl),0
        ld ix,ctrsizinp
        jp strini

barsdia ld a,0                  ;set to default
        call barsdi2
        ld a,(windianum)
        ld e,winsizrec_inp
        call SyDesktop_WININH
        jp guiret0

barsdib call wincls             ;ok -> use new size
        ld iy,txtsizinp
        ld hl,barsdit
        ld bc,1
        ld de,126
        call cnvs16
        jp c,guiret0
        ex de,hl
        ld l,ret_barsdi
        jp jmp_bnkret

;### BARMEN -> context menu for column bar
;### Input      D=type, E=column
barmen  push de
        dec d
        ld de,ctxclmdat
        jr nz,barmen0
        ld de,ctxrowdat
barmen0 ld a,(App_BnkNum)
        ld hl,-1
        call SyDesktop_MENCTX
        pop de
        jp c,guiret0
        ld d,e
        jp jmp_bnkret

;### CELMEN -> context menu for cells
;### Input      L=(coprngsta)
celmen  ld a,l
        or a
        ld a,16
        jr z,celmen1
        inc a
celmen1 ld (2*8+ctxceldat+2),a
        push de
        ld de,ctxceldat
        jr barmen0


;==============================================================================
;### CELL FORMAT DIALOGUE #####################################################
;==============================================================================

;### CFDOPN -> open cell format window
cfdopn  push de
        call cfgget
        pop de
        call fmtval         ;get cell format
        ld a,(fmtvalaln)
        ld ix,ctrcfdalh     ;set horizontal alignment
        call lstput
        ld a,(fmtvalfnt)
        ld ix,ctrcfdstl     ;set font style
        call lstput
        ld a,(fmtvalspc)
        ld ix,ctrcfddpl     ;set decimal places/digits
        call lstput
        ld a,(fmtvalsep)
        ld (flgcfdsep),a    ;set separator
        ld a,(fmtvaltyp)
        ld ix,ctrcfdcat     ;set type
        push af
        call lstput
        pop af
        call cfdmod1        ;show/hide/modify elements depending on type
        ld a,(fmtvaluni)
        ld ix,ctrcfdtyp     ;set unit/type
        call lstput
        ld a,-1             ;set colours
        ld (wincolrec1+16+6),a
        ld (wincolrec1+16+8),a
        ld ix,wincfdrecc1
        ld hl,(fmtvalcol)
        push hl
        ld de,256*75+14
        call fmtcol3
        ld ix,wincfdrecc2
        pop hl
        ld l,h
        ld de,256*75+101
        call fmtcol3
        call cfdprv1
        ld de,wincfddat     ;open window
        jp windia

;### CFGPAP -> paper selection
cfdpap  ld iy,fmtvalcol+1
        ld ix,wincfdrecc2
        ld a,wincfdrecc_pap
        ld hl,256*75+101
        call cfdpen1
        jp cfdprv

;### CFGPEN -> pen selection
cfdpen  ld iy,fmtvalcol+0
        ld ix,wincfdrecc1
        ld a,wincfdrecc_pen
        ld hl,256*75+14
        call cfdpen1
        jp cfdprv

cfdpen1 push af
        push hl
        ld a,e
        sub l
        call cfdpen2
        ld l,c
        ld a,d
        sub h
        call cfdpen2
        ld a,c
        add a:add a
        add l
        ld (iy+0),a
        ld l,a
        pop de
        call fmtcol3
        pop de
        ld e,-2
        ld a,(windianum)
        jp SyDesktop_WININH
cfdpen2 ld c,0      ;c=a/15
cfdpen3 sub 15
        ret c
        inc c
        jr cfdpen3

;### CFDMOD -> modify num tab for different types
;### Input      (ctrcfdcat...)=new type, (fmtvaltyp)=old type
;[b1]unit[b0]options,1000er,unit/type(ctrcfdtyt),digit/decimal(ctrcfddpt)
cfdmodt db %11,"1":dw txtcfdtyt1,txtcfddpt1,lstcfddpl:db 4 ;decimal
        db %10," ":dw txtcfdtyt1,0         ,lstcfddpl:db 4 ;exponent
        db %01,"1":dw 0         ,txtcfddpt1,lstcfddpl:db 0 ;percentage
        db %10," ":dw txtcfdtyt2,0         ,lstcfddpl:db 1 ;date
        db %10," ":dw txtcfdtyt2,0         ,lstcfddpl:db 2 ;time
        db %10," ":dw txtcfdtyt3,0         ,lstcfddpl:db 3 ;bool
        db %11,"0":dw txtcfdtyt1,txtcfddpt2,lstcfddbl:db 5 ;bin
        db %11,"0":dw txtcfdtyt1,txtcfddpt2,lstcfddxl:db 5 ;hex
        db %00," ":dw 0         ,0         ,lstcfddpl:db 0 ;text
cfdmoda dw wincfdreca1+2,16*1+wincfdreca1+2,16*2+wincfdreca1+2,0
cfdmodb dw wincfdreca2+2,16*1+wincfdreca2+2,16*2+wincfdreca2+2,16*3+wincfdreca2+2,16*4+wincfdreca2+2,0
cfdmodo db -1

cfdmod  ld ix,ctrcfdcat
        call lstget
        ld hl,fmtvaltyp
        cp (hl)
        jp z,guiret0
        ld (hl),a
        call cfdmod1
        ld de,256*wincfdreca_opt2+256-5
        jr z,cfdmod6
        or a
        ld a,0
        ld ix,ctrcfdtyp
        call nz,lstput
        ld de,256*wincfdreca_opt1+256-8
cfdmod6 ld a,(windianum)
        call SyDesktop_WININH
        jr cfdprv
;a=new type -> zf=0 new unit list, a=list type (0=no)
cfdmod1 ld hl,cfdmodt
        ld c,a
        add a:add a:add a
        add c               ;*9
        ld c,a
        ld b,0
        add hl,bc
        bit 1,(hl)
        ld de,cfdmoda
        call cfdmod2
        bit 0,(hl)
        ld de,cfdmodb
        call cfdmod2
        inc hl
        ld de,txtcfdsep
        ldi
        ld de,ctrcfdtyt
        ldi:ldi
        ld de,ctrcfddpt
        ldi:ldi
        ld de,ctrcfddpl+4
        ldi:ldi
        ld a,(hl)
        or a
        push af
        ld c,0
        call nz,lstbld
        pop af
        ld hl,cfdmodo
        cp (hl)
        ld (hl),a
        ret
cfdmod2 push hl
        ex de,hl
        ld c,0
        jr z,cfdmod3
        ld c,64
cfdmod3 call cfdmod5
        ld a,c
        xor 64
        ld c,a
cfdmod4 call cfdmod5
        jr nz,cfdmod4
        pop hl
        ret
cfdmod5 ld e,(hl):inc hl
        ld d,(hl):inc hl
        ld a,e
        or d
        ret z
        ex de,hl
        ld a,(hl)
        and 63
        or c
        ld (hl),a
        ex de,hl
        ret

;### CFDPRV -> update preview
cfdprv  call cfdprv1
        ld de,256*wincfdrec_prev+256-2
        ld a,(windianum)
        call SyDesktop_WININH
        jp guiret0
cfdprv1 call cfdfmt
        ld ix,cfdcelrec
        ld de,wincfdreca0
        ld hl,ctrcfdprv
        call fmtctr0
        ld a,(fmtvalcol+1)
        add 128
        ld (wincfdreca0-16+4),a
        ld (wincfdrecb0-16+4),a
        ld (wincfdrecc0-16+4),a
        ld a,(wincfdreca0+2)
        ld (wincfdrecb0+2),a
        ld (wincfdrecc0+2),a
        ld hl,ctrcfdprv+3
        set 6,(hl)
        ld a,(cfdfmttyp)
        cp celtyptxt
        ld hl,fmtvaltxt         ;text
        jr nc,cfdprv2
        ld bc,(fmtvalvar+0)     ;number
        ld de,(fmtvalvar+2)
        ld a,(fmtvalvar+4)
        ld hl,(cfdfmtdsp)
        call ibkcll:dw celbld_
        ld de,txtcfdprv
        ld bc,40+6
        jp ibkget
cfdprv2 ld de,txtcfdprv
        ld bc,40+6
        ldir
        ret

;### CFDFMT -> generate cell format data from dialogue
cfdcelrec
cfdfmttyp   db 0    ;1=number, 3=text
            ds 8
cfdfmtcol   db 0
cfdfmtdsp   db 0
cfdfmtfmt   db 0

cfdfmt  ld ix,ctrcfdcat     ;get number type
        call lstget
        cp 8
        jr c,cfdfmt1
        xor a               ;** text -> just store main type
        ld (cfdfmtfmt),a
        ld a,3
        ld (cfdfmttyp),a
        jr cfdfmt2
cfdfmt1 or a                ;** number
        rrca:rrca:rrca      ;category
        ld e,a
        ld ix,ctrcfddpl
        call lstget         ;decimal places/digits
        or e
        ld e,a
        ld a,(flgcfdsep)
        add a:add a:add a   ;1000 separator
        or e
        ld (cfdfmtfmt),a    ;-> format
        ld a,1
        ld (cfdfmttyp),a
cfdfmt2 ld a,(ctrcfdtyp+12)
        add a:add a:add a   ;unit/type
        ld e,a
        ld ix,ctrcfdstl
        call lstget         ;font style
        or e
        ld e,a
        ld ix,ctrcfdalh
        call lstget
        rrca:rrca           ;alignment
        or e
        ld (cfdfmtdsp),a    ;-> display
        ld hl,fmtvalcol
        ld a,(hl)
        add a:add a:add a:add a
        inc hl
        add (hl)
        ld (cfdfmtcol),a    ;-> colour
        ret

;### CFDOKY -> "ok" clicked
cfdoky  call wincls
        ld ix,(cfdfmttyp)
        ld a,(cfdfmtcol)
        ld ixh,a
        ld de,(cfdfmtdsp)
        ld l,ret_cfdoky
        jp jmp_bnkret

;### CFDTAB  -> main tab clicked
cfdtabo db 0
cfdtabt db wincfdreca_cnt:dw wincfdreca
        db wincfdrecb_cnt:dw wincfdrecb
        db wincfdrecc_cnt:dw wincfdrecc

cfdtab  ld a,(ctrcfdtab0)
        ld hl,cfdtabo
        ld de,wincfdgrp
        jp wintab


fmtvalcol   db 0,0      ;pen, paper
fmtvaltyp   db 0        ;number type (0-8)
fmtvalaln   db 0        ;alignment (0-3)
fmtvalfnt   db 0        ;font (0-2)
fmtvalsep   db 0        ;separator flag
fmtvaluni   db 0        ;unit (0-7)
fmtvalspc   db 0        ;specification (comma, type, etc.)

fmtvalvar   ds 5        ;original/default value
fmtvaltxt   ds 32       ;original/default text
fmtvaldft   db #85,#eb,#51,#1a,#8b
            db "Hello world",0:fmtvaldft0

fmtvalcel   ds 12   ;cell data record

;### FMTVAL -> get cell format values
;### Input      DE=cell data record/0
;### Output     (fmtval???)=values
;### Destroyed  AF,BC,DE,HL,IX
fmtval  push de
        ld hl,fmtvaldft
        ld de,fmtvalvar
        ld bc,fmtvaldft0-fmtvaldft
        ldir
        ld hl,fmtvaltyp
        ld (hl),0
        ld de,fmtvaltyp+1
        ld bc,fmtvalspc-fmtvaltyp
        ldir
        ld a,7
        ld (fmtvalspc),a
        ld a,1
        ld (fmtvalsep),a
        pop hl              ;HL=cell data record
        ld de,(cfgcolpen)
        ld a,l:or h
        jr z,fmtval1
        ld de,fmtvalcel
        push de
        ld bc,12
        call ibkget
        pop hl
        push hl             ;current/first in multiple -> use cell
        ld de,celrectxt
        add hl,de
        ld a,32
        ld de,fmtvaltxt
        call fmtval2
        ld a,5
        ld de,fmtvalvar
        call fmtval2
        pop hl
        ld c,(hl)           ;c=main type
        ld de,celreccol
        add hl,de
        ld a,(hl)
        and #0f
        ld d,a              ;d=paper
        ld a,(hl)
        and #f0
        rrca:rrca:rrca:rrca
        ld e,a              ;e=pen
        inc hl
        ld a,(hl)
        and #03
        ld (fmtvalfnt),a
        ld a,(hl)
        and #38
        rrca:rrca:rrca
        ld (fmtvaluni),a
        ld a,(hl)
        and #c0
        rlca:rlca
        ld (fmtvalaln),a
        inc hl
        ld a,(hl)
        and #07
        ld (fmtvalspc),a
        ld a,(hl)
        and #e0
        rlca:rlca:rlca
        ld (fmtvaltyp),a
        ld a,(hl)
        and #08
        rrca:rrca:rrca
        ld (fmtvalsep),a
        ld a,c
        cp celtyptxt
        jr c,fmtval1
        ld a,8
        ld (fmtvaltyp),a
fmtval1 ex de,hl
        ld (fmtvalcol),hl
        ret
fmtval2 ld c,(hl):inc hl        ;copy cell data to preview
        ld b,(hl):inc hl
        push hl
        ld l,c:ld h,b
        ld c,a
        ld b,0
        inc hl:inc hl:inc hl
        call ibkget
        pop hl
        ret

;### FMTCOL -> open colour dropdown
fmtcol  call fmtval
        ld hl,(fmtvalcol)
        call fmtcol2
        ld ix,wincoldat
        ld bc,157
        ld de,32
        jp windrp
fmtcol2 ld de,256*15+1
        ld a,(ctrcoltab0)
        ld ix,wincolrec1
        or a
        jr z,fmtcol3
        ld l,h
;l=colour, ix=control record, de=x/yofs
fmtcol3 ld a,(ix+16+6)
        ld (ix+00+6),a
        ld a,(ix+16+8)
        ld (ix+00+8),a
        ld a,l
        call fmtcol4
        add e
        ld (ix+16+6),a
        ld a,l
        rrca:rrca
        call fmtcol4
        add d
        ld (ix+16+8),a
        ret
fmtcol4 and #03
        ld h,a
        add a:add a:add a:add a
        sub h
        ret

;### FMTCTB -> colour dropdown pen/paper tab clicked
fmtctb  ld hl,(fmtvalcol)
        call fmtcol2
        ld a,(windrpnum)
        ld de,256*wincolrec_sel+256-3
        call SyDesktop_WININH
        jp guiret0

;### FMTCTB -> colour dropdown colour clicked
fmtces  call winblr0
        ld e, 0:jr fmtcst1
fmtc00  xor a:  jr fmtcst
fmtc01  ld a,01:jr fmtcst
fmtc02  ld a,02:jr fmtcst
fmtc03  ld a,03:jr fmtcst
fmtc04  ld a,04:jr fmtcst
fmtc05  ld a,05:jr fmtcst
fmtc06  ld a,06:jr fmtcst
fmtc07  ld a,07:jr fmtcst
fmtc08  ld a,08:jr fmtcst
fmtc09  ld a,09:jr fmtcst
fmtc10  ld a,10:jr fmtcst
fmtc11  ld a,11:jr fmtcst
fmtc12  ld a,12:jr fmtcst
fmtc13  ld a,13:jr fmtcst
fmtc14  ld a,14:jr fmtcst
fmtc15  ld a,15
fmtcst  push af
        call winblr0
        pop bc
        ld a,(ctrcoltab0)
        or a
        ld a,b
        ld e,#f0
        jr nz,fmtcst1
        add a:add a:add a:add a
        ld e,#0f
fmtcst1 ld d,a
        ld l,ret_fmtaln
        jp jmp_bnkret

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
;### CONFIG ROUTINES ##########################################################
;==============================================================================

align 2
read"App-SymCalc-Config.asm"

cfgdocdef   ds cfgdocend-cfgdocbeg

;### CFGMEM -> loads and inits config
;### Input      HL=config adr in 1st bank
cfgmem  ld (cfgput+1),hl
        ld hl,cfgdocbeg             ;store default document config
        ld de,cfgdocdef
        ld bc,cfgdocend-cfgdocbeg
        ldir
        ld ix,(App_BnkNum-1)        ;open config file
        ld hl,datful
        call SyFile_FILOPN
        jp c,jmp_bnkret
        push af
        ld hl,cfgprgbeg
        ld bc,cfgprgend-cfgprgbeg
        ld de,(App_BnkNum)
        call SyFile_FILINP          ;load config
        pop bc
        push af
        ld a,b
        call SyFile_FILCLO
        pop af
        jp c,jmp_bnkret
        call cfgput                 ;copy to 1st bank
        jp jmp_bnkret

;### CFGSAV -> saves config to file
cfgsav  call cfgget
        xor a
        ld ix,(App_BnkNum-1)
        ld hl,datful
        call SyFile_FILNEW          ;create config file
        jp c,jmp_bnkret
        push af
        ld hl,cfgprgbeg
        ld bc,cfgprgend-cfgprgbeg
        ld de,(App_BnkNum)
        call SyFile_FILOUT          ;save config
        pop af
        call SyFile_FILCLO
        jp jmp_bnkret

;### CFGGET -> get config copy from 1st bank
cfgget  ld hl,(cfgput+1)
        ld de,cfgprgbeg
        ld bc,cfgdocend-cfgprgbeg
        jp ibkget

;### CFGPUT -> put config copy to 1st bank
cfgput  ld de,0
        ld hl,cfgprgbeg
        ld bc,cfgdocend-cfgprgbeg
        jp ibkput


;==============================================================================
;### CONFIG LIST ROUTINES #####################################################
;==============================================================================

;### LSTBLD -> builds unit/format list
;### Input      A=type (1=date, 2=time, 3=boolean, 4=number, 5=bin/hex), C=flag, if skip "[None]"
;### Output     lstcfduni prepared
;### Destroyed  AF,BC,DE,HL,IXL
lstbld  ld hl,8
        ld (ctrprptyp),hl
        cp 2
        ld hl,cfgmsktim
        jp z,lsttim
        ld hl,cfgmskdat
        jp c,lsttim
        cp 4
        jr c,lstbol
        ld a,"#"
        ld hl,cfguninum
        jr z,lstuni
        ld a,"."
        ld hl,cfgunibin
        jr lstuni

;### LSTBOL -> builds boolean list from config
;### Output     lstcfduni prepared
lstbol  ld hl,cfgunibol
        ld de,lstcfduni0
        ld b,8
lstbol1 push bc
        push de
        call lstbol2
        ld a," ":ld (de),a:inc de
        ld a,"/":ld (de),a:inc de
        ld a," ":ld (de),a:inc de
        call lstbol2
        pop de
        ex de,hl
        ld c,32
        add hl,bc
        ex de,hl
        pop bc
        djnz lstbol1
        ret
lstbol2 push hl
lstbol3 ld a,(hl)
        ldi
        or a
        jr nz,lstbol3
        dec de
        pop hl
        ld bc,8
        add hl,bc
        ret

;### LSTUNI -> builds unit list from config
;### Input      HL=config (cfguninum/cfgunibin), A=char ("#"/"."), C=flag, if skip "[None]"
;### Output     lstcfduni prepared
lstunin db "[none]",0
lstunic ds 3

lstuni  push hl
        ld l,a:ld h,a
        ld (lstunic+0),a
        ld (lstunic+1),hl
        ld a,7
        ld (ctrprptyp),a
        ld de,lstcfduni0
        dec c
        ld bc,32
        jr z,lstuni0
        ld hl,lstunin
        ldir
        ld a,8
        ld (ctrprptyp),a
lstuni0 pop hl
        ld c,8
        add hl,bc
        ld b,7
lstuni1 push bc
        push hl
        push de
        ld a,(hl)
        inc hl
        or a
        jr nz,lstuni3
        call lstuni4
        call lstuni5
lstuni2 xor a
        ld (de),a
        pop hl
        ld bc,32
        add hl,bc
        ex de,hl
        pop hl
        ld c,8
        add hl,bc
        pop bc
        djnz lstuni1
        ret
lstuni3 push hl
        call lstuni5
        pop hl
        call lstuni4
        jr lstuni2
lstuni4 ld a,(hl)
        or a
        ret z
        ldi
        jr lstuni4
lstuni5 ld hl,lstunic
        ldi:ldi:ldi
        ret

;### LSTTIM -> builds time format list from config
;### Input      HL=config (cfgmsktim/cfgmskdat)
;### Output     lstcfduni prepared
lsttim  ld de,lstcfduni0
        ld b,8
lsttim1 push bc
        push hl
        push de
        call lstmsk
        pop hl
        ld bc,32
        add hl,bc
        ex de,hl
        pop hl
        ld c,16
        add hl,bc
        pop bc
        djnz lsttim1
        ret

;### LSTMSK -> converts datetime mask to format item
;### Input      HL=mask, DE=destination item text
lstmskt
db "s":dw lstmska
db "m":dw lstmskb
db "H":dw lstmskc
db "h":dw lstmskd
db "D":dw lstmske
db "M":dw lstmskf
db "Y":dw lstmskg
db "y":dw lstmskh
db "n":dw lstmski
db "N":dw lstmskj
db "w":dw lstmskk
db "W":dw lstmskl
db "a":dw lstmskm
db 0
lstmska db "59",0
lstmskb db "37",0
lstmskc db "13",0
lstmskd db "01",0
lstmske db "31",0
lstmskf db "12",0
lstmskg db "1999",0
lstmskh db "99",0
lstmski db "Dec",0
lstmskj db "December",0
lstmskk db "Fri",0
lstmskl db "Friday",0
lstmskm db "PM",0

lstmsk  ld ixl,31
        ld c,e:ld b,d
lstmsk1 ld a,(hl)
        inc hl
        ex de,hl
        cp "\"
        jr nz,lstmsk2
        ld a,(de)
        inc de
        jr lstmsk4
lstmsk2 ld hl,lstmskt
lstmsk3 inc (hl):dec (hl)
        jr z,lstmsk4
        cp (hl)
        inc hl
        jr z,lstmsk6
        inc hl:inc hl
        jr lstmsk3
lstmsk4 ld (bc),a
        or a
        ret z
        inc bc
        dec ixl
        ret z
lstmsk5 ex de,hl
        jr lstmsk1
lstmsk6 ld a,(hl):inc hl
        ld h,(hl):ld l,a
lstmsk7 ld a,(hl)
        or a
        jr z,lstmsk5
        ld (bc),a
        inc hl
        inc bc
        dec ixl
        ret z
        jr lstmsk7


;==============================================================================
;### DOCUMENT PROPERTIES ROUTINES #############################################
;==============================================================================

prpvaltyp   db 0    ;type (1=date, 2=time, 3=boolean, 4=number, 5=bin/hex)
prpadrcfg   dw 0
prpadrtab   dw 0
prpflgchg   db 0    ;flag, if formats/units changed

;### PRPSTA -> prepares stats
;### Input      DE=stats adr in 2st bank
prpstam ds 20+4
prpstat
dw txtprpsa3,txtprpsa2:db 0
dw txtprpsr3,txtprpsr2:db 0+2
dw txtprpsb3,txtprpsb2:db 1+2
dw txtprpsc3,txtprpsc2:db 1+2
dw txtprpsd3,txtprpsd2:db 0
prpstas ds 4

prpsta  xor a:ld l,a:ld h,a
        ld (prpstas+0),a
        ld (prpstas+1),hl
        ex de,hl            ;get stats
        ld de,prpstam+4
        ld bc,16+3
        call ibkget
        ld hl,prpstam+4
        ld de,prpstam+0
        ld bc,4
        ldir
        ld hl,prpstam+4
        call prpsta6
        ld hl,prpstam+6
        call prpsta6
        ld b,5
        ld hl,prpstam
        ld de,prpstat
prpsta1 push bc
        ld c,(hl):inc hl
        ld b,(hl):inc hl            ;bc=max
        ld (prpsta2+1),bc
        ld a,(hl):inc hl:ld ixl,a
        ld a,(hl):inc hl:ld ixh,a   ;ix=used/free
        ex de,hl
        ld c,(hl):inc hl
        ld b,(hl):inc hl            ;bc=dst free
        ld a,(hl):inc hl:ld iyl,a
        ld a,(hl):inc hl:ld iyh,a   ;iy=dst used
        ld a,(hl):inc hl            ;a=type(0=used[],1=free[b])
        push hl
        push de
        push bc
        bit 0,a
        jr z,prpsta5
        ld hl,(prpsta2+1)
        push ix:pop de
        sbc hl,de
        push hl:pop ix
prpsta5 bit 1,a
        jr z,prpsta7
        push ix:pop hl
        ld de,(prpstas+0)
        add hl,de
        ld (prpstas+0),hl
        jr nc,prpsta7
        ld hl,prpstas+1
        inc (hl)
prpsta7 push ix
        call prpsta3
prpsta2 ld hl,0
        pop bc
        or a
        sbc hl,bc
        push hl:pop ix
        pop iy
        call prpsta3
        pop hl
        pop de
        pop bc
        djnz prpsta1
        ld ix,(prpstam+20)
        ld de,(prpstam+22)
        ld iy,txtprpse2
        push ix
        push de
        call cnv32s
        call prpsta4
        pop de
        pop bc
        ld hl,(prpstas+0)
        add hl,bc
        push hl:pop ix
        ld hl,(prpstas+2)
        adc hl,de
        ex de,hl
        ld iy,txtprpsf2
        call cnv32s
        jr prpsta4
prpsta3 push af
        ld de,0
        call cnv32s
        pop af
        bit 1,a
        ret z
prpsta4 ld (iy+1),"B"
        ld (iy+2),0
        ret
prpsta6 ld c,(hl):inc hl
        ld b,(hl)
        ex de,hl
        ld l,c:ld h,b
        add hl,hl:add hl,bc
        add hl,hl:add hl,hl
        ex de,hl
        ld (hl),d:dec hl
        ld (hl),e
        ret

;### PRPOPN -> opens document properties dialogue
prpopn  call prpsta         ;build statistics
        xor a
        ld (prpflgchg),a
        call cfgget         ;load config
        ld ix,ctrprpcat
        ld a,4
        ld (prpvaltyp),a    ;prepare window
        push af
        call lstput
        pop af
        call prpcat1
        ld a,-1             ;prepare colours
        ld (wincolrec1+16+6),a
        ld (wincolrec1+16+8),a
        call prpctp1
        ld de,winprpdat     ;open window
        jp windia

;### PRPTAB -> document properties tab pressed
prptabo db 0
prptabt db winprpreca_cnt:dw winprpreca
        db winprprecb_cnt:dw winprprecb
        db winprprecc_cnt:dw winprprecc
        db winprprecd_cnt:dw winprprecd

prptab  ld a,(ctrprptab0)
        ld hl,prptabo
        ld de,winprpgrp
        jp wintab

;### PRPCAT -> category changed
prpcat  call prpput
        ld ix,ctrprpcat
        call lstget
        ld hl,prpvaltyp
        cp (hl)
        jp z,guiret0
        ld (hl),a
        call prpcat1
        ld de,256*winprprecb_hlp+256-4
        ld a,(windianum)
        call SyDesktop_WININH
prpcat0 ld de,256*winprprecb_edc+256-winprprecb_edt-1
        ld a,(windianum)
        call SyDesktop_WININH
        jp guiret0
;a=new type -> init form
prpcat1 push af
        ld c,1
        call lstbld
        xor a
        ld ix,ctrprptyp
        call lstput
        call prpget
        pop af
        cp 3
        ld de,winprprecb_ed1
        ld c,64
        jr c,prpcat2
        ld de,winprprecb_ed2
        ld c,0
        jr nz,prpcat2
        ld de,winprprecb_ed3
prpcat2 push de
        ld de,16
        ld hl,16*winprprecb_hlp+winprprecb+16+2
        call prpcat5
        ld a,c:xor 64:ld c,a
        call prpcat5
        call prpcat5
        ld hl,16*winprprecb_edc+winprprecb+16+2         ;deactivate all editors
        ld b,winprprecb_edt
prpcat3 set 6,(hl)
        add hl,de
        djnz prpcat3
        pop bc
        ld l,b
        ld h,0
        add hl,hl:add hl,hl:add hl,hl:add hl,hl         ;activate required editor
        ld de,winprprecb+2
        add hl,de
        ld b,c
        ld de,16
prpcat4 res 6,(hl)
        add hl,de
        djnz prpcat4
        ret
prpcat5 ld a,(hl)
        res 6,a
        or c
        ld (hl),a
        add hl,de
        ret

;### PRPGET -> loads from list/config into editor
;### Output     ZF=1 no changes
prpgetc db 0    ;changed flag (0/1)

prpgett dw cfgmskdat  ,ctrprpe1i:db 16:dw 0        :db 0
        dw cfgmsktim  ,ctrprpe1i:db 16:dw 0        :db 0
        dw cfgunibol  ,ctrprpe1i:db  8:dw ctrprpe2i:db 8
        dw cfguninum+8,flgprpfix:db  1:dw ctrprpe1i:db 7
        dw cfgunibin+8,flgprpfix:db  1:dw ctrprpe1i:db 7

prpget  call prpget8
        xor a
prpget0 ld (prpadrcfg),hl
        ld (prpget3),a
        xor a
        ld (prpgetc),a
        ld e,(ix+2)
        ld d,(ix+3)
        ld c,(ix+4)
        ld b,0
        call prpget4
        ld c,(ix+7)
        inc c:dec c
        jr z,prpget2
        ld e,(ix+5)
        ld d,(ix+6)
        call prpget4
prpget2 ld ix,ctrprpe1i
        call strini
        ld ix,ctrprpe2i
        call strini
        ld a,(prpgetc)
        or a
        ret
prpget4 ld a,c
        dec a
        jr z,prpget7
        push de:pop iy
        ld (iy+10),a
        ld e,(iy+0)
        ld d,(iy+1)
prpget7 call prpget3
prpget6 ld a,(de)
        cp (hl)
        jr nz,prpget5
        inc de
        inc hl
        dec c
        jr nz,prpget6
        jr prpget3
prpget5 ldir
        ld a,1
        ld (prpgetc),a
prpget3 db 0
        ret
;-> ix,(prpadrtab)=config infos, hl=adr of current element
prpget8 ld a,(prpvaltyp)
        ld hl,(ctrprptyp+12)
        add hl,hl:add hl,hl:add hl,hl
        cp 4
        jr nc,prpget1
        add hl,hl
prpget1 dec a
        add a:add a:add a
        ld c,a:ld b,0
        ld ix,prpgett
        add ix,bc
        ld (prpadrtab),ix
        ld c,(ix+0)
        ld b,(ix+1)
        add hl,bc
        ret

;### PRPPUT -> saves from editor into config
;### Output     ZF=1 no changes
prpput  db #3e:ex de,hl
        ld hl,(prpadrcfg)
        ld ix,(prpadrtab)
        call prpget0
        push af
        ld hl,prpflgchg
        or (hl)
        ld (hl),a
        pop af
        ret

;### PRPTYP -> unit/format list clicked
prptyp  call prpput
        jp z,prptyp1
prptyp0 ld a,(prpvaltyp)
        ld c,1
        call lstbld
        ld e,winprprecb_hlp
        ld a,(windianum)
        call SyDesktop_WININH
prptyp1 call prpget
        jp z,guiret0
        jp prpcat0

;### PRPUP  -> move list entry up
prpup   ld hl,ctrprptyp+12
        inc (hl):dec (hl)
        jp z,guiret0
        push hl
        call prpput
        pop hl
        dec (hl)
        call prpdwn1        ;swap with previous
prpup1  ld a,(hl)
        ld ix,ctrprptyp
        call lstput
        jr prptyp0

;### PRPDWN -> move list entry down
prpdwn  ld hl,ctrprptyp+12
        ld a,(ctrprptyp+0)
        dec a
        cp (hl)
        jp z,guiret0
        push hl
        call prpput
        pop hl
        call prpdwn1        ;swap with next
        inc (hl)
        jr prpup1
prpdwn1 push hl
        call prpget8        ;ix=config infos, hl=adr of current element
        ld a,(ix+4)
        add (ix+7)
        ld b,a
        add l:ld e,a
        ld a,0
        adc h:ld d,a
prpdwn2 ld a,(de)
        ld c,(hl)
        ld (hl),a
        ld a,c
        ld (de),a
        inc de
        inc hl
        djnz prpdwn2
        pop hl
        ret

;### PRPOKY -> saves document properties
prpoky  call prpput
        call wincls
        call cfgput
        ld l,ret_prpoky
        ld a,(prpflgchg)
        ld e,a
        jp jmp_bnkret

;### PRPCTP -> colour type has been clicked
prpctp  call prpctp1
        ld de,256*winprprecc_col+256-2
        ld a,(windianum)
        call SyDesktop_WININH
        jp guiret0
prpctp1 ld hl,(flgcfgcor)
        ld h,0
        ld bc,cfgcolpen
        add hl,bc
        ld l,(hl)
        ld ix,winprprecc1
        ld de,256*25+110
        jp fmtcol3

;### PRPCOL -> colour has been clicked
prpcol  ld hl,prpflgchg
        set 1,(hl)
        ld iy,cfgcolpen
        ld a,(flgcfgcor)
        ld c,a
        ld b,0
        add iy,bc
        ld ix,winprprecc1
        ld a,winprprecc_col
        ld hl,256*25+110
        call cfdpen1
        jp guiret0

;### PRPDEF -> sets default document config and clears meta data
prpdef  call cfgget
        ld hl,cfgdocdef     ;copy default document config
        ld de,cfgdocbeg
        ld bc,cfgdocend-cfgdocbeg
        ldir
        call cfgput
        ld a,prpcprc        ;clear meta data
        ld ix,prpcprt
        call prpunc2
        jp jmp_bnkret

;### PRPSAV -> saves meta data into opened file
;### Input      H=file handler
;### Output     HL=AF from FILOUT
prpsav  push hl
        call prpcpr
        ld (txtprpchk+4),hl
        pop af
        push hl
        ld bc,8
        add hl,bc
        ld c,l:ld b,h
        ld hl,txtprpchk
        ld de,(App_BnkNum)
        call SyFile_FILOUT
        pop bc
        push af
        or a
prpsav1 call nc,prpunc
        pop hl
        jp jmp_bnkret

;### PRPLOD -> loads meta data from opened file
;### Input      H=file handler, DE=chunk length
;### Output     HL,DE=AF,BC from FILINP
prplod  ld a,h
        ld c,e:ld b,d
        ld hl,txtprpinf
        ld de,(App_BnkNum)
        push bc
        call SyFile_FILINP
        pop bc
        push af
        jr prpsav1

;### PRPUNC -> unpacks meta data
;### Input      BC=length
;### Destroyed  AF,BC,DE,HL,IX
prpunc  ld hl,txtprpinf-1
        add hl,bc
        ld de,txtprpsmi+256-1
        lddr
        inc de
        ex de,hl            ;hl=start of packed data
        ld a,(hl)
        push af
        inc hl
        ld ix,prpcprt
prpunc1 ld c,(hl)
        inc hl
        inc c
        ex de,hl
        ld l,(ix+0)
        ld h,(ix+1)
        ld b,(hl):inc hl
        ld h,(hl):ld l,b
        ld b,0
        ex de,hl
        ldir
        inc ix:inc ix
        dec a
        jr nz,prpunc1
        pop bc
        ld a,prpcprc
        sub b
        jr z,prpunc3
prpunc2 ld l,(ix+0)
        ld h,(ix+1)
        ld b,(hl):inc hl
        ld h,(hl):ld l,b
        ld (hl),0
        inc ix:inc ix
        dec a
        jr nz,prpunc2
prpunc3 ld b,prpcprc
        ld de,prpcprt
prpunc4 push bc
        ld a,(de):ld ixl,a:inc de
        ld a,(de):ld ixh,a:inc de
        call strini
        pop bc
        djnz prpunc4
        ld hl,-8
        ld (ctrprpsmi+24),hl
        ret

;### PRPCPR -> packs meta data
;### Output     HL=length
;### Destroyed  AF,DE,HL,IX
prpcprt dw ctrprpsti,ctrprpssi,ctrprpsai,ctrprpsoi,ctrprpski,ctrprpsmi
prpcprc equ 6

prpcpr  ld a,prpcprc
        ld ix,prpcprt
        ld de,txtprpinf
        push de
        ld (de),a
        inc de
prpcpr1 ld l,(ix+0)
        ld h,(ix+1)
        ld c,(hl):inc hl
        ld b,(hl)
        push bc
        ld bc,8-1
        add hl,bc
        ld c,(hl)
        ldi
        inc bc:inc c
        pop hl
        ldir
        inc ix:inc ix
        dec a
        jr nz,prpcpr1
        ex de,hl
        pop de
        or a
        sbc hl,de
        ret


;==============================================================================
;### CONFIG (APPLICATION PREFERENCES) DIALOGUE ################################
;==============================================================================

;### CFGOPN -> opens config dialogue
cfgopnt dw cfgfmtflt,cfgfmtbin,cfgfmthex

cfgopn  call cfgget         ;load config
        ld hl,cfgopnt       ;prepare formats
        ld ix,ctrcfgdpl
        ld b,3
cfgopn1 ld e,(hl):inc hl
        ld d,(hl):inc hl
        ld a,(de)
        and #38
        jr z,cfgopn2
        ld a,1
cfgopn2 ld (ix+17),a
        inc de
        ld a,(de)
        ld c,a
        and 7
        inc a
        and 7
        ld (ix+12),a
        ld a,c
        and 8
        rrca:rrca:rrca
        ld (ix+16),a
        ld de,18
        add ix,de
        djnz cfgopn1
        call cfgopn3
        ld ix,ctrcfgbgi
        call strini
        ld hl,txtcfgswi     ;prepare sizes
        ld ix,ctrcfgswi
        ld a,(cfgcelxln)
        call cfgopn4
        ld hl,txtcfgshi
        ld ix,ctrcfgshi
        ld a,(cfgcelyln)
        call cfgopn4
        ld a,-1             ;prepare colours
        ld (wincolrec1+16+6),a
        ld (wincolrec1+16+8),a
        call cfgctp1
        ld de,wincfgdat     ;open window
        jp windia
cfgopn3 ld ix,ctrcfgdsi
        call strini
        ld ix,ctrcfgdgi
        jp strini
cfgopn4 call cnv08s
        ld (hl),0
        jp strini

;### CFGTAB -> preferences tab pressed
cfgtabo db 0
cfgtabt db wincfgreca_cnt:dw wincfgreca
        db wincfgrecb_cnt:dw wincfgrecb

cfgtab  ld a,(ctrcfgtab0)
        ld hl,cfgtabo
        ld de,wincfggrp
        jp wintab

;### CFGREG -> region preset entry selected
cfgregt db ",." ;europe
        db ".," ;english
        db ".," ;asia
        db ",." ;south
        db ",'" ;switzerland

cfgreg  ld a,(ctrcfgrgl+12)
        sub 1
        jp c,guiret0
        add a
        ld l,a:ld h,0
        ld bc,cfgregt
        add hl,bc
        ld a,(hl):ld (cfgnumcom),a
        inc hl
        ld a,(hl):ld (cfgnumpoi),a
        call cfgopn3
        ld de,256*wincfgreca_sep+256-2
        ld a,(windianum)
        call SyDesktop_WININH
        jp guiret0

;### CFGOKY -> saves document properties
cfgoky  call wincls
        ld hl,cfgopnt       ;store format settings
        ld ix,ctrcfgdpl
        ld b,3
cfgoky1 ld e,(hl):inc hl
        ld d,(hl):inc hl
        ld a,(ix+17)
        or a
        jr z,cfgoky2
        ld a,3
        sub b
        add a:add a:add a
cfgoky2 ld (de),a
        inc de
        ld a,(ix+16)
        add a:add a:add a
        ld c,a
        ld a,(ix+12)
        and 7
        dec a
        and 7
        or c
        ld c,a
        ld a,(de)
        and #f0
        or c
        ld (de),a
        ld de,18
        add ix,de
        djnz cfgoky1
        ld iy,txtcfgswi     ;store cell size
        ld hl,cfgcelxln
        call cfgoky3
        ld iy,txtcfgshi
        inc hl
        call cfgoky3
        call cfgput
        jp guiret0
cfgoky3 push hl
        ld hl,barsdit
        ld bc,1
        ld de,126
        call cnvs16
        ld a,l
        pop hl
        ret c
        ld (hl),a
        ret

;### CFGCTP -> colour type has been clicked
cfgctp  call cfgctp1
        ld de,256*wincfgrecb_col+256-2
        ld a,(windianum)
        call SyDesktop_WININH
        jp guiret0
cfgctp1 ld hl,(flgcfgcor)
        ld h,0
        ld bc,cfgapppen
        add hl,bc
        ld l,(hl)
        ld ix,wincfgrecb1
        ld de,256*25+110
        jp fmtcol3

;### CFGCOL -> colour has been clicked
cfgcol  ld iy,cfgapppen
        ld a,(flgcfgcor)
        ld c,a
        ld b,0
        add iy,bc
        ld ix,wincfgrecb1
        ld a,wincfgrecb_col
        ld hl,256*25+110
        call cfdpen1
        jp guiret0


;==============================================================================
;### EXCHANGE (IMPORT/EXPORT) FILEFORMATS #####################################
;==============================================================================

xchhnd  db 0        ;file handle
        db "="  ;\
xchdat  ds 255  ;/
xchdln  db 0
xchbmx  equ 1024    ;buffer max
xchbuf  ds xchbmx   ;buffer memory
xchbfl  dw 0        ;current buffer length
xchbps  dw 0        ;current buffer position

;### XCHBFR -> reset buffer
xchbfr  ld hl,0
        ld (xchbfl),hl
        ret

;### XCHDEC -> dialogue for CSV format export
xchdec  ld de,txtdectit     ;open export csv dialogue
        ld bc,_xchexc
        ld hl,txtcsvexb
        ld a,15
        ld ixl,2
        ld iy,64*256+3
xchdec1 ld (wincsvdat1),de
        ld (wincsvrec1+0),bc
        ld (wincsvrec1+4),hl
        ld (wincsvgrp),a
        ld a,ixl
        ld (ctrcsvlmd),a
        call xchdec2
        ld de,wincsvdat
        jp windia
xchdec2 ld (xchbrw0+1),iy       ;iyl=type (6=sylk), iyh=load(0)/save(64)
xchdec3 ld l,-1
        call ibkcll:dw filbrw_
        ld (xchbrw2+1),hl
        jr xchbrw1

;### XCHDIC -> dialogue for CSV format import
xchdic  ld de,txtdictit     ;open import csv dialogue
        ld bc,_xchimc
        ld hl,txtcsvimb
        ld a,12
        ld ixl,4
        ld iy,0*256+3
        jr xchdec1

;### XCHBRW -> browses for a file
xchbrw  call xchbrw2
        or a
        jp nz,guiret0
        call xchbrw1
        ld e,wincsvrec_inp
        ld a,(windianum)
        call SyDesktop_WININH
        jp guiret0

xchbrw2 ld de,0
        ld hl,txtcsvfli
        ld bc,256
        call ibkput
xchbrw0 ld hl,0
        ld a,h
        call ibkcll:dw filbrw_
        ret
xchbrw1 ld hl,(xchbrw2+1)
        ld de,txtcsvfli
        ld bc,256
        call ibkget
        ld ix,ctrcsvfli
        jp strini

xchbrw3 ld iyl,6            ;iyh=load(0)/save(64) -> zf=1 txtcsvfli=path
        call xchdec2
        call xchbrw2
        or a
        ret nz
        call xchbrw1
        xor a
        ret

xchlstf db ";,",9," "
xchlstt db 34,"'"
xchlsti db 34,34, "'","'", 34,"'", 0,0

;### XCHEXC -> exports CSV
xchexc  call wincls

        ld hl,flgcsvquc
        ld a,(hl):add a
        dec hl
        add (hl) :add a
        dec hl
        add (hl)
        ld (xchexc1+1),a

        call xchlmg
        ld (xchlim+1),a
        ld (xchexc6+1),a
        ld bc,xchlstt
        add hl,bc
        ld a,(hl)
        ld (xchexc3+1),a

        call xchexc0
        jp c,xcherr0
        ld a,c:ld (xchexc5+1),a
        ld a,b:ld (xchexc8+1),a
        ld hl,0                     ;HL=first column/row

xchexc1 ld e,0
        push hl
        call ibkcll:dw celget_      ;L,H=cell, E=type (+1=as displayed, +2=formula) -> (celcnvtxt)=text, BC=length (without 0term), HL=celcnvtxt, A=type
        or a
        jr z,xchexc5                ;cell empty, go to next
        ld de,xchdat+1
        push af
        push bc
        call ibkget                 ;get cell content
        pop bc
        pop af
        cp celtyptxt
        jr nz,xchexc2
        ld a,(xchexc1+1)            ;cell is text..
        bit 2,a
        jr nz,xchexc3               ;..and quote always -> set quotes
xchexc2 call xchlim                 ;check, if field limiter inside text
        jr z,xchexc3                ;yes -> set quotes
        ld hl,xchdat+1
        ld de,xchdat+0
        push bc
        ldir                        ;no -> remove opening quote, go to next
        pop bc
        jr xchexc4
xchexc3 ld a,0                      ;set closing quote
        ld hl,xchdat
        ld (hl),a
        inc c
        add hl,bc
        ld (hl),a
        inc c
xchexc4 call xchwrt                 ;write cell content
xchexc5 ld a,0
        pop hl
        inc l
        cp l
        jr c,xchexc7
xchexc6 ld a,0
        ld (xchdat),a
        ld c,1
        push hl
        call xchwrt                 ;write field delimiter
        pop hl
        jr xchexc1
xchexc7 push hl
        ld hl,10*256+13             ;next line
        call xchexc9
        pop hl
xchexc8 ld a,0
        inc h
        ld l,0
        cp h
        jr nc,xchexc1

xchexca ld hl,#001a
        call xchexc9

        call xchwrt2
        ld a,(xchhnd)
        call SyFile_FILCLO
        ld a,1
        jp c,xcherr0
        jp guiret0

xchexc9 ld (xchdat),hl
        ld c,2
        jr xchwrt

xchexc0 ld hl,txtcsvfli
        ld ix,(App_BnkNum-1)
        xor a
        call SyFile_FILNEW
        ld (xchhnd),a
        ld a,1
        ret c
        ld hl,0
        ld de,256*fldmaxy+fldmaxx
        call ibkcll:dw movopt_      ;BC=last column/row
        ld (xcherr+1),sp
        call xchbfr                 ;reset buffer
        or a
        ret

;### XCHLIM -> check, if cell content contains field limiter
;### Input      C=length
;### Output     ZF=1 -> contains limiter
;### Destroyed  AF,HL
xchlim  ld a,0
        push bc
        ld hl,xchdat+1
xchlim1 cp (hl)
        jr z,xchlim2
        inc hl
        dec c
        jr nz,xchlim1
        inc c
xchlim2 pop bc
        ret

;### XCHLMG -> get delimiters
;### Output     A=field delimiter, HL=text limiter index
xchlmg  ld hl,(ctrcsvspd+12)        ;field delimiter
        ld bc,xchlstf
        add hl,bc
        ld a,(hl)
        ld hl,(ctrcsvlmd+12)        ;text limiter
        ret

;### XCHWRT -> writes into file output buffer
;### Input      C=data length, xchdat=data
;### Ouput      CF=0 ok, CF=1 -> error while writing
;### Destroyed  AF,BC,DE,HL,IX,IY
xchwrt  ld b,0
        ld hl,(xchbfl)
        ld e,l:ld d,h
        add hl,bc
        bit 2,h
        jr nz,xchwrt1       ;buffer >=xchbmx
        ld (xchbfl),hl
        ex de,hl
        ld de,xchbuf
        add hl,de
        ex de,hl
        ld hl,xchdat
        ldir
        ret
xchwrt1 push bc
        call xchwrt2
        pop bc
        jr nc,xchwrt
        ret
xchwrt2 ld hl,xchbuf
        ld bc,(xchbfl)
        ld a,(App_BnkNum)
        ld e,a
        ld a,(xchhnd)
        call SyFile_FILOUT
        ld hl,0
        ld (xchbfl),hl
        ld a,1
        jr c,xcherr
        ret z
;### XCHERR -> error during import/export
;### Input      A=reason (0=error while reading, 1=error while writing, 2=memory full, 3=format error [?])
xcherr  ld sp,0
        push af
        ld a,(xchhnd)
        call SyFile_FILCLO
        pop af
xcherr0 ;...alert
        jp guiret0

;### XCHIPC -> imports CSV from command line parameter
xchipc  call xchdec3                ;copy docpth to txtcsvfli
        ld a,";"
        ld hl,2         ;both ' and "
        jr xchimc0

;### XCHIMC -> imports CSV
xchimc  call wincls
        call xchlmg

xchimc0 ld (xchred3+1),a
        add hl,hl
        ld bc,xchlsti
        add hl,bc
        ld a,(hl):ld (xchred1+1),a
        inc hl
        ld a,(hl):ld (xchred2+1),a

        xor a
        call ibkcll:dw celimp_
        jp c,guiret0
        ld (xchimc2+1),hl

        ld hl,txtcsvfli
        ld ix,(App_BnkNum-1)
        call SyFile_FILOPN
        ld (xchhnd),a
        ld a,0
        jr c,xcherr0
        call xchbfr                 ;reset buffer
        ld (xcherr+1),sp

        ld hl,0                     ;HL=first column/row
xchimc1 push hl
        call xchred                 ;read one field from CSV
        pop hl
        jr c,xchimc4
        jr z,xchimc3
        ld de,xchdat
        call xchimc5
        ;jr c,...memful
        inc l
        ld a,fldmaxx
        cp l
        jr nc,xchimc1
xchimc3 ld l,0
        inc h
        ld a,fldmaxy
        cp h
        jr nc,xchimc1
xchimc4 ld a,(xchhnd)
        call SyFile_FILCLO
        ld a,1
        call ibkcll:dw celimp_
        jp guiret0
;hl=clm/row, bc=length, de=source
xchimc5 push hl
        ex de,hl
xchimc2 ld de,0
        ld b,0
        push bc
        call ibkput
        pop bc
        pop hl
        push hl
        call ibkcll:dw celput_
        pop hl
        ret

;### XCHIMS -> imports SYLK
xchimsc db 0,0      ;x,y

xchims  ld hl,256*0+0
        ld (xchimsc),hl
        ld iyh,0
        call xchbrw3            ;zf=1 -> txtcsvfli=path
        jp nz,guiret0
        xor a
        call ibkcll:dw celimp_
        jp c,guiret0
        ld (xchimc2+1),hl
        ld a,34
        ld (xchred1+1),a
        ld (xchred2+1),a
        ld a,";"
        ld (xchred3+1),a
        ld hl,txtcsvfli
        ld ix,(App_BnkNum-1)
        call SyFile_FILOPN
        ld (xchhnd),a
        ld a,0
        jp c,xcherr0
        call xchbfr             ;reset buffer
        ld (xcherr+1),sp

xchims1 call xchred                 ;read first field from new SYLK line
        jp c,xchimc4            ;EOF -> finished
        jr z,xchims1            ;only line feed -> try again
        dec c:dec c
        jr nz,xchims2
        ld a,(xchdat)
        cp "C"
        jr z,xchims3
xchims2 call xchred             ;unknown/unsupported -> skip line
        jp c,xchimc4
        jr nz,xchims2
        jr xchims1

xchims3 ld a,(xchred+1)         ;check eof/lf status after last field
        cp 1
        ld a,2
        ld (xchred+1),a
        jp c,xchimc4
        jr z,xchims1
        call xchlfd             ;read first char from next field
        jp c,xchimc4            ;eof -> finished
        jr z,xchims1            ;lf -> next
        cp "X"                  ;column
        ld hl,xchimsc+0
        ld de,fldmaxx+1
        jr z,xchims6
        cp "Y"                  ;row
        inc hl
        ld de,fldmaxy+1
        jr z,xchims6
        cp "E"                  ;formula
        ld de,-1
        jr z,xchims5
        cp "K"                  ;content
        inc de
        jr nz,xchims2           ;unknown -> skip line
xchims5 push de
        call xchred
        pop de
        jp c,xchimc4            ;eof
        jr z,xchims1            ;lf
        push de
        ld hl,xchdat
        call xchsuc             ;unescape ";"
        pop de
        add hl,de
        ld a,c
        sub e
        ld c,a
        ex de,hl
        ld hl,(xchimsc)
        call xchimc5            ;hl=clm/row, bc=length, de=source -> save cell
        ;jr c,...memful
        jr xchims3
xchims6 push de
        push hl
        call xchred
        pop hl
        pop de
        jp c,xchimc4
        jr z,xchims1
        push hl
        ld iy,xchdat
        ld bc,1
        ld hl,barsdit
        call cnvs16
        ld a,l
        pop hl
        jr c,xchims2            ;wrong row/column -> skip line
        dec a
        ld (hl),a
        jr xchims3

;### XCHRED -> read next field from file
;### Input      (xchred1+1)=text delimiter 1, (xchred2+1)=text delimiter 2, (xchred3+1)=field delimiter
;### Output     CF=1 -> EOF, ZF=1 -> next line, ZF=CF=0 -> xchdat=field, C=length (with 0term)
xchred  ld a,2              ;next status, 0=eof, 1=line feed
        cp 1
        ld a,2
        ld (xchred+1),a
        ret c
        ret z
        xor a
        ld (xchdln),a
        ld (xchdat),a
        call xchlfd         ;check for linefeed
        ret c
        ret z
xchred1 cp 0
        jr z,xchred4        ;limiter1 -> will end with text delimiter1
xchred2 cp 0
        jr z,xchred4        ;limiter2 -> will end with text delimiter2
        push af
xchred3 ld a,0              ;no limiter -> will end with field delimiter
        ld (xchred7+1),a
        pop af
        jr xchred7
xchred4 ld (xchred7+1),a
xchred5 call xchchr             ;** next char
xchred6 ld d,0
        jr c,xchreda        ;eof ->  finish field, set eof as next status
xchred7 cp 0                ;a=first/next char
        jr z,xchred8        ;end delimiter reached
        call xchlfd0
        jr c,xchred6        ;linefeed+eof
        ld d,1
        jr z,xchreda        ;linefeed -> finish field, set linefeed as next status
        ld e,a
        ld hl,xchdln
        ld a,(hl)
        cp 250
        jr z,xchred5        ;field too long, skip char
        inc (hl)
        ld c,a:ld b,0
        ld hl,xchdat
        add hl,bc
        ld (hl),e           ;add char to field
        inc hl
        ld (hl),0
        jr xchred5          ;get next
xchred8 ld hl,xchred3+1         ;** end delimiter reached
        cp (hl)
        ld d,2
        jr z,xchreda
xchred9 call xchlfd         ;was text delimiter -> check for field delimiter or linefeed
        ld d,0
        jr c,xchreda        ;-> eof
        ld d,1
        jr z,xchreda        ;-> linefeed
        ld hl,xchred3+1
        cp (hl)
        jr nz,xchred9
        ld d,2              ;-> field delimiter
xchreda ld a,d                  ;** finish field
        ld (xchred+1),a
        ld a,(xchdln)
        ld c,a
        inc c
        ret

;### XCHLFD -> (get char and) check for linefeed
;### Output     A=char, CF=1 -> EOF, ZF=1 -> linefeed
xchlfd  call xchchr
        ret c
xchlfd0 cp 13
        jr z,xchlfd
        cp 10
        scf:ccf
        ret

;### XCHCHR -> read char from file
;### Output     CF=0 -> A=char, CF=1 -> EOF
xchchr  ld hl,(xchbfl)
        ld a,l:or h
        jr z,xchchr2
        dec hl
        ld (xchbfl),hl
        ld hl,(xchbps)
xchchr1 xor a:inc a
        ld a,(hl)
        inc hl
        ld (xchbps),hl
        or a
        scf
        ret z
        cp 26
        scf
        ret z
        or a
        ret
xchchr2 ld hl,xchbuf
        ld bc,xchbmx
        ld a,(xchhnd)
        ld de,(App_BnkNum)
        call SyFile_FILINP
        ;jr c,...error
        ld a,c:or b
        scf
        ret z
        dec bc
        ld (xchbfl),bc
        ld hl,xchbuf
        jr xchchr1

;### XCHEXS -> exports SYLK
xchexsh db "ID;PSYMCALC",13,10:xchexsh0

xchexs  ld iyh,64
        call xchbrw3            ;zf=1 -> txtcsvfli=path
        jp nz,guiret0
        call xchexc0
        jp c,xcherr0
        ld a,c:ld (xchexs7+1),a
        ld a,b:ld (xchexs8+1),a

        ld hl,xchexsh               ;write header
        ld bc,xchexsh0-xchexsh
        ld a,(xchhnd)
        ld de,(App_BnkNum)
        call SyFile_FILOUT
        ld a,1
        jp c,xcherr

        ld hl,0                     ;HL=first column/row
xchexs1 push hl
        ld a,"C"                    ;start cell record
        ld (xchdat),a
        ld de,xchdat+1
        ld b,1
        ld c,"X":call xchsfl        ;write x coordinate
        call xchsnm
        pop hl:push hl
        ld c,"Y":call xchsfl        ;write y coordinate
        ld l,h
        call xchsnm
        ld c,"K":call xchsfl        ;start value
        pop hl:push hl

        push bc
        push de
        ld e,0
        call ibkcll:dw celget_      ;L,H=cell, E=type (+1=as displayed, +2=formula) -> (celcnvtxt)=text, BC=length (without 0term), HL=celcnvtxt, A=type
        pop de
        or a
        jr z,xchexs5
        ld (xchexs3+1),a
        cp celtyptxt
        jr nz,xchexs2
        ld a,34                     ;text -> add first quote
        ld (de),a:inc de
xchexs2 push bc
        push de
        call ibkget                 ;get cell content
        pop hl
        pop bc
        call xchsec                 ;escape ";"
        add hl,bc
        ex de,hl
        pop af
        add c
        ld b,a
xchexs3 ld a,0
        cp celtyptxt
        jr nz,xchexs4
        ld a,34                     ;text -> add last quote
        ld (de),a
        inc de
        inc b:inc b
xchexs4 cp celtypfrn
        jr nz,xchexs6
        xor a
        ld (xchexs3+1),a
        pop hl:push hl
        ld c,"E":call xchsfl        ;add formula
        push bc
        push de
        ld e,2
        call ibkcll:dw celget_      ;L,H=cell, E=type (+1=as displayed, +2=formula) -> (celcnvtxt)=text, BC=length (without 0term), HL=celcnvtxt, A=type
        pop de
        inc hl
        dec c
        jr xchexs2

xchexs5 pop bc
        jr xchexs7
xchexs6 ex de,hl
        ld (hl),13:inc hl
        ld (hl),10
        inc b:inc b
        ld c,b
        call xchwrt
xchexs7 ld a,0
        pop hl
        inc l
        cp l
        jp nc,xchexs1
        ld l,0
        inc h
xchexs8 ld a,0
        cp h
        jp nc,xchexs1
        ld a,"E"       :ld (xchdat+0),a
        ld hl,10*256+13:ld (xchdat+1),hl
        ld c,3
        call xchwrt
        jp xchexca

;### XCHSFL -> starts new field in sylk record
;### Input      C=letter, B=length, DE=pointer
;### Output     B,DE updated
xchsfl  ld a,";"
        call xchsfl1
        ld a,c
xchsfl1 ld (de),a
        inc de
        inc b
        ret

;### XCHSNM -> adds decimal number to sylk record
;### Input      L=number-1, B=length, DE=pointer
;### Output     B,DE updated
xchsnm  ld a,l
        inc a
        ex de,hl
        call cnv08s
        ex de,hl
        ret

;### XCHSEC -> escape sylk text (replaces ";" with ";;")
;### Input      HL=string, BC=length
;### Output     BC=new length
;### Destroyed  AF,DE,IXL
xchsec  inc c:dec c
        ret z
        push hl
        ld a,";"
        ld b,c
        ld e,l:ld d,h
xchsec1 cp (hl)
        jr nz,xchsec2
        inc de
xchsec2 inc hl
        inc de
        djnz xchsec1
        dec hl          ;hl=last char
        dec de          ;de=last char in escaped string
        ld ixl,c
        lddr
        ex de,hl
        inc hl          ;hl=first char destination
        inc de          ;de=first char source
        ld c,-1
        ld b,ixl
xchsec3 ld a,(hl)
        ldi
        cp ";"
        jr nz,xchsec4
        ld (de),a
        inc de
xchsec4 djnz xchsec3
        ld c,ixl
        pop hl
        ret

;### XCHSUC -> unescape sylk text (replaces ";;" with ";")
;### Input      HL=string, BC=length
;### Output     BC=new length
;### Destroyed  AF,DE,IXL
xchsuc  inc c:dec c
        ret z
        push hl
        ld e,l:ld d,h
        ld ixl,c
xchsuc1 ld a,(hl)
        ldi
        jp po,xchsuc2
        cp ";"
        jr nz,xchsuc1
        cp (hl)
        jr nz,xchsuc1
        inc hl
        dec ixl
        jr xchsuc1
xchsuc2 pop hl
        ld c,ixl
        ret


;==============================================================================
;### SUB ROUTINES #############################################################
;==============================================================================

;### PRGFIL -> Generates config file path
datnam  db "symcalc.ini",0:datnam0
datful  ds 256

datpth  dw 0
datfil  dw 0

prgfil  ld hl,(App_BegCode)
        ld de,App_BegCode
        dec h
        add hl,de           ;HL = CodeEnd = path
        ld de,datful
        push de
        ld bc,256
        ldir
        pop hl
        ld (datpth),hl
        ld e,l
        ld d,h              ;DE=HL
        ld b,255
prgfil1 ld a,(hl)           ;search end of path
        or a
        jr z,prgfil2
        inc hl
        djnz prgfil1
        jr prgfil4
        ld a,255
        sub b
        jr z,prgfil4
        ld b,a
prgfil2 dec hl              ;search start of filename
        ld a,(hl)
        cp "/"
        jr z,prgfil3
        cp "\"
        jr z,prgfil3
        cp ":"
        jr z,prgfil3
        djnz prgfil2
        jr prgfil4
prgfil3 inc hl
        ex de,hl
prgfil4 ld (datfil),de
        ld hl,datnam        ;replace application filename with extended filename
        ld bc,datnam0-datnam
        ldir
        ret

;### LSTPUT -> select list entry by value
;### Input      IX=list, A=value
;### Destroyed  AF,BC,DE,HL
lstput  ld l,(ix+04)
        ld h,(ix+05)
        ld b,(ix+00)
        ld c,0
lstput1 cp (hl)
        inc hl
        jr nz,lstput2
        set 7,(hl)
        ld (ix+12),c
        jr lstput3
lstput2 res 7,(hl)
lstput3 inc hl:inc hl:inc hl
        inc c
        djnz lstput1
        ret

;### LSTGET -> get value from list
;### Input      IX=list
;### Output     A=value
;### Destroyed  F,BC,HL
lstget  ld l,(ix+12)
        ld h,0
        add hl,hl:add hl,hl
        ld c,(ix+04)
        ld b,(ix+05)
        add hl,bc
        ld a,(hl)
        ret

;### FLDWDT -> get cell width table from 1st bank
;### Input      HL=adr in 1st bank
;### Output     (flddimt)=cell width table
;### Destryoed  AF,BC,DE,HL
fldwdt  ld de,flddimt
        ld bc,255
        jp ibkget

;### FLDDIM -> calculates cell begin and width
;### Input      (flddimt)=width table, A=position
;### Output     HL=cell begin, DE=cell width
;### Destroyed  AF,IX
flddimt ds 255
flddimx dw 0
flddimy dw 0

flddimb push ix:pop hl
        push af
        push bc
        push de
        call fldwdt
        pop de
        pop bc
        pop af
flddim  ld ix,flddimt
        ld hl,0
        ld d,l
flddim1 ld e,(ix+0)
        srl e
        or a
        ret z
        add hl,de
        inc ix
        dec a
        jr flddim1

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


;==============================================================================
;### MODULES ##################################################################
;==============================================================================

read"App-SymCalc-Subs.asm"


hshbkt  db 0,0,0        ;2048*6 -> hash buckets   1W clm/row, 1W record address (0=empty), 1W pointer to next bucket entry
;*** LAST IN CODE AREA!***


;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;### DATA AREA ################################################################
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

App_BegData

read"App-SymCalc-Font.asm"

txtcfdprv   ds 40+6

;==============================================================================
;### BITMAPS ##################################################################
;==============================================================================

;toolbar
gfxtolopn db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;new
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#11,#11,#11,#66,#66,#67, #68,#66,#18,#88,#81,#16,#66,#67, #68,#66,#18,#88,#81,#81,#66,#67, #68,#66,#18,#88,#81,#11,#16,#67, #68,#66,#18,#88,#88,#88,#16,#67
db #68,#66,#18,#88,#88,#88,#16,#67, #68,#66,#18,#88,#88,#88,#16,#67, #68,#66,#18,#88,#88,#88,#16,#67, #68,#66,#18,#88,#88,#88,#16,#67, #68,#66,#11,#11,#11,#11,#16,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77
gfxtolnew db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;open
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#61,#16,#66,#67, #68,#66,#66,#66,#16,#61,#61,#67, #68,#66,#11,#16,#66,#66,#11,#67, #68,#61,#00,#01,#11,#61,#11,#67, #68,#61,#00,#00,#01,#66,#66,#67, #68,#61,#00,#00,#11,#11,#11,#17
db #68,#61,#00,#01,#22,#22,#21,#67, #68,#61,#00,#12,#22,#22,#16,#67, #68,#61,#01,#22,#22,#21,#66,#67, #68,#61,#12,#22,#22,#16,#66,#67, #68,#61,#11,#11,#11,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77
gfxtolsav db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;save
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#66,#66,#66,#67, #68,#61,#11,#11,#11,#11,#11,#67, #68,#61,#F1,#00,#00,#01,#01,#67, #68,#61,#F1,#00,#00,#01,#11,#67, #68,#61,#F1,#00,#00,#01,#F1,#67, #68,#61,#FF,#11,#11,#1F,#F1,#67
db #68,#61,#FF,#FF,#FF,#FF,#F1,#67, #68,#61,#FF,#11,#11,#11,#F1,#67, #68,#61,#FF,#11,#11,#01,#F1,#67, #68,#61,#FF,#11,#11,#01,#F1,#67, #68,#66,#11,#11,#11,#11,#11,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77
gfxtolprt db 8,16,14:dw $+7,$+4,16*14:db 5        ;print
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#61,#11,#11,#11,#67, #68,#66,#66,#61,#88,#88,#81,#67, #68,#66,#66,#18,#11,#18,#16,#67, #68,#66,#66,#18,#88,#88,#16,#67, #68,#66,#61,#81,#11,#81,#66,#67
db #68,#66,#11,#11,#11,#11,#16,#67, #68,#61,#aa,#aa,#aa,#aa,#a1,#67, #68,#61,#ad,#dd,#af,#fa,#a1,#67, #68,#61,#aa,#aa,#aa,#aa,#a1,#67, #68,#66,#11,#11,#11,#11,#16,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77
gfxtolund db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;undo
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#61,#11,#66,#67, #68,#61,#66,#61,#16,#66,#16,#67, #68,#61,#56,#56,#66,#66,#61,#67
db #68,#65,#55,#66,#66,#66,#61,#67, #68,#65,#55,#56,#66,#66,#61,#67, #68,#65,#55,#51,#66,#66,#16,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77

;font style
gfxstybld db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;bold
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#11,#11,#11,#11,#66,#67, #68,#66,#61,#11,#66,#11,#16,#67, #68,#66,#61,#11,#66,#11,#16,#67, #68,#66,#61,#11,#66,#11,#16,#67
db #68,#66,#61,#11,#11,#11,#66,#67, #68,#66,#61,#11,#66,#11,#16,#67, #68,#66,#61,#11,#66,#11,#16,#67, #68,#66,#61,#11,#66,#11,#16,#67, #68,#66,#11,#11,#11,#11,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77
gfxstyita db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;italics
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#11,#11,#16,#67, #68,#66,#66,#66,#61,#16,#66,#67, #68,#66,#66,#66,#61,#16,#66,#67, #68,#66,#66,#66,#11,#66,#66,#67
db #68,#66,#66,#66,#11,#66,#66,#67, #68,#66,#66,#61,#16,#66,#66,#67, #68,#66,#66,#61,#16,#66,#66,#67, #68,#66,#66,#11,#66,#66,#66,#67, #68,#66,#11,#11,#11,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77
gfxstyuli db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;underlined
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#11,#11,#61,#11,#16,#67, #68,#66,#61,#16,#66,#11,#66,#67, #68,#66,#61,#16,#66,#11,#66,#67, #68,#66,#61,#16,#66,#11,#66,#67
db #68,#66,#61,#16,#66,#11,#66,#67, #68,#66,#61,#16,#66,#11,#66,#67, #68,#66,#66,#11,#11,#16,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#11,#11,#11,#11,#16,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77
gfxstybig db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;big
ds 14*8

gfxstycol db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;colour
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#56,#66,#66,#67, #68,#66,#66,#65,#55,#66,#66,#67, #68,#66,#66,#65,#55,#66,#66,#67, #68,#66,#66,#55,#65,#56,#66,#67, #68,#66,#66,#55,#55,#56,#66,#67
db #68,#66,#65,#56,#66,#55,#66,#67, #68,#66,#55,#55,#65,#55,#56,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#6f,#ff,#ff,#ff,#ff,#ff,#67, #68,#6f,#ff,#ff,#ff,#ff,#ff,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77

;text alignment
gfxalglft db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;left
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#61,#11,#11,#11,#11,#16,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#61,#11,#11,#11,#66,#66,#67
db #68,#66,#66,#66,#66,#66,#66,#67, #68,#61,#11,#11,#11,#11,#16,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#61,#11,#11,#11,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77
gfxalgcen db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;centered
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#11,#11,#11,#11,#16,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#11,#11,#16,#66,#67
db #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#11,#11,#11,#11,#16,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#11,#11,#16,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77
gfxalgrgt db 8,16,14:dw $+7:dw $+4,8*14:db 5      ;right
db #68,#88,#88,#88,#88,#88,#88,#88, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#11,#11,#11,#11,#11,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#61,#11,#11,#11,#67
db #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#11,#11,#11,#11,#11,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#61,#11,#11,#11,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#66,#66,#66,#66,#66,#66,#67, #68,#77,#77,#77,#77,#77,#77,#77

;misc
gfxfrma db 6,12,12:dw $+7,$+4,6*12:db 5         ;function
db #88,#88,#88,#88,#88,#88, #86,#66,#66,#66,#66,#67, #86,#66,#11,#16,#66,#67, #86,#61,#16,#66,#66,#67
db #86,#61,#16,#66,#66,#67, #86,#11,#11,#66,#66,#67, #86,#61,#16,#ff,#6f,#f7, #86,#61,#16,#6f,#ff,#67
db #86,#61,#16,#6f,#ff,#67, #86,#11,#66,#ff,#6f,#f7, #86,#66,#66,#66,#66,#67, #87,#77,#77,#77,#77,#77

;sheet tabs
gfxtablft db 4,8,8:dw $+7,$+4,4*8:db 5          ;tab left
db #66,#66,#66,#66, #66,#66,#61,#16, #66,#61,#11,#16, #61,#11,#11,#16, #61,#11,#11,#16, #66,#61,#11,#16, #66,#66,#61,#16, #66,#66,#66,#66
gfxtabrgt db 4,8,8:dw $+7,$+4,4*8:db 5          ;tab right
db #66,#66,#66,#66, #61,#16,#66,#66, #61,#11,#16,#66, #61,#11,#11,#16, #61,#11,#11,#16, #61,#11,#16,#66, #61,#16,#66,#66, #66,#66,#66,#66
gfxtabpls db 4,8,8:dw $+7,$+4,4*8:db 5          ;tab plus
db #66,#66,#66,#66, #66,#61,#16,#66, #66,#61,#16,#66, #61,#11,#11,#16, #61,#11,#11,#16, #66,#61,#16,#66, #66,#61,#16,#66, #66,#66,#66,#66
gfxtabiop db 2,4,8:dw $+7,$+4,2*8:db 5          ;tab inactive open
db #16,#66, #16,#66, #61,#66, #61,#66, #66,#16, #66,#16, #66,#61, #66,#61
gfxtabicl db 2,4,8:dw $+7,$+4,2*8:db 5          ;tab inactive close
db #66,#61, #66,#61, #66,#16, #66,#16, #61,#66, #61,#66, #16,#66, #16,#66
gfxtabaop db 2,4,8:dw $+7,$+4,2*8:db 5          ;tab active open
db #18,#88, #18,#88, #61,#88, #61,#88, #66,#18, #66,#18, #66,#61, #66,#61
gfxtabacl db 2,4,8:dw $+7,$+4,2*8:db 5          ;tab active close
db #88,#81, #88,#81, #88,#16, #88,#16, #81,#66, #81,#66, #16,#66, #16,#66
gfxtabi2i db 4,5,8:dw $+7,$+4,4*8:db 5          ;tab inact to inact
db #66,#61,#66,#66, #66,#61,#66,#66, #66,#16,#66,#66, #66,#16,#66,#66, #61,#61,#66,#66, #61,#61,#66,#66, #16,#66,#16,#66, #16,#66,#16,#66
gfxtabi2a db 4,5,8:dw $+7,$+4,4*8:db 5          ;tab inact to act
db #61,#88,#88,#88, #61,#88,#88,#88, #66,#18,#88,#88, #66,#18,#88,#88, #61,#61,#88,#88, #61,#61,#88,#88, #16,#66,#18,#88, #16,#66,#18,#88
gfxtaba2i db 4,5,8:dw $+7,$+4,4*8:db 5          ;tab act to inact
db #88,#81,#66,#66, #88,#81,#66,#66, #88,#16,#66,#66, #88,#16,#66,#66, #81,#61,#66,#66, #81,#61,#66,#66, #16,#66,#16,#66, #16,#66,#16,#66

arrdwngfx db 4,8,8:dw $+7:dw $+4,4*8:db 5
db #11,#11,#11,#11, #18,#88,#88,#81, #18,#81,#18,#81, #11,#11,#11,#11, #18,#11,#11,#81, #18,#81,#18,#81, #18,#88,#88,#81, #11,#11,#11,#11


;==============================================================================
;### GENERAL STRINGS ##########################################################
;==============================================================================

prgtxtok    db "Ok",0
prgtxtcnc   db "Cancel",0
prgtxtdef   db "Default",0
prgtxtbrw   db "Browse...",0
prgtxtnul   db 0


;==============================================================================
;### FORMS ####################################################################
;==============================================================================

;*** ROW/COLUMN WIDTH/HEIGHT **************************************************

txtsizclm   db "Width",0
txtsizrow   db "Height",0

;*** COLOUR DROPDOWN **********************************************************

txtcolpen   db "Pen",0
txtcolpap   db "Paper",0

;*** FORMAT DIALOGUE **********************************************************

txtcfdtit   db "Format cells",0
txtcfdnum   db "Number",0
txtcfdaln   db "Alignment",0
txtcfdfnt   db "Font & Colours",0
txtcfdpfr   db "Preview",0
txtcfdclb   db "Category:",0
txtcfdcat0  db "Number",0
txtcfdcat1  db "Date",0
txtcfdcat2  db "Time",0
txtcfdcat3  db "Percentage",0
txtcfdcat4  db "Scientific",0
txtcfdcat5  db "Boolean",0
txtcfdcat6  db "Binary",0
txtcfdcat7  db "Hexadecimal",0
txtcfdcat8  db "Text",0
txtcfdtyt1  db "Unit:        ",0
txtcfdtyt2  db "Format:    ",0
txtcfdtyt3  db "Keywords:",0
txtcfdopt   db "Options",0
txtcfddpt1  db " Decimal places",0
txtcfddpt2  db "Displayed digits",0
txtcfddpl7  db "-",0
txtcfddpl0  db "0",0
txtcfddpl1  db "1",0
txtcfddpl2  db "2",0
txtcfddpl3  db "3",0
txtcfddpl4  db "4",0
txtcfddpl5  db "5",0
txtcfddpl6  db "6",0
txtcfddbl7  db "-",0
txtcfddbl0  db "1",0
txtcfddbl1  db "4",0
txtcfddbl2  db "8",0
txtcfddbl3  db "12",0
txtcfddbl4  db "16",0
txtcfddbl5  db "24",0
txtcfddbl6  db "32",0
txtcfddxl7  db "-",0
txtcfddxl0  db "1",0
txtcfddxl1  db "2",0
txtcfddxl2  db "3",0
txtcfddxl3  db "4",0
txtcfddxl4  db "5",0
txtcfddxl5  db "6",0
txtcfddxl6  db "8",0
txtcfdsep   db "1000 separator",0
txtcfdahl   db "Horizontal:",0
txtcfdavl   db "Vertical:",0
txtcfdalh0  db "General",0
txtcfdalh1  db "Left",0
txtcfdalh2  db "Centered",0
txtcfdalh3  db "Right",0
txtcfdalv0  db "Top",0
txtcfdstt   db "Font style:",0
txtcfdstl0  db "Regular",0
txtcfdstl1  db "Bold",0
txtcfdstl2  db "Italics",0
txtcfdcot   db "Colour:",0
txtcfdbgr   db "Background:",0

;*** DOCUMENT PROPERTIES ******************************************************

txtprptit   db "Document properties",0
txtprpsum   db "Summary",0
txtprpfmt   db "Units & Formats",0
txtprpcol   db "Colours",0
txtprpsta   db "Statistics",0

;*** summary ******************************************************************

txtprpstl   db "Title",0
txtprpssl   db "Subject",0
txtprpsal   db "Author",0
txtprpsol   db "Company",0
txtprpskl   db "Keywords",0
txtprpsml   db "Comment",0

txtprpchk   db "META":ds 4
txtprpinf   ds prpcprc+1    ;num,length
txtprpsti   ds 40           ;title
txtprpssi   ds 40           ;subject
txtprpsai   ds 40           ;author
txtprpsoi   ds 40           ;company
txtprpski   ds 40           ;keywords
txtprpsmi   ds 256          ;comment

;*** units & formats **********************************************************

txtprpcat0  db "Number units",0
txtprpcat1  db "Date formats",0
txtprpcat2  db "Time formats",0
txtprpcat3  db "Boolean display",0
txtprpcat4  db "Bin/Hex units",0

txtprpup    db "Up",0
txtprpdwn   db "Down",0
txtprptyt1  db "Units:    ",0
txtprptyt2  db "Formats:",0
txtprphlt   db "Help:",0

txtprpe1i   ds 16
txtprpe2i   ds 8
txtprpedf   db "Editor",0
txtprptlb   db "True (>0)",0
txtprpflb   db "False (=0)",0
txtprpfix0  db "Prefix",0
txtprpfix1  db "Suffix",0

txtprphlp
db "s second",13,10
db "m minute",13,10
db "H hour 24",13,10
db "h hour 12",13,10
db "a AM/PM",13,10
db "D day",13,10
db "M month",13,10
db "Y year 4digits",13,10
db "y year 2digits",13,10
db "W weekday long",13,10
db "w weekday short",13,10
db "N month long",13,10
db "n month short",13,10
db "\ print next",13,10
txtprphlp0  db 0

;*** statistics ***************************************************************

txtprpta2   db "Used",0
txtprpta3   db "Available",0
txtprpsa1   db "Cells",0
txtprpsa2   ds 5
txtprpsa3   ds 5
txtprpsr1   db "Cell records",0
txtprpsr2   ds 7
txtprpsr3   ds 7
txtprpsb1   db "Cell data",0
txtprpsb2   ds 7
txtprpsb3   ds 7
txtprpsc1   db "Cell texts",0
txtprpsc2   ds 7
txtprpsc3   ds 7
txtprpsd1   db "GUI controls",0
txtprpsd2   ds 4
txtprpsd3   ds 4
txtprpse1   db "External Sheets",0
txtprpse2   ds 8
txtprpsf1   db "Total memory",0
txtprpsf2   ds 8

;### PREFERENCES DIALOGUE #####################################################

txtcfgtit   db "Preferences",0
txtcfgfrm   db "Formatting",0
txtcfgcol   db "Colours & Sizes",0
txtprpdst   db "Decimal separator",0
txtcfgdgt   db "Digit grouping",0
txtcfgbgt   db "Bin/Hex group.",0
txtcfgrgf   db "Regional settings",0
txtcfgrgt   db "Regional presets",0

txtcfgrgl0  db "[please select...]",0
txtcfgrgl1  db "Continental Europe, Iceland",0
txtcfgrgl2  db "USA, United Kingdom, Australia",0
txtcfgrgl3  db "Japan, China, India, Pakistan",0
txtcfgrgl4  db "South America, Africa (mostly)",0
txtcfgrgl5  db "Switzerland",0

txtcfgfmf   db "Default cell formatting",0
txtcfgdpt   db "Decimal places",0
txtcfgdbt   db "Binary digits",0
txtcfgdxt   db "Hexadecimal digits",0
txtcfggrt   db "Grouping",0
txtcfgprt   db "Prefix",0

txtcfgcof   db "Colour settings",0
txtcfgctt   db "Cell text",0
txtcfgcbt   db "Cell background",0
txtcfgcgt   db "Grid lines",0
txtcfgszf   db "Default cell size",0
txtcfgswt   db "Width",0
txtcfgsht   db "Height",0

;*** EXCHANGE (IMPORT/EXPORT) FILEFORMATS *************************************

txtdectit   db "Export CSV File",0
txtdictit   db "Import CSV File",0
txtdestit   db "Export SYLK File",0
txtdistit   db "Import SYLK File",0

txtcsvexb   db "Export",0
txtcsvimb   db "Import",0

txtcsvfll   db "File",0
txtcsvfli   ds 256

txtcsvopl   db "Field options",0
txtcsvspl   db "Field delimiter",0
txtcsvlml   db "Text delimiter",0

lstcsvlmd0  db 34,0
lstcsvlmd1  db "'",0
lstcsvlmd2  db 34," or '",0
lstcsvlmd3  db "[None]",0

lstcsvspd0  db ";",0
lstcsvspd1  db ",",0
lstcsvspd2  db "[Tabulator]",0
lstcsvspd3  db "[Space]",0

txtcsvshc   db "Save cell content as shown",0
txtcsvfoc   db "Save formulas instead of calc.values",0
txtcsvquc   db "Quote all text cells",0


;### ALERT WINDOWS ############################################################

prgtxtinf1  db "SymCalc Spreadsheet",0
prgtxtinf2  db " Version ",ver_app_maj+"0",".",ver_app_min+"0"," (Build "
read "..\..\..\SRC-Main\build.asm"
            db "pdt)",0
prgtxtinf3  db " Copyright <c> 20":dw ver_app_year:db " SymbiosiS"
prgtxtinf0  db 0

prgtxtsav1  db "Save changes?",0
prgerrtxt1a db "Error while loading:",0
prgerrtxt1b db "Error while saving:",0

prgerrtxt2a db "A disc error occured",0
prgerrtxt2b db "Wrong file format",0
prgerrtxt2c db "New unsupported version",0
prgerrtxt2d db "Corrupt file",0
prgerrtxt2e db "Memory full",0

prgerrtxt3a db "(see symbos.org/err.htm#"
prgerrtxt3a0 db "##)",0
prgerrtxt3b equ prgtxtinf0
prgerrtxt3c db "Please upgrade SymCalc",0
prgerrtxt3d equ prgtxtinf0
prgerrtxt3e db "Please upgrade your machine",0

;error - formula
errfortxta  db "Incorrect formula:",0
errfortxt0  db "Invalid component",0
errfortxt1  db "Syntax error",0
errfortxt2  db "Number mistyped",0
errfortxt3  db "Overflow, number too large",0
errfortxt4  db "Reference mistyped",0
errfortxt5  db "Bracket(s) not closed",0
errfortxt6  db "List outside function",0
errfortxt7  db "Expression error",0

;error - recalculation
errfortxta1 db "Can't recalulate:",0
errfortxta2 db "Circular reference found!",0

;error - copy/cut/paste
errcoptxta1 db "Can't paste cell(s):",0
errcoptxta2 db "Destination range outside field",0

;error - memory full
errmemtxta1 db "Memory full!",0
errmemtxta2 db "Can't create new cell",0
errmemtxtb2 db "Can't modify cell.",0
errmemtxtc2 db "Can't copy cell(s).",0
errmemtxtd2 db "Can't recalculate cell(s).",0

;*** PULL DOWN / CONTEXT MENUS ************************************************

menicn_null         db 4,8,1:dw $+7,$+4,4:db 5: db #66,#66,#66,#66

;main
menmaitxt1  db "File",0
menmaitxt11 db 6,128,-1:dw menicn_filenew+1:      db " New",0
menmaitxt12 db 6,128,-1:dw menicn_fileopen+1:     db " Open...",0
menmaitxt13 db 6,128,-1:dw menicn_filesave+1:     db " Save",0
menmaitxt14 db 6,128,-1:dw menicn_filesaveas+1:   db " Save as...",0
menmaitxt15 db 6,128,-1:dw menicn_import+1:       db " Import",0
menmaitxt151 db 6,128,-1:dw menicn_filcsv+1:       db " CSV file...",0
menmaitxt152 db 6,128,-1:dw menicn_filsylk+1:      db " SYLK file...",0
menmaitxt16 db 6,128,-1:dw menicn_export+1:       db " Export",0
menmaitxt17 db 6,128,-1:dw menicn_properties+1:   db " Properties...",0
menmaitxt10 db 6,128,-1:dw menicn_quit+1:         db " Exit",0

menicn_filenew      db 4,8,7:dw $+7,$+4,28:db 5: db #61,#11,#11,#66, #61,#88,#81,#16, #61,#88,#88,#16, #61,#88,#88,#16, #61,#88,#88,#16, #61,#88,#88,#16, #61,#11,#11,#16
menicn_fileopen     db 4,8,7:dw $+7,$+4,28:db 5: db #61,#16,#66,#66, #18,#81,#16,#66, #18,#88,#77,#77, #18,#87,#22,#27, #18,#72,#22,#76, #17,#22,#27,#66, #77,#77,#76,#66
menicn_filesave     db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#11, #1f,#ee,#ee,#f1, #1f,#ee,#ee,#f1, #1f,#ff,#ff,#f1, #1f,#11,#c1,#f1, #1f,#11,#c1,#f1, #61,#11,#11,#11
menicn_filesaveas   db 4,8,7:dw $+7,$+4,28:db 5: db #66,#11,#11,#16, #66,#18,#88,#11, #11,#11,#11,#81, #1f,#ee,#f1,#81, #1f,#ff,#f1,#81, #1f,#11,#f1,#11, #61,#11,#11,#66
menicn_import       db 4,8,7:dw $+7,$+4,28:db 5: db #16,#77,#77,#76, #11,#78,#88,#77, #1e,#18,#88,#87, #1e,#e1,#88,#87, #1e,#18,#88,#87, #11,#78,#88,#87, #16,#77,#77,#77
menicn_export       db 4,8,7:dw $+7,$+4,28:db 5: db #66,#71,#77,#76, #66,#11,#88,#77, #61,#e1,#88,#87, #1e,#e1,#88,#87, #61,#e1,#88,#87, #66,#11,#88,#87, #66,#71,#77,#77
menicn_filcsv       db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #66,#66,#66,#66, #6f,#61,#67,#67, #f6,#16,#67,#67, #f6,#61,#67,#67, #f6,#61,#66,#76, #6f,#16,#66,#76
menicn_filsylk      db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #66,#66,#66,#66, #77,#61,#6f,#6f, #76,#61,#6f,#6f, #67,#61,#6f,#f6, #67,#61,#6f,#6f, #77,#61,#1f,#6f
menicn_properties   db 4,8,7:dw $+7,$+4,28:db 5: db #16,#77,#17,#77, #66,#66,#66,#66, #16,#77,#77,#17, #66,#66,#66,#66, #86,#77,#71,#77, #66,#66,#66,#66, #16,#71,#77,#77
menicn_quit         db 4,8,7:dw $+7,$+4,28:db 5: db #11,#16,#16,#66, #14,#46,#11,#66, #14,#11,#1e,#16, #14,#1e,#ee,#e1, #14,#11,#1e,#16, #14,#46,#11,#66, #11,#16,#16,#66

menmaitxt2  db "Edit",0
menmaitxt21 db 6,128,-1:dw menicn_cut+1:          db " Cut",0
menmaitxt22 db 6,128,-1:dw menicn_copy+1:         db " Copy",0
menmaitxt23 db 6,128,-1:dw menicn_paste+1:        db " Paste",0
menmaitxt24 db 6,128,-1:dw menicn_delete+1:       db " Clear cell(s)",0
menmaitxt25 db 6,128,-1:dw menicn_selall+1:       db " Select All",0
menmaitxt26 db 6,128,-1:dw menicn_sel+1:          db " Select",0
menmaitxt261 db 6,128,-1:dw menicn_selcolumn+1:    db " Column",0
menmaitxt262 db 6,128,-1:dw menicn_selrow+1:       db " Row",0

menicn_cut          db 4,8,7:dw $+7,$+4,28:db 5: db #61,#a6,#a1,#66, #61,#a6,#a1,#66, #66,#16,#16,#66, #66,#61,#66,#66, #66,#71,#76,#66, #67,#a7,#a7,#66, #67,#76,#77,#66
menicn_copy         db 4,8,7:dw $+7,$+4,28:db 5: db #55,#55,#56,#66, #58,#88,#56,#66, #58,#88,#57,#77, #58,#88,#50,#07, #55,#55,#50,#07, #66,#67,#00,#07, #66,#67,#77,#77
menicn_paste        db 4,8,7:dw $+7,$+4,28:db 5: db #66,#33,#36,#66, #33,#22,#23,#36, #32,#22,#22,#36, #32,#55,#55,#55, #33,#58,#88,#85, #66,#58,#88,#85, #66,#55,#55,#55
menicn_delete       db 4,8,7:dw $+7,$+4,28:db 5: db #ff,#66,#66,#ff, #6f,#f6,#6f,#f6, #66,#ff,#ff,#66, #66,#6f,#f6,#66, #66,#ff,#ff,#66, #6f,#f6,#6f,#f6, #ff,#66,#66,#ff
menicn_selall       db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #1a,#a1,#aa,#16, #1a,#a1,#aa,#16, #11,#11,#11,#16, #1a,#a1,#aa,#16, #1a,#a1,#aa,#16, #11,#11,#11,#16
menicn_sel          equ menicn_null
menicn_selcolumn    db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #1a,#a1,#88,#16, #1a,#a1,#88,#16, #11,#11,#11,#16, #1a,#a1,#88,#16, #1a,#a1,#88,#16, #11,#11,#11,#16
menicn_selrow       db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #1a,#a1,#aa,#16, #1a,#a1,#aa,#16, #11,#11,#11,#16, #18,#81,#88,#16, #18,#81,#88,#16, #11,#11,#11,#16

menmaitxt3  db "View",0
menmaitxt31 db 6,128,-1:dw menicn_vwtool+1:       db " Tool bar",0
menmaitxt32 db 6,128,-1:dw menicn_vwstatus+1:     db " Status bar",0
menmaitxt33 db 6,128,-1:dw menicn_settings+1:     db " Preferences...",0

menicn_vwtool       db 4,8,7:dw $+7,$+4,28:db 5: db #61,#11,#11,#16, #17,#77,#76,#71, #13,#22,#33,#31, #13,#22,#33,#31, #18,#88,#88,#81, #18,#88,#88,#81, #18,#88,#88,#81
menicn_vwstatus     db 4,8,7:dw $+7,$+4,28:db 5: db #18,#88,#88,#81, #18,#88,#88,#81, #18,#88,#88,#81, #18,#88,#88,#81, #13,#32,#32,#31, #13,#32,#32,#31, #61,#11,#11,#16
menicn_settings     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#6c,#66,#66, #6c,#6c,#6c,#66, #6f,#cd,#cf,#66, #cc,#c1,#cc,#c6, #ff,#cc,#cf,#f6, #6c,#fc,#fc,#66, #6f,#6c,#6f,#66

menmaitxt4  db "Format",0
menmaitxt41 db 6,128,-1:dw menicn_fmstyle+1:      db " Style",0
menmaitxt411    db 6,128,-1:dw menicn_fmnormal+1:     db " Normal",0
menmaitxt412    db 6,128,-1:dw menicn_fmbold+1:       db " Bold",0
menmaitxt413    db 6,128,-1:dw menicn_fmitalic+1:     db " Italic",0
menmaitxt42 db 6,128,-1:dw menicn_fmalign+1:      db " Alignment",0
menmaitxt421    db 6,128,-1:dw menicn_fmgeneral+1:    db " General",0
menmaitxt422    db 6,128,-1:dw menicn_fmleft+1:       db " Left",0
menmaitxt423    db 6,128,-1:dw menicn_fmcenter+1:     db " Centered",0
menmaitxt424    db 6,128,-1:dw menicn_fmright+1:      db " Right",0
menmaitxt43 db 6,128,-1:dw menicn_fmformat+1:         db " Number format",0
menmaitxt431    db 6,128,-1:dw menicn_fmnumber+1:     db " Number",0
menmaitxt432    db 6,128,-1:dw menicn_fmdate+1:       db " Date",0
menmaitxt433    db 6,128,-1:dw menicn_fmtime+1:       db " Time",0
menmaitxt434    db 6,128,-1:dw menicn_fmpercent+1:    db " Percentage",0
menmaitxt435    db 6,128,-1:dw menicn_fmscient+1:     db " Scientific",0
menmaitxt436    db 6,128,-1:dw menicn_fmbool+1:       db " Boolean",0
menmaitxt437    db 6,128,-1:dw menicn_fmbin+1:        db " Binary",0
menmaitxt438    db 6,128,-1:dw menicn_fmhex+1:        db " Hexadecimal",0
menmaitxt439    db 6,128,-1:dw menicn_fm1000sep+1:    db " 1000 separator",0
menmaitxt44 db 6,128,-1:dw menicn_fmcell+1:       db " Cells...",0
menmaitxt45 db 6,128,-1:dw menicn_selcolumn+1:    db " Columns",0
menmaitxt451    db 6,128,-1:dw menicn_sizcolumn+1:    db " Width...",0
menmaitxt46 db 6,128,-1:dw menicn_selrow+1:       db " Rows",0
menmaitxt461    db 6,128,-1:dw menicn_sizrow+1:       db " Height...",0

menicn_fmnormal     db 4,8,7:dw $+7,$+4,28:db 5: db #11,#16,#61,#11, #61,#16,#66,#16, #61,#61,#66,#16, #61,#66,#16,#16, #61,#66,#61,#16, #61,#66,#66,#16, #11,#16,#61,#11
menicn_fmbold       db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #11,#16,#61,#11, #11,#16,#61,#11, #11,#11,#11,#16, #11,#16,#61,#11, #11,#16,#61,#11, #11,#11,#11,#16
menicn_fmitalic     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#11,#11, #66,#66,#61,#16, #66,#66,#11,#66, #66,#61,#16,#66, #66,#11,#66,#66, #61,#16,#66,#66, #11,#11,#66,#66
menicn_fmstyle      equ menicn_fmnormal

menicn_fmgeneral    db 4,8,7:dw $+7,$+4,28:db 5: db #61,#11,#11,#16, #66,#66,#66,#66, #61,#11,#11,#16, #66,#66,#66,#66, #61,#11,#11,#16, #66,#66,#66,#66, #61,#11,#11,#16
menicn_fmleft       db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #66,#66,#66,#66, #11,#11,#66,#66, #66,#66,#66,#66, #11,#11,#11,#16, #66,#66,#66,#66, #11,#11,#66,#66
menicn_fmcenter     db 4,8,7:dw $+7,$+4,28:db 5: db #61,#11,#11,#16, #66,#66,#66,#66, #66,#11,#11,#66, #66,#66,#66,#66, #61,#11,#11,#16, #66,#66,#66,#66, #66,#11,#11,#66
menicn_fmright      db 4,8,7:dw $+7,$+4,28:db 5: db #61,#11,#11,#11, #66,#66,#66,#66, #66,#66,#11,#11, #66,#66,#66,#66, #61,#11,#11,#11, #66,#66,#66,#66, #66,#66,#11,#11
menicn_fmalign      equ menicn_fmgeneral

menicn_fmnumber     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #56,#66,#61,#16, #56,#77,#66,#61, #56,#66,#76,#11, #56,#67,#66,#61, #56,#76,#61,#16, #66,#77,#76,#66
menicn_fmdate       db 4,8,7:dw $+7,$+4,28:db 5: db #18,#88,#88,#16, #18,#78,#88,#01, #18,#78,#78,#16, #18,#77,#78,#01, #18,#88,#78,#16, #18,#88,#88,#01, #6d,#dd,#dd,#16
menicn_fmtime       db 4,8,7:dw $+7,$+4,28:db 5: db #66,#77,#77,#66, #67,#8e,#18,#76, #78,#88,#18,#87, #7e,#81,#88,#e7, #78,#18,#88,#87, #67,#88,#e8,#76, #66,#77,#77,#66
menicn_fmpercent    db 4,8,7:dw $+7,$+4,28:db 5: db #11,#66,#61,#16, #11,#66,#11,#16, #66,#61,#11,#66, #66,#11,#16,#66, #61,#11,#66,#66, #11,#16,#61,#16, #11,#66,#61,#16
menicn_fmscient     db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #63,#61,#16,#76, #33,#61,#66,#77, #63,#61,#16,#77, #63,#61,#66,#67, #63,#61,#16,#66, #66,#66,#66,#66
menicn_fmbool       db 4,8,7:dw $+7,$+4,28:db 5: db #f6,#66,#f6,#66, #ff,#6f,#f6,#69, #6f,#ff,#66,#99, #ff,#6f,#f9,#99, #f9,#96,#f9,#96, #69,#99,#99,#66, #66,#99,#96,#66
menicn_fmbin        db 4,8,7:dw $+7,$+4,28:db 5: db #76,#77,#76,#76, #76,#76,#76,#76, #76,#77,#76,#76, #66,#66,#66,#66, #77,#76,#77,#76, #76,#76,#76,#76, #77,#76,#77,#76
menicn_fmhex        db 4,8,7:dw $+7,$+4,28:db 5: db #66,#66,#66,#66, #66,#66,#66,#66, #61,#61,#16,#11, #16,#61,#66,#16, #16,#61,#16,#11, #16,#61,#66,#16, #61,#61,#16,#16
menicn_fm1000sep    db 4,8,7:dw $+7,$+4,28:db 5: db #63,#66,#63,#36, #33,#66,#36,#63, #63,#66,#36,#63, #63,#66,#36,#63, #63,#66,#36,#63, #63,#66,#63,#36, #66,#61,#66,#66
menicn_fmformat     equ menicn_fmnumber

menicn_fmcell       db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#11, #18,#88,#ff,#81, #18,#8f,#8f,#81, #18,#f8,#8f,#81, #18,#ff,#ff,#81, #18,#f8,#8f,#81, #11,#11,#11,#11
menicn_sizcolumn    db 4,8,7:dw $+7,$+4,28:db 5: db #66,#11,#11,#66, #66,#18,#81,#66, #f6,#18,#81,#6f, #ff,#11,#11,#ff, #f6,#18,#81,#6f, #66,#18,#81,#66, #66,#11,#11,#66
menicn_sizrow       db 4,8,8:dw $+7,$+4,32:db 5: db #66,#ff,#f6,#66, #66,#6f,#66,#66, #11,#11,#11,#16, #18,#81,#88,#16, #18,#81,#88,#16, #11,#11,#11,#16, #66,#6f,#66,#66, #66,#ff,#f6,#66

menmaitxt5  db "Table",0
menmaitxt51 db 6,128,-1:dw menicn_celinsert+1:    db " Insert cell(s)",0
menmaitxt511    db 6,128,-1:dw menicn_celdown+1:      db " Move cells down",0
menmaitxt512    db 6,128,-1:dw menicn_celright+1:     db " Move cells right",0
menmaitxt513    db 6,128,-1:dw menicn_rowdown+1:      db " Insert whole row(s)",0
menmaitxt514    db 6,128,-1:dw menicn_clmright+1:     db " Insert whole column(s)",0
menmaitxt52 db 6,128,-1:dw menicn_rowdown+1:      db " Insert row(s)",0
menmaitxt53 db 6,128,-1:dw menicn_clmright+1:     db " Insert column(s)",0
menmaitxt54 db 6,128,-1:dw menicn_celremove+1:    db " Remove cell(s)",0
menmaitxt541    db 6,128,-1:dw menicn_celup+1:        db " Move cells up",0
menmaitxt542    db 6,128,-1:dw menicn_celleft+1:      db " Move cells left",0
menmaitxt543    db 6,128,-1:dw menicn_rowup+1:        db " Remove whole row(s)",0
menmaitxt544    db 6,128,-1:dw menicn_clmleft+1:      db " Remove whole column(s)",0
menmaitxt55 db 6,128,-1:dw menicn_rowup+1:        db " Remove row(s)",0
menmaitxt56 db 6,128,-1:dw menicn_clmleft+1:      db " Remove column(s)",0
menmaitxt57 db 6,128,-1:dw menicn_delete+1:       db " Clear cell content",0

menicn_celinsert    db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #18,#88,#88,#16, #18,#87,#88,#16, #18,#77,#78,#16, #18,#87,#88,#16, #18,#88,#88,#16, #11,#11,#11,#16
menicn_celright     db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #18,#71,#aa,#16, #17,#77,#aa,#16, #11,#71,#11,#16, #18,#81,#88,#16, #18,#81,#88,#16, #11,#11,#11,#16
menicn_celdown      db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #18,#71,#88,#16, #17,#77,#88,#16, #11,#71,#11,#16, #1a,#a1,#88,#16, #1a,#a1,#88,#16, #11,#11,#11,#16
menicn_clmright     db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #18,#81,#aa,#16, #18,#81,#aa,#16, #11,#71,#11,#16, #17,#77,#aa,#16, #18,#71,#aa,#16, #11,#11,#11,#16
menicn_rowdown      db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #18,#81,#78,#16, #18,#87,#77,#16, #11,#11,#71,#16, #1a,#a1,#aa,#16, #1a,#a1,#aa,#16, #11,#11,#11,#16

menicn_celremove    db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #18,#88,#88,#16, #18,#88,#88,#16, #18,#ff,#f8,#16, #18,#88,#88,#16, #18,#88,#88,#16, #11,#11,#11,#16
menicn_celleft      db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #18,#81,#ff,#16, #12,#21,#ff,#16, #11,#11,#11,#16, #18,#81,#88,#16, #18,#81,#88,#16, #11,#11,#11,#16
menicn_celup        db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #12,#21,#88,#16, #18,#81,#88,#16, #11,#11,#11,#16, #1f,#f1,#88,#16, #1f,#f1,#88,#16, #11,#11,#11,#16
menicn_clmleft      db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #18,#81,#ff,#16, #18,#81,#ff,#16, #11,#11,#11,#16, #12,#21,#ff,#16, #18,#81,#ff,#16, #11,#11,#11,#16
menicn_rowup        db 4,8,7:dw $+7,$+4,28:db 5: db #11,#11,#11,#16, #18,#81,#22,#16, #18,#81,#88,#16, #11,#11,#11,#16, #1f,#f1,#ff,#16, #1f,#f1,#ff,#16, #11,#11,#11,#16

;menmaitxt6  db "Data",0

menmaitxt7  db "?",0
menmaitxt71 db 6,128,-1:dw menicn_help+1:         db " Help topics",0
menmaitxt72 db 6,128,-1:dw menicn_about+1:        db " About",0

menicn_help         db 4,8,7:dw $+7,$+4,28:db 5: db #66,#1f,#f1,#66, #61,#fc,#cf,#16, #1f,#ff,#fc,#f1, #ff,#fc,#cc,#f1, #ff,#ff,#ff,#18, #1f,#cf,#f1,#81, #61,#ff,#18,#16
menicn_about        db 4,8,7:dw $+7,$+4,28:db 5: db #66,#10,#07,#66, #66,#10,#07,#66, #66,#66,#66,#66, #61,#00,#07,#66, #66,#10,#07,#66, #66,#10,#07,#66, #61,#00,#00,#76

;menmaitxtx  db 6,128,-1:dw menicn_null+1:         db " Coming soon...",0

if 0
menicn_print        db 4,8,7:dw $+7,$+4,28:db 5: db #66,#61,#11,#11, #66,#18,#d8,#16, #61,#8d,#81,#66, #11,#11,#11,#16, #1a,#aa,#0a,#76, #1a,#9a,#9a,#76, #67,#77,#77,#66
menicn_find         db 4,8,7:dw $+7,$+4,28:db 5: db #66,#16,#61,#66, #61,#71,#17,#16, #61,#71,#17,#16, #17,#71,#17,#71, #18,#16,#61,#81, #17,#16,#61,#71, #11,#16,#61,#11
menicn_findagain    db 4,8,7:dw $+7,$+4,28:db 5: db #61,#66,#16,#66, #67,#11,#76,#f6, #17,#11,#71,#f6, #18,#11,#81,#f6, #17,#66,#71,#f6, #11,#66,#1f,#ff, #66,#66,#66,#f6
menicn_replace      db 4,8,7:dw $+7,$+4,28:db 5: db #66,#61,#11,#16, #66,#61,#88,#11, #33,#33,#88,#81, #33,#63,#38,#81, #33,#33,#88,#81, #33,#63,#31,#11, #33,#63,#36,#66
endif

;context
ctxclmtxt1  db 6,128,-1:dw menicn_sizcolumn+1:    db " Column width...",0
ctxclmtxt2  db 6,128,-1:dw menicn_selcolumn+1:    db " Mark this column",0
ctxclmtxt3  db 6,128,-1:dw menicn_selall+1:       db " Mark column range",0
ctxclmtxt4  db 6,128,-1:dw menicn_clmright+1:     db " Insert column",0
ctxclmtxt5  db 6,128,-1:dw menicn_clmleft+1:      db " Remove column",0

ctxrowtxt1  db 6,128,-1:dw menicn_sizrow+1:       db " Row height...",0
ctxrowtxt2  db 6,128,-1:dw menicn_selrow+1:       db " Mark this row",0
ctxrowtxt3  db 6,128,-1:dw menicn_selall+1:       db " Mark row range",0
ctxrowtxt4  db 6,128,-1:dw menicn_rowdown+1:      db " Insert row",0
ctxrowtxt5  db 6,128,-1:dw menicn_rowup+1:        db " Remove row",0

ctxceltxt1  equ menmaitxt21     ;"Cut"
ctxceltxt2  equ menmaitxt22     ;"Copy"
ctxceltxt3  equ menmaitxt23     ;"Paste"

ctxceltxt4  equ menmaitxt51     ;"Insert cell(s)"
ctxceltxt4a equ menmaitxt511    ;"Move cells down"
ctxceltxt4b equ menmaitxt512    ;"Move cells right"
ctxceltxt4c equ menmaitxt513    ;"Insert whole row(s)"
ctxceltxt4d equ menmaitxt514    ;"Insert whole column(s)"

ctxceltxt5  equ menmaitxt54     ;"Remove cell(s)",0
ctxceltxt5a equ menmaitxt541    ;"Move cells up",0
ctxceltxt5b equ menmaitxt542    ;"Move cells left",0
ctxceltxt5c equ menmaitxt543    ;"Remove whole row(s)",0
ctxceltxt5d equ menmaitxt544    ;"Remove whole column(s)",0

ctxceltxt6  equ menmaitxt24     ;"Clear cell(s)",0
ctxceltxt7  db 6,128,-1:dw menicn_fmcell+1:       db " Format cell(s)...",0



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
        ds 64
prgstk2a dw 0

        ds 128
prgstk2 ds 6*2
        dw prgprz
App_PrcID  db 0
App_MsgBuf ds 14

;### ALERT WINDOWS ############################################################

prgerrinf
errforinf0  equ 00: dw errfortxta,4*1+2,errfortxt0,4*1+2,prgtxtinf0,4*1+2       ;formula
errforinf1  equ 01: dw errfortxta,4*1+2,errfortxt1,4*1+2,prgtxtinf0,4*1+2
errforinf2  equ 02: dw errfortxta,4*1+2,errfortxt2,4*1+2,prgtxtinf0,4*1+2
errforinf3  equ 03: dw errfortxta,4*1+2,errfortxt3,4*1+2,prgtxtinf0,4*1+2
errforinf4  equ 04: dw errfortxta,4*1+2,errfortxt4,4*1+2,prgtxtinf0,4*1+2
errforinf5  equ 05: dw errfortxta,4*1+2,errfortxt5,4*1+2,prgtxtinf0,4*1+2
errforinf6  equ 06: dw errfortxta,4*1+2,errfortxt6,4*1+2,prgtxtinf0,4*1+2
errforinf7  equ 07: dw errfortxta,4*1+2,errfortxt7,4*1+2,prgtxtinf0,4*1+2
errforinfa  equ 08: dw errfortxta1,4*1+2,errfortxta2,4*1+2,prgtxtinf0,4*1+2     ;recalc
errcopinfa  equ 09: dw errcoptxta1,4*1+2,errcoptxta2,4*1+2,prgtxtinf0,4*1+2     ;copy
errmeminfa  equ 10: dw errmemtxta1,4*1+2,errmemtxta2,4*1+2,prgtxtinf0,4*1+2     ;memory
errmeminfb  equ 11: dw errmemtxta1,4*1+2,errmemtxtb2,4*1+2,prgtxtinf0,4*1+2
errmeminfc  equ 12: dw errmemtxta1,4*1+2,errmemtxtc2,4*1+2,prgtxtinf0,4*1+2
errmeminfd  equ 13: dw errmemtxta1,4*1+2,errmemtxtd2,4*1+2,prgtxtinf0,4*1+2

;error - load/save
prgerrinfa  equ 14: dw prgerrtxt1a,4*1+2,prgerrtxt2a,4*1+2,prgerrtxt3a,4*1+2
prgerrinfb  equ 15: dw prgerrtxt1a,4*1+2,prgerrtxt2b,4*1+2,prgerrtxt3b,4*1+2
prgerrinfc  equ 16: dw prgerrtxt1a,4*1+2,prgerrtxt2c,4*1+2,prgerrtxt3c,4*1+2
prgerrinfd  equ 17: dw prgerrtxt1a,4*1+2,prgerrtxt2d,4*1+2,prgerrtxt3d,4*1+2
prgerrinfe  equ 18: dw prgerrtxt1a,4*1+2,prgerrtxt2e,4*1+2,prgerrtxt3e,4*1+2

                    dw prgerrtxt1b,4*1+2,prgerrtxt2a,4*1+2,prgerrtxt3a,4*1+2
                    dw prgerrtxt1b,4*1+2,prgerrtxt2b,4*1+2,prgerrtxt3b,4*1+2
                    dw prgerrtxt1b,4*1+2,prgerrtxt2c,4*1+2,prgerrtxt3c,4*1+2
                    dw prgerrtxt1b,4*1+2,prgerrtxt2d,4*1+2,prgerrtxt3d,4*1+2
                    dw prgerrtxt1b,4*1+2,prgerrtxt2e,4*1+2,prgerrtxt3e,4*1+2

prgmsgsav   equ 24: dw prgtxtsav1,4*1+2,prgtxtinf0,4*1+2,prgtxtinf0,4*1+2
prgmsginf   equ 25: dw prgtxtinf1,4*1+2,prgtxtinf2,4*1+2,prgtxtinf3,4*1+2,0,prgicnbig,prgicn16c


;==============================================================================
;### PULL DOWN / CONTEXT MENUS ################################################
;==============================================================================

;main
menmaidat   dw 6, 1+4,menmaitxt1, menmaidat1,0
            dw    1+4,menmaitxt2, menmaidat2,0
            dw    1+4,menmaitxt3, menmaidat3,0
            dw    1+4,menmaitxt4, menmaidat4,0
            dw    1+4,menmaitxt5, menmaidat5,0
            ;dw    1+4,menmaitxt6, menmaidat6,0
            dw    1+4,menmaitxt7, menmaidat7,0

menmaidat1  dw 11,17,menmaitxt11,mai_filnew,0, 17,menmaitxt12,mai_filopn,0, 17,menmaitxt13,mai_filsav,0, 17,menmaitxt14,mai_filsas,0, 1+8,0,0,0, 17+4,menmaitxt15,menmaidat15,0, 17+4,menmaitxt16,menmaidat16,0
            dw 1+8,0,0,0, 17,menmaitxt17,mai_prpopn,0, 1+8,0,0,0, 17,menmaitxt10,mai_prgend0,0
menmaidat15 dw 2, 17,menmaitxt151,mai_filimc,0,17,menmaitxt152,mai_filims,0
menmaidat16 dw 2, 17,menmaitxt151,mai_filexc,0,17,menmaitxt152,mai_filexs,0

menmaidat2  dw 8 ,17,menmaitxt21,mai_copcut,0, 17,menmaitxt22,mai_copcop,0
menmaidat2a dw    16,menmaitxt23,mai_coppst,0, 1+8,0,0,0, 17,menmaitxt24,mai_fldclr,0, 1+8,0,0,0, 17,menmaitxt25,mai_mrkall,0, 17+4,menmaitxt26,menmaidat26,0
menmaidat26 dw 2, 17,menmaitxt261,mai_mencsl,0,17,menmaitxt262,mai_menrsl,0

menmaidat3  dw 4
menmaidat30 dw 17+2, menmaitxt31,mai_mentol,0
menmaidat31 dw 17+2, menmaitxt32,mai_mensta,0, 1+8,0,0,0, 17,menmaitxt33,mai_cfgopn,0

menmaidat4  dw 7, 17+4,menmaitxt41,menmaidat41,0,17+4,menmaitxt42,menmaidat42,0,17+4,menmaitxt43,menmaidat43,0, 1+8,0,0,0, 17,menmaitxt44 ,mai_cfdopn,0,17+4,menmaitxt45,menmaidat45,0,17+4,menmaitxt46,menmaidat46,0
menmaidat41 dw 3, 17,menmaitxt411,mai_mensnr,0,17,menmaitxt412,mai_mensbl,0,17,menmaitxt413,mai_mensit,0
menmaidat42 dw 4, 17,menmaitxt421,mai_menagn,0,17,menmaitxt422,mai_menalf,0,17,menmaitxt423,mai_menacn,0, 17,menmaitxt424,mai_menarg,0
menmaidat43 dw 10,17,menmaitxt431,mai_mennfl,0,17,menmaitxt432,mai_menndt,0,17,menmaitxt433,mai_menntm,0, 17,menmaitxt434,mai_mennpr,0,17,menmaitxt435,mai_mennex,0
            dw    17,menmaitxt436,mai_mennbl,0,17,menmaitxt437,mai_mennbn,0,17,menmaitxt438,mai_mennhx,0, 17+8,0,0,0, 17,menmaitxt439,mai_mensep,0
menmaidat45 dw 1, 17,menmaitxt451,mai_mencsz,0
menmaidat46 dw 1, 17,menmaitxt461,mai_menrsz,0

menmaidat5  dw 9, 17+4,menmaitxt51,menmaidat51,0, 17,menmaitxt52,mai_movrwi,   0, 17,menmaitxt53,mai_movcli,0, 1+8,0,0,0, 17+4,menmaitxt54,menmaidat54,0, 17,menmaitxt55,mai_movrwr,0, 17,menmaitxt56,mai_movclr,0, 1+8,0,0,0, 17,menmaitxt57,mai_fldclr,0
menmaidat51 dw 4, 17,menmaitxt511, mai_movcid, 0, 17,menmaitxt512, mai_movcir, 0, 17,menmaitxt513, mai_movrwi, 0, 17,menmaitxt514, mai_movcli, 0
menmaidat54 dw 4, 17,menmaitxt541, mai_movcru, 0, 17,menmaitxt542, mai_movcrl, 0, 17,menmaitxt543, mai_movrwr, 0, 17,menmaitxt544, mai_movclr, 0

;menmaidat6  dw 1, 1,menmaitxtx ,000000,0

menmaidat7  dw 3, 17,menmaitxt71,mai_prghlp,0, 1+8,0,0,0, 17,menmaitxt72,mai_prginf,0


;column bar
ctxclmdat   dw 7
dw 17,ctxclmtxt1, barmsc, 0
dw 8,0,0,0
dw 17,ctxclmtxt2, barmcs, 0
dw 17,ctxclmtxt3, barmcm, 0
dw 8,0,0,0
dw 17,ctxclmtxt4, barmci, 0
dw 17,ctxclmtxt5, barmcr, 0

;row bar
ctxrowdat   dw 7
dw 17,ctxrowtxt1, barmsr, 0
dw 8,0,0,0
dw 17,ctxrowtxt2, barmrs, 0
dw 17,ctxrowtxt3, barmrm, 0
dw 8,0,0,0
dw 17,ctxrowtxt4, barmri, 0
dw 17,ctxrowtxt5, barmrr, 0

;cells
ctxceldat   dw 9
dw 17,ctxceltxt1, cmncut, 0
dw 17,ctxceltxt2, cmncop, 0
dw 17,ctxceltxt3, cmnpst, 0
dw 8,0,0,0
dw 16+5,ctxceltxt4, ctxceldat4, 0
dw 16+5,ctxceltxt5, ctxceldat5, 0
dw 17,ctxceltxt6, cmnclr, 0
dw 8,0,0,0
dw 17,ctxceltxt7, cmnfmt, 0

ctxceldat4  dw 4
dw 17,ctxceltxt4a, cmncid, 0
dw 17,ctxceltxt4b, cmncir, 0
dw 17,ctxceltxt4c, cmnrwi, 0
dw 17,ctxceltxt4d, cmncmi, 0

ctxceldat5  dw 4
dw 17,ctxceltxt5a, cmncru, 0
dw 17,ctxceltxt5b, cmncrl, 0
dw 17,ctxceltxt5c, cmnrwr, 0
dw 17,ctxceltxt5d, cmncmr, 0


;==============================================================================
;### COLOUR DROPDOWN ##########################################################
;==============================================================================

wincoldat   dw #0001,4+8+16, 0,0, 63, 77,0,0,  63,  77,63,77,63,77,  0,0,0,0,wincolgrp,0,0:ds 136+14
wincolgrp   db 21,0:dw wincolrec,0,0,21*256+00,0,0,00
wincolrec
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0   ;00=Background

wincolrec_sel   equ 01
dw _fmtctb ,255*256+20,ctrcoltab,  00, 02, 63,11,0       ;01=Tabs
wincolrec1
dw       0,255*256+02,256*102+128+00+00, 01,15,16,16,0   ;02=unselect
dw       0,255*256+02,256*17 +128+00+00, 01,15,16,16,0   ;03=selected

dw _fmtc00 ,255*256+02,256*120+128+64+00, 03,17,12,12,0  ;04=col 00
dw _fmtc01 ,255*256+02,256*120+128+64+01, 18,17,12,12,0  ;05=col 01
dw _fmtc02 ,255*256+02,256*120+128+64+02, 33,17,12,12,0  ;06=col 02
dw _fmtc03 ,255*256+02,256*120+128+64+03, 48,17,12,12,0  ;07=col 03
dw _fmtc04 ,255*256+02,256*120+128+64+04, 03,32,12,12,0  ;08=col 04
dw _fmtc05 ,255*256+02,256*120+128+64+05, 18,32,12,12,0  ;09=col 05
dw _fmtc06 ,255*256+02,256*120+128+64+06, 33,32,12,12,0  ;10=col 06
dw _fmtc07 ,255*256+02,256*120+128+64+07, 48,32,12,12,0  ;11=col 07
dw _fmtc08 ,255*256+02,256*120+128+64+08, 03,47,12,12,0  ;12=col 08
dw _fmtc09 ,255*256+02,256*120+128+64+09, 18,47,12,12,0  ;13=col 09
dw _fmtc10 ,255*256+02,256*120+128+64+10, 33,47,12,12,0  ;14=col 10
dw _fmtc11 ,255*256+02,256*120+128+64+11, 48,47,12,12,0  ;15=col 11
dw _fmtc12 ,255*256+02,256*120+128+64+12, 03,62,12,12,0  ;16=col 12
dw _fmtc13 ,255*256+02,256*120+128+64+13, 18,62,12,12,0  ;17=col 13
dw _fmtc14 ,255*256+02,256*120+128+64+14, 33,62,12,12,0  ;18=col 14
dw _fmtc15 ,255*256+02,256*120+128+64+15, 48,62,12,12,0  ;19=col 15

dw _fmtces ,255*256+19,0                , -2,-2, 1, 1,0  ;20=escape

ctrcoltab   db 2,2+4+48+64
ctrcoltab0  db 0:dw txtcolpen:db -1:dw txtcolpap:db -1


;==============================================================================
;### FORMAT DIALOGUE ##########################################################
;==============================================================================

wincfddat   dw #1401,4+16,50,2,200,162,0,0,200,162,200,162,200,162,prgicnsml,txtcfdtit,0,0,wincfdgrp,0,0:ds 136+14
wincfdgrp   db wincfdreca_cnt,0:dw wincfdreca,0,0,05*256+04,0,0,00
;title

;main tabs
ctrcfdtab   db 3,2+4+48+64
ctrcfdtab0  db 0:dw txtcfdnum:db -1:dw txtcfdaln:db -1:dw txtcfdfnt:db -1

;preview
ctrcfdpfr   dw txtcfdpfr,2+4

ctrcfdprv   dw txtcfdprv,256*193+8+16,0

;*** number *******************************************************************

wincfdreca_cnt  equ 18
wincfdreca
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0  ;00=Background
dw _cfdtab,255*256+20,ctrcfdtab,  00, 02, 200,11,0      ;01=Tabs
dw       0,255*256+00,128+1    ,  00,143,10000,1,0      ;02=bottom line
dw _cfdoky,255*256+16,prgtxtok,   96,147,  48,12,0      ;03="Ok"     -Button
dw _wincnc,255*256+16,prgtxtcnc, 147,147,  48,12,0      ;04="Cancel" -Button
dw       0,255*256+ 3,ctrcfdpfr,  89, 25, 109,28,0      ;05=Frame Preview
wincfdrec_prev  equ 6
dw       0,255*256+00,128+8    ,  97, 35,  93,10,0      ;06=Bckgr Preview
wincfdreca0
dw       0,255*256+ 1,ctrcfdprv,  97, 36,  93, 8,0      ;07=Text  Preview

dw       0,255*256+ 1,ctrcfdclb,   5, 18,  80, 8,0      ;08=Label Category
dw _cfdmod,255*256+41,ctrcfdcat,   5, 28,  80,84,0      ;09=List  Category

wincfdreca_opt1 equ 10
wincfdreca1
dw       0,255*256+00,000+2    ,  92, 55, 103,57,0      ;10=Hide  Types
dw       0,255*256+ 1,ctrcfdtyt,  92, 55, 103, 8,0      ;11=Label Types
dw _cfdprv,255*256+41,ctrcfdtyp,  92, 65, 103,47,0      ;12=List  Types

wincfdreca_opt2 equ 13
wincfdreca2
dw       0,255*256+00,000+2    ,   2,115, 196,27,0      ;13=Hide  Options
dw       0,255*256+ 3,ctrcfdopt,   2,115, 196,27,0      ;14=Frame Options
dw       0,255*256+ 1,ctrcfddpt,   5,125,  72, 8,0      ;15=Text  Decimal places
dw _cfdprv,255*256+42,ctrcfddpl,  81,124,  24,10,0      ;16=List  Decimal places
dw _cfdprv,255*256+17,ctrcfdsep, 120,125,  75, 8,0      ;17=Check 1000 separator

;category
ctrcfdclb   dw txtcfdclb,2+4

ctrcfdcat   dw 9,00,lstcfdcat,0,1,rowcfdcat,0,1
rowcfdcat   dw 0,96,0,0
lstcfdcat
dw 00,txtcfdcat0, 03,txtcfdcat1, 04,txtcfdcat2, 02,txtcfdcat3
dw 01,txtcfdcat4, 05,txtcfdcat5, 06,txtcfdcat6, 07,txtcfdcat7
dw 08,txtcfdcat8

;units/types
ctrcfdtyt   dw txtcfdtyt1,2+4

ctrcfdtyp   dw 8,00,lstcfduni,0,1,rowcfdtyp,0,1
rowcfdtyp   dw 0,96,0,0

lstcfduni
dw 00,lstcfduni0, 01,lstcfduni1, 02,lstcfduni2, 03,lstcfduni3
dw 04,lstcfduni4, 05,lstcfduni5, 06,lstcfduni6, 07,lstcfduni7
lstcfduni0  ds 32
lstcfduni1  ds 32
lstcfduni2  ds 32
lstcfduni3  ds 32
lstcfduni4  ds 32
lstcfduni5  ds 32
lstcfduni6  ds 32
lstcfduni7  ds 32

;options
ctrcfdopt   dw txtcfdopt,2+4

;decimal places
ctrcfddpt   dw txtcfddpt1,256*1+2+4

ctrcfddpl   dw 8,00,lstcfddpl,0,1,rowcfddpl,0,1
rowcfddpl   dw 0,99,0,0

lstcfddpl
dw 07,txtcfddpl7, 00,txtcfddpl0, 01,txtcfddpl1, 02,txtcfddpl2
dw 03,txtcfddpl3, 04,txtcfddpl4, 05,txtcfddpl5, 06,txtcfddpl6

lstcfddbl
dw 07,txtcfddbl7, 00,txtcfddbl0, 01,txtcfddbl1, 02,txtcfddbl2
dw 03,txtcfddbl3, 04,txtcfddbl4, 05,txtcfddbl5, 06,txtcfddbl6

lstcfddxl
dw 07,txtcfddxl7, 00,txtcfddxl0, 01,txtcfddxl1, 02,txtcfddxl2
dw 03,txtcfddxl3, 04,txtcfddxl4, 05,txtcfddxl5, 06,txtcfddxl6

;1000 separator
ctrcfdsep   dw flgcfdsep,txtcfdsep,2+4
flgcfdsep   db 0

;*** alignment ****************************************************************

wincfdrecb_cnt  equ 12
wincfdrecb
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0  ;00=Background
dw _cfdtab,255*256+20,ctrcfdtab,  00, 02, 200,11,0      ;01=Tabs
dw       0,255*256+00,128+1    ,  00,143,10000,1,0      ;02=bottom line
dw _cfdoky,255*256+16,prgtxtok,   96,147,  48,12,0      ;03="Ok"     -Button
dw _wincnc,255*256+16,prgtxtcnc, 147,147,  48,12,0      ;04="Cancel" -Button
dw       0,255*256+ 3,ctrcfdpfr,  89, 25, 109,28,0      ;05=Frame Preview
dw       0,255*256+00,128+8    ,  97, 35,  93,10,0      ;06=Bckgr Preview
wincfdrecb0
dw       0,255*256+ 1,ctrcfdprv,  97, 36,  93, 8,0      ;07=Text  Preview

dw       0,255*256+ 1,ctrcfdahl,   5, 18,  80, 8,0      ;08=Label Horizontal
dw _cfdprv,255*256+41,ctrcfdalh,   5, 28,  80,36,0      ;09=List  Horizontal

dw       0,255*256+ 1,ctrcfdavl,   5, 72,  80, 8,0      ;10=Label Vertical
dw       0,255*256+41,ctrcfdalv,   5, 82,  80,36,0      ;11=List  Vertical

;horizontal
ctrcfdahl   dw txtcfdahl,2+4

ctrcfdalh   dw 4,00,lstcfdalh,0,1,rowcfdalh,0,1
rowcfdalh   dw 0,99,0,0
lstcfdalh
dw 00,txtcfdalh0, 01,txtcfdalh1, 03,txtcfdalh2, 02,txtcfdalh3

;vertical
ctrcfdavl   dw txtcfdavl,2+4

ctrcfdalv   dw 1,00,lstcfdalv,0,1,rowcfdalv,0,1
rowcfdalv   dw 0,99,0,0
lstcfdalv
dw 32768,txtcfdalv0

;*** font & colours ***********************************************************

wincfdrecc_cnt  equ 22
wincfdrecc
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0  ;00=Background
dw _cfdtab,255*256+20,ctrcfdtab,  00, 02, 200,11,0      ;01=Tabs
dw       0,255*256+00,128+1    ,  00,143,10000,1,0      ;02=bottom line
dw _cfdoky,255*256+16,prgtxtok,   96,147,  48,12,0      ;03="Ok"     -Button
dw _wincnc,255*256+16,prgtxtcnc, 147,147,  48,12,0      ;04="Cancel" -Button
dw       0,255*256+ 3,ctrcfdpfr,  89, 25, 109,28,0      ;05=Frame Preview
dw       0,255*256+00,128+8    ,  97, 35,  93,10,0      ;06=Bckgr Preview
wincfdrecc0
dw       0,255*256+ 1,ctrcfdprv,  97, 36,  93, 8,0      ;07=Text  Preview

dw       0,255*256+ 1,ctrcfdstt,   5, 18,  80, 8,0      ;08=Label Style
dw _cfdprv,255*256+41,ctrcfdstl,   5, 28,  80,28,0      ;09=List  Style

dw       0,255*256+ 1,ctrcfdcot,   5, 62,  80, 8,0      ;10=Label  Colour
dw       0,255*256+ 2,1+4      ,   5, 72,  80,67,0      ;11=Frame  Colour
dw       0,255*256+25,ctrcfdcog,  13, 74,  63,63,0      ;12=Select Colour
wincfdrecc_pen  equ 13
wincfdrecc1
dw       0,255*256+02,256*102+128,13, 74,  16,16,0      ;13=unselect
dw       0,255*256+02,256*17 +128,13, 74,  16,16,0      ;14=selected
dw _cfdpen,255*256+19,0        ,  13, 74,  63,63,0      ;15=Click  Colour

dw       0,255*256+17,ctrcfdbgr,  92, 62,  80, 8,0      ;16=Label  Background
dw       0,255*256+ 2,1+4      ,  92, 72,  80,67,0      ;17=Frame  Background
dw       0,255*256+25,ctrcfdcog, 100, 74,  63,63,0      ;18=Select Background
wincfdrecc_pap  equ 19
wincfdrecc2
dw       0,255*256+02,256*102+128,100,74,  16,16,0      ;19=unselect
dw       0,255*256+02,256*17 +128,100,74,  16,16,0      ;20=selected
dw _cfdpap,255*256+19,0        , 100, 74,  63,63,0      ;21=Click  Background

ctrcfdstt   dw txtcfdstt,2+4

;style
ctrcfdstl   dw 3,00,lstcfdstl,0,1,rowcfdstl,0,1
rowcfdstl   dw 0,99,0,0
lstcfdstl
dw 00,txtcfdstl0, 01,txtcfdstl1, 02,txtcfdstl2

;colour
ctrcfdcot   dw txtcfdcot,2+4
ctrcfdcog   dw wincolgrp,63,63,0,14,0,0

;background
ctrcfdbgr   dw flgcfdbgr,txtcfdbgr,2+4
flgcfdbgr   db 0


;==============================================================================
;### COLUMN/ROW SIZE DIALOGUE #################################################
;==============================================================================

winsizdat   dw #1401,4+16,50,50,120,40,0,0,120,40,120,40,120,40,prgicnsml
winsizdat1  dw ctxclmtxt1,0,0,winsizgrp,0,0:ds 136+14
winsizgrp   db 6,0:dw winsizrec,0,0,06*256+05,0,0,00
winsizrec
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0  ;00=Background
dw       0,255*256+ 1,ctrsizlab,  08, 08, 54, 8,0       ;01=Label "width"
winsizrec_inp equ 2
dw       0,255*256+32,ctrsizinp,  38, 06, 30,12,0       ;02=Input "width"
dw _barsdia,255*256+16,prgtxtdef, 70, 06, 40,12,0       ;03="Default"-Button

dw _barsdib,255*256+16,prgtxtok,  11, 23, 48,12,0       ;04="Ok"     -Button
dw _wincnc,255*256+16,prgtxtcnc,  62, 23, 48,12,0       ;05="Cancel" -Button

ctrsizlab   dw txtsizclm,2+4

ctrsizinp   dw txtsizinp,0,0,0,0,3,0
txtsizinp   ds 4


;==============================================================================
;### SHEET TABS ###############################################################
;==============================================================================

dw      0+00,0,-8,256*1+1,     8,       0,     8,       0     ;Tab Left
dw      0+08,0,-8,256*1+1,     8,       0,     8,       0     ;Tab Right
dw      0+18,0,-8,256*1+1,     8,       0,     8,       0     ;Tab Plus

dw      0+28,0,-8,256*1+1,     4,       0,     8,       0     ;Tab1 Open
dw      4+28,0,-8,256*1+1,    40,       0,     8,       0     ;Tab1 Text
dw     44+28,0,-8,256*1+1,     5,       0,     8,       0     ;Tab1-2
dw     49+28,0,-8,256*1+1,    40,       0,     8,       0     ;Tab2 Text
dw     89+28,0,-8,256*1+1,     5,       0,     8,       0     ;Tab2-3
dw     94+28,0,-8,256*1+1,    40,       0,     8,       0     ;Tab3 Text
dw    134+28,0,-8,256*1+1,     5,       0,     8,       0     ;Tab3-4
dw    139+28,0,-8,256*1+1,    40,       0,     8,       0     ;Tab4 Text
dw    179+28,0,-8,256*1+1,     4,       0,     8,       0     ;Tab4 Close

dw     00,255*256+10,gfxtablft ,0,0,0,0,0   ;12=Tab Left
dw     00,255*256+10,gfxtabrgt ,0,0,0,0,0   ;13=Tab Right
dw     00,255*256+10,gfxtabpls ,0,0,0,0,0   ;14=Tab Plus

dw     00,255*256+10,gfxtabiop ,0,0,0,0,0   ;15=Tab1 Open
dw     00,255*256+ 1,ctrbotsh1 ,0,0,0,0,0   ;16=Tab1 Text
dw     00,255*256+10,gfxtabi2a ,0,0,0,0,0   ;17=Tab1-2
dw     00,255*256+ 1,ctrbotsh2 ,0,0,0,0,0   ;18=Tab2 Text
dw     00,255*256+10,gfxtaba2i ,0,0,0,0,0   ;19=Tab2-3
dw     00,255*256+ 1,ctrbotsh3 ,0,0,0,0,0   ;20=Tab3 Text
dw     00,255*256+10,gfxtabi2i ,0,0,0,0,0   ;21=Tab3-4
dw     00,255*256+ 1,ctrbotsh4 ,0,0,0,0,0   ;22=Tab4 Text
dw     00,255*256+10,gfxtabicl ,0,0,0,0,0   ;23=Tab4 Close

ctrbotsh1   dw txtbotsh1,256*2+2+4+128
txtbotsh1   db "Sheet1",0
ctrbotsh2   dw txtbotsh2,256*2+0+4+128
txtbotsh2   db "Sheet2",0
ctrbotsh3   dw txtbotsh3,256*2+2+4+128
txtbotsh3   db "Sheet3",0
ctrbotsh4   dw txtbotsh4,256*2+2+4+128
txtbotsh4   db "Sheet4",0


;==============================================================================
;### DOCUMENT PROPERTIES DIALOGUE #############################################
;==============================================================================

winprpdat   dw #1401,4+16,50,2,200,162,0,0,200,162,200,162,200,162,prgicnsml,txtprptit,0,0,winprpgrp,0,0:ds 136+14
winprpgrp   db winprpreca_cnt,0:dw winprpreca,0,0,05*256+04,0,0,00

ctrprptab   db 4,2+4+48+64
ctrprptab0  db 0:dw txtprpsum:db -1:dw txtprpfmt:db -1:dw txtprpcol:db -1:dw txtprpsta:db -1

;*** summary ******************************************************************

winprpreca_cnt  equ 19
winprpreca
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0  ;00=Background
dw _prptab,255*256+20,ctrprptab,  00, 02, 200,11,0      ;01=Tabs
dw       0,255*256+00,128+1    ,  00,143,10000,1,0      ;02=bottom line
dw _prpoky,255*256+16,prgtxtok,   96,147,  48,12,0      ;03="Ok"     -Button
dw _wincnc,255*256+16,prgtxtcnc, 147,147,  48,12,0      ;04="Cancel" -Button

dw       0,255*256+ 1,ctrprpstl,   5, 20,  43, 8,0      ;05=Label Title
dw       0,255*256+32,ctrprpsti,  50, 18, 145,12,0      ;06=Input Title
dw       0,255*256+ 1,ctrprpssl,   5, 34,  43, 8,0      ;07=Label Subject
dw       0,255*256+32,ctrprpssi,  50, 32, 145,12,0      ;08=Input Subject
dw       0,255*256+ 1,ctrprpsal,   5, 48,  43, 8,0      ;09=Label Author
dw       0,255*256+32,ctrprpsai,  50, 46, 145,12,0      ;10=Input Author
dw       0,255*256+00,1        ,   5, 60, 190, 1,0      ;11=sep
dw       0,255*256+ 1,ctrprpsol,   5, 65,  43, 8,0      ;12=Label Company
dw       0,255*256+32,ctrprpsoi,  50, 63, 145,12,0      ;13=Input Company
dw       0,255*256+ 1,ctrprpskl,   5, 79,  43, 8,0      ;14=Label Keywords
dw       0,255*256+32,ctrprpski,  50, 77, 145,12,0      ;15=Input Keywords
dw       0,255*256+ 1,ctrprpsml,   5, 93,  43, 8,0      ;16=Label Comment
dw       0,255*256+ 2,4*3+1    ,  50, 91, 145,48,0      ;17=Frame Comment
dw       0,255*256+33,ctrprpsmi,  51, 92, 144,46,0      ;18=Input Comment

ctrprpstl   dw txtprpstl,2+4
ctrprpsti   dw txtprpsti,0,0,0,0,39,0   ;title
ctrprpssl   dw txtprpssl,2+4
ctrprpssi   dw txtprpssi,0,0,0,0,39,0   ;subject
ctrprpsal   dw txtprpsal,2+4
ctrprpsai   dw txtprpsai,0,0,0,0,39,0   ;author
ctrprpsol   dw txtprpsol,2+4
ctrprpsoi   dw txtprpsoi,0,0,0,0,39,0   ;organisation
ctrprpskl   dw txtprpskl,2+4
ctrprpski   dw txtprpski,0,0,0,0,39,0   ;keywords
ctrprpsml   dw txtprpsml,2+4            ;message
ctrprpsmi   dw txtprpsmi,0,0,0,0,254,0, 0,0,0,-1,8,0,0,ctrprpsmi,0,0,0,0,2,0,0,0,0:ds 8*2

;*** units & formats **********************************************************

winprprecb_cnt  equ 24
winprprecb
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0  ;00=Background
dw _prptab,255*256+20,ctrprptab,  00, 02, 200,11,0      ;01=Tabs
dw       0,255*256+00,128+1    ,  00,143,10000,1,0      ;02=bottom line
dw _prpoky,255*256+16,prgtxtok,   96,147,  48,12,0      ;03="Ok"     -Button
dw _wincnc,255*256+16,prgtxtcnc, 147,147,  48,12,0      ;04="Cancel" -Button

dw       0,255*256+ 1,ctrcfdclb,   5, 18, 103, 8,0      ;05=Label Category
dw _prpcat,255*256+42,ctrprpcat,   5, 28, 103,10,0      ;06=List  Category
dw       0,255*256+ 1,ctrprptyt,   5, 45, 103, 8,0      ;07=Label Types
dw _prpup ,255*256+16,txtprpup,   50, 42,  28,12,0      ;08="Up"  Types
dw _prpdwn,255*256+16,txtprpdwn,  80, 42,  28,12,0      ;09="Down"Types

winprprecb_hlp  equ 10
dw _prptyp,255*256+41,ctrprptyp,   5, 55, 103,55,0      ;10=List  Types
dw       0,255*256+00,000+2    , 115, 18,  80,92,0      ;11=Clear Help
dw       0,255*256+ 1,ctrprphlt, 115, 18,  80, 8,0      ;12=Label Help
dw       0,255*256+33,ctrprphlp, 115, 30,  80,80,0      ;13=TxBox Help

dw       0,255*256+03,ctrprpedf,   2,113, 196,29,0      ;14=Frame Editor

winprprecb_edc  equ 15
dw       0,255*256+00,000+2    ,  10,123,  180,12,0     ;15=Clear Editor

winprprecb_ed1  equ 256*16+1
dw       0,255*256+32,ctrprpe1i,  10,123, 180,12,0      ;16=Input Editor1 full

winprprecb_ed2  equ 256*17+3
dw       0,255*256+32,ctrprpe1i,  10,123, 98 ,12,0      ;17=Input Editor1 unit
dw       0,255*256+18,ctrprpfix0,115,125, 36 , 8,0      ;18=Radio Unit prefix
dw       0,255*256+18,ctrprpfix1,155,125, 36 , 8,0      ;19=Radio Unit suffix

winprprecb_ed3  equ 256*20+4
dw       0,255*256+01,ctrprptlb,  10,125, 30 , 8,0      ;20=Text          true
dw       0,255*256+32,ctrprpe1i,  50,123, 45 ,12,0      ;21=Input Editor1 true
dw       0,255*256+01,ctrprpflb, 102,125, 30 , 8,0      ;22=Text          false
dw       0,255*256+32,ctrprpe2i, 145,123, 45 ,12,0      ;23=Input Editor2 false
winprprecb_edt  equ 8

ctrprpcat   dw 5,00,lstprpcat,0,1,rowcfdcat,0,1
lstprpcat
dw 04,txtprpcat0, 01,txtprpcat1, 02,txtprpcat2, 03,txtprpcat3, 05,txtprpcat4

ctrprptyp   dw 8,00,lstcfduni,0,1,rowcfdtyp,0,1
ctrprptyt   dw txtprptyt1,2+4
ctrprphlt   dw txtprphlt,2+4

;editor
ctrprpedf   dw txtprpedf,2+4                                ;frame
ctrprpe1i   dw txtprpe1i,0,0,0,0,15,0                       ;editor1 (unit, format, bool true)
ctrprpe2i   dw txtprpe2i,0,0,0,0, 7,0                       ;editor2 (bool false)
ctrprptlb   dw txtprptlb,2+4                                ;boolean
ctrprpflb   dw txtprpflb,2+4
ctrprpfix0  dw flgprpfix,txtprpfix0,256*0+2+4,corprpfix     ;prefix
ctrprpfix1  dw flgprpfix,txtprpfix1,256*1+2+4,corprpfix     ;suffix
flgprpfix   db 0
corprpfix   db -1,-1,-1,-1
ctrprphlp   dw txtprphlp,0,0,0,0,txtprphlp0-txtprphlp,256*18+2+4, 0,0,0,-1,14,-8,0,ctrprphlp,0,0,0,0,2,0,0,0,0:ds 14*2

;*** colours ******************************************************************

winprprecc_cnt  equ 13
winprprecc
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0  ;00=Background
dw _prptab,255*256+20,ctrprptab,  00, 02, 200,11,0      ;01=Tabs
dw       0,255*256+00,128+1    ,  00,143,10000,1,0      ;02=bottom line
dw _prpoky,255*256+16,prgtxtok,   96,147,  48,12,0      ;03="Ok"     -Button
dw _wincnc,255*256+16,prgtxtcnc, 147,147,  48,12,0      ;04="Cancel" -Button

dw       0,255*256+ 3,ctrcfgcof,   2, 18, 196,75,0      ;05=Frame  Colours
dw       0,255*256+25,ctrcfdcog, 109, 24,  63,63,0      ;06=Select Colour
winprprecc_col equ 7
winprprecc1
dw       0,255*256+02,256*102+128,110,25,  16,16,0      ;07=unselect
dw       0,255*256+02,256*17 +128,110,25,  16,16,0      ;08=selected
dw _prpcol,255*256+19,0        ,  109,24,  63,63,0      ;09=Click  Colour

dw _prpctp,255*256+18,ctrcfgctt,  18, 36,  70, 8,0      ;10=Radio Pen
dw _prpctp,255*256+18,ctrcfgcbt,  18, 52,  70, 8,0      ;11=Radio Paper
dw _prpctp,255*256+18,ctrcfgcgt,  18, 68,  70, 8,0      ;12=Radio Text

;*** statistics ***************************************************************

winprprecd_cnt  equ 29
winprprecd
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0  ;00=Background
dw _prptab,255*256+20,ctrprptab,  00, 02, 200,11,0      ;01=Tabs
dw       0,255*256+00,128+1    ,  00,143,10000,1,0      ;02=bottom line
dw _prpoky,255*256+16,prgtxtok,   96,147,  48,12,0      ;03="Ok"     -Button
dw _wincnc,255*256+16,prgtxtcnc, 147,147,  48,12,0      ;04="Cancel" -Button

dw       0,255*256+ 1,ctrprpta2,  65, 26,  60, 8,0      ;05=Title Used
dw       0,255*256+ 1,ctrprpta3, 135, 26,  50, 8,0      ;06=Title Free

dw       0,255*256+00,128+1    ,   5, 38, 190, 1,0      ;10=sepline

dw       0,255*256+ 1,ctrprpsa1,   5, 43,  50, 8,0      ;07=Label Cells
dw       0,255*256+ 1,ctrprpsa2,  65, 43,  60, 8,0      ;08=Used  Cells
dw       0,255*256+ 1,ctrprpsa3, 135, 43,  50, 8,0      ;09=Free  Cells

dw       0,255*256+00,128+1    ,   5, 55, 190, 1,0      ;10=sepline

dw       0,255*256+ 1,ctrprpsr1,   5, 60,  50, 8,0      ;11=Label Cell Records
dw       0,255*256+ 1,ctrprpsr2,  65, 60,  65, 8,0      ;12=Used  Cell Records
dw       0,255*256+ 1,ctrprpsr3, 135, 60,  55, 8,0      ;13=Free  Cell Records
dw       0,255*256+ 1,ctrprpsb1,   5, 72,  50, 8,0      ;14=Label Cell Data
dw       0,255*256+ 1,ctrprpsb2,  65, 72,  65, 8,0      ;15=Used  Cell Data
dw       0,255*256+ 1,ctrprpsb3, 135, 72,  55, 8,0      ;16=Free  Cell Data
dw       0,255*256+ 1,ctrprpsc1,   5, 84,  50, 8,0      ;17=Label Cell Text
dw       0,255*256+ 1,ctrprpsc2,  65, 84,  65, 8,0      ;18=Used  Cell Text
dw       0,255*256+ 1,ctrprpsc3, 135, 84,  55, 8,0      ;19=Free  Cell Text
dw       0,255*256+ 1,ctrprpsd1,   5, 96,  50, 8,0      ;20=Label Cell Controls
dw       0,255*256+ 1,ctrprpsd2,  65, 96,  60, 8,0      ;21=Used  Cell Controls
dw       0,255*256+ 1,ctrprpsd3, 135, 96,  50, 8,0      ;22=Free  Cell Controls

dw       0,255*256+00,128+1    ,   5,108, 190, 1,0      ;23=sepline

dw       0,255*256+ 1,ctrprpse1,   5,113,  55, 8,0      ;24=Label External Sheets
dw       0,255*256+ 1,ctrprpse2,  65,113,  65, 8,0      ;25=Used  External Sheets
dw       0,255*256+ 1,ctrprpsf1,   5,125,  55, 8,0      ;26=Label Total
dw       0,255*256+ 1,ctrprpsf2,  65,125,  65, 8,0      ;27=Used  Total

ctrprpta2   dw txtprpta2,6+256
ctrprpta3   dw txtprpta3,6+256
ctrprpsa1   dw txtprpsa1,6    
ctrprpsa2   dw txtprpsa2,6+256
ctrprpsa3   dw txtprpsa3,6+256
ctrprpsr1   dw txtprpsr1,6    
ctrprpsr2   dw txtprpsr2,6+256
ctrprpsr3   dw txtprpsr3,6+256
ctrprpsb1   dw txtprpsb1,6    
ctrprpsb2   dw txtprpsb2,6+256
ctrprpsb3   dw txtprpsb3,6+256
ctrprpsc1   dw txtprpsc1,6    
ctrprpsc2   dw txtprpsc2,6+256
ctrprpsc3   dw txtprpsc3,6+256
ctrprpsd1   dw txtprpsd1,6    
ctrprpsd2   dw txtprpsd2,6+256
ctrprpsd3   dw txtprpsd3,6+256
ctrprpse1   dw txtprpse1,6    
ctrprpse2   dw txtprpse2,6+256
ctrprpsf1   dw txtprpsf1,6    
ctrprpsf2   dw txtprpsf2,6+256


;==============================================================================
;### EXCHANGE (IMPORT/EXPORT) FILEFORMATS #####################################
;==============================================================================

wincsvdat   dw #1401,4+16,50,20,200,126,0,0,200,126,200,126,200,126,prgicnsml
wincsvdat1  dw txtdectit,0,0,wincsvgrp,0,0:ds 136+14

windesdat   dw #1401,4+16,50,2,200,126,0,0,200,126,200,126,200,126,prgicnsml,txtdestit,0,0,winsylgrp,0,0:ds 136+14
windisdat   dw #1401,4+16,50,2,200,126,0,0,200,126,200,126,200,126,prgicnsml,txtdistit,0,0,winsylgrp,0,0:ds 136+14

wincsvgrp   db 15,0:dw wincsvrec,0,0,04*256+03,0,0,00
winsylgrp   db 15,0:dw wincsvrec,0,0,04*256+03,0,0,00

wincsvrec
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0  ;00=Background
dw       0,255*256+00,128+1    ,  00,107,10000,1,0      ;01=bottom line
wincsvrec1
dw       0,255*256+16,txtcsvexb,  96,111,  48,12,0      ;02="Ok"     -Button
dw _wincnc,255*256+16,prgtxtcnc, 147,111,  48,12,0      ;03="Cancel" -Button

dw       0,255*256+ 1,ctrcsvfll,   5, 08,  43, 8,0      ;04=Label    File
wincsvrec_inp equ 5
dw       0,255*256+32,ctrcsvfli,  30, 06, 120,12,0      ;05=Input    File
dw _xchbrw,255*256+16,prgtxtbrw, 153, 06,  42,12,0      ;06=Browse   File

dw       0,255*256+ 3,ctrcsvopf,   2, 26, 196,80,0      ;07=Frame    Cell options

dw       0,255*256+ 1,ctrcsvspl,  11, 39,  83, 8,0      ;08=Label    Separator
dw       0,255*256+42,ctrcsvspd,  80, 38, 109,10,0      ;09=Dropdown Separator
dw       0,255*256+ 1,ctrcsvlml,  11, 51,  83, 8,0      ;10=Label    Limiter
dw       0,255*256+42,ctrcsvlmd,  80, 50, 109,10,0      ;11=Dropdown Limiter

dw       0,255*256+17,ctrcsvshc,  11, 65, 180, 8,0      ;12=Check    Cells as shown
dw       0,255*256+17,ctrcsvfoc,  11, 76, 180, 8,0      ;13=Check    Save formulas
dw       0,255*256+17,ctrcsvquc,  11, 87, 180, 8,0      ;14=Check    Quote text cells

ctrcsvfll   dw txtcsvfll,2+4
ctrcsvfli   dw txtcsvfli,0,0,0,0,255,0

ctrcsvopf   dw txtcsvopl,2+4

ctrcsvspl   dw txtcsvspl,2+4
ctrcsvlml   dw txtcsvlml,2+4

ctrcsvspd   dw 4,00,lstcsvspd,0,1,rowcsvspd,0,1
rowcsvspd   dw 0,100,0,0
lstcsvspd   dw 00,lstcsvspd0, 01,lstcsvspd1, 02,lstcsvspd2, 03,lstcsvspd3

ctrcsvlmd   dw 2,00,lstcsvlmd,0,1,rowcsvlmd,0,1
rowcsvlmd   dw 0,100,0,0
lstcsvlmd   dw 00,lstcsvlmd0, 01,lstcsvlmd1, 02,lstcsvlmd2, 03,lstcsvlmd3

ctrcsvshc   dw flgcsvshc,txtcsvshc,2+4
ctrcsvfoc   dw flgcsvfoc,txtcsvfoc,2+4
ctrcsvquc   dw flgcsvquc,txtcsvquc,2+4

flgcsvshc   db 0
flgcsvfoc   db 0
flgcsvquc   db 0


;==============================================================================
;### PREFERENCES DIALOGUE #####################################################
;==============================================================================

wincfgdat   dw #1401,4+16,50,2,200,162,0,0,200,162,200,162,200,162,prgicnsml,txtcfgtit,0,0,wincfggrp,0,0:ds 136+14
wincfggrp   db wincfgreca_cnt,0:dw wincfgreca,0,0,05*256+04,0,0,00

ctrcfgtab   db 2,2+4+48+64
ctrcfgtab0  db 0:dw txtcfgfrm:db -1:dw txtcfgcol:db -1

;*** formats ******************************************************************

wincfgreca_cnt  equ 28
wincfgreca
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0  ;00=Background
dw _cfgtab,255*256+20,ctrcfgtab,  00, 02, 200,11,0      ;01=Tabs
dw       0,255*256+00,128+1    ,  00,143,10000,1,0      ;02=bottom line
dw _cfgoky,255*256+16,prgtxtok,   96,147,  48,12,0      ;03="Ok"     -Button
dw _wincnc,255*256+16,prgtxtcnc, 147,147,  48,12,0      ;04="Cancel" -Button

dw       0,255*256+ 3,ctrcfgrgf,   2, 18, 196,61,0      ;05=Frame Regional

dw       0,255*256+ 1,ctrcfgdst,  11, 32,  73, 8,0      ;06=Text  Decimal Separator
dw       0,255*256+ 1,ctrcfgdgt, 110, 32,  37, 8,0      ;07=Text  Digit Grouping
dw       0,255*256+ 1,ctrcfgbgt, 110, 46,  37, 8,0      ;08=Text  Bin/Hex Grouping
wincfgreca_sep equ 9
dw       0,255*256+32,ctrcfgdsi,  88, 30,  14,12,0      ;09=Input Decimal Separator
dw       0,255*256+32,ctrcfgdgi, 175, 30,  14,12,0      ;10=Input Digit Grouping
dw       0,255*256+32,ctrcfgbgi, 175, 44,  14,12,0      ;11=Input Bin/Hex Grouping

dw       0,255*256+ 1,ctrcfgrgt,  11, 50,  90, 8,0      ;12=Text  Presets
dw _cfgreg,255*256+42,ctrcfgrgl,  11, 60, 178, 8,0      ;13=List  Presets

dw       0,255*256+ 3,ctrcfgfmf,   2, 80, 196,62,0      ;14=Frame Formatting
dw       0,255*256+ 1,ctrcfgdpt,  11,100,  70, 8,0      ;15=Text  Decimal places
dw       0,255*256+ 1,ctrcfgdbt,  11,112,  70, 8,0      ;16=Text  Bin digits
dw       0,255*256+ 1,ctrcfgdxt,  11,124,  70, 8,0      ;17=Text  Hex digits
dw       0,255*256+ 1,ctrcfggrt, 119, 90,  60, 8,0      ;18=Text  Grouping
dw       0,255*256+ 1,ctrcfgprt, 165, 90,  60, 8,0      ;19=Text  Prefix

dw       0,255*256+42,ctrcfgdpl,  90, 99,  24,10,0      ;20=List  Decimal places
dw       0,255*256+42,ctrcfgdbl,  90,111,  24,10,0      ;21=List  Decimal places
dw       0,255*256+42,ctrcfgdxl,  90,123,  24,10,0      ;22=List  Decimal places

dw       0,255*256+17,ctrcfgngc, 134,100,   8, 8,0      ;23=Check num grouping
dw       0,255*256+17,ctrcfgbgc, 134,112,   8, 8,0      ;24=Check bin grouping
dw       0,255*256+17,ctrcfgxgc, 134,124,   8, 8,0      ;25=Check hex grouping
dw       0,255*256+17,ctrcfgbpc, 174,112,   8, 8,0      ;26=Check bin prefix
dw       0,255*256+17,ctrcfgxpc, 174,124,   8, 8,0      ;27=Check hex prefix


ctrcfgdst   dw txtprpdst,2+4
ctrcfgdsi   dw cfgnumcom,0,0,0,0,1,0    ;decimal separator
ctrcfgdgt   dw txtcfgdgt,2+4
ctrcfgdgi   dw cfgnumpoi,0,0,0,0,1,0    ;digit grouping
ctrcfgbgt   dw txtcfgbgt,2+4
ctrcfgbgi   dw cfgnumbin,0,0,0,0,1,0    ;bin/hex grouping

ctrcfgrgf   dw txtcfgrgf,2+4
ctrcfgrgt   dw txtcfgrgt,2+4

ctrcfgrgl   dw 6,00,lstcfgrgl,0,1,rowcfgrgl,0,1
rowcfgrgl   dw 0,1000,0,0
lstcfgrgl
dw 00,txtcfgrgl0, 01,txtcfgrgl1, 02,txtcfgrgl2, 03,txtcfgrgl3, 04,txtcfgrgl4, 05,txtcfgrgl5

ctrcfgfmf   dw txtcfgfmf,2+4

ctrcfgdpt   dw txtcfgdpt,2+4
ctrcfgdbt   dw txtcfgdbt,2+4
ctrcfgdxt   dw txtcfgdxt,2+4
ctrcfggrt   dw txtcfggrt,2+4
ctrcfgprt   dw txtcfgprt,2+4

ctrcfgdpl   dw 8,00,lstcfddpl,0,1,rowcfddpl,0,1 ;\
flgcfgngc   db 0                                ;|
            db 0                                ;|
ctrcfgdbl   dw 8,00,lstcfddbl,0,1,rowcfddpl,0,1 ;|
flgcfgbgc   db 0                                ;|
flgcfgbpc   db 0                                ;|
ctrcfgdxl   dw 8,00,lstcfddxl,0,1,rowcfddpl,0,1 ;|
flgcfgxgc   db 0                                ;|
flgcfgxpc   db 0                                ;/

ctrcfgngc   dw flgcfgngc,prgtxtnul,2+4
ctrcfgbgc   dw flgcfgbgc,prgtxtnul,2+4
ctrcfgxgc   dw flgcfgxgc,prgtxtnul,2+4
ctrcfgbpc   dw flgcfgbpc,prgtxtnul,2+4
ctrcfgxpc   dw flgcfgxpc,prgtxtnul,2+4

;*** colours &sizes ***********************************************************

wincfgrecb_cnt  equ 18
wincfgrecb
dw       0,255*256+00,000+2    ,  00, 00,10000,10000,0  ;00=Background
dw _cfgtab,255*256+20,ctrcfgtab,  00, 02, 200,11,0      ;01=Tabs
dw       0,255*256+00,128+1    ,  00,143,10000,1,0      ;02=bottom line
dw _cfgoky,255*256+16,prgtxtok,   96,147,  48,12,0      ;03="Ok"     -Button
dw _wincnc,255*256+16,prgtxtcnc, 147,147,  48,12,0      ;04="Cancel" -Button

dw       0,255*256+ 3,ctrcfgcof,   2, 18, 196,75,0      ;05=Frame  Colours
dw       0,255*256+25,ctrcfdcog, 109, 24,  63,63,0      ;06=Select Colour
wincfgrecb_col equ 7
wincfgrecb1
dw       0,255*256+02,256*102+128,110,25,  16,16,0      ;07=unselect
dw       0,255*256+02,256*17 +128,110,25,  16,16,0      ;08=selected
dw _cfgcol,255*256+19,0        ,  109,24,  63,63,0      ;09=Click  Colour

dw _cfgctp,255*256+18,ctrcfgctt,  18, 36,  70, 8,0      ;10=Radio Pen
dw _cfgctp,255*256+18,ctrcfgcbt,  18, 52,  70, 8,0      ;11=Radio Paper
dw _cfgctp,255*256+18,ctrcfgcgt,  18, 68,  70, 8,0      ;12=Radio Text

dw       0,255*256+ 3,ctrcfgszf,   2, 94, 196,48,0      ;13=Frame  Sizes
dw       0,255*256+ 1,ctrcfgswt,  11,109,  28, 8,0      ;14=Label Width
dw       0,255*256+32,ctrcfgswi,  40,107,  28,12,0      ;15=Input Width
dw       0,255*256+ 1,ctrcfgsht,  11,123,  28, 8,0      ;16=Label Height
dw       0,255*256+32,ctrcfgshi,  40,121,  28,12,0      ;17=Input Height


ctrcfgszf   dw txtcfgszf,2+4
ctrcfgswt   dw txtcfgswt,2+4
ctrcfgsht   dw txtcfgsht,2+4

ctrcfgswi   dw txtcfgswi,0,0,0,0,3,0    ;width
txtcfgswi   ds 4
ctrcfgshi   dw txtcfgshi,0,0,0,0,3,0    ;height
txtcfgshi   ds 4

ctrcfgcof   dw txtcfgcof,2+4
ctrcfgctt   dw flgcfgcor,txtcfgctt,256*0+2+4,bufcfgcor
ctrcfgcbt   dw flgcfgcor,txtcfgcbt,256*1+2+4,bufcfgcor
ctrcfgcgt   dw flgcfgcor,txtcfgcgt,256*2+2+4,bufcfgcor

flgcfgcor   db 0
bufcfgcor   ds 4


;==============================================================================
;### COLUMN/ROW BARS ##########################################################
;==============================================================================

supclmctr   dw supclmgrp,9999,  10,0,0,   0, 0
suprowctr   dw suprowgrp,  20,9999,0,0,   0, 0
supclmgrp   db 33,0:dw 000000000,0,0,256*0+0,0,0,2
suprowgrp   db 65,0:dw 000000000,0,0,256*0+0,0,0,2

bardat  db 0        ;*** last byte ***

prgtrnend
