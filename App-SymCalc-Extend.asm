;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               S y m C a l c                                @
;@              (extend 1 - cell hash table, dialogues, config)               @
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
;### PRPOKY -> saves document properties
;### PRPDEF -> sets default document config and clears meta data
;### PRPSAV -> saves meta data into opened file
;### PRPLOD -> loads meta data from opened file
;### PRPUNC -> unpacks meta data
;### PRPCPR -> packs meta data

;--- SUB ROUTINES -------------------------------------------------------------
;### PRGFIL -> Generates config file path
;### STRLEN -> get string length
;### STRINI -> inits string input control
;### CHRTRM -> check for terminator
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

celbld_   equ 00


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
;### Input      L,H=position
;### Output     C,B=position, HL=hash table address
;### Destroyed  AF
hshadr  ld c,l:ld b,h
        ld a,l              ;1
        rrca:rrca:rrca:rrca ;4
        and #f0             ;2
        ld l,a              ;1
        ld a,h              ;1
        and #0f             ;2
        or l                ;1
        ld l,a              ;1
hshadr1 ld h,0              ;2
        ret

;### HSHNEW -> adds a hash entry for a cell
;### Input      L,H=position, DE=cell adr
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
;### Input      L,H=position
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
;### Input      L,H=position, DE=cell adr
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
;### Input      L,H=position
;### Output     E=0 -> no cell, HL=0
;###            E=1 -> HL=cell data record
;### Destroyed  AF,BC,D,IX
hshfnd  call hshadr
        ld a,(hl)           ;2
        or a                ;1
        jr z,hshfnd4        ;2
        ld ixl,a            ;2
        inc h               ;1
        ld a,(hl)           ;2
        inc h               ;1
        ld h,(hl)           ;2
        ld l,a              ;1      hl=bucket adr
        ld de,4-1           ;3 34

hshfnd1 ld a,(hl):inc hl    ;4
        cp c                ;1
        jr nz,hshfnd2       ;2/3 -> 7/8
        ld a,(hl)           ;2
        cp b                ;1
        jr z,hshfnd3        ;2/3 -> 5/6
hshfnd2 dec ixl             ;2
        jr z,hshfnd4        ;2
        add hl,de           ;3
        ld a,(hl):inc hl    ;4
        ld h,(hl):ld l,a    ;3
        jr hshfnd1          ;3 17+8/17+7+5->25/29-> 27 (not found)

hshfnd3 inc hl:ld e,(hl)    ;4
        inc hl:ld d,(hl)    ;4
        ex de,hl            ;1 13+9-> 22 (found) -> average 34+2.5*25 = ca 94 (1,5 rasterlines)
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
dw 0                  ; 13
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
        jr z,celmen1
        ld a,1
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

;### CFGPEN/PAP -> pen/paper selection
cfdpen  ld iy,fmtvalcol+0
        ld ix,wincfdrecc1
        ld a,wincfdrecc_pen
        ld hl,256*75+14
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
        call SyDesktop_WININH
        jp cfdprv
cfdpen2 ld c,0      ;c=a/15
cfdpen3 sub 15
        ret c
        inc c
        jr cfdpen3
cfdpap  ld iy,fmtvalcol+1
        ld ix,wincfdrecc2
        ld a,wincfdrecc_pap
        ld hl,256*75+101
        jr cfdpen1


;### Input      A=type (0=date, 1=time, 2=boolean, 3=number, 4=bin/hex)

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
;l=colour, ix=control record
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
fmtc15  ld a,15:jr fmtcst
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
        ld de,winprpdat     ;open window
        jp windia

;### PRPTAB -> document properties tab pressed
prptabo db 0
prptabt db winprpreca_cnt:dw winprpreca
        db winprprecb_cnt:dw winprprecb
        db winprprecc_cnt:dw winprprecc

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
prpgetc db 0    ;changed flag

prpgett dw cfgmskdat  ,ctrprpe1i:db 16:dw 0:db 0
        dw cfgmsktim  ,ctrprpe1i:db 16:dw 0:db 0
        dw cfgunibol  ,ctrprpe1i:db  8:dw ctrprpe2i:db 8
        dw cfguninum+8,flgprpfix:db  1:dw ctrprpe1i:db 7
        dw cfgunibin+8,flgprpfix:db  1:dw ctrprpe1i:db 7

prpget  ld a,(prpvaltyp)
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
        ld a,(prpvaltyp)
        ld c,1
        call lstbld
        ld e,winprprecb_hlp
        ld a,(windianum)
        call SyDesktop_WININH
prptyp1 call prpget
        jp z,guiret0
        jp prpcat0

prpup   ;...
        jp jmp_bnkret

prpdwn  ;...
        jp jmp_bnkret

;### PRPOKY -> saves document properties
prpoky  call prpput
        call wincls
        call cfgput
        ld l,ret_prpoky
        ld de,(prpflgchg)
        jp jmp_bnkret

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

;### STRLEN -> get string length
;### Input      HL=String (0-terminated)
;### Output     HL=Stringend (0), BC=length (max 255, without 0-terminator)
;### Destroyed  -
strlen  push af
        xor a
        ld bc,255
        cpir
        ld a,254
        sub c
        ld c,a
        dec hl
        pop af
        ret

;### STRINI -> inits string input control
;### Input      IX=control
;### Destroyed  BC,HL
strini  ld l,(ix+0)
        ld h,(ix+1)
        call strlen
strini1 ld (ix+8),c
        ld (ix+4),c
        ld (ix+2),b
        ld (ix+6),b
        ret

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

;### CNV08S -> converts 8bit to decimal string
;### Input      A=number, HL=destination string, B=counter
;### Output     HL=next string position, B+=length
;### Destroyed  AF,DE
cnv08s  ld de,255
        cp 100
        jr c,cnv08s2
        ld d,"2"
        sub 200
        jr nc,cnv08s1
        add 100
        dec d
cnv08s1 ld (hl),d
        inc hl
        inc b
cnv08s2 sub 10
        inc e
        jr nc,cnv08s2
        jr nz,cnv08s3
        inc d:dec d
        jr z,cnv08s4
cnv08s3 set 4,e
        set 5,e
        ld (hl),e
        inc hl
        inc b
cnv08s4 add "0"+10
        ld (hl),a
        inc hl
        inc b
        ret

;### CNV32S -> Converts 32Bit-number (unsigned) to string (terminated by 0)
;### Input      DE,IX=value, IY=destination address
;### Output     IY=Address of last char
;### Destroyed  AF,BC,DE,HL,IX
cnv32st dw 1,0,     10,0,     100,0,     1000,0,     10000,0
        dw #86a0,1, #4240,#f, #9680,#98, #e100,#5f5, #ca00,#3b9a
cnv32sz ds 4

cnv32s  ld (cnv32sz),ix
        ld (cnv32sz+2),de
        ld ix,cnv32st+36
        ld b,9
        ld c,0
cnv32s1 ld a,"0"
        or a
cnv32s2 ld e,(ix+0):ld d,(ix+1):ld hl,(cnv32sz):  sbc hl,de:ld (cnv32sz),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(cnv32sz+2):sbc hl,de:ld (cnv32sz+2),hl
        jr c,cnv32s5
        inc c
        inc a
        jr cnv32s2
cnv32s5 ld e,(ix+0):ld d,(ix+1):ld hl,(cnv32sz):  add hl,de:ld (cnv32sz),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(cnv32sz+2):adc hl,de:ld (cnv32sz+2),hl
        ld de,-4
        add ix,de
        inc c
        dec c
        jr z,cnv32s3
        ld (iy+0),a
        inc iy
cnv32s3 djnz cnv32s1
        ld a,(cnv32sz)
        add "0"
        ld (iy+0),a
        ld (iy+1),0
        ret

;### CNVS16 -> converts string into number
;### Input      IY=string, HL=terminator list, BC=min (>=0), DE=max (<=65534)
;### Output     IY=string behind terminator, HL=number (>=min, <=max), CF=1 -> invalid
;### Destroyed  AF,DE
cnvs16  ld (cnvs166+1),hl
        ld hl,0
cnvs161 ld a,(iy+0)
        inc iy
        push bc
        push hl
cnvs166 ld hl,0
        call chrtrm0
        pop hl
        pop bc
        jr nc,cnvs163
        sub "0"
        cp 10
        ccf
        ret c
        push bc
        add hl,hl:jr c,cnvs162
        ld c,l
        ld b,h
        add hl,hl:jr c,cnvs162
        add hl,hl:jr c,cnvs162
        add hl,bc:jr c,cnvs162
        ld c,a
        ld b,0
        add hl,bc:ret c
        pop bc
        jr cnvs161
cnvs162 pop bc
cnvs165 ex de,hl
        or a
        ret
cnvs163 sbc hl,bc
        jr c,cnvs164
        add hl,bc
        inc de
        sbc hl,de
        jr nc,cnvs165
        add hl,de
        or a
        ret
cnvs164 ld l,c
        ld h,b
        or a
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

;### ALERT WINDOWS ############################################################

prgtxtinf1  db "SymCalc Spreadsheet",0
prgtxtinf2  db " Version 0.8 (Build "
read "..\..\..\SRC-Main\build.asm"
            db "pdt)",0
prgtxtinf3  db " Copyright <c> 2024 SymbiosiS"
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

ctxclmtxt1  db "Column width",0
ctxclmtxt2  db "Mark this column",0
ctxclmtxt3  db "Mark column range",0
ctxclmtxt4  db "Insert column",0
ctxclmtxt5  db "Remove column",0

ctxrowtxt1  db "Row height",0
ctxrowtxt2  db "Mark this row",0
ctxrowtxt3  db "Mark row range",0
ctxrowtxt4  db "Insert row",0
ctxrowtxt5  db "Remove row",0

ctxceltxt1  db "Cut",0
ctxceltxt2  db "Copy",0
ctxceltxt3  db "Paste",0
ctxceltxt4  db "Insert cell(s)",0
ctxceltxt4a   db "Move cells down",0
ctxceltxt4b   db "Move cells right",0
ctxceltxt4c   db "Insert whole row(s)",0
ctxceltxt4d   db "Insert whole column(s)",0
ctxceltxt5  db "Remove cell(s)",0
ctxceltxt5a   db "Move cells up",0
ctxceltxt5b   db "Move cells left",0
ctxceltxt5c   db "Remove whole row(s)",0
ctxceltxt5d   db "Remove whole column(s)",0
ctxceltxt6  db "Clear cell(s)",0
ctxceltxt7  db "Format cell(s)...",0



;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;### TRANSFER AREA ############################################################
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

App_BegTrns

;### PRGPRZS -> Stack for application process
        ds 128
prgstk2 ds 6*2
        dw prgprz
App_PrcID  db 0
App_MsgBuf ds 14

;### ALERT WINDOWS ############################################################

prgtxtinf   dw prgtxtinf1,4*1+2,prgtxtinf2,4*1+2,prgtxtinf3,4*1+2,prgicnbig

prgtxtsav   dw prgtxtsav1,4*1+2,prgtxtinf0,4*1+2,prgtxtinf0,4*1+2

;error - load/save
prgerrinfa  dw prgerrtxt1a,4*1+2,prgerrtxt2a,4*1+2,prgerrtxt3a,4*1+2
prgerrinfb  dw prgerrtxt1a,4*1+2,prgerrtxt2b,4*1+2,prgerrtxt3b,4*1+2
prgerrinfc  dw prgerrtxt1a,4*1+2,prgerrtxt2c,4*1+2,prgerrtxt3c,4*1+2
prgerrinfd  dw prgerrtxt1a,4*1+2,prgerrtxt2d,4*1+2,prgerrtxt3d,4*1+2
prgerrinfe  dw prgerrtxt1a,4*1+2,prgerrtxt2e,4*1+2,prgerrtxt3e,4*1+2


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


;==============================================================================
;### PULL DOWN / CONTEXT MENUS ################################################
;==============================================================================

;column bar
ctxclmdat   dw 7
dw 1,ctxclmtxt1, barmsc, 0
dw 8,0,0,0
dw 1,ctxclmtxt2, barmcs, 0
dw 1,ctxclmtxt3, barmcm, 0
dw 8,0,0,0
dw 1,ctxclmtxt4, barmci, 0
dw 1,ctxclmtxt5, barmcr, 0

;row bar
ctxrowdat   dw 7
dw 1,ctxrowtxt1, barmsr, 0
dw 8,0,0,0
dw 1,ctxrowtxt2, barmrs, 0
dw 1,ctxrowtxt3, barmrm, 0
dw 8,0,0,0
dw 1,ctxrowtxt4, barmri, 0
dw 1,ctxrowtxt5, barmrr, 0

;cells
ctxceldat   dw 9
dw 1,ctxceltxt1, cmncut, 0
dw 1,ctxceltxt2, cmncop, 0
dw 1,ctxceltxt3, cmnpst, 0
dw 8,0,0,0
dw 5,ctxceltxt4, ctxceldat4, 0
dw 5,ctxceltxt5, ctxceldat5, 0
dw 1,ctxceltxt6, cmnclr, 0
dw 8,0,0,0
dw 1,ctxceltxt7, cmnfmt, 0

ctxceldat4  dw 4
dw 1,ctxceltxt4a, cmncid, 0
dw 1,ctxceltxt4b, cmncir, 0
dw 1,ctxceltxt4c, cmnrwi, 0
dw 1,ctxceltxt4d, cmncmi, 0

ctxceldat5  dw 4
dw 1,ctxceltxt5a, cmncru, 0
dw 1,ctxceltxt5b, cmncrl, 0
dw 1,ctxceltxt5c, cmnrwr, 0
dw 1,ctxceltxt5d, cmncmr, 0


;==============================================================================
;### COLOUR DROPDOWN ##########################################################
;==============================================================================

wincoldat   dw #0001,4+8+16, 0,0, 63, 77,0,0,  63,  77,63,77,63,77,  0,0,0,0,wincolgrp,0,0:ds 136+14
wincolgrp   db 20,0:dw wincolrec,0,0,00*256+00,0,0,00
wincolrec
dw       0,255*256+00,128+6    ,  00, 00,10000,10000,0   ;00=Background

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
dw       0,255*256+00,128+6    ,  00, 00,10000,10000,0  ;00=Background
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
dw       0,255*256+00,128+6    ,  92, 55, 103,57,0      ;10=Hide  Types
dw       0,255*256+ 1,ctrcfdtyt,  92, 55, 103, 8,0      ;11=Label Types
dw _cfdprv,255*256+41,ctrcfdtyp,  92, 65, 103,47,0      ;12=List  Types

wincfdreca_opt2 equ 13
wincfdreca2
dw       0,255*256+00,128+6    ,   2,115, 196,27,0      ;13=Hide  Options
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
dw       0,255*256+00,128+6    ,  00, 00,10000,10000,0  ;00=Background
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
dw       0,255*256+00,128+6    ,  00, 00,10000,10000,0  ;00=Background
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
dw       0,255*256+00,128+6    ,  00, 00,10000,10000,0  ;00=Background
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

ctrprptab   db 3,2+4+48+64
ctrprptab0  db 0:dw txtprpsum:db -1:dw txtprpfmt:db -1:dw txtprpsta:db -1

;*** summary ******************************************************************

winprpreca_cnt  equ 19
winprpreca
dw       0,255*256+00,128+6    ,  00, 00,10000,10000,0  ;00=Background
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
dw       0,255*256+00,128+6    ,  00, 00,10000,10000,0  ;00=Background
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
dw       0,255*256+00,128+6    , 115, 18,  80,92,0      ;11=Clear Help
dw       0,255*256+ 1,ctrprphlt, 115, 18,  80, 8,0      ;12=Label Help
dw       0,255*256+33,ctrprphlp, 115, 30,  80,80,0      ;13=TxBox Help

dw       0,255*256+03,ctrprpedf,   2,113, 196,29,0      ;14=Frame Editor

winprprecb_edc  equ 15
dw       0,255*256+00,128+6    ,  10,123,  180,12,0     ;15=Clear Editor

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

;*** statistics ***************************************************************

winprprecc_cnt  equ 29
winprprecc
dw       0,255*256+00,128+6    ,  00, 00,10000,10000,0  ;00=Background
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
;### COLUMN/ROW BARS ##########################################################
;==============================================================================

supclmctr   dw supclmgrp,9999,  10,0,0,   0, 0
suprowctr   dw suprowgrp,  20,9999,0,0,   0, 0
supclmgrp   db 33,0:dw 000000000,0,0,256*0+0,0,0,2
suprowgrp   db 65,0:dw 000000000,0,0,256*0+0,0,0,2

bardat  db 0        ;*** last byte ***

prgtrnend
