;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               S y m C a l c                                @
;@                     (extend 2 - cell text mangagement)                     @
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

;--- SUB ROUTINES -------------------------------------------------------------

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
ptctab0


;==============================================================================
;### PROGRAM ##################################################################
;==============================================================================

prgprz  ld ix,(App_PrcID)           ;check for messages
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

windianum       db 0
winmaidat_adr   dw 0
winmaidat_bnk   db 0
winmaigrp_adr   dw 0
winmairec_adr   dw 0

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
;### SUB ROUTINES #############################################################
;==============================================================================



;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;### DATA AREA ################################################################
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

App_BegData

read"App-SymCalc-Font.asm"


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

prgtrnend
