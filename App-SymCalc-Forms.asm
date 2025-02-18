;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               S y m C a l c                                @
;@                                (form data)                                 @
;@                                                                            @
;@             (c) 2024-2024 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;==============================================================================
;### GENERAL STRINGS ##########################################################
;==============================================================================

docmsk      db "SCS",0
docpth      ds 256

maitittxt   db "untitled - SymCalc",0:ds 12-8


;### ALERT WINDOW #############################################################

errforinf0  equ 00
errforinf1  equ 01
errforinf2  equ 02
errforinf3  equ 03
errforinf4  equ 04
errforinf5  equ 05
errforinf6  equ 06
errforinf7  equ 07
errforinfa  equ 08
errcopinfa  equ 09
errmeminfa  equ 10
errmeminfb  equ 11
errmeminfc  equ 12
errmeminfd  equ 13
errlodinfa  equ 14
errlodinfb  equ 15
errlodinfc  equ 16
errlodinfd  equ 17
errlodinfe  equ 18
prgmsgsav   equ 24
prgmsginf   equ 25


;==============================================================================
;### BOOT WINDOW ##############################################################
;==============================================================================

winbotdat    dw #0081,4+8+16, 0,0, 188, 80,0,0, 188, 80, 188, 80, 188, 80,0,0,0,0,winbotgrp,0,0:ds 136+14

winbotgrp    db 7,0: dw winbotrec,0,0:db 6,7:dw 0,0,0
winbotrec
dw      0,255*256+ 0,1+128,    0,0,1000,1000,0
dw      0,255*256+ 2,128+#6600,0,0, 188,  80,0
dw      0,255*256+10,botlog,    16,18,156,20,0

dw      0,255*256+ 1,ctrbot1,16, 6, 156,   8,0
dw      0,255*256+ 1,ctrbot2,16,42, 156,   8,0
dw      0,255*256+ 1,ctrbot3,16,54, 156,   8,0
dw      0,255*256+ 1,ctrbot4,16,66, 156,   8,0

ctrbot1 dw txtbot1:db 16*7+1,2+128
ctrbot2 dw txtbot2:db 16*6+1,2+128
ctrbot3 dw txtbot3:db 16*8+1,2+128
ctrbot4 dw txtbot4:db 16*4+1,2+128


;==============================================================================
;### MAIN WINDOW ##############################################################
;==============================================================================

menmaidat   equ winmaidat1

windrpnum   db 0    ;dropdown window ID \
winmaidat           ;                   /
dw #3f02,3,30,10,248,114,0,0,248,114,248,61,10000,10000,prgicnsml,maitittxt,0
winmaidat1
dw menmaidat,winmaigrp,wintolgrp:db 17
winmaidat2
db 0:ds 136+14

wintolgrp   db 17,0:dw wintolrec,0,0,256*0+0,0,0,2


gfxtolopn   equ 16*01+wintolrec+4
gfxtolnew   equ 16*02+wintolrec+4
gfxtolsav   equ 16*03+wintolrec+4
gfxtolprt   equ 16*04+wintolrec+4
gfxstybld   equ 16*06+wintolrec+4
gfxstyita   equ 16*07+wintolrec+4
gfxalglft   equ 16*10+wintolrec+4
gfxalgcen   equ 16*11+wintolrec+4
gfxalgrgt   equ 16*12+wintolrec+4
gfxstycol   equ 16*15+wintolrec+4
arrdwngfx   equ 16*04+winmairec+4
gfxfrma     equ 16*05+winmairec+4

wintolrec
dw     00,255*256+0, 2,       0,0,10000,10000,0       ;00=Background1
dw filnew,255*256+10,gfxtolopn,  0,  1, 16,14,0       ;01=Button "New"
dw filopn,255*256+10,gfxtolnew, 16,  1, 16,14,0       ;02=Button "Open"
dw filsav,255*256+10,gfxtolsav, 32,  1, 16,14,0       ;03=Button "Save"
dw     00,255*256+10,gfxtolprt, 48,  1, 16,14,0       ;04=Button "Print"
dw      0,255*256+0, 1,         66,  0,  1,16,0       ;05=seperator
wintolrec_font equ 6
dw fmtfbl,255*256+10,gfxstybld, 68,  1, 16,14,0       ;06=Button "Bold"
dw fmtfit,255*256+10,gfxstyita, 84,  1, 16,14,0       ;07=Button "Italics"
wintolrec1
dw fmtfdf,255*256+0, 1+128+64, -16,  2, 13,12,0       ;08=marked font
dw      0,255*256+0, 1,        102,  0,  1,16,0       ;09=seperator
wintolrec_algn equ 10
dw fmtalf,255*256+10,gfxalglft,104,  1, 16,14,0       ;10=Button "Left"
dw fmtacn,255*256+10,gfxalgcen,120,  1, 16,14,0       ;11=Button "Centered"
dw fmtarg,255*256+10,gfxalgrgt,136,  1, 16,14,0       ;12=Button "Right"
wintolrec2
dw fmtadf,255*256+0, 1+128+64, -16,  2, 13,12,0       ;13=marked alignment
dw      0,255*256+0, 1,        154,  0,  1,16,0       ;14=seperator
dw fmtcol,255*256+10,gfxstycol,156,  1, 16,14,0       ;15=Button "Colour"
dw      0,255*256+0, 1,        174,  0,  1,16,0       ;16=seperator

winmaigrp   db wincnt_all,0:dw winmairec,winmaiclc,0,256*0+0,0,0,winmairec_field+1

winmaiclc
dw       0,      0,  0, 0,     10000,   0,    24,       0     ;Background Top
dw      20,      0, 24, 0,     10000,   0,   -40, 256*1+1     ;Background Sheet
dw       0,      0,-16,256*1+1,10000,   0,    16,       0     ;Background Bottom

dw     100,      0,  1, 0,  -101, 256*1+1,    12,       0     ;Formline Input
dw      61,      0,  3, 0,     8,       0,     8,       0     ;Formline Field Select
dw      77,      0,  1, 0,    12,       0,    12,       0     ;Formline Function Select
dw      94,      0,  3, 0,     8,       0,     8,       0     ;Formline Input OK
dw       1,      0,  1, 0,    60,       0,    12,       0     ;Formline Field Input
dw      20,      0, 14, 0,   -28, 256*1+1,    10,       0     ;Sheet Columns
winmaiclc1
dw       0,      0, 24, 0,    20,       0,   -40, 256*1+1     ;Sheet Rows
dw      20,      0, 24, 0,   -20, 256*1+1,   -32, 256*1+1     ;Sheet Content
dw       0,      0, 14, 0,    20,       0,    10,       0     ;Mark All

dw      0,   0,-8,256*1+1,  -140, 256*1+1,     8,       0     ;Sheet Tabs
dw   -140,256*1+1,-8,256*1+1,  1,       0,     8,       0     ;Sheet Sep
dw   -139,256*1+1,-8,256*1+1,139,       0,     8,       0     ;Sum/Avg Text

winmairec
dw     00,255*256+00,2         ,0,0,0,0,0   ;00=Background Top
dw     00,255*256+00,0         ,0,0,0,0,0   ;01=Background Sheet
dw     00,255*256+00,2         ,0,0,0,0,0   ;02=Background Bottom

winmairec_formu equ 3
dw     00,255*256+32,ctrfrminp ,0,0,0,0,0   ;03=Formline Input

winmairec_next  equ 4
dw     00,255*256+10,arrdwngfx ,0,0,0,0,0   ;04=Formline Field Select
dw     00,255*256+10,gfxfrma   ,0,0,0,0,0   ;05=Formline Function Select
dw     00,255*256+01,ctrfrmoky ,0,0,0,0,0   ;06=Formline Input OK
winmairec_fpos  equ 7
dw     00,255*256+32,ctrcelinp ,0,0,0,0,0   ;07=Formline Field Input
winmairec2
dw barccl,000*256+25,000000000 ,0,0,0,0,0   ;08=Bar Columns
dw barcrw,000*256+25,000000000 ,0,0,0,0,0   ;09=Bar Rows
winmairec_field equ 10  ;**onchange -> set in SymCalc-Extend as well!**
winmairec1
dw fldclk,255*256+25,supcelctr ,0,0,0,0,0   ;10=Sheet Cells
dw mrkall,255*256+19,0         ,0,0,0,0,0   ;11=MarkAll

wincnt_all  equ 15
wincnt_sta  equ 3
dw     00,255*256+00,2         ,0,0,0,0,0   ;12=Sheet Tabs
dw     00,255*256+00,1         ,0,0,0,0,0   ;13=Sheet Sep

winmairec_sum   equ 14
dw     00,255*256+01,ctrbotinf ,0,0,0,0,0   ;14=Bottom Text

ctrbotinf   dw txtbotinf,256*1+2+4+128
txtbotinf   db "Sum ":ds 40
txtbotbsy   db "Calculating...",0

ctrfrmoky   dw txtfrmoky,2+4
txtfrmoky   db "=",0

ctrcelinp   dw txtcelinp,0,0,0,0, 11,0
txtcelinp   ds  11+1
ctrfrminp   dw txtfrminp,0,0,0,0,251,0
txtfrminp   ds 251+1

supcelctr   dw supcelgrp,2560,1200,0,0,3+12, 0
supcelgrp   db 6,0:dw supcelrec,0,0,256*0+0,0,0,2

ctrgrdver   db 2+128,fldmaxx+1
repeat fldmaxx+1
    db 50*2
rend

ctrgrdhor   db 2+000,fldmaxy+1
repeat fldmaxy+1
    db 10*2
rend

ctrcelnum   db 0
ctrcelmem   ds ctrcelmax*6

supcelrec
dw     00,255*256+00,0         ,0,0,10000,10000,0           ;00=Background
dw     00,255*256+07,ctrgrdhor ,0,0,10000,10000,0           ;01=horizontal lines
dw     00,255*256+07,ctrgrdver ,0,0,10000,10000,0           ;02=vertical lines
supcelrec_cur   equ 3
supcelrec1
dw     00,255*256+02,2+8       ,  0,  0,   51,   11,0       ;03=cursor old
dw     00,255*256+02,1+4       ,  0,  0,   51,   11,0       ;04=cursor new
supcelrec_cell  equ 5
supcelrec2
dw     00,255*256+64,128+64+1  , 41, 41,   79,   29,0       ;05=mark


pt1tab
dw 5*0+App_BegCode+48+3 ;len code
dw 5*1+App_BegCode+48+3 ;len data
dw 5*2+App_BegCode+48+3 ;len trns
dw 5*0+App_BegCode+48+1 ;adr code
dw 5*1+App_BegCode+48+1 ;adr data
dw 5*2+App_BegCode+48+1 ;adr trns
read"App-SymCalc-Patch1.asm"
pt1tab0

pt1bnk
dw 5*0+App_BegCode+48+0 ;bnk code
dw 5*1+App_BegCode+48+0 ;bnk data
dw 5*2+App_BegCode+48+0 ;bnk trns
dw gfxtolopn-1
dw gfxtolnew-1
dw gfxtolsav-1
dw gfxtolprt-1
dw gfxstybld-1
dw gfxstyita-1
dw gfxalglft-1
dw gfxalgcen-1
dw gfxalgrgt-1
dw gfxstycol-1
dw arrdwngfx-1
dw gfxfrma-1
dw prgalr2+1
dw ib1cll+1
dw ib1clx+1
dw ibkbyt1+1
dw winmairec2+00+3
dw winmairec2+16+3
dw winmaidat2
pt1bnk0

pt2tab
dw 5*3+App_BegCode+48+3 ;len code
dw 5*4+App_BegCode+48+3 ;len data
dw 5*5+App_BegCode+48+3 ;len trns
dw 5*3+App_BegCode+48+1 ;adr code
dw 5*4+App_BegCode+48+1 ;adr data
dw 5*5+App_BegCode+48+1 ;adr trns
read"App-SymCalc-Patch2.asm"
pt2tab0

pt2bnk
dw 5*3+App_BegCode+48+0 ;bnk code
dw 5*4+App_BegCode+48+0 ;bnk data
dw 5*5+App_BegCode+48+0 ;bnk trns
pt2bnk0
;##!!## remove pt2bnk0-pt1tab from add_trn_mem in header, as only needed during INIT

;*** LAST IN TRANSFER AREA ***
db 0
