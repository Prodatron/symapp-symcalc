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


;==============================================================================
;### PULL DOWN / CONTEXT MENUS ################################################
;==============================================================================

;main
menmaidat   dw 7, 1+4,menmaitxt1, menmaidat1,0
            dw    1+4,menmaitxt2, menmaidat2,0
            dw    1+4,menmaitxt3, menmaidat3,0
            dw    1+4,menmaitxt4, menmaidat4,0
            dw    1+4,menmaitxt5, menmaidat5,0
            dw    1+4,menmaitxt6, menmaidat6,0
            dw    1+4,menmaitxt7, menmaidat7,0

menmaidat1  dw 11,1,menmaitxt11,filnew,0, 1,menmaitxt12,filopn,0, 1,menmaitxt13,filsav,0, 1,menmaitxt14,filsas,0, 1+8,0,0,0, 1+4,menmaitxt15,menmaidat15,0, 1+4,menmaitxt16,menmaidat16,0
            dw 1+8,0,0,0, 1,menmaitxt17,prpopn,0, 1+8,0,0,0, 1,menmaitxt10,prgend0,0
menmaidat15 dw 2, 1,menmaitxt151,filimc,0,1,menmaitxt152,filims,0
menmaidat16 dw 2, 1,menmaitxt151,filexc,0,1,menmaitxt152,filexs,0

menmaidat2  dw 8, 1,menmaitxt21,copcut,0, 1,menmaitxt22,copcop,0
menmaidat2a dw    0,menmaitxt23,coppst,0, 1+8,0,0,0, 1,menmaitxt24,fldclr,0, 1+8,0,0,0, 1,menmaitxt25,mrkall,0, 1+4,menmaitxt26,menmaidat26,0
menmaidat26 dw 2, 1,menmaitxt261,mencsl,0,1,menmaitxt262,menrsl,0

menmaidat3  dw 2, 1+2,menmaitxt31,mentol,0, 1+2,menmaitxt32,mensta,0

menmaidat4  dw 7, 1+4,menmaitxt41,menmaidat41,0,1+4,menmaitxt42,menmaidat42,0,1+4,menmaitxt43,menmaidat43,0, 1+8,0,0,0, 1,menmaitxt44 ,cfdopn,0,1+4,menmaitxt45,menmaidat45,0,1+4,menmaitxt46,menmaidat46,0
menmaidat41 dw 3, 1,menmaitxt411,mensnr,0,1,menmaitxt412,mensbl,0,1,menmaitxt413,mensit,0
menmaidat42 dw 4, 1,menmaitxt421,menagn,0,1,menmaitxt422,menalf,0,1,menmaitxt423,menacn,0,1,menmaitxt424,menarg,0
menmaidat43 dw 10,1,menmaitxt431,mennfl,0,1,menmaitxt432,menndt,0,1,menmaitxt433,menntm,0,1,menmaitxt434,mennpr,0,1,menmaitxt435,mennex,0,1,menmaitxt436,mennbl,0,1,menmaitxt437,mennbn,0,1,menmaitxt438,mennhx,0, 1+8,0,0,0, 1,menmaitxt439,mensep,0
menmaidat45 dw 1, 1,menmaitxt451,mencsz,0
menmaidat46 dw 1, 1,menmaitxt461,menrsz,0

menmaidat5  dw 9, 1+4,menmaitxt51,menmaidat51,0, 1,menmaitxt52,movrwi,0, 1,menmaitxt53,movcli,0, 1+8,0,0,0, 1+4,menmaitxt54,menmaidat54,0, 1,menmaitxt55,movrwr,0, 1,menmaitxt56,movclr,0, 1+8,0,0,0, 1,menmaitxt57,fldclr,0
menmaidat51 dw 4, 1,menmaitxt511, movcid, 0, 1,menmaitxt512, movcir, 0, 1,menmaitxt513, movrwi, 0, 1,menmaitxt514, movcli, 0
menmaidat54 dw 4, 1,menmaitxt541, movcru, 0, 1,menmaitxt542, movcrl, 0, 1,menmaitxt543, movrwr, 0, 1,menmaitxt544, movclr, 0

menmaidat6  dw 1, 1,menmaitxtx ,000000,0
menmaidat7  dw 3, 1,menmaitxt71,prghlp,0, 1+8,0,0,0, 1,menmaitxt72,prginf,0

menmaitxt1  db "File",0
menmaitxt11 db "New",0
menmaitxt12 db "Open...",0
menmaitxt13 db "Save",0
menmaitxt14 db "Save as...",0
menmaitxt15 db "Import",0
menmaitxt151 db "CSV file...",0
menmaitxt152 db "SYLK file...",0
menmaitxt16 db "Export",0
menmaitxt17 db "Properties...",0
menmaitxt10 db "Exit",0

menmaitxt2  db "Edit",0
menmaitxt21 db "Cut",0
menmaitxt22 db "Copy",0
menmaitxt23 db "Paste",0
menmaitxt24 db "Clear cell(s)",0
menmaitxt25 db "Select All",0
menmaitxt26 db "Select",0
menmaitxt261 db "Column",0
menmaitxt262 db "Row",0

menmaitxt3  db "View",0
menmaitxt31 db "Tool bar",0
menmaitxt32 db "Status bar",0

menmaitxt4  db "Format",0
menmaitxt41 db "Style",0
menmaitxt411    db "Normal",0
menmaitxt412    db "Bold",0
menmaitxt413    db "Italic",0
menmaitxt42 db "Alignment",0
menmaitxt421    db "General",0
menmaitxt422    db "Left",0
menmaitxt423    db "Centered",0
menmaitxt424    db "Right",0
menmaitxt43 db "Number format",0
menmaitxt431    db "Number",0
menmaitxt432    db "Date",0
menmaitxt433    db "Time",0
menmaitxt434    db "Percentage",0
menmaitxt435    db "Scientific",0
menmaitxt436    db "Boolean",0
menmaitxt437    db "Binary",0
menmaitxt438    db "Hexadecimal",0
menmaitxt439    db "1000 separator",0
menmaitxt44 db "Cells...",0
menmaitxt45 db "Columns",0
menmaitxt451    db "Width...",0
menmaitxt46 db "Rows",0
menmaitxt461    db "Height...",0

menmaitxt5  db "Table",0
menmaitxt51 db "Insert cell(s)",0
menmaitxt511    db "Move cells down",0
menmaitxt512    db "Move cells right",0
menmaitxt513    db "Insert whole row(s)",0
menmaitxt514    db "Insert whole column(s)",0
menmaitxt52 db "Insert row(s)",0
menmaitxt53 db "Insert column(s)",0
menmaitxt54 db "Remove cell(s)",0
menmaitxt541    db "Move cells up",0
menmaitxt542    db "Move cells left",0
menmaitxt543    db "Remove whole row(s)",0
menmaitxt544    db "Remove whole column(s)",0
menmaitxt55 db "Remove row(s)",0
menmaitxt56 db "Remove column(s)",0
menmaitxt57 db "Clear cell content",0

menmaitxt6  db "Data",0

menmaitxt7  db "?",0
menmaitxt71 db "Help topics",0
menmaitxt72 db "About",0

menmaitxtx  db "Coming soon...",0


;### ALERT WINDOW #############################################################

prgtxtinf   dw prgtxtinf1,4*1+2,prgtxtinf2,4*1+2,prgtxtinf3,4*1+2,0,prgicnbig,prgicn16c
prgtxtinf1  db "SymCalc Spreadsheet",0
prgtxtinf2  db " Version 0.8 (Build "
read "..\..\..\SRC-Main\build.asm"
            db "pdt)",0
prgtxtinf3  db " Copyright <c> 2024 SymbiosiS"
prgtxtinf0  db 0

prgtxtsav   dw prgtxtsav1,4*1+2,prgtxtinf0,4*1+2,prgtxtinf0,4*1+2
prgtxtsav1  db "Save changes?",0

;error - load/save
prgerrinfa  dw prgerrtxt1a,4*1+2,prgerrtxt2a,4*1+2,prgerrtxt3a,4*1+2
prgerrinfb  dw prgerrtxt1a,4*1+2,prgerrtxt2b,4*1+2,prgerrtxt3b,4*1+2
prgerrinfc  dw prgerrtxt1a,4*1+2,prgerrtxt2c,4*1+2,prgerrtxt3c,4*1+2
prgerrinfd  dw prgerrtxt1a,4*1+2,prgerrtxt2d,4*1+2,prgerrtxt3d,4*1+2
prgerrinfe  dw prgerrtxt1a,4*1+2,prgerrtxt2e,4*1+2,prgerrtxt3e,4*1+2

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


;==============================================================================
;### BOOT WINDOW ##############################################################
;==============================================================================

winbotdat    dw #0081,4+8+16,58+63,70, 108, 63,0,0, 108, 63, 108, 63, 108, 63,0,0,0,0,winbotgrp,0,0:ds 136+14

winbotgrp    db 5,0: dw winbotrec,0,0:db 6,7:dw 0,0,0
winbotrec
dw      0,255*256+ 0,1+128,     0, 0,1000,1000,0
dw      0,255*256+ 2,128+#6600, 0, 0, 108,  63,0

dw      0,255*256+ 1,ctrbot1, 0, 9, 108,   9,0
dw      0,255*256+ 1,ctrbot2, 0,28, 108,   9,0
dw      0,255*256+ 1,ctrbot3, 0,46, 108,   9,0

ctrbot1 dw txtbot1:db 16*8+1,2+128
ctrbot2 dw txtbot2:db 16*2+1,2+128
ctrbot3 dw txtbot3:db 16*6+1,2+128

txtbot1 db "SymCalc",0
txtbot2 db "(c) 2024 SymbiosiS",0
txtbot3 db "Loading...",0


;==============================================================================
;### MAIN WINDOW ##############################################################
;==============================================================================

windrpnum   db 0    ;dropdown window ID \
winmaidat           ;                   /
dw #3f02,3,30,10,248,114,0,0,248,114,248,61,10000,10000,prgicnsml,maitittxt,0,menmaidat,winmaigrp,wintolgrp,17:ds 136+14

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
dw     00,255*256+32,ctrfrminp ,0,0,0,0,0   ;07=Formline Input
dw     00,255*256+10,arrdwngfx ,0,0,0,0,0   ;04=Formline Field Select
dw     00,255*256+10,gfxfrma   ,0,0,0,0,0   ;05=Formline Function Select
dw     00,255*256+01,ctrfrmoky ,0,0,0,0,0   ;06=Formline Input OK
winmairec_fpos  equ 7
dw     00,255*256+32,ctrcelinp ,0,0,0,0,0   ;03=Formline Field Input
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
dw winmairec2+00+3
dw winmairec2+16+3
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
;##!!## remove pt2bnk0-pt1tab from add_trn_mem in header

;*** LAST IN TRANSFER AREA ***
db 0
