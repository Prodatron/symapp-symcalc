dw gfxtolopn
dw gfxtolnew
dw gfxtolsav
dw gfxtolprt
dw gfxstybld
dw gfxstyita
dw gfxalglft
dw gfxalgcen
dw gfxalgrgt
dw gfxstycol
dw arrdwngfx
dw gfxfrma
dw prgerrinf
dw hshini
dw hshnew
dw hshrem
dw hshupd
dw hshfnd
dw prgstk2
dw prgstk2a
dw guijmp
dw barmem
dw barini0
dw barini1
dw barmov
dw barset
dw barofs
dw barmen
dw barsdi
dw celmen
dw cfgmem
dw cfgsav
dw prpdef
dw prpsav
dw prplod
dw menmaidat
dw menmaidat2a
dw menmaidat30
dw menmaidat31

;gui return
ret_cfdoky  equ 1
ret_barsdi  equ 2
ret_prpoky  equ 3
ret_fmtaln  equ 4

;bar context menu
barmcs  equ  1      ;mark single column
barmcm  equ  2      ;mark multiple columns
barmrs  equ  3      ;mark single rows
barmrm  equ  4      ;mark multiple rows
barmsc  equ  5      ;set column size
barmsr  equ  6      ;set row size

barmci  equ  7      ;column insert
barmcr  equ  8      ;column remove
barmri  equ  9      ;row insert
barmrr  equ 10      ;row remove

;cell context menu
cmncut  equ 1       ;cut
cmncop  equ 2       ;copy
cmnpst  equ 3       ;paste
cmnclr  equ 4       ;clear
cmnfmt  equ 5       ;format dialogue

cmncid  equ 6       ;cells insert down
cmncir  equ 7       ;cells insert right
cmnrwi  equ 8       ;rows insert
cmncmi  equ 9       ;columns insert
cmncru  equ 10      ;cells remove up
cmncrl  equ 11      ;cells remove left
cmnrwr  equ 12      ;rows remove
cmncmr  equ 13      ;columns remove


;main menu (cur last=54)
mai_filnew  equ 1   ;file
mai_filopn  equ 2
mai_filsav  equ 3
mai_filsas  equ 4
mai_filimc  equ 5
mai_filims  equ 6
mai_filexc  equ 7
mai_filexs  equ 8
mai_prpopn  equ 9
mai_prgend0 equ 10

mai_copcut  equ 11  ;edit
mai_copcop  equ 12
mai_coppst  equ 13
mai_fldclr  equ 14
mai_mrkall  equ 15
mai_mencsl  equ 16
mai_menrsl  equ 17

mai_mentol  equ 18  ;view
mai_mensta  equ 19
mai_cfgopn  equ 20

mai_cfdopn  equ 21  ;format
mai_mensnr  equ 22
mai_mensbl  equ 23
mai_mensit  equ 24
mai_menagn  equ 25
mai_menalf  equ 26
mai_menacn  equ 27
mai_menarg  equ 28

mai_mennfl  equ 29
mai_menndt  equ 30
mai_menntm  equ 31
mai_mennpr  equ 32
mai_mennex  equ 33
mai_mennbl  equ 34
mai_mennbn  equ 35
mai_mennhx  equ 36
mai_mensep  equ 37
mai_mencsz  equ 38
mai_menrsz  equ 39

mai_movrwi  equ 40  ;table
mai_movcli  equ 41
mai_movrwr  equ 42
mai_movclr  equ 43
mai_movcid  equ 44
mai_movcir  equ 45
mai_movcru  equ 46
mai_movcrl  equ 47

mai_prghlp  equ 48
mai_prginf  equ 49
