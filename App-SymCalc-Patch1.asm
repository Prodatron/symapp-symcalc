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
