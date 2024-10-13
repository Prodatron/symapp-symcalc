;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               S y m C a l c                                @
;@                           (common sub routines)                            @
;@                                                                            @
;@             (c) 2024-2024 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;--- SUB ROUTINES -------------------------------------------------------------

;### CHRTRM -> check for terminator
;### CHRUCS -> uppercase
;### STRLEN -> get string length
;### STRINI -> inits string input control
;### CNVS16 -> converts string into number
;### CNV08S -> converts 8bit to decimal string
;### CNV32S -> Converts 32Bit-number (unsigned) to string (terminated by 0)

;---



;==============================================================================
;### SUB ROUTINES #############################################################
;==============================================================================

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
