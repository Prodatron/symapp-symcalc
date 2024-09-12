;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@     C P C   4 0 B i t - F l o a t i n g - P o i n t - R o u t i n e s      @
;@                    (c) Amstrad/Locomotive Software 1984                    @
;@          disassembled and commented by Prodatron / SymbiosiS 2006          @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- RANDOM NUMBERS -----------------------------------------------------------
;### FLO_RANDOMIZE0 -> RND seek to 0
;### FLO_RANDOMIZE  -> RND seek to (HL)
;### FLO_RND        -> Gets next RND value
;### FLO_LAST_RND   -> Gets current RND value

;--- OPERATIONS ---------------------------------------------------------------
;### FLO_ADD        -> Adds (HL) and (DE) and copies the result in (HL)
;### FLO_SUB        -> Substracts (DE) from (HL) and copies the result in (HL)
;### FLO_SUBX       -> Substracts (HL) from (DE) and copies the result in (HL)
;### FLO_MULT       -> Multiplicates (HL) with (DE) and copies the result in (HL)
;### FLO_DIV        -> Divides (HL) by (DE) and copies the result in (HL)
;### FLO_POT        -> Raises (HL) to the power of (DE) and copies the result in (HL)
;### FLO_VGL        -> Compares (HL) with (DE)

;--- FUNCTIONS ----------------------------------------------------------------
;### FLO_VZW        -> Changes the sign of (HL)
;### FLO_SQR        -> Extracts the root of (HL)
;### FLO_LOG_NAT    -> Gets natural logarythm of (HL)
;### FLO_LOG_DEC    -> Gets 10-logarythm of (HL)
;### FLO_POT_E      -> (HL)=E^(HL)
;### FLO_SIN        -> Calculates the sinus of (HL)
;### FLO_COS        -> Calculates the cosinus of (HL)
;### FLO_TAN        -> Calculates the tangent of (HL)
;### FLO_ARC_TAN    -> Calculates the arcus tangent of (HL)
;### FLO_10A        -> (HL)=(HL)*10^A
;### FLO_SGN        -> Tests the sign of (HL)

;--- MISCELLANEOUS ------------------------------------------------------------
;### FLO_PI         -> Gets PI
;### FLO_MOVE       -> Copies Value in (DE) to (HL)
;### FLO_DEGRAD     -> Set Deg/Rad

;--- CONVERSION ---------------------------------------------------------------
;### FLO_KONV_HLA_TO_FLO    -> Converts 16bit Integer + sign to FLO
;### FLO_KONV_LW_TO_FLO     -> Converts 32bit Integer + sign to FLO
;### FLO_ROUND_FLO_TO_HLA   -> Rounds FLO value and converts it to 16bit Integer + sign
;### FLO_ROUND_FLO_TO_LW    -> Rounds FLO value and converts it to 32bit Integer + sign
;### FLO_FIX_FLO_TO_LW      -> Cuts FLO value and converts it to 32bit Integer + sign
;### FLO_INT_FLO_TO_LW      -> Cuts FLO value (and decrease if negative) and converts it to 32bit Integer + sign
;### FLO_KONV_HLB_TO_INT    -> Converts 16bit integer + sign into 16bit Integer

;--- DISPLAY-PREPARATION ------------------------------------------------------
;### FLO_PREPARE            -> Prepares the display of a FLO value

;---

;--- begin routines -----------------------------------------------------------

;### FLO_PI -> Gets PI
;### Input      (HL)=Destination
;### Output     (HL)=PI, CF=1
;### Unchanged  BC,HL,IX,IY
.FLO_PI
        ld de,FLO_CONST_PI
        jr FLO_MOVE

.l2f7d
        ld de,FLO_CONST_C
        jr FLO_MOVE
.l2f87
        ex de,hl
.l2f88
        ld hl,FLO_BUFFER_3
        jr FLO_MOVE
.l2f8d
        ld de,FLO_BUFFER_1
.l2f90
        ex de,hl

;### FLO_MOVE -> Copies Value in (DE) to (HL)
;### Input      (DE)=Source value, (HL)=Destination
;### Output     (HL)=Value, CF=1
;### Unchanged  A,BC,DE,HL,IX,IY,(DE)
.FLO_MOVE
        push hl
        push de
        push bc
        ex de,hl
        ld bc,#0005
        ldir
        pop bc
        pop de
        pop hl
        scf
        ret


;### FLO_KONV_HLA_TO_FLO -> Converts 16bit Integer + sign to FLO
;### Input      HL=value, A[bit7]=sign, (DE)=destination
;### Output     HL=DE, (HL)=FLO value
;### Unchanged  BC,IX,IY
.FLO_KONV_HLA_TO_FLO
        push de
        push bc
        or #7f
        ld b,a
        xor a
        ld (de),a
        inc de
        ld (de),a
        inc de
        ld c,#90
        or h
        jr nz,l2fbb
        ld c,a
        or l
        jr z,l2fbf
        ld l,h
        ld c,#88
        jr l2fbb
.l2fb7
        dec c
        sla l
        adc a
.l2fbb
        jp p,l2fb7
        and b
.l2fbf
        ex de,hl
        ld (hl),e
        inc hl
        ld (hl),a
        inc hl
        ld (hl),c
        pop bc
        pop hl
        ret

;### FLO_KONV_LW_TO_FLO -> Converts 32bit Integer + sign to FLO
;### Input      (HL)=LW, A[bit7]=sign
;### Output     (HL)=FLO value
;### Unchanged  BC,DE,HL,IY
.FLO_KONV_LW_TO_FLO
        push bc
        ld bc,#a000
        call l2fd3
        pop bc
        ret
        ld b,#a8
.l2fd3
        push de
        call l379c
        pop de
        ret

;### FLO_ROUND_FLO_TO_HLA -> Rounds FLO value and converts it to 16bit Integer + sign
;### Input      (HL)=FLO value
;### Output     HL=abs((HL)), A[bit7]=sign
;###            CF=0 Overflow
;### Unchanged  BC,DE,IY
.FLO_ROUND_FLO_TO_HLA
        push hl
        pop ix
        xor a
        sub (ix+#04)
        jr z,l2ffd
        add #90
        ret nc
        push de
        push bc
        add #10
        call l373d
        sla c
        adc hl,de
        jr z,l2ffa
        ld a,(ix+#03)
        or a
.l2ff6
        ccf
        pop bc
        pop de
        ret
.l2ffa
        sbc a
        jr l2ff6
.l2ffd
        ld l,a
        ld h,a
        scf
        ret

;### FLO_ROUND_FLO_TO_LW -> Rounds FLO value and converts it to 32bit Integer + sign
;### Input      (HL)=FLO value
;### Output     (HL)=abs((HL)), B[bit7]=sign
;###            CF=0 Overflow
;### Unchanged  DE,HL,IY
.FLO_ROUND_FLO_TO_LW
        call FLO_FIX_FLO_TO_LW
        ret nc
        ret p
.l3006
        push hl
        ld a,c
.l3008
        inc (hl)
        jr nz,l3011
        inc hl
        dec a
        jr nz,l3008
        inc (hl)
        inc c
.l3011
        pop hl
        scf
        ret

;### FLO_FIX_FLO_TO_LW -> Cuts FLO value and converts it to 32bit Integer + sign
;### Input      (HL)=FLO value
;### Output     (HL)=abs((HL)), B[bit7]=sign
;###            CF=0 Overflow
;### Unchanged  DE,HL,IY
.FLO_FIX_FLO_TO_LW
        push hl
        push de
        push hl
        pop ix
        xor a
        sub (ix+#04)
        jr nz,l3029
        ld b,#04
.l3021
        ld (hl),a
        inc hl
        djnz l3021
        ld c,#01
        jr l3051
.l3029
        add #a0
        jr nc,l3052
        push hl
        call l373d
        xor a
        cp b
        adc a
        or c
        ld c,l
        ld b,h
        pop hl
        ld (hl),c
        inc hl
        ld (hl),b
        inc hl
        ld (hl),e
        inc hl
        ld e,a
        ld a,(hl)
        ld (hl),d
        and #80
        ld b,a
        ld c,#04
        xor a
.l3047
        or (hl)
        jr nz,l304f
        dec hl
        dec c
        jr nz,l3047
        inc c
.l304f
        ld a,e
        or a
.l3051
        scf
.l3052
        pop de
        pop hl
        ret

;### FLO_INT_FLO_TO_LW -> Cuts FLO value (and decrease if negative) and converts it to 32bit Integer + sign
;### Input      (HL)=FLO value
;### Output     (HL)=abs((HL)), B[bit7]=sign
;###            CF=0 Overflow
;### Unchanged  DE,HL,IY
.FLO_INT_FLO_TO_LW
        call FLO_FIX_FLO_TO_LW
        ret nc
        ret z
        bit 7,b
        ret z
        jr l3006

;### FLO_PREPARE -> Prepares the display of a FLO value
;### Input      (HL)=FLO value
;### Output     (HL)=LW normed mantissa (unsigned)
;###            B = sign of mantissa (0 -> FLO=0)
;###            D = sign of exponent
;###            E = exponent/comma position (decimal)
;###            C = number of significant mantissa bytes (NOT digits!)
;### Unchanged  HL
.FLO_PREPARE
        call FLO_SGN
        ld b,a
        jr z,l30b7
        call m,l3734
        push hl
        ld a,(ix+#04)
        sub #80
        ld e,a
        sbc a
        ld d,a
        ld l,e
        ld h,d
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,de
        add hl,hl
        add hl,de
        add hl,hl
        add hl,hl
        add hl,de
        ld a,h
        sub #09
        ld c,a
        pop hl
        push bc
        call nz,l30c8
.l3085
        ld de,FLO_CONST_A
        call l36e2
        jr nc,l3098
        ld de,FLO_CONST_D1
        call FLO_MULT
        pop de
        dec e
        push de
        jr l3085
.l3098
        ld de,FLO_CONST_B
        call l36e2
        jr c,l30ab
        ld de,FLO_CONST_D1
        call FLO_DIV
        pop de
        inc e
        push de
        jr l3098
.l30ab
        call FLO_ROUND_FLO_TO_LW
        ld a,c
        pop de
.l30b0
        ld b,d
        dec a
        add l
        ld l,a
        ret nc
        inc h
        ret
.l30b7
        ld e,a
        ld (hl),a
        ld c,#01
        ret

;### FLO_KONV_HLB_TO_INT -> Converts 16bit integer + sign into 16bit Integer
;### Input      HL=16 bit integer, B[bit7]=sign
;### Output     HL=signed 16 bit integer
;###            CF=0 Overflow
;### Unchanged  BC,DE,IX,IY
.FLO_KONV_HLB_TO_INT
        ld a,h
        or a
        jp m,ldd42
        or b
        jp m,ldded
        scf
        ret
.ldd42
        xor #80
        or l
        ret nz
        ld a,b
        scf
        adc a
        ret
.ldded
        xor a
        sub l
        ld l,a
        sbc h
        sub l
        cp h
        ld h,a
        scf
        ret nz
        cp #01
        ret

;### FLO_10A -> (HL)=(HL)*10^A
;### Input      (HL)=value, A=potence (signed)
;### Output     (HL)=(HL)*10^A
;###            CF=0 Overflow
;### Unchanged  HL
.FLO_10A
        cpl
        inc a
.l30c8
        or a
        scf
        ret z
        ld c,a
        jp p,l30d1
        cpl
        inc a
.l30d1
        ld de,FLO_CONST_D2
        sub #0d
        jr z,l30ed
        jr c,l30e3
        push bc
        push af
        call l30ed
        pop af
        pop bc
        jr l30d1
.l30e3
        ld b,a
        add a
        add a
        add b
        add e
        ld e,a
        ld a,#ff
        adc d
        ld d,a
.l30ed
        ld a,c
        or a
        jp p,FLO_DIV
        jp FLO_MULT

;### FLO_RANDOMIZE0 -> RND seek to 0
;### Unchanged  AF,BC,DE,IX,IY
.FLO_RANDOMIZE0
        ld hl,#8965
        ld (FLO_VALUE_RND+2),hl
        ld hl,#6c07
        ld (FLO_VALUE_RND),hl
        ret

;### FLO_RANDOMIZE -> RND seek to (HL)
;### Input      (HL)=value
;### Unchanged  C,IY,FLO(HL)
.FLO_RANDOMIZE
        ex de,hl
        call FLO_RANDOMIZE0
        ex de,hl
        call FLO_SGN
        ret z
        ld de,FLO_VALUE_RND
        ld b,#04
.l3151
        ld a,(de)
        xor (hl)
        ld (de),a
        inc de
        inc hl
        djnz l3151
        ret

;### FLO_RND -> Gets next RND value
;### Input      (HL)=Destination
;### Output     (HL)=new RND
;### Unchanged  HL,IY
.FLO_RND
        push hl
        ld hl,(FLO_VALUE_RND+2)
        ld bc,#6c07
        call l319c
        push hl
        ld hl,(FLO_VALUE_RND)
        ld bc,#8965
        call l319c
        push de
        push hl
        ld hl,(FLO_VALUE_RND+2)
        call l319c
        ex (sp),hl
        add hl,bc
        ld (FLO_VALUE_RND),hl
        pop hl
        ld bc,#6c07
        adc hl,bc
        pop bc
        add hl,bc
        pop bc
        add hl,bc
        ld (FLO_VALUE_RND+2),hl
        pop hl

;### FLO_LAST_RND -> Gets current RND value
;### Input      (HL)=Destination
;### Output     (HL)=current RND
;### Unchanged  HL,IY
.FLO_LAST_RND
        push hl
        pop ix
        ld hl,(FLO_VALUE_RND)
        ld de,(FLO_VALUE_RND+2)
        ld bc,#0000
        ld (ix+#04),#80
        jp l37ac
.l319c
        ex de,hl
        ld hl,#0000
        ld a,#11
.l31a2
        dec a
        ret z
        add hl,hl
        rl e
        rl d
        jr nc,l31a2
        add hl,bc
        jr nc,l31a2
        inc de
        jr l31a2

;### FLO_LOG_DEC -> Gets 10-logarythm of (HL)
;### Input      (HL)=value
;### Output     (HL)=LOG10((HL))
;###            CF=0 error, value zero or negative
;### Unchanged  HL
.FLO_LOG_DEC
        ld de,FLO_CONST_LOGDEC
        jr l31b9

;### FLO_LOG_NAT -> Gets natural logarythm of (HL)
;### Input      (HL)=value
;### Output     (HL)=LOG((HL))
;###            CF=0 error, value zero or negative
;### Unchanged  HL
.FLO_LOG_NAT
        ld de,FLO_CONST_LOGNAT
.l31b9
        call FLO_SGN
        dec a
        cp #01
        ret nc
        push de
        call l36d3
        push af
        ld (ix+#04),#80
        ld de,FLO_CONST_LOG
        call FLO_VGL
        jr nc,l31d7
        inc (ix+#04)
        pop af
        dec a
        push af
.l31d7
        call l2f87
        push de
        ld de,FLO_CONST_C
        push de
        call FLO_ADD
        pop de
        ex (sp),hl
        call FLO_SUB
        pop de
        call FLO_DIV
        call l3440
        db #04
        db #4c,#4b,#57,#5e,#7f
        db #0d,#08,#9b,#13,#80
        db #23,#93,#38,#76,#80
        db #20,#3b,#aa,#38,#82

.l3203
        push de
        call FLO_MULT
        pop de
        ex (sp),hl
        ld a,h
        or a
        jp p,l3210
        cpl
        inc a
.l3210
        ld l,a
        ld a,h
        ld h,#00
        call FLO_KONV_HLA_TO_FLO
        ex de,hl
        pop hl
        call FLO_ADD
        pop de
        jp FLO_MULT

;### FLO_POT_E -> (HL)=E^(HL)
;### Input      (HL)=value
;### Output     (HL)=E^(HL)
;###            CF=0 Overflow
;### Unchanged  HL
.FLO_POT_E
        ld b,#e1
        call l3492
        jp nc,l2f7d
        ld de,FLO_CONST_POTE2
        call FLO_VGL
        jp p,l37e8
        ld de,FLO_CONST_POTE3
        call FLO_VGL
        jp m,l37e2
        ld de,FLO_CONST_POTE1
        call l3469
        ld a,e
        jp p,l3255
        neg
.l3255
        push af
        call l3570
        call l2f8d
        push de
        call l3443
        db #03
        db #f4,#32,#eb,#0f,#73
        db #08,#b8,#d5,#52,#7b
.FLO_CONST_HALF                     ;=1/2
        db #00,#00,#00,#00,#80

.l3270
        ex (sp),hl
        call l3443
        db #02
        db #09,#60,#de,#01,#78
        db #f8,#17,#72,#31,#7e

.l327f
        call FLO_MULT
        pop de
        push hl
        ex de,hl
        call FLO_SUB
        ex de,hl
        pop hl
        call FLO_DIV
        ld de,FLO_CONST_HALF
        call FLO_ADD
        pop af
        scf
        adc (ix+#04)
        ld (ix+#04),a
        scf
        ret

;### FLO_SQR -> Extracts the root of (HL)
;### Input      (HL)=value
;### Output     (HL)=sqr(HL)
;###            CF=0 error, negative value
;### Unchanged  HL
.FLO_SQR
        ld de,FLO_CONST_HALF

;### FLO_POT -> Raises (HL) to the power of (DE) and copies the result in (HL)
;### Input      (HL)=first value, (DE)=second value
;### Output     (HL)=(HL)^(DE)
;###            CF=0 Error [S=1 invalid parameter -X^(z/n), P=1 Overflow]
;### Unchanged  HL,FLO(DE)
.FLO_POT
        ex de,hl
        call FLO_SGN
        ex de,hl
        jp z,l2f7d
        push af
        call FLO_SGN
        jr z,l32e2
        ld b,a
        call m,l3734
        push hl
        call l3324
        pop hl
        jr c,l32ed
        ex (sp),hl
        pop hl
        jp m,l32ea
        push bc
        push de
        call FLO_LOG_NAT
        pop de
        call c,FLO_MULT
        call c,FLO_POT_E
.l32d9
        pop bc
        ret nc
        ld a,b
        or a
        call m,FLO_VZW
        scf
        ret
.l32e2
        pop af
        scf
        ret p
        call l37e8
        xor a
        ret
.l32ea
        xor a
        inc a
        ret
.l32ed
        ld c,a
        pop af
        push bc
        push af
        ld a,c
        scf
.l32f3
        adc a
        jr nc,l32f3
        ld b,a
        call l2f8d
        ex de,hl
        ld a,b
.l32fc
        add a
        jr z,l3314
        push af
        call l3570
        jr nc,l331b
        pop af
        jr nc,l32fc
        push af
        ld de,FLO_BUFFER_1
        call FLO_MULT
        jr nc,l331b
        pop af
        jr l32fc
.l3314
        pop af
        scf
        call m,l35fb
        jr l32d9
.l331b
        pop af
        pop af
        pop bc
        jp m,l37e2
        jp l37ea
.l3324
        push bc
        call l2f88
        call FLO_FIX_FLO_TO_LW
        ld a,c
        pop bc
        jr nc,l3331
        jr z,l3334
.l3331
        ld a,b
        or a
        ret
.l3334
        ld c,a
        ld a,(hl)
        rra
        sbc a
        and b
        ld b,a
        ld a,c
        cp #02
        sbc a
        ret nc
        ld a,(hl)
        cp #27
        ret c
        xor a
        ret

;### FLO_DEGRAD -> Set Deg/Rad
;### Input      A=Typ (=0 -> Rad, >0 -> Deg)
;### Unchanged  AF,BC,DE,HL,IX,IY
.FLO_DEGRAD    
        ld (FLO_VALUE_DEGRAD),a
        ret

;### FLO_COS -> Calculates the cosinus of (HL)
;### Input      (HL)=value
;### Output     (HL)=cos(HL)
;###            CF=0 value too big
;### Unchanged  HL
.FLO_COS
        call FLO_SGN
        call m,l3734
        or #01
        jr l3354

;### FLO_SIN -> Calculates the sinus of (HL)
;### Input      (HL)=value
;### Output     (HL)=sin(HL)
;###            CF=0 value too big
;### Unchanged  HL
.FLO_SIN
        xor a
.l3354
        push af
        ld de,FLO_CONST_SINA
        ld b,#f0
        ld a,(FLO_VALUE_DEGRAD)
        or a
        jr z,l3365
        ld de,FLO_CONST_SINB
        ld b,#f6
.l3365
        call l3492
        jr nc,l33a4
        pop af
        call l346a
        ret nc
        ld a,e
        rra
        call c,l3734
        ld b,#e8
        call l3492
        jp nc,l37e2
        inc (ix+#04)
        call l3440
        db #06
        db #1b,#2d,#1a,#e6,#6e
        db #f8,#fb,#07,#28,#74
        db #01,#89,#68,#99,#79
        db #e1,#df,#35,#23,#7d
        db #28,#e7,#5d,#a5,#80
.FLO_CONST_F
        db #a2,#da,#0f,#49,#81

.l33a1
        jp FLO_MULT
.l33a4
        pop af
        jp nz,l2f7d
        ld a,(FLO_VALUE_DEGRAD)
        cp #01
        ret c
        ld de,FLO_CONST_SINC
        jp FLO_MULT

;### FLO_TAN -> Calculates the tangent of (HL)
;### Input      (HL)=value
;### Output     (HL)=tan(HL)
;###            CF=0 error [Z=1 division by zero, S=1 value too big]
;### Unchanged  HL
.FLO_TAN
        call l2f8d
        push de
        call FLO_COS
        ex (sp),hl
        call c,FLO_SIN
        pop de
        jp c,FLO_DIV
        ret

;### FLO_ARC_TAN -> Calculates the arcus tangent of (HL)
;### Input      (HL)=value
;### Output     (HL)=atn(HL)
;### Unchanged  HL
.FLO_ARC_TAN
        call FLO_SGN
        push af
        call m,l3734
        ld b,#f0
        call l3492
        jr nc,l3430
        dec a
        push af
        call p,l35fb
        call l3440
        db #0b
        db #ff,#c1,#03,#0f,#77
        db #83,#fc,#e8,#eb,#79
        db #6f,#ca,#78,#36,#7b
        db #d5,#3e,#b0,#b5,#7c
        db #b0,#c1,#8b,#09,#7d
        db #af,#e8,#32,#b4,#7d
        db #74,#6c,#65,#62,#7d
        db #d1,#f5,#37,#92,#7e
        db #7a,#c3,#cb,#4c,#7e
        db #83,#a7,#aa,#aa,#7f
        db #fe,#ff,#ff,#7f,#80

.l3426
        call FLO_MULT
        pop af
        ld de,FLO_CONST_F
        call p,FLO_SUBX
.l3430
        ld a,(FLO_VALUE_DEGRAD)
        or a
        ld de,FLO_CONST_TAN
        call nz,FLO_MULT
        pop af
        call m,l3734
        scf
        ret
.l3440
        call l3570
.l3443
        call l2f87
        pop hl
        ld b,(hl)
        inc hl
        call l2f90
.l344c
        inc de
        inc de
        inc de
        inc de
        inc de
        push de
        ld de,FLO_BUFFER_2
        dec b
        ret z
        push bc
        ld de,FLO_BUFFER_3
        call FLO_MULT
        pop bc
        pop de
        push de
        push bc
        call FLO_ADD
        pop bc
        pop de
        jr l344c
.l3469
        xor a
.l346a
        push af
        call FLO_MULT
        pop af
        ld de,FLO_CONST_HALF
        call nz,FLO_ADD
        push hl
        call FLO_ROUND_FLO_TO_HLA
        jr nc,l348e
        pop de
        push hl
        push af
        push de
        ld de,FLO_BUFFER_2
        call FLO_KONV_HLA_TO_FLO
        ex de,hl
        pop hl
        call FLO_SUB
        pop af
        pop de
        scf
        ret
.l348e
        pop hl
        xor a
        inc a
        ret
.l3492
        call l36d3
        ret p
        cp b
        ret z
        ccf
        ret

;### FLO_SUB -> Substracts (DE) from (HL) and copies the result in (HL)
;### Input      (HL)=first value, (DE)=second value
;### Output     (HL)=(HL)-(DE)
;###            CF=0 Overflow
;### Unchanged  HL,FLO(DE)
.FLO_SUB
        ld a,#01
        jr l34a3

;### FLO_SUBX -> Substracts (HL) from (DE) and copies the result in (HL)
;### Input      (DE)=first value, (HL)=second value
;### Output     (HL)=(DE)-(HL)
;###            CF=0 Overflow
;### Unchanged  HL,FLO(DE)
.FLO_SUBX
        ld a,#80
        jr l34a3

;### FLO_ADD -> Adds (HL) and (DE) and copies the result in (HL)
;### Input      (HL)=first value, (DE)=second value
;### Output     (HL)=(HL)+(DE)
;###            CF=0 Overflow
;### Unchanged  HL,FLO(DE)
.FLO_ADD
        xor a
.l34a3
        push hl
        pop ix
        push de
        pop iy
        ld b,(ix+#03)
        ld c,(iy+#03)
        or a
        jr z,l34bc
        jp m,l34ba
        rrca
        xor c
        ld c,a
        jr l34bc
.l34ba
        xor b
        ld b,a
.l34bc
        ld a,(ix+#04)
        cp (iy+#04)
        jr nc,l34d8
        ld d,b
        ld b,c
        ld c,d
        or a
        ld d,a
        ld a,(iy+#04)
        ld (ix+#04),a
        jr z,l3525
        sub d
        cp #21
        jr nc,l3525
        jr l34e9
.l34d8
        xor a
        sub (iy+#04)
        jr z,l3537
        add (ix+#04)
        cp #21
        jr nc,l3537
        push hl
        pop iy
        ex de,hl
.l34e9
        ld e,a
        ld a,b
        xor c
        push af
        push bc
        ld a,e
        call l3743
        ld a,c
        pop bc
        ld c,a
        pop af
        jp m,l353c
        ld a,(iy+#00)
        add l
        ld l,a
        ld a,(iy+#01)
        adc h
        ld h,a
        ld a,(iy+#02)
        adc e
        ld e,a
        ld a,(iy+#03)
        set 7,a
        adc d
        ld d,a
        jp nc,l37b7
        rr d
        rr e
        rr h
        rr l
        rr c
        inc (ix+#04)
        jp nz,l37b7
        jp l37ea
.l3525
        ld a,(iy+#02)
        ld (ix+#02),a
        ld a,(iy+#01)
        ld (ix+#01),a
        ld a,(iy+#00)
        ld (ix+#00),a
.l3537
        ld (ix+#03),b
        scf
        ret
.l353c
        xor a
        sub c
        ld c,a
        ld a,(iy+#00)
        sbc l
        ld l,a
        ld a,(iy+#01)
        sbc h
        ld h,a
        ld a,(iy+#02)
        sbc e
        ld e,a
        ld a,(iy+#03)
        set 7,a
        sbc d
        ld d,a
        jr nc,l356d
        ld a,b
        cpl
        ld b,a
        xor a
        sub c
        ld c,a
        ld a,#00
        sbc l
        ld l,a
        ld a,#00
        sbc h
        ld h,a
        ld a,#00
        sbc e
        ld e,a
        ld a,#00
        sbc d
        ld d,a
.l356d
        jp l37ac
.l3570
        ld de,FLO_BUFFER_2
        call l2f90
        ex de,hl

;### FLO_MULT -> Multiplicates (HL) with (DE) and copies the result in (HL)
;### Input      (HL)=first value, (DE)=second value
;### Output     (HL)=(HL)*(DE)
;###            CF=0 Overflow
;### Unchanged  HL,FLO(DE)
.FLO_MULT
        push de
        pop iy
        push hl
        pop ix
        ld a,(iy+#04)
        or a
        jr z,l35ad
        dec a
        call l36af
        jr z,l35ad
        jr nc,l35aa
        push af
        push bc
        call l35b0
        ld a,c
        pop bc
        ld c,a
        pop af
        bit 7,d
        jr nz,l35a3
        dec a
        jr z,l35ad
        sla c
        adc hl,hl
        rl e
        rl d
.l35a3
        ld (ix+#04),a
        or a
        jp nz,l37b7
.l35aa
        jp l37ea
.l35ad
        jp l37e2
.l35b0
        ld hl,#0000
        ld e,l
        ld d,h
        ld a,(iy+#00)
        call l35f3
        ld a,(iy+#01)
        call l35f3
        ld a,(iy+#02)
        call l35f3
        ld a,(iy+#03)
        or #80
.l35cc
        ld b,#08
        rra
        ld c,a
.l35d0
        jr nc,l35e6
        ld a,l
        add (ix+#00)
        ld l,a
        ld a,h
        adc (ix+#01)
        ld h,a
        ld a,e
        adc (ix+#02)
        ld e,a
        ld a,d
        adc (ix+#03)
        ld d,a
.l35e6
        rr d
        rr e
        rr h
        rr l
        rr c
        djnz l35d0
        ret
.l35f3
        or a
        jr nz,l35cc
        ld l,h
        ld h,e
        ld e,d
        ld d,a
        ret
.l35fb
        call l2f87
        ex de,hl
        push de
        call l2f7d
        pop de

;### FLO_DIV -> Divides (HL) by (DE) and copies the result in (HL)
;### Input      (HL)=first value, (DE)=second value
;### Output     (HL)=(HL)/(DE)
;###            CF=0 Overflow (and ZF=1 -> division by zero)
;### Unchanged  HL,FLO(DE)
.FLO_DIV
        push de
        pop iy
        push hl
        pop ix
        xor a
        sub (iy+#04)
        jr z,l366a
        call l36af
        jp z,l37e2
        jr nc,l3667
        push bc
        ld c,a
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ex de,hl
        ld b,(iy+#03)
        set 7,b
        call l369d
        jr c,l3633
        ld a,c
        or a
        jr nz,l3639
        jr l3666
.l3633
        dec c
        add hl,hl
        rl e
        rl d
.l3639
        ld (ix+#04),c
        call l3672
        ld (ix+#03),c
        call l3672
        ld (ix+#02),c
        call l3672
        ld (ix+#01),c
        call l3672
        ccf
        call c,l369d
        ccf
        sbc a
        ld l,c
        ld h,(ix+#01)
        ld e,(ix+#02)
        ld d,(ix+#03)
        pop bc
        ld c,a
        jp l37b7
.l3666
        pop bc
.l3667
        jp l37ea
.l366a
        ld b,(ix+#03)
        call l37ea
        xor a
        ret
.l3672
        ld c,#01
.l3674
        jr c,l367e
        ld a,d
        cp b
.l3678
        call z,l36a0
        ccf
        jr nc,l3691
.l367e
        ld a,l
        sub (iy+#00)
        ld l,a
        ld a,h
        sbc (iy+#01)
        ld h,a
        ld a,e
        sbc (iy+#02)
        ld e,a
        ld a,d
        sbc b
        ld d,a
        scf
.l3691
        rl c
        sbc a
        add hl,hl
        rl e
        rl d
        inc a
        jr nz,l3674
        ret
.l369d
        ld a,d
        cp b
        ret nz
.l36a0
        ld a,e
        cp (iy+#02)
        ret nz
        ld a,h
        cp (iy+#01)
        ret nz
        ld a,l
        cp (iy+#00)
        ret
.l36af
        ld c,a
        ld a,(ix+#03)
        xor (iy+#03)
        ld b,a
        ld a,(ix+#04)
        or a
        ret z
        add c
        ld c,a
        rra
        xor c
        ld a,c
        jp p,l36cf
        set 7,(ix+#03)
        sub #7f
        scf
        ret nz
        cp #01
        ret
.l36cf
        or a
        ret m
        xor a
        ret
.l36d3
        push hl
        pop ix
        ld a,(ix+#04)
        or a
        ret z
        sub #80
        scf
        ret

;### FLO_VGL -> Compares (HL) with (DE)
;### Input      (HL)=first value, (DE)=second value
;### Output     A=Result [-1 -> (HL)<(DE), 0 -> (HL)=(DE), 1 -> (HL)>(DE)]
;###            ZF=1 -> (HL)=(DE), CF=1 -> (HL)<(DE)
;### Unchanged  BC,DE,HL,FLO(HL),FLO(DE)
.FLO_VGL
        push hl
        pop ix
.l36e2
        push de
        pop iy
        ld a,(ix+#04)
        cp (iy+#04)
        jr c,l3719
        jr nz,l3722
        or a
        ret z
        ld a,(ix+#03)
        xor (iy+#03)
        jp m,l3722
        ld a,(ix+#03)
        sub (iy+#03)
        jr nz,l3719
        ld a,(ix+#02)
        sub (iy+#02)
        jr nz,l3719
        ld a,(ix+#01)
        sub (iy+#01)
        jr nz,l3719
        ld a,(ix+#00)
        sub (iy+#00)
        ret z
.l3719
        sbc a
        xor (iy+#03)
.l371d
        add a
        sbc a
        ret c
        inc a
        ret
.l3722
        ld a,(ix+#03)
        jr l371d

;### FLO_SGN -> Tests the sign of (HL)
;### Input      (HL)=value
;### Output     A=sign [-1 -> (HL)<0, 0 -> (HL)=0, 1 -> (HL)>0]
;###            ZF=1 -> (HL)=0, CF=1 -> (HL)<0
;### Unchanged  BC,DE,HL,IY,FLO(HL)
.FLO_SGN
        push hl
        pop ix
        ld a,(ix+#04)
        or a
        ret z
        jr l3722

;### FLO_VZW -> Changes the sign of (HL)
;### Input      (HL)=value
;### Output     (HL)=-(HL)
;### Unchanged  BC,DE,HL,IY
.FLO_VZW
        push hl
        pop ix
.l3734
        ld a,(ix+#03)
        xor #80
        ld (ix+#03),a
        ret
.l373d
        cp #21
        jr c,l3743
        ld a,#21
.l3743
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld c,(hl)
        inc hl
        ld h,(hl)
        ld l,c
        ex de,hl
        set 7,d
        ld bc,#0000
        jr l375e
.l3753
        ld c,a
        ld a,b
        or l
        ld b,a
        ld a,c
        ld c,l
        ld l,h
        ld h,e
        ld e,d
        ld d,#00
.l375e
        sub #08
        jr nc,l3753
        add #08
        ret z
.l3765
        srl d
        rr e
        rr h
        rr l
        rr c
        dec a
        jr nz,l3765
        ret
.l3773
        jr nz,l378c
        ld d,a
        ld a,e
        or h
        or l
        or c
        ret z
        ld a,d
.l377c
        sub #08
        jr c,l379a
        ret z
        ld d,e
        ld e,h
        ld h,l
        ld l,c
        ld c,#00
        inc d
        dec d
        jr z,l377c
        ret m
.l378c
        dec a
        ret z
        sla c
        adc hl,hl
        rl e
        rl d
        jp p,l378c
        ret
.l379a
        xor a
        ret
.l379c
        push hl
        pop ix
        ld (ix+#04),b
        ld b,a
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ex de,hl
.l37ac
        ld a,(ix+#04)
        dec d
        inc d
        call p,l3773
        ld (ix+#04),a
.l37b7
        sla c
        jr nc,l37cd
        inc l
        jr nz,l37cd
        inc h
        jr nz,l37cd
        inc de
        ld a,d
        or e
        jr nz,l37cd
        inc (ix+#04)
        jr z,l37ea
        ld d,#80
.l37cd
        ld a,b
        or #7f
        and d
        ld (ix+#03),a
        ld (ix+#02),e
        ld (ix+#01),h
        ld (ix+#00),l
.l37dd
        push ix
        pop hl
        scf
        ret
.l37e2
        xor a
        ld (ix+#04),a
        jr l37dd
.l37e8
        ld b,#00
.l37ea
        push ix
        pop hl
        ld a,b
        or #7f
        ld (ix+#03),a
        or #ff
        ld (ix+#04),a
        ld (hl),a
        ld (ix+#01),a
        ld (ix+#02),a
        ret

;--- begin variables ----------------------------------------------------------

FLO_VALUE_DEGRAD    db 0
FLO_VALUE_RND       ds 4
FLO_BUFFER_1        ds 5
FLO_BUFFER_2        ds 5
FLO_BUFFER_3        ds 5

;--- begin constants ----------------------------------------------------------

.FLO_CONST_PI                       ;=PI
        db #a2,#da,#0f,#49,#82

.FLO_CONST_LOG
        db #34,#f3,#04,#35,#80
.FLO_CONST_LOGNAT
        db #f8,#17,#72,#31,#80
.FLO_CONST_LOGDEC
        db #85,#9a,#20,#1a,#7f

.FLO_CONST_POTE1
        db #29,#3b,#aa,#38,#81
.FLO_CONST_POTE2
        db #c7,#33,#0f,#30,#87
.FLO_CONST_POTE3
        db #f8,#17,#72,#b1,#87

.FLO_CONST_SINA
        db #6e,#83,#f9,#22,#7f
.FLO_CONST_SINB
        db #b6,#60,#0b,#36,#79
.FLO_CONST_SINC
        db #13,#35,#fa,#0e,#7b
.FLO_CONST_TAN
        db #d3,#e0,#2e,#65,#86

.FLO_CONST_A
        db #f0,#1f,#bc,#3e,#96
.FLO_CONST_B
        db #fe,#27,#6b,#6e,#9e
.FLO_CONST_C
        db #00,#00,#00,#00,#81

.FLO_CONST_D1
        db #00,#00,#00,#20,#84
        db #00,#00,#00,#48,#87
        db #00,#00,#00,#7a,#8a
        db #00,#00,#40,#1c,#8e
        db #00,#00,#50,#43,#91
        db #00,#00,#24,#74,#94
        db #00,#80,#96,#18,#98
        db #00,#20,#bc,#3e,#9b
        db #00,#28,#6b,#6e,#9e
        db #00,#f9,#02,#15,#a2
        db #40,#b7,#43,#3a,#a5
        db #10,#a5,#d4,#68,#a8
.FLO_CONST_D2
        db #2a,#e7,#84,#11,#ac

.FLO_CONST_1NEG
        db #00,#00,#00,#80,#81
.FLO_CONST_0
        db #00,#00,#00,#00,#00
.FLO_CONST_1
        db #00,#00,#00,#00,#81
.FLO_CONST_2
        db #00,#00,#00,#00,#82
.FLO_CONST_3
        db #00,#00,#00,#40,#82
.FLO_CONST_100
        db #00,#00,#00,#48,#87
.FLO_CONST_1D3                  ;1/3
        db #ab,#aa,#aa,#2a,#7f
.FLO_CONST_E                    ;euler
        db #59,#54,#f8,#2d,#82
