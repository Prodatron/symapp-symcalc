;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               S y m C a l c                                @
;@                            (memory management)                             @
;@                                                                            @
;@             (c) 2024-2024 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- MEMORY MANAGEMENT --------------------------------------------------------
;### MEMNEW -> adds a new element
;### MEMRSZ -> resizes an existing element (content may be destroyed, if new>old)
;### MEMDEL -> removes an existing element
;### MEMGRB -> does a garbage collection
;### MEMSIZ -> changes size of free memory
;### MEMLST -> tests, if element is last one
;### MEMEMP -> tests, if element is empty
;### MEMINC -> increase number of elements
;### MEMDEC -> decrease number of elements
;### MEMPOI -> updates "who points here" and returns new address


;==============================================================================
;### MEMORY MANAGEMENT ########################################################
;==============================================================================

;test cases
;- memful for new
;- memful for resize
;- combine wholes which are too big


memstrbeg   equ 0       ;start address of the memory area
memstrend   equ 2       ;first byte of the free area
memstrmax   equ 4       ;maximum length of the memory area
memstrfre   equ 6       ;free bytes (headers of empty elements do count as free, too)
memstrnum   equ 8       ;number of existing elements (including empty ones)

memhedsiz   equ 0       ;length of record including header; <3 -> unused
memhedpoi   equ 1       ;who points here (if len>2); 0 -> unused


;### MEMNEW -> adds a new element
;### Input      IX=memory management record, C=payload length (1-252), HL=pointer, CF=flag, if textdata
;### Output     CF=0 -> ok, (pointer),HL=address, CF=1 -> memory full
;### Destroyed  AF,BC,DE,IY
memnew  call mempoia
memnew0 ld b,0
        inc c:inc c:inc c       ;C=required length
        ld l,(ix+memstrfre+0)
        ld h,(ix+memstrfre+1)
        or a
        sbc hl,bc
        ret c                   ;memory full
        ld (ix+memstrfre+0),l
        ld (ix+memstrfre+1),h   ;update free memory
        ld a,(ix+memstrbeg+0)
        ld iyl,a
        ld a,(ix+memstrbeg+1)
        ld iyh,a                ;IY=memory begin
        ld l,(ix+memstrnum+0)
        ld h,(ix+memstrnum+1)
        inc hl
        ld (memnew1+1),hl

memnew1 ld hl,0                     ;** search empty hole
        dec hl                  ;* loop beg
        ld a,l
        or h
        jr z,memnew4            ;try to add at end or do garbage collection
        ld (memnew1+1),hl
        ld e,(iy+memhedsiz)
        ld d,0                  ;DE=length of free element including header
        ld a,e
        cp c
        jr c,memnew2            ;hole too small -> skip
        ld a,(iy+memhedpoi+0)
        or   (iy+memhedpoi+1)
        jr z,memnew3            ;element free -> use it
memnew2 add iy,de               ;* loop end -> next one
        jr memnew1

memnew3 ld a,e                  ;a=free element
        sub c                   ;a=free-required
        ld (iy+memhedsiz+0),c   ;use hole for element
        jp z,mempoi             ;size=hole -> just use it
        push iy:pop hl          ;hole is bigger -> split it into new element and remaining hole
        add hl,bc
        ld (hl),a               ;set empty element
        push hl
        call meminc
        neg
        ld c,a
        ld b,-1
        call memsiz             ;remove new empty element from free memory, as it will be freed again
        pop hl
        push iy
        call memdel             ;delete "empty" element (maybe merge with next)
        pop hl
        jp mempoi0

memnew4 ld l,(ix+memstrend+0)       ;** no hole found -> place element at the end
        ld h,(ix+memstrend+1)
        add hl,bc
        call memnew5            ;check, if free at end

        push bc
        call c,memgrb           ;not enough free at end -> garbage collection
        pop bc

        call meminc
        ld l,(ix+memstrend+0)
        ld h,(ix+memstrend+1)
        ld (hl),c
        push hl
        call memnew6
        pop hl
        jp mempoi0
memnew6 add hl,bc
        ld (ix+memstrend+0),l
        ld (ix+memstrend+1),h
        ret
memnew5 ld e,(ix+memstrbeg+0)
        ld d,(ix+memstrbeg+1)
        sbc hl,de
        ex de,hl                ;de=len behind extended element
        ld l,(ix+memstrmax+0)
        ld h,(ix+memstrmax+1)
        sbc hl,de
        ret

;### MEMRSZ -> resizes an existing element (content may be destroyed, if new>old)
;### Input      IX=memory management record, HL=pointer to element, C=new payload length (1-252), CF=flag, if textdata
;### Output     CF=0 -> ok, (pointer),HL=address, CF=1 -> memory full
;### Destroyed  AF,BC,DE,IY
memrsz  ld e,(hl)
        inc hl
        ld d,(hl)
        dec hl
        call mempoia
        ex de,hl
        inc c:inc c:inc c
        ld a,(hl)
        sub c                   ;a=old-new
        ret z                       ;** new is same
        jr c,memrsz1
        ld b,0                      ;** new is smaller
        ld (hl),c               ;shrink element
        push hl
        add hl,bc
        ld (hl),a               ;add new element behind
        push hl
        call meminc
        pop hl
        call memdel             ;delete element behind
        pop hl
        jp mempoi0              ;update "who points here"
memrsz1 ld e,(ix+memstrfre+1)       ;** new is larger
        ld (memrsz6+1),a
        neg                     ;a=new-old=additional size
        inc e:dec e
        jr nz,memrsz2
        cp (ix+memstrfre+0)
        jr z,memrsz2
        ccf
        ret c                   ;memory full
memrsz2 call memlst             ;hl=adr behind element
        jr nz,memrsz3

        ld e,a                      ;** element is last
        ld d,0
        add hl,de               ;hl=adr behind extended element
        call memnew5

        jr c,memrsz7            ;not enough free behind
        ld c,a
        add (iy+memhedsiz)      ;last element can be extended
        ld (iy+memhedsiz),a
        ld b,0
        ld l,(ix+memstrend+0)
        ld h,(ix+memstrend+1)
        call memnew6
memrsz6 ld c,0                  ;decrease free size
        ld b,-1
        call memsiz
        jp mempoi               ;return unmodified element address
memrsz3 ld b,a                      ;** element not last
        call mememp             ;c=len next
        jr nc,memrsz5           ;next not empty
        ld a,c
        sub b                   ;b=additional size, a=new next len
        jr c,memrsz5            ;next not big enough
        push af                     ;** next empty and big enough
        ld a,(iy+memhedsiz)
        add b
        ld (iy+memhedsiz),a     ;extend element
        ld b,a
        pop af
        jr nz,memrsz4
        call memdec                 ;** next fits exactly -> just delete it
        jr memrsz6
memrsz4 ld c,b                      ;** next is larger
        ld b,0
        push iy:pop hl
        add hl,bc
        ld (hl),a               ;move forward and shrink it
        cp 3
        jr c,memrsz6
        xor a
        inc hl
        ld (hl),a
        inc hl
        ld (hl),a
        jr memrsz6
memrsz5 ld a,b                      ;** element can't be extended -> remove and recreate
memrsz7 push iy:pop hl
        add (hl)
        push af
        call memdel
        pop af
        sub 3
        ld c,a
        jp memnew0

;### MEMDEL -> removes an existing element
;### Input      IX=memory management record, HL=address
;### Destroyed  F,BC,DE,HL,IY
memdel  call memlst
        call memsiz
        jr z,memdel2
        ld a,(iy+memhedsiz)         ;** not last -> mark this element as "free" (if not already)
        call memdel3
        call mememp
        ret nc
        ld a,(iy+memhedsiz)         ;** next element is free, too
        add c
        jr c,memdel1            ;too long
        ld (iy+memhedsiz),a     ;not too long -> combine it
        call memdel3
        jp memdec               ;remove next
memdel1 inc c
        ret z                   ;next has full length -> already optimized
        ld c,255                    ;** optimize length
        ld (iy+memhedsiz),c     ;this gets full length
        call memdel4
        push iy:pop hl
        add hl,bc
        inc a
        ld (hl),a               ;next gets remaining length
        cp 3
        ret c
        xor a
        inc hl:ld (hl),a
        inc hl:ld (hl),a
        ret
memdel2 push iy:pop hl              ;** last element -> remove it
        ld (ix+memstrend+0),l
        ld (ix+memstrend+1),h   ;free area starts with this element
        jp memdec
memdel3 cp 3
        ret c
memdel4 ld (iy+memhedpoi+0),b
        ld (iy+memhedpoi+1),b
        ret

;### MEMGRB -> does a garbage collection
;### Input      IX=memory management record
;### Destroyed  AF,BC,DE,HL,IY
memgrb  ld e,(ix+memstrbeg+0)
        ld d,(ix+memstrbeg+1)       ;DE=destination
        ld l,e
        ld h,d                      ;HL=source
        ld c,(ix+memstrnum+0)
        ld b,(ix+memstrnum+1)
        push bc:pop iy              ;IY=number of entries
memgrb1 ld a,iyl
        or iyh
        jr z,memgrb3
        dec iy
        ld a,(hl)
        ld c,a
        ld b,0
        cp 3
        jr c,memgrb2
        inc hl:ld a,(hl)
        inc hl:or (hl)
        dec hl:dec hl
        jr z,memgrb2
        push de                     ;element used -> move it
        ldir
        ex (sp),hl                  ;hl=new adr
        push de
        call mempoib                ;update pointer(s)
        pop de
        pop hl
        jr memgrb1
memgrb2 push hl
        call memdec                 ;element empty -> delete and skip it
        pop hl
        add hl,bc
        jr memgrb1
memgrb3 ld (ix+memstrend+0),e
        ld (ix+memstrend+1),d       ;free area starts behind last shifted element
        ret

;### MEMSIZ -> changes size of free memory
;### Input      BC=free size difference
;### Destroyed  CF,DE
memsiz  ld e,(ix+memstrfre+0)   ;increase number of free bytes by element length
        ld d,(ix+memstrfre+1)
        ex de,hl
        add hl,bc
        ex de,hl
        ld (ix+memstrfre+0),e
        ld (ix+memstrfre+1),d
        ret

;### MEMLST -> tests, if element is last one
;### Input      IX=memory management record, HL=element
;### Output     IY=element, HL=address behind element, BC=element length, ZF=1 -> last one
;### Destoyed   F,DE
memlst  push hl:pop iy
        ld c,(hl)
        ld b,0                  ;BC=data length
        add hl,bc               ;HL points behind element
        push hl
        ld e,(ix+memstrend+0)
        ld d,(ix+memstrend+1)
        or a
        sbc hl,de               ;test, if last
        pop hl
        ret

;### MEMEMP -> tests, if element is empty
;### Input      HL=element
;### Output     CF=1 empty, C=length
;### Destroyed  AF
mememp  ld a,(hl)               ;test, if next element is free (as it's not the last, there is always a next one)
        ld c,a
        cp 3
        ret c
        inc hl
        ld a,(hl)
        inc hl
        or (hl)
        ret nz
        dec hl
        dec hl
        scf
        ret

;### MEMINC -> increase number of elements
;### Input      IX=memory management record
;### Destroyed  HL
meminc  ld l,(ix+memstrnum+0)   ;increase number of elements
        ld h,(ix+memstrnum+1)
        inc hl
        ld (ix+memstrnum+0),l
        ld (ix+memstrnum+1),h
        ret

;### MEMDEC -> decrease number of elements
;### Input      IX=memory management record
;### Destroyed  HL
memdec  ld l,(ix+memstrnum+0)   ;decrease number of elements
        ld h,(ix+memstrnum+1)
        dec hl
        ld (ix+memstrnum+0),l
        ld (ix+memstrnum+1),h
        ret

;### MEMPOI -> updates "who points here" and returns new address
;### Input      (mempoi0+1)=pointer, IY/HL=new address
;### Output     HL=new address, CF=0
mempoi  push iy:pop hl
mempoi0 ld de,0         ;de=who points here
        inc hl
        ld (hl),e       ;save it to data structure
        inc hl
        ld (hl),d
        dec hl
        dec hl
mempoi4 ex de,hl        ;update data adr to who points here
        ld (hl),e
        inc hl
        ld (hl),d
mempoi1 ld hl,mempoi2   ;dummy/call mempoi2
        ex de,hl        ;hl=data
        or a
        ret

mempoi2 dec hl              ;** update text control pointer
        dec hl
        ld l,(hl)
        inc l:dec l
        ret z
        ld h,0
        add hl,hl   ;*2
        ld c,l:ld b,h
        add hl,hl   ;*4
        add hl,bc   ;*6
        ld bc,ctrcelmem-6
        add hl,bc
        push de
        inc de:inc de:inc de
        ld (hl),e
        inc hl
        ld (hl),d
        pop de
        ret

mempoia ld (mempoi0+1),hl
mempoic ld a,#21
        jr nc,mempoi3
        ld a,#cd
mempoi3 ld (mempoi1),a
        ret

;for garbage collection -> HL=element head -> update pointer at "who points here"
mempoib inc hl
        ld e,(hl)
        inc hl
        ld d,(hl)       ;de=pointer
        dec hl:dec hl   ;hl=element adr
        jr mempoi4
