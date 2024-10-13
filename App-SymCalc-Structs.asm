;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               S y m C a l c                                @
;@                           (structs & constants)                            @
;@                                                                            @
;@             (c) 2024-2024 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


fldmaxx equ 63
fldmaxy equ 253

date_year_min    equ 1980
date_year_max    equ 2047


celtypnum   equ 1   ;number
celtypfrn   equ 2   ;formula number
celtyptxt   equ 3   ;text
celtyptxf   equ 4   ;forced text  (N/A)
celtypfrt   equ 5   ;formula text (N/A)
celtyperf   equ 6   ;formula with syntax error
celtyperv   equ 7   ;formula with value error

celrectyp   equ 0   ;type (0=undefined, 1=constant number, 2=number formula, 3=constant text, 4=forced text, 5=text formula,
                    ;      6=formula with syntax error [data -> original text], 7=formula with reference/calculation error [data -> see type 2],
                    ;      +64=recalculation mark, +128=redraw mark)
celrecclm   equ 1   ;column
celrecrow   equ 2   ;line
celrecctr   equ 3   ;control ID+1 or 0
celrectxt   equ 4   ;pointer to text (displayed in sheet)
celrecdat   equ 6   ;pointer to data
celrecref   equ 8   ;[b0-3] number of single references,
                    ;[b4-7] number of range references
celreccol   equ 9   ;[b0-3] Paper,
                    ;[b4-7] Pen
celrecdsp   equ 10  ;[b0-1] Font (0=system, 1=bold, 2=italics)
                    ;[b2]   V-alignment (0=middle, 1=lower) [*TODO*] (middle because there is a useful gap to both up/down cells)
                    ;[b3-5] unit -> Front/End-Chars (0=no, 1-7=configurable),
                    ;       type -> time -> format (predef "hh;mm", "hh;mm;ss", "ht;mm PM", "ht;mm;ss PM", "dd.mm.yyyy hh;mm", "mm/dd/yyyy hh;mm", "yyyy-mm-dd hh;mm")
                    ;               date -> format (predef "ww, dd.mmm", "dd.mmm yyyy", "ww, dd.mmm yy", "dd.mm.yyyy", "mm/dd/yyyy", "yyyy-mm-dd")
                    ;[b6-7] H-alignment (0=default [text left, number right], 1=left, 2=right, 3=center)
celrecfmt   equ 11  ;[b0-2] float/%  -> comma (0-6, 7=as it is)
                    ;       hex      -> digits (7=as it is, 1, 2, 3, 4, 5, 6, 8)
                    ;       bin      -> digits (7=as it is, 1, 4, 8,12,16,24,32)
                    ;[b3]   1000er (3er ./, for dec, 4er ./space for hex/bin),
                    ;[b4]   *res*,
                    ;[b5-7] category (0=decimal, 1=exponent, 2=%, 3=date, 4=time, 5=bool, 6=bin, 7=hex)
