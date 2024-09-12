;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               S y m C a l c                                @
;@                               (config area)                                @
;@                                                                            @
;@             (c) 2024-2024 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- CONFIG -------------------------------------------------------------------

;---


;==============================================================================
;### PROGRAM CONFIG ###########################################################
;==============================================================================

cfgprgbeg

;cell size
cfgcelxln   db 50   ;column width
cfgcelyln   db 10   ;row height

;view
cfgviwtol   db 1    ;tool bar
cfgviwsta   db 1    ;status bar

;default colours
cfgapppen   db 1    ;default pen   \
cfgapppap   db 8    ;field paper   |
cfgappgrd   db 2    ;field grid    /

;default number formats
cfgfmtflt   db 0,0*32+7+8   ;float
cfgfmtdat   db 0,3*32+0     ;date
cfgfmttim   db 0,4*32+0     ;time
cfgfmtbin   db 0,6*32+7+8   ;binary
cfgfmthex   db 0,7*32+7+8   ;hexadecimal

;default cell formats
cfgfmtnum   db 16*1+8,0
cfgfmttxt   db 16*1+8,0

cfgprgend


;==============================================================================
;### ACTUAL SHEET CONFIG ######################################################
;==============================================================================

;default colours of actual sheet
cfgcolpen   db 1    ;default pen   \
cfgcolpap   db 8    ;field paper   |
cfgcolgrd   db 2    ;field grid    /


;==============================================================================
;### DOCUMENT CONFIG ##########################################################
;==============================================================================

cfgdocbeg

;comma/1000er dot setting
cfgnumcom   db ","
cfgnumpoi   db "."
cfgnumbin   db " "

;unit - percentage
cfguniper
db 1,"%":ds 6

;units - numbers
cfguninum
db 0       :ds 7
db 1,"$"   :ds 6
db 1," EUR":ds 3
db 0,"EUR ":ds 3
db 1," l"  :ds 5
db 1," cm" :ds 4
db 1," km" :ds 4
db 1," ly" :ds 4

;units - binary/hexadecimal
cfgunibin
db 0       :ds 7
db 0,"#"   :ds 6
db 0,"$"   :ds 6
db 0,"%"   :ds 6
db 0,"0x"  :ds 5
db 0,"0b"  :ds 5
db 1,"H"   :ds 6
db 1,"B"   :ds 6

;boolean expressions
cfgunibol
db "TRUE"   :ds 4:db "FALSE"  :ds 3
db "On"     :ds 6:db "Off"    :ds 5
db "Yes"    :ds 5:db "No"     :ds 6
db "1"      :ds 7:db "0"      :ds 7
db "Up"     :ds 6:db "Down"   :ds 4
db "High"   :ds 4:db "Low"    :ds 5
db "Ok"     :ds 6:db "Error"  :ds 3
db "Run"    :ds 5:db "Stop"   :ds 4

;time format masks
cfgmsktim
db "H:m:s"          :ds 16-05
db "H:m"            :ds 16-03
db "h:m a"          :ds 16-05
db "h:m:s a"        :ds 16-07
db "D.M.Y H:m"      :ds 16-09
db "D.M.Y H:m:s"    :ds 16-11
db "D.M.Y h:m a"    :ds 16-11
db "Y-M-D H:m:s"    :ds 16-11

;date format masks
cfgmskdat
db "D.M.Y"          :ds 16-5
db "D.n y"          :ds 16-5
db "D.N Y"          :ds 16-5
db "W, D.n"         :ds 16-6
db "w, D.n Y"       :ds 16-8
db "w, D.N"         :ds 16-6
db "w, D.N Y"       :ds 16-8
db "Y-M-D"          :ds 16-5

cfgdocend
