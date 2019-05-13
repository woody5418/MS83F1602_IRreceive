opt subtitle "HI-TECH Software Omniscient Code Generator (Lite mode) build 10920"

opt pagewidth 120

	opt lm

	processor	16F685
clrc	macro
	bcf	3,0
	endm
clrz	macro
	bcf	3,2
	endm
setc	macro
	bsf	3,0
	endm
setz	macro
	bsf	3,2
	endm
skipc	macro
	btfss	3,0
	endm
skipz	macro
	btfss	3,2
	endm
skipnc	macro
	btfsc	3,0
	endm
skipnz	macro
	btfsc	3,2
	endm
indf	equ	0
indf0	equ	0
pc	equ	2
pcl	equ	2
status	equ	3
fsr	equ	4
fsr0	equ	4
c	equ	1
z	equ	0
pclath	equ	10
	FNCALL	_main,_System_init
	FNCALL	_main,_GPIO_Init
	FNCALL	_main,_Timer1_Init
	FNCALL	_main,_LED_Study_End
	FNCALL	_main,_Read_EEPROM_u8
	FNCALL	_main,_Control
	FNCALL	_Control,_isKeyPressed
	FNCALL	_Control,_Delay_xms
	FNCALL	_Control,_VS1838_Process
	FNCALL	_Control,_Write_EEPROM_u8
	FNCALL	_Control,_isSwitchOn
	FNCALL	_Write_EEPROM_u8,_Delay_xms
	FNCALL	_LED_Study_End,_Delay_xms
	FNROOT	_main
	FNCALL	_ISR,_GetCCP1Value
	FNCALL	_ISR,_VS1838ReceiveHandle
	FNCALL	_ISR,_Clean_countReg
	FNCALL	intlevel1,_ISR
	global	intlevel1
	FNROOT	intlevel1
	global	_ADCIN4
	global	_Cycle_CCPR
	global	_StudyVS1838DATA
	global	_VS1838_Data
	global	_VS1838_Receive_Count
	global	_VS1838_Status
	global	_VS1838_receive_ok
	global	_flag_led_put
	global	_vs_times
	global	_VS838_Receive_Data
	global	_PSTRCON
psect	text436,local,class=CODE,delta=2
global __ptext436
__ptext436:
_PSTRCON	set	413
	DABS	1,413,1	;_PSTRCON

	global	_SRCON
_SRCON	set	414
	DABS	1,414,1	;_SRCON

	global	_C1SEN
_C1SEN	set	3317
	DABS	1,414,1	;_C1SEN

	global	_C2REN
_C2REN	set	3316
	DABS	1,414,1	;_C2REN

	global	_EEPGD
_EEPGD	set	3175
	DABS	1,396,1	;_EEPGD

	global	_PULSR
_PULSR	set	3314
	DABS	1,414,1	;_PULSR

	global	_PULSS
_PULSS	set	3315
	DABS	1,414,1	;_PULSS

	global	_SR0
_SR0	set	3318
	DABS	1,414,1	;_SR0

	global	_SR1
_SR1	set	3319
	DABS	1,414,1	;_SR1

	global	_STRA
_STRA	set	3304
	DABS	1,413,1	;_STRA

	global	_STRB
_STRB	set	3305
	DABS	1,413,1	;_STRB

	global	_STRC
_STRC	set	3306
	DABS	1,413,1	;_STRC

	global	_STRD
_STRD	set	3307
	DABS	1,413,1	;_STRD

	global	_STRSYNC
_STRSYNC	set	3308
	DABS	1,413,1	;_STRSYNC

	global	_WREN
_WREN	set	3170
	DABS	1,396,1	;_WREN

	global	_ADCON0
_ADCON0	set	31
	global	_ADRESH
_ADRESH	set	30
	global	_CCP1CON
_CCP1CON	set	21
	global	_CCPR1H
_CCPR1H	set	20
	global	_CCPR1L
_CCPR1L	set	19
	global	_TMR1H
_TMR1H	set	15
	global	_TMR1L
_TMR1L	set	14
	global	_TMR2
_TMR2	set	17
	global	_WDTCON
_WDTCON	set	24
	global	_ADFM
_ADFM	set	255
	global	_ADON
_ADON	set	248
	global	_CARRY
_CARRY	set	24
	global	_CCP1IF
_CCP1IF	set	104
	global	_CHS0
_CHS0	set	250
	global	_CHS1
_CHS1	set	251
	global	_CHS2
_CHS2	set	252
	global	_CM0
_CM0	set	200
	global	_CM1
_CM1	set	201
	global	_CM2
_CM2	set	202
	global	_GIE
_GIE	set	95
	global	_GO_DONE
_GO_DONE	set	249
	global	_PA5
_PA5	set	45
	global	_PC0
_PC0	set	56
	global	_PC1
_PC1	set	57
	global	_PC3
_PC3	set	59
	global	_PC4
_PC4	set	60
	global	_PEIE
_PEIE	set	94
	global	_T1CKPS0
_T1CKPS0	set	132
	global	_T1CKPS1
_T1CKPS1	set	133
	global	_T1OSCEN
_T1OSCEN	set	131
	global	_T2CKPS0
_T2CKPS0	set	144
	global	_T2CKPS1
_T2CKPS1	set	145
	global	_TMR1CS
_TMR1CS	set	129
	global	_TMR1ON
_TMR1ON	set	128
	global	_TMR2IF
_TMR2IF	set	97
	global	_TMR2ON
_TMR2ON	set	146
	global	_TOUTPS0
_TOUTPS0	set	147
	global	_TOUTPS1
_TOUTPS1	set	148
	global	_TOUTPS2
_TOUTPS2	set	149
	global	_TOUTPS3
_TOUTPS3	set	150
	global	_VCFG0
_VCFG0	set	253
	global	_ADRESL
_ADRESL	set	158
	global	_EEADR
_EEADR	set	155
	global	_EECON1
_EECON1	set	156
	global	_EECON2
_EECON2	set	157
	global	_EEDAT
_EEDAT	set	154
	global	_EEDATA
_EEDATA	set	154
	global	_OPTION
_OPTION	set	129
	global	_OSCCON
_OSCCON	set	143
	global	_PR2
_PR2	set	146
	global	_ADCS0
_ADCS0	set	1276
	global	_ADCS1
_ADCS1	set	1277
	global	_ADCS2
_ADCS2	set	1278
	global	_ANSEL0
_ANSEL0	set	1160
	global	_ANSEL1
_ANSEL1	set	1161
	global	_ANSEL2
_ANSEL2	set	1162
	global	_ANSEL3
_ANSEL3	set	1163
	global	_ANSEL4
_ANSEL4	set	1164
	global	_ANSEL5
_ANSEL5	set	1165
	global	_ANSEL6
_ANSEL6	set	1166
	global	_ANSEL7
_ANSEL7	set	1167
	global	_CCP1IE
_CCP1IE	set	1128
	global	_DIVS
_DIVS	set	1279
	global	_RD
_RD	set	1248
	global	_TMR2IE
_TMR2IE	set	1121
	global	_TRISA0
_TRISA0	set	1064
	global	_TRISA1
_TRISA1	set	1065
	global	_TRISA2
_TRISA2	set	1066
	global	_TRISA3
_TRISA3	set	1067
	global	_TRISA4
_TRISA4	set	1068
	global	_TRISA5
_TRISA5	set	1069
	global	_TRISA6
_TRISA6	set	1070
	global	_TRISA7
_TRISA7	set	1071
	global	_TRISC0
_TRISC0	set	1080
	global	_TRISC1
_TRISC1	set	1081
	global	_TRISC2
_TRISC2	set	1082
	global	_TRISC3
_TRISC3	set	1083
	global	_TRISC4
_TRISC4	set	1084
	global	_TRISC5
_TRISC5	set	1085
	global	_WPDA4
_WPDA4	set	1100
	global	_WPDC1
_WPDC1	set	1099
	global	_WPDC2
_WPDC2	set	1098
	global	_WPDC3
_WPDC3	set	1097
	global	_WPUA0
_WPUA0	set	1192
	global	_WPUA1
_WPUA1	set	1193
	global	_WPUA2
_WPUA2	set	1194
	global	_WPUA3
_WPUA3	set	1195
	global	_WPUA4
_WPUA4	set	1196
	global	_WPUA5
_WPUA5	set	1197
	global	_WPUA6
_WPUA6	set	1198
	global	_WPUA7
_WPUA7	set	1199
	global	_WPUC0
_WPUC0	set	1088
	global	_WPUC1
_WPUC1	set	1089
	global	_WPUC2
_WPUC2	set	1090
	global	_WPUC3
_WPUC3	set	1091
	global	_WPUC4
_WPUC4	set	1092
	global	_WPUC5
_WPUC5	set	1093
	global	_WR
_WR	set	1256
	global	_WREN1
_WREN1	set	1250
	global	_WREN2
_WREN2	set	1252
	global	_WREN3
_WREN3	set	1253
	file	"ms83f1602_irreceive.as"
	line	#
psect cinit,class=CODE,delta=2
global start_initialization
start_initialization:

psect	bssCOMMON,class=COMMON,space=1
global __pbssCOMMON
__pbssCOMMON:
_VS838_Receive_Data:
       ds      4

psect	bssBANK0,class=BANK0,space=1
global __pbssBANK0
__pbssBANK0:
_ADCIN4:
       ds      2

_Cycle_CCPR:
       ds      2

_StudyVS1838DATA:
       ds      1

_VS1838_Data:
       ds      1

_VS1838_Receive_Count:
       ds      1

_VS1838_Status:
       ds      1

_VS1838_receive_ok:
       ds      1

_flag_led_put:
       ds      1

_vs_times:
       ds      1

psect clrtext,class=CODE,delta=2
global clear_ram
;	Called with FSR containing the base address, and
;	W with the last address+1
clear_ram:
	clrwdt			;clear the watchdog before getting into this loop
clrloop:
	clrf	indf		;clear RAM location pointed to by FSR
	incf	fsr,f		;increment pointer
	xorwf	fsr,w		;XOR with final address
	btfsc	status,2	;have we reached the end yet?
	retlw	0		;all done for this memory range, return
	xorwf	fsr,w		;XOR again to restore value
	goto	clrloop		;do the next byte

; Clear objects allocated to COMMON
psect cinit,class=CODE,delta=2
	movlw	low(__pbssCOMMON)
	movwf	fsr
	movlw	low((__pbssCOMMON)+04h)
	fcall	clear_ram
; Clear objects allocated to BANK0
psect cinit,class=CODE,delta=2
	bcf	status, 7	;select IRP bank0
	movlw	low(__pbssBANK0)
	movwf	fsr
	movlw	low((__pbssBANK0)+0Bh)
	fcall	clear_ram
psect cinit,class=CODE,delta=2
global end_of_initialization

;End of C runtime variable initialization code

end_of_initialization:
clrf status
ljmp _main	;jump to C main() function
psect	cstackCOMMON,class=COMMON,space=1
global __pcstackCOMMON
__pcstackCOMMON:
	global	?_System_init
?_System_init:	; 0 bytes @ 0x0
	global	?_GPIO_Init
?_GPIO_Init:	; 0 bytes @ 0x0
	global	?_Timer1_Init
?_Timer1_Init:	; 0 bytes @ 0x0
	global	?_LED_Study_End
?_LED_Study_End:	; 0 bytes @ 0x0
	global	?_Control
?_Control:	; 0 bytes @ 0x0
	global	?_VS1838ReceiveHandle
?_VS1838ReceiveHandle:	; 0 bytes @ 0x0
	global	?_Clean_countReg
?_Clean_countReg:	; 0 bytes @ 0x0
	global	??_Clean_countReg
??_Clean_countReg:	; 0 bytes @ 0x0
	global	?_main
?_main:	; 0 bytes @ 0x0
	global	?_ISR
?_ISR:	; 0 bytes @ 0x0
	global	?_Read_EEPROM_u8
?_Read_EEPROM_u8:	; 1 bytes @ 0x0
	global	?_isKeyPressed
?_isKeyPressed:	; 1 bytes @ 0x0
	global	?_isSwitchOn
?_isSwitchOn:	; 1 bytes @ 0x0
	global	?_VS1838_Process
?_VS1838_Process:	; 1 bytes @ 0x0
	global	?_GetCCP1Value
?_GetCCP1Value:	; 2 bytes @ 0x0
	global	VS1838ReceiveHandle@capdata
VS1838ReceiveHandle@capdata:	; 2 bytes @ 0x0
	ds	2
	global	??_GetCCP1Value
??_GetCCP1Value:	; 0 bytes @ 0x2
	global	??_VS1838ReceiveHandle
??_VS1838ReceiveHandle:	; 0 bytes @ 0x2
	ds	2
	global	GetCCP1Value@values
GetCCP1Value@values:	; 2 bytes @ 0x4
	ds	2
	global	??_ISR
??_ISR:	; 0 bytes @ 0x6
	ds	4
psect	cstackBANK0,class=BANK0,space=1
global __pcstackBANK0
__pcstackBANK0:
	global	??_System_init
??_System_init:	; 0 bytes @ 0x0
	global	??_GPIO_Init
??_GPIO_Init:	; 0 bytes @ 0x0
	global	??_Timer1_Init
??_Timer1_Init:	; 0 bytes @ 0x0
	global	??_Read_EEPROM_u8
??_Read_EEPROM_u8:	; 0 bytes @ 0x0
	global	?_Delay_xms
?_Delay_xms:	; 0 bytes @ 0x0
	global	??_isKeyPressed
??_isKeyPressed:	; 0 bytes @ 0x0
	global	??_isSwitchOn
??_isSwitchOn:	; 0 bytes @ 0x0
	global	??_VS1838_Process
??_VS1838_Process:	; 0 bytes @ 0x0
	global	Delay_xms@x
Delay_xms@x:	; 2 bytes @ 0x0
	ds	1
	global	Read_EEPROM_u8@EEAddress
Read_EEPROM_u8@EEAddress:	; 1 bytes @ 0x1
	ds	1
	global	??_Delay_xms
??_Delay_xms:	; 0 bytes @ 0x2
	global	Read_EEPROM_u8@EepromData
Read_EEPROM_u8@EepromData:	; 1 bytes @ 0x2
	ds	2
	global	Delay_xms@i
Delay_xms@i:	; 2 bytes @ 0x4
	ds	1
	global	VS1838_Process@Address_H
VS1838_Process@Address_H:	; 1 bytes @ 0x5
	ds	1
	global	??_LED_Study_End
??_LED_Study_End:	; 0 bytes @ 0x6
	global	?_Write_EEPROM_u8
?_Write_EEPROM_u8:	; 0 bytes @ 0x6
	global	VS1838_Process@Address_L
VS1838_Process@Address_L:	; 1 bytes @ 0x6
	global	Write_EEPROM_u8@EEDatas
Write_EEPROM_u8@EEDatas:	; 1 bytes @ 0x6
	ds	1
	global	??_Write_EEPROM_u8
??_Write_EEPROM_u8:	; 0 bytes @ 0x7
	global	VS1838_Process@Data_L
VS1838_Process@Data_L:	; 1 bytes @ 0x7
	global	Write_EEPROM_u8@EEAddress
Write_EEPROM_u8@EEAddress:	; 1 bytes @ 0x7
	ds	1
	global	VS1838_Process@Data_H
VS1838_Process@Data_H:	; 1 bytes @ 0x8
	ds	1
	global	??_Control
??_Control:	; 0 bytes @ 0x9
	ds	1
	global	??_main
??_main:	; 0 bytes @ 0xA
	ds	1
;;Data sizes: Strings 0, constant 0, data 0, bss 15, persistent 0 stack 0
;;Auto spaces:   Size  Autos    Used
;; COMMON          14     10      14
;; BANK0           80     11      22
;; BANK1           32      0       0

;;
;; Pointer list with targets:

;; ?_GetCCP1Value	unsigned int  size(1) Largest target is 0
;;


;;
;; Critical Paths under _main in COMMON
;;
;;   None.
;;
;; Critical Paths under _ISR in COMMON
;;
;;   _ISR->_GetCCP1Value
;;   _ISR->_VS1838ReceiveHandle
;;
;; Critical Paths under _main in BANK0
;;
;;   _main->_Control
;;   _Control->_VS1838_Process
;;   _Write_EEPROM_u8->_Delay_xms
;;   _LED_Study_End->_Delay_xms
;;
;; Critical Paths under _ISR in BANK0
;;
;;   None.
;;
;; Critical Paths under _main in BANK1
;;
;;   None.
;;
;; Critical Paths under _ISR in BANK1
;;
;;   None.

;;
;;Main: autosize = 0, tempsize = 1, incstack = 0, save=0
;;

;;
;;Call Graph Tables:
;;
;; ---------------------------------------------------------------------------------
;; (Depth) Function   	        Calls       Base Space   Used Autos Params    Refs
;; ---------------------------------------------------------------------------------
;; (0) _main                                                 1     1      0     342
;;                                             10 BANK0      1     1      0
;;                        _System_init
;;                          _GPIO_Init
;;                        _Timer1_Init
;;                      _LED_Study_End
;;                     _Read_EEPROM_u8
;;                            _Control
;; ---------------------------------------------------------------------------------
;; (1) _Control                                              1     1      0     250
;;                                              9 BANK0      1     1      0
;;                       _isKeyPressed
;;                          _Delay_xms
;;                     _VS1838_Process
;;                    _Write_EEPROM_u8
;;                         _isSwitchOn
;; ---------------------------------------------------------------------------------
;; (2) _Write_EEPROM_u8                                      2     1      1      90
;;                                              6 BANK0      2     1      1
;;                          _Delay_xms
;; ---------------------------------------------------------------------------------
;; (1) _LED_Study_End                                        0     0      0      46
;;                          _Delay_xms
;; ---------------------------------------------------------------------------------
;; (3) _Delay_xms                                            6     4      2      46
;;                                              0 BANK0      6     4      2
;; ---------------------------------------------------------------------------------
;; (1) _Read_EEPROM_u8                                       3     3      0      46
;;                                              0 BANK0      3     3      0
;; ---------------------------------------------------------------------------------
;; (2) _isSwitchOn                                           0     0      0       0
;; ---------------------------------------------------------------------------------
;; (2) _isKeyPressed                                         0     0      0       0
;; ---------------------------------------------------------------------------------
;; (2) _VS1838_Process                                       9     9      0     114
;;                                              0 BANK0      9     9      0
;; ---------------------------------------------------------------------------------
;; (1) _Timer1_Init                                          0     0      0       0
;; ---------------------------------------------------------------------------------
;; (1) _GPIO_Init                                            0     0      0       0
;; ---------------------------------------------------------------------------------
;; (1) _System_init                                          0     0      0       0
;; ---------------------------------------------------------------------------------
;; Estimated maximum stack depth 3
;; ---------------------------------------------------------------------------------
;; (Depth) Function   	        Calls       Base Space   Used Autos Params    Refs
;; ---------------------------------------------------------------------------------
;; (4) _ISR                                                  4     4      0     202
;;                                              6 COMMON     4     4      0
;;                       _GetCCP1Value
;;                _VS1838ReceiveHandle
;;                     _Clean_countReg
;; ---------------------------------------------------------------------------------
;; (5) _Clean_countReg                                       0     0      0       0
;; ---------------------------------------------------------------------------------
;; (5) _VS1838ReceiveHandle                                  6     4      2     132
;;                                              0 COMMON     6     4      2
;; ---------------------------------------------------------------------------------
;; (5) _GetCCP1Value                                         6     4      2      70
;;                                              0 COMMON     6     4      2
;; ---------------------------------------------------------------------------------
;; Estimated maximum stack depth 5
;; ---------------------------------------------------------------------------------

;; Call Graph Graphs:

;; _main (ROOT)
;;   _System_init
;;   _GPIO_Init
;;   _Timer1_Init
;;   _LED_Study_End
;;     _Delay_xms
;;   _Read_EEPROM_u8
;;   _Control
;;     _isKeyPressed
;;     _Delay_xms
;;     _VS1838_Process
;;     _Write_EEPROM_u8
;;       _Delay_xms
;;     _isSwitchOn
;;
;; _ISR (ROOT)
;;   _GetCCP1Value
;;   _VS1838ReceiveHandle
;;   _Clean_countReg
;;

;; Address spaces:

;;Name               Size   Autos  Total    Cost      Usage
;;BITCOMMON            E      0       0       0        0.0%
;;EEDATA             100      0       0       0        0.0%
;;NULL                 0      0       0       0        0.0%
;;CODE                 0      0       0       0        0.0%
;;COMMON               E      A       E       1      100.0%
;;BITSFR0              0      0       0       1        0.0%
;;SFR0                 0      0       0       1        0.0%
;;BITSFR1              0      0       0       2        0.0%
;;SFR1                 0      0       0       2        0.0%
;;STACK                0      0       7       2        0.0%
;;BITBANK0            50      0       0       3        0.0%
;;BANK0               50      B      16       4       27.5%
;;BITBANK1            20      0       0       5        0.0%
;;BITSFR2              0      0       0       5        0.0%
;;SFR2                 0      0       0       5        0.0%
;;BANK1               20      0       0       6        0.0%
;;ABS                  0      0      24       7        0.0%
;;DATA                 0      0      2B       8        0.0%

	global	_main
psect	maintext,global,class=CODE,delta=2
global __pmaintext
__pmaintext:

;; *************** function _main *****************
;; Defined at:
;;		line 88 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2, status,0, pclath, cstack
;; Tracked objects:
;;		On entry : 17F/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       1       0
;;      Totals:         0       1       0
;;Total ram usage:        1 bytes
;; Hardware stack levels required when called:    5
;; This function calls:
;;		_System_init
;;		_GPIO_Init
;;		_Timer1_Init
;;		_LED_Study_End
;;		_Read_EEPROM_u8
;;		_Control
;; This function is called by:
;;		Startup code after reset
;; This function uses a non-reentrant model
;;
psect	maintext
	file	"main.c"
	line	88
	global	__size_of_main
	__size_of_main	equ	__end_of_main-_main
	
_main:	
	opt	stack 3
; Regs used in _main: [wreg+status,2+status,0+pclath+cstack]
	line	89
	
l3663:	
# 89 "main.c"
clrwdt ;#
psect	maintext
	line	90
	
l3665:	
;main.c: 90: System_init();
	fcall	_System_init
	line	91
	
l3667:	
;main.c: 91: GPIO_Init();
	fcall	_GPIO_Init
	line	92
	
l3669:	
;main.c: 92: Timer1_Init();
	fcall	_Timer1_Init
	line	95
	
l3671:	
;main.c: 95: PC4=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(60/8),(60)&7
	line	96
	
l3673:	
;main.c: 96: LED_Study_End();
	fcall	_LED_Study_End
	line	97
	
l3675:	
;main.c: 97: StudyVS1838DATA = Read_EEPROM_u8(0x00);
	movlw	(0)
	fcall	_Read_EEPROM_u8
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	(??_main+0)+0
	movf	(??_main+0)+0,w
	movwf	(_StudyVS1838DATA)
	line	98
	
l3677:	
;main.c: 98: if( StudyVS1838DATA == 0XFF)
	movf	(_StudyVS1838DATA),w
	xorlw	0FFh
	skipz
	goto	u2801
	goto	u2800
u2801:
	goto	l3681
u2800:
	line	99
	
l3679:	
;main.c: 99: StudyVS1838DATA = 0x90;
	movlw	(090h)
	movwf	(??_main+0)+0
	movf	(??_main+0)+0,w
	movwf	(_StudyVS1838DATA)
	goto	l3681
	
l1115:	
	goto	l3681
	line	100
;main.c: 100: while(1) {
	
l1116:	
	line	101
	
l3681:	
# 101 "main.c"
clrwdt ;#
psect	maintext
	line	102
	
l3683:	
;main.c: 102: Control();
	fcall	_Control
	goto	l3681
	line	103
	
l1117:	
	line	100
	goto	l3681
	
l1118:	
	line	104
	
l1119:	
	global	start
	ljmp	start
	opt stack 0
GLOBAL	__end_of_main
	__end_of_main:
;; =============== function _main ends ============

	signat	_main,88
	global	_Control
psect	text437,local,class=CODE,delta=2
global __ptext437
__ptext437:

;; *************** function _Control *****************
;; Defined at:
;;		line 385 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2, status,0, pclath, cstack
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       1       0
;;      Totals:         0       1       0
;;Total ram usage:        1 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    4
;; This function calls:
;;		_isKeyPressed
;;		_Delay_xms
;;		_VS1838_Process
;;		_Write_EEPROM_u8
;;		_isSwitchOn
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text437
	file	"main.c"
	line	385
	global	__size_of_Control
	__size_of_Control	equ	__end_of_Control-_Control
	
_Control:	
	opt	stack 3
; Regs used in _Control: [wreg+status,2+status,0+pclath+cstack]
	line	386
	
l3615:	
;main.c: 386: if(isKeyPressed()) {
	fcall	_isKeyPressed
	xorlw	0
	skipnz
	goto	u2711
	goto	u2710
u2711:
	goto	l1174
u2710:
	line	387
	
l3617:	
;main.c: 387: PC4=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(60/8),(60)&7
	line	388
;main.c: 388: while(!VS1838_Process()) {
	goto	l1175
	
l1176:	
	line	389
;main.c: 389: PC3=1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(59/8),(59)&7
	line	390
	
l3619:	
;main.c: 390: Delay_xms(1);
	movlw	low(01h)
	movwf	(?_Delay_xms)
	movlw	high(01h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	394
	
l1175:	
	line	388
	fcall	_VS1838_Process
	xorlw	0
	skipnz
	goto	u2721
	goto	u2720
u2721:
	goto	l1176
u2720:
	goto	l3621
	
l1177:	
	line	395
	
l3621:	
;main.c: 394: }
;main.c: 395: Write_EEPROM_u8(0x00,VS1838_Data);
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(_VS1838_Data),w
	movwf	(??_Control+0)+0
	movf	(??_Control+0)+0,w
	movwf	(?_Write_EEPROM_u8)
	movlw	(0)
	fcall	_Write_EEPROM_u8
	line	396
	
l3623:	
;main.c: 396: StudyVS1838DATA = VS1838_Data;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(_VS1838_Data),w
	movwf	(??_Control+0)+0
	movf	(??_Control+0)+0,w
	movwf	(_StudyVS1838DATA)
	line	397
	
l3625:	
;main.c: 397: VS1838_Data=0;
	clrf	(_VS1838_Data)
	line	398
	
l3627:	
;main.c: 398: PC3=0;
	bcf	(59/8),(59)&7
	line	399
	
l3629:	
;main.c: 399: PC4=0;
	bcf	(60/8),(60)&7
	line	400
;main.c: 400: } else {
	goto	l1192
	
l1174:	
	line	403
;main.c: 403: if(PC0 == 0) {
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	btfsc	(56/8),(56)&7
	goto	u2731
	goto	u2730
u2731:
	goto	l3635
u2730:
	line	404
	
l3631:	
;main.c: 404: VS838_Receive_Data=0;
	movlw	0
	movwf	(_VS838_Receive_Data+3)
	movlw	0
	movwf	(_VS838_Receive_Data+2)
	movlw	0
	movwf	(_VS838_Receive_Data+1)
	movlw	0
	movwf	(_VS838_Receive_Data)

	line	405
	
l3633:	
;main.c: 405: PC4=0;
	bcf	(60/8),(60)&7
	line	406
;main.c: 406: } else {
	goto	l1192
	
l1179:	
	line	407
	
l3635:	
;main.c: 407: VS1838_Process();
	fcall	_VS1838_Process
	line	408
;main.c: 408: if(isSwitchOn()) {
	fcall	_isSwitchOn
	xorlw	0
	skipnz
	goto	u2741
	goto	u2740
u2741:
	goto	l3647
u2740:
	line	409
	
l3637:	
;main.c: 409: if(VS1838_Data == StudyVS1838DATA) {
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(_VS1838_Data),w
	xorwf	(_StudyVS1838DATA),w
	skipz
	goto	u2751
	goto	u2750
u2751:
	goto	l1192
u2750:
	line	410
	
l3639:	
;main.c: 410: if(flag_led_put == 0) {
	movf	(_flag_led_put),f
	skipz
	goto	u2761
	goto	u2760
u2761:
	goto	l1183
u2760:
	line	411
	
l3641:	
;main.c: 411: VS1838_Data=0;
	clrf	(_VS1838_Data)
	line	412
	
l3643:	
;main.c: 412: flag_led_put=1;
	clrf	(_flag_led_put)
	bsf	status,0
	rlf	(_flag_led_put),f
	line	413
	
l3645:	
;main.c: 413: PC4=1;
	bsf	(60/8),(60)&7
	line	414
;main.c: 414: } else {
	goto	l1192
	
l1183:	
	line	415
;main.c: 415: VS1838_Data=0;
	clrf	(_VS1838_Data)
	line	416
;main.c: 416: flag_led_put=0;
	clrf	(_flag_led_put)
	line	417
;main.c: 417: PC4=0;
	bcf	(60/8),(60)&7
	goto	l1192
	line	418
	
l1184:	
	goto	l1192
	line	419
	
l1182:	
	line	420
;main.c: 418: }
;main.c: 419: }
;main.c: 420: } else {
	goto	l1192
	
l1181:	
	line	421
	
l3647:	
;main.c: 421: if(VS1838_Data == StudyVS1838DATA) {
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(_VS1838_Data),w
	xorwf	(_StudyVS1838DATA),w
	skipz
	goto	u2771
	goto	u2770
u2771:
	goto	l1192
u2770:
	line	422
	
l3649:	
;main.c: 422: PC4=1;
	bsf	(60/8),(60)&7
	line	423
;main.c: 423: vs_times=0;
	clrf	(_vs_times)
	goto	l3651
	line	424
;main.c: 424: while(1) {
	
l1187:	
	line	425
	
l3651:	
;main.c: 425: if((vs_times==255)&&(VS1838_Data == StudyVS1838DATA))
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(_vs_times),w
	xorlw	0FFh
	skipz
	goto	u2781
	goto	u2780
u2781:
	goto	l3659
u2780:
	
l3653:	
	movf	(_VS1838_Data),w
	xorwf	(_StudyVS1838DATA),w
	skipz
	goto	u2791
	goto	u2790
u2791:
	goto	l3659
u2790:
	goto	l1189
	line	426
	
l3655:	
;main.c: 426: break;
	goto	l1189
	
l3657:	
	goto	l3661
	line	427
	
l1188:	
	line	428
	
l3659:	
;main.c: 427: else
;main.c: 428: vs_times++;
	movlw	(01h)
	movwf	(??_Control+0)+0
	movf	(??_Control+0)+0,w
	addwf	(_vs_times),f
	goto	l3661
	
l1190:	
	line	429
	
l3661:	
;main.c: 429: Delay_xms(1);
	movlw	low(01h)
	movwf	(?_Delay_xms)
	movlw	high(01h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	goto	l3651
	line	430
	
l1191:	
	line	424
	goto	l3651
	
l1189:	
	line	431
;main.c: 430: }
;main.c: 431: VS1838_Data=0;
	clrf	(_VS1838_Data)
	line	432
;main.c: 432: PC4=0;
	bcf	(60/8),(60)&7
	goto	l1192
	line	433
	
l1186:	
	goto	l1192
	line	434
	
l1185:	
	goto	l1192
	line	435
	
l1180:	
	goto	l1192
	line	436
	
l1178:	
	line	437
	
l1192:	
	return
	opt stack 0
GLOBAL	__end_of_Control
	__end_of_Control:
;; =============== function _Control ends ============

	signat	_Control,88
	global	_Write_EEPROM_u8
psect	text438,local,class=CODE,delta=2
global __ptext438
__ptext438:

;; *************** function _Write_EEPROM_u8 *****************
;; Defined at:
;;		line 502 in file "main.c"
;; Parameters:    Size  Location     Type
;;  EEAddress       1    wreg     unsigned char 
;;  EEDatas         1    6[BANK0 ] unsigned char 
;; Auto vars:     Size  Location     Type
;;  EEAddress       1    7[BANK0 ] unsigned char 
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2, status,0, pclath, cstack
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       1       0
;;      Locals:         0       1       0
;;      Temps:          0       0       0
;;      Totals:         0       2       0
;;Total ram usage:        2 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    3
;; This function calls:
;;		_Delay_xms
;; This function is called by:
;;		_Control
;;		_Write_EEPROM_u16
;; This function uses a non-reentrant model
;;
psect	text438
	file	"main.c"
	line	502
	global	__size_of_Write_EEPROM_u8
	__size_of_Write_EEPROM_u8	equ	__end_of_Write_EEPROM_u8-_Write_EEPROM_u8
	
_Write_EEPROM_u8:	
	opt	stack 3
; Regs used in _Write_EEPROM_u8: [wreg+status,2+status,0+pclath+cstack]
;Write_EEPROM_u8@EEAddress stored from wreg
	line	504
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	(Write_EEPROM_u8@EEAddress)
	
l3597:	
;main.c: 504: GIE=0;
	bcf	(95/8),(95)&7
	line	505
;main.c: 505: if(GIE==0&&WR==0) {
	btfsc	(95/8),(95)&7
	goto	u2691
	goto	u2690
u2691:
	goto	l1211
u2690:
	
l3599:	
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	btfsc	(1256/8)^080h,(1256)&7
	goto	u2701
	goto	u2700
u2701:
	goto	l1211
u2700:
	line	506
	
l3601:	
;main.c: 506: EEADR = EEAddress;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(Write_EEPROM_u8@EEAddress),w
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	movwf	(155)^080h	;volatile
	line	507
;main.c: 507: EEDAT = EEDatas;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(Write_EEPROM_u8@EEDatas),w
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	movwf	(154)^080h	;volatile
	line	508
	
l3603:	
;main.c: 508: WREN3=1;
	bsf	(1253/8)^080h,(1253)&7
	line	509
	
l3605:	
;main.c: 509: WREN2=1;
	bsf	(1252/8)^080h,(1252)&7
	line	510
	
l3607:	
;main.c: 510: WREN1=1;
	bsf	(1250/8)^080h,(1250)&7
	line	511
	
l3609:	
;main.c: 511: WR=1;
	bsf	(1256/8)^080h,(1256)&7
	line	512
	
l3611:	
;main.c: 512: GIE=1;
	bsf	(95/8),(95)&7
	line	513
	
l3613:	
;main.c: 513: Delay_xms(2);
	movlw	low(02h)
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	(?_Delay_xms)
	movlw	high(02h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	goto	l1211
	line	514
	
l1210:	
	line	515
	
l1211:	
	return
	opt stack 0
GLOBAL	__end_of_Write_EEPROM_u8
	__end_of_Write_EEPROM_u8:
;; =============== function _Write_EEPROM_u8 ends ============

	signat	_Write_EEPROM_u8,8312
	global	_LED_Study_End
psect	text439,local,class=CODE,delta=2
global __ptext439
__ptext439:

;; *************** function _LED_Study_End *****************
;; Defined at:
;;		line 462 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2, status,0, pclath, cstack
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    3
;; This function calls:
;;		_Delay_xms
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text439
	file	"main.c"
	line	462
	global	__size_of_LED_Study_End
	__size_of_LED_Study_End	equ	__end_of_LED_Study_End-_LED_Study_End
	
_LED_Study_End:	
	opt	stack 4
; Regs used in _LED_Study_End: [wreg+status,2+status,0+pclath+cstack]
	line	463
	
l3583:	
;main.c: 463: PC3 = 1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(59/8),(59)&7
	line	464
	
l3585:	
;main.c: 464: Delay_xms(100);
	movlw	low(064h)
	movwf	(?_Delay_xms)
	movlw	high(064h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	465
	
l3587:	
;main.c: 465: PC3 = 0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(59/8),(59)&7
	line	466
;main.c: 466: Delay_xms(100);
	movlw	low(064h)
	movwf	(?_Delay_xms)
	movlw	high(064h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	467
	
l3589:	
;main.c: 467: PC3 = 1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(59/8),(59)&7
	line	468
;main.c: 468: Delay_xms(100);
	movlw	low(064h)
	movwf	(?_Delay_xms)
	movlw	high(064h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	469
	
l3591:	
;main.c: 469: PC3 = 0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(59/8),(59)&7
	line	470
;main.c: 470: Delay_xms(100);
	movlw	low(064h)
	movwf	(?_Delay_xms)
	movlw	high(064h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	471
	
l3593:	
;main.c: 471: PC3 = 1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(59/8),(59)&7
	line	472
;main.c: 472: Delay_xms(100);
	movlw	low(064h)
	movwf	(?_Delay_xms)
	movlw	high(064h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	473
	
l3595:	
;main.c: 473: PC3 = 0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(59/8),(59)&7
	line	474
;main.c: 474: Delay_xms(100);
	movlw	low(064h)
	movwf	(?_Delay_xms)
	movlw	high(064h)
	movwf	((?_Delay_xms))+1
	fcall	_Delay_xms
	line	475
	
l1201:	
	return
	opt stack 0
GLOBAL	__end_of_LED_Study_End
	__end_of_LED_Study_End:
;; =============== function _LED_Study_End ends ============

	signat	_LED_Study_End,88
	global	_Delay_xms
psect	text440,local,class=CODE,delta=2
global __ptext440
__ptext440:

;; *************** function _Delay_xms *****************
;; Defined at:
;;		line 535 in file "main.c"
;; Parameters:    Size  Location     Type
;;  x               2    0[BANK0 ] unsigned int 
;; Auto vars:     Size  Location     Type
;;  i               2    4[BANK0 ] unsigned int 
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       2       0
;;      Locals:         0       2       0
;;      Temps:          0       2       0
;;      Totals:         0       6       0
;;Total ram usage:        6 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_Control
;;		_LED_Study_End
;;		_Write_EEPROM_u8
;;		_ADC_Init
;; This function uses a non-reentrant model
;;
psect	text440
	file	"main.c"
	line	535
	global	__size_of_Delay_xms
	__size_of_Delay_xms	equ	__end_of_Delay_xms-_Delay_xms
	
_Delay_xms:	
	opt	stack 3
; Regs used in _Delay_xms: [wreg+status,2]
	line	537
	
l3577:	
;main.c: 536: unsigned int i;
;main.c: 537: for(i=0; i<x; i++) {
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(Delay_xms@i)
	clrf	(Delay_xms@i+1)
	goto	l1217
	
l1218:	
	line	538
	
l3579:	
;main.c: 538: _delay((unsigned long)((1)*(16000000/4000.0)));
	opt asmopt_off
movlw	6
movwf	((??_Delay_xms+0)+0+1),f
	movlw	48
movwf	((??_Delay_xms+0)+0),f
u2817:
	decfsz	((??_Delay_xms+0)+0),f
	goto	u2817
	decfsz	((??_Delay_xms+0)+0+1),f
	goto	u2817
	clrwdt
opt asmopt_on

	line	539
	
l3581:	
# 539 "main.c"
clrwdt ;#
psect	text440
	line	537
	movlw	low(01h)
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	addwf	(Delay_xms@i),f
	skipnc
	incf	(Delay_xms@i+1),f
	movlw	high(01h)
	addwf	(Delay_xms@i+1),f
	
l1217:	
	movf	(Delay_xms@x+1),w
	subwf	(Delay_xms@i+1),w
	skipz
	goto	u2685
	movf	(Delay_xms@x),w
	subwf	(Delay_xms@i),w
u2685:
	skipc
	goto	u2681
	goto	u2680
u2681:
	goto	l3579
u2680:
	goto	l1220
	
l1219:	
	line	541
	
l1220:	
	return
	opt stack 0
GLOBAL	__end_of_Delay_xms
	__end_of_Delay_xms:
;; =============== function _Delay_xms ends ============

	signat	_Delay_xms,4216
	global	_Read_EEPROM_u8
psect	text441,local,class=CODE,delta=2
global __ptext441
__ptext441:

;; *************** function _Read_EEPROM_u8 *****************
;; Defined at:
;;		line 519 in file "main.c"
;; Parameters:    Size  Location     Type
;;  EEAddress       1    wreg     unsigned char 
;; Auto vars:     Size  Location     Type
;;  EEAddress       1    1[BANK0 ] unsigned char 
;;  EepromData      1    2[BANK0 ] unsigned char 
;; Return value:  Size  Location     Type
;;                  1    wreg      unsigned char 
;; Registers used:
;;		wreg
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       2       0
;;      Temps:          0       1       0
;;      Totals:         0       3       0
;;Total ram usage:        3 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_main
;;		_Read_EEPROM_u16
;; This function uses a non-reentrant model
;;
psect	text441
	file	"main.c"
	line	519
	global	__size_of_Read_EEPROM_u8
	__size_of_Read_EEPROM_u8	equ	__end_of_Read_EEPROM_u8-_Read_EEPROM_u8
	
_Read_EEPROM_u8:	
	opt	stack 5
; Regs used in _Read_EEPROM_u8: [wreg]
;Read_EEPROM_u8@EEAddress stored from wreg
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	(Read_EEPROM_u8@EEAddress)
	line	520
	
l3567:	
;main.c: 520: unsigned char EepromData=0;
	clrf	(Read_EEPROM_u8@EepromData)
	line	521
	
l3569:	
;main.c: 521: EEADR = EEAddress;
	movf	(Read_EEPROM_u8@EEAddress),w
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	movwf	(155)^080h	;volatile
	line	522
	
l3571:	
;main.c: 522: RD = 1;
	bsf	(1248/8)^080h,(1248)&7
	line	523
	
l3573:	
;main.c: 523: _nop();
	nop
	line	524
;main.c: 524: EepromData = EEDAT;
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	movf	(154)^080h,w	;volatile
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	(??_Read_EEPROM_u8+0)+0
	movf	(??_Read_EEPROM_u8+0)+0,w
	movwf	(Read_EEPROM_u8@EepromData)
	line	525
;main.c: 525: return EepromData;
	movf	(Read_EEPROM_u8@EepromData),w
	goto	l1214
	
l3575:	
	line	526
	
l1214:	
	return
	opt stack 0
GLOBAL	__end_of_Read_EEPROM_u8
	__end_of_Read_EEPROM_u8:
;; =============== function _Read_EEPROM_u8 ends ============

	signat	_Read_EEPROM_u8,4217
	global	_isSwitchOn
psect	text442,local,class=CODE,delta=2
global __ptext442
__ptext442:

;; *************** function _isSwitchOn *****************
;; Defined at:
;;		line 451 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;                  1    wreg      unsigned char 
;; Registers used:
;;		wreg, status,2, status,0
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_Control
;; This function uses a non-reentrant model
;;
psect	text442
	file	"main.c"
	line	451
	global	__size_of_isSwitchOn
	__size_of_isSwitchOn	equ	__end_of_isSwitchOn-_isSwitchOn
	
_isSwitchOn:	
	opt	stack 4
; Regs used in _isSwitchOn: [wreg+status,2+status,0]
	line	452
	
l3551:	
;main.c: 452: return PA5 ? 0:1;
	clrc
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	btfss	(45/8),(45)&7
	setc
	movlw	0
	skipnc
	movlw	1

	goto	l1198
	
l3553:	
	line	453
	
l1198:	
	return
	opt stack 0
GLOBAL	__end_of_isSwitchOn
	__end_of_isSwitchOn:
;; =============== function _isSwitchOn ends ============

	signat	_isSwitchOn,89
	global	_isKeyPressed
psect	text443,local,class=CODE,delta=2
global __ptext443
__ptext443:

;; *************** function _isKeyPressed *****************
;; Defined at:
;;		line 446 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;                  1    wreg      unsigned char 
;; Registers used:
;;		wreg, status,2, status,0
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_Control
;; This function uses a non-reentrant model
;;
psect	text443
	file	"main.c"
	line	446
	global	__size_of_isKeyPressed
	__size_of_isKeyPressed	equ	__end_of_isKeyPressed-_isKeyPressed
	
_isKeyPressed:	
	opt	stack 4
; Regs used in _isKeyPressed: [wreg+status,2+status,0]
	line	447
	
l3547:	
;main.c: 447: return PC1 ? 0:1;
	clrc
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	btfss	(57/8),(57)&7
	setc
	movlw	0
	skipnc
	movlw	1

	goto	l1195
	
l3549:	
	line	448
	
l1195:	
	return
	opt stack 0
GLOBAL	__end_of_isKeyPressed
	__end_of_isKeyPressed:
;; =============== function _isKeyPressed ends ============

	signat	_isKeyPressed,89
	global	_VS1838_Process
psect	text444,local,class=CODE,delta=2
global __ptext444
__ptext444:

;; *************** function _VS1838_Process *****************
;; Defined at:
;;		line 313 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;  Data_H          1    8[BANK0 ] unsigned char 
;;  Data_L          1    7[BANK0 ] unsigned char 
;;  Address_L       1    6[BANK0 ] unsigned char 
;;  Address_H       1    5[BANK0 ] unsigned char 
;; Return value:  Size  Location     Type
;;                  1    wreg      unsigned char 
;; Registers used:
;;		wreg, status,2, status,0
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       4       0
;;      Temps:          0       5       0
;;      Totals:         0       9       0
;;Total ram usage:        9 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_Control
;; This function uses a non-reentrant model
;;
psect	text444
	file	"main.c"
	line	313
	global	__size_of_VS1838_Process
	__size_of_VS1838_Process	equ	__end_of_VS1838_Process-_VS1838_Process
	
_VS1838_Process:	
	opt	stack 4
; Regs used in _VS1838_Process: [wreg+status,2+status,0]
	line	317
	
l3505:	
;main.c: 314: unsigned char Address_H,Address_L;
;main.c: 315: unsigned char Data_H,Data_L;
;main.c: 317: if(VS1838_receive_ok==1) {
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(_VS1838_receive_ok),w
	xorlw	01h
	skipz
	goto	u2621
	goto	u2620
u2621:
	goto	l3523
u2620:
	line	318
	
l3507:	
;main.c: 318: Address_H=VS838_Receive_Data>>24;
	movf	(_VS838_Receive_Data),w
	movwf	(??_VS1838_Process+0)+0
	movf	(_VS838_Receive_Data+1),w
	movwf	((??_VS1838_Process+0)+0+1)
	movf	(_VS838_Receive_Data+2),w
	movwf	((??_VS1838_Process+0)+0+2)
	movf	(_VS838_Receive_Data+3),w
	movwf	((??_VS1838_Process+0)+0+3)
	movlw	018h
u2635:
	clrc
	rrf	(??_VS1838_Process+0)+3,f
	rrf	(??_VS1838_Process+0)+2,f
	rrf	(??_VS1838_Process+0)+1,f
	rrf	(??_VS1838_Process+0)+0,f
u2630:
	addlw	-1
	skipz
	goto	u2635
	movf	0+(??_VS1838_Process+0)+0,w
	movwf	(??_VS1838_Process+4)+0
	movf	(??_VS1838_Process+4)+0,w
	movwf	(VS1838_Process@Address_H)
	line	319
;main.c: 319: Address_L=(VS838_Receive_Data>>16)&0xff;
	movf	(_VS838_Receive_Data),w
	movwf	(??_VS1838_Process+0)+0
	movf	(_VS838_Receive_Data+1),w
	movwf	((??_VS1838_Process+0)+0+1)
	movf	(_VS838_Receive_Data+2),w
	movwf	((??_VS1838_Process+0)+0+2)
	movf	(_VS838_Receive_Data+3),w
	movwf	((??_VS1838_Process+0)+0+3)
	movlw	010h
u2645:
	clrc
	rrf	(??_VS1838_Process+0)+3,f
	rrf	(??_VS1838_Process+0)+2,f
	rrf	(??_VS1838_Process+0)+1,f
	rrf	(??_VS1838_Process+0)+0,f
u2640:
	addlw	-1
	skipz
	goto	u2645
	movf	0+(??_VS1838_Process+0)+0,w
	movwf	(??_VS1838_Process+4)+0
	movf	(??_VS1838_Process+4)+0,w
	movwf	(VS1838_Process@Address_L)
	line	320
	
l3509:	
;main.c: 320: if((Address_H==(unsigned char)~Address_L)) {
	comf	(VS1838_Process@Address_L),w
	xorwf	(VS1838_Process@Address_H),w
	skipz
	goto	u2651
	goto	u2650
u2651:
	goto	l3523
u2650:
	line	321
	
l3511:	
;main.c: 321: Data_H=VS838_Receive_Data>>8;
	movf	(_VS838_Receive_Data),w
	movwf	(??_VS1838_Process+0)+0
	movf	(_VS838_Receive_Data+1),w
	movwf	((??_VS1838_Process+0)+0+1)
	movf	(_VS838_Receive_Data+2),w
	movwf	((??_VS1838_Process+0)+0+2)
	movf	(_VS838_Receive_Data+3),w
	movwf	((??_VS1838_Process+0)+0+3)
	movlw	08h
u2665:
	clrc
	rrf	(??_VS1838_Process+0)+3,f
	rrf	(??_VS1838_Process+0)+2,f
	rrf	(??_VS1838_Process+0)+1,f
	rrf	(??_VS1838_Process+0)+0,f
u2660:
	addlw	-1
	skipz
	goto	u2665
	movf	0+(??_VS1838_Process+0)+0,w
	movwf	(??_VS1838_Process+4)+0
	movf	(??_VS1838_Process+4)+0,w
	movwf	(VS1838_Process@Data_H)
	line	322
	
l3513:	
;main.c: 322: Data_L=VS838_Receive_Data;
	movf	(_VS838_Receive_Data),w
	movwf	(??_VS1838_Process+0)+0
	movf	(??_VS1838_Process+0)+0,w
	movwf	(VS1838_Process@Data_L)
	line	323
	
l3515:	
;main.c: 323: if(Data_H==(unsigned char)~Data_L) {
	comf	(VS1838_Process@Data_L),w
	xorwf	(VS1838_Process@Data_H),w
	skipz
	goto	u2671
	goto	u2670
u2671:
	goto	l3523
u2670:
	line	324
	
l3517:	
;main.c: 324: VS1838_Data=Data_H;
	movf	(VS1838_Process@Data_H),w
	movwf	(??_VS1838_Process+0)+0
	movf	(??_VS1838_Process+0)+0,w
	movwf	(_VS1838_Data)
	line	325
	
l3519:	
;main.c: 325: VS1838_Status=0;
	clrf	(_VS1838_Status)
	line	326
;main.c: 326: VS838_Receive_Data=0;
	movlw	0
	movwf	(_VS838_Receive_Data+3)
	movlw	0
	movwf	(_VS838_Receive_Data+2)
	movlw	0
	movwf	(_VS838_Receive_Data+1)
	movlw	0
	movwf	(_VS838_Receive_Data)

	line	327
;main.c: 327: return 1;
	movlw	(01h)
	goto	l1164
	
l3521:	
	goto	l1164
	line	328
	
l1163:	
	goto	l3523
	line	329
	
l1162:	
	goto	l3523
	line	330
	
l1161:	
	line	331
	
l3523:	
;main.c: 328: }
;main.c: 329: }
;main.c: 330: }
;main.c: 331: return 0;
	movlw	(0)
	goto	l1164
	
l3525:	
	line	332
	
l1164:	
	return
	opt stack 0
GLOBAL	__end_of_VS1838_Process
	__end_of_VS1838_Process:
;; =============== function _VS1838_Process ends ============

	signat	_VS1838_Process,89
	global	_Timer1_Init
psect	text445,local,class=CODE,delta=2
global __ptext445
__ptext445:

;; *************** function _Timer1_Init *****************
;; Defined at:
;;		line 232 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text445
	file	"main.c"
	line	232
	global	__size_of_Timer1_Init
	__size_of_Timer1_Init	equ	__end_of_Timer1_Init-_Timer1_Init
	
_Timer1_Init:	
	opt	stack 5
; Regs used in _Timer1_Init: [wreg+status,2]
	line	234
	
l3417:	
;main.c: 234: T1CKPS1 = 1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(133/8),(133)&7
	line	235
;main.c: 235: T1CKPS0 = 1;
	bsf	(132/8),(132)&7
	line	236
;main.c: 236: T1OSCEN = 0;
	bcf	(131/8),(131)&7
	line	237
	
l3419:	
;main.c: 237: CCP1CON = 0x05;
	movlw	(05h)
	movwf	(21)	;volatile
	line	238
	
l3421:	
;main.c: 238: TMR1CS = 0X00;
	bcf	(129/8),(129)&7
	line	240
	
l3423:	
;main.c: 240: PEIE = 1;
	bsf	(94/8),(94)&7
	line	241
	
l3425:	
;main.c: 241: CCP1IE=1;
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	bsf	(1128/8)^080h,(1128)&7
	line	242
	
l3427:	
;main.c: 242: CCP1IF=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bcf	(104/8),(104)&7
	line	243
	
l3429:	
;main.c: 243: TRISC5 = 1;
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	bsf	(1085/8)^080h,(1085)&7
	line	244
	
l3431:	
;main.c: 244: TMR1H=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(15)	;volatile
	line	245
	
l3433:	
;main.c: 245: TMR1L=0;
	clrf	(14)	;volatile
	line	246
	
l3435:	
;main.c: 246: GIE=1;
	bsf	(95/8),(95)&7
	line	247
	
l3437:	
;main.c: 247: TMR1ON = 1;
	bsf	(128/8),(128)&7
	line	248
	
l1135:	
	return
	opt stack 0
GLOBAL	__end_of_Timer1_Init
	__end_of_Timer1_Init:
;; =============== function _Timer1_Init ends ============

	signat	_Timer1_Init,88
	global	_GPIO_Init
psect	text446,local,class=CODE,delta=2
global __ptext446
__ptext446:

;; *************** function _GPIO_Init *****************
;; Defined at:
;;		line 126 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		None
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text446
	file	"main.c"
	line	126
	global	__size_of_GPIO_Init
	__size_of_GPIO_Init	equ	__end_of_GPIO_Init-_GPIO_Init
	
_GPIO_Init:	
	opt	stack 5
; Regs used in _GPIO_Init: []
	line	128
	
l3415:	
;main.c: 128: TRISA0=0;
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	bcf	(1064/8)^080h,(1064)&7
	line	129
;main.c: 129: TRISA1=0;
	bcf	(1065/8)^080h,(1065)&7
	line	130
;main.c: 130: TRISA2=0;
	bcf	(1066/8)^080h,(1066)&7
	line	131
;main.c: 131: TRISA3=0;
	bcf	(1067/8)^080h,(1067)&7
	line	132
;main.c: 132: TRISA4=0;
	bcf	(1068/8)^080h,(1068)&7
	line	133
;main.c: 133: TRISA5=1;
	bsf	(1069/8)^080h,(1069)&7
	line	134
;main.c: 134: TRISA6=0;
	bcf	(1070/8)^080h,(1070)&7
	line	135
;main.c: 135: TRISA7=0;
	bcf	(1071/8)^080h,(1071)&7
	line	137
;main.c: 137: TRISC0=1;
	bsf	(1080/8)^080h,(1080)&7
	line	138
;main.c: 138: TRISC1=1;
	bsf	(1081/8)^080h,(1081)&7
	line	139
;main.c: 139: TRISC2=0;
	bcf	(1082/8)^080h,(1082)&7
	line	140
;main.c: 140: TRISC3=0;
	bcf	(1083/8)^080h,(1083)&7
	line	141
;main.c: 141: TRISC4=0;
	bcf	(1084/8)^080h,(1084)&7
	line	142
;main.c: 142: TRISC5=0;
	bcf	(1085/8)^080h,(1085)&7
	line	144
;main.c: 144: CM0=1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	bsf	(200/8),(200)&7
	line	145
;main.c: 145: CM1=1;
	bsf	(201/8),(201)&7
	line	146
;main.c: 146: CM2=1;
	bsf	(202/8),(202)&7
	line	150
;main.c: 150: ANSEL0=0;
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	bcf	(1160/8)^080h,(1160)&7
	line	151
;main.c: 151: ANSEL1=0;
	bcf	(1161/8)^080h,(1161)&7
	line	152
;main.c: 152: ANSEL2=0;
	bcf	(1162/8)^080h,(1162)&7
	line	153
;main.c: 153: ANSEL3=0;
	bcf	(1163/8)^080h,(1163)&7
	line	154
;main.c: 154: ANSEL4=0;
	bcf	(1164/8)^080h,(1164)&7
	line	155
;main.c: 155: ANSEL5=0;
	bcf	(1165/8)^080h,(1165)&7
	line	156
;main.c: 156: ANSEL6=0;
	bcf	(1166/8)^080h,(1166)&7
	line	157
;main.c: 157: ANSEL7=0;
	bcf	(1167/8)^080h,(1167)&7
	line	159
;main.c: 159: WPUA0=1;
	bsf	(1192/8)^080h,(1192)&7
	line	160
;main.c: 160: WPUA1=1;
	bsf	(1193/8)^080h,(1193)&7
	line	161
;main.c: 161: WPUA2=1;
	bsf	(1194/8)^080h,(1194)&7
	line	162
;main.c: 162: WPUA3=1;
	bsf	(1195/8)^080h,(1195)&7
	line	163
;main.c: 163: WPUA4=1;
	bsf	(1196/8)^080h,(1196)&7
	line	164
;main.c: 164: WPUA5=1;
	bsf	(1197/8)^080h,(1197)&7
	line	165
;main.c: 165: WPUA6=1;
	bsf	(1198/8)^080h,(1198)&7
	line	166
;main.c: 166: WPUA7=1;
	bsf	(1199/8)^080h,(1199)&7
	line	168
;main.c: 168: WPUC0=1;
	bsf	(1088/8)^080h,(1088)&7
	line	169
;main.c: 169: WPUC1=1;
	bsf	(1089/8)^080h,(1089)&7
	line	170
;main.c: 170: WPUC2=0;
	bcf	(1090/8)^080h,(1090)&7
	line	171
;main.c: 171: WPUC3=0;
	bcf	(1091/8)^080h,(1091)&7
	line	172
;main.c: 172: WPUC4=0;
	bcf	(1092/8)^080h,(1092)&7
	line	173
;main.c: 173: WPUC5=0;
	bcf	(1093/8)^080h,(1093)&7
	line	175
;main.c: 175: WPDA4=0;
	bcf	(1100/8)^080h,(1100)&7
	line	176
;main.c: 176: WPDC1=0;
	bcf	(1099/8)^080h,(1099)&7
	line	177
;main.c: 177: WPDC2=0;
	bcf	(1098/8)^080h,(1098)&7
	line	178
;main.c: 178: WPDC3=0;
	bcf	(1097/8)^080h,(1097)&7
	line	179
	
l1125:	
	return
	opt stack 0
GLOBAL	__end_of_GPIO_Init
	__end_of_GPIO_Init:
;; =============== function _GPIO_Init ends ============

	signat	_GPIO_Init,88
	global	_System_init
psect	text447,local,class=CODE,delta=2
global __ptext447
__ptext447:

;; *************** function _System_init *****************
;; Defined at:
;;		line 113 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, status,2
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    2
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_main
;; This function uses a non-reentrant model
;;
psect	text447
	file	"main.c"
	line	113
	global	__size_of_System_init
	__size_of_System_init	equ	__end_of_System_init-_System_init
	
_System_init:	
	opt	stack 5
; Regs used in _System_init: [wreg+status,2]
	line	114
	
l3409:	
;main.c: 114: OPTION = 0x00;
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	clrf	(129)^080h	;volatile
	line	115
	
l3411:	
;main.c: 115: OSCCON = 0x70;
	movlw	(070h)
	movwf	(143)^080h	;volatile
	line	116
	
l3413:	
;main.c: 116: WDTCON = 0x1F;
	movlw	(01Fh)
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	(24)	;volatile
	line	117
	
l1122:	
	return
	opt stack 0
GLOBAL	__end_of_System_init
	__end_of_System_init:
;; =============== function _System_init ends ============

	signat	_System_init,88
	global	_ISR
psect	text448,local,class=CODE,delta=2
global __ptext448
__ptext448:

;; *************** function _ISR *****************
;; Defined at:
;;		line 188 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, fsr0l, fsr0h, status,2, status,0, pclath, cstack
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          4       0       0
;;      Totals:         4       0       0
;;Total ram usage:        4 bytes
;; Hardware stack levels used:    1
;; Hardware stack levels required when called:    1
;; This function calls:
;;		_GetCCP1Value
;;		_VS1838ReceiveHandle
;;		_Clean_countReg
;; This function is called by:
;;		Interrupt level 1
;; This function uses a non-reentrant model
;;
psect	text448
	file	"main.c"
	line	188
	global	__size_of_ISR
	__size_of_ISR	equ	__end_of_ISR-_ISR
	
_ISR:	
	opt	stack 3
; Regs used in _ISR: [wreg-fsr0h+status,2+status,0+pclath+cstack]
psect	intentry,class=CODE,delta=2
global __pintentry
__pintentry:
global interrupt_function
interrupt_function:
	global saved_w
	saved_w	set	btemp+0
	movwf	saved_w
	swapf	status,w
	movwf	(??_ISR+0)
	movf	fsr0,w
	movwf	(??_ISR+1)
	movf	pclath,w
	movwf	(??_ISR+2)
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	btemp+1,w
	movwf	(??_ISR+3)
	ljmp	_ISR
psect	text448
	line	189
	
i1l3439:	
;main.c: 189: if(CCP1IE&CCP1IF == 1) {
	movlw	1
	bsf	status, 5	;RP0=1, select bank1
	bcf	status, 6	;RP1=0, select bank1
	btfss	(1128/8)^080h,(1128)&7
	andlw	0
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	btfss	(104/8),(104)&7
	andlw	0
	iorlw	0
	skipnz
	goto	u252_21
	goto	u252_20
u252_21:
	goto	i1l1129
u252_20:
	line	190
	
i1l3441:	
;main.c: 190: CCP1IF = 0;
	bcf	(104/8),(104)&7
	line	191
	
i1l3443:	
;main.c: 191: Cycle_CCPR = GetCCP1Value();
	fcall	_GetCCP1Value
	movf	(1+(?_GetCCP1Value)),w
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(_Cycle_CCPR+1)
	addwf	(_Cycle_CCPR+1)
	movf	(0+(?_GetCCP1Value)),w
	clrf	(_Cycle_CCPR)
	addwf	(_Cycle_CCPR)

	line	192
	
i1l3445:	
;main.c: 192: vs_times=0;
	clrf	(_vs_times)
	line	193
	
i1l3447:	
;main.c: 193: VS1838ReceiveHandle(Cycle_CCPR);
	movf	(_Cycle_CCPR+1),w
	clrf	(?_VS1838ReceiveHandle+1)
	addwf	(?_VS1838ReceiveHandle+1)
	movf	(_Cycle_CCPR),w
	clrf	(?_VS1838ReceiveHandle)
	addwf	(?_VS1838ReceiveHandle)

	fcall	_VS1838ReceiveHandle
	line	194
	
i1l3449:	
;main.c: 194: Clean_countReg();
	fcall	_Clean_countReg
	goto	i1l1129
	line	195
	
i1l1128:	
	line	196
	
i1l1129:	
	movf	(??_ISR+3),w
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movwf	btemp+1
	movf	(??_ISR+2),w
	movwf	pclath
	movf	(??_ISR+1),w
	movwf	fsr0
	swapf	(??_ISR+0)^0FFFFFF80h,w
	movwf	status
	swapf	saved_w,f
	swapf	saved_w,w
	retfie
	opt stack 0
GLOBAL	__end_of_ISR
	__end_of_ISR:
;; =============== function _ISR ends ============

	signat	_ISR,88
	global	_Clean_countReg
psect	text449,local,class=CODE,delta=2
global __ptext449
__ptext449:

;; *************** function _Clean_countReg *****************
;; Defined at:
;;		line 261 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		status,2
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         0       0       0
;;      Locals:         0       0       0
;;      Temps:          0       0       0
;;      Totals:         0       0       0
;;Total ram usage:        0 bytes
;; Hardware stack levels used:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_ISR
;; This function uses a non-reentrant model
;;
psect	text449
	file	"main.c"
	line	261
	global	__size_of_Clean_countReg
	__size_of_Clean_countReg	equ	__end_of_Clean_countReg-_Clean_countReg
	
_Clean_countReg:	
	opt	stack 3
; Regs used in _Clean_countReg: [status,2]
	line	262
	
i1l3503:	
;main.c: 262: TMR1H=0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(15)	;volatile
	line	263
;main.c: 263: TMR1L=0;
	clrf	(14)	;volatile
	line	264
	
i1l1141:	
	return
	opt stack 0
GLOBAL	__end_of_Clean_countReg
	__end_of_Clean_countReg:
;; =============== function _Clean_countReg ends ============

	signat	_Clean_countReg,88
	global	_VS1838ReceiveHandle
psect	text450,local,class=CODE,delta=2
global __ptext450
__ptext450:

;; *************** function _VS1838ReceiveHandle *****************
;; Defined at:
;;		line 273 in file "main.c"
;; Parameters:    Size  Location     Type
;;  capdata         2    0[COMMON] unsigned int 
;; Auto vars:     Size  Location     Type
;;		None
;; Return value:  Size  Location     Type
;;		None               void
;; Registers used:
;;		wreg, fsr0l, fsr0h, status,2, status,0
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         2       0       0
;;      Locals:         0       0       0
;;      Temps:          4       0       0
;;      Totals:         6       0       0
;;Total ram usage:        6 bytes
;; Hardware stack levels used:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_ISR
;; This function uses a non-reentrant model
;;
psect	text450
	file	"main.c"
	line	273
	global	__size_of_VS1838ReceiveHandle
	__size_of_VS1838ReceiveHandle	equ	__end_of_VS1838ReceiveHandle-_VS1838ReceiveHandle
	
_VS1838ReceiveHandle:	
	opt	stack 3
; Regs used in _VS1838ReceiveHandle: [wreg-fsr0h+status,2+status,0]
	line	274
	
i1l3463:	
;main.c: 274: switch(VS1838_Status) {
	goto	i1l3501
	line	275
;main.c: 275: case 0:
	
i1l1145:	
	line	276
	
i1l3465:	
;main.c: 276: if((capdata >= 3000)&&(capdata <= 7000)) {
	movlw	high(0BB8h)
	subwf	(VS1838ReceiveHandle@capdata+1),w
	movlw	low(0BB8h)
	skipnz
	subwf	(VS1838ReceiveHandle@capdata),w
	skipc
	goto	u253_21
	goto	u253_20
u253_21:
	goto	i1l1146
u253_20:
	
i1l3467:	
	movlw	high(01B59h)
	subwf	(VS1838ReceiveHandle@capdata+1),w
	movlw	low(01B59h)
	skipnz
	subwf	(VS1838ReceiveHandle@capdata),w
	skipnc
	goto	u254_21
	goto	u254_20
u254_21:
	goto	i1l1146
u254_20:
	line	277
	
i1l3469:	
;main.c: 277: VS1838_Status = 1;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(_VS1838_Status)
	bsf	status,0
	rlf	(_VS1838_Status),f
	line	278
;main.c: 278: } else {
	goto	i1l1158
	
i1l1146:	
	line	279
;main.c: 279: VS1838_Status = 0;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	clrf	(_VS1838_Status)
	line	280
;main.c: 280: VS1838_Receive_Count = 0;
	clrf	(_VS1838_Receive_Count)
	goto	i1l1158
	line	281
	
i1l1147:	
	line	282
;main.c: 281: }
;main.c: 282: break;
	goto	i1l1158
	line	283
;main.c: 283: case 1:
	
i1l1149:	
	line	284
	
i1l3471:	
;main.c: 284: if((capdata>=800)&&(capdata<=1500)) {
	movlw	high(0320h)
	subwf	(VS1838ReceiveHandle@capdata+1),w
	movlw	low(0320h)
	skipnz
	subwf	(VS1838ReceiveHandle@capdata),w
	skipc
	goto	u255_21
	goto	u255_20
u255_21:
	goto	i1l3477
u255_20:
	
i1l3473:	
	movlw	high(05DDh)
	subwf	(VS1838ReceiveHandle@capdata+1),w
	movlw	low(05DDh)
	skipnz
	subwf	(VS1838ReceiveHandle@capdata),w
	skipnc
	goto	u256_21
	goto	u256_20
u256_21:
	goto	i1l3477
u256_20:
	line	285
	
i1l3475:	
;main.c: 285: VS838_Receive_Data=VS838_Receive_Data<<1;
	movf	(_VS838_Receive_Data),w
	movwf	(??_VS1838ReceiveHandle+0)+0
	movf	(_VS838_Receive_Data+1),w
	movwf	((??_VS1838ReceiveHandle+0)+0+1)
	movf	(_VS838_Receive_Data+2),w
	movwf	((??_VS1838ReceiveHandle+0)+0+2)
	movf	(_VS838_Receive_Data+3),w
	movwf	((??_VS1838ReceiveHandle+0)+0+3)
	movlw	01h
u257_25:
	clrc
	rlf	(??_VS1838ReceiveHandle+0)+0,f
	rlf	(??_VS1838ReceiveHandle+0)+1,f
	rlf	(??_VS1838ReceiveHandle+0)+2,f
	rlf	(??_VS1838ReceiveHandle+0)+3,f
u257_20:
	addlw	-1
	skipz
	goto	u257_25
	movf	3+(??_VS1838ReceiveHandle+0)+0,w
	movwf	(_VS838_Receive_Data+3)
	movf	2+(??_VS1838ReceiveHandle+0)+0,w
	movwf	(_VS838_Receive_Data+2)
	movf	1+(??_VS1838ReceiveHandle+0)+0,w
	movwf	(_VS838_Receive_Data+1)
	movf	0+(??_VS1838ReceiveHandle+0)+0,w
	movwf	(_VS838_Receive_Data)

	line	286
;main.c: 286: VS1838_Receive_Count++;
	movlw	(01h)
	movwf	(??_VS1838ReceiveHandle+0)+0
	movf	(??_VS1838ReceiveHandle+0)+0,w
	addwf	(_VS1838_Receive_Count),f
	line	287
;main.c: 287: } else if((capdata >= 1800)&&(capdata <= 2800)) {
	goto	i1l3497
	
i1l1150:	
	
i1l3477:	
	movlw	high(0708h)
	subwf	(VS1838ReceiveHandle@capdata+1),w
	movlw	low(0708h)
	skipnz
	subwf	(VS1838ReceiveHandle@capdata),w
	skipc
	goto	u258_21
	goto	u258_20
u258_21:
	goto	i1l1152
u258_20:
	
i1l3479:	
	movlw	high(0AF1h)
	subwf	(VS1838ReceiveHandle@capdata+1),w
	movlw	low(0AF1h)
	skipnz
	subwf	(VS1838ReceiveHandle@capdata),w
	skipnc
	goto	u259_21
	goto	u259_20
u259_21:
	goto	i1l1152
u259_20:
	line	288
	
i1l3481:	
;main.c: 288: VS838_Receive_Data=VS838_Receive_Data<<1;
	movf	(_VS838_Receive_Data),w
	movwf	(??_VS1838ReceiveHandle+0)+0
	movf	(_VS838_Receive_Data+1),w
	movwf	((??_VS1838ReceiveHandle+0)+0+1)
	movf	(_VS838_Receive_Data+2),w
	movwf	((??_VS1838ReceiveHandle+0)+0+2)
	movf	(_VS838_Receive_Data+3),w
	movwf	((??_VS1838ReceiveHandle+0)+0+3)
	movlw	01h
u260_25:
	clrc
	rlf	(??_VS1838ReceiveHandle+0)+0,f
	rlf	(??_VS1838ReceiveHandle+0)+1,f
	rlf	(??_VS1838ReceiveHandle+0)+2,f
	rlf	(??_VS1838ReceiveHandle+0)+3,f
u260_20:
	addlw	-1
	skipz
	goto	u260_25
	movf	3+(??_VS1838ReceiveHandle+0)+0,w
	movwf	(_VS838_Receive_Data+3)
	movf	2+(??_VS1838ReceiveHandle+0)+0,w
	movwf	(_VS838_Receive_Data+2)
	movf	1+(??_VS1838ReceiveHandle+0)+0,w
	movwf	(_VS838_Receive_Data+1)
	movf	0+(??_VS1838ReceiveHandle+0)+0,w
	movwf	(_VS838_Receive_Data)

	line	289
	
i1l3483:	
;main.c: 289: VS838_Receive_Data=VS838_Receive_Data|0x0001;
	movlw	01h
	iorwf	(_VS838_Receive_Data),w
	movwf	(_VS838_Receive_Data)
	movlw	0
	iorwf	(_VS838_Receive_Data+1),w
	movwf	1+(_VS838_Receive_Data)
	movlw	0
	iorwf	(_VS838_Receive_Data+2),w
	movwf	2+(_VS838_Receive_Data)
	movlw	0
	iorwf	(_VS838_Receive_Data+3),w
	movwf	3+(_VS838_Receive_Data)
	line	290
	
i1l3485:	
;main.c: 290: VS1838_Receive_Count++;
	movlw	(01h)
	movwf	(??_VS1838ReceiveHandle+0)+0
	movf	(??_VS1838ReceiveHandle+0)+0,w
	addwf	(_VS1838_Receive_Count),f
	line	291
;main.c: 291: } else {
	goto	i1l3497
	
i1l1152:	
	line	292
;main.c: 292: VS1838_Status = 0;
	clrf	(_VS1838_Status)
	line	293
	
i1l3487:	
;main.c: 293: VS838_Receive_Data = 0;
	movlw	0
	movwf	(_VS838_Receive_Data+3)
	movlw	0
	movwf	(_VS838_Receive_Data+2)
	movlw	0
	movwf	(_VS838_Receive_Data+1)
	movlw	0
	movwf	(_VS838_Receive_Data)

	line	294
	
i1l3489:	
;main.c: 294: VS1838_Receive_Count = 0;
	clrf	(_VS1838_Receive_Count)
	goto	i1l3497
	line	295
	
i1l1153:	
	goto	i1l3497
	
i1l1151:	
	line	297
;main.c: 295: }
;main.c: 297: while(VS1838_Receive_Count == 32) {
	goto	i1l3497
	
i1l1155:	
	line	298
	
i1l3491:	
;main.c: 298: VS1838_receive_ok = 1;
	clrf	(_VS1838_receive_ok)
	bsf	status,0
	rlf	(_VS1838_receive_ok),f
	line	299
	
i1l3493:	
;main.c: 299: VS1838_Status = 0;
	clrf	(_VS1838_Status)
	line	300
	
i1l3495:	
;main.c: 300: VS1838_Receive_Count = 0;
	clrf	(_VS1838_Receive_Count)
	line	301
;main.c: 301: break;
	goto	i1l1158
	line	302
	
i1l1154:	
	line	297
	
i1l3497:	
	movf	(_VS1838_Receive_Count),w
	xorlw	020h
	skipnz
	goto	u261_21
	goto	u261_20
u261_21:
	goto	i1l3491
u261_20:
	goto	i1l1158
	
i1l1156:	
	line	303
;main.c: 302: }
;main.c: 303: break;
	goto	i1l1158
	line	304
;main.c: 304: default : {
	
i1l1157:	
	line	306
;main.c: 306: VS1838_Status=0;
	clrf	(_VS1838_Status)
	line	308
;main.c: 307: }
;main.c: 308: break;
	goto	i1l1158
	line	309
	
i1l3499:	
;main.c: 309: }
	goto	i1l1158
	line	274
	
i1l1144:	
	
i1l3501:	
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(_VS1838_Status),w
	; Switch size 1, requested type "space"
; Number of cases is 2, Range of values is 0 to 1
; switch strategies available:
; Name         Instructions Cycles
; simple_byte            7     4 (average)
; direct_byte            8     6 (fixed)
; jumptable            260     6 (fixed)
; rangetable             6     6 (fixed)
; spacedrange           10     9 (fixed)
; locatedrange           2     3 (fixed)
;	Chosen strategy is simple_byte

	opt asmopt_off
	xorlw	0^0	; case 0
	skipnz
	goto	i1l3465
	xorlw	1^0	; case 1
	skipnz
	goto	i1l3471
	goto	i1l1157
	opt asmopt_on

	line	309
	
i1l1148:	
	line	310
	
i1l1158:	
	return
	opt stack 0
GLOBAL	__end_of_VS1838ReceiveHandle
	__end_of_VS1838ReceiveHandle:
;; =============== function _VS1838ReceiveHandle ends ============

	signat	_VS1838ReceiveHandle,4216
	global	_GetCCP1Value
psect	text451,local,class=CODE,delta=2
global __ptext451
__ptext451:

;; *************** function _GetCCP1Value *****************
;; Defined at:
;;		line 251 in file "main.c"
;; Parameters:    Size  Location     Type
;;		None
;; Auto vars:     Size  Location     Type
;;  values          2    4[COMMON] unsigned int 
;; Return value:  Size  Location     Type
;;                  2    0[COMMON] unsigned int 
;; Registers used:
;;		wreg, status,2, status,0
;; Tracked objects:
;;		On entry : 0/0
;;		On exit  : 0/0
;;		Unchanged: 0/0
;; Data sizes:     COMMON   BANK0   BANK1
;;      Params:         2       0       0
;;      Locals:         2       0       0
;;      Temps:          2       0       0
;;      Totals:         6       0       0
;;Total ram usage:        6 bytes
;; Hardware stack levels used:    1
;; This function calls:
;;		Nothing
;; This function is called by:
;;		_ISR
;; This function uses a non-reentrant model
;;
psect	text451
	file	"main.c"
	line	251
	global	__size_of_GetCCP1Value
	__size_of_GetCCP1Value	equ	__end_of_GetCCP1Value-_GetCCP1Value
	
_GetCCP1Value:	
	opt	stack 3
; Regs used in _GetCCP1Value: [wreg+status,2+status,0]
	line	252
	
i1l3451:	
;main.c: 252: unsigned int values=0;
	clrf	(GetCCP1Value@values)
	clrf	(GetCCP1Value@values+1)
	line	254
	
i1l3453:	
;main.c: 254: values = CCPR1H;
	bcf	status, 5	;RP0=0, select bank0
	bcf	status, 6	;RP1=0, select bank0
	movf	(20),w	;volatile
	movwf	(??_GetCCP1Value+0)+0
	clrf	(??_GetCCP1Value+0)+0+1
	movf	0+(??_GetCCP1Value+0)+0,w
	movwf	(GetCCP1Value@values)
	movf	1+(??_GetCCP1Value+0)+0,w
	movwf	(GetCCP1Value@values+1)
	line	255
	
i1l3455:	
;main.c: 255: values = values<<8;
	movf	(GetCCP1Value@values),w
	movwf	(??_GetCCP1Value+0)+0+1
	clrf	(??_GetCCP1Value+0)+0
	movf	0+(??_GetCCP1Value+0)+0,w
	movwf	(GetCCP1Value@values)
	movf	1+(??_GetCCP1Value+0)+0,w
	movwf	(GetCCP1Value@values+1)
	line	256
	
i1l3457:	
;main.c: 256: values = values|CCPR1L;
	movf	(GetCCP1Value@values),w
	iorwf	(19),w	;volatile
	movwf	(GetCCP1Value@values)
	movf	(GetCCP1Value@values+1),w
	movwf	1+(GetCCP1Value@values)
	line	257
	
i1l3459:	
;main.c: 257: return values;
	movf	(GetCCP1Value@values+1),w
	clrf	(?_GetCCP1Value+1)
	addwf	(?_GetCCP1Value+1)
	movf	(GetCCP1Value@values),w
	clrf	(?_GetCCP1Value)
	addwf	(?_GetCCP1Value)

	goto	i1l1138
	
i1l3461:	
	line	258
	
i1l1138:	
	return
	opt stack 0
GLOBAL	__end_of_GetCCP1Value
	__end_of_GetCCP1Value:
;; =============== function _GetCCP1Value ends ============

	signat	_GetCCP1Value,90
psect	text452,local,class=CODE,delta=2
global __ptext452
__ptext452:
	global	btemp
	btemp set 07Eh

	DABS	1,126,2	;btemp
	global	wtemp0
	wtemp0 set btemp
	end
