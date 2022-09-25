;Universal PT2 and PT3 player for ZX Spectrum and MSX
;(c)2004-2007 S.V.Bulba <vorobey@mail.khstu.ru>
;http://bulba.untergrund.net (http://bulba.at.kz)

;Converted to z88dk-z80asm by @feilipu
;z88dk-z80asm  PS-PTxPlay.asm -m -l -b

;Release number
DEFC Release	= '1'

;Conditional assembly
;1) Version of ROUT (only CP/M BDOS)

;2) Current position counter at (START+13)
DEFC CurPosCounter	= 0
;3) Allow channels allocation bits at (START+12)
DEFC ACBBAC	= 0
;4) Allow loop checking and disabling
DEFC LoopChecker= 1
;5) Insert official identificator
DEFC Id		= 1

;Features
;--------
;-Can be compiled at any address (i.e. no need rounding ORG
; address).
;-Variables (VARS) can be located at any address (not only after
;code block).
;-INIT subprogram checks PT3-module version and rightly
; generates both note and volume tables outside of code block
; (in VARS).
;-Two portamento (spc. command 3xxx) algorithms (depending of
; PT3 module version).
;-New 1.XX and 2.XX special command behaviour (only for PT v3.7
; and higher).
;-Any Tempo value are accepted (including Tempo=1 and Tempo=2).
;-Fully compatible with Ay_Emul PT3 and PT2 players codes.
;-See also notes at the end of this source code.

;Limitations
;-----------
;-Can run in RAM only (self-modified code is used).
;-PT2 position list must be end by $FF marker only.

;Warning!!! PLAY subprogram can crash if no module are loaded
;into RAM or INIT subprogram was not called before.

;Call MUTE or INIT one more time to mute sound after stopping
;playing

DEFC boot	= $0000
DEFC bdos	= $0005
DEFC fcb1	= $005c
DEFC sfcb	= fcb1
DEFC sfcbcr	= sfcb+$20	;should point to CURRENT RECORD byte in SFCB
DEFC dbuff	= $0080
DEFC printf	= 9
DEFC openf	= 15
DEFC closef	= 16
DEFC readf	= 20
DEFC todma	= $80		;this is the size of the read to DMA buffer

DEFC ymreg	= $d8		;entry to RC2014 YM/AY register array
DEFC ymdat	= $d0		;and data port for when we need to write to selected register

SECTION code_user

ORG $100

	ld	(oldstack),sp
	ld	sp,stack
	ld	de,sfcb		;name of file and drive are stored here
	call	open
	ld	de,nofile	;just in case of error
	inc	a
	jp	Z,badfile	;no file found if zero
	xor	a		;have to do this every file open
	ld	(sfcbcr),a	;set current record to 0 as CP/M does not
	ld	de,MDLADDR	;it is there but got corrupted for some reason
	ld	(next_dest),de	;we now have where the file is copied too and where it is expected.

.r_loop
	ld	de,sfcb		;fcb needed prior to the call
	call	read
	or	a		;this will set zero flag unless it is eofile...
	jp	nz,eofile

	ld	hl,dbuff	;ok we need to copy from DMA to where PTX-player expects it to be
	ld	de,(next_dest)	;sets the de to current end of address
	ld	bc,todma	;CP/M only gives us 128 bytes at a time
	ldir			;ldir does the xfer for us
	ld	(next_dest),de	;DE is now 128 bytes higher, and, we can use to show where we got to
	jp	r_loop

.eofile
	ld	de,sfcb
	call	close

.finis
	ld	de,success	;message at this location
	ld	c,printf	;print it
	call	bdos
	ld	sp,(oldstack)	;put stack back to how we found it
	jp	begin		;and start PTX-player

.badfile
	ld	de,nofile
	ld	c,printf
	call	bdos
	ld	sp,(oldstack)	;put stack back to how we found it
	ld	c,$0		;this is the CP/M proper exit call
	jp	bdos

.open
	ld	c,openf
	jp	bdos

.close
	ld	c,closef
	jp	bdos

.read
	ld	c,readf
	jp	bdos

.begin
	;ld A,2 ;PT2,ABC,Looped
	ld a,0
	ld (START+12),a
	call START
	ld de,startupstr	;startup message at this location
	ld c,printf		;print it
	call bdos
._LP
	call START+6
	call pause
	jp _LP

	ld de,endstr		;end message at this location
	ld c,printf		;print it
	call bdos
	jp START+9

DEFC TonA	= 0
DEFC TonB	= 2
DEFC TonC	= 4
DEFC Noise	= 6
DEFC Mixer	= 7
DEFC AmplA	= 8
DEFC AmplB	= 9
DEFC AmplC	= 10
DEFC Env	= 11
DEFC EnvTp	= 13

;Entry and other points
;START initialize playing of module at MDLADDR
;START+3 initialization with module address in HL
;START+6 play one quark
;START+9 mute
;START+12 setup and status flags
;START+13 current position value (byte) (optional)

.START
	LD HL,MDLADDR
	JP INIT
	JP PLAY
	JP MUTE

.SETUP
	DB 0
			;set bit0, if you want to play without looping
			;(optional);
			;set bit1 for PT2 and reset for PT3 before
			;calling INIT;
			;bits2-3: %00-ABC, %01 ACB, %10 BAC (optional);
			;bits4-6 are not used
			;bit7 is set each time, when loop point is passed

IF CurPosCounter
.CurPos
	DB 0		;for visualization only (i.e. no need for playing)
			;(optional)
ENDIF

;Identifier
IF Id
	DM "=Uni PT2 and PT3 Player r.",Release,"="
ENDIF

IF LoopChecker
.CHECKLP
	LD HL,SETUP
	SET 7,(HL)
	BIT 0,(HL)
	RET Z
	POP HL
	LD HL,DelyCnt
	INC (HL)
	LD HL,ChanA+CHP_NtSkCn
	INC (HL)
ENDIF

.MUTE
	XOR A
	LD H,A
	LD L,A
	LD (AYREGS+AmplA),A
	LD (AYREGS+AmplB),HL
	JP ROUT

.INIT
;HL - AddressOfModule
	LD A,(START+12)
	AND 2
	JP NZ,INITPT2

	CALL SETMDAD
	PUSH HL
	LD DE,100
	ADD HL,DE
	LD A,(HL)
	LD (Delay),A
	PUSH HL
	POP IX
	ADD HL,DE
	LD (CrPsPtr),HL
	LD E,(IX+102-100)
	INC HL

IF CurPosCounter
	LD A,L
	LD (PosSub+1),A
ENDIF

	ADD HL,DE
	LD (LPosPtr),HL
	POP DE
	LD L,(IX+103-100)
	LD H,(IX+104-100)
	ADD HL,DE
	LD (PatsPtr),HL
	LD HL,169
	ADD HL,DE
	LD (OrnPtrs),HL
	LD HL,105
	ADD HL,DE
	LD (SamPtrs),HL
	LD A,(IX+13-100) ;EXTRACT VERSION NUMBER
	SUB $30
	JP C,L20
	CP 10
	JP C,L21
.L20
	LD A,6
.L21
	LD (Version),A
	PUSH AF ;VolTable version
	CP 4
	LD A,(IX+99-100) ;TONE TABLE NUMBER
	RLA
	AND 7
	PUSH AF ;NoteTable number
	LD HL,+(e_-SamCnv-2)*256+$18
	LD (SamCnv),HL
	LD A,$BA
	LD (OrnCP),A
	LD (SamCP),A
	LD A,$7B
	LD (OrnLD),A
	LD (SamLD),A
	LD A,$87
	LD (SamClc2),A
	LD BC,PT3PD
	LD HL,0
	LD DE,PT3EMPTYORN
	JP INITCOMMON

.INITPT2
	LD A,(HL)
	LD (Delay),A
	PUSH HL
	PUSH HL
	PUSH HL
	INC HL
	INC HL
	LD A,(HL)
	INC HL
	LD (SamPtrs),HL
	LD E,(HL)
	INC HL
	LD D,(HL)
	POP HL
	AND A
	SBC HL,DE
	CALL SETMDAD
	POP HL
	LD DE,67
	ADD HL,DE
	LD (OrnPtrs),HL
	LD E,32
	ADD HL,DE
	LD C,(HL)
	INC HL
	LD B,(HL)
	LD E,30
	ADD HL,DE
	LD (CrPsPtr),HL
	LD E,A
	INC HL

IF CurPosCounter
	LD A,L
	LD (PosSub+1),A
ENDIF

	ADD HL,DE
	LD (LPosPtr),HL
	POP HL
	ADD HL,BC
	LD (PatsPtr),HL
	LD A,5
	LD (Version),A
	PUSH AF
	LD A,2
	PUSH AF
	LD HL,$51CB
	LD (SamCnv),HL
	LD A,$BB
	LD (OrnCP),A
	LD (SamCP),A
	LD A,$7A
	LD (OrnLD),A
	LD (SamLD),A
	LD A,$80
	LD (SamClc2),A
	LD BC,PT2PD
	LD HL,$8687
	LD DE,PT2EMPTYORN

.INITCOMMON
	LD (PTDECOD+1),BC
	LD (PsCalc),HL
	PUSH DE

;note table data depacker
;(c) Ivan Roshin
	LD DE,T_PACK
	LD BC,T1_+(2*49)-1
.TP_0
	LD A,(DE)
	INC DE
	CP 15*2
	JR NC,TP_1
	LD H,A
	LD A,(DE)
	LD L,A
	INC DE
	JR TP_2
.TP_1
	PUSH DE
	LD D,0
	LD E,A
	ADD HL,DE
	ADD HL,DE
	POP DE
.TP_2
	LD A,H
	LD (BC),A
	DEC BC
	LD A,L
	LD (BC),A
	DEC BC
	SUB +($F8*2)%256
	JR NZ,TP_0

IF LoopChecker
	LD HL,SETUP
	RES 7,(HL)

IF CurPosCounter
	INC HL
	LD (HL),A
ENDIF

ELSE

IF CurPosCounter
	LD (CurPos),A
ENDIF

ENDIF

	LD HL,VARS
	LD (HL),A
	LD DE,VARS+1
	LD BC,VAR0END-VARS-1
	LDIR
	LD (AdInPtA),HL ;ptr to zero
	INC A
	LD (DelyCnt),A
	LD HL,$F001 ;H - CHP_Volume, L - CHP_NtSkCn
	LD (ChanA+CHP_NtSkCn),HL
	LD (ChanB+CHP_NtSkCn),HL
	LD (ChanC+CHP_NtSkCn),HL
	POP HL
	LD (ChanA+CHP_OrnPtr),HL
	LD (ChanB+CHP_OrnPtr),HL
	LD (ChanC+CHP_OrnPtr),HL

	POP AF

;NoteTableCreator (c) Ivan Roshin
;A - NoteTableNumber*2+VersionForNoteTable
;(xx1b - 3.xx..3.4r, xx0b - 3.4x..3.6x..VTII1.0)

	LD HL,NT_DATA
	PUSH DE
	LD D,B
	ADD A,A
	LD E,A
	ADD HL,DE
	LD E,(HL)
	INC HL
	SRL E
	SBC A,A
	AND $A7 ;$00 (NOP) or $A7 (AND A)
	LD (L3),A
	EX DE,HL
	POP BC ;BC=T1_
	ADD HL,BC

	LD A,(DE)
	ADD A,T_%256
	LD C,A
	ADC A,T_/256
	SUB C
	LD B,A
	PUSH BC
	LD DE,NT_
	PUSH DE

	LD B,12
.L1
	PUSH BC
	LD C,(HL)
	INC HL
	PUSH HL
	LD B,(HL)

	PUSH DE
	EX DE,HL
	LD DE,23
	LD IXH,8

.L2
	SRL B
	RR C
.L3
	DB $19	;AND A or NOP
	LD A,C
	ADC A,D	;=ADC 0
	LD (HL),A
	INC HL
	LD A,B
	ADC A,D
	LD (HL),A
	ADD HL,DE
	DEC IXH
	JR NZ,L2

	POP DE
	INC DE
	INC DE
	POP HL
	INC HL
	POP BC
	DJNZ L1

	POP HL
	POP DE

	LD A,E
	CP TCOLD_1%256
	JR NZ,CORR_1
	LD A,$FD
	LD (NT_+$2E),A

.CORR_1
	LD A,(DE)
	AND A
	JR Z,TC_EXIT
	RRA
	PUSH AF
	ADD A,A
	LD C,A
	ADD HL,BC
	POP AF
	JR NC,CORR_2
	DEC (HL)
	DEC (HL)
.CORR_2
	INC (HL)
	AND A
	SBC HL,BC
	INC DE
	JR CORR_1

.TC_EXIT

	POP AF

;VolTableCreator (c) Ivan Roshin
;A - VersionForVolumeTable (0..4 - 3.xx..3.4x;
			;5.. - 2.x,3.5x..3.6x..VTII1.0)

	CP 5
	LD HL,$11
	LD D,H
	LD E,H
	LD A,$17
	JR NC,M1
	DEC L
	LD E,L
	XOR A
.M1
	LD (M2),A

	LD IX,VT_+16

	LD C,$0F
.INITV2
	PUSH HL

	ADD HL,DE
	EX DE,HL
	SBC HL,HL

	LD B,$10
.INITV1
	LD A,L
.M2
	DB $7D
	LD A,H
	ADC A,0
	LD (IX),A
	INC IX
	ADD HL,DE
	DJNZ INITV1

	POP HL
	LD A,E
	CP $77
	JR NZ,M3
	INC E
.M3
	DEC C
	JR NZ,INITV2

	JP ROUT

.SETMDAD
	LD (MODADDR),HL
	LD (MDADDR1),HL
	LD (MDADDR2),HL
	RET

.PTDECOD
	JP $C3C3

;PT2 pattern decoder
.PD2_SAM
	CALL SETSAM
	JR PD2_LOOP

.PD2_EOff
	LD (IX-12+CHP_Env_En),A
	JR PD2_LOOP

.PD2_ENV
	LD (IX-12+CHP_Env_En),16
	LD (AYREGS+EnvTp),A
	LD A,(BC)
	INC BC
	LD L,A
	LD A,(BC)
	INC BC
	LD H,A
	LD (EnvBase),HL
	JR PD2_LOOP

.PD2_ORN
	CALL SETORN
	JR PD2_LOOP

.PD2_SKIP
	INC A
	LD (IX-12+CHP_NNtSkp),A
	JR PD2_LOOP

.PD2_VOL
	RRCA
	RRCA
	RRCA
	RRCA
	LD (IX-12+CHP_Volume),A
	JR PD2_LOOP

.PD2_DEL
	CALL C_DELAY
	JR PD2_LOOP

.PD2_GLIS
	SET 2,(IX-12+CHP_Flags)
	INC A
	LD (IX-12+CHP_TnSlDl),A
	LD (IX-12+CHP_TSlCnt),A
	LD A,(BC)
	INC BC
        LD (IX-12+CHP_TSlStp),A
	ADD A,A
	SBC A,A
        LD (IX-12+CHP_TSlStp+1),A
	SCF
	JR PD2_LP2

.PT2PD
	AND A

.PD2_LP2
	EX AF,AF'

.PD2_LOOP
	LD A,(BC)
	INC BC
	ADD A,$20
	JR Z,PD2_REL
	JR C,PD2_SAM
	ADD A,96
	JR C,PD2_NOTE
	INC A
	JR Z,PD2_EOff
	ADD A,15
	JP Z,PD_FIN
	JR C,PD2_ENV
	ADD A,$10
	JR C,PD2_ORN
	ADD A,$40
	JR C,PD2_SKIP
	ADD A,$10
	JR C,PD2_VOL
	INC A
	JR Z,PD2_DEL
	INC A
	JR Z,PD2_GLIS
	INC A
	JR Z,PD2_PORT
	INC A
	JR Z,PD2_STOP
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_CrNsSl),A
	JR PD2_LOOP

.PD2_PORT
	RES 2,(IX-12+CHP_Flags)
	LD A,(BC)
	INC BC
	INC BC ;ignoring precalc delta to right sound
	INC BC
	SCF
	JR PD2_LP2

.PD2_STOP
	LD (IX-12+CHP_TSlCnt),A
	JR PD2_LOOP

.PD2_REL
	LD (IX-12+CHP_Flags),A
	JR PD2_EXIT

.PD2_NOTE
	LD L,A
	LD A,(IX-12+CHP_Note)
	LD (PrNote+1),A
	LD (IX-12+CHP_Note),L
	XOR A
	LD (IX-12+CHP_TSlCnt),A
	SET 0,(IX-12+CHP_Flags)
	EX AF,AF'
	JR NC,NOGLIS2
	BIT 2,(IX-12+CHP_Flags)
	JR NZ,NOPORT2
	LD (LoStep),A
	ADD A,A
	SBC A,A
	EX AF,AF'
	LD H,A
	LD L,A
	INC A
	CALL SETPORT
.NOPORT2
	LD (IX-12+CHP_TSlCnt),1
.NOGLIS2
	XOR A


.PD2_EXIT
	LD (IX-12+CHP_PsInSm),A
	LD (IX-12+CHP_PsInOr),A
	LD (IX-12+CHP_CrTnSl),A
	LD (IX-12+CHP_CrTnSl+1),A
	JP PD_FIN

;PT3 pattern decoder
.PD_OrSm
	LD (IX-12+CHP_Env_En),0
	CALL SETORN
.PD_SAM_
	LD A,(BC)
	INC BC
	RRCA

.PD_SAM
	CALL SETSAM
	JR PD_LOOP

.PD_VOL
	RRCA
	RRCA
	RRCA
	RRCA
	LD (IX-12+CHP_Volume),A
	JR PD_LP2

.PD_EOff
	LD (IX-12+CHP_Env_En),A
	LD (IX-12+CHP_PsInOr),A
	JR PD_LP2

.PD_SorE
	DEC A
	JR NZ,PD_ENV
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_NNtSkp),A
	JR PD_LP2

.PD_ENV
	CALL SETENV
	JR PD_LP2

.PD_ORN
	CALL SETORN
	JR PD_LOOP

.PD_ESAM
	LD (IX-12+CHP_Env_En),A
	LD (IX-12+CHP_PsInOr),A
	CALL NZ,SETENV
	JR PD_SAM_

.PT3PD
	LD A,(IX-12+CHP_Note)
	LD (PrNote+1),A
	LD L,(IX-12+CHP_CrTnSl)
	LD H,(IX-12+CHP_CrTnSl+1)
	LD (PrSlide+1),HL

.PD_LOOP
	LD DE,$2010
.PD_LP2
	LD A,(BC)
	INC BC
	ADD A,E
	JR C,PD_OrSm
	ADD A,D
	JR Z,PD_FIN
	JR C,PD_SAM
	ADD A,E
	JR Z,PD_REL
	JR C,PD_VOL
	ADD A,E
	JR Z,PD_EOff
	JR C,PD_SorE
	ADD A,96
	JR C,PD_NOTE
	ADD A,E
	JR C,PD_ORN
	ADD A,D
	JR C,PD_NOIS
	ADD A,E
	JR C,PD_ESAM
	ADD A,A
	LD E,A
	LD HL,SPCCOMS+$FF20-$2000
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	PUSH DE
	JR PD_LOOP

.PD_NOIS
	LD (Ns_Base),A
	JR PD_LP2

.PD_REL
	RES 0,(IX-12+CHP_Flags)
	JR PD_RES

.PD_NOTE
	LD (IX-12+CHP_Note),A
	SET 0,(IX-12+CHP_Flags)
	XOR A

.PD_RES
	LD (PDSP_+1),SP
	LD SP,IX
	LD H,A
	LD L,A
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
.PDSP_
	LD SP,$3131

.PD_FIN
	LD A,(IX-12+CHP_NNtSkp)
	LD (IX-12+CHP_NtSkCn),A
	RET

.C_PORTM
	LD A,(BC)
	INC BC
;SKIP PRECALCULATED TONE DELTA (BECAUSE
;CANNOT BE RIGHT AFTER PT3 COMPILATION)
	INC BC
	INC BC
	EX AF,AF'
	LD A,(BC) ;SIGNED TONE STEP
	INC BC
	LD (LoStep),A
	LD A,(BC)
	INC BC
	AND A
	EX AF,AF'
	LD L,(IX-12+CHP_CrTnSl)
	LD H,(IX-12+CHP_CrTnSl+1)

;Set portamento variables
;A - Delay; A' - Hi(Step); ZF' - (A'=0); HL - CrTnSl

.SETPORT
	RES 2,(IX-12+CHP_Flags)
	LD (IX-12+CHP_TnSlDl),A
	LD (IX-12+CHP_TSlCnt),A
	PUSH HL
	LD DE,NT_
	LD A,(IX-12+CHP_Note)
	LD (IX-12+CHP_SlToNt),A
	ADD A,A
	LD L,A
	LD H,0
	ADD HL,DE
	LD A,(HL)
	INC HL
	LD H,(HL)
	LD L,A
	PUSH HL
.PrNote
	LD A,$3E
	LD (IX-12+CHP_Note),A
	ADD A,A
	LD L,A
	LD H,0
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	POP HL
	SBC HL,DE
	LD (IX-12+CHP_TnDelt),L
	LD (IX-12+CHP_TnDelt+1),H
	POP DE

DEFC Version = ASMPC+1

	LD A,$3E
	CP 6
	JR C,OLDPRTM ;Old 3xxx for PT v3.5-
.PrSlide
	LD DE,$1111
	LD (IX-12+CHP_CrTnSl),E
	LD (IX-12+CHP_CrTnSl+1),D

DEFC LoStep	= ASMPC+1

.OLDPRTM
	LD A,$3E
	EX AF,AF'
	JR Z,NOSIG
	EX DE,HL
.NOSIG
	SBC HL,DE
	JP P,SET_STP
	CPL
	EX AF,AF'
	NEG
	EX AF,AF'
.SET_STP
	LD (IX-12+CHP_TSlStp+1),A
	EX AF,AF'
	LD (IX-12+CHP_TSlStp),A
	LD (IX-12+CHP_COnOff),0
	RET

.C_GLISS
	SET 2,(IX-12+CHP_Flags)
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_TnSlDl),A
	AND A
	JR NZ,GL36
	LD A,(Version) ;AlCo PT3.7+
	CP 7
	SBC A,A
	INC A
.GL36
	LD (IX-12+CHP_TSlCnt),A
	LD A,(BC)
	INC BC
	EX AF,AF'
	LD A,(BC)
	INC BC
	JR SET_STP

.C_SMPOS
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_PsInSm),A
	RET

.C_ORPOS
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_PsInOr),A
	RET

.C_VIBRT
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_OnOffD),A
	LD (IX-12+CHP_COnOff),A
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_OffOnD),A
	XOR A
	LD (IX-12+CHP_TSlCnt),A
	LD (IX-12+CHP_CrTnSl),A
	LD (IX-12+CHP_CrTnSl+1),A
	RET

.C_ENGLS
	LD A,(BC)
	INC BC
	LD (Env_Del),A
	LD (CurEDel),A
	LD A,(BC)
	INC BC
	LD L,A
	LD A,(BC)
	INC BC
	LD H,A
	LD (ESldAdd),HL
	RET

.C_DELAY
	LD A,(BC)
	INC BC
	LD (Delay),A
	RET

.SETENV
	LD (IX-12+CHP_Env_En),E
	LD (AYREGS+EnvTp),A
	LD A,(BC)
	INC BC
	LD H,A
	LD A,(BC)
	INC BC
	LD L,A
	LD (EnvBase),HL
	XOR A
	LD (IX-12+CHP_PsInOr),A
	LD (CurEDel),A
	LD H,A
	LD L,A
	LD (CurESld),HL
.C_NOP
	RET

.SETORN
	ADD A,A
	LD E,A
	LD D,0
	LD (IX-12+CHP_PsInOr),D

DEFC OrnPtrs = ASMPC+1

	LD HL,$2121
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)

DEFC MDADDR2 = ASMPC+1

	LD HL,$2121
	ADD HL,DE
	LD (IX-12+CHP_OrnPtr),L
	LD (IX-12+CHP_OrnPtr+1),H
	RET

.SETSAM
	ADD A,A
	LD E,A
	LD D,0

DEFC  SamPtrs = ASMPC+1

	LD HL,$2121
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)

DEFC MDADDR1 = ASMPC+1

	LD HL,$2121
	ADD HL,DE
	LD (IX-12+CHP_SamPtr),L
	LD (IX-12+CHP_SamPtr+1),H
	RET

;ALL 16 ADDRESSES TO PROTECT FROM BROKEN PT3 MODULES
.SPCCOMS
	DW C_NOP
	DW C_GLISS
	DW C_PORTM
	DW C_SMPOS
	DW C_ORPOS
	DW C_VIBRT
	DW C_NOP
	DW C_NOP
	DW C_ENGLS
	DW C_DELAY
	DW C_NOP
	DW C_NOP
	DW C_NOP
	DW C_NOP
	DW C_NOP
	DW C_NOP

.CHREGS
	XOR A
	LD (Ampl),A
	BIT 0,(IX+CHP_Flags)
	PUSH HL
	JP Z,CH_EXIT
	LD (CSP_+1),SP
	LD L,(IX+CHP_OrnPtr)
	LD H,(IX+CHP_OrnPtr+1)
	LD SP,HL
	POP DE
	LD H,A
	LD A,(IX+CHP_PsInOr)
	LD L,A
	ADD HL,SP
	INC A
		;PT2	PT3
.OrnCP
	INC A	;CP E	CP D
	JR C,CH_ORPS
.OrnLD
	DB 1	;LD A,D	LD A,E
.CH_ORPS
	LD (IX+CHP_PsInOr),A
	LD A,(IX+CHP_Note)
	ADD A,(HL)
	JP P,CH_NTP
	XOR A
.CH_NTP
	CP 96
	JR C,CH_NOK
	LD A,95
.CH_NOK
	ADD A,A
	EX AF,AF'
	LD L,(IX+CHP_SamPtr)
	LD H,(IX+CHP_SamPtr+1)
	LD SP,HL
	POP DE
	LD H,0
	LD A,(IX+CHP_PsInSm)
	LD B,A
	ADD A,A
.SamClc2
	ADD A,A ;or ADD A,B for PT2
	LD L,A
	ADD HL,SP
	LD SP,HL
	LD A,B
	INC A
		;PT2	PT3
.SamCP
	INC A	;CP E	CP D
	JR C,CH_SMPS
.SamLD
	DB 1	;LD A,D	LD A,E
.CH_SMPS
	LD (IX+CHP_PsInSm),A
	POP BC
	POP HL

;Convert PT2 sample to PT3
		;PT2		PT3
.SamCnv
	POP HL  ;BIT 2,C	JR e_
	POP HL
	LD H,B
	JR NZ,$+8
	EX DE,HL
	AND A
	SBC HL,HL
	SBC HL,DE
	LD D,C
	RR C
	SBC A,A
	CPL
	AND $3E
	RR C
	RR B
	AND C
	LD C,A
	LD A,B
	RRA
	RRA
	RR D
	RRA
	AND $9F
	LD B,A

.e_
	LD E,(IX+CHP_TnAcc)
	LD D,(IX+CHP_TnAcc+1)
	ADD HL,DE
	BIT 6,B
	JR Z,CH_NOAC
	LD (IX+CHP_TnAcc),L
	LD (IX+CHP_TnAcc+1),H
.CH_NOAC
	EX DE,HL
	EX AF,AF'
	ADD A,NT_%256
	LD L,A
	ADC A,NT_/256
	SUB L
	LD H,A
	LD SP,HL
	POP HL
	ADD HL,DE
	LD E,(IX+CHP_CrTnSl)
	LD D,(IX+CHP_CrTnSl+1)
	ADD HL,DE
.CSP_
	LD SP,$3131
	EX (SP),HL
	XOR A
	OR (IX+CHP_TSlCnt)
	JR Z,CH_AMP
	DEC (IX+CHP_TSlCnt)
	JR NZ,CH_AMP
	LD A,(IX+CHP_TnSlDl)
	LD (IX+CHP_TSlCnt),A
	LD L,(IX+CHP_TSlStp)
	LD H,(IX+CHP_TSlStp+1)
	LD A,H
	ADD HL,DE
	LD (IX+CHP_CrTnSl),L
	LD (IX+CHP_CrTnSl+1),H
	BIT 2,(IX+CHP_Flags)
	JR NZ,CH_AMP
	LD E,(IX+CHP_TnDelt)
	LD D,(IX+CHP_TnDelt+1)
	AND A
	JR Z,CH_STPP
	EX DE,HL
.CH_STPP
	SBC HL,DE
	JP M,CH_AMP
	LD A,(IX+CHP_SlToNt)
	LD (IX+CHP_Note),A
	XOR A
	LD (IX+CHP_TSlCnt),A
	LD (IX+CHP_CrTnSl),A
	LD (IX+CHP_CrTnSl+1),A
.CH_AMP
	LD A,(IX+CHP_CrAmSl)
	BIT 7,C
	JR Z,CH_NOAM
	BIT 6,C
	JR Z,CH_AMIN
	CP 15
	JR Z,CH_NOAM
	INC A
	JR CH_SVAM
.CH_AMIN
	CP -15
	JR Z,CH_NOAM
	DEC A
.CH_SVAM
	LD (IX+CHP_CrAmSl),A
.CH_NOAM
	LD L,A
	LD A,B
	AND 15
	ADD A,L
	JP P,CH_APOS
	XOR A
.CH_APOS
	CP 16
	JR C,CH_VOL
	LD A,15
.CH_VOL
	OR (IX+CHP_Volume)
	ADD A,VT_%256
	LD L,A
	ADC A,VT_/256
	SUB L
	LD H,A
	LD A,(HL)
.CH_ENV
	BIT 0,C
	JR NZ,CH_NOEN
	OR (IX+CHP_Env_En)
.CH_NOEN
	LD (Ampl),A
	BIT 7,B
	LD A,C
	JR Z,NO_ENSL
	RLA
	RLA
	SRA A
	SRA A
	SRA A
	ADD A,(IX+CHP_CrEnSl) ;SEE COMMENT BELOW
	BIT 5,B
	JR Z,NO_ENAC
	LD (IX+CHP_CrEnSl),A
.NO_ENAC
	LD HL,AddToEn
	ADD A,(HL) ;BUG IN PT3 - NEED WORD HERE
	LD (HL),A
	JR CH_MIX
.NO_ENSL
	RRA
	ADD A,(IX+CHP_CrNsSl)
	LD (AddToNs),A
	BIT 5,B
	JR Z,CH_MIX
	LD (IX+CHP_CrNsSl),A
.CH_MIX
	LD A,B
	RRA
	AND $48
.CH_EXIT
	LD HL,AYREGS+Mixer
	OR (HL)
	RRCA
	LD (HL),A
	POP HL
	XOR A
	OR (IX+CHP_COnOff)
	RET Z
	DEC (IX+CHP_COnOff)
	RET NZ
	XOR (IX+CHP_Flags)
	LD (IX+CHP_Flags),A
	RRA
	LD A,(IX+CHP_OnOffD)
	JR C,CH_ONDL
	LD A,(IX+CHP_OffOnD)
.CH_ONDL
	LD (IX+CHP_COnOff),A
	RET

.PLAY
	XOR A
	LD (AddToEn),A
	LD (AYREGS+Mixer),A
	DEC A
	LD (AYREGS+EnvTp),A
	LD HL,DelyCnt
	DEC (HL)
	JP NZ,PL2
	LD HL,ChanA+CHP_NtSkCn
	DEC (HL)
	JR NZ,PL1B

DEFC AdInPtA = ASMPC+1

	LD BC,$0101
	LD A,(BC)
	AND A
	JR NZ,PL1A
	LD D,A
	LD (Ns_Base),A

DEFC CrPsPtr = ASMPC+1

	LD HL,$2121
	INC HL
	LD A,(HL)
	INC A
	JR NZ,PLNLP

IF LoopChecker
	CALL CHECKLP
ENDIF

DEFC LPosPtr = ASMPC+1

	LD HL,$2121
	LD A,(HL)
	INC A
.PLNLP
	LD (CrPsPtr),HL
	DEC A
		;PT2		PT3
.PsCalc
	DEC A	;ADD A,A	NOP
	DEC A	;ADD A,(HL)	NOP
	ADD A,A
	LD E,A
	RL D

IF CurPosCounter
	LD A,L
.PosSub
	SUB $D6
	LD (CurPos),A
ENDIF

DEFC PatsPtr = ASMPC+1

	LD HL,$2121
	ADD HL,DE

DEFC MODADDR = ASMPC+1

	LD DE,$1111
	LD (PSP_+1),SP
	LD SP,HL
	POP HL
	ADD HL,DE
	LD B,H
	LD C,L
	POP HL
	ADD HL,DE
	LD (AdInPtB),HL
	POP HL
	ADD HL,DE
	LD (AdInPtC),HL
.PSP_
	LD SP,$3131
.PL1A
	LD IX,ChanA+12
	CALL PTDECOD
	LD (AdInPtA),BC

.PL1B
	LD HL,ChanB+CHP_NtSkCn
	DEC (HL)
	JR NZ,PL1C
	LD IX,ChanB+12

DEFC AdInPtB = ASMPC+1

	LD BC,$0101
	CALL PTDECOD
	LD (AdInPtB),BC

.PL1C
	LD HL,ChanC+CHP_NtSkCn
	DEC (HL)
	JR NZ,PL1D
	LD IX,ChanC+12

DEFC AdInPtC = ASMPC+1

	LD BC,$0101
	CALL PTDECOD
	LD (AdInPtC),BC

DEFC Delay = ASMPC+1

.PL1D
	LD A,$3E
	LD (DelyCnt),A

.PL2
	LD IX,ChanA
	LD HL,(AYREGS+TonA)
	CALL CHREGS
	LD (AYREGS+TonA),HL
	LD A,(Ampl)
	LD (AYREGS+AmplA),A
	LD IX,ChanB
	LD HL,(AYREGS+TonB)
	CALL CHREGS
	LD (AYREGS+TonB),HL
	LD A,(Ampl)
	LD (AYREGS+AmplB),A
	LD IX,ChanC
	LD HL,(AYREGS+TonC)
	CALL CHREGS
	LD (AYREGS+TonC),HL

	LD HL,(Ns_Base_AddToNs)
	LD A,H
	ADD A,L
	LD (AYREGS+Noise),A

DEFC AddToEn = ASMPC+1

	LD A,$3E
	LD E,A
	ADD A,A
	SBC A,A
	LD D,A
	LD HL,(EnvBase)
	ADD HL,DE
	LD DE,(CurESld)
	ADD HL,DE
	LD (AYREGS+Env),HL

	XOR A
	LD HL,CurEDel
	OR (HL)
	JR Z,ROUT
	DEC (HL)
	JR NZ,ROUT

DEFC Env_Del = ASMPC+1

	LD A,$3E
	LD (HL),A

DEFC ESldAdd = ASMPC+1

	LD HL,$2121
	ADD HL,DE
	LD (CurESld),HL

.ROUT

IF ACBBAC
	LD A,(SETUP)
	AND 12
	JR Z,ABC
	ADD A,CHTABLE%256
	LD E,A
	ADC A,CHTABLE/256
	SUB E
	LD D,A
	LD B,0
	LD IX,AYREGS
	LD HL,AYREGS
	LD A,(DE)
	INC DE
	LD C,A
	ADD HL,BC
	LD A,(IX+TonB)
	LD C,(HL)
	LD (IX+TonB),C
	LD (HL),A
	INC HL
	LD A,(IX+TonB+1)
	LD C,(HL)
	LD (IX+TonB+1),C
	LD (HL),A
	LD A,(DE)
	INC DE
	LD C,A
	ADD HL,BC
	LD A,(IX+AmplB)
	LD C,(HL)
	LD (IX+AmplB),C
	LD (HL),A
	LD A,(DE)
	INC DE
	LD (RxCA1),A
	XOR 8
	LD (RxCA2),A
	LD HL,AYREGS+Mixer
	LD A,(DE)
	AND (HL)
	LD E,A
	LD A,(HL)
.RxCA1
	LD A,(HL)
	AND %010010
	OR E
	LD E,A
	LD A,(HL)
	AND %010010
.RxCA2
	OR E
	OR E
	LD (HL),A
.ABC
ENDIF

	XOR A
	LD C,ymreg
	LD HL,AYREGS
.LOUT
	OUT (C),A
	LD C,ymdat
	OUTI
	LD C,ymreg
	INC A
	CP 13
	JP NZ,LOUT
	OUT (C),A
	LD A,(HL)
	AND A
	RET M
	LD C,ymdat
	OUT (C),A
	RET

IF ACBBAC
DEFC CHTABLE = ASMPC - 4
	DB 4,5,15,%001001,0,7,7,%100100
ENDIF

.pause				; MM put in a test for character at terminal - RAW CP/M I/O called
	push bc
	push de

	LD BC,$1500		;Loads BC with hex 1500
	; outer: LD DE,$1000	;Loads DE with hex 1000
	; inner: DEC DE		;Decrements DE
	; LD A,D		;Copies D into A
	; OR E			;Bitwise OR of E with A (now, A = D | E)
	; JP NZ,inner		;Jumps back to Inner: label if A is not zero
.outer
	dec bc			;Decrements BC
	ld a,b			;Copies B into A
	or c			;Bitwise OR of C with A (now, A = B | C)
	jp NZ,outer		;Jumps back to Outer: label if A is not zero


	ld c,6			; MM going to check for a console input
	ld e,$ff		; MM tell CP/M we want a character
	call bdos
	cp a,0			; MM 'a' will be zero if nothing there.
	jp NZ,quit		; MM quit if there is a character

	pop de
	pop bc

	ret			;Return from call to this subroutine

.quit				; MM a key was pressed, don't care which, just exit.
				; MM set volume of YM2149 to off before exiting
	ld a,8			; MM the channel a volume register
	out (ymreg),a		; MM select
	ld a,0			; MM and volume zero, repeat for channel B and C.
	out (ymdat),a 
	ld a,9			; MM channel B volume register
	out (ymreg),a
	ld a,0
	out (ymdat),a 
	ld  a,$0A		; MM as per data sheet descript. Ch C volume register is 0A not 10.
	out (ymreg),a
	ld a,0
	out (ymdat),a		; MM should be all quiet now

	ld de,endstr		;end message at this location
	ld c,printf		;print it
	call bdos

	ld c,0			; MM quit to CP/M command prompt
	jp bdos


SECTION rodata_user

.success
	db "file read to memory, will play now",13,10,10,$24
.nofile
	db "no file of that name",13,10,$24
.startupstr
	db "Megabanghra 3000",13,10,10,$24
.loopstr
	db "*",13,10,$24
.endstr
	db "the end",13,10,$24

.NT_DATA
	DB (T_NEW_0-T1_)*2
	DB TCNEW_0-T_
	DB (T_OLD_0-T1_)*2+1
	DB TCOLD_0-T_
	DB (T_NEW_1-T1_)*2+1
	DB TCNEW_1-T_
	DB (T_OLD_1-T1_)*2+1
	DB TCOLD_1-T_
	DB (T_NEW_2-T1_)*2
	DB TCNEW_2-T_
	DB (T_OLD_2-T1_)*2
	DB TCOLD_2-T_
	DB (T_NEW_3-T1_)*2
	DB TCNEW_3-T_
	DB (T_OLD_3-T1_)*2
	DB TCOLD_3-T_

.T_

.TCOLD_0
	DB $00+1,$04+1,$08+1,$0A+1,$0C+1,$0E+1,$12+1,$14+1
	DB $18+1,$24+1,$3C+1,0
.TCOLD_1
	DB $5C+1,0
.TCOLD_2
	DB $30+1,$36+1,$4C+1,$52+1,$5E+1,$70+1,$82,$8C,$9C
	DB $9E,$A0,$A6,$A8,$AA,$AC,$AE,$AE,0
.TCNEW_3
	DB $56+1
.TCOLD_3
	DB $1E+1,$22+1,$24+1,$28+1,$2C+1,$2E+1,$32+1,$BE+1,0
.TCNEW_0
	DB $1C+1,$20+1,$22+1,$26+1,$2A+1,$2C+1,$30+1,$54+1
	DB $BC+1,$BE+1,0

DEFC TCNEW_1 = TCOLD_1

.TCNEW_2
	DB $1A+1,$20+1,$24+1,$28+1,$2A+1,$3A+1,$4C+1,$5E+1
	DB $BA+1,$BC+1,$BE+1,0

DEFC PT3EMPTYORN = ASMPC-1

	DB 1,0

;first 12 values of tone tables (packed)

.T_PACK
	DB +($06EC*2)/256,+($06EC*2)%256
	DB $0755-$06EC
	DB $07C5-$0755
	DB $083B-$07C5
	DB $08B8-$083B
	DB $093D-$08B8
	DB $09CA-$093D
	DB $0A5F-$09CA
	DB $0AFC-$0A5F
	DB $0BA4-$0AFC
	DB $0C55-$0BA4
	DB $0D10-$0C55
	DB +($066D*2)/256,+($066D*2)%256
	DB $06CF-$066D
	DB $0737-$06CF
	DB $07A4-$0737
	DB $0819-$07A4
	DB $0894-$0819
	DB $0917-$0894
	DB $09A1-$0917
	DB $0A33-$09A1
	DB $0ACF-$0A33
	DB $0B73-$0ACF
	DB $0C22-$0B73
	DB $0CDA-$0C22
	DB +($0704*2)/256,+($0704*2)%256
	DB $076E-$0704
	DB $07E0-$076E
	DB $0858-$07E0
	DB $08D6-$0858
	DB $095C-$08D6
	DB $09EC-$095C
	DB $0A82-$09EC
	DB $0B22-$0A82
	DB $0BCC-$0B22
	DB $0C80-$0BCC
	DB $0D3E-$0C80
	DB +($07E0*2)/256,+($07E0*2)%256
	DB $0858-$07E0
	DB $08E0-$0858
	DB $0960-$08E0
	DB $09F0-$0960
	DB $0A88-$09F0
	DB $0B28-$0A88
	DB $0BD8-$0B28
	DB $0C80-$0BD8
	DB $0D60-$0C80
	DB $0E10-$0D60
	DB $0EF8-$0E10

SECTION data_user

;vars from here can be stripped
;you can move VARS to any other address

.VARS

;ChannelsVars

DEFC CHP	= 29

DEFC CHP_PsInOr	= 0
DEFC CHP_PsInSm	= 1
DEFC CHP_CrAmSl	= 2
DEFC CHP_CrNsSl	= 3
DEFC CHP_CrEnSl	= 4
DEFC CHP_TSlCnt	= 5
DEFC CHP_CrTnSl	= 6
DEFC CHP_TnAcc	= 8
DEFC CHP_COnOff	= 10
DEFC CHP_OnOffD	= 11

;IX for PTDECOD here (+12)
DEFC CHP_OffOnD	= 12
DEFC CHP_OrnPtr	= 13
DEFC CHP_SamPtr	= 15
DEFC CHP_NNtSkp	= 17
DEFC CHP_Note	= 18
DEFC CHP_SlToNt	= 19
DEFC CHP_Env_En	= 20
DEFC CHP_Flags	= 21
 ;Enabled - 0,SimpleGliss - 2
DEFC CHP_TnSlDl	= 22
DEFC CHP_TSlStp	= 23
DEFC CHP_TnDelt	= 25
DEFC CHP_NtSkCn	= 27
DEFC CHP_Volume	= 28

.ChanA
	DS CHP
.ChanB
	DS CHP
.ChanC
	DS CHP

;GlobalVars

.next_dest
	DW MDLADDR
.oldstack
	DW $0000
.DelyCnt
	DB 0
.CurESld
	DW 0
.CurEDel
	DB 0
.Ns_Base_AddToNs
.Ns_Base
	DB 0
.AddToNs
	DB 0

.AYREGS

.VT_
	DS 256			;CreatedVolumeTableAddress

DEFC EnvBase	= VT_+14

DEFC T1_	= VT_+16	;Tone tables data depacked here

DEFC T_OLD_1	= T1_
DEFC T_OLD_2	= T_OLD_1+24
DEFC T_OLD_3	= T_OLD_2+24
DEFC T_OLD_0	= T_OLD_3+2
DEFC T_NEW_0	= T_OLD_0
DEFC T_NEW_1	= T_OLD_1
DEFC T_NEW_2	= T_NEW_0+24
DEFC T_NEW_3	= T_OLD_3

DEFC PT2EMPTYORN = VT_+31	;1,0,0 sequence

.NT_
	DS 192			;CreatedNoteTableAddress

;local var
DEFC Ampl	= AYREGS+AmplC

DEFC VAR0END	= VT_+16	;INIT zeroes from VARS to VAR0END-1

DEFC VARSEND	= ASMPC

	ds $40		;z80 stack grows down
.stack

DEFC MDLADDR	= ASMPC

;Release 0 steps:
;02/27/2005
;Merging PT2 and PT3 players; debug
;02/28/2005
;debug; optimization
;03/01/2005
;Migration to SjASM; conditional assembly (ZX, MSX and
;visualization)
;03/03/2005
;SETPORT subprogram (35 bytes shorter)
;03/05/2005
;fixed CurPosCounter error
;03/06/2005
;Added ACB and BAC channels swapper (for Spectre); more cond.
;assembly keys; optimization
;Release 1 steps:
;04/15/2005
;Removed loop bit resetting for no loop build (5 bytes shorter)
;04/30/2007
;New 1.xx and 2.xx interpretation for PT 3.7+.

;Tests in IMMATION TESTER V1.0 by Andy Man/POS
;(for minimal build)
;Module name/author	Min tacts	Max tacts
;PT3 (a little slower than standalone player)
;Spleen/Nik-O		1720		9368
;Chuta/Miguel		1720		9656
;Zhara/Macros		4536		8792
;PT2 (more slower than standalone player)
;Epilogue/Nik-O		3928		10232
;NY tHEMEs/zHenYa	3848		9208
;GUEST 4/Alex Job	2824		9352
;KickDB/Fatal Snipe	1720		9880

;Size (build for CP/M):
;Code $0840 bytes
;RO Data $00E7 bytes
;Variables $0261 bytes (can be stripped)
;Size in RAM $8F5+$088+$21D=$B88 (2952) bytes

;Notes:
;Pro Tracker 3.4r can not be detected by header, so PT3.4r tone
;tables realy used only for modules of 3.3 and older versions.
