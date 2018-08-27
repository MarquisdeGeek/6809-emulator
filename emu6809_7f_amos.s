;	
;	6809E Emulator - Copyright 1994-6 - Steven Goodwin
;
;			Version 0.01
;
;	Latest edit: Sunday 1st June 1997
;
; This version has different condition code checking
; Lines 1027, onwards
;
V_68000	equ	0	; Use special 68000 move sr instruction

amos_mem	equ	3005528

;use_boot		; Sets PC to boot up code of Dragon
;use_amos   		; allows use of AMOS memory, and parameters
use_sbase		; use stack base. Should be left on.
;
; Input:
;	A0	Memory block:	0-65535		6809e's virtual memory
;				65536+		Internal data:
;				: +0		PC register
;				: +4		S register (stack pointer)
;				: +8		U register (user stack ptr)
;				: +12		Pseudo stack base

;				: +16 		Screen ram, bit plane 0
;				: +20		Bit plane #1
;				: +24		Bit plane #2
;				: +28		Bit plane #3
;				: +32		Data bank (undefined)
;				: +36		1 = Ignore illegal op-codes
;						0 = Return error
;				:
;	D1	CC (condition codes) reg.
;	D2	A reg
;	D3	B reg. Note: D is built in real-time, as required
;	D4	X reg
;	D5	Y reg
;	D6	DP (direct page) reg
;	D7	0 = Execute code
;		1 = Single Step
;		2 = Execute with breakpoints
;
;
; Processing
;	a1	S stack ptr
;	a2	U stack ptr
;	a3 	Pointer to start of dragon's memory

;	
; FIRST SET OF COMMON INSTRUCTIONS
;
d_neg	macro		; *** NEG - Negate ***
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A,
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension

	\1	a0,d0,\4\5,get8	; macro call may be null
	putcc
	neg.b	\3
	getcc	
	\2	d7,d0,\4\5	; return data to appropiate location
	bra	next
	endm

d_com	macro		; *** COM - Complement all bits ***
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A,
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension

	\1	a0,d0,\4\5,get8	; macro call may be null
	putcc
	not.b	\3
	getcc	
	\2	d7,d0,\4\5	; return data to appropiate location
	bset	#0,d1		; set carry flag
	bra	next
	endm

d_lsr	macro		; *** LSR - Logical Shift Right ***
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A,
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension
	btst	#1,d1		; check CC reg to check V (overflow)
	bne	nc_lsr\4	; V bit is set
	\1	a0,d0,\4\5,get8	; macro call may be null
	putcc
	lsr.b	\3
	getcc	
	\2	d7,d0,\4\5	; return data to appropiate location
	bra	next
nc_lsr\4:
	\1	a0,d0,\4\6,get8	; macro call may be null
	putcc
	lsr.b	\3
	getcc
	\2	d7,d0,\4\6	; return data to appropiate location
	bra	next
	setv			; set V bit as 68K wipes it
	endm


d_ror	macro		; *** ROR - Rotate to Right ***
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A,
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension
	btst	#1,d1
	bne	nc_ror\4	; V bit set
	\1	a0,d0,\4\5,get8	; macro call may be null
	putcc
	ror.b	\3
	getcc	
	\2	d7,d0,\4\5	; return data to appropiate location
	bra	next
nc_ror\4:
	\1	a0,d0,\4\6,get8	; macro call may be null
	putcc
	ror.b	\3
	getcc
	\2	d7,d0,\4\6	; return data to appropiate location
	bra	next
	setv
	endm


d_asr	macro		; *** ASR - Arithmetic Shift Right (retains sign)
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A,
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension
	btst	#1,d1
	bne	nc_asr\4	; V bit set
	\1	a0,d0,\4\5,get8	; macro call may be null
	putcc
	asr.b	\3
	getcc	
	\2	d7,d0,\4\5	; return data to appropiate location
	bra	next
nc_asr\4:
	\1	a0,d0,\4\6,get8	; macro call may be null
	putcc
	asr.b	\3
	getcc
	bset	#1,d1
	\2	d7,d0,\4\6	; return data to appropiate location
	bra	next
	endm


d_lsl	macro		; *** LSL - Logical Shift Left ***
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A,
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension
	\1	a0,d0,\4\5,get8	; macro call may be null
	putcc
	lsl.b	\3
	getcc	
	\2	d7,d0,\4\5	; return data to appropiate location
	bra	next
	endm


d_rol	macro		; *** ROL - Rotate Left ***
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A,
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension

	\1	a0,d0,\4\5,get8	; macro call may be null
	putcc
	rol.b	\3
	getcc	
	\2	d7,d0,\4\5	; return data to appropiate location
	bra	next
	endm


d_dec	macro		; *** DEC - Decrement by 1 ***
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A,
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension
	btst	#0,d1
	bne	nc_dec\4	; carry bit set
	\1	a0,d0,\4\5,get8	; macro call may be null
	putcc
	subq.b	#1,\3
	getcc	
	\2	d7,d0,\4\5	; return data to appropiate location
	bra	next
nc_dec\4:
	\1	a0,d0,\4\6,get8	; macro call may be null
	putcc
	subq.b	#1,\3
	getcc
	bset	#0,d1
	\2	d7,d0,\4\6	; return data to appropiate location
	bra	next
	endm

d_inc	macro		; *** INC - Increment by 1 ***
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A, d3 for b etc)
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension
	btst	#0,d1
	bne	nc_inc\4	; carry bit set
	\1	a0,d0,\4\5,get8	; macro call may be null
	putcc
	addq.b	#1,\3
	getcc	
	\2	d7,d0,\4\5	; return data to appropiate location
	bra	next
nc_inc\4:
	\1	a0,d0,\4\6,get8	; macro call may be null
	putcc
	addq.b	#1,\3
	getcc
	bset	#0,d1
	\2	d7,d0,\4\6	; return data to appropiate location
	bra	next
	endm


d_tst	macro		; *** TST - Test status of ... ***
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A,
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension
	btst	#0,d1
	bne	nc_tst\4	; Carry bit set
	\1	a0,d0,\4\5,get8	; macro call may be null
	putcc
	tst.b	\3
	getcc	
	\2	d7,d0,\4\5	; return data to appropiate location
	bra	next
nc_tst\4:
	\1	a0,d0,\4\6,get8	; macro call may be null
	putcc
	tst.b	\3
	getcc	
	\2	d7,d0,\4\6	; return data to appropiate location
	setc
	bra	next
	endm

d_jmp	macro		; *** JMP - Jump to memory location ***
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A,
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension
	\1	a0,d0,\4\5,get8	; macro call may be null
	move.l	\3,a0
	endm
	
d_clr	macro		; *** CLR - Reset to zero ***
			; \1 = macro to find value from memory
			; \2 = macro to return value to memory
			; \3 = register to perform operation on (d2 for A,
			;	and d0 for memory recalled)
			; \4 = Main label
			; \5 = Label extension
	\1	a0,d0,\4\5,get8	; macro call may be null
	moveq	#0,\3
	move.b	#4,d1		; moveq ignores the 'b' perogative, thus
				; wiping MY status bits at 31-28
	\2	d7,d0,\4\5
	bra	next
	endm
;		
; SECOND SET OF COMMON INSTRUCTIONS
;

splitd	macro			; *** SPLITD - place hi/lo bytes into A & B
				; \1 = Source register
	move.b	\1,d3		; place lower byte into B
	move.w	\1,d2		; place whole thing into A, 
	lsr.w	#8,d2		; ...and shift it 8 places to the left
	endm
	
joind	macro			; *** JOIND - produce the complete reg D
				; \1 = desination reg for D
	move.b	d2,\1		; move A into reg
	lsl.w	#8,\1		; shift A along 8 places and...
	move.b	d3,\1		; ...move B into lower byte
	endm
	
d_sub	macro		; *** SUB - Subtract ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d2 for A,
			;	or d3 for B, are the only possibilities)
			; \3 = Main label

	\1	a0,d0,\3,get8
	putcc
	sub.b	d0,\2
	getcc
	bra	next
	endm

	
d_cmp	macro		; *** CMP - Compare ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d2 for A,
			;	or d3 for B, are the only possibilities)
			; \3 = Main label

	\1	a0,d0,\3,get8
	putcc
	cmp.b	d0,\2
	getcc
	bra	next
	endm


d_sbc	macro		; *** SBC - Subtract With Carry ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d2 for A,
			;	or d3 for B, are the only possibilities)
			; \3 = Main label

	\1	a0,d0,\3,get8
	btst	#0,d1		;if the C bit is set
	beq	ncsbc\3
	addq.b	#1,d0		; then carry causes an extra +1 to be SUB'ed
ncsbc\3	putcc
	sub.b	d0,\2
	getcc
	bra	next
	endm

	
d_subd	macro		; *** SUB - Subtract ***
			; \1 = macro to find value from memory
			; \2 = not used
			; \3 = Main label

	\1	a0,d0,\3,null	; the null prevents any action taking
				; place within the index macro, if called

	IFNC	'\1','get16inc'		; if Imm addr, the data's already in d0
	get16	d7,d0		; get data into d0
	ENDC
	
	joind	d7		; get pseudo D-reg into d7
	putcc
	sub.w	d0,d7		; do it!
	getcc
	splitd	d7		; put the data back into A and B
	bra	next
	endm


d_and	macro		; *** AND - Logical And ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d2 for A,
			;	or d3 for B, are the only possibilities)
			; \3 = Main label


	\1	a0,d0,\3,get8	;get D0 to = value to AND
	btst	#0,d1
	bne	nc_andr\3	; carry bit set
	putcc
	and.b	d0,\2
	getcc
	bra	next	
nc_andr\3:
	putcc
	and.b	d0,\2
	getcc
	setc
	bra	next
	endm

d_bit	macro		; *** BIT - Logical AND on memory. ONLY CC CHANGES ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d2 for A,
			;	or d3 for B, are the only possibilities)
			; \3 = Main label


	\1	a0,d0,\3,get8	;get D0 to = value to use
	btst	#0,d1
	bne	nc_bit\3	; carry bit set
	putcc
	and.b	\2,d0		; contents of mem and R are ANDed together
	getcc			; and the flags set - nothing more!
	bra	next	
nc_bit\3:
	putcc
	and.b	\2,d0
	getcc
	setc
	bra	next
	endm


d_ldr	macro		; *** LDx - Load register ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d2 for A,
			;	or d3 for B, are the only possibilities)
			; \3 = Main label
;	illegal
	\1	a0,d0,\3,get8	;get D0 to = value to use
	btst	#0,d1
	bne	nc_ldr\3	; carry bit set
	putcc
	move.b	d0,\2
	getcc
	bra	next	
nc_ldr\3:
	putcc
	move.b	d0,\2
	getcc
	setc
	bra	next
	endm


d_str	macro		; *** STx - Store register in ... ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d2 for A,
			;	or d3 for B, are the only possibilities)
			; \3 = Main label

	\1	a0,d0,\3,null	; gets address into D7	
	put8  	d7,\2,\3
	adjz	\2,\3,w
	adjn	\2,\3,15
	clrv
	bra	next	
	endm


d_eor	macro		; *** EOR - Exclusive OR ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d2 for A,
			;	or d3 for B, are the only possibilities)
			; \3 = Main label

	\1	a0,d0,\3,get8	;get D0 to = value to use
	btst	#0,d1
	bne	nc_eor\3	; carry bit set
	putcc
	eor.b	d0,\2
	getcc
	bra	next	
nc_eor\3:
	putcc
	eor.b	d0,\2
	getcc
	setc
	bra	next
	endm


d_adc	macro		; *** ADC - Add with Carry ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d2 for A,
			;	or d3 for B, are the only possibilities)
			; \3 = Main label

	\1	a0,d0,\3,get8
	btst	#0,d1
	beq	no_caradc\3		; there is no carry bit to add
	addq.b	#1,d0			
no_caradc\3:
	putcc
	add.b	d0,\2
	getcc
	bra	next
	endm


d_or	macro		; *** OR - Logical OR ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d2 for A,
			;	or d3 for B, are the only possibilities)
			; \3 = Main label

	\1	a0,d0,\3,get8	;get D0 to = value to or
	btst	#0,d1
	bne	nc_orr\3	; carry bit set
	putcc
	or.b	d0,\2
	getcc
	bra	next	
nc_orr\3:
	putcc
	or.b	d0,\2
	getcc
	setc
	bra	next
	endm

d_add	macro		; *** ADD - Add (good eh??) ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d2 for A,
			;	or d3 for B, are the only possibilities)
			; \3 = Main label

	\1	a0,d0,\3,get8
	putcc
	add.b	d0,\2
	getcc
	bra	next
	endm


d_cmp16	macro		; *** CMP - Compare a 16 bit value ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d4 for X,
			;	or d5 for Y, etc)
			; \3 = Main label

	\1	a0,d0,\3,get16
	putcc
	cmp.w	d0,\2
	getcc
	bra	next
	endm
	
	
d_jsr	macro			; *** JSR - Jump into a subroutine ***
				; \1 = macro to get address
				; \2 = register of new addr (d0 for ext,
				; 	d7 for index and d6 for DP)
				; \3 = d0 in all cases except PAGE (=d6)
				; \4 = number of bytes to jump over before
				; 	storing the value on the stack

	move.l	a0,d0		;push don't work with An, so a copy is made
	addq.w	#\4,d0		; increase duplicate PC in d0 now...
	push	a1,d0		; ... and store it on the 'S' stack
	\1	a0,\3,_,null	; get new address into d0 amd THEN copy
	move.l	\2,a0		; into a0 - stops corruption of a0
	bra next		; must be .l to stop 2's c affecting no.
	endm

a_ldr16	macro		; *** LDx - Load 16 bit register, S or U ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (a1 for S,
			;	or a2 for U, are the only possibilities)
			; \3 = Main label
			; ------ A separate routine required because
			; 	when the register is in an address register
			;	(because move.l ...,An  doesn't set flags

	\1	a0,d0,\3,get16	;get D0 to = value to use
	btst	#0,d1
	bne	nca_ldr16\3	; carry bit set
	putcc
	tst.w	d0
	getcc
	move.l	d0,\2
	bra	next	
nca_ldr16\3:
	putcc
	tst.w	d0
	getcc
	setc
	move.l	d0,\2
	bra	next
	endm
	
	
d_ldr16	macro		; *** LDx - Load 16 bit register, X or Y ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d0 for D, d4 for X,
			;	or d5 for Y, are the only possibilities)
			; \3 = Main label


	\1	a0,d0,\3,get16	;get D0 to = value to use
	btst	#0,d1
	bne	nc_ldr16\3	; carry bit set
	putcc
	move.l	d0,\2
	getcc
	IFC	'd0','\2'
	splitd	d0
	ENDC
	bra	next	
nc_ldr16\3:
	putcc
	move.l	d0,\2
	getcc
	setc
	IFC	'd0','\2'
	splitd	d0
	ENDC
	bra	next
	endm

d_str16	macro		; *** STx - Store 16 bit register ***
			; \1 = macro to find value from memory
			; \2 = register to perform operation on (d4 for X,
			;	or d5 for Y etc)
			; \3 = Main label

	IFC	'd0','\2'
	joind	d0
	ENDC
	
	\1	a0,d0,\3,null	;get D0 to = value to use
	move.w	\2,d0		; make d0 temp store for reg...
	
	adjz	d0,\3,w
	adjn	d0,\3,15
	put16	\4,d0,\3	; ...'cause put16 destroy contents and
				; flag set macros wont work with An
	clrv
	bra	next
	endm


d_addd	macro		; *** ADDD - Adds to D reg ***
			; \1 = macro to find value from memory
			; \2 = not used
			; \3 = Main label

   IFC	'\1','getdps'
	\1	a0,d0,\3,get16	; the null prevents any action taking
				; place within the index macro, if called
	ELSEIF
	\1 a0,d0,\3, null
	get16	d7,d0		; get data into d0
	ENDC
	
	joind	d7		; get pseudo D-reg into d7
	putcc
	add.w	d0,d7		; do it!
	getcc
	splitd	d7		; put the data back into A and B
	bra	next
	endm
	
;
;  ADDRESSING MODE MACROS - as used in the above instances
;


extend	macro			; *** extended mode ***
	get16inc \1,d7		; Get address from 'program space' into d7
	IFC  	'\4','get16'
	get16	d7,\2
	ELSEIF
	IFC	'\2','d4'
	get16	d7,\2
	ELSEIF
	IFC	'\2','d5'
	get16	d7,\2
	ELSEIF
	IFC	'\2','a2'
	get16	d7,\2
	ELSEIF
	IFC	'\2','a3'
	get16	d7,\2
	ELSEIF
	get8	d7,\2		; Get data from that address
	ENDC
	ENDC
	ENDC
	ENDC
	ENDC
	
	endm
	
getdps	macro			; *** Direct Page ***
	move.w	d6,d7		; Copy DP into d7
	asl.w	#8,d7		; shift by 8 so it works as a page
	add.b	(a3,\1.l),d7	; Load lower byte with offset from memory

	\4	d7,\2		; \4 = get16 for example, \2 register for result
	; this is used instead of the convential..
	;	move.b	(a3,d7.l),\2	; Get data from DP addr
	;.. for when we need 16 bit data
	incpc
	endm	
		
dobranch macro			; *** BRx Branches ***
	get8sinc a0,d0
	add.w	d0,a0
	btst	#7,d0		; is it a -ve branch?? 
	beq	next		; No - we're finished here
	sub.w	#$100,a0	; Yes - subtract from PC
	bra	next
	endm

longbranch macro		; *** LBRx branches ***
	get16inc  a0,d0
	add.l	d0,a0		
	bra	next
	endm

null	macro			; *** usually used for immediate addressing
	endm			;     modes where no actually macro is used ***
	
_lea	macro
	move.l	\1,\2
	endm

stack	macro			; stack - permits ALL the push/pull ops
				; that involve THE PB
	get8inc	a0,d0
	dostack	\1,\2,\3,\4,\5,\6,\7,\8,\9
				; pass all 'parameters' through...
	bra	next		; ...then quit
	endm
	
dostack macro			; *** DOSTACK - peforms stack ops ***
				; \1 = operations: push/pull
				; \2 = stack to use (a1 = S, a2 = U)
				; \3 = the other stack
				; \4 = label
				; \5 \6 and \7 are for the real cc conversion
				; and so are \8 and \9
				; d0 = details of stuff to stack
	btst	#0,d0
	beq	pnc\4
	
	\8	\5,\6,\7	; real_cc - if pushing (else null)
	\1_8	\2,d1
	\9	\5,\6,\7	; real_cc conversion if POPing (else null)
pnc\4	btst	#1,d0
	beq	pna\4
	\1_8	\2,d2		; A
pna\4 	btst	#2,d0
	beq	pnb\4
	\1_8	\2,d3		; B
pnb\4	btst	#3,d0
	beq	pnd\4
	asr.w	#8,d6
	\1_8	\2,d6
	asl.w	#8,d6		; DP
pnd\4	btst	#4,d0
	beq	pnx\4
	\1d	\2,d4		; X - pushD is used (stop X reg corrupting)
pnx\4	btst	#5,d0
	beq	pny\4
	\1d	\2,d5		; Y - pushD again
pny\4	btst	#6,d0
	beq	pns\4		; S (or U) depending on which stack is in
				; use at this time (reason for \3)
	\1d	\2,\3		; d is used again
pns\4	btst	#7,d0		
	beq	ppp\4
	\1d	\2,a0		; PC
ppp\4
	endm

in_sub	macro
	bsr	index_subroutine
	endm
	
index	macro
	in_sub
	index_code \1,\2,\3,\4
	endm
		
			
index_code	macro	
	\4	d7,\2,\3	; should be either get8 or put8 - but put involves
			; different code. SOLVE THIS
	endm

; --------------------------------------------------------------

getdp	macro
	move.b	(a3,\1.l),d6
	move.l	(a3,d6.l),\2	; \2 = actual AMIGA address to use
	endm
	
; INDEX addressing, a few macros to help me...
indirect macro			; indirect - read data from addr
	get16		d7,d0		; input: d7 = addr,
	move.w	d0,d7		; output: d7 = indirect addr at old d7 location
	endm

addoff	macro			;ADDOFF - add the following X bit offset
				; to the value already in D7
				; \1 = number of bits: 8/16
				; \2 = .and in chars.  b/w
	get\1inc a0,d0
	add.\2	d0,d7
	endm


getdpinc macro
	getdp
	incpc
	endm
	
incpc	macro
	addq.w	#1,a0
	endm
	
incpc2	macro	
	addq.w	#2,a0
	endm
	
adjz	macro
	setz
	cmp.\3	#0,\1
	beq	resetz\2
	clrz
resetz\2:
	endm
	
adjn	macro
	setn
	btst	#\3,\1
	bne	resetn\2
	clrn
resetn\2:
	endm

clrc	macro
	bclr	#0,d1
	endm
setc	macro
	bset	#0,d1
	endm
clrv	macro
	bclr	#1,d1	
	endm
setv	macro
	bset	#1,d1
	endm
clrz	macro
	bclr	#2,d1
	endm
setz	macro
	bset	#2,d1
	endm	
clrn	macro
	bclr	#3,d1
	endm
setn	macro
	bset	#3,d1
	endm
clrh	macro
	bclr	#4,d1
	endm
seth	macro
	bset	#4,d1
	endm	
clrf	macro
	bclr	#30,d1
	endm
setf	macro
	bset	#30,d1
	endm

	; --------------------------------------------------------------

pop_8	macro			; *** pop8 - d0=value on top of stack
				; \1 = Stack to remove from (a1=S, a2=u)
	move.b	(a3,\1.l),\2
	addq.w	#1,\1
	endm
	
push_8	macro			; *** push8 - place reg on stack
	subq.w	#1,\1		; \1 = Stack to place onto (a1=S, a2=U)
	move.b	\2,(a3,\1.l)	; \2 = Register to push
	endm
	
pop	macro			; *** pop - d0 = 16 bit value from stack
				; \1 = Stack to remove from (a1=S, a2=u)
				; \2 = location of data
	move.b	(a3,\1.l),\2	; load hi byte into d0
	asl.w	#8,\2		; shift into msb position
	add.b 	1(a3,\1.l),\2	; add low byte
	addq.w	#2,\1
	endm

popd	macro			
	pop	\1,d7
	move.l	d7,\2
	endm
	
push	macro			; *** push - place 16 bit reg on stack
	subq.w	#2,\1
	move.b	\2,1(a3,\1.l)	; \1 = Stack to place onto (a1=S, a2=U)
	lsr.w 	#8,\2		; \2 = Reg to push on stack
	move.b	\2,(a3,\1.l)
	endm
	
pushd	macro
	move.w	\2,d7
	push	\1,d7
	endm
	
get16:	macro			; *** get16 - get a 16 bit value into reg
				; \1 = register holding memory location
				; \2 = destination of word
	move.b	(a3,\1.l),\2		; can't move.w because it might not
	asl.w	#8,\2						; be word aligned
	move.b	1(a3,\1.l),\2
	endm

get16inc macro			; *** as get16, but also increments the PC
	get16	\1,\2
	incpc2
	endm

get8:	macro			; *** get8 - get an 8 bit val into reg
				; \1 = register holding memory location
				; \2 = destination register	
	move.b	(a3,\1.l),\2	; the .l stops addresses >$7fff being 
				; taken as -ve
	endm
		
get8s:	macro			; *** get8s - special version of above:
				;     Guarantees register is empty
				; \1 = register holding memory location
				; \2 = destination register	
	moveq	#0,\2		
	move.b	(a3,\1.l),\2
	endm

get8sinc macro
	get8s	\1,\2
	incpc
	endm
	
get8inc:	macro
	get8 \1,\2
	incpc
	endm

put16	macro			
	addq	#1,\1		;YUK - I add 1 here, and subtract it later!
	put8	\1,\2,\3_a
	lsr.w 	#8,\2
	subq	#1,\1		; move pointer onto next byte
	put8	\1,\2,\3_b
	endm	
	
put16inc	macro
	put16 \1,\2,\3
	addq	#2,\1
	endm
	
put8	macro
	cmp.w	#$f000,\1
	bhs	poke\3
	cmp.w	#$8000,\1
	bhs	rom\3
	cmp.w	#$600,\1
	bhs	poke\3
	cmp.w	#$400,\1
	blo	poke\3
	move.l	\2,d0
	bsr	draw_on_text_screen
poke\3:
	move.b	\2,(a3,\1.l)
rom\3:
	endm

put8inc	macro
	put8 \1 \2
	addq 	#1,\1
	endm

putcc	macro
	move	d1,ccr
	endm

 IFD 	V_68000		
getcc	macro	
	move	sr,d1
	endm
 ELSE
getcc	macro
	movem.l	a0/d0,-(sp)
	movea.l	4.w,a0
	jsr	-528(a0)	; _LVOGetCC
	move.w	d0,d1
	move	d0,ccr
	movem.l	(sp)+,a0/d0
	endm
 ENDC
;
;

d_brn	macro			; *** BRN - Branch Never macro ***
				; \1 = incpc or incpc2 command
	\1
	bra	next
	endm

d_bhi	macro			; BHI	 - if C=Z>>>>> !C & !Z = !(C+Z)
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	moveq	#5,d0
	and.b	d1,d0
	beq	dobhibr\2
	
	;cmp.b	#5,d0
	;beq	dobhibr\2	
	\1
	bra	next
dobhibr\2
	\3branch
	endm

d_bls	macro			; BLS, if C != Z>>> C+Z
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	move.b	#%00000101,d0	; mask off C and V flags
	and.b	d1,d0
	beq	dontbls\2
;	cmp.b	#5,d0
;	beq	dontbls\2
	\3branch
	bra	next
dontbls\2
	\1
	bra	next
	endm
	
d_bcc	macro			; BCC, C=0
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	btst	#0,d1
	beq	dobcc\2
	\1
	bra	next
dobcc\2
	\3branch
	endm

d_bcs	macro			; BCS, C=1
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	btst	#0,d1
	bne	dobcs\2
	\1
	bra	next
dobcs\2
	\3branch
	endm
	
d_bne	macro			; BNE Branch - if Z = 0
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	btst	#2,d1
	beq	dobnebr\2
	\1
	bra	next
dobnebr\2
	\3branch
	endm
	
d_beq	macro			; BEQ- if Z=1
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	btst	#2,d1
	bne	dobeqbr\2
	\1
	bra	next
dobeqbr\2
	\3branch
	endm
	
d_bvc	macro			; BVC - branch if V = 0
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	btst	#1,d1
	beq	dobvc\2
	\1
	bra	next
dobvc\2
	\3branch
	endm
	
d_bvs	macro			; BVS - if v=1
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	btst	#1,d1
	bne	dobvs\2
	\1
	bra	next
dobvs\2
	\3branch
	endm
	
d_bpl	macro			; BPL, if N=0
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	btst	#3,d1
	beq	dobpl\2
	\1
	bra	next
dobpl\2
	\3branch
	endm
	
d_bmi	macro			; BMI, if N=1
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	btst	#3,d1
	bne	dobmi\2
	\1
	bra	next
dobmi\2
	\3branch
	endm
	
d_bge	macro			; BGE, 	if N=V (if N=0 or N=1)
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	move.b	#%00001010,d0	; mask off N and V flags
	and.b	d1,d0
	beq	dobge\2		; Zero flag is set, both are = (to 0)
	cmp.b	#%00001010,d0
	beq	dobge\2
	\1
	bra	next
dobge\2
	\3branch
	endm
	
d_blt	macro			; BLT, if N != V
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	move.b	#%00001010,d0	; mask off N and V flags
	and.b	d1,d0
	bne	doblt\2
	cmp.b	#%00001010,d0
	bne	doblt\2
	\1
	bra	next
doblt\2
	\3branch		; ...we CAN do it
	endm

d_bgt	macro			; BGT, if Z=0 and (V=N)
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	btst	#2,d1
	beq	do_ngt\2		; Z=0, so use BGE macro to check rest
	bra	next
do_ngt\2
	d_bge	\1,_\2,\3	; '_' added to stop label confusion
	endm
	
d_ble	macro			; BLE, if Z=1 or (V != N)
				; \1 = incpc or incpc2 command
				; \2 = label extension (l for long)
				; \3 = 'do' or 'long' 
	btst	#2,d1
	beq	nle\2		; Z=0, so BLE=(V!=N) : pass control to blt
	d_blt	\1,_\2,\3
nle\2
	\3branch
	endm	
	
;
;
real_cc	macro			; REALCC - convert between my CC and the
				; one used in the 6809
				; \1 = Mask for copy. 
				; \2 = Mask for current CC (in create real
				;	CC this will ignore the pseudo-flags)
				; \3 = Instructions (lsl/r)
	movem.l	d0/d7,-(a7)
	move.l	d1,d7		; copy CC
	andi.l	#\1,d7		; Isolate MY flags
	
	andi.l	#\2,d1		; Isolate the real flags
	
	move.l	#24,d0
	\3.l	d0,d7
	or.l	d7,d1		; join them up
	
	movem.l	(a7)+,d0/d7
	endm
	
exit	macro
exit\1
	real_cc	$f0000000,$f000000f,lsr
	bsr	store_regs
	move.l	#\1,d0
	movem.l	(a7)+,a3-a6
	rts
	endm
	
; start of code proper...
; ------------------------------------------------------
;	
	movem.l	a3-a6,-(a7)	; stack all AMOS stuff before we start

;	illegal
		
;	real_cc	$f0000000,$f000000f,lsr		;make it real
;	real_cc	$f0,$f,lsl			;back to my system!
	
	;
	tst.b d7
	beq 	execute
	subq.b #1,d7
	beq 	step_code		
	subq.b	#1,d7
	beq	breakpoints
	subq.b	#1,d7
	beq	getbreaks
	
breakpoints:

	bsr 	init_regs

	lea	next(pc),a5		; THIS IS DANGEROUS TO USE
	move.l	#$0c39000c,(a5)		; SO DONT TRY THIS AT HOME
	move.l	a5,4(a5)		; CHILDREN!!!! :-) I know - COPY & PASTE

	addq.l	#8,a5
	move.l	#(check_breaks-tbs)-2,d7
	or.l	#$67000000,d7		; This re-routes the 'beq' line to
	move.l	d7,(a5)			; to branch to the break checking
					; routine
	bsr	get_break_counts
	
	bra 	instr
	
step_code:
	bsr 	init_regs
	;illegal
	
	lea	next(pc),a5		; THIS IS DANGEROUS TO USE
	move.l	#$0c39000c,(a5)		; SO DONT TRY THIS AT HOME
	move.l	a5,4(a5)		; CHILDREN!!!! :-)
					; This re-programs the code to jump
					; out of the instruction loop after
					; one instruction by comparing
					; part of its own instruction
	bra	instr
		
execute:
	bsr	init_regs
					; A blast from the past.....
	lea	next(pc),a5		; SELF MODIFYING CODE!!!!!!!
	move.l	#$c390041,(a5)		; there are instructions:
	move.l	#$bfec01,4(a5)		; cmpi.b #$41,$bfec01
	
	addq.l	#8,a5
	move.l	#(_quit_emu-tbs)-2,d0	; MORE DANGER CODE. This changes
	or.l	#$67000000,d0		; the 'beq _quit_emu' to, er,  
	move.l	d0,(a5)			; well - 'beq _quit_emu'. Just to
					; make sure its correct!
	moveq	#0,d0
	
next:
run_code:
	cmp.b	#65,$bfec01
tbs	beq	_quit_emu
 
;	illegal
instr	
	get8s	a0,d0
	incpc
	;
	asl.w	#2,d0
	jmp	branch_table(pc,d0)
	;
branch_table:
      bra _00
      bra _illegal
      bra _illegal
      bra _03
      bra _04
      bra _illegal
      bra _06
      bra _07
      bra _08
      bra _09
      bra _0A
      bra _illegal
      bra _0C
      bra _0D
      bra _0E
      bra _0F
      bra _10
      bra _11
      bra _12
      bra _13
      bra _illegal
      bra _illegal
      bra _16
      bra _17
      bra _illegal
      bra _19
      bra _1A
      bra _illegal
      bra _1C
      bra _1D
      bra _1E
      bra _1F
      bra _20
      bra _21
      bra _22
      bra _23
      bra _24
      bra _25
      bra _26
      bra _27
      bra _28
      bra _29
      bra _2A
      bra _2B
      bra _2C
      bra _2D
      bra _2E
      bra _2F
      bra _30
      bra _31
      bra _32
      bra _33
      bra _34
      bra _35
      bra _36
      bra _37
      bra _illegal
      bra _39
      bra _3A
      bra _3B
      bra _3C
      bra _3D
      bra _3E
      bra _3F
      bra _40
      bra _illegal
      bra _illegal
      bra _43
      bra _44
      bra _illegal
      bra _46
      bra _47
      bra _48
      bra _49
      bra _4A
      bra _illegal
      bra _4C
      bra _4D
      bra _illegal
      bra _4F
      bra _50
      bra _illegal
      bra _illegal
      bra _53
      bra _54
      bra _illegal
      bra _56
      bra _57
      bra _58
      bra _59
      bra _5A
      bra _illegal
      bra _5C
      bra _5D
      bra _illegal
      bra _5F
      bra _60
      bra _illegal
      bra _illegal
      bra _63
      bra _64
      bra _illegal
      bra _66
      bra _67
      bra _68
      bra _69
      bra _6A
      bra _illegal
      bra _6C
      bra _6D
      bra _6E
      bra _6F
      bra _70
      bra _illegal
      bra _illegal
      bra _73
      bra _74
      bra _illegal
      bra _76
      bra _77
      bra _78
      bra _79
      bra _7A
      bra _illegal
      bra _7C
      bra _7D
      bra _7E
      bra _7F
      bra _80
      bra _81
      bra _82
      bra _83
      bra _84
      bra _85
      bra _86
      bra _illegal
      bra _88
      bra _89
      bra _8A
      bra _8B
      bra _8C
      bra _8D
      bra _8E
      bra _illegal
      bra _90
      bra _91
      bra _92
      bra _93
      bra _94
      bra _95
      bra _96
      bra _97
      bra _98
      bra _99
      bra _9A
      bra _9B
      bra _9C
      bra _9D
      bra _9E
      bra _9F
      bra _A0
      bra _A1
      bra _A2
      bra _A3
      bra _A4
      bra _A5
      bra _A6
      bra _A7
      bra _A8
      bra _A9
      bra _AA
      bra _AB
      bra _AC
      bra _AD
      bra _AE
      bra _AF
      bra _B0
      bra _B1
      bra _B2
      bra _B3
      bra _B4
      bra _B5
      bra _B6
      bra _B7
      bra _B8
      bra _B9
      bra _BA
      bra _BB
      bra _BC
      bra _BD
      bra _BE
      bra _BF
      bra _C0
      bra _C1
      bra _C2
      bra _C3
      bra _C4
      bra _C5
      bra _C6
      bra _illegal
      bra _C8
      bra _C9
      bra _CA
      bra _CB
      bra _CC
      bra _illegal
      bra _CE
      bra _illegal
      bra _D0
      bra _D1
      bra _D2
      bra _D3
      bra _D4
      bra _D5
      bra _D6
      bra _D7
      bra _D8
      bra _D9
      bra _DA
      bra _DB
      bra _DC
      bra _DD
      bra _DE
      bra _DF
      bra _E0
      bra _E1
      bra _E2
      bra _E3
      bra _E4
      bra _E5
      bra _E6
      bra _E7
      bra _E8
      bra _E9
      bra _EA
      bra _EB
      bra _EC
      bra _ED
      bra _EE
      bra _EF
      bra _F0
      bra _F1
      bra _F2
      bra _F3
      bra _F4
      bra _F5
      bra _F6
      bra _F7
      bra _F8
      bra _F9
      bra _FA
      bra _FB
      bra _FC
      bra _FD
      bra _FE
      bra _FF

init_regs 
	move.l	#$10000,d0		; needed 'cause can't use nos.
	
	IFD	use_amos
	move.l	#amos_mem,a0		; TEMPORARY
	ENDC
	
	move.l	a0,a3			; >$1fff as offsets
	move.l	(a3,d0.l),a0		; PC
	
	IFD	use_boot
	move.l	#$b3ba,a0		; TEMP
	ENDC
	
	move.l	$4(a3,d0.l),a1		; S stack pointer
	move.l	$8(a3,d0.l),a2		; U stack pointer
	move.l	$C(a3,d0.l),a4		; stack base. The recoil limit of
					; the 'S' stack.
	lea	$24(a3,d0.l),a5
	lea	_illstore(pc),a6
	move.l	(a5),(a6)
	
	andi.l	#$ff,d2
	andi.l	#$ff,d3
	rts

store_regs
	move.l	#$10000,d0		; needed 'cause can't use nos.
	move.l	a0,(a3,d0.l)		; PC
	move.l	a1,4(a3,d0.l)		; S stack pointer
	move.l	a2,8(a3,d0.l)		; U stack pointer
	move.l	a3,a0			; put ptr to start of memory back
	andi.l	#$ff,d2			; mask rubbish that creeps in A - HOW??
	rts
	

draw_on_text_screen:
	; ITS ON THE TEXT SCREEN!!!!!
	movem.l	d7/a0-a1,-(a7)	; find the physbase
	;
	move.l	d0,-(a7)
	move.w	d7,d0		; hold D7 in D0 for the moment..
	and.l	#$001f,d7	; calculate the physical screen offset
	move.l	d7,a5
	move.w	d0,d7		; ...get d7 back from d0
	and.l	#$01e0,d7
	mulu	#12,d7		; CHANGE  Y co-ordinate
	add.l	d7,a5
	;
	move.l	#$10000,d0
	lea	(a3,d0.l),a6
	move.l	$20(a6),a1	; a1 = start of font start
	lea	$10(a6),a0	; a0 = location of start of AMOS text screen
	;
	move.l	(a7)+,d0
	andi.l	#$ff,d0		; This shouldn't really be needed? But 
				; some instances don't clear the d0
				; registers, causing the MULU to exceed
				; the font bank. YUK ERROR
	;
	mulu	#48,d0		; calculate the text font bank position
	add.l	d0,a1
	;
	move.l	#4,d0 
	;
next_bit_plane
	move.l	(a0),a6
	add.l	a5,a6
	move.b	(a1),(a6)
	move.b	1(a1),32(a6)
	move.b	2(a1),64(a6)
	move.b	3(a1),96(a6)
	move.b	4(a1),128(a6)
	move.b	5(a1),160(a6)
	move.b	6(a1),192(a6)
	move.b	7(a1),224(a6)
	move.b	8(a1),256(a6)
	move.b	9(a1),288(a6)
	move.b	10(a1),320(a6)
	move.b	11(a1),352(a6)

	add.l	#12,a1
	addq.l	#4,a0
	;
	subq	#1,d0
	bne 	next_bit_plane
	;
	movem.l	(a7)+,d7/a0-a1

;	move.l	a0,d0
;	andi.l	#$ffff,d0		; the 'i' pads with ZEROS
;	move.l	d0,a0
	
	rts

get_break_counts
	move.l	#$10028,d7		; needed 'cause can't use nos.
	
	lea	brk1(pc),a5
	
	moveq	#4,d0
gbcl	
	lea	(a3,d7.l),a6		; get count for breakpoint
	move.l	a6,6(a5)		; store it...
	move.l	a6,26(a5)		; ..in both places

	add.l	#38,a5			; move onto next bp routine
	add.l	#4,a4			; and next count location
	
	dbeq	d0,gbcl
	rts

	
_illegal
	move.l	_illstore(pc),d0
	bne	next			; ignore illegal op-codes
	exit 10
	
_illstore
	dc.l	0	
	
_quit_emu
	exit 	5

		
index_subroutine:
	move.l	d0,-(a7)	; save d0 - we'll be using it a lot in here!
	
	get8s	a0,d7	
	incpc
	
	btst	#7,d7
	bne	not5bit
	move.l	d7,d0
	and.b	#%00011111,d7	; isolate offset in d7
	btst	#4,d7
	beq	notneg		; is this -ve, if so adjust d7
	or.w	#$fff0,d7	; allows us to add.w and retain -ve status
notneg
	and.b	#%01100000,d0	;isolate the register
	tst.w	d0		; Is it X
	bne	not_X
	add.w	d4,d7
	move.l	(a7)+,d0
	rts
not_X
	cmp.w	#$20,d0		; Y?
	bne	not_Y
	add.w	d5,d7
	move.l	(a7)+,d0
	rts
not_Y
	cmp.w	#$40,d0		; U?
	bne	not_U
	add.w	a2,d7
	move.l	(a7)+,d0
	rts
not_U
	add.w	a1,d7
	move.l	(a7)+,d0
	rts
;
	
getbreaks
	lea	break_data(pc),a0	; pass the memory locations of the
	lea	instr(pc),a1		; jump locs back to AMOS, for POKEing
	lea	brk1(pc),a2			; this gets start of bp exits
	movem.l	(a7)+,a3-a6
	rts

;
not5bit				; indexed addressing for ,r+ ,r++ b,r  a,r  ,-r  ,--r
	bclr	#7,d7
	;
	asl	#2,d7
	jmp	index_table(pc,d7.l)
	
index_table
     bra    index_00
     bra    index_01
     bra    index_02
     bra    index_03
     bra    index_04
     bra    index_05
     bra    index_06
     bra    illin
     bra    index_08
     bra    index_09
     bra    illin
     bra    index_0B
     bra    index_0C
     bra    index_0D
     bra    illin
     bra    illin
     bra    illin
     bra    index_11
     bra    illin
     bra    index_13
     bra    index_14
     bra    index_15
     bra    index_16
     bra    illin
     bra    index_18
     bra    index_19
     bra    illin
     bra    index_1B
     bra    index_1C
     bra    index_1D
     bra    illin
     bra    index_1F
     bra    index_20
     bra    index_21
     bra    index_22
     bra    index_23
     bra    index_24
     bra    index_25
     bra    index_26
     bra    illin
     bra    index_28
     bra    index_29
     bra    illin
     bra    index_2B
     bra    index_0C
     bra    index_0D
     bra    illin
     bra    illin
     bra    illin
     bra    index_31
     bra    illin
     bra    index_33
     bra    index_34
     bra    index_35
     bra    index_36
     bra    illin
     bra    index_38
     bra    index_39
     bra    illin
     bra    index_3B
     bra    index_1C
     bra    index_1D
     bra    illin
     bra    illin
     bra    index_40
     bra    index_41
     bra    index_42
     bra    index_43
     bra    index_44
     bra    index_45
     bra    index_46
     bra    illin
     bra    index_48
     bra    index_49
     bra    illin
     bra    index_4B
     bra    index_0C
     bra    index_0D
     bra    illin
     bra    illin
     bra    illin
     bra    index_51
     bra    illin
     bra    index_53
     bra    index_54
     bra    index_55
     bra    index_56
     bra    illin
     bra    index_58
     bra    index_59
     bra    illin
     bra    index_5B
     bra    index_1C
     bra    index_1D
     bra    illin
     bra    illin
     bra    index_60
     bra    index_61
     bra    index_62
     bra    index_63
     bra    index_64
     bra    index_65
     bra    index_66
     bra    illin
     bra    index_68
     bra    index_69
     bra    illin
     bra    index_6B
     bra    index_0C
     bra    index_0D
     bra    illin
     bra    illin
     bra    illin
     bra    index_71
     bra    illin
     bra    index_73
     bra    index_74
     bra    index_75
     bra    index_76
     bra    illin
     bra    index_78
     bra    index_79
     bra    illin
     bra    index_7B
     bra    index_1C
     bra    index_1D
     bra    illin
     bra    illin
     
index_00
	move.w	d4,d7			; ,x+
	addq.w	#1,d4
            move.l   (a7)+,d0
            rts
index_01				
	move.w	d4,d7			; ,x++
	addq.w	#2,d4
            move.l   (a7)+,d0
            rts
index_02
	subq.w	#1,d4			; ,-x
 	move.w	d4,d7
           move.l   (a7)+,d0
            rts
index_03
	subq.w	#2,d4			; ,--x
 	move.w	d4,d7
           move.l   (a7)+,d0
            rts
index_04
	move.w	d4,d7			; ,x
            move.l   (a7)+,d0
            rts
index_05
	move.w	d4,d7			; b,x
	add.w	d3,d4
            move.l   (a7)+,d0
            rts
index_06
   	move.w	d4,d7			; a,x
	add.w	d2,d4
          move.l   (a7)+,d0
            rts
index_08
	move.w	d4,d7			; 8 bit signed offset ,x
	addoff	8,b
            move.l   (a7)+,d0
            rts
index_09
	move.w	d4,d7			; 16 bit signed offset ,x
	addoff	16,w
            move.l   (a7)+,d0
            rts
index_0B
	joind	d0			; d,x
	move.w	d4,d7
	add.w	d0,d7
            move.l   (a7)+,d0
            rts

index_2C		; the same instruction because bits are unused
index_4C
index_6C
index_0C
	move.w	a0,d7			; 8 bit signed offset,pc
	addoff	8,b
	addq.w	#1,d7		; needed 'cause PC is wrong when added
            move.l   (a7)+,d0
            rts
index_0D
index_2D
index_4D
index_6D
  	move.w	a0,d7		; 16 bit signed offset,pc
	addoff	16,w
	addq.w	#2,d7		; needed 'cause PC is wrong when added
          move.l   (a7)+,d0
            rts
index_11
	move.w	d4,d7		; [,x++]
	addq.w	#2,d4
        indirect
            move.l   (a7)+,d0
            rts
index_13
	subq.w	#2,d4		; [,--x]
	move.w	d4,d7
	indirect
            move.l   (a7)+,d0
            rts
index_14
	move.w	d4,d7		; [,x]
	indirect
            move.l   (a7)+,d0
            rts
index_15
    	move.w	d4,d7		; [b,x]
	add.w	d3,d4
	indirect
        move.l   (a7)+,d0
            rts
index_16
    	move.w	d4,d7		; [a,x]
	add.w	d2,d4
	indirect
        move.l   (a7)+,d0
            rts
index_18
	move.w	d4,d7		; [ 8, x]
	addoff	8,b
	indirect
            move.l   (a7)+,d0
            rts
index_19
 	move.w	d4,d7		; [8, x]
	addoff	16,w
	indirect
           move.l   (a7)+,d0
            rts
index_1B
	joind	d0		; [d,x]
	move.w	d4,d7
	add.w	d0,d7
	indirect
            move.l   (a7)+,d0
            rts
index_1C
index_3C
index_5C
index_7C
	move.w	a0,d7		; [8,pc]
	addoff	8,b
	addq.w	#1,d7		; needed 'cause PC is wrong when added
	indirect
            move.l   (a7)+,d0
            rts
index_1D
index_3D
index_5D
index_7D
  	move.w	a0,d7		;[16,pc]
	addoff	16,w
	addq.w	#2,d7		; needed 'cause PC is wrong when added
	indirect
            move.l   (a7)+,d0
            rts
           
index_1F			; [addr] - special case
	get16inc a0,d7
	indirect
 			; NOW THE Y REGISTER - Y - Y - Y
index_20
	move.w	d5,d7		; ,y+
	addq.w	#1,d5
            move.l   (a7)+,d0
            rts
index_21
	move.w	d5,d7		; ,y++
	addq.w	#2,d5
            move.l   (a7)+,d0
            rts
index_22
	subq.w	#1,d5		; ,-y
 	move.w	d5,d7
           move.l   (a7)+,d0
            rts
index_23
	subq.w	#2,d5		; ,--y
 	move.w	d5,d7
           move.l   (a7)+,d0
            rts
index_24
	move.w	d5,d7		; ,y
            move.l   (a7)+,d0
            rts
index_25
	move.w	d5,d7			; b,y
	add.w	d3,d7
            move.l   (a7)+,d0
            rts
index_26
   	move.w	d5,d7		; a,y
	add.w	d2,d7
          move.l   (a7)+,d0
            rts
index_28
	move.w	d5,d7		; 8,y
	addoff	8,b
            move.l   (a7)+,d0
            rts
index_29
	move.w	d5,d7		; 16,y
	addoff	16,w
            move.l   (a7)+,d0
            rts
index_2B
	joind	d0		; d,y
	move.w	d5,d7
	add.w	d0,d7
            move.l   (a7)+,d0
            rts
index_31
	move.w	d5,d7		; [,y++]
	addq.w	#2,d5
        indirect
            move.l   (a7)+,d0
            rts
index_33
	subq.w	#2,d5		; [,--y]
	move.w	d5,d7
	indirect
            move.l   (a7)+,d0
            rts
index_34
	move.w	d5,d7		; [y]
	indirect
            move.l   (a7)+,d0
            rts
index_35
    	move.w	d5,d7		; [b,y]
	add.w	d3,d7
	indirect
        move.l   (a7)+,d0
            rts
index_36
    	move.w	d5,d7		; [a,y]
	add.w	d2,d7
	indirect
        move.l   (a7)+,d0
            rts
index_38
	move.w	d5,d7		; [8,y]
	addoff	8,b
	indirect
            move.l   (a7)+,d0
            rts
index_39
 	move.w	d5,d7		; [16,y]
	addoff	16,w
	indirect
           move.l   (a7)+,d0
            rts
index_3B
	joind	d0		; [d,y]
	move.w	d5,d7
	add.w	d0,d7
	indirect
            move.l   (a7)+,d0
            rts
					; U - U - U - U - U - U - U
index_40
	move.w	a2,d7		; ,u+
	addq.w	#1,a2
            move.l   (a7)+,d0
            rts
index_41
	move.w	a2,d7		; ,u++		
	addq.w	#2,a2
            move.l   (a7)+,d0
            rts
index_42
	subq.w	#1,a2		; ,-u
 	move.w	a2,d7
           move.l   (a7)+,d0
            rts
index_43
	subq.w	#2,a2		; ,--u
 	move.w	a2,d7
           move.l   (a7)+,d0
            rts
index_44
	move.w	a2,d7		; ,u
            move.l   (a7)+,d0
            rts
index_45
	move.w	a2,d7		; b,u
	add.w	d3,d7
            move.l   (a7)+,d0
            rts
index_46
   	move.w	a2,d7		; a,u
	add.w	d2,d7
          move.l   (a7)+,d0
            rts
index_48
	move.w	a2,d7		; 8,u
	addoff	8,b
            move.l   (a7)+,d0
            rts
index_49
	move.w	a2,d7		; 16,u
	addoff	16,w
            move.l   (a7)+,d0
            rts
index_4B
	joind	d0		; d,u
	move.w	a2,d7
	add.w	d0,d7
            move.l   (a7)+,d0
            rts
index_51
	move.w	a2,d7		; [,u++]
	addq.w	#2,a2
        indirect
            move.l   (a7)+,d0
            rts
index_53
	subq.w	#2,a2		; [,--u]
	move.w	a2,d7
	indirect
            move.l   (a7)+,d0
            rts
index_54
	move.w	a2,d7		; [u]
	indirect
            move.l   (a7)+,d0
            rts
index_55
    	move.w	a2,d7		; [b,u]
	add.w	d3,d7
	indirect
        move.l   (a7)+,d0
            rts
index_56
    	move.w	a2,d7		; [a,u]
	add.w	d2,d7
	indirect
        move.l   (a7)+,d0
            rts
index_58
	move.w	a2,d7		; [8,u]
	addoff	8,b
	indirect
            move.l   (a7)+,d0
            rts
index_59
 	move.w	a2,d7		; [16,u]
	addoff	16,w
	indirect
           move.l   (a7)+,d0
            rts
index_5B 
	joind	d0		; [d,u]
	move.w	a2,d7
	add.w	d0,d7
	indirect
            move.l   (a7)+,d0
            rts
            				; S - S - S - S - S - S
index_60
	move.w	a1,d7		; ,s+
	addq.w	#1,a1
            move.l   (a7)+,d0
            rts
index_61
	move.w	a1,d7		; ,s++
	addq.w	#2,a1
            move.l   (a7)+,d0
            rts
index_62
	subq.w	#1,a1		; ,-s
 	move.w	a1,d7
           move.l   (a7)+,d0
            rts
index_63
	subq.w	#2,a1		; ,--s
 	move.w	a1,d7
           move.l   (a7)+,d0
            rts
index_64
	move.w	a1,d7		; ,s
            move.l   (a7)+,d0
            rts
index_65
	move.w	a1,d7		; b,s
	add.w	d3,d7
	move.l   (a7)+,d0
            rts
index_66
   	move.w	a1,d7		; a,s
	add.w	d2,d7
          move.l   (a7)+,d0
            rts
index_68
	move.w	a1,d7		; 8,s
	addoff	8,b
            move.l   (a7)+,d0
            rts
index_69
	move.w	a1,d7		; 16,s
	addoff	16,w
            move.l   (a7)+,d0
            rts
index_6B
	joind	d0		; d,s
	move.w	a1,d7
	add.w	d0,d7
            move.l   (a7)+,d0
            rts
index_71
	move.w	a1,d7		; [,s++]
	addq.w	#2,a1
        indirect
            move.l   (a7)+,d0
            rts
index_73
	subq.w	#2,a1		; [,--s]
	move.w	a1,d7
	indirect
            move.l   (a7)+,d0
            rts
index_74
	move.w	a1,d7		; [s]
	indirect
            move.l   (a7)+,d0
            rts
index_75
    	move.w	a1,d7		; [b,s]
	add.w	d3,d7
	indirect
        move.l   (a7)+,d0
            rts
index_76
    	move.w	a1,d7		; [a,s]
	add.w	d2,d7
	indirect
        move.l   (a7)+,d0
            rts
index_78
	move.w	a1,d7		; [8,s]
	addoff	8,b
	indirect
            move.l   (a7)+,d0
            rts
index_79
 	move.w	a1,d7		; [16,s]
	addoff	16,w
	indirect
           move.l   (a7)+,d0
            rts
index_7B
	joind	d0		; [d,s]
	move.w	a1,d7
	add.w	d0,d7
	indirect
            move.l   (a7)+,d0
            rts

illin
	; Illegal Index instruction found
	; problem - this is a subroutine, therefore we can't quit cause
	; the stack's still got our r/a on it. Is it safe to remove it?
	move.l	(a7)+,d0
	rts
	
; -----------------------------------------------------------
	
_1F:
	get8s	a0,d7	
	incpc
	asl.w	#2,d7
	jmp	tfr_branchs(pc,d7)

tfr_branchs:
      bra    _taeT_00
      bra    _taeT_01
      bra    _taeT_02
      bra    _taeT_03
      bra    _taeT_04
      bra    _taeT_05
     bra   illtae
     bra   illtae
      bra    _taeT_08
      bra    _taeT_09
      bra    _taeT_0A
      bra    _taeT_0B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeT_10
      bra    _taeT_11
      bra    _taeT_12
      bra    _taeT_13
      bra    _taeT_14
      bra    _taeT_15
     bra   illtae
     bra   illtae
      bra    _taeT_18
      bra    _taeT_19
      bra    _taeT_1A
      bra    _taeT_1B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeT_20
      bra    _taeT_21
      bra    _taeT_22
      bra    _taeT_23
      bra    _taeT_24
      bra    _taeT_25
     bra   illtae
     bra   illtae
      bra    _taeT_28
      bra    _taeT_29
      bra    _taeT_2A
      bra    _taeT_2B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeT_30
      bra    _taeT_31
      bra    _taeT_32
      bra    _taeT_33
      bra    _taeT_34
      bra    _taeT_35
     bra   illtae
     bra   illtae
      bra    _taeT_38
      bra    _taeT_39
      bra    _taeT_3A
      bra    _taeT_3B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeT_40
      bra    _taeT_41
      bra    _taeT_42
      bra    _taeT_43
      bra    _taeT_44
      bra    _taeT_45
     bra   illtae
     bra   illtae
      bra    _taeT_48
      bra    _taeT_49
      bra    _taeT_4A
      bra    _taeT_4B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeT_50
      bra    _taeT_51
      bra    _taeT_52
      bra    _taeT_53
      bra    _taeT_54
      bra    _taeT_55
     bra   illtae
     bra   illtae
      bra    _taeT_58
      bra    _taeT_59
      bra    _taeT_5A
      bra    _taeT_5B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeT_80
      bra    _taeT_81
      bra    _taeT_82
      bra    _taeT_83
      bra    _taeT_84
      bra    _taeT_85
     bra   illtae
     bra   illtae
      bra    _taeT_88
      bra    _taeT_89
      bra    _taeT_8A
      bra    _taeT_8B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeT_90
      bra    _taeT_91
      bra    _taeT_92
      bra    _taeT_93
      bra    _taeT_94
      bra    _taeT_95
     bra   illtae
     bra   illtae
      bra    _taeT_98
      bra    _taeT_99
      bra    _taeT_9A
      bra    _taeT_9B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeT_A0
      bra    _taeT_A1
      bra    _taeT_A2
      bra    _taeT_A3
      bra    _taeT_A4
      bra    _taeT_A5
     bra   illtae
     bra   illtae
      bra    _taeT_A8
      bra    _taeT_A9
      bra    _taeT_AA
      bra    _taeT_AB
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeT_B0
      bra    _taeT_B1
      bra    _taeT_B2
      bra    _taeT_B3
      bra    _taeT_B4
      bra    _taeT_B5
     bra   illtae
     bra   illtae
      bra    _taeT_B8
      bra    _taeT_B9
      bra    _taeT_BA
      bra    _taeT_BB
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae

_taeT_00:
       joind d0
       move.l d0,d0
       splitd d0
        bra next
_taeT_01:
       joind d0
       move.l d0,d4
        bra next
_taeT_02:
       joind d0
       move.l d0,d5
        bra next
_taeT_03:
       joind d0
       move.l d0,a2
        bra next
_taeT_04:
       joind d0
       move.l d0,a1
        bra next
_taeT_05:
       joind d0
       move.l d0,a0
        bra next
_taeT_08:
       joind d0
       move.l d0,d2
        bra next
_taeT_09:
       joind d0
       move.l d0,d3
        bra next
_taeT_0A:
       joind d0
       real_cc $f0000000,$f000000f,lsr
       move.l d0,d1
       real_cc $f0,$f,lsl
        bra next
_taeT_0B:
       joind d0
       lsr #8,d6
       move.l d0,d6
       lsl #8,d6
        bra next
_taeT_10:
       move.l d4,d0
       splitd d0
        bra next
_taeT_11:
       move.l d4,d4
        bra next
_taeT_12:
       move.l d4,d5
        bra next
_taeT_13:
       move.l d4,a2
        bra next
_taeT_14:
       move.l d4,a1
        bra next
_taeT_15:
       move.l d4,a0
        bra next
_taeT_18:
       move.l d4,d2
        bra next
_taeT_19:
       move.l d4,d3
        bra next
_taeT_1A:
       real_cc $f0000000,$f000000f,lsr
       move.l d4,d1
       real_cc $f0,$f,lsl
        bra next
_taeT_1B:
       lsr #8,d6
       move.l d4,d6
       lsl #8,d6
        bra next
_taeT_20:
       move.l d5,d0
       splitd d0
        bra next
_taeT_21:
       move.l d5,d4
        bra next
_taeT_22:
       move.l d5,d5
        bra next
_taeT_23:
       move.l d5,a2
        bra next
_taeT_24:
       move.l d5,a1
        bra next
_taeT_25:
       move.l d5,a0
        bra next
_taeT_28:
       move.l d5,d2
        bra next
_taeT_29:
       move.l d5,d3
        bra next
_taeT_2A:
       real_cc $f0000000,$f000000f,lsr
       move.l d5,d1
       real_cc $f0,$f,lsl
        bra next
_taeT_2B:
       lsr #8,d6
       move.l d5,d6
       lsl #8,d6
        bra next
_taeT_30:
       move.l a2,d0
       splitd d0
        bra next
_taeT_31:
       move.l a2,d4
        bra next
_taeT_32:
       move.l a2,d5
        bra next
_taeT_33:
       move.l a2,a2
        bra next
_taeT_34:
       move.l a2,a1
        bra next
_taeT_35:
       move.l a2,a0
        bra next
_taeT_38:
       move.l a2,d2
        bra next
_taeT_39:
       move.l a2,d3
        bra next
_taeT_3A:
       real_cc $f0000000,$f000000f,lsr
       move.l a2,d1
       real_cc $f0,$f,lsl
        bra next
_taeT_3B:
       lsr #8,d6
       move.l a2,d6
       lsl #8,d6
        bra next
_taeT_40:
       move.l a1,d0
       splitd d0
        bra next
_taeT_41:
       move.l a1,d4
        bra next
_taeT_42:
       move.l a1,d5
        bra next
_taeT_43:
       move.l a1,a2
        bra next
_taeT_44:
       move.l a1,a1
        bra next
_taeT_45:
       move.l a1,a0
        bra next
_taeT_48:
       move.l a1,d2
        bra next
_taeT_49:
       move.l a1,d3
        bra next
_taeT_4A:
       real_cc $f0000000,$f000000f,lsr
       move.l a1,d1
       real_cc $f0,$f,lsl
        bra next
_taeT_4B:
       lsr #8,d6
       move.l a1,d6
       lsl #8,d6
        bra next
_taeT_50:
       move.l a0,d0
       splitd d0
        bra next
_taeT_51:
       move.l a0,d4
        bra next
_taeT_52:
       move.l a0,d5
        bra next
_taeT_53:
       move.l a0,a2
        bra next
_taeT_54:
       move.l a0,a1
        bra next
_taeT_55:
       move.l a0,a0
        bra next
_taeT_58:
       move.l a0,d2
        bra next
_taeT_59:
       move.l a0,d3
        bra next
_taeT_5A:
       real_cc $f0000000,$f000000f,lsr
       move.l a0,d1
       real_cc $f0,$f,lsl
        bra next
_taeT_5B:
       lsr #8,d6
       move.l a0,d6
       lsl #8,d6
        bra next
_taeT_80:
       move.l d2,d0
       splitd d0
        bra next
_taeT_81:
       move.l d2,d4
        bra next
_taeT_82:
       move.l d2,d5
        bra next
_taeT_83:
       move.l d2,a2
        bra next
_taeT_84:
       move.l d2,a1
        bra next
_taeT_85:
       move.l d2,a0
        bra next
_taeT_88:
       move.l d2,d2
        bra next
_taeT_89:
       move.l d2,d3
        bra next
_taeT_8A:
       real_cc $f0000000,$f000000f,lsr
       move.l d2,d1
       real_cc $f0,$f,lsl
        bra next
_taeT_8B:
       lsr #8,d6
       move.l d2,d6
       lsl #8,d6
        bra next
_taeT_90:
       move.l d3,d0
       splitd d0
        bra next
_taeT_91:
       move.l d3,d4
        bra next
_taeT_92:
       move.l d3,d5
        bra next
_taeT_93:
       move.l d3,a2
        bra next
_taeT_94:
       move.l d3,a1
        bra next
_taeT_95:
       move.l d3,a0
        bra next
_taeT_98:
       move.l d3,d2
        bra next
_taeT_99:
       move.l d3,d3
        bra next
_taeT_9A:
       real_cc $f0000000,$f000000f,lsr
       move.l d3,d1
       real_cc $f0,$f,lsl
        bra next
_taeT_9B:
       lsr #8,d6
       move.l d3,d6
       lsl #8,d6
        bra next
_taeT_A0:
       real_cc $f0000000,$f000000f,lsr
       move.l d1,d0
       real_cc $f0,$f,lsl
       splitd d0
        bra next
_taeT_A1:
       real_cc $f0000000,$f000000f,lsr
       move.l d1,d4
       real_cc $f0,$f,lsl
        bra next
_taeT_A2:
       real_cc $f0000000,$f000000f,lsr
       move.l d1,d5
       real_cc $f0,$f,lsl
        bra next
_taeT_A3:
       real_cc $f0000000,$f000000f,lsr
       move.l d1,a2
       real_cc $f0,$f,lsl
        bra next
_taeT_A4:
       real_cc $f0000000,$f000000f,lsr
       move.l d1,a1
       real_cc $f0,$f,lsl
        bra next
_taeT_A5:
       real_cc $f0000000,$f000000f,lsr
       move.l d1,a0
       real_cc $f0,$f,lsl
        bra next
_taeT_A8:
       real_cc $f0000000,$f000000f,lsr
       move.l d1,d2
       real_cc $f0,$f,lsl
        bra next
_taeT_A9:
       real_cc $f0000000,$f000000f,lsr
       move.l d1,d3
       real_cc $f0,$f,lsl
        bra next
_taeT_AA:
       real_cc $f0000000,$f000000f,lsr
       move.l d1,d1
       real_cc $f0,$f,lsl
        bra next
_taeT_AB:
       lsr #8,d6
       real_cc $f0000000,$f000000f,lsr
       move.l d1,d6
       real_cc $f0,$f,lsl
       lsl #8,d6
        bra next
_taeT_B0:
       lsr #8,d6
       move.l d6,d0
       lsl #8,d6
       splitd d0
        bra next
_taeT_B1:
       lsr #8,d6
       move.l d6,d4
       lsl #8,d6
        bra next
_taeT_B2:
       lsr #8,d6
       move.l d6,d5
       lsl #8,d6
        bra next
_taeT_B3:
       lsr #8,d6
       move.l d6,a2
       lsl #8,d6
        bra next
_taeT_B4:
       lsr #8,d6
       move.l d6,a1
       lsl #8,d6
        bra next
_taeT_B5:
       lsr #8,d6
       move.l d6,a0
       lsl #8,d6
        bra next
_taeT_B8:
       lsr #8,d6
       move.l d6,d2
       lsl #8,d6
        bra next
_taeT_B9:
       lsr #8,d6
       move.l d6,d3
       lsl #8,d6
        bra next
_taeT_BA:
       lsr #8,d6
       real_cc $f0000000,$f000000f,lsr
       move.l d6,d1
       real_cc $f0,$f,lsl
       lsl #8,d6
        bra next
_taeT_BB:
       lsr #8,d6
       move.l d6,d6
       lsl #8,d6
        bra next
      
_1E:
	get8s	a0,d7	
	incpc
	asl.w	#2,d7
	jmp	exg_branchs(pc,d7)

exg_branchs:
      bra    _taeE_00
      bra    _taeE_01
      bra    _taeE_02
      bra    _taeE_03
      bra    _taeE_04
      bra    _taeE_05
     bra   illtae
     bra   illtae
      bra    _taeE_08
      bra    _taeE_09
      bra    _taeE_0A
      bra    _taeE_0B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeE_10
      bra    _taeE_11
      bra    _taeE_12
      bra    _taeE_13
      bra    _taeE_14
      bra    _taeE_15
     bra   illtae
     bra   illtae
      bra    _taeE_18
      bra    _taeE_19
      bra    _taeE_1A
      bra    _taeE_1B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeE_20
      bra    _taeE_21
      bra    _taeE_22
      bra    _taeE_23
      bra    _taeE_24
      bra    _taeE_25
     bra   illtae
     bra   illtae
      bra    _taeE_28
      bra    _taeE_29
      bra    _taeE_2A
      bra    _taeE_2B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeE_30
      bra    _taeE_31
      bra    _taeE_32
      bra    _taeE_33
      bra    _taeE_34
      bra    _taeE_35
     bra   illtae
     bra   illtae
      bra    _taeE_38
      bra    _taeE_39
      bra    _taeE_3A
      bra    _taeE_3B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeE_40
      bra    _taeE_41
      bra    _taeE_42
      bra    _taeE_43
      bra    _taeE_44
      bra    _taeE_45
     bra   illtae
     bra   illtae
      bra    _taeE_48
      bra    _taeE_49
      bra    _taeE_4A
      bra    _taeE_4B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeE_50
      bra    _taeE_51
      bra    _taeE_52
      bra    _taeE_53
      bra    _taeE_54
      bra    _taeE_55
     bra   illtae
     bra   illtae
      bra    _taeE_58
      bra    _taeE_59
      bra    _taeE_5A
      bra    _taeE_5B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeE_80
      bra    _taeE_81
      bra    _taeE_82
      bra    _taeE_83
      bra    _taeE_84
      bra    _taeE_85
     bra   illtae
     bra   illtae
      bra    _taeE_88
      bra    _taeE_89
      bra    _taeE_8A
      bra    _taeE_8B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeE_90
      bra    _taeE_91
      bra    _taeE_92
      bra    _taeE_93
      bra    _taeE_94
      bra    _taeE_95
     bra   illtae
     bra   illtae
      bra    _taeE_98
      bra    _taeE_99
      bra    _taeE_9A
      bra    _taeE_9B
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeE_A0
      bra    _taeE_A1
      bra    _taeE_A2
      bra    _taeE_A3
      bra    _taeE_A4
      bra    _taeE_A5
     bra   illtae
     bra   illtae
      bra    _taeE_A8
      bra    _taeE_A9
      bra    _taeE_AA
      bra    _taeE_AB
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
      bra    _taeE_B0
      bra    _taeE_B1
      bra    _taeE_B2
      bra    _taeE_B3
      bra    _taeE_B4
      bra    _taeE_B5
     bra   illtae
     bra   illtae
      bra    _taeE_B8
      bra    _taeE_B9
      bra    _taeE_BA
      bra    _taeE_BB
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae
     bra   illtae

_taeE_00:
       joind d0
      exg d0,d0
       splitd d0
        bra next
_taeE_01:
       joind d0
      exg d0,d4
       splitd d0
        bra next
_taeE_02:
       joind d0
      exg d0,d5
       splitd d0
        bra next
_taeE_03:
       joind d0
      exg d0,a2
       splitd d0
        bra next
_taeE_04:
       joind d0
      exg d0,a1
       splitd d0
        bra next
_taeE_05:
	illegal
       joind d0
      exg d0,a0
       splitd d0
        bra next
_taeE_08:
       joind d0
      exg d0,d2
       splitd d0
        bra next
_taeE_09:
       joind d0
      exg d0,d3
       splitd d0
        bra next
_taeE_0A:
       joind d0
       real_cc $f0000000,$f000000f,lsr
      exg d0,d1
       real_cc $f0,$f,lsl
       splitd d0
        bra next
_taeE_0B:
       joind d0
       lsr #8,d6
      exg d0,d6
       lsl #8,d6
       splitd d0
        bra next
        ; X <--> ?
_taeE_10:
       joind d0
      exg d4,d0
       splitd d0
        bra next
_taeE_11:
      exg d4,d4
        bra next
_taeE_12:
      exg d4,d5
        bra next
_taeE_13:
      exg d4,a2
        bra next
_taeE_14:
      exg d4,a1
        bra next
_taeE_15:
      exg d4,a0
        bra next
_taeE_18:
      exg d4,d2
        bra next
_taeE_19:
      exg d4,d3
        bra next
_taeE_1A:
       real_cc $f0000000,$f000000f,lsr
      exg d4,d1
       real_cc $f0,$f,lsl
        bra next
_taeE_1B:
       lsr #8,d6
      exg d4,d6
       lsl #8,d6
        bra next
        ; Y <--> ?
_taeE_20:
       joind d0
      exg d5,d0
       splitd d0
        bra next
_taeE_21:
      exg d5,d4
        bra next
_taeE_22:
      exg d5,d5
        bra next
_taeE_23:
      exg d5,a2
        bra next
_taeE_24:
      exg d5,a1
        bra next
_taeE_25:
      exg d5,a0
        bra next
_taeE_28:
      exg d5,d2
        bra next
_taeE_29:
      exg d5,d3
        bra next
_taeE_2A:
       real_cc $f0000000,$f000000f,lsr
      exg d5,d1
       real_cc $f0,$f,lsl
        bra next
_taeE_2B:
       lsr #8,d6
      exg d5,d6
       lsl #8,d6
        bra next
        ; U<--> ?
_taeE_30:
       joind d0
      exg a2,d0
       splitd d0
        bra next
_taeE_31:
      exg a2,d4
        bra next
_taeE_32:
      exg a2,d5
        bra next
_taeE_33:
      exg a2,a2
        bra next
_taeE_34:
      exg a2,a1
        bra next
_taeE_35:
      exg a2,a0
        bra next
_taeE_38:
      exg a2,d2
        bra next
_taeE_39:
      exg a2,d3
        bra next
_taeE_3A:
       real_cc $f0000000,$f000000f,lsr
      exg a2,d1
       real_cc $f0,$f,lsl
        bra next
_taeE_3B:
       lsr #8,d6
      exg a2,d6
       lsl #8,d6
        bra next
        ; S <--> ?
_taeE_40:
       joind d0
      exg a1,d0
       splitd d0
        bra next
_taeE_41:
      exg a1,d4
        bra next
_taeE_42:
      exg a1,d5
        bra next
_taeE_43:
      exg a1,a2
        bra next
_taeE_44:
      exg a1,a1
        bra next
_taeE_45:
      exg a1,a0
        bra next
_taeE_48:
      exg a1,d2
        bra next
_taeE_49:
      exg a1,d3
        bra next
_taeE_4A:
       real_cc $f0000000,$f000000f,lsr
      exg a1,d1
       real_cc $f0,$f,lsl
        bra next
_taeE_4B:
       lsr #8,d6
      exg a1,d6
       lsl #8,d6
        bra next
        ; PC <-->
_taeE_50:
       joind d0
      exg a0,d0
       splitd d0
        bra next
_taeE_51:
      exg a0,d4
        bra next
_taeE_52:
      exg a0,d5
        bra next
_taeE_53:
      exg a0,a2
        bra next
_taeE_54:
      exg a0,a1
        bra next
_taeE_55:
      exg a0,a0
        bra next
_taeE_58:
      exg a0,d2
        bra next
_taeE_59:
      exg a0,d3
        bra next
_taeE_5A:
       real_cc $f0000000,$f000000f,lsr
      exg a0,d1
       real_cc $f0,$f,lsl
        bra next
_taeE_5B:
       lsr #8,d6
      exg a0,d6
       lsl #8,d6
        bra next
        ; A <--> ?
_taeE_80:
       joind d0
      exg d2,d0
       splitd d0
        bra next
_taeE_81:
      exg d2,d4
        bra next
_taeE_82:
      exg d2,d5
        bra next
_taeE_83:
      exg d2,a2
        bra next
_taeE_84:
      exg d2,a1
        bra next
_taeE_85:
      exg d2,a0
        bra next
_taeE_88:
      exg d2,d2
        bra next
_taeE_89:
      exg d2,d3
        bra next
_taeE_8A:
		real_cc $f0000000,$f000000f,lsr
      exg d2,d1
      real_cc $f0,$f,lsl
      bra next
_taeE_8B:
       lsr #8,d6
      exg d2,d6
       lsl #8,d6
        bra next
        ; B <--> ?
_taeE_90:
       joind d0
      exg d3,d0
       splitd d0
        bra next
_taeE_91:
      exg d3,d4
        bra next
_taeE_92:
      exg d3,d5
        bra next
_taeE_93:
      exg d3,a2
        bra next
_taeE_94:
      exg d3,a1
        bra next
_taeE_95:
      exg d3,a0
        bra next
_taeE_98:
      exg d3,d2
        bra next
_taeE_99:
      exg d3,d3
        bra next
_taeE_9A:
       real_cc $f0000000,$f000000f,lsr
      exg d3,d1
       real_cc $f0,$f,lsl
        bra next
_taeE_9B:
       lsr #8,d6
      exg d3,d6
       lsl #8,d6
        bra next
        ; CC <--> ?
_taeE_A0:
       joind d0
       real_cc $f0000000,$f000000f,lsr
      exg d1,d0
       real_cc $f0,$f,lsl
       splitd d0
        bra next
_taeE_A1:
       real_cc $f0000000,$f000000f,lsr
      exg d1,d4
       real_cc $f0,$f,lsl
        bra next
_taeE_A2:
       real_cc $f0000000,$f000000f,lsr
      exg d1,d5
       real_cc $f0,$f,lsl
        bra next
_taeE_A3:
       real_cc $f0000000,$f000000f,lsr
      exg d1,a2
       real_cc $f0,$f,lsl
        bra next
_taeE_A4:
       real_cc $f0000000,$f000000f,lsr
      exg d1,a1
       real_cc $f0,$f,lsl
        bra next
_taeE_A5:
       real_cc $f0000000,$f000000f,lsr
      exg d1,a0
       real_cc $f0,$f,lsl
        bra next
_taeE_A8:
       real_cc $f0000000,$f000000f,lsr
      exg d1,d2
       real_cc $f0,$f,lsl
        bra next
_taeE_A9:
       real_cc $f0000000,$f000000f,lsr
      exg d1,d3
       real_cc $f0,$f,lsl
        bra next
_taeE_AA:
       real_cc $f0000000,$f000000f,lsr
      exg d1,d1
       real_cc $f0,$f,lsl
        bra next
_taeE_AB:
       lsr #8,d6
       real_cc $f0000000,$f000000f,lsr
      exg d1,d6
       real_cc $f0,$f,lsl
       lsl #8,d6
        bra next
        
        ; DP <--> ?
_taeE_B0:
       joind d0
       lsr #8,d6
      exg d6,d0
       lsl #8,d6
       splitd d0
        bra next
_taeE_B1:
       lsr #8,d6
      exg d6,d4
       lsl #8,d6
        bra next
_taeE_B2:
       lsr #8,d6
      exg d6,d5
       lsl #8,d6
        bra next
_taeE_B3:
       lsr #8,d6
      exg d6,a2
       lsl #8,d6
        bra next
_taeE_B4:
       lsr #8,d6
      exg d6,a1
       lsl #8,d6
        bra next
_taeE_B5:
       lsr #8,d6
      exg d6,a0
       lsl #8,d6
        bra next
_taeE_B8:
       lsr #8,d6
      exg d6,d2
       lsl #8,d6
        bra next
_taeE_B9:
       lsr #8,d6
      exg d6,d3
       lsl #8,d6
        bra next
_taeE_BA:
       lsr #8,d6
       real_cc $f0000000,$f000000f,lsr
      exg d6,d1
       real_cc $f0,$f,lsl
       lsl #8,d6
        bra next
_taeE_BB:
        bra next

illtae
	;Illegal TFR or EXG instruction found
	; should we do *something* for this????
	bra	next


; -----------------------------------------------------------
	
_00	
	d_neg	getdps,put8,d0,_00,_a,_b
_03
	d_com	getdps,put8,d0,_02,_a,_b	
_04
	d_lsr	getdps,put8,d0,_04,_a,_b
_06
	d_ror	getdps,put8,d0,_06,_a,_b
_07	
	d_asr	getdps,put8,d0,_07,_a,_b
_08:
	d_lsl	getdps,put8,d0,_08,_a,_b
_09:
	d_rol	getdps,put8,d0,_09,_a,_b
_0A:
	d_dec	getdps,put8,d0,_0a,_a,_b
_0C:
	d_inc	getdps,put8,d0,_0c,_a,_b
_0D:
	d_tst	getdps,put8,d0,_0d,_a,_b
_0E:
	move.b	(a3,a0.l),d6
	move.l	d6,a0
				; DP JMP - the macro expands into
				; TWO instructions, so this is better
        bra next
_0F:
	move.w	d6,d7		; Copy DP into d7
	move.b	(a3,a0.l),d7	; Load lower byte with offset from memory
	addq.w	#1,a0
	put8	d7,#0
	move.b	#4,d1		
        bra next
_12:
	nop		; Ahhhh! That was easier!
       bra next
_13:
	;SYNC		No real use within the emulator
       bra next
_16:			; LBRA
	longbranch
       bra next
_17:			; LBSR
	move.w	a0,d0		;push don't work with An, so a copy is made
	addq.w	#2,d0		; increase duplicate PC in d0 now!
	push	a1,d0		; old PC now stored on stack
	get16	a0,d0		; ....likewise here, copy new addr to d0
	add.w	d0,a0		; .w ensures -ve correct themselves!
	bra	next
_19:			; DAA
	exit 20	

       bra next
_1A:			; ORCC,    CC Or M -> CC
	real_cc	$f0000000,$f000000f,lsr		; d1 lo byte is now correct
	get8	a0,d0				; d0 = memory to OR with
	putcc
	or.b	d0,d1
	; Don't 'getcc', because the new CC is already in d1
	incpc
       bra next
_3C:			; CWAI - the emulator is slow enough - it doesn't
			;	need the wait part :-)
			
_1C:			; ANDCC, CC And M -> CC
	real_cc	$f0000000,$f000000f,lsr		; d1 lo byte is now correct
	get8	a0,d0				; d0 = memory to AND with
	putcc
	and.b	d0,d1
	; Don't 'getcc', because the new CC is already in d1
	incpc
       bra next
_1D:
	exit 21
       bra next
_20				; BRA - therefore no test
	dobranch

_21				; BRN
	d_brn	incpc,_,do
_22				; BHI	 - if C=Z=0
	d_bhi	incpc,_,do
_23:				; BLS, if C=1 or Z=1 - i.e. if C&Z !=0
	d_bls	incpc,_,do
_24:				; BCC, C=0
	d_bcc	incpc,_,do
_25:				; BCS, C=1
	d_bcs	incpc,_,do
_26				; BNE Branch - if Z = 0
	d_bne	incpc,_,do
_27:				; BEQ
	d_beq	incpc,_,do
_28:				; BVC - branch if V = 0
	d_bvc	incpc,_,do
_29:				; BVS - if v=1
	d_bvs	incpc,_,do
_2A:				; BPL, if N=0
	d_bpl	incpc,_,do
_2B:				; BMI, if N=1
	d_bmi	incpc,_,do	
_2C:				; BGE, 	if N=V
	d_bge	incpc,_,do
_2D:				; BLT, if N != V
	d_blt	incpc,_,do
_2E:				; BGT, if Z=0 and (V=N)
	d_bgt	incpc,_,do
_2F:				; BLE, if Z=1 or (V != N)
	d_ble	incpc,_,do
_30				; LEAX
	index	a0,d4,_30,_lea
	adjz	d4,_30,w
	bra next			
_31	
	index	a0,d5,_31,_lea	; LEAY
	adjz	d5,_31,w
	bra next			

_32	
	index	a0,a1,_32,_lea	; LEAS
	bra next			

_33	
	index	a0,a2,_33,_lea	; LEAU
	bra next			

_34
	stack	push,a1,a2,_34,$f0000000,$f000000f,lsr,real_cc,null
_35
	;illegal
	stack	pop,a1,a2,_35,$f0,$f,lsl,null,real_cc
_36
	stack	push,a2,a1,_36,$f0000000,$f000000f,lsr,real_cc,null
_37
	stack	pop,a2,a1,_37,$f0,$f,lsl,null,real_cc
_39
	IFD	use_sbase
	cmp.w a4,a1
	beq no_stack		; the stack is exhausted if eq
	ENDC
	pop	a1,d0		; pop stack a1 into d0
	and.l	#$0000ffff,d0	; mask top - YUK - shouldn't need this
	move.l	d0,a0		; adjust PC
	bra next
no_stack:
	exit 1
	
_3A:
	add.w	d3,d4		; ABX - so SIMPLE, it doesn't really
				; need its own routine.
       bra next
_3B:
	; RETURN FROM INTERUPPT
	moveq	#1,d0		; set up stack register
	dostack	pop,a1,a2,_3b,$f0,$f,lsl,null,real_cc
	btst	#31,d1
	bne	pull_all_3b
	move.b	#$ff,d0
	dostack	pop,a1,a2,_3b_2,$f0,$f,lsl,null,real_cc
	bra	next
pull_all_3b
	move.b	#$80,d0
	dostack	pop,a1,a2,_3b_3,$f0,$f,lsl,null,real_cc
	bra	next
_3D:				; MUL: D = A * B
	move.b	d2,d0		; we have just come from the main loop
				; so .b will be enough to clear old contents
	mulu	d3,d0		; multiplication done with duplicate d0 reg
	bclr	#0,d1
	btst	#16,d0
	beq	rest3d		; is there a carry???
	bset	#0,d1
rest3d:	
	splitd	d0		; split d0 into A and B 
	tst.w	d0		; is D zero??? Splitd does not damage 'D'
	beq	z3d
	bset	#2,d1		
	bra	next
z3d:	bclr	#2,d1
	bra 	next
	
_3E:
	exit 15 			; EXIT 15 = RESET	
       bra next
       
_3F:				;SWI
	bset	#31,d1		; set E = 1
	move.b	#$ff,d0		; set up stack register
	dostack	push,a1,a2,_3f,$f0000000,$f000000f,lsr,real_cc,null
	or.l	#$60000000,d1	; set F and I
	move.l	#$fffa,a0	; get indirect location of service routine
	get16	a0,d0		; find address held there
	move.l	d0,a0		; store it in PC
	get16	a0,d0		; find address held there
	move.l	d0,a0		; store it in PC
       bra next
_40:				;NEGA
	d_neg	null,null,d2,_40,_,_,.b
_43:
	d_com	null,null,d2,_43,_,_,.b
_44:		
	d_lsr	null,null,d2,_44,_,_,.b
_46:		
	d_ror	null,null,d2,_46,_,_,.b
_47:
	d_neg	null,null,d2,_47,_,_,.b
_48:			; LSLA (or ASLA)
	d_lsl	null,null,d2,_48,_,_,.b
_49:			; ROL
	d_rol	null,null,d2,_49,_,_,.b
_4A:
	d_dec	null,null,d2,_4a,_,_,.b
_4B:
_4C:	
	d_inc	null,null,d2,_4c,_,_,.b
_4D:			; TSTA 
	d_tst	null,null,d2,_4d,_,_,.b
_4F:			; CLRA 
	d_clr	null,null,d2,_4f,_,_,.b
_50:				;NEGb
	d_neg	null,null,d3,_50,_,_,.b
_53:
	d_com	null,null,d3,_53,_,_,.b
_54:			; LSRb
	d_lsr	null,null,d3,_54,_,_,.b
_56:			; RORb 
	d_ror	null,null,d3,_56,_,_,.b
_57:
	d_neg	null,null,d3,_57,_,_,.b
_58:			; LSLb (or ASLb)
	d_lsl	null,null,d3,_58,_,_,.b
_59:			; ROL
	d_rol	null,null,d3,_59,_,_,.b
_5A:			; DECB
	d_dec	null,null,d3,_5a,_,_,.b
_5C:			; INCB
	d_inc	null,null,d3,_5c,_,_,.b
_5D:			; TSTb 
	d_tst	null,null,d3,_5d,_,_,.b
_5F:			; CLRB 
	d_clr	null,null,d3,_5f,_,_,.b
_60:
	d_neg	index,put8,d0,_60,_a,_b,.b
_63:
	d_com	index,put8,d0,_63,_a,_b,.b
_64:
	d_lsr	index,put8,d0,_64,_a,_b,.b
_66:
	d_ror	index,put8,d0,_66,_a,_b,.b
_67:
	d_asr	index,put8,d0,_67,_a,_b,.b
_68:
	d_lsl	index,put8,d0,_68,_a,_b,.b
_69:
	d_rol	index,put8,d0,_69,_a,_b,.b
_6A:
	d_dec	index,put8,d0,_6a,_a,_b,.b
_6C:
	d_inc	index,put8,d0,_6c,_a,_b,.b
_6D:
	d_tst	index,put8,d0,_6d,_a,_b,.b
_6E:
	index	a0,d0,_6e,null			; HELP - is this right??	
	move.l	d7,a0	
	bra	next
_6F:
	d_clr	index,put8,d0,_6f,_a,_b,.b
_70:
	d_neg	extend,put8,d0,_70,_a,_b,.b
_73:
	d_com	extend,put8,d0,_73,_a,_b,.b
_74:
	d_lsr	extend,put8,d0,_74,_a,_b,.b
_76:
	d_ror	extend,put8,d0,_76,_a,_b,.b
_77:
	d_asr	extend,put8,d0,_77,_a,_b,.b
_78:
	d_lsl	extend,put8,d0,_78,_a,_b,.b
_79:
	d_rol	extend,put8,d0,_79,_a,_b,.b
_7A:
	d_dec	extend,put8,d0,_7a,_a,_b,.b
_7C:
	d_inc	extend,put8,d0,_7c,_a,_b,.b
_7D:
	d_tst	extend,put8,d0,_7d,_a,_b,.b
_7E:
	get16	a0,d0
	move.l	d0,a0
	bra	next
	;d_jmp	extend,put8,d0,_7e,_a,_b,.b 	; ERROR
_7F:
	d_clr	extend,put8,d0,_7f,_a,_b,.b
	;
	; IMMEDIATE MODE INSTRUCTIONS
	;
_80:						; SUBA
	d_sub	get8sinc,d2,_80
_81:
	d_cmp	get8sinc,d2,_81
_82:
	d_sbc	get8sinc,d2,_82
_83:
	d_subd	get16inc,d2,_83
_84:
	d_and	get8sinc,d2,_84
_85:
	d_bit	get8sinc,d2,_85
_86
	d_ldr	get8sinc,d2,_86
_88:
	d_eor	get8sinc,d2,_88
_89:
	d_adc	get8sinc,d2,_89
_8A:
	d_or	get8sinc,d2,_8A
_8B:
  	d_add	get8sinc,d2,_8B
_8C:
	d_cmp16	get16inc,d4,_8C
_8D:	
	d_jsr	dobranch,d0,d0,1
_8E
	d_ldr16 get16inc,d4,_8E
_90:
	d_sub	getdps,d2,_90
_91:
	d_cmp	getdps,d2,_91
_92:
	d_sbc	getdps,d2,_92
_93:
	d_subd	getdps,d2,_93
_94:
	d_and	getdps,d2,_94
_95:
	d_bit	getdps,d2,_95
_96
	d_ldr	getdps,d2,_96
_97:
	d_str	getdps,d2,_97,d7
_98:
	d_eor	getdps,d2,_98
_99:
	d_adc	getdps,d2,_99
_9A:
	d_or	getdps,d2,_9a
_9B:
	d_add	getdps,d2,_9b
_9C:
	d_cmp16	getdps,d4,_9c
_9D:
	d_jsr	get8,d6,d6,1
_9E:
	d_ldr16	getdps,d4,_9e
_9F:
	d_str16	getdps,d4,_9f,d7
_A0:
	d_sub	index,d2,_A0
_A1:
	d_cmp	index,d2,_a1
_A2:
	d_sbc	index,d2,_a2
_A3:
	d_subd	index,d2,_a3
_A4:
	d_and	index,d2,_a4
_A5:
	d_bit	index,d2,_a5
_A6
	d_ldr	index,d2,_a6
_A7:
	d_str	index,d2,_a7,d7
_A8:
	d_eor	index,d2,_a8
_A9:
	d_adc	index,d2,_a9
_AA:
	d_or	index,d2,_aa
_AB:
	d_add	index,d2,_ab
_AC:
	d_cmp16	index,d4,_ac
_AD:
	d_jsr	index,d7,d0,1
_AE:
	d_ldr16	index,d4,_Ae
_AF:
	d_str16	index,d4,_Af,d7
_B0:
	d_sub	extend,d2,_B0
_B1:
	d_cmp	extend,d2,_B1
_B2:
	d_sbc	extend,d2,_B2
_B3:
	d_subd	extend,d2,_B3
_B4:
	d_and	extend,d2,_B4
_B5:
	d_bit	extend,d2,_B5
_B6
	d_ldr	extend,d2,_B6
_B7:
	d_str	extend,d2,_B7,d7
_B8:
	d_eor	extend,d2,_B8
_B9:
	d_adc	extend,d2,_B9
_BA:
	d_or	extend,d2,_Ba
_BB:
	d_add	extend,d2,_Bb
_BC:
	d_cmp16	extend,d4,_Bc
_BD:
	d_jsr	get16,d0,d0,2
_BE:
	d_ldr16	extend,d4,_Be
_BF:
	d_str16	extend,d4,_Bf,d7
	
	; same...but with the B register (and accessing S and U)
_C0:						; SUB - B
	d_sub	get8sinc,d3,_C0
_C1:
	d_cmp	get8sinc,d3,_C1
_C2:
	d_sbc	get8sinc,d3,_C2
_C3:
	d_addd	get16inc,d3,_C3
_C4:
	d_and	get8sinc,d3,_C4
_C5:
	d_bit	get8sinc,d3,_C5
_C6
	d_ldr	get8sinc,d3,_C6
_C8:
	d_eor	get8sinc,d3,_C8
_C9:
	d_adc	get8sinc,d3,_C9
_CA:
	d_or	get8sinc,d3,_CA
_CB:
       	d_add	get8sinc,d3,_CB
_CC:
	get16inc a0,d0
	adjz	d0,_cc,w
	adjn	d0,_cc,15
	clrv
	splitd	d0
	bra	next	
_CE
	a_ldr16 get16inc,A2,_CE
_D0:
	d_sub	getdps,d3,_D0
_D1:
	d_cmp	getdps,d3,_D1
_D2:
	d_sbc	getdps,d3,_D2
_D3:
	d_addd	getdps,d3,_D3
_D4:
	d_and	getdps,d3,_D4
_D5:
	d_bit	getdps,d3,_D5
_D6
	d_ldr	getdps,d3,_D6
_D7:
	d_str	getdps,d3,_D7,d7
_D8:
	d_eor	getdps,d3,_D8
_D9:
	d_adc	getdps,d3,_D9
_DA:
	d_or	getdps,d3,_Da
_DB:
	d_add	getdps,d3,_Db
_DC:
	getdps	a0,d0,_,get16
	adjz	d0,_dc,w
	adjn	d0,_dc,15
	clrv
	splitd	d0
	bra	next	
_DD:
	joind	d0
	getdps	a0,d0,_,put16
	adjz	d0,_dd,w
	adjn	d0,_dd,15
	clrv
	bra	next	

	d_str16	getdps,a1,_Dd,d7
_DE:
	d_ldr16	getdps,a2,_De
_DF:
	d_str16	getdps,a2,_Df,d7
_E0:
	d_sub	index,d3,_E0
_E1:
	d_cmp	index,d3,_E1
_E2:
	d_sbc	index,d3,_E2
_E3:
	d_addd	index,d3,_E3
_E4:
	d_and	index,d3,_E4
_E5:
	d_bit	index,d3,_E5
_E6
	d_ldr	index,d3,_E6
_E7:
	d_str	index,d3,_E7,d7
_E8:
	d_eor	index,d3,_E8
_E9:
	d_adc	index,d3,_E9
_EA:
	d_or	index,d3,_Ea
_EB:
	d_add	index,d3,_Eb
_EC:
	d_ldr16	index,d0,_ec
_ED:
	d_str16	index,d0,_ed,d7
_EE:
	a_ldr16	index,a2,_ee
_EF:
	d_str16	index,a2,_ef,d7
_F0:
	d_sub	extend,d3,_F0
_F1:
	d_cmp	extend,d3,_F1
_F2:
	d_sbc	extend,d3,_F2
_F3:
	d_addd	extend,d3,_F3
_F4:
	d_and	extend,d3,_F4
_F5:
	d_bit	extend,d3,_F5
_F6
	d_ldr	extend,d3,_F6
_F7:
	d_str	extend,d3,_F7,d7
_F8:
	d_eor	extend,d3,_F8
_F9:
	d_adc	extend,d3,_F9
_FA:
	d_or	extend,d3,_Fa
_FB:
	d_add	extend,d3,_Fb
_FC:
	d_ldr16	extend,d0,_Fc
_FD:
	d_str16	extend,d0,_Fd,d7
_FE:
	a_ldr16	extend,a2,_Fe
_FF:
	d_str16	extend,a2,_Ff,d7

;
; PAGE ONE INSTRUCTIONS
;
_10
	get8inc	a0,d0
	asl	#2,d0
	jmp	pojump(pc,d0)
pojump
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_21
       bra    _p2_22
       bra    _p2_23
       bra    _p2_24
       bra    _p2_25
       bra    _p2_26
       bra    _p2_27
       bra    _p2_28
       bra    _p2_29
       bra    _p2_2A
       bra    _p2_2B
       bra    _p2_2C
       bra    _p2_2D
       bra    _p2_2E
       bra    _p2_2F
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_3F
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_83
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_8C
       bra   _illegal
       bra    _p2_8E
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_93
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_9C
       bra   _illegal
       bra    _p2_9E
       bra    _p2_9F
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_A3
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_AC
       bra   _illegal
       bra    _p2_AE
       bra    _p2_AF
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_B3
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_BC
       bra   _illegal
       bra    _p2_BE
       bra    _p2_BF
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_CE
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _p2_DE
       bra    _p2_DF
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _p2_EE
       bra   _p2_EF
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra    _p2_FE
       bra    _p2_FF
;
_p2_21
	d_brn	incpc2,l,long
_p2_22
 	d_bhi	incpc2,l,long
_p2_23
  	d_bls	incpc2,l,long
_p2_24
 	d_bcc	incpc2,l,long
_p2_25
  	d_bcs	incpc2,l,long
_p2_26
 	d_bne	incpc2,l,long
_p2_27
 	d_beq	incpc2,l,long
_p2_28
  	d_bvc	incpc2,l,long
_p2_29
  	d_bvs	incpc2,l,long
_p2_2A
  	d_bpl	incpc2,l,long
_p2_2B
  	d_bmi	incpc2,l,long
_p2_2C
  	d_bge	incpc2,l,long
_p2_2D
  	d_blt	incpc2,l,long
_p2_2E
  	d_bgt	incpc2,l,long
_p2_2F
  	d_ble	incpc2,l,long
_p2_3F
	bset	#31,d1		; set E = 1
	move.b	#$ff,d0		; set up stack register
	dostack	push,a1,a2,_103f,$f0000000,$f000000f,lsr,real_cc,null
	or.l	#$60000000,d1	; set F and I
	move.l	#$fff4,a0	; get indirect location of service routine
	get16	a0,d0		; find address held there
	move.w	d0,a0		; store it in PC
	get16	a0,d0		; find address held there
	move.l	d0,a0		; store it in PC

       bra      next
_p2_83
	joind	d7
	d_cmp16	get16inc,d7,_1083
_p2_8C
	d_cmp16	get16inc,d5,_108c
_p2_8E
	d_ldr16	get16inc,d5,_108e
_p2_93
	; Manual unrolling of d_cmp16 macro
	; (I don't think this needs it, only the 'index' macro was dodgy)
	joind	d7
	;
	move	d7,-(a7)
	index	a0,d0,_10a3,get16
	move	(a7)+,d7
	putcc
	cmp.w	d0,d7
	getcc
	bra	next
	; Old version
	;joind	d7
	;d_cmp16	getdps,d7,_1093,d7
_p2_9C
	d_cmp16	getdps,d5,_109c,d7
_p2_9E
	d_ldr16	getdps,d5,_109e,d7
_p2_9F
	d_str16	getdps,d5,_109f,d7
_p2_A3
	; Manual unrolling of d_cmp16 macro
	joind	d7
	;
	move	d7,-(a7)
	index	a0,d0,_10a3,get16
	move	(a7)+,d7
	putcc
	cmp.w	d0,d7
	getcc
	bra	next
	; Old version
	;d_cmp16	index,d7,_1093,d7
	
_p2_AC
	d_cmp16	index,d5,_10ac,d7
_p2_AE
       	d_ldr16	index,d5,_10ae,d7
_p2_AF
	d_str16	index,d5,_10af,d7
_p2_B3
	joind	d7
	d_cmp16	extend,d7,_10b3,d7
_p2_BC
	d_cmp16	extend,d5,_10ac,d7
_p2_BE
	d_ldr16	extend,d5,_10be,d7
_p2_BF
	d_str16	extend,d5,_10bf,d7
_p2_CE
	a_ldr16	get16inc,a1,_10ce
_p2_DE
	a_ldr16	getdps,a1,_10de,d7
_p2_DF
	a_ldr16	getdps,a1,_10df,d7
_p2_EE
	a_ldr16	index,a1,_10ee,d7
_p2_EF
	d_str16	index,a1,_10ef,d7
_p2_FE
	a_ldr16	extend,a1,_10fe
_p2_FF
	d_str16	extend,a1,_10ff,d7
;
; PAGE TWO INSTRUCTIONS
;

_11
	get8inc	a0,d0
	asl	#2,d0
	jmp	ptjump(pc,d0)
ptjump
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _p3_3F
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _p3_83
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _p3_8C
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _p3_93
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _p3_9C
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _p3_A3
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _p3_AC
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _p3_B3
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
       bra   _illegal
;
;
_p3_3F		; SWI3
	bset	#31,d1		; set E = 1
	move.b	#$ff,d0		; set up stack register
	dostack	push,a1,a2,_113f,$f0000000,$f000000f,lsr,real_cc,null
	move.l	#$fff2,a0	; get indirect location of service routine
	get16	a0,d0		; find address held there
	move.l	d0,a0		; store it in PC
	get16	a0,d0		; find address held there
	move.l	d0,a0		; store it in PC
       bra      next
_p3_83
	d_cmp16	get16inc,a2,_1183,d7
_p3_8C
 	d_cmp16	get16inc,a1,_118c,d7
_p3_93
  	d_cmp16	getdps,a2,_1193,d7
_p3_9C
  	d_cmp16	getdps,a1,_119c,d7
_p3_A3
  	d_cmp16	extend,a2,_11a3,d7
_p3_AC
  	d_cmp16	extend,a1,_11ac,d7
_p3_B3
 	d_cmp16	index,a2,_11b3,d7
_p3_BC
 	d_cmp16	index,a1,_11bc,d7
;
 
three
	rts

four
	rts 




check_breaks
	cmp.b	#65,$bfec01		; check for the HELP key (as usual)
	beq	_quit_emu
break_data
	cmp.w	#0,d2			; these instructions are replaced by
	beq	brk1			; the AMOS routine
	cmp.w	#0,d2
	beq	brk2
	cmp.w	#0,d2
	beq	brk3
	cmp.w	#0,d2
	beq	brk4
	cmp.w	#0,d2
	beq	brk5
	bra	instr

; Notes for dynamic re-programming of the breakpoints
;
; Registers:
;		D2 = A = B47C
;		D3 = B = B67C
;		D4 = X = B87C
;		D5 = Y = BA7C
;		D6 = DP= ????
;		D1 = CC= ????
;
;		A0 = PC = B0FC
;		A1 = S  = B2FC
;		A2 = U  = B4FC
;		A4 = SB = B8FC
;
; Comparisons:
;		bne 6600
;		beq 6700
; 		bgt 6e00
;		bhi 6200
;		blt 6d00
;		bcs 6500
;  		bge 6c00
;		ble 6f00
;
brk1
	cmp.l	#-1,$00fffffe		; -1 = break all the time
	bne	notp1
gotbr1	move.l	#1,d7
	bra	brexit
notp1	subq.l	#1,$00fffffe
	beq	gotbr1
	jmp	instr(pc)

brk2
	cmp.l	#-1,$00fffffe
	bne	notp2
gotbr2	move.l	#2,d7
	bra	brexit
notp2	subq.l	#1,$00fffffe
	beq	gotbr2
	jmp	instr(pc)

brk3
	cmp.l	#-1,$00fffffe
	bne	notp3
gotbr3	move.l	#3,d7
	bra	brexit
notp3	subq.l	#1,$00fffffe
	beq	gotbr3
	jmp	instr(pc)

brk4
	cmp.l	#$ffffffff,$00fffffe
	bne	notp4
gotbr4	move.l	#4,d7
	bra	brexit
notp4	subq.l	#1,$00fffffe
	beq	gotbr4
	jmp	instr(pc)
	
brk5
	cmp.l	#$ffffffff,$00fffffe
	bne	notp5
gotbr5	move.l	#5,d7
	bra	brexit
notp5	subq.l	#1,$00fffffe
	beq	gotbr5
	jmp	instr(pc)
	
brexit
	exit	3


memory
	dc.b	$8e,9,0,$6f,$83,$30,1,$26,$fa,57
	dc.b	$8e,$5,$fe,$e7,$80,$8c,$5,$ff,$23,$f9,57
	dc.b	$86,1,$c6,2,$8e,0,3,$10,$8e,0,4,$10,$ce,0,5,$ce,0,6,57	
	dc.b	$10,$ef,1,57
	dc.b	$10,$8e,$12,$34,$10,$8c,$1,$2,$10,$83,$1,$2,$39
	dc.b	$34,$01,$35,$01,57
	dc.b	$7e,0,3
	dc.b	$cc,$12,$34			; LDD #$1234..and U
	dc.b	$39

	dc.l	1
	dc.l	2
	dc.l	3
	dc.l	4

