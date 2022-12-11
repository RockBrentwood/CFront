## 1985 Feb 08 14:05
	.file	"crt0.s"
# %Z% %M% %I% %H% %T%
#	C runtime startup - call main; call exit when done.
#		new startup procedure uses ost "50" to write
#		the EPSW to trap on overflow and invalid operation
#		this ties in to the 5.0 ucode delivery (I hope).

#	modified by Jerry Schwarz to call constructors and
#	destructors of CC

	.set	EXIT,	1
	.set	EPSWOST,50		# new ost for fp trapping modes
	.set	WREPSW,10		# 10 says write
	.set	DIVZERO,1		# trap on divide by zero bit
	.set	UNFL,2			# trap on underflow bit
	.set	OVFL,4			# trap on overflow bit
	.set	INVOP,8			# trap on
	.set	INEXACT,0x10		# trap on
	.set	TRAPBITS,DIVZERO+UNFL+OVFL+INVOP+INEXACT
	.set	STZERO,0x100		# divide by zero sticky bit
	.set	STUFLOW,0x200		# underflow sticky bit
	.set	STOFLOW,0x400		# overflow sticky bit
	.set	STINVOP,0x800		# invalid op sticky bit
	.set	STINEX,0x1000		# iexact result sticky bit
	.set	STBITS,STZERO+STUFLOW+STOFLOW+STINVOP+STINEX
	.set	EMASK,TRAPBITS+STBITS	# initially clear all via EMASK
	.set	NEWEPSW,DIVZERO+OVFL+INVOP	# default startup condition
	.globl	main
	.globl	_start
	.globl	_mcount
	.globl	environ
	.globl	_STC_all
_start:
	ost	&EPSWOST
	call	&O,_STC_all
	movaw	.ostargs,%ap		# sleazy, but it should work
	movw	-4(%sp), environ
	call	&3, main
	pushw	%r0
	call	&1, exit
	pushw	%r0
	ost	&EXIT
#	good-bye

_mcount:			# dummy in case an object module has been
	rsb			# compiled with cc -p but not the load module

	.data
	.align 4
.ostargs:
	.word	WREPSW
	.word	EMASK
	.word	NEWEPSW
environ:
	.word	0
