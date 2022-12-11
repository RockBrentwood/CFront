## 1985 Feb 08 14:05
	.file	"mcrt0.s"
# %Z% %M% %I% %H% %T%
#	C runtime startup and exit with profiling.
#	see crt0.s for explanation of EPSW stuff

	.set	CBUFS,	600
	.set	EXIT,	1
	.set	WORDSIZE, 4
	.set	EPSWOST,50
	.set	WREPSW,10
	.set	DIVZERO,1
	.set	UNFL,2
	.set	OVFL,4
	.set	INVOP,8
	.set	INEXACT,0x10
	.set	TRAPBITS,DIVZERO+UNFL+OVFL+INVOP+INEXACT
	.set	STZERO,0x100
	.set	STUFLO,0x200
	.set	STOFLO,0x400
	.set	STINVOP,0x800
	.set	STINEX,0x1000
	.set	STBITS,STZERO+STUFLO+STOFLO+STINVOP+STINEX
	.set	EMASK,TRAPBITS+STBITS
	.set	NEWEPSW,DIVZERO+OVFL+INVOP
	.globl	_cleanup
	.globl	_start
	.globl	environ
	.globl	etext
	.globl	exit
	.globl	main
	.globl	monitor
	.globl	write
	.globl	___Argv
_start:
	movsw	.ostargs,%ap
	ost	&EPSWOST
	movw	-4(%sp), environ
	movw	-8(%sp),    Argv			# get prog name for profiling
	subw3	&.eprol, &etext, %r8			# get text size in bytes
	andw2	&-WORDSIZE, %r8				# range/#buckets ~= scalefactor
							# fails if #buckets rounded up
	addw2	&8*CBUFS+12+WORDSIZE-1, %r8		# add in entry counts and header and
	andw2	&-WORDSIZE, %r8				#	round to word boundary
	addw3	&4,%r8,%r7
	pushw	%r7					# get space
	call	&1, sbrk
	cmpw	&-1, %r0
	je	.nospace				# start profiling
	pushaw	.eprol
	pushaw	etext
	pushw	%r0
	lrsw2	&1, %r8					# monitor wants # of shorts in buff
	pushw	%r8
	pushw	&CBUFS
	call	&5, monitor
	call	&3, main				# start user program
	pushw	%r0
	call	&1, exit
.nospace:
	pushw	&2					# write error message and exit
	pushaw	.emesg
	pushaw	MESSL
	call	&3, write
	pushw	&-1
	ost	&EXIT
.eprol:
	.data
	.align	4
.ostargs:
	.word	WREPSW
	.word	EMASK
	.word	NEWEPSW
environ:
	.word	0
.emesg:
#	"No space for monitor buffer\n"
	.byte	78,111,32,115,112,97,99,101,32,102,111,114
	.byte	32,109,111,110,105,116,111,114
	.byte	32,98,117,102,102,101,114,10
	.set	MESSL, .-.emesg
	.byte	0
