	.text
	.file	"grace program"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	leaq	".LPlease, give me the number of rings: "(%rip), %rdi
	callq	writeString@PLT
	callq	readInteger@PLT
	movl	%eax, .LNumberOfRings(%rip)
	leaq	".L\nHere is the solution:\n\n"(%rip), %rdi
	callq	writeString@PLT
	movl	.LNumberOfRings(%rip), %edi
	leaq	.Lleft(%rip), %rsi
	leaq	.Lright(%rip), %rdx
	leaq	.Lmiddle(%rip), %rcx
	callq	hanoi_$1@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	hanoi_$1                        # -- Begin function hanoi_$1
	.p2align	4, 0x90
	.type	hanoi_$1,@function
hanoi_$1:                               # @"hanoi_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movl	%edi, 12(%rsp)
	movq	%rsi, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 32(%rsp)
	cmpl	$0, 12(%rsp)
	jle	.LBB1_2
# %bb.1:                                # %then
	movl	12(%rsp), %edi
	decl	%edi
	movq	24(%rsp), %rsi
	movq	32(%rsp), %rdx
	movq	16(%rsp), %rcx
	callq	hanoi_$1@PLT
	movq	24(%rsp), %rdi
	movq	16(%rsp), %rsi
	callq	move_$2@PLT
	movl	12(%rsp), %edi
	decl	%edi
	movq	32(%rsp), %rsi
	movq	16(%rsp), %rdx
	movq	24(%rsp), %rcx
	callq	hanoi_$1@PLT
.LBB1_2:                                # %after
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	hanoi_$1, .Lfunc_end1-hanoi_$1
	.cfi_endproc
                                        # -- End function
	.globl	move_$2                         # -- Begin function move_$2
	.p2align	4, 0x90
	.type	move_$2,@function
move_$2:                                # @"move_$2"
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	".LMove from "(%rip), %rdi
	callq	writeString@PLT
	movq	16(%rsp), %rdi
	callq	writeString@PLT
	leaq	".L to "(%rip), %rdi
	callq	writeString@PLT
	movq	8(%rsp), %rdi
	callq	writeString@PLT
	leaq	".L.\n"(%rip), %rdi
	callq	writeString@PLT
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	move_$2, .Lfunc_end2-move_$2
	.cfi_endproc
                                        # -- End function
	.type	".LMove from ",@object          # @"Move from "
	.section	.rodata,"a",@progbits
".LMove from ":
	.asciz	"Move from "
	.size	".LMove from ", 11

	.type	".L to ",@object                # @" to "
".L to ":
	.asciz	" to "
	.size	".L to ", 5

	.type	".L.\n",@object                 # @".\0A"
".L.\n":
	.asciz	".\n"
	.size	".L.\n", 3

	.type	.LNumberOfRings,@object         # @NumberOfRings
	.local	.LNumberOfRings
	.comm	.LNumberOfRings,4,4
	.type	".LPlease, give me the number of rings: ",@object # @"Please, give me the number of rings: "
".LPlease, give me the number of rings: ":
	.asciz	"Please, give me the number of rings: "
	.size	".LPlease, give me the number of rings: ", 38

	.type	".L\nHere is the solution:\n\n",@object # @"\0AHere is the solution:\0A\0A"
".L\nHere is the solution:\n\n":
	.asciz	"\nHere is the solution:\n\n"
	.size	".L\nHere is the solution:\n\n", 25

	.type	.Lleft,@object                  # @left
.Lleft:
	.asciz	"left"
	.size	.Lleft, 5

	.type	.Lright,@object                 # @right
.Lright:
	.asciz	"right"
	.size	.Lright, 6

	.type	.Lmiddle,@object                # @middle
.Lmiddle:
	.asciz	"middle"
	.size	.Lmiddle, 7

	.section	".note.GNU-stack","",@progbits
