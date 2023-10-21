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
	leaq	".LCondition correctly evaluated!\n"(%rip), %rdi
	callq	writeString@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	ret_1_$1                        # -- Begin function ret_1_$1
	.p2align	4, 0x90
	.type	ret_1_$1,@function
ret_1_$1:                               # @"ret_1_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	leaq	".LYou should NOT see the message!\n"(%rip), %rdi
	callq	writeString@PLT
	movl	$1, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	ret_1_$1, .Lfunc_end1-ret_1_$1
	.cfi_endproc
                                        # -- End function
	.type	".LYou should NOT see the message!\n",@object # @"You should NOT see the message!\0A"
	.section	.rodata,"a",@progbits
".LYou should NOT see the message!\n":
	.asciz	"You should NOT see the message!\n"
	.size	".LYou should NOT see the message!\n", 33

	.type	".LCondition correctly evaluated!\n",@object # @"Condition correctly evaluated!\0A"
".LCondition correctly evaluated!\n":
	.asciz	"Condition correctly evaluated!\n"
	.size	".LCondition correctly evaluated!\n", 32

	.type	".LCondition wrongly evaluated!\n",@object # @"Condition wrongly evaluated!\0A"
".LCondition wrongly evaluated!\n":
	.asciz	"Condition wrongly evaluated!\n"
	.size	".LCondition wrongly evaluated!\n", 30

	.section	".note.GNU-stack","",@progbits
