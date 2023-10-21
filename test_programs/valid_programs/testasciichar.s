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
	movb	$65, .Lmychar(%rip)
	movl	$65, %edi
	callq	ascii@PLT
	movl	%eax, .Lmyascii(%rip)
	leaq	".LThe ascii value of "(%rip), %rdi
	callq	writeString@PLT
	movzbl	.Lmychar(%rip), %edi
	callq	writeChar@PLT
	leaq	".L is "(%rip), %rdi
	callq	writeString@PLT
	movl	.Lmyascii(%rip), %edi
	callq	writeInteger@PLT
	movl	$10, %edi
	callq	writeChar@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lmychar,@object                # @mychar
	.local	.Lmychar
	.comm	.Lmychar,1,1
	.type	.Lmyascii,@object               # @myascii
	.local	.Lmyascii
	.comm	.Lmyascii,4,4
	.type	".LThe ascii value of ",@object # @"The ascii value of "
	.section	.rodata,"a",@progbits
".LThe ascii value of ":
	.asciz	"The ascii value of "
	.size	".LThe ascii value of ", 20

	.type	".L is ",@object                # @" is "
".L is ":
	.asciz	" is "
	.size	".L is ", 5

	.section	".note.GNU-stack","",@progbits
