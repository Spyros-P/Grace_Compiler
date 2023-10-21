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
	leaq	".LHello world!\n"(%rip), %rdi
	callq	writeString@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	".LHello world!\n",@object      # @"Hello world!\0A"
	.section	.rodata,"a",@progbits
".LHello world!\n":
	.asciz	"Hello world!\n"
	.size	".LHello world!\n", 14

	.section	".note.GNU-stack","",@progbits
