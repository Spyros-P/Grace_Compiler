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
	callq	bruh_$1@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	bruh_$1                         # -- Begin function bruh_$1
	.p2align	4, 0x90
	.type	bruh_$1,@function
bruh_$1:                                # @"bruh_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	leaq	".Lbruh\n"(%rip), %rdi
	callq	writeString@PLT
	callq	hello_$2@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	bruh_$1, .Lfunc_end1-bruh_$1
	.cfi_endproc
                                        # -- End function
	.globl	hello_$2                        # -- Begin function hello_$2
	.p2align	4, 0x90
	.type	hello_$2,@function
hello_$2:                               # @"hello_$2"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	leaq	".LHello bruh!\n"(%rip), %rdi
	callq	writeString@PLT
	callq	hello_$2@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	hello_$2, .Lfunc_end2-hello_$2
	.cfi_endproc
                                        # -- End function
	.type	".LHello bruh!\n",@object       # @"Hello bruh!\0A"
	.section	.rodata,"a",@progbits
".LHello bruh!\n":
	.asciz	"Hello bruh!\n"
	.size	".LHello bruh!\n", 13

	.type	".Lbruh\n",@object              # @"bruh\0A"
".Lbruh\n":
	.asciz	"bruh\n"
	.size	".Lbruh\n", 6

	.type	.Ltest,@object                  # @test
	.local	.Ltest
	.comm	.Ltest,4,4
	.type	".LHello world!\n",@object      # @"Hello world!\0A"
".LHello world!\n":
	.asciz	"Hello world!\n"
	.size	".LHello world!\n", 14

	.section	".note.GNU-stack","",@progbits
