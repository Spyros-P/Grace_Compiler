	.text
	.file	"grace program"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%r15
	.cfi_def_cfa_offset 16
	pushq	%r14
	.cfi_def_cfa_offset 24
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	.cfi_offset %r15, -16
	leaq	".LFirst\n"(%rip), %rdi
	callq	test_$1@PLT
	cltq
	leaq	(%rax,%rax,2), %rax
	leaq	.Larray(%rip), %rcx
	leaq	(%rcx,%rax,4), %r14
	leaq	".LSecond\n"(%rip), %rdi
	callq	test_$1@PLT
	movslq	%eax, %r15
	leaq	".LThird\n"(%rip), %rdi
	callq	test_$1@PLT
	movl	%eax, %ebx
	leaq	".LFourth\n"(%rip), %rdi
	callq	test_$1@PLT
	movl	%ebx, %edi
	movl	%eax, %esi
	callq	sum_$3@PLT
	movl	%eax, (%r14,%r15,4)
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	test_$1                         # -- Begin function test_$1
	.p2align	4, 0x90
	.type	test_$1,@function
test_$1:                                # @"test_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	movq	%rsp, %rdi
	callq	ret_1_$2@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	test_$1, .Lfunc_end1-test_$1
	.cfi_endproc
                                        # -- End function
	.globl	ret_1_$2                        # -- Begin function ret_1_$2
	.p2align	4, 0x90
	.type	ret_1_$2,@function
ret_1_$2:                               # @"ret_1_$2"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rdi
	callq	writeString@PLT
	movl	$1, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	ret_1_$2, .Lfunc_end2-ret_1_$2
	.cfi_endproc
                                        # -- End function
	.globl	sum_$3                          # -- Begin function sum_$3
	.p2align	4, 0x90
	.type	sum_$3,@function
sum_$3:                                 # @"sum_$3"
	.cfi_startproc
# %bb.0:                                # %entry
                                        # kill: def $esi killed $esi def $rsi
                                        # kill: def $edi killed $edi def $rdi
	movl	%edi, -4(%rsp)
	movl	%esi, -8(%rsp)
	leal	(%rdi,%rsi), %eax
	retq
.Lfunc_end3:
	.size	sum_$3, .Lfunc_end3-sum_$3
	.cfi_endproc
                                        # -- End function
	.type	.Larray,@object                 # @array
	.local	.Larray
	.comm	.Larray,36,16
	.type	".LFirst\n",@object             # @"First\0A"
	.section	.rodata,"a",@progbits
".LFirst\n":
	.asciz	"First\n"
	.size	".LFirst\n", 7

	.type	".LSecond\n",@object            # @"Second\0A"
".LSecond\n":
	.asciz	"Second\n"
	.size	".LSecond\n", 8

	.type	".LThird\n",@object             # @"Third\0A"
".LThird\n":
	.asciz	"Third\n"
	.size	".LThird\n", 7

	.type	".LFourth\n",@object            # @"Fourth\0A"
".LFourth\n":
	.asciz	"Fourth\n"
	.size	".LFourth\n", 8

	.section	".note.GNU-stack","",@progbits
