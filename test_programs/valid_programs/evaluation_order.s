	.text
	.file	"grace program"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %rbp, -16
	leaq	.Lfirst(%rip), %rdi
	callq	test_$1@PLT
	movl	%eax, %ebx
	leaq	.Lsecond(%rip), %rdi
	callq	test_$1@PLT
	movl	%eax, %ebp
	leaq	.Lthird(%rip), %rdi
	callq	test_$1@PLT
	movl	%ebx, %edi
	movl	%ebp, %esi
	movl	%eax, %edx
	callq	do_nothing_$3@PLT
	xorl	%eax, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
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
	.globl	do_nothing_$3                   # -- Begin function do_nothing_$3
	.p2align	4, 0x90
	.type	do_nothing_$3,@function
do_nothing_$3:                          # @"do_nothing_$3"
	.cfi_startproc
# %bb.0:                                # %entry
	movl	%edi, -4(%rsp)
	movl	%esi, -8(%rsp)
	movl	%edx, -12(%rsp)
	retq
.Lfunc_end3:
	.size	do_nothing_$3, .Lfunc_end3-do_nothing_$3
	.cfi_endproc
                                        # -- End function
	.type	.Lfirst,@object                 # @first
	.section	.rodata,"a",@progbits
.Lfirst:
	.asciz	"first"
	.size	.Lfirst, 6

	.type	.Lsecond,@object                # @second
.Lsecond:
	.asciz	"second"
	.size	.Lsecond, 7

	.type	.Lthird,@object                 # @third
.Lthird:
	.asciz	"third"
	.size	.Lthird, 6

	.section	".note.GNU-stack","",@progbits
