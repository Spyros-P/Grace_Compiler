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
	movl	.La(%rip), %edi
	movl	.Lb(%rip), %esi
	movl	.Lc(%rip), %edx
	callq	test_$1@PLT
	movl	.La(%rip), %esi
	addl	.Lb(%rip), %esi
	movl	.Lc(%rip), %edx
	addl	%edx, %esi
	movl	%eax, %edi
	callq	test_$1@PLT
	movl	%eax, .Ld(%rip)
	xorl	%eax, %eax
	popq	%rcx
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
                                        # kill: def $esi killed $esi def $rsi
                                        # kill: def $edi killed $edi def $rdi
	movl	%edi, -4(%rsp)
	movl	%esi, -8(%rsp)
	movl	%edx, -12(%rsp)
	leal	(%rdi,%rsi), %eax
	addl	%edx, %eax
	retq
.Lfunc_end1:
	.size	test_$1, .Lfunc_end1-test_$1
	.cfi_endproc
                                        # -- End function
	.type	.La,@object                     # @a
	.local	.La
	.comm	.La,4,4
	.type	.Lb,@object                     # @b
	.local	.Lb
	.comm	.Lb,4,4
	.type	.Lc,@object                     # @c
	.local	.Lc
	.comm	.Lc,4,4
	.type	.Ld,@object                     # @d
	.local	.Ld
	.comm	.Ld,4,4
	.section	".note.GNU-stack","",@progbits
