	.text
	.file	"grace program"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movl	.La(%rip), %edi
	callq	foo_$1@PLT
	movl	%eax, %ebx
	movl	.La(%rip), %edi
	callq	bar_$3@PLT
	addl	%ebx, %eax
	movl	%eax, .La(%rip)
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	foo_$1                          # -- Begin function foo_$1
	.p2align	4, 0x90
	.type	foo_$1,@function
foo_$1:                                 # @"foo_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	%edi, 4(%rsp)
	callq	bar_$2@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	foo_$1, .Lfunc_end1-foo_$1
	.cfi_endproc
                                        # -- End function
	.globl	bar_$2                          # -- Begin function bar_$2
	.p2align	4, 0x90
	.type	bar_$2,@function
bar_$2:                                 # @"bar_$2"
	.cfi_startproc
# %bb.0:                                # %entry
	movl	%edi, -4(%rsp)
	movl	$5, %eax
	retq
.Lfunc_end2:
	.size	bar_$2, .Lfunc_end2-bar_$2
	.cfi_endproc
                                        # -- End function
	.globl	bar_$3                          # -- Begin function bar_$3
	.p2align	4, 0x90
	.type	bar_$3,@function
bar_$3:                                 # @"bar_$3"
	.cfi_startproc
# %bb.0:                                # %entry
	movl	%edi, -4(%rsp)
	movl	$6, %eax
	retq
.Lfunc_end3:
	.size	bar_$3, .Lfunc_end3-bar_$3
	.cfi_endproc
                                        # -- End function
	.type	.La,@object                     # @a
	.local	.La
	.comm	.La,4,4
	.section	".note.GNU-stack","",@progbits
