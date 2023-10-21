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
	movl	$46, .Llimit(%rip)
	movl	$0, .LN(%rip)
	leaq	".Lfibo("(%rip), %r14
	leaq	".L) = "(%rip), %r15
	leaq	".L\n"(%rip), %rbx
	.p2align	4, 0x90
.LBB0_1:                                # %cond
                                        # =>This Inner Loop Header: Depth=1
	movl	.LN(%rip), %eax
	cmpl	.Llimit(%rip), %eax
	jg	.LBB0_3
# %bb.2:                                # %body
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r14, %rdi
	callq	writeString@PLT
	movl	.LN(%rip), %edi
	callq	writeInteger@PLT
	movq	%r15, %rdi
	callq	writeString@PLT
	movl	.LN(%rip), %edi
	callq	fibo_$1@PLT
	movl	%eax, %edi
	callq	writeInteger@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
	incl	.LN(%rip)
	jmp	.LBB0_1
.LBB0_3:                                # %after
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
	.globl	fibo_$1                         # -- Begin function fibo_$1
	.p2align	4, 0x90
	.type	fibo_$1,@function
fibo_$1:                                # @"fibo_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movl	%edi, 12(%rsp)
	testl	%edi, %edi
	je	.LBB1_1
# %bb.3:                                # %after
	cmpl	$1, 12(%rsp)
	jne	.LBB1_5
# %bb.4:                                # %then2
	movl	$1, %eax
	jmp	.LBB1_2
.LBB1_1:                                # %then
	xorl	%eax, %eax
	jmp	.LBB1_2
.LBB1_5:                                # %after3
	movl	12(%rsp), %edi
	decl	%edi
	callq	fibo_$1@PLT
	movl	%eax, %ebx
	movl	12(%rsp), %edi
	addl	$-2, %edi
	callq	fibo_$1@PLT
	addl	%ebx, %eax
.LBB1_2:                                # %then
	addq	$16, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	fibo_$1, .Lfunc_end1-fibo_$1
	.cfi_endproc
                                        # -- End function
	.type	.LN,@object                     # @N
	.local	.LN
	.comm	.LN,4,4
	.type	.Llimit,@object                 # @limit
	.local	.Llimit
	.comm	.Llimit,4,4
	.type	".Lfibo(",@object               # @"fibo("
	.section	.rodata,"a",@progbits
".Lfibo(":
	.asciz	"fibo("
	.size	".Lfibo(", 6

	.type	".L) = ",@object                # @") = "
".L) = ":
	.asciz	") = "
	.size	".L) = ", 5

	.type	".L\n",@object                  # @"\0A"
".L\n":
	.asciz	"\n"
	.size	".L\n", 2

	.section	".note.GNU-stack","",@progbits
