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
	movabsq	$103079215119, %rax             # imm = 0x180000000F
	movq	%rax, .Larr(%rip)
	leaq	.Larr(%rip), %rdi
	movabsq	$180388626465, %rax             # imm = 0x2A00000021
	movq	%rax, .Larr+8(%rip)
	movl	$51, .Larr+16(%rip)
	movl	$5, %esi
	callq	printArray_$1@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	printArray_$1                   # -- Begin function printArray_$1
	.p2align	4, 0x90
	.type	printArray_$1,@function
printArray_$1:                          # @"printArray_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	movl	%esi, 12(%rsp)
	movl	$0, 8(%rsp)
	movl	$91, %edi
	callq	writeChar@PLT
	jmp	.LBB1_1
	.p2align	4, 0x90
.LBB1_4:                                # %after6
                                        #   in Loop: Header=BB1_1 Depth=1
	incl	8(%rsp)
.LBB1_1:                                # %cond
                                        # =>This Inner Loop Header: Depth=1
	movl	8(%rsp), %eax
	cmpl	12(%rsp), %eax
	jge	.LBB1_5
# %bb.2:                                # %body
                                        #   in Loop: Header=BB1_1 Depth=1
	movslq	8(%rsp), %rax
	movq	16(%rsp), %rcx
	movl	(%rcx,%rax,4), %edi
	callq	writeInteger@PLT
	movl	12(%rsp), %eax
	decl	%eax
	cmpl	%eax, 8(%rsp)
	jge	.LBB1_4
# %bb.3:                                # %then
                                        #   in Loop: Header=BB1_1 Depth=1
	movl	$44, %edi
	callq	writeChar@PLT
	jmp	.LBB1_4
.LBB1_5:                                # %after
	leaq	".L]\n"(%rip), %rdi
	callq	writeString@PLT
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	printArray_$1, .Lfunc_end1-printArray_$1
	.cfi_endproc
                                        # -- End function
	.type	".L]\n",@object                 # @"]\0A"
	.section	.rodata,"a",@progbits
".L]\n":
	.asciz	"]\n"
	.size	".L]\n", 3

	.type	.Larr,@object                   # @arr
	.local	.Larr
	.comm	.Larr,20,16
	.section	".note.GNU-stack","",@progbits
