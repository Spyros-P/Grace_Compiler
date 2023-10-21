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
	leaq	.Lmyset(%rip), %rbx
	leaq	.Labcde(%rip), %rsi
	movq	%rbx, %rdi
	callq	strcpy@PLT
	movq	%rbx, %rdi
	callq	powerset_$2@PLT
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	power_$1                        # -- Begin function power_$1
	.p2align	4, 0x90
	.type	power_$1,@function
power_$1:                               # @"power_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movl	%edi, 20(%rsp)
	movl	%esi, 12(%rsp)
	movl	$0, 16(%rsp)
	cmpl	$0, 12(%rsp)
	je	.LBB1_3
# %bb.1:                                # %after
	movl	20(%rsp), %edi
	movl	12(%rsp), %eax
	movl	%eax, %esi
	shrl	$31, %esi
	addl	%eax, %esi
	sarl	%esi
	callq	power_$1@PLT
	movl	%eax, 16(%rsp)
	movl	12(%rsp), %eax
	movl	%eax, %ecx
	shrl	$31, %ecx
	addl	%eax, %ecx
	andl	$-2, %ecx
	cmpl	%ecx, %eax
	je	.LBB1_4
# %bb.2:                                # %else
	movl	16(%rsp), %ecx
	movl	20(%rsp), %eax
	imull	%ecx, %eax
	imull	%ecx, %eax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB1_3:                                # %then
	.cfi_def_cfa_offset 32
	movl	$1, %eax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB1_4:                                # %then4
	.cfi_def_cfa_offset 32
	movl	16(%rsp), %eax
	imull	%eax, %eax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	power_$1, .Lfunc_end1-power_$1
	.cfi_endproc
                                        # -- End function
	.globl	powerset_$2                     # -- Begin function powerset_$2
	.p2align	4, 0x90
	.type	powerset_$2,@function
powerset_$2:                            # @"powerset_$2"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -16
	movq	%rdi, 24(%rsp)
	callq	strlen@PLT
	movl	%eax, 20(%rsp)
	movl	$2, %edi
	movl	%eax, %esi
	callq	power_$1@PLT
	movl	%eax, 16(%rsp)
	movl	$0, 12(%rsp)
	jmp	.LBB2_1
	.p2align	4, 0x90
.LBB2_8:                                # %after6
                                        #   in Loop: Header=BB2_1 Depth=1
	movl	$10, %edi
	callq	writeChar@PLT
	incl	12(%rsp)
.LBB2_1:                                # %cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_3 Depth 2
	movl	12(%rsp), %eax
	cmpl	16(%rsp), %eax
	jge	.LBB2_7
# %bb.2:                                # %body
                                        #   in Loop: Header=BB2_1 Depth=1
	movl	$0, 8(%rsp)
	jmp	.LBB2_3
	.p2align	4, 0x90
.LBB2_6:                                # %after11
                                        #   in Loop: Header=BB2_3 Depth=2
	incl	8(%rsp)
.LBB2_3:                                # %cond4
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	8(%rsp), %eax
	cmpl	20(%rsp), %eax
	jge	.LBB2_8
# %bb.4:                                # %body5
                                        #   in Loop: Header=BB2_3 Depth=2
	movl	12(%rsp), %ebx
	movl	8(%rsp), %esi
	movl	$2, %edi
	callq	power_$1@PLT
	movl	%eax, %ecx
	movl	%ebx, %eax
	cltd
	idivl	%ecx
	movl	%eax, %ecx
	shrl	$31, %ecx
	addl	%eax, %ecx
	andl	$-2, %ecx
	subl	%ecx, %eax
	cmpl	$1, %eax
	jne	.LBB2_6
# %bb.5:                                # %then
                                        #   in Loop: Header=BB2_3 Depth=2
	movslq	8(%rsp), %rax
	movq	24(%rsp), %rcx
	movzbl	(%rcx,%rax), %edi
	callq	writeChar@PLT
	jmp	.LBB2_6
.LBB2_7:                                # %after
	addq	$32, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	powerset_$2, .Lfunc_end2-powerset_$2
	.cfi_endproc
                                        # -- End function
	.type	.Lmyset,@object                 # @myset
	.local	.Lmyset
	.comm	.Lmyset,5,1
	.type	.Labcde,@object                 # @abcde
	.section	.rodata,"a",@progbits
.Labcde:
	.asciz	"abcde"
	.size	.Labcde, 6

	.section	".note.GNU-stack","",@progbits
