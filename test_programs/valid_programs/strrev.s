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
	leaq	".L\n!dlrow olleH"(%rip), %rdi
	callq	reverse_$1@PLT
	leaq	.Lr(%rip), %rdi
	callq	writeString@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	reverse_$1                      # -- Begin function reverse_$1
	.p2align	4, 0x90
	.type	reverse_$1,@function
reverse_$1:                             # @"reverse_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	callq	strlen@PLT
	movl	%eax, 12(%rsp)
	movl	$0, 8(%rsp)
	leaq	.Lr(%rip), %rax
	.p2align	4, 0x90
.LBB1_1:                                # %cond
                                        # =>This Inner Loop Header: Depth=1
	movl	8(%rsp), %ecx
	cmpl	12(%rsp), %ecx
	jge	.LBB1_3
# %bb.2:                                # %body
                                        #   in Loop: Header=BB1_1 Depth=1
	movslq	8(%rsp), %rcx
	movl	%ecx, %edx
	notl	%edx
	addl	12(%rsp), %edx
	movq	16(%rsp), %rsi
	movslq	%edx, %rdx
	movzbl	(%rsi,%rdx), %edx
	movb	%dl, (%rcx,%rax)
	incl	%ecx
	movl	%ecx, 8(%rsp)
	jmp	.LBB1_1
.LBB1_3:                                # %after
	movslq	8(%rsp), %rcx
	movb	$0, (%rcx,%rax)
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	reverse_$1, .Lfunc_end1-reverse_$1
	.cfi_endproc
                                        # -- End function
	.type	.Lr,@object                     # @r
	.local	.Lr
	.comm	.Lr,20,16
	.type	".L\n!dlrow olleH",@object      # @"\0A!dlrow olleH"
	.section	.rodata,"a",@progbits
".L\n!dlrow olleH":
	.asciz	"\n!dlrow olleH"
	.size	".L\n!dlrow olleH", 14

	.section	".note.GNU-stack","",@progbits
