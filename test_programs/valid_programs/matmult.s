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
	movl	$3, .LN(%rip)
	movl	$0, .Li(%rip)
	leaq	.LA(%rip), %rbx
	jmp	.LBB0_1
	.p2align	4, 0x90
.LBB0_5:                                # %after4
                                        #   in Loop: Header=BB0_1 Depth=1
	incl	.Li(%rip)
.LBB0_1:                                # %cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_3 Depth 2
	movl	.Li(%rip), %eax
	cmpl	.LN(%rip), %eax
	jge	.LBB0_6
# %bb.2:                                # %body
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	$0, .Lj(%rip)
	.p2align	4, 0x90
.LBB0_3:                                # %cond2
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	.Lj(%rip), %eax
	cmpl	.LN(%rip), %eax
	jge	.LBB0_5
# %bb.4:                                # %body3
                                        #   in Loop: Header=BB0_3 Depth=2
	callq	readInteger@PLT
	movl	%eax, .Lelement(%rip)
	movslq	.Li(%rip), %rcx
	leaq	(%rcx,%rcx,2), %rcx
	leaq	(%rbx,%rcx,4), %rcx
	movslq	.Lj(%rip), %rdx
	movl	%eax, (%rcx,%rdx,4)
	incl	.Lj(%rip)
	jmp	.LBB0_3
.LBB0_6:                                # %after
	movl	$0, .Li(%rip)
	leaq	.LB(%rip), %rbx
	jmp	.LBB0_7
	.p2align	4, 0x90
.LBB0_11:                               # %after23
                                        #   in Loop: Header=BB0_7 Depth=1
	incl	.Li(%rip)
.LBB0_7:                                # %cond15
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_9 Depth 2
	movl	.Li(%rip), %eax
	cmpl	.LN(%rip), %eax
	jge	.LBB0_12
# %bb.8:                                # %body16
                                        #   in Loop: Header=BB0_7 Depth=1
	movl	$0, .Lj(%rip)
	.p2align	4, 0x90
.LBB0_9:                                # %cond21
                                        #   Parent Loop BB0_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	.Lj(%rip), %eax
	cmpl	.LN(%rip), %eax
	jge	.LBB0_11
# %bb.10:                               # %body22
                                        #   in Loop: Header=BB0_9 Depth=2
	callq	readInteger@PLT
	movl	%eax, .Lelement(%rip)
	movslq	.Li(%rip), %rcx
	leaq	(%rcx,%rcx,2), %rcx
	leaq	(%rbx,%rcx,4), %rcx
	movslq	.Lj(%rip), %rdx
	movl	%eax, (%rcx,%rdx,4)
	incl	.Lj(%rip)
	jmp	.LBB0_9
.LBB0_12:                               # %after17
	movl	.LN(%rip), %ecx
	leaq	.LA(%rip), %rdi
	leaq	.LB(%rip), %rsi
	leaq	.LC(%rip), %rbx
	movq	%rbx, %rdx
	callq	matmult_$2@PLT
	movl	.LN(%rip), %esi
	movq	%rbx, %rdi
	callq	print_$1@PLT
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	print_$1                        # -- Begin function print_$1
	.p2align	4, 0x90
	.type	print_$1,@function
print_$1:                               # @"print_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	movq	%rdi, 16(%rsp)
	movl	%esi, 12(%rsp)
	movl	$0, 8(%rsp)
	leaq	".L "(%rip), %rbx
	leaq	".L\n"(%rip), %r14
	jmp	.LBB1_1
	.p2align	4, 0x90
.LBB1_5:                                # %after4
                                        #   in Loop: Header=BB1_1 Depth=1
	movslq	8(%rsp), %rax
	leaq	(%rax,%rax,2), %rax
	shlq	$2, %rax
	addq	16(%rsp), %rax
	movl	12(%rsp), %ecx
	decl	%ecx
	movslq	%ecx, %rcx
	movl	(%rax,%rcx,4), %edi
	callq	writeInteger@PLT
	movq	%r14, %rdi
	callq	writeString@PLT
	incl	8(%rsp)
.LBB1_1:                                # %cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_3 Depth 2
	movl	12(%rsp), %eax
	decl	%eax
	cmpl	%eax, 8(%rsp)
	jge	.LBB1_6
# %bb.2:                                # %body
                                        #   in Loop: Header=BB1_1 Depth=1
	movl	$0, 4(%rsp)
	.p2align	4, 0x90
.LBB1_3:                                # %cond2
                                        #   Parent Loop BB1_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	12(%rsp), %eax
	decl	%eax
	cmpl	%eax, 4(%rsp)
	jge	.LBB1_5
# %bb.4:                                # %body3
                                        #   in Loop: Header=BB1_3 Depth=2
	movslq	8(%rsp), %rax
	leaq	(%rax,%rax,2), %rax
	shlq	$2, %rax
	addq	16(%rsp), %rax
	movslq	4(%rsp), %rcx
	movl	(%rax,%rcx,4), %edi
	callq	writeInteger@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
	incl	4(%rsp)
	jmp	.LBB1_3
.LBB1_6:                                # %after
	movl	12(%rsp), %eax
	decl	%eax
	movl	%eax, 8(%rsp)
	movl	$0, 4(%rsp)
	leaq	".L "(%rip), %rbx
	.p2align	4, 0x90
.LBB1_7:                                # %cond26
                                        # =>This Inner Loop Header: Depth=1
	movl	12(%rsp), %eax
	decl	%eax
	cmpl	%eax, 4(%rsp)
	jge	.LBB1_9
# %bb.8:                                # %body27
                                        #   in Loop: Header=BB1_7 Depth=1
	movslq	8(%rsp), %rax
	leaq	(%rax,%rax,2), %rax
	shlq	$2, %rax
	addq	16(%rsp), %rax
	movslq	4(%rsp), %rcx
	movl	(%rax,%rcx,4), %edi
	callq	writeInteger@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
	incl	4(%rsp)
	jmp	.LBB1_7
.LBB1_9:                                # %after28
	movslq	8(%rsp), %rax
	leaq	(%rax,%rax,2), %rax
	shlq	$2, %rax
	addq	16(%rsp), %rax
	movl	12(%rsp), %ecx
	decl	%ecx
	movslq	%ecx, %rcx
	movl	(%rax,%rcx,4), %edi
	callq	writeInteger@PLT
	leaq	".L\n"(%rip), %rdi
	callq	writeString@PLT
	addq	$24, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	print_$1, .Lfunc_end1-print_$1
	.cfi_endproc
                                        # -- End function
	.globl	matmult_$2                      # -- Begin function matmult_$2
	.p2align	4, 0x90
	.type	matmult_$2,@function
matmult_$2:                             # @"matmult_$2"
	.cfi_startproc
# %bb.0:                                # %entry
	movq	%rdi, -8(%rsp)
	movq	%rsi, -16(%rsp)
	movq	%rdx, -24(%rsp)
	movl	%ecx, -36(%rsp)
	movl	$0, -40(%rsp)
	jmp	.LBB2_1
	.p2align	4, 0x90
.LBB2_9:                                # %after4
                                        #   in Loop: Header=BB2_1 Depth=1
	incl	-40(%rsp)
.LBB2_1:                                # %cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_3 Depth 2
                                        #       Child Loop BB2_5 Depth 3
	movl	-40(%rsp), %eax
	cmpl	-36(%rsp), %eax
	jge	.LBB2_8
# %bb.2:                                # %body
                                        #   in Loop: Header=BB2_1 Depth=1
	movl	$0, -44(%rsp)
	jmp	.LBB2_3
	.p2align	4, 0x90
.LBB2_7:                                # %after10
                                        #   in Loop: Header=BB2_3 Depth=2
	movslq	-40(%rsp), %rax
	leaq	(%rax,%rax,2), %rax
	shlq	$2, %rax
	addq	-24(%rsp), %rax
	movslq	-44(%rsp), %rcx
	movl	-28(%rsp), %edx
	movl	%edx, (%rax,%rcx,4)
	incl	-44(%rsp)
.LBB2_3:                                # %cond2
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_5 Depth 3
	movl	-44(%rsp), %eax
	cmpl	-36(%rsp), %eax
	jge	.LBB2_9
# %bb.4:                                # %body3
                                        #   in Loop: Header=BB2_3 Depth=2
	movl	$0, -32(%rsp)
	movl	$0, -28(%rsp)
	.p2align	4, 0x90
.LBB2_5:                                # %cond8
                                        #   Parent Loop BB2_1 Depth=1
                                        #     Parent Loop BB2_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	-32(%rsp), %eax
	cmpl	-36(%rsp), %eax
	jge	.LBB2_7
# %bb.6:                                # %body9
                                        #   in Loop: Header=BB2_5 Depth=3
	movslq	-40(%rsp), %rax
	leaq	(%rax,%rax,2), %rax
	shlq	$2, %rax
	addq	-8(%rsp), %rax
	movslq	-32(%rsp), %rcx
	movl	(%rax,%rcx,4), %eax
	leaq	(%rcx,%rcx,2), %rdx
	shlq	$2, %rdx
	addq	-16(%rsp), %rdx
	movslq	-44(%rsp), %rsi
	imull	(%rdx,%rsi,4), %eax
	addl	%eax, -28(%rsp)
	leal	1(%rcx), %eax
	movl	%eax, -32(%rsp)
	jmp	.LBB2_5
.LBB2_8:                                # %after
	retq
.Lfunc_end2:
	.size	matmult_$2, .Lfunc_end2-matmult_$2
	.cfi_endproc
                                        # -- End function
	.type	".L ",@object                   # @" "
	.section	.rodata,"a",@progbits
".L ":
	.asciz	" "
	.size	".L ", 2

	.type	".L\n",@object                  # @"\0A"
".L\n":
	.asciz	"\n"
	.size	".L\n", 2

	.type	.LA,@object                     # @A
	.local	.LA
	.comm	.LA,36,16
	.type	.LB,@object                     # @B
	.local	.LB
	.comm	.LB,36,16
	.type	.LC,@object                     # @C
	.local	.LC
	.comm	.LC,36,16
	.type	.LN,@object                     # @N
	.local	.LN
	.comm	.LN,4,4
	.type	.Li,@object                     # @i
	.local	.Li
	.comm	.Li,4,4
	.type	.Lj,@object                     # @j
	.local	.Lj
	.comm	.Lj,4,4
	.type	.Lelement,@object               # @element
	.local	.Lelement
	.comm	.Lelement,4,4
	.section	".note.GNU-stack","",@progbits
