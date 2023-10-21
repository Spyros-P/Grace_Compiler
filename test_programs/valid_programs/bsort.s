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
	movl	$65, .Lseed(%rip)
	movl	$0, .Li(%rip)
	leaq	.Lx(%rip), %rax
	cmpl	$15, .Li(%rip)
	jg	.LBB0_3
	.p2align	4, 0x90
.LBB0_2:                                # %body
                                        # =>This Inner Loop Header: Depth=1
	imull	$137, .Lseed(%rip), %ecx
	movl	.Li(%rip), %edx
	leal	221(%rcx,%rdx), %ecx
	movslq	%ecx, %rcx
	imulq	$680390859, %rcx, %rsi          # imm = 0x288DF0CB
	movq	%rsi, %rdi
	shrq	$63, %rdi
	sarq	$36, %rsi
	addl	%edi, %esi
	imull	$101, %esi, %esi
	subl	%esi, %ecx
	movl	%ecx, .Lseed(%rip)
	movslq	.Li(%rip), %rsi
	movl	%ecx, (%rax,%rsi,4)
	incl	%edx
	movl	%edx, .Li(%rip)
	cmpl	$15, .Li(%rip)
	jle	.LBB0_2
.LBB0_3:                                # %after
	leaq	".LInitial array: "(%rip), %rdi
	leaq	.Lx(%rip), %rbx
	movl	$16, %esi
	movq	%rbx, %rdx
	callq	writeArray_$3@PLT
	movl	$16, %edi
	movq	%rbx, %rsi
	callq	bsort_$1@PLT
	leaq	".LSorted  array: "(%rip), %rdi
	movl	$16, %esi
	movq	%rbx, %rdx
	callq	writeArray_$3@PLT
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	bsort_$1                        # -- Begin function bsort_$1
	.p2align	4, 0x90
	.type	bsort_$1,@function
bsort_$1:                               # @"bsort_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movl	%edi, 12(%rsp)
	movq	%rsi, 16(%rsp)
	movl	$1, 8(%rsp)
	.p2align	4, 0x90
.LBB1_1:                                # %cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_3 Depth 2
	cmpl	$0, 8(%rsp)
	jle	.LBB1_7
# %bb.2:                                # %body
                                        #   in Loop: Header=BB1_1 Depth=1
	movl	$0, 8(%rsp)
	movl	$0, 4(%rsp)
	jmp	.LBB1_3
	.p2align	4, 0x90
.LBB1_6:                                # %after7
                                        #   in Loop: Header=BB1_3 Depth=2
	incl	4(%rsp)
.LBB1_3:                                # %cond1
                                        #   Parent Loop BB1_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	12(%rsp), %eax
	decl	%eax
	cmpl	%eax, 4(%rsp)
	jge	.LBB1_1
# %bb.4:                                # %body2
                                        #   in Loop: Header=BB1_3 Depth=2
	movslq	4(%rsp), %rax
	movq	16(%rsp), %rcx
	movl	(%rcx,%rax,4), %edx
	incl	%eax
	cltq
	cmpl	(%rcx,%rax,4), %edx
	jle	.LBB1_6
# %bb.5:                                # %then
                                        #   in Loop: Header=BB1_3 Depth=2
	movslq	4(%rsp), %rax
	movq	16(%rsp), %rcx
	leaq	(%rcx,%rax,4), %rdi
	incl	%eax
	cltq
	leaq	(%rcx,%rax,4), %rsi
	callq	swap_$2@PLT
	movl	$1, 8(%rsp)
	jmp	.LBB1_6
.LBB1_7:                                # %after
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	bsort_$1, .Lfunc_end1-bsort_$1
	.cfi_endproc
                                        # -- End function
	.globl	swap_$2                         # -- Begin function swap_$2
	.p2align	4, 0x90
	.type	swap_$2,@function
swap_$2:                                # @"swap_$2"
	.cfi_startproc
# %bb.0:                                # %entry
	movq	%rdi, -8(%rsp)
	movq	%rsi, -16(%rsp)
	movl	(%rdi), %eax
	movl	%eax, -20(%rsp)
	movl	(%rsi), %eax
	movl	%eax, (%rdi)
	movq	-16(%rsp), %rax
	movl	-20(%rsp), %ecx
	movl	%ecx, (%rax)
	retq
.Lfunc_end2:
	.size	swap_$2, .Lfunc_end2-swap_$2
	.cfi_endproc
                                        # -- End function
	.globl	writeArray_$3                   # -- Begin function writeArray_$3
	.p2align	4, 0x90
	.type	writeArray_$3,@function
writeArray_$3:                          # @"writeArray_$3"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -16
	movq	%rdi, 24(%rsp)
	movl	%esi, 12(%rsp)
	movq	%rdx, 16(%rsp)
	callq	writeString@PLT
	movl	$0, 8(%rsp)
	leaq	".L, "(%rip), %rbx
	jmp	.LBB3_1
	.p2align	4, 0x90
.LBB3_4:                                # %after4
                                        #   in Loop: Header=BB3_1 Depth=1
	movslq	8(%rsp), %rax
	movq	16(%rsp), %rcx
	movl	(%rcx,%rax,4), %edi
	callq	writeInteger@PLT
	incl	8(%rsp)
.LBB3_1:                                # %cond
                                        # =>This Inner Loop Header: Depth=1
	movl	8(%rsp), %eax
	cmpl	12(%rsp), %eax
	jge	.LBB3_5
# %bb.2:                                # %body
                                        #   in Loop: Header=BB3_1 Depth=1
	cmpl	$0, 8(%rsp)
	jle	.LBB3_4
# %bb.3:                                # %then
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	%rbx, %rdi
	callq	writeString@PLT
	jmp	.LBB3_4
.LBB3_5:                                # %after
	leaq	".L\n"(%rip), %rdi
	callq	writeString@PLT
	addq	$32, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end3:
	.size	writeArray_$3, .Lfunc_end3-writeArray_$3
	.cfi_endproc
                                        # -- End function
	.type	".L, ",@object                  # @", "
	.section	.rodata,"a",@progbits
".L, ":
	.asciz	", "
	.size	".L, ", 3

	.type	".L\n",@object                  # @"\0A"
".L\n":
	.asciz	"\n"
	.size	".L\n", 2

	.type	.Lseed,@object                  # @seed
	.local	.Lseed
	.comm	.Lseed,4,4
	.type	.Li,@object                     # @i
	.local	.Li
	.comm	.Li,4,4
	.type	.Lx,@object                     # @x
	.local	.Lx
	.comm	.Lx,64,16
	.type	".LInitial array: ",@object     # @"Initial array: "
".LInitial array: ":
	.asciz	"Initial array: "
	.size	".LInitial array: ", 16

	.type	".LSorted  array: ",@object     # @"Sorted  array: "
".LSorted  array: ":
	.asciz	"Sorted  array: "
	.size	".LSorted  array: ", 16

	.section	".note.GNU-stack","",@progbits
