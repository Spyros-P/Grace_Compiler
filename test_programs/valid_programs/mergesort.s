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
	movl	$421742, .LN(%rip)              # imm = 0x66F6E
	movl	$65, .Lseed(%rip)
	movl	$0, .Li(%rip)
	leaq	.Lx(%rip), %rax
	.p2align	4, 0x90
.LBB0_1:                                # %cond
                                        # =>This Inner Loop Header: Depth=1
	movl	.Li(%rip), %ecx
	cmpl	.LN(%rip), %ecx
	jge	.LBB0_3
# %bb.2:                                # %body
                                        #   in Loop: Header=BB0_1 Depth=1
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
	jmp	.LBB0_1
.LBB0_3:                                # %after
	movl	.LN(%rip), %esi
	leaq	".LInitial array: "(%rip), %rdi
	leaq	.Lx(%rip), %rbx
	movq	%rbx, %rdx
	callq	writeArray_$3@PLT
	movl	.LN(%rip), %edx
	decl	%edx
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	mergeSort_$1@PLT
	movl	.LN(%rip), %esi
	leaq	".LSorted  array: "(%rip), %rdi
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
	.globl	mergeSort_$1                    # -- Begin function mergeSort_$1
	.p2align	4, 0x90
	.type	mergeSort_$1,@function
mergeSort_$1:                           # @"mergeSort_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	movl	%esi, 8(%rsp)
	movl	%edx, 4(%rsp)
	movl	8(%rsp), %eax
	cmpl	4(%rsp), %eax
	jge	.LBB1_2
# %bb.1:                                # %then
	movl	8(%rsp), %esi
	movl	4(%rsp), %eax
	subl	%esi, %eax
	movl	%eax, %edx
	shrl	$31, %edx
	addl	%eax, %edx
	sarl	%edx
	addl	%esi, %edx
	movl	%edx, 12(%rsp)
	movq	16(%rsp), %rdi
	callq	mergeSort_$1@PLT
	movq	16(%rsp), %rdi
	movl	12(%rsp), %esi
	incl	%esi
	movl	4(%rsp), %edx
	callq	mergeSort_$1@PLT
	movq	16(%rsp), %rdi
	movl	8(%rsp), %esi
	movl	12(%rsp), %edx
	movl	4(%rsp), %ecx
	callq	merge_$2@PLT
.LBB1_2:                                # %after
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	mergeSort_$1, .Lfunc_end1-mergeSort_$1
	.cfi_endproc
                                        # -- End function
	.globl	merge_$2                        # -- Begin function merge_$2
	.p2align	4, 0x90
	.type	merge_$2,@function
merge_$2:                               # @"merge_$2"
	.cfi_startproc
# %bb.0:                                # %entry
	movq	%rdi, -16(%rsp)
	movl	%esi, -8(%rsp)
	movl	%edx, -20(%rsp)
	movl	%ecx, -24(%rsp)
	movl	%esi, -36(%rsp)
	leaq	.Lstorage(%rip), %rax
	.p2align	4, 0x90
.LBB2_1:                                # %cond
                                        # =>This Inner Loop Header: Depth=1
	movl	-36(%rsp), %ecx
	cmpl	-24(%rsp), %ecx
	jg	.LBB2_3
# %bb.2:                                # %body
                                        #   in Loop: Header=BB2_1 Depth=1
	movslq	-36(%rsp), %rcx
	movq	-16(%rsp), %rdx
	movl	(%rdx,%rcx,4), %edx
	movl	%edx, (%rax,%rcx,4)
	incl	%ecx
	movl	%ecx, -36(%rsp)
	jmp	.LBB2_1
.LBB2_3:                                # %after
	movl	-8(%rsp), %ecx
	movl	%ecx, -36(%rsp)
	movl	%ecx, -28(%rsp)
	movl	-20(%rsp), %ecx
	incl	%ecx
	movl	%ecx, -32(%rsp)
	jmp	.LBB2_4
	.p2align	4, 0x90
.LBB2_7:                                # %then
                                        #   in Loop: Header=BB2_4 Depth=1
	movslq	-36(%rsp), %rcx
	movq	-16(%rsp), %rdx
	movslq	-28(%rsp), %rsi
	movl	(%rax,%rsi,4), %esi
	movl	%esi, (%rdx,%rcx,4)
	incl	-28(%rsp)
	incl	-36(%rsp)
.LBB2_4:                                # %cond13
                                        # =>This Inner Loop Header: Depth=1
	movl	-28(%rsp), %ecx
	cmpl	-20(%rsp), %ecx
	jg	.LBB2_9
# %bb.5:                                # %and_no_short
                                        #   in Loop: Header=BB2_4 Depth=1
	movl	-32(%rsp), %ecx
	cmpl	-24(%rsp), %ecx
	jg	.LBB2_9
# %bb.6:                                # %body14
                                        #   in Loop: Header=BB2_4 Depth=1
	movslq	-28(%rsp), %rcx
	movl	(%rax,%rcx,4), %ecx
	movslq	-32(%rsp), %rdx
	cmpl	(%rax,%rdx,4), %ecx
	jl	.LBB2_7
# %bb.8:                                # %else
                                        #   in Loop: Header=BB2_4 Depth=1
	movslq	-36(%rsp), %rcx
	movq	-16(%rsp), %rdx
	movslq	-32(%rsp), %rsi
	movl	(%rax,%rsi,4), %esi
	movl	%esi, (%rdx,%rcx,4)
	incl	-32(%rsp)
	incl	-36(%rsp)
	jmp	.LBB2_4
	.p2align	4, 0x90
.LBB2_10:                               # %body49
                                        #   in Loop: Header=BB2_9 Depth=1
	movslq	-36(%rsp), %rcx
	movq	-16(%rsp), %rdx
	movslq	-28(%rsp), %rsi
	movl	(%rax,%rsi,4), %esi
	movl	%esi, (%rdx,%rcx,4)
	incl	-28(%rsp)
	incl	-36(%rsp)
.LBB2_9:                                # %cond48
                                        # =>This Inner Loop Header: Depth=1
	movl	-28(%rsp), %ecx
	cmpl	-20(%rsp), %ecx
	jle	.LBB2_10
	jmp	.LBB2_11
	.p2align	4, 0x90
.LBB2_12:                               # %body65
                                        #   in Loop: Header=BB2_11 Depth=1
	movslq	-36(%rsp), %rcx
	movq	-16(%rsp), %rdx
	movslq	-32(%rsp), %rsi
	movl	(%rax,%rsi,4), %esi
	movl	%esi, (%rdx,%rcx,4)
	incl	-32(%rsp)
	incl	-36(%rsp)
.LBB2_11:                               # %cond64
                                        # =>This Inner Loop Header: Depth=1
	movl	-32(%rsp), %ecx
	cmpl	-24(%rsp), %ecx
	jle	.LBB2_12
# %bb.13:                               # %after66
	retq
.Lfunc_end2:
	.size	merge_$2, .Lfunc_end2-merge_$2
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
	.type	.Lstorage,@object               # @storage
	.local	.Lstorage
	.comm	.Lstorage,1686968,16
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
	.type	.LN,@object                     # @N
	.local	.LN
	.comm	.LN,4,4
	.type	.Lx,@object                     # @x
	.local	.Lx
	.comm	.Lx,1686968,16
	.type	".LInitial array: ",@object     # @"Initial array: "
".LInitial array: ":
	.asciz	"Initial array: "
	.size	".LInitial array: ", 16

	.type	".LSorted  array: ",@object     # @"Sorted  array: "
".LSorted  array: ":
	.asciz	"Sorted  array: "
	.size	".LSorted  array: ", 16

	.section	".note.GNU-stack","",@progbits
