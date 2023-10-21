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
	movl	$421742, .LN(%rip)              # imm = 0x66F6E
	movl	$0, .Li(%rip)
	leaq	.Ldata(%rip), %rax
	.p2align	4, 0x90
.LBB0_1:                                # %cond
                                        # =>This Inner Loop Header: Depth=1
	movl	.Li(%rip), %ecx
	cmpl	.LN(%rip), %ecx
	jge	.LBB0_3
# %bb.2:                                # %body
                                        #   in Loop: Header=BB0_1 Depth=1
	movslq	.Li(%rip), %rcx
	leal	(%rcx,%rcx,2), %edx
	movslq	%edx, %rdx
	imulq	$1717986919, %rdx, %rsi         # imm = 0x66666667
	movq	%rsi, %rdi
	shrq	$63, %rdi
	sarq	$34, %rsi
	addl	%edi, %esi
	addl	%esi, %esi
	leal	(%rsi,%rsi,4), %esi
	subl	%esi, %edx
	leal	10(%rcx), %esi
	imull	%ecx, %esi
	addl	%edx, %esi
	movslq	%esi, %rdx
	imulq	$274877907, %rdx, %rsi          # imm = 0x10624DD3
	movq	%rsi, %rdi
	shrq	$63, %rdi
	sarq	$38, %rsi
	addl	%edi, %esi
	imull	$1000, %esi, %esi               # imm = 0x3E8
	subl	%esi, %edx
	movl	%edx, (%rax,%rcx,4)
	incl	%ecx
	movl	%ecx, .Li(%rip)
	jmp	.LBB0_1
.LBB0_3:                                # %after
	leaq	".LUnsorted Array\n"(%rip), %rdi
	callq	writeString@PLT
	movl	.LN(%rip), %edx
	decl	%edx
	leaq	.Ldata(%rip), %rdi
	xorl	%esi, %esi
	callq	quickSort_$3@PLT
	leaq	".LSorted array in ascending order: \n"(%rip), %rdi
	callq	writeString@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	swap_$1                         # -- Begin function swap_$1
	.p2align	4, 0x90
	.type	swap_$1,@function
swap_$1:                                # @"swap_$1"
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
.Lfunc_end1:
	.size	swap_$1, .Lfunc_end1-swap_$1
	.cfi_endproc
                                        # -- End function
	.globl	partition_$2                    # -- Begin function partition_$2
	.p2align	4, 0x90
	.type	partition_$2,@function
partition_$2:                           # @"partition_$2"
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
                                        # kill: def $esi killed $esi def $rsi
	movq	%rdi, 24(%rsp)
	movl	%esi, 36(%rsp)
	movl	%edx, 20(%rsp)
	movslq	%edx, %rax
	movl	(%rdi,%rax,4), %eax
	movl	%eax, 32(%rsp)
	leal	-1(%rsi), %eax
	movl	%eax, 16(%rsp)
	movl	%esi, 12(%rsp)
	jmp	.LBB2_1
	.p2align	4, 0x90
.LBB2_4:                                # %after8
                                        #   in Loop: Header=BB2_1 Depth=1
	incl	12(%rsp)
.LBB2_1:                                # %cond
                                        # =>This Inner Loop Header: Depth=1
	movl	12(%rsp), %eax
	cmpl	20(%rsp), %eax
	jge	.LBB2_5
# %bb.2:                                # %body
                                        #   in Loop: Header=BB2_1 Depth=1
	movslq	12(%rsp), %rax
	movq	24(%rsp), %rcx
	movl	(%rcx,%rax,4), %eax
	cmpl	32(%rsp), %eax
	jg	.LBB2_4
# %bb.3:                                # %then
                                        #   in Loop: Header=BB2_1 Depth=1
	movl	16(%rsp), %eax
	incl	%eax
	movl	%eax, 16(%rsp)
	cltq
	movq	24(%rsp), %rcx
	leaq	(%rcx,%rax,4), %rdi
	movslq	12(%rsp), %rax
	leaq	(%rcx,%rax,4), %rsi
	callq	swap_$1@PLT
	jmp	.LBB2_4
.LBB2_5:                                # %after
	movl	16(%rsp), %eax
	incl	%eax
	movq	24(%rsp), %rcx
	cltq
	leaq	(%rcx,%rax,4), %rdi
	movslq	20(%rsp), %rax
	leaq	(%rcx,%rax,4), %rsi
	callq	swap_$1@PLT
	movl	16(%rsp), %eax
	incl	%eax
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	partition_$2, .Lfunc_end2-partition_$2
	.cfi_endproc
                                        # -- End function
	.globl	quickSort_$3                    # -- Begin function quickSort_$3
	.p2align	4, 0x90
	.type	quickSort_$3,@function
quickSort_$3:                           # @"quickSort_$3"
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 8(%rsp)
	movl	%esi, 4(%rsp)
	movl	%edx, (%rsp)
	movl	4(%rsp), %eax
	cmpl	(%rsp), %eax
	jge	.LBB3_2
# %bb.1:                                # %then
	movq	8(%rsp), %rdi
	movl	4(%rsp), %esi
	movl	(%rsp), %edx
	callq	partition_$2@PLT
                                        # kill: def $eax killed $eax def $rax
	movl	%eax, 20(%rsp)
	movq	8(%rsp), %rdi
	movl	4(%rsp), %esi
	leal	-1(%rax), %edx
	callq	quickSort_$3@PLT
	movq	8(%rsp), %rdi
	movl	20(%rsp), %esi
	incl	%esi
	movl	(%rsp), %edx
	callq	quickSort_$3@PLT
.LBB3_2:                                # %after
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end3:
	.size	quickSort_$3, .Lfunc_end3-quickSort_$3
	.cfi_endproc
                                        # -- End function
	.globl	printArray_$4                   # -- Begin function printArray_$4
	.p2align	4, 0x90
	.type	printArray_$4,@function
printArray_$4:                          # @"printArray_$4"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movq	%rdi, 8(%rsp)
	movl	%esi, 4(%rsp)
	movl	$0, (%rsp)
	leaq	".L  "(%rip), %rbx
	.p2align	4, 0x90
.LBB4_1:                                # %cond
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rsp), %eax
	cmpl	4(%rsp), %eax
	jge	.LBB4_3
# %bb.2:                                # %body
                                        #   in Loop: Header=BB4_1 Depth=1
	movslq	(%rsp), %rax
	movq	8(%rsp), %rcx
	movl	(%rcx,%rax,4), %edi
	callq	writeInteger@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
	incl	(%rsp)
	jmp	.LBB4_1
.LBB4_3:                                # %after
	leaq	".L\n"(%rip), %rdi
	callq	writeString@PLT
	addq	$16, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end4:
	.size	printArray_$4, .Lfunc_end4-printArray_$4
	.cfi_endproc
                                        # -- End function
	.type	".L  ",@object                  # @"  "
	.section	.rodata,"a",@progbits
".L  ":
	.asciz	"  "
	.size	".L  ", 3

	.type	".L\n",@object                  # @"\0A"
".L\n":
	.asciz	"\n"
	.size	".L\n", 2

	.type	.Ldata,@object                  # @data
	.local	.Ldata
	.comm	.Ldata,1686968000,16
	.type	.Li,@object                     # @i
	.local	.Li
	.comm	.Li,4,4
	.type	.LN,@object                     # @N
	.local	.LN
	.comm	.LN,4,4
	.type	".LUnsorted Array\n",@object    # @"Unsorted Array\0A"
".LUnsorted Array\n":
	.asciz	"Unsorted Array\n"
	.size	".LUnsorted Array\n", 16

	.type	".LSorted array in ascending order: \n",@object # @"Sorted array in ascending order: \0A"
".LSorted array in ascending order: \n":
	.asciz	"Sorted array in ascending order: \n"
	.size	".LSorted array in ascending order: \n", 35

	.section	".note.GNU-stack","",@progbits
