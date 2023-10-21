	.text
	.file	"grace program"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	movl	$0, .Lx(%rip)
	leaq	.Lx(%rip), %r14
	leaq	".Lx = "(%rip), %rbx
	movq	%rbx, %rdi
	callq	writeString@PLT
	movl	.Lx(%rip), %edi
	callq	writeInteger@PLT
	leaq	".L (before inc)\n"(%rip), %rdi
	callq	writeString@PLT
	movq	%r14, %rdi
	callq	inc_$1@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
	movl	.Lx(%rip), %edi
	callq	writeInteger@PLT
	leaq	".L (after inc)\n"(%rip), %rdi
	callq	writeString@PLT
	xorl	%eax, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	inc_$1                          # -- Begin function inc_$1
	.p2align	4, 0x90
	.type	inc_$1,@function
inc_$1:                                 # @"inc_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	movq	%rdi, -8(%rsp)
	incl	(%rdi)
	retq
.Lfunc_end1:
	.size	inc_$1, .Lfunc_end1-inc_$1
	.cfi_endproc
                                        # -- End function
	.type	.Lx,@object                     # @x
	.local	.Lx
	.comm	.Lx,4,4
	.type	".Lx = ",@object                # @"x = "
	.section	.rodata,"a",@progbits
".Lx = ":
	.asciz	"x = "
	.size	".Lx = ", 5

	.type	".L (before inc)\n",@object     # @" (before inc)\0A"
".L (before inc)\n":
	.asciz	" (before inc)\n"
	.size	".L (before inc)\n", 15

	.type	".L (after inc)\n",@object      # @" (after inc)\0A"
".L (after inc)\n":
	.asciz	" (after inc)\n"
	.size	".L (after inc)\n", 14

	.section	".note.GNU-stack","",@progbits
