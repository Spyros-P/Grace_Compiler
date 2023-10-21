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
	movl	$0, .Lx(%rip)
	leaq	".Lx = "(%rip), %rbx
	movq	%rbx, %rdi
	callq	writeString@PLT
	movl	.Lx(%rip), %edi
	callq	writeInteger@PLT
	leaq	".L (before inc)\n"(%rip), %rdi
	callq	writeString@PLT
	addl	$5, .Lx(%rip)
	movq	%rbx, %rdi
	callq	writeString@PLT
	movl	.Lx(%rip), %edi
	callq	writeInteger@PLT
	leaq	".L (after inc)\n"(%rip), %rdi
	callq	writeString@PLT
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
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
