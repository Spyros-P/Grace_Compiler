	.text
	.file	"grace program"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%r15
	.cfi_def_cfa_offset 24
	pushq	%r14
	.cfi_def_cfa_offset 32
	pushq	%r13
	.cfi_def_cfa_offset 40
	pushq	%r12
	.cfi_def_cfa_offset 48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	pushq	%rax
	.cfi_def_cfa_offset 64
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	.cfi_offset %rbp, -16
	movabsq	$180388626449, %rax             # imm = 0x2A00000011
	movq	%rax, .Lx(%rip)
	leaq	.Lx(%rip), %r14
	leaq	.Lx+4(%rip), %r12
	leaq	".Lx[0] = "(%rip), %rdi
	callq	writeString@PLT
	movl	.Lx(%rip), %edi
	callq	writeInteger@PLT
	leaq	".L\n"(%rip), %r13
	movq	%r13, %rdi
	callq	writeString@PLT
	leaq	".Lx[1] = "(%rip), %r15
	movq	%r15, %rdi
	callq	writeString@PLT
	movl	.Lx+4(%rip), %edi
	callq	writeInteger@PLT
	leaq	".L\n\nSWAPPP\n"(%rip), %rbp
	movq	%rbp, %rdi
	callq	writeString@PLT
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	swap_$1@PLT
	leaq	".L\nx[0] = "(%rip), %rbx
	movq	%rbx, %rdi
	callq	writeString@PLT
	movl	.Lx(%rip), %edi
	callq	writeInteger@PLT
	movq	%r13, %rdi
	callq	writeString@PLT
	movq	%r15, %rdi
	callq	writeString@PLT
	movl	.Lx+4(%rip), %edi
	callq	writeInteger@PLT
	movq	%rbp, %rdi
	callq	writeString@PLT
	movq	%r14, %rdi
	callq	swap_2_$2@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
	movl	.Lx(%rip), %edi
	callq	writeInteger@PLT
	movq	%r13, %rdi
	callq	writeString@PLT
	movq	%r15, %rdi
	callq	writeString@PLT
	movl	.Lx+4(%rip), %edi
	callq	writeInteger@PLT
	movq	%r13, %rdi
	callq	writeString@PLT
	xorl	%eax, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%r12
	.cfi_def_cfa_offset 40
	popq	%r13
	.cfi_def_cfa_offset 32
	popq	%r14
	.cfi_def_cfa_offset 24
	popq	%r15
	.cfi_def_cfa_offset 16
	popq	%rbp
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
	.globl	swap_2_$2                       # -- Begin function swap_2_$2
	.p2align	4, 0x90
	.type	swap_2_$2,@function
swap_2_$2:                              # @"swap_2_$2"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	leaq	4(%rdi), %rsi
	callq	swap_$1@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	swap_2_$2, .Lfunc_end2-swap_2_$2
	.cfi_endproc
                                        # -- End function
	.type	.Lx,@object                     # @x
	.local	.Lx
	.comm	.Lx,8,4
	.type	".Lx[0] = ",@object             # @"x[0] = "
	.section	.rodata,"a",@progbits
".Lx[0] = ":
	.asciz	"x[0] = "
	.size	".Lx[0] = ", 8

	.type	".L\n",@object                  # @"\0A"
".L\n":
	.asciz	"\n"
	.size	".L\n", 2

	.type	".Lx[1] = ",@object             # @"x[1] = "
".Lx[1] = ":
	.asciz	"x[1] = "
	.size	".Lx[1] = ", 8

	.type	".L\n\nSWAPPP\n",@object        # @"\0A\0ASWAPPP\0A"
".L\n\nSWAPPP\n":
	.asciz	"\n\nSWAPPP\n"
	.size	".L\n\nSWAPPP\n", 10

	.type	".L\nx[0] = ",@object           # @"\0Ax[0] = "
".L\nx[0] = ":
	.asciz	"\nx[0] = "
	.size	".L\nx[0] = ", 9

	.section	".note.GNU-stack","",@progbits
