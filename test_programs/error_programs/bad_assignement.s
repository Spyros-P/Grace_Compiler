	.text
	.file	"grace program"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	movq	.Ly+32(%rip), %rax
	movaps	.Ly+16(%rip), %xmm0
	movaps	.Ly(%rip), %xmm1
	movaps	%xmm1, .Lx(%rip)
	movaps	%xmm0, .Lx+16(%rip)
	movq	%rax, .Lx+32(%rip)
	xorl	%eax, %eax
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lx,@object                     # @x
	.local	.Lx
	.comm	.Lx,40,16
	.type	.Ly,@object                     # @y
	.local	.Ly
	.comm	.Ly,40,16
	.section	".note.GNU-stack","",@progbits
