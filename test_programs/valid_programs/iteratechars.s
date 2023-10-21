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
	movl	$26, .Llimit(%rip)
	movl	$0, .Lcounter(%rip)
	movb	$65, .Lmychar(%rip)
	movl	$65, %edi
	callq	ascii@PLT
	movl	%eax, .Lmyascii(%rip)
	leaq	".LThe ASCII code for "(%rip), %r14
	leaq	".L is "(%rip), %rbx
	.p2align	4, 0x90
.LBB0_1:                                # %cond
                                        # =>This Inner Loop Header: Depth=1
	movl	.Lcounter(%rip), %eax
	cmpl	.Llimit(%rip), %eax
	jge	.LBB0_3
# %bb.2:                                # %body
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r14, %rdi
	callq	writeString@PLT
	movzbl	.Lmychar(%rip), %edi
	callq	writeChar@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
	movl	.Lmyascii(%rip), %edi
	callq	writeInteger@PLT
	movl	$10, %edi
	callq	writeChar@PLT
	movl	.Lmyascii(%rip), %edi
	incl	%edi
	callq	chr@PLT
	movb	%al, .Lmychar(%rip)
	movzbl	%al, %edi
	callq	ascii@PLT
	movl	%eax, .Lmyascii(%rip)
	incl	.Lcounter(%rip)
	jmp	.LBB0_1
.LBB0_3:                                # %after
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
	.type	.Lmychar,@object                # @mychar
	.local	.Lmychar
	.comm	.Lmychar,1,1
	.type	.Lmyascii,@object               # @myascii
	.local	.Lmyascii
	.comm	.Lmyascii,4,4
	.type	.Llimit,@object                 # @limit
	.local	.Llimit
	.comm	.Llimit,4,4
	.type	.Lcounter,@object               # @counter
	.local	.Lcounter
	.comm	.Lcounter,4,4
	.type	".LThe ASCII code for ",@object # @"The ASCII code for "
	.section	.rodata,"a",@progbits
".LThe ASCII code for ":
	.asciz	"The ASCII code for "
	.size	".LThe ASCII code for ", 20

	.type	".L is ",@object                # @" is "
".L is ":
	.asciz	" is "
	.size	".L is ", 5

	.section	".note.GNU-stack","",@progbits
