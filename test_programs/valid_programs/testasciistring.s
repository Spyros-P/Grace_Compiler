	.text
	.file	"grace program"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%r15
	.cfi_def_cfa_offset 16
	pushq	%r14
	.cfi_def_cfa_offset 24
	pushq	%r13
	.cfi_def_cfa_offset 32
	pushq	%r12
	.cfi_def_cfa_offset 40
	pushq	%rbx
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -48
	.cfi_offset %r12, -40
	.cfi_offset %r13, -32
	.cfi_offset %r14, -24
	.cfi_offset %r15, -16
	leaq	.Lalphabet(%rip), %r15
	leaq	.Labcdefghijklmnopqrstuvwxyz(%rip), %rsi
	movq	%r15, %rdi
	callq	strcpy@PLT
	leaq	.Lcapitals(%rip), %r14
	leaq	.LABCDEFGHIJKLMNOPQRSTUVWXYZ(%rip), %rsi
	movq	%r14, %rdi
	callq	strcpy@PLT
	movl	$26, .Llimit(%rip)
	movl	$0, .Lcounter(%rip)
	leaq	".LTesting lowercase alphabet:\n"(%rip), %rdi
	callq	writeString@PLT
	leaq	".LThe ASCII value of "(%rip), %r12
	leaq	".L is "(%rip), %r13
	leaq	".L\n"(%rip), %rbx
	.p2align	4, 0x90
.LBB0_1:                                # %cond
                                        # =>This Inner Loop Header: Depth=1
	movl	.Lcounter(%rip), %eax
	cmpl	.Llimit(%rip), %eax
	jge	.LBB0_3
# %bb.2:                                # %body
                                        #   in Loop: Header=BB0_1 Depth=1
	movslq	.Lcounter(%rip), %rax
	movzbl	(%rax,%r15), %edi
	movb	%dil, .Ltempchar(%rip)
	callq	ascii@PLT
	movl	%eax, .Ltempascii(%rip)
	movq	%r12, %rdi
	callq	writeString@PLT
	movzbl	.Ltempchar(%rip), %edi
	callq	writeChar@PLT
	movq	%r13, %rdi
	callq	writeString@PLT
	movl	.Ltempascii(%rip), %edi
	callq	writeInteger@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
	incl	.Lcounter(%rip)
	jmp	.LBB0_1
.LBB0_3:                                # %after
	leaq	".LTesting uppercase alphabet:\n"(%rip), %rdi
	callq	writeString@PLT
	movl	$0, .Lcounter(%rip)
	leaq	".LThe ASCII value of "(%rip), %r15
	leaq	".L is "(%rip), %r12
	leaq	".L\n"(%rip), %rbx
	.p2align	4, 0x90
.LBB0_4:                                # %cond8
                                        # =>This Inner Loop Header: Depth=1
	movl	.Lcounter(%rip), %eax
	cmpl	.Llimit(%rip), %eax
	jge	.LBB0_6
# %bb.5:                                # %body9
                                        #   in Loop: Header=BB0_4 Depth=1
	movslq	.Lcounter(%rip), %rax
	movzbl	(%rax,%r14), %edi
	movb	%dil, .Ltempchar(%rip)
	callq	ascii@PLT
	movl	%eax, .Ltempascii(%rip)
	movq	%r15, %rdi
	callq	writeString@PLT
	movzbl	.Ltempchar(%rip), %edi
	callq	writeChar@PLT
	movq	%r12, %rdi
	callq	writeString@PLT
	movl	.Ltempascii(%rip), %edi
	callq	writeInteger@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
	incl	.Lcounter(%rip)
	jmp	.LBB0_4
.LBB0_6:                                # %after10
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lalphabet,@object              # @alphabet
	.local	.Lalphabet
	.comm	.Lalphabet,26,16
	.type	.Lcapitals,@object              # @capitals
	.local	.Lcapitals
	.comm	.Lcapitals,26,16
	.type	.Ltempchar,@object              # @tempchar
	.local	.Ltempchar
	.comm	.Ltempchar,1,1
	.type	.Ltempascii,@object             # @tempascii
	.local	.Ltempascii
	.comm	.Ltempascii,4,4
	.type	.Llimit,@object                 # @limit
	.local	.Llimit
	.comm	.Llimit,4,4
	.type	.Lcounter,@object               # @counter
	.local	.Lcounter
	.comm	.Lcounter,4,4
	.type	.Labcdefghijklmnopqrstuvwxyz,@object # @abcdefghijklmnopqrstuvwxyz
	.section	.rodata,"a",@progbits
.Labcdefghijklmnopqrstuvwxyz:
	.asciz	"abcdefghijklmnopqrstuvwxyz"
	.size	.Labcdefghijklmnopqrstuvwxyz, 27

	.type	.LABCDEFGHIJKLMNOPQRSTUVWXYZ,@object # @ABCDEFGHIJKLMNOPQRSTUVWXYZ
.LABCDEFGHIJKLMNOPQRSTUVWXYZ:
	.asciz	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	.size	.LABCDEFGHIJKLMNOPQRSTUVWXYZ, 27

	.type	".LTesting lowercase alphabet:\n",@object # @"Testing lowercase alphabet:\0A"
".LTesting lowercase alphabet:\n":
	.asciz	"Testing lowercase alphabet:\n"
	.size	".LTesting lowercase alphabet:\n", 29

	.type	".LThe ASCII value of ",@object # @"The ASCII value of "
".LThe ASCII value of ":
	.asciz	"The ASCII value of "
	.size	".LThe ASCII value of ", 20

	.type	".L is ",@object                # @" is "
".L is ":
	.asciz	" is "
	.size	".L is ", 5

	.type	".L\n",@object                  # @"\0A"
".L\n":
	.asciz	"\n"
	.size	".L\n", 2

	.type	".LTesting uppercase alphabet:\n",@object # @"Testing uppercase alphabet:\0A"
".LTesting uppercase alphabet:\n":
	.asciz	"Testing uppercase alphabet:\n"
	.size	".LTesting uppercase alphabet:\n", 29

	.section	".note.GNU-stack","",@progbits
