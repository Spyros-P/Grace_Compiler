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
	leaq	".LPlease, give me the upper limit: "(%rip), %rdi
	callq	writeString@PLT
	callq	readInteger@PLT
	movl	%eax, .Llimit(%rip)
	leaq	".LPrime numbers between 0 and "(%rip), %rdi
	callq	writeString@PLT
	movl	.Llimit(%rip), %edi
	callq	writeInteger@PLT
	leaq	".L:\n\n"(%rip), %rdi
	callq	writeString@PLT
	movl	$0, .Lcounter(%rip)
	cmpl	$2, .Llimit(%rip)
	jl	.LBB0_2
# %bb.1:                                # %then
	incl	.Lcounter(%rip)
	leaq	".L2\n"(%rip), %rdi
	callq	writeString@PLT
.LBB0_2:                                # %after
	cmpl	$3, .Llimit(%rip)
	jl	.LBB0_4
# %bb.3:                                # %then4
	incl	.Lcounter(%rip)
	leaq	".L3\n"(%rip), %rdi
	callq	writeString@PLT
.LBB0_4:                                # %after5
	movl	$6, .Lnumber(%rip)
	leaq	".L\n"(%rip), %rbx
	jmp	.LBB0_5
	.p2align	4, 0x90
.LBB0_11:                               # %after24
                                        #   in Loop: Header=BB0_5 Depth=1
	addl	$6, .Lnumber(%rip)
.LBB0_5:                                # %cond10
                                        # =>This Inner Loop Header: Depth=1
	movl	.Lnumber(%rip), %eax
	cmpl	.Llimit(%rip), %eax
	jg	.LBB0_12
# %bb.6:                                # %body
                                        #   in Loop: Header=BB0_5 Depth=1
	movl	.Lnumber(%rip), %edi
	decl	%edi
	callq	prime_$1@PLT
	cmpl	$1, %eax
	jne	.LBB0_8
# %bb.7:                                # %then15
                                        #   in Loop: Header=BB0_5 Depth=1
	incl	.Lcounter(%rip)
	movl	.Lnumber(%rip), %edi
	decl	%edi
	callq	writeInteger@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
.LBB0_8:                                # %after16
                                        #   in Loop: Header=BB0_5 Depth=1
	movl	.Lnumber(%rip), %eax
	cmpl	.Llimit(%rip), %eax
	je	.LBB0_11
# %bb.9:                                # %and_no_short
                                        #   in Loop: Header=BB0_5 Depth=1
	movl	.Lnumber(%rip), %edi
	incl	%edi
	callq	prime_$1@PLT
	cmpl	$1, %eax
	jne	.LBB0_11
# %bb.10:                               # %then23
                                        #   in Loop: Header=BB0_5 Depth=1
	incl	.Lcounter(%rip)
	movl	.Lnumber(%rip), %edi
	incl	%edi
	callq	writeInteger@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
	jmp	.LBB0_11
.LBB0_12:                               # %after11
	leaq	".L\n"(%rip), %rdi
	callq	writeString@PLT
	movl	.Lcounter(%rip), %edi
	callq	writeInteger@PLT
	leaq	".L prime number(s) were found.\n"(%rip), %rdi
	callq	writeString@PLT
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	prime_$1                        # -- Begin function prime_$1
	.p2align	4, 0x90
	.type	prime_$1,@function
prime_$1:                               # @"prime_$1"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	%edi, (%rsp)
	testl	%edi, %edi
	js	.LBB1_8
# %bb.1:                                # %else
	cmpl	$1, (%rsp)
	jg	.LBB1_2
.LBB1_9:                                # %then3
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.LBB1_8:                                # %then
	.cfi_def_cfa_offset 16
	xorl	%edi, %edi
	subl	(%rsp), %edi
	callq	prime_$1@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.LBB1_2:                                # %else4
	.cfi_def_cfa_offset 16
	cmpl	$2, (%rsp)
	jne	.LBB1_3
.LBB1_10:                               # %then10
	movl	$1, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.LBB1_3:                                # %else11
	.cfi_def_cfa_offset 16
	movl	(%rsp), %eax
	movl	%eax, %ecx
	shrl	$31, %ecx
	addl	%eax, %ecx
	andl	$-2, %ecx
	cmpl	%ecx, %eax
	je	.LBB1_9
# %bb.4:                                # %else17
	movl	$3, 4(%rsp)
	.p2align	4, 0x90
.LBB1_5:                                # %cond22
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rsp), %eax
	movl	%eax, %ecx
	shrl	$31, %ecx
	addl	%eax, %ecx
	sarl	%ecx
	cmpl	%ecx, 4(%rsp)
	jg	.LBB1_10
# %bb.6:                                # %body
                                        #   in Loop: Header=BB1_5 Depth=1
	movl	(%rsp), %eax
	cltd
	idivl	4(%rsp)
	testl	%edx, %edx
	je	.LBB1_9
# %bb.7:                                # %after28
                                        #   in Loop: Header=BB1_5 Depth=1
	addl	$2, 4(%rsp)
	jmp	.LBB1_5
.Lfunc_end1:
	.size	prime_$1, .Lfunc_end1-prime_$1
	.cfi_endproc
                                        # -- End function
	.type	.Llimit,@object                 # @limit
	.local	.Llimit
	.comm	.Llimit,4,4
	.type	.Lnumber,@object                # @number
	.local	.Lnumber
	.comm	.Lnumber,4,4
	.type	.Lcounter,@object               # @counter
	.local	.Lcounter
	.comm	.Lcounter,4,4
	.type	".LPlease, give me the upper limit: ",@object # @"Please, give me the upper limit: "
	.section	.rodata,"a",@progbits
".LPlease, give me the upper limit: ":
	.asciz	"Please, give me the upper limit: "
	.size	".LPlease, give me the upper limit: ", 34

	.type	".LPrime numbers between 0 and ",@object # @"Prime numbers between 0 and "
".LPrime numbers between 0 and ":
	.asciz	"Prime numbers between 0 and "
	.size	".LPrime numbers between 0 and ", 29

	.type	".L:\n\n",@object               # @":\0A\0A"
".L:\n\n":
	.asciz	":\n\n"
	.size	".L:\n\n", 4

	.type	".L2\n",@object                 # @"2\0A"
".L2\n":
	.asciz	"2\n"
	.size	".L2\n", 3

	.type	".L3\n",@object                 # @"3\0A"
".L3\n":
	.asciz	"3\n"
	.size	".L3\n", 3

	.type	".L\n",@object                  # @"\0A"
".L\n":
	.asciz	"\n"
	.size	".L\n", 2

	.type	".L prime number(s) were found.\n",@object # @" prime number(s) were found.\0A"
".L prime number(s) were found.\n":
	.asciz	" prime number(s) were found.\n"
	.size	".L prime number(s) were found.\n", 30

	.section	".note.GNU-stack","",@progbits
