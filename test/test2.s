	.file	"test2.ll"
	.text
	.globl	c_1
	.align	16, 0x90
	.type	c_1,@function
c_1:                                    # @c_1
	.cfi_startproc
# BB#0:                                 # %entry
	vmovsd	%xmm0, -8(%rsp)
	vmovsd	%xmm1, -16(%rsp)
	leaq	-8(%rsp), %rax
	ret
.Ltmp0:
	.size	c_1, .Ltmp0-c_1
	.cfi_endproc

	.globl	True
	.align	16, 0x90
	.type	True,@function
True:                                   # @True
	.cfi_startproc
# BB#0:                                 # %entry
	movl	$c_1, %eax
	ret
.Ltmp1:
	.size	True, .Ltmp1-True
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
