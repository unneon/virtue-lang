.text
.globl syscall2
syscall2:
        movq %rdi, %rax
        movq %rsi, %rdi
        syscall
        ret

.text
.globl syscall4
syscall4:
        movq %rdi, %rax
        movq %rsi, %rdi
        movq %rdx, %rsi
        movq %rcx, %rdx
        syscall
        ret
