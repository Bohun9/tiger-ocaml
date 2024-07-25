.globl tigermain
.section .data
L5:
    .quad 2
    .ascii " O"
L6:
    .quad 2
    .ascii " ."
L12:
    .quad 1
    .ascii "\n"
L15:
    .quad 1
    .ascii "\n"
.section .text
L1:
    push %rbp
    movq %rsp, %rbp
    subq $48, %rsp
    movq %rdi, -8(%rbp)
    movq $0, %rax
    movq %rax, -32(%rbp)
    movq -8(%rbp), %rax
    movq -8(%rax), %rax
    movq $1, %rcx
    subq %rcx, %rax
    movq %rax, -16(%rbp)
L3:
    movq -16(%rbp), %rcx
    movq -32(%rbp), %rax
    cmpq %rax, %rcx
    jge L13
L14:
    movq $L15, %rdi
    call print
    jmp L37
L13:
    movq $0, %rax
    movq %rax, -40(%rbp)
    movq -8(%rbp), %rax
    movq -8(%rax), %rax
    movq $1, %rcx
    subq %rcx, %rax
    movq %rax, -24(%rbp)
L4:
    movq -24(%rbp), %rcx
    movq -40(%rbp), %rax
    cmpq %rax, %rcx
    jge L10
L11:
    movq $L12, %rdi
    call print
    movq -32(%rbp), %rax
    leaq 1(%rax), %rax
    movq %rax, -32(%rbp)
    jmp L3
L10:
    movq -8(%rbp), %rax
    movq -24(%rax), %rcx
    movq -40(%rbp), %rax
    movq (%rcx, %rax, 8), %rcx
    movq -32(%rbp), %rax
    cmpq %rcx, %rax
    je L7
L8:
    movq $L6, %rdi
L9:
    call print
    movq -40(%rbp), %rax
    leaq 1(%rax), %rax
    movq %rax, -40(%rbp)
    jmp L4
L7:
    movq $L5, %rdi
    jmp L9
L37:
    addq $48, %rsp
    pop %rbp
    ret
L2:
    push %rbp
    movq %rsp, %rbp
    subq $48, %rsp
    movq %rdi, -8(%rbp)
    movq %rsi, -24(%rbp)
    movq -8(%rbp), %rax
    movq -8(%rax), %rcx
    movq -24(%rbp), %rax
    cmpq %rax, %rcx
    je L32
L33:
    movq $0, %rax
    movq %rax, -32(%rbp)
    movq -8(%rbp), %rax
    movq -8(%rax), %rax
    movq $1, %rcx
    subq %rcx, %rax
    movq %rax, -16(%rbp)
L16:
    movq -16(%rbp), %rcx
    movq -32(%rbp), %rax
    cmpq %rax, %rcx
    jge L30
L31:
    movq $0, %rax
L34:
    jmp L36
L32:
    movq -8(%rbp), %rdi
    call L1
    jmp L34
L30:
    movq -8(%rbp), %rax
    movq -16(%rax), %rcx
    movq -32(%rbp), %rax
    movq (%rcx, %rax, 8), %rcx
    movq $0, %rax
    cmpq %rcx, %rax
    je L17
L18:
    movq $0, %rax
L19:
    movq $0, %rcx
    cmpq %rax, %rcx
    jne L22
L23:
    movq $0, %rdx
L24:
    movq $0, %rax
    cmpq %rdx, %rax
    jne L27
L28:
    movq $0, %rax
L29:
    movq -32(%rbp), %rax
    leaq 1(%rax), %rax
    movq %rax, -32(%rbp)
    jmp L16
L17:
    movq $0, %rax
    movq -32(%rbp), %rdx
    movq -24(%rbp), %rcx
    addq %rcx, %rdx
    movq -8(%rbp), %rcx
    movq -32(%rcx), %rcx
    movq (%rcx, %rdx, 8), %rdx
    movq $0, %rcx
    cmpq %rdx, %rcx
    je L20
L21:
    jmp L19
L20:
    movq $1, %rax
    jmp L21
L22:
    movq $0, %rdx
    movq -32(%rbp), %rax
    leaq 7(%rax), %rcx
    movq -24(%rbp), %rax
    subq %rax, %rcx
    movq -8(%rbp), %rax
    movq -40(%rax), %rax
    movq (%rax, %rcx, 8), %rcx
    movq $0, %rax
    cmpq %rcx, %rax
    je L25
L26:
    jmp L24
L25:
    movq $1, %rdx
    jmp L26
L27:
    movq -8(%rbp), %rax
    movq -16(%rax), %rcx
    movq $8, %rdx
    movq -32(%rbp), %rax
    imulq %rdx, %rax
    addq %rax, %rcx
    movq $1, %rax
    movq %rax, (%rcx)
    movq -8(%rbp), %rax
    movq -32(%rax), %rdx
    movq -32(%rbp), %rax
    movq -24(%rbp), %rcx
    addq %rcx, %rax
    movq $8, %rcx
    imulq %rcx, %rax
    addq %rax, %rdx
    movq $1, %rax
    movq %rax, (%rdx)
    movq -8(%rbp), %rax
    movq -40(%rax), %rdx
    movq -32(%rbp), %rax
    leaq 7(%rax), %rcx
    movq -24(%rbp), %rax
    subq %rax, %rcx
    movq $8, %rax
    imulq %rax, %rcx
    addq %rcx, %rdx
    movq $1, %rax
    movq %rax, (%rdx)
    movq -8(%rbp), %rax
    movq -24(%rax), %rcx
    movq $8, %rdx
    movq -24(%rbp), %rax
    imulq %rdx, %rax
    addq %rax, %rcx
    movq -32(%rbp), %rax
    movq %rax, (%rcx)
    movq -8(%rbp), %rdi
    movq -24(%rbp), %rax
    leaq 1(%rax), %rsi
    call L2
    movq -8(%rbp), %rax
    movq -16(%rax), %rcx
    movq $8, %rdx
    movq -32(%rbp), %rax
    imulq %rdx, %rax
    addq %rax, %rcx
    movq $0, %rax
    movq %rax, (%rcx)
    movq -8(%rbp), %rax
    movq -32(%rax), %rdx
    movq -32(%rbp), %rcx
    movq -24(%rbp), %rax
    addq %rax, %rcx
    movq $8, %rax
    imulq %rax, %rcx
    addq %rcx, %rdx
    movq $0, %rax
    movq %rax, (%rdx)
    movq -8(%rbp), %rax
    movq -40(%rax), %rdx
    movq -32(%rbp), %rax
    leaq 7(%rax), %rax
    movq -24(%rbp), %rcx
    subq %rcx, %rax
    movq $8, %rcx
    imulq %rcx, %rax
    addq %rax, %rdx
    movq $0, %rax
    movq %rax, (%rdx)
    movq $0, %rax
    jmp L29
L36:
    addq $48, %rsp
    pop %rbp
    ret
tigermain:
    push %rbp
    movq %rsp, %rbp
    subq $64, %rsp
    movq %r15, -48(%rbp)
    movq $8, -8(%rbp)
    leaq -16(%rbp), %r15
    movq -8(%rbp), %rdi
    movq $0, %rsi
    call initArray
    movq %rax, (%r15)
    leaq -24(%rbp), %r15
    movq -8(%rbp), %rdi
    movq $0, %rsi
    call initArray
    movq %rax, (%r15)
    leaq -32(%rbp), %r15
    movq -8(%rbp), %rdi
    movq -8(%rbp), %rax
    addq %rax, %rdi
    movq $1, %rax
    subq %rax, %rdi
    movq $0, %rsi
    call initArray
    movq %rax, (%r15)
    leaq -40(%rbp), %r15
    movq -8(%rbp), %rdi
    movq -8(%rbp), %rax
    addq %rax, %rdi
    movq $1, %rax
    subq %rax, %rdi
    movq $0, %rsi
    call initArray
    movq %rax, (%r15)
    movq %rbp, %rdi
    movq $0, %rsi
    call L2
    movq -48(%rbp), %r15
    jmp L35
L35:
    addq $64, %rsp
    pop %rbp
    ret
