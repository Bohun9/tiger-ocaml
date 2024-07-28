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
    movq %r14, -40(%rbp)
    movq %r15, -32(%rbp)
    movq %rdi, -8(%rbp)
    movq $0, %r15
    movq -8(%rbp), %rax
    movq -8(%rax), %rax
    movq $1, %rcx
    subq %rcx, %rax
    movq %rax, -16(%rbp)
L3:
    movq -16(%rbp), %rax
    cmpq %r15, %rax
    jge L13
L14:
    movq $L15, %rdi
    call print
    movq -40(%rbp), %r14
    movq -32(%rbp), %r15
    jmp L37
L13:
    movq $0, %r14
    movq -8(%rbp), %rax
    movq -8(%rax), %rax
    movq $1, %rcx
    subq %rcx, %rax
    movq %rax, -24(%rbp)
L4:
    movq -24(%rbp), %rax
    cmpq %r14, %rax
    jge L10
L11:
    movq $L12, %rdi
    call print
    leaq 1(%r15), %r15
    jmp L3
L10:
    movq -8(%rbp), %rax
    movq -24(%rax), %rax
    movq (%rax, %r14, 8), %rax
    cmpq %rax, %r15
    je L7
L8:
    movq $L6, %rdi
L9:
    call print
    leaq 1(%r14), %r14
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
    subq $32, %rsp
    movq %r14, -32(%rbp)
    movq %r15, -24(%rbp)
    movq %rdi, -8(%rbp)
    movq %rsi, %r15
    movq -8(%rbp), %rax
    movq -8(%rax), %rax
    cmpq %r15, %rax
    je L32
L33:
    movq $0, %r14
    movq -8(%rbp), %rax
    movq -8(%rax), %rax
    movq $1, %rcx
    subq %rcx, %rax
    movq %rax, -16(%rbp)
L16:
    movq -16(%rbp), %rax
    cmpq %r14, %rax
    jge L30
L31:
    movq $0, %rax
L34:
    movq -32(%rbp), %r14
    movq -24(%rbp), %r15
    jmp L36
L32:
    movq -8(%rbp), %rdi
    call L1
    jmp L34
L30:
    movq -8(%rbp), %rax
    movq -16(%rax), %rax
    movq (%rax, %r14, 8), %rcx
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
    movq $0, %rcx
L24:
    movq $0, %rax
    cmpq %rcx, %rax
    jne L27
L28:
    movq $0, %rax
L29:
    leaq 1(%r14), %r14
    jmp L16
L17:
    movq $0, %rax
    movq %r14, %rdx
    addq %r15, %rdx
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
    movq $0, %rcx
    leaq 7(%r14), %rax
    subq %r15, %rax
    movq -8(%rbp), %rdx
    movq -40(%rdx), %rdx
    movq (%rdx, %rax, 8), %rdx
    movq $0, %rax
    cmpq %rdx, %rax
    je L25
L26:
    jmp L24
L25:
    movq $1, %rcx
    jmp L26
L27:
    movq -8(%rbp), %rax
    movq -16(%rax), %rax
    movq $8, %rdx
    movq %r14, %rcx
    imulq %rdx, %rcx
    addq %rcx, %rax
    movq $1, %rcx
    movq %rcx, (%rax)
    movq -8(%rbp), %rax
    movq -32(%rax), %rcx
    movq %r14, %rax
    addq %r15, %rax
    movq $8, %rdx
    imulq %rdx, %rax
    addq %rax, %rcx
    movq $1, %rax
    movq %rax, (%rcx)
    movq -8(%rbp), %rax
    movq -40(%rax), %rcx
    leaq 7(%r14), %rax
    subq %r15, %rax
    movq $8, %rdx
    imulq %rdx, %rax
    addq %rax, %rcx
    movq $1, %rax
    movq %rax, (%rcx)
    movq -8(%rbp), %rax
    movq -24(%rax), %rax
    movq $8, %rdx
    movq %r15, %rcx
    imulq %rdx, %rcx
    addq %rcx, %rax
    movq %r14, (%rax)
    movq -8(%rbp), %rdi
    leaq 1(%r15), %rsi
    call L2
    movq -8(%rbp), %rax
    movq -16(%rax), %rax
    movq $8, %rdx
    movq %r14, %rcx
    imulq %rdx, %rcx
    addq %rcx, %rax
    movq $0, %rcx
    movq %rcx, (%rax)
    movq -8(%rbp), %rax
    movq -32(%rax), %rcx
    movq %r14, %rax
    addq %r15, %rax
    movq $8, %rdx
    imulq %rdx, %rax
    addq %rax, %rcx
    movq $0, %rax
    movq %rax, (%rcx)
    movq -8(%rbp), %rax
    movq -40(%rax), %rcx
    leaq 7(%r14), %rax
    subq %r15, %rax
    movq $8, %rdx
    imulq %rdx, %rax
    addq %rax, %rcx
    movq $0, %rax
    movq %rax, (%rcx)
    movq $0, %rax
    jmp L29
L36:
    addq $32, %rsp
    pop %rbp
    ret
tigermain:
    push %rbp
    movq %rsp, %rbp
    subq $48, %rsp
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
    addq $48, %rsp
    pop %rbp
    ret
