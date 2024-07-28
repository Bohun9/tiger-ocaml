.globl tigermain
.section .data
L4:
    .quad 1
    .ascii "0"
L8:
    .quad 1
    .ascii "-"
L9:
    .quad 1
    .ascii "0"
L17:
    .quad 1
    .ascii "0"
L18:
    .quad 1
    .ascii "9"
L25:
    .quad 1
    .ascii " "
L26:
    .quad 1
    .ascii "\n"
L33:
    .quad 1
    .ascii "0"
L51:
    .quad 1
    .ascii "\n"
L52:
    .quad 1
    .ascii " "
.section .text
L3:
    push %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq %r15, -16(%rbp)
    movq %rdi, -8(%rbp)
    movq %rsi, %r15
    movq $0, %rax
    cmpq %r15, %rax
    jl L5
L6:
    movq $0, %rax
L7:
    movq -16(%rbp), %r15
    jmp L63
L5:
    movq -8(%rbp), %rdi
    movq $10, %rcx
    movq $0, %rdx
    movq %r15, %rax
    idivq %rcx
    movq %rax, %rsi
    call L3
    movq $10, %rcx
    movq $0, %rdx
    movq %r15, %rax
    idivq %rcx
    movq $10, %rcx
    imulq %rcx, %rax
    subq %rax, %r15
    movq $L4, %rdi
    call ord
    addq %rax, %r15
    movq %r15, %rdi
    call chr
    movq %rax, %rdi
    call print
    jmp L7
L63:
    addq $16, %rsp
    pop %rbp
    ret
L1:
    push %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq %r15, -16(%rbp)
    movq %rdi, -8(%rbp)
    movq %rsi, %r15
    movq $0, %rax
    cmpq %r15, %rax
    jl L13
L14:
    movq $0, %rax
    cmpq %r15, %rax
    jg L10
L11:
    movq $L9, %rdi
    call print
L12:
L15:
    movq -16(%rbp), %r15
    jmp L62
L13:
    movq %rbp, %rdi
    movq %r15, %rsi
    call L3
    jmp L15
L10:
    movq $L8, %rdi
    call print
    movq %rbp, %rdi
    movq $0, %rsi
    subq %r15, %rsi
    call L3
    jmp L12
L62:
    addq $16, %rsp
    pop %rbp
    ret
L16:
    push %rbp
    movq %rsp, %rbp
    subq $32, %rsp
    movq %r14, -24(%rbp)
    movq %r15, -16(%rbp)
    movq %rdi, -8(%rbp)
    movq %rsi, %r15
    movq $L17, %rdi
    call ord
    movq %rax, %r14
    movq %r15, %rdi
    call ord
    cmpq %r14, %rax
    jge L19
L20:
    movq $0, %rax
L21:
    movq -24(%rbp), %r14
    movq -16(%rbp), %r15
    jmp L61
L19:
    movq $0, %r14
    movq %r15, %rdi
    call ord
    movq %rax, %r15
    movq $L18, %rdi
    call ord
    cmpq %r15, %rax
    jge L22
L23:
    movq %r14, %rax
    jmp L21
L22:
    movq $1, %r14
    jmp L23
L61:
    addq $32, %rsp
    pop %rbp
    ret
L2:
    push %rbp
    movq %rsp, %rbp
    subq $32, %rsp
    movq %r13, -32(%rbp)
    movq %r14, -24(%rbp)
    movq %r15, -16(%rbp)
    movq %rdi, -8(%rbp)
    movq %rsi, %r13
    movq $0, %r14
L24:
    movq -8(%rbp), %rax
    movq -8(%rax), %rdi
    movq $L25, %rsi
    call stringEqual
    movq $0, %rcx
    cmpq %rax, %rcx
    jne L27
L28:
    movq -8(%rbp), %rax
    movq -8(%rax), %rdi
    movq $L26, %rsi
    call stringEqual
L29:
    movq $0, %rcx
    cmpq %rax, %rcx
    jne L30
L31:
    movq $0, %rax
    movq $8, %rcx
    imulq %rcx, %rax
    addq %rax, %r13
    movq %rbp, %rdi
    movq -8(%rbp), %rax
    movq -8(%rax), %rsi
    call L16
    movq %rax, (%r13)
L32:
    movq %rbp, %rdi
    movq -8(%rbp), %rax
    movq -8(%rax), %rsi
    call L16
    movq $0, %rcx
    cmpq %rax, %rcx
    jne L34
L35:
    movq %r14, %rax
    movq -32(%rbp), %r13
    movq -24(%rbp), %r14
    movq -16(%rbp), %r15
    jmp L60
L27:
    movq $1, %rax
    jmp L29
L30:
    movq -8(%rbp), %rax
    leaq -8(%rax), %r15
    call getChar
    movq %rax, (%r15)
    jmp L24
L34:
    movq $10, %r13
    imulq %r14, %r13
    movq -8(%rbp), %rax
    movq -8(%rax), %rdi
    call ord
    addq %rax, %r13
    movq $L33, %rdi
    call ord
    subq %rax, %r13
    movq %r13, %r14
    movq -8(%rbp), %rax
    leaq -8(%rax), %r13
    call getChar
    movq %rax, (%r13)
    jmp L32
L60:
    addq $32, %rsp
    pop %rbp
    ret
L36:
    push %rbp
    movq %rsp, %rbp
    subq $32, %rsp
    movq %r14, -24(%rbp)
    movq %r15, -16(%rbp)
    movq %rdi, -8(%rbp)
    movq $8, %rdi
    call allocRecord
    movq %rax, %r15
    movq $0, 0(%r15)
    movq -8(%rbp), %rdi
    movq %r15, %rsi
    call L2
    movq %rax, %r14
    movq $0, %rax
    movq (%r15, %rax, 8), %rcx
    movq $0, %rax
    cmpq %rcx, %rax
    jne L39
L40:
    movq -8(%rbp), %rax
    leaq -8(%rax), %r14
    call getChar
    movq %rax, (%r14)
    movq $0, %rax
L41:
    movq -24(%rbp), %r14
    movq -16(%rbp), %r15
    jmp L59
L39:
    movq $16, %rdi
    call allocRecord
    movq %rax, %r15
    movq %r14, 0(%r15)
    leaq 8(%r15), %r14
    movq -8(%rbp), %rdi
    call L36
    movq %rax, (%r14)
    movq %r15, %rax
    jmp L41
L59:
    addq $32, %rsp
    pop %rbp
    ret
L37:
    push %rbp
    movq %rsp, %rbp
    subq $48, %rsp
    movq %r12, -40(%rbp)
    movq %r13, -32(%rbp)
    movq %r14, -24(%rbp)
    movq %r15, -16(%rbp)
    movq %rdi, -8(%rbp)
    movq %rsi, %r14
    movq %rdx, %r13
    movq $0, %rax
    cmpq %r14, %rax
    je L48
L49:
    movq $0, %rax
    cmpq %r13, %rax
    je L45
L46:
    movq $0, %rax
    movq (%r14, %rax, 8), %rcx
    movq $0, %rax
    movq (%r13, %rax, 8), %rax
    cmpq %rcx, %rax
    jg L42
L43:
    movq $16, %rdi
    call allocRecord
    movq %rax, %r12
    movq $0, %rax
    movq (%r13, %rax, 8), %rax
    movq %rax, 0(%r12)
    leaq 8(%r12), %r15
    movq -8(%rbp), %rdi
    movq %r14, %rsi
    movq $1, %rax
    movq (%r13, %rax, 8), %rdx
    call L37
    movq %rax, (%r15)
L44:
    movq %r12, %r14
L47:
    movq %r14, %r13
L50:
    movq %r13, %rax
    movq -40(%rbp), %r12
    movq -32(%rbp), %r13
    movq -24(%rbp), %r14
    movq -16(%rbp), %r15
    jmp L58
L48:
    jmp L50
L45:
    jmp L47
L42:
    movq $16, %rdi
    call allocRecord
    movq %rax, %r12
    movq $0, %rax
    movq (%r14, %rax, 8), %rax
    movq %rax, 0(%r12)
    leaq 8(%r12), %r15
    movq -8(%rbp), %rdi
    movq $1, %rax
    movq (%r14, %rax, 8), %rsi
    movq %r13, %rdx
    call L37
    movq %rax, (%r15)
    jmp L44
L58:
    addq $48, %rsp
    pop %rbp
    ret
L38:
    push %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq %r15, -16(%rbp)
    movq %rdi, -8(%rbp)
    movq %rsi, %r15
    movq $0, %rax
    cmpq %r15, %rax
    je L53
L54:
    movq -8(%rbp), %rdi
    movq $0, %rax
    movq (%r15, %rax, 8), %rsi
    call L1
    movq $L52, %rdi
    call print
    movq -8(%rbp), %rdi
    movq $1, %rax
    movq (%r15, %rax, 8), %rsi
    call L38
L55:
    movq -16(%rbp), %r15
    jmp L57
L53:
    movq $L51, %rdi
    call print
    jmp L55
L57:
    addq $16, %rsp
    pop %rbp
    ret
tigermain:
    push %rbp
    movq %rsp, %rbp
    subq $32, %rsp
    movq %r13, -32(%rbp)
    movq %r14, -24(%rbp)
    movq %r15, -16(%rbp)
    leaq -8(%rbp), %r13
    call getChar
    movq %rax, (%r13)
    movq $8, %rdi
    call allocRecord
    movq $0, 0(%rax)
    movq %rbp, %r13
    movq %rbp, %r14
    movq %rbp, %rdi
    call L36
    movq %rax, %r15
    movq %rbp, %rdi
    call L36
    movq %rax, %rdx
    movq %r14, %rdi
    movq %r15, %rsi
    call L37
    movq %rax, %rsi
    movq %r13, %rdi
    call L38
    movq -32(%rbp), %r13
    movq -24(%rbp), %r14
    movq -16(%rbp), %r15
    jmp L56
L56:
    addq $32, %rsp
    pop %rbp
    ret
