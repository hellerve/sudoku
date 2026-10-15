        .data
        .p2align 4
board:  .byte 8,0,0,0,0,0,0,0,0
        .byte 0,0,3,6,0,0,0,0,0
        .byte 0,7,0,0,9,0,2,0,0
        .byte 0,5,0,0,0,7,0,0,0
        .byte 0,0,0,0,4,5,7,0,0
        .byte 0,0,0,1,0,0,0,3,0
        .byte 0,0,1,0,0,0,0,6,8
        .byte 0,0,8,5,0,0,0,1,0
        .byte 0,9,0,0,0,0,4,0,0

nope:   .ascii "unsatisfiable\n"
out:    .space 162

        .text
        .p2align 2

.macro copy81 dst, src
        ldp     q0, q1, [\src]
        stp     q0, q1, [\dst]
        ldp     q0, q1, [\src, #32]
        stp     q0, q1, [\dst, #32]
        ldr     q0, [\src, #64]
        str     q0, [\dst, #64]
        ldrb    w9, [\src, #80]
        strb    w9, [\dst, #80]
.endm

.macro mark used, at
        ldrb    w10, \at
        lsl     w10, w6, w10
        orr     \used, \used, w10
.endm

cands:
        adrp    x9, board@PAGE
        add     x9, x9, board@PAGEOFF
        mov     w2, #9
        udiv    w3, w0, w2
        msub    w4, w3, w2, w0
        mov     w5, #0
        mov     w6, #1

        mul     w7, w3, w2
        add     x7, x9, w7, uxtw
        mov     w8, #9
1:      mark    w5, "[x7], #1"
        subs    w8, w8, #1
        b.ne    1b

        add     x7, x9, w4, uxtw
        mov     w8, #9
2:      mark    w5, "[x7], #9"
        subs    w8, w8, #1
        b.ne    2b

        mov     w2, #3
        udiv    w3, w3, w2
        mov     w8, #27
        mul     w3, w3, w8
        udiv    w4, w4, w2
        mul     w4, w4, w2
        add     w3, w3, w4
        add     x7, x9, w3, uxtw
        mov     w8, #3
3:      mark    w5, "[x7]"
        mark    w5, "[x7, #1]"
        mark    w5, "[x7, #2]"
        add     x7, x7, #9
        subs    w8, w8, #1
        b.ne    3b

        mvn     w0, w5
        and     w0, w0, #0x3fe
        ret

propagate:
        stp     x29, x30, [sp, #-48]!
        mov     x29, sp
        stp     x19, x20, [sp, #16]
        str     x21, [sp, #32]
        adrp    x19, board@PAGE
        add     x19, x19, board@PAGEOFF
again:
        mov     w20, #0
        mov     w21, #0
cell:
        ldrb    w9, [x19, w21, uxtw]
        cbnz    w9, next
        mov     w0, w21
        bl      cands
        cbz     w0, dead
        sub     w9, w0, #1
        tst     w0, w9
        b.ne    next
        rbit    w9, w0
        clz     w9, w9
        strb    w9, [x19, w21, uxtw]
        mov     w20, #1
next:
        add     w21, w21, #1
        cmp     w21, #81
        b.lo    cell
        cbnz    w20, again
        mov     w0, #1
        b       1f
dead:
        mov     w0, #0
1:      ldr     x21, [sp, #32]
        ldp     x19, x20, [sp, #16]
        ldp     x29, x30, [sp], #48
        ret

solve:
        stp     x29, x30, [sp, #-64]!
        mov     x29, sp
        stp     x19, x20, [sp, #16]
        stp     x21, x22, [sp, #32]
        str     x23, [sp, #48]
        bl      propagate
        cbz     w0, fail

        adrp    x19, board@PAGE
        add     x19, x19, board@PAGEOFF
        mov     w20, #-1
        mov     w21, #10
        mov     w22, #0
mrv:
        ldrb    w9, [x19, w22, uxtw]
        cbnz    w9, 1f
        mov     w0, w22
        bl      cands
        fmov    s0, w0
        cnt     v0.8b, v0.8b
        addv    b0, v0.8b
        fmov    w9, s0
        cmp     w9, w21
        b.hs    1f
        mov     w21, w9
        mov     w20, w22
        mov     w23, w0
1:      add     w22, w22, #1
        cmp     w22, #81
        b.lo    mrv
        tbnz    w20, #31, done

        sub     sp, sp, #96
        copy81  sp, x19
try:
        rbit    w9, w23
        clz     w9, w9
        strb    w9, [x19, w20, uxtw]
        bl      solve
        cbnz    w0, found
        copy81  x19, sp
        sub     w9, w23, #1
        and     w23, w23, w9
        cbnz    w23, try
        add     sp, sp, #96
fail:
        mov     w0, #0
        b       1f
found:
        add     sp, sp, #96
done:
        mov     w0, #1
1:      ldr     x23, [sp, #48]
        ldp     x21, x22, [sp, #32]
        ldp     x19, x20, [sp, #16]
        ldp     x29, x30, [sp], #64
        ret

write:
        mov     x0, #1
        mov     x16, #4
        svc     #0x80
        ret

print:
        adrp    x9, board@PAGE
        add     x9, x9, board@PAGEOFF
        adrp    x1, out@PAGE
        add     x1, x1, out@PAGEOFF
        mov     x12, x1
        mov     w11, #0
        mov     w14, #9
1:      ldrb    w13, [x9, w11, uxtw]
        add     w13, w13, #'0'
        strb    w13, [x12], #1
        add     w11, w11, #1
        udiv    w15, w11, w14
        msub    w15, w15, w14, w11
        mov     w13, #' '
        cbnz    w15, 2f
        mov     w13, #'\n'
2:      strb    w13, [x12], #1
        cmp     w11, #81
        b.lo    1b
        mov     x2, #162
        b       write

        .globl  _main
_main:
        stp     x29, x30, [sp, #-16]!
        mov     x29, sp
        bl      solve
        cbz     w0, 1f
        bl      print
        b       2f
1:      adrp    x1, nope@PAGE
        add     x1, x1, nope@PAGEOFF
        mov     x2, #14
        bl      write
2:      mov     w0, #0
        ldp     x29, x30, [sp], #16
        ret
