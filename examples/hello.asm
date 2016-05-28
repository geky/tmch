; entry point
.org 0x00
main:
    pushw hello
    call puts
    halt

puts:
    pushw b
    add sp, 2
_loop:
    popw b
    ld a, b
    pushw b
    bz _exit
    mvw b, stdout+1
    st b, a
    b _loop
_exit:
    sub sp, 2
    popw b
    ret

.org 0x100
stdin:
.org 0x101
stdout:
.org 0x102
stderr:

.org 0x200
hello:
    .byte "Hello World", 0x21, 0x0a, 0
