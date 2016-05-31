## The Terse Machine ##

The Terse Machine is a minimal computer architecture designed to require a minimum number of gates while still being reasonably useful and flexible.

Initially started as a little [Minecraft](http://minecraft.gamepedia.com/Redstone_circuit) computer, the Terse Machine is intended for Turing complete environments where large gate counts can result in prohibitively tedious construction (for impatient people like me). Although terse, the goal of the architecture is to still be useful, and I'm hoping to provide tools that make programming the computer relatively easy.

My long term goal is to put together a homebrew Terse Machine out of 7400 chips after being encouraged by the many interesting projects on the [Homebuilt CPUs WebRing](http://members.iinet.net.au/~daveb/simplex/ringhome.html).

### Registers ###

```
r0 a  [-- addr --]
r1 b  [-- addr --]
r2 sp [-- addr --]
r3 pc [-- addr --]

ir    [- byte -]
d     [- byte -]
```

### Instruction Set ###

```
op   encoding       if i == 0                   if i == 1

mv   [000 i rd ra]  rd = ra                     rd = rd<<8 | mem[ra++]
st   [001 i rd ra]  mem[--rd], ra = ra, ra>>8   mem[--rd] = mem[ra++]

cz   [010 i rd ra]  rd = rd - ra if a==0        rd = rd - mem[ra++] if a==0
cnz  [011 i rd ra]  rd = rd - ra if a!=0        rd = rd - mem[ra++] if a!=0

and  [100 i rd ra]  rd = rd & ra                rd = rd & mem[ra++]
xor  [101 i rd ra]  rd = rd ^ ra                rd = rd ^ mem[ra++]
add  [110 i rd ra]  rd = rd + ra                rd = rd + mem[ra++]
sub  [111 i rd ra]  rd = rd - ra                rd = rd - mem[ra++]
```

### Composite Instructions ###

```
op      instructions

nop     mv a, a
halt    sub pc, 2
swi i   st pc, i

push r  st sp, r
pop r   ld r, sp

b i     sub pc, -i
bz i    cz pc, -i
bnz i   cnz pc, -i

call i  mv a, i
        mv b, pc
        mv pc, a
ret     add b, 1
        mv pc, b
```

### State Machine ###

```
 -------
| fetch |
 -------
    |  \--+--------+--------+------\
    v     |        |        |      |
 -------  |        |        |      |
|  ld   | |        |        |      |
 -------  |        |        |      |
    \---+--------+--------+------\ |
        v v      v v      v v    | |
      -------  -------  -------  | |
     |  mov  ||  st1  || cond  | | |
      -------  -------  -------  | |
         |        |        |  \  | |
         v        v        v   v v v
               -------        -------
              |  st2  |      | math  |
               -------        -------
                  |              |
                  v              v
```
