
; boots at &8000, normally at &4000

               org gnd+&4000
               dump gnd.bank,&0000

comm:          equ 224
trck:          equ 225
sect:          equ 226
dtrq:          equ 227

if defined (include-header)

    ; disk file header

               defb 3
               defw 0
               defw 0
               defw 0
               defb 0
               defb 0

    org.adjust: equ 0

else

    org.adjust: equ 9

               org $ + org.adjust

endif

               ld hl,&8000+510
               ld de,&0402

dos:           xor a
               ld (dct+&4000),a
               ld (svhl+&4000),hl

               ld a,e
               out (sect),a

dos2:          in a,(comm)
               bit 0,a
               jr nz,dos2

               in a,(trck)
               cp d
               jr z,dos4

               ld a,stpout
               jr nc,dos3
               ld a,stpin

dos3:          out (comm),a
               ld b,20
del1:          djnz del1
               jr dos2

dos4:          di
               ld a,drsec
               out (comm),a
               ld b,20
del2:          djnz del2

               ld hl,(svhl+&4000)
               ld bc,dtrq
               jr dos6

dos5:          ini

dos6:          in a,(comm)
               bit 1,a
               jr nz,dos5
               bit 0,a
               jr nz,dos6

               ei

;CHECK DISC ERR COUNT

               and &1c
               jr z,dos8

               ld a,(dct+&4000)
               inc a
               ld (dct+&4000),a
               push af
               and 2
               jr z,dos7

               ld a,dres
               out (comm),a
               ld b,20
del3:          djnz del3

dos7:          pop af
               cp 10
               jr c,dos2

               rst 8
               defb 19

dos8:          dec hl
               ld e,(hl)
               dec hl
               ld d,(hl)
               ld a,d
               or e
               jr nz,dos

               ld a,(&5cb4)   ;page
               ld (port2+&4000),a
               dec a
               ld (snprt2+&4000),a
               dec a
               ld (&5bc2),a   ;dosflg

               ld h,&51
               ld l,a         ;dsc use
               ld (hl),&60

               ld hl,&0144    ;device
               ld (&5a06),hl

               ret

               org $ - &4000

dvar:          equ $

rbcc:          defb 7
traks1:        defb 128+80
traks2:        defb 0
stprat:        defb 0
stprt2:        defb 0
chdir:         defb &20
nstat:         defb 1
vers:          defb 20

size1:         defb 80
size2:         defb 0
szea:          defb 12
lfeed:         defb 1
lmarg:         defb 0
graph:         defb 1
               defb 0
               defb 0
               defb 0
               defb 0
               defb 0
               defb 0
               defb 0
               defb 0

extadd:        call cmr
onerr:         defw 0
               ret

hksp:          defw 0
               defw 0


;PRINTER INITIALISE

pcc1:          defb &0d,&80,&80,&80

;CHARACTER PITCH

pcc2:          defb &1b,&4d,&80,&80

;LINE SPACING CODES

pcc3:          defb &1b,&41,&80,&80

;PIN GRAPHICS CODES

pcc4:          defb &1b,&2a,&05,&80

;ANY OTHER INITIALISE

pcc5:          defb &80,&80,&80,&80

;SPECIAL GRAPHIC CODES

pound:         defb &18,&20,&20,&78
               defb &20,&20,&7c,&00

hash:          defb &00,&24,&7e,&24
               defb &24,&7e,&24,&00

crite:         defb &7e,&81,&bd,&a1
               defb &a1,&bd,&81,&7e

gcc1:          defb &1b,&2a,&05,&40
               defb &02,&80,&80,&80

; <noise>
               defm "UTPUT DIFA TO ROMUIFA"
               defb &00,&00,&00,&80,&30,&00
; </noise>

               org gnd + &0100
               dump gnd.bank,&0100 - org.adjust

               defm "BOO"
               defb "T"+&80

entsp:         defw &7ffa
snprt0:        defb &1f
snprt1:        defb 2
snprt2:        defb &1e
snpsva:        defb 0

svhdr:         defw &4b00
cchad:         defw &9f34
cnt:           defw &0058

dsc:           defb rtrk
dct:           defb 0
dst:           defb 0
               defb &80,&32,&00,&02,&4c,&44,&07
nbot:          defb 0
rcmr:          defb 0
count:         defb 0
sva:           defb 13
svc:           defb 0
samcnt:        defb 0
rmse:          defb 0
smse:          defb 0

svdpt:         defw 0
svtrs:         defw 0
svbuf:         defw 0
svcnt:         defw 0
hldi:          defw &0044
ptrscr:        defw ftadd

port1:         defb 0
port2:         defb &1f
port3:         defb 0

cstr1:         defb &1d
tstr1:         defb &ff
               defb &ff
               defb &ff
hstr1:         defb &ff

dstr1:         defb 1
fstr1:         defb &ff
sstr1:         defb &ff
lstr1:         defb "D"
nstr1:         defb &13
               defm "samdos2       "
hd001:         defb &13
hd0b1:         defw &2710
hd0d1:         defw &8009
hd0f1:         defw &ffff
pges1:         defb 0
page1:         defb &7d

dstr2:         defb &ff
fstr2:         defb &ff
sstr2:         defb &ff
lstr2:         defb &ff
nstr2:         defb &ff
               defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
hd002:         defb &ff
hd0b2:         defw &ffff
hd0d2:         defw &ffff
hd0f2:         defw &ffff
pges2:         defb &ff
page2:         defb &ff

nstr3:         defb &ff
               defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff

uifa:          defb &13
               defm "samdos2                  "
               defb &ff,&ff,&ff,&ff,&ff
               defb &7d
               defb &09
               defb &80
               defb &00
               defb &10
               defb &27
               defb &ff
               defb &ff
               defb &ff
               defb &00,&00,&00,&00,&00,&00,&00,&00

difa:          defb 0
               defb &00,&80,&43,&00,&02,&4c,&44,&03,&42,&2c,&30,&00,&80,&44,&00,&02
               defb &4c,&44,&03,&43,&2c,&41,&00,&80,&45,&00,&03,&41,&44,&44,&05,&48
               defb &4c,&2c,&42,&43,&00,&80,&46,&00,&02,&4c,&44,&04,&42,&2c,&34

hka:           defb &44
hkhl:          defw &4b1f
hkde:          defw &4b25
hkbc:          defw &0202

snme:          defb &13
               defm "SNAP          "
               defb &13
snlen:         defw 49152
snadd:         defw 16384
               defw 0
               defw &ffff

; <noise>
               defm "CALL"
; </noise>

size:          equ zzend-gnd+&0220


;MAIN PROGRAM ENTRY POINT

               org gnd + &0200
               dump gnd.bank,&0200 - org.adjust

               jp hook

               jp syntax

               jp nmi

; <noise>
               defm "DEFW"
               defb &06
               defm "NR"
; </noise>

               org gnd + &0210
               dump gnd.bank,&0210 - org.adjust

               defw errtbl+&4000

; <noise>
               defm "TE"
               defb &00,&80
               defm "Q"
               defb &00,&03
               defm "INC"
               defb &02
               defm "HL"
               defb &00
; </noise>

               org gnd + &0220
               dump gnd.bank,&0220 - org.adjust

;TEST FOR CODE ON ERROR

syntax:        ld (entsp),sp

               cp 29          ;notund
               jp nz,synt3

synt1:         ld (cstr1),a
               call setbit
               call resreg

               ld hl,0
               ld (hksp),hl
               xor a
               ld (flag3),a
               ld ix,dchan

;GET CHADD AND SAVE IT

               call nrrdd
               defw chadd
               ld (cchad),bc

;GET START OF STATEMENT

               call nrrdd
               defw cstat

               call nrwrd
               defw chadd

               call gchr

               cp &90         ;dir
               jp z,dir

               cp &91         ;format
               jp z,wfod

               cp &92         ;erase
               jp z,eraz

               cp &86         ;write
               jp z,write

               cp &95         ;load
               jp z,load

               cp &b8         ;read
               jp z,read

               cp &cf         ;copy
               jp z,copy

               cp &e3         ;rename
               jp z,renam

               cp &e4         ;call
               jp z,calll

               cp &f1         ;protect
               jp z,prot

               cp &f2         ;hide
               jp z,hide


;CHECK EXTERNAL SYNTAX VECTOR

               ld bc,(cchad)
               call nrwrd
               defw chadd

               ld hl,(onerr)
               ld a,h
               or l
               ld a,(cstr1)
               call nz,extadd

synt3:         ld e,0

               ret


;SAMDOS HOOK CODE ROUTINE

hook:          ld (entsp),sp
               ld (svhdr),ix
               exx
               ex af,af'
               ld (hka),a
               ld (hkhl),hl
               ld (hkde),de
               ld (hkbc),bc
               ex af,af'
               exx
               call setbit
               ld hl,0
               ld (hksp),hl
               scf
               sbc 127
               jp c,rep17

               add a,a
               ld l,a
               ld h,0
               ld de,samhk
               add hl,de
               ld e,(hl)
               inc hl
               ld d,(hl)
               ld hl,rfhk
               push hl
               push de
               xor a
               ld (flag3),a
               ld a,(hka)
               ret


;RETURN FROM HOOK CODE O.K

rfhk:          xor a
               ld e,a
               call nrwr
               defw var2+&01c3
               jp bcr


;RESET ALL REGISTERS

resreg:        ld hl,tstr1
               ld bc,uifa-tstr1
resr1:         ld (hl),&ff
               inc hl
               dec bc
               ld a,b
               or c
               jr nz,resr1
               ret


;COMMAND CODE TABLE

samhk:         defw init      ;128
               defw hgthd     ;129
               defw hload     ;130
               defw hvery     ;131
               defw hsave     ;132
               defw s         ;133
               defw hopen     ;134
               defw hclos     ;135
               defw initx     ;136
               defw hdir      ;137
               defw s         ;138
               defw hvar      ;139
               defw heof      ;140
               defw hptr      ;141
               defw hpath     ;142
               defw s         ;143
               defw s         ;144
               defw s         ;145
               defw s         ;146
               defw hofle     ;147
               defw sbyt      ;148
               defw hwsad     ;149
               defw hsvbk     ;150
               defw s         ;151
               defw cfsm      ;152
               defw s         ;153
               defw pntp      ;154
               defw cops1     ;155
               defw cops2     ;156
               defw s         ;157
               defw hgfle     ;158
               defw lbyt      ;159
               defw hrsad     ;160
               defw hldbk     ;161
               defw s         ;162
               defw s         ;163
               defw rest      ;164
               defw pcat      ;165
               defw heraz     ;166
               defw s         ;167
               defw s         ;168

setbit:        push af
               ld a,1
               call nrwr
               defw var2+&01c3
               pop af
               ret

