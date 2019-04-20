               ORG  gnd
               DUMP gnd.bank,&0000

comm:          EQU  224
trck:          EQU  225
sect:          EQU  226
dtrq:          EQU  227

               DEFB 3
               DEFW 0
               DEFW 0
               DEFW 0
               DEFW 0

               LD   HL,&8000+510
               LD   DE,&0402

dos:           XOR  A
               LD   (dct),A
               LD   (svhl),HL

               LD   A,E
               OUT  (sect),A

dos2:          IN   A,(comm)
               BIT  0,A
               JR   NZ,dos2

               IN   A,(trck)
               CP   D
               JR   Z,dos4

               LD   A,stpout
               JR   NC,dos3
               LD   A,stpin

dos3:          OUT  (comm),A
               LD   B,20
del1:          DJNZ del1
               JR   dos2

dos4:          DI
               LD   A,drsec
               OUT  (comm),A
               LD   B,20
del2:          DJNZ del2

               LD   HL,(svhl)
               LD   BC,dtrq
               JR   dos6

dos5:          INI

dos6:          IN   A,(comm)
               BIT  1,A
               JR   NZ,dos5
               BIT  0,A
               JR   NZ,dos6

               EI

;CHECK DISC ERR COUNT

               AND  &1C
               JR   Z,dos8

               LD   A,(dct)
               INC  A
               LD   (dct),A
               PUSH AF
               AND  2
               JR   Z,dos7

               LD   A,dres
               OUT  (comm),A
               LD   B,20
del3:          DJNZ del3

dos7:          POP  AF
               CP   10
               JR   C,dos2

               RST  8
               DEFB 19

dos8:          DEC  HL
               LD   E,(HL)
               DEC  HL
               LD   D,(HL)
               LD   A,D
               OR   E
               JR   NZ,dos

               LD   A,(&5CB4)   ;page
               LD   (port2+&4000),A
               DEC  A
               LD   (snprt2+&4000),A
               DEC  A
               LD   (&5BC2),A   ;dosflg

               LD   H,&51
               LD   L,A         ;dsc use
               LD   (HL),&60

               LD   HL,&0144    ;device
               LD   (&5A06),HL

               LD   DE,&4BA0
               LD   HL,andy+&4000
               LD   BC,andend-andy
               LDIR

               LD   HL,&4BA0
               LD   (cmdv),HL

               XOR  A
               LD   (samcnt+&4000),A
               LD   (&5BC3),A

               RET

dvar:          EQU  $

rbcc:          DEFB 7
traks1:        DEFB 128+80
traks2:        DEFB 128+80
stprat:        DEFB 0
stprt2:        DEFB 0
chdir:         DEFB &20
nstat:         DEFB 1
vers:          DEFB 01

size1:         DEFB 2
size2:         DEFB 3

szea:          DEFB 0
lfeed:         DEFB 1
lmarg:         DEFB 0
graph:         DEFB 1
               DEFB 0
               DEFB 0
               DEFB 0
               DEFB 0
               DEFB 0
               DEFB 0
               DEFB 0
               DEFB 0

extadd:        CALL cmr
onerr:         DEFW 0
               RET

hksp:          DEFW 0
               DEFW 0


;PRINTER INITIALISE

pcc1:          DEFB &D,&80,&80,&80

;CHARACTER PITCH

pcc2:          DEFB &1B,&4D,&80,&80

;LINE SPACING CODES

pcc3:          DEFB &1B,&41,&80,&80

;PIN GRAPHICS CODES

pcc4:          DEFB &1B,&2A,&05,&80

;ANY OTHER INITIALISE

pcc5:          DEFB &80,&80,&80,&80

;SPECIAL GRAPHIC CODES

pound:         DEFB &18,&20,&20,&78
               DEFB &20,&20,&7C,&00

hash:          DEFB &00,&24,&7E,&24
               DEFB &24,&7E,&24,&00

crite:         DEFB &7E,&81,&BD,&A1
               DEFB &A1,&BD,&81,&7E

gcc1:          DEFB &1B,&2A,&05,&40
               DEFB &02,&80,&80,&80


               ORG  gnd+&0100
               DUMP gnd.bank,&0100


               DEFM "BOO"
               DEFB "T"+&80

entsp:         DEFW 0
snprt0:        DEFB &1F
snprt1:        DEFB 2
snprt2:        DEFB 0
snpsva:        DEFB 0

svhdr:         DEFW 0
cchad:         DEFW 0
cnt:           DEFW 0

dsc:           DEFB 0
dct:           DEFB 0
dst:           DEFB 0
               DEFS 7
nbot:          DEFB 0
rcmr:          DEFB 0
count:         DEFB 0
sva:           DEFB 0
svc:           DEFB 0
samcnt:        DEFB 0
rmse:          DEFB 0
smse:          DEFB 0

svdpt:         DEFW 0
svtrs:         DEFW 0
svbuf:         DEFW 0
svcnt:         DEFW 0
hldi:          DEFW 0
ptrscr:        DEFW 0

port1:         DEFB 0
port2:         DEFB 0
port3:         DEFB 0

tstr1:         DEFB 0
ostr1:         DEFB 0
cstr1:         DEFB 0
hstr1:         DEFB 0

dstr1:         DEFB 0
fstr1:         DEFB 0
sstr1:         DEFB 0
lstr1:         DEFB 0
nstr1:         DEFB 0
               DEFS 14
hd001:         DEFB 0
hd0b1:         DEFW 0
hd0d1:         DEFW 0
hd0f1:         DEFW 0
pges1:         DEFB 0
page1:         DEFB 0

dstr2:         DEFB 0
fstr2:         DEFB 0
sstr2:         DEFB 0
lstr2:         DEFB 0
nstr2:         DEFB 0
               DEFS 14
hd002:         DEFB 0
hd0b2:         DEFW 0
hd0d2:         DEFW 0
hd0f2:         DEFW 0
pges2:         DEFB 0
page2:         DEFB 0

nstr3:         DEFB 0
               DEFS 14

uifa:          DEFB 0
               DEFS 47

difa:          DEFB 0
               DEFS 47

hka:           DEFB 0
hkhl:          DEFW 0
hkde:          DEFW 0
hkbc:          DEFW 0

xpt:           DEFW 0
xtch:          DEFW 0

snme:          DEFB &13
               DEFM "SNAP          "
               DEFB &13
snlen:         DEFW 49152
snadd:         DEFW 16384
               DEFW 0
               DEFW &FFFF


size:          EQU  zzend-gnd+&0220


;MAIN PROGRAM ENTRY POINT

               ORG  gnd+&0200
               DUMP gnd.bank,&0200

               JP   hook

               JP   syntax


               ORG  gnd+&0210
               DUMP gnd.bank,&0210

               DEFW errtbl+&4000


               ORG  gnd+&0220
               DUMP gnd.bank,&0220

;origin at insbf

data:          LD   (curcmd),A
               CP   newtok
               JR   Z,newp
               CP   savtok
               JR   Z,savp

dirp:          RST  &20
               RST  8
               DEFB 29

savp:          LD   HL,(chadd)
               PUSH HL
               RST  &20
               POP  HL
               SUB  ovrtok
               LD   (overf),A
               JR   Z,svp1
               LD   (chadd),HL
svp1:          LD   A,savtok
               LD   HL,(comad)
               SUB  &90
               ADD  A,A
               LD   D,0
               LD   E,A
               ADD  HL,DE

svp2:          LD   C,250
               IN   B,(C)
               SET  6,B
               OUT  (C),B
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               PUSH DE
               RST  &20
               RET

newp:          LD   HL,insbf+newa-data
               JR   svp2

newa:          DEFW insbf+nnew-data

nnew:          LD   A,(flags)
               RLA
               RET  NC
               DI
               LD   HL,(comad)
               PUSH HL
               LD   HL,(cmdv)
               PUSH HL
               LD   A,(dosflg)
               LD   HL,var2
               LD   DE,var2+1
               LD   (HL),L
               LD   BC,&029E
               LDIR

               LD   (dosflg),A
               POP  HL
               LD   (cmdv),HL
               LD   HL,insbf+indev-data
               LD   (&5AEE),HL
               POP  HL
               LD   DE,&3E
               ADD  HL,DE
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               EX   DE,HL
               LD   DE,17
               ADD  HL,DE
               JP   (HL)

indev:         LD   HL,0
               LD   (&5AEE),HL
               LD   HL,&0144
               LD   (&5A06),HL
               RET

datend:        EQU  $


;BOOT LOAD TRAP FOR EXTENSIONS

andy:          CP   newtok
               JR   Z,andy1
               CP   savtok
               JR   Z,andy1
               CP   dirtok
               RET  NZ

andy1:         POP  HL
               PUSH AF
               LD   HL,data+&4000
               LD   BC,datend-data
               IN   A,(251)
               LD   D,A
               LD   A,(dosflg)
               OUT  (251),A
               LD   A,D
               LD   DE,insbf
               LDIR
               OUT  (251),A
               POP  AF
               JP   insbf

andend:        EQU  $

;TEST FOR CODE ON ERROR

syntax:        LD   (entsp),SP

               CP   29        ;notund
               JP   NZ,synt3

synt1:         LD   (cstr1),A
               CALL setbit
               CALL resreg

               XOR  A
               LD   (flag3),A
               LD   IX,dchan

;GET CHADD AND SAVE IT

               CALL nrrdd
               DEFW chadd
               LD   (cchad),BC

;GET START OF STATEMENT

               CALL nrrdd
               DEFW cstat

               CALL nrwrd
               DEFW chadd

               CALL gchr

               CP   &90       ;dir
               JP   Z,dir

               CP   &91       ;format
               JP   Z,wfod

               CP   &92       ;erase
               JP   Z,eraz

               CP   &93       ;move
               JP   Z,move

               CP   &86       ;write
               JP   Z,write

               CP   &95       ;load
               JP   Z,load

               CP   &B8      ;read
               JP   Z,read

               CP   &98       ;open
               JP   Z,open

               CP   &99       ;close
               JP   Z,close

               CP   &CF      ;copy
               JP   Z,copy

               CP   &E3      ;rename
               JP   Z,renam

               CP   &E4      ;call
               JP   Z,calll

               CP   &F1      ;protect
               JP   Z,prot

               CP   &F2      ;hide
               JP   Z,hide


;CHECK EXTERNAL SYNTAX VECTOR

               LD   BC,(cchad)
               CALL nrwrd
               DEFW chadd

               LD   HL,(onerr)
               LD   A,H
               OR   L
               LD   A,(cstr1)
               CALL NZ,extadd

synt3:         LD   E,0

               RET


;SAMDOS HOOK CODE ROUTINE

hook:          LD   (entsp),SP
               LD   (svhdr),IX
               EXX
               EX   AF,AF'
               LD   (hka),A
               LD   (hkhl),HL
               LD   (hkde),DE
               LD   (hkbc),BC
               EX   AF,AF'
               EXX
               CALL setbit
               SCF
               SBC  127
               JP   C,rep17

               ADD  A,A
               LD   L,A
               LD   H,0
               LD   DE,samhk
               ADD  HL,DE
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               LD   HL,rfhk
               PUSH HL
               PUSH DE
               XOR  A
               LD   (flag3),A
               LD   A,(hka)
               RET


;RETURN FROM HOOK CODE O.K

rfhk:          XOR  A
               LD   E,A
               CALL nrwr
               DEFW var2+&01C3
               JP   bcr


;RESET ALL REGISTERS

resreg:        LD   HL,tstr1
               LD   BC,uifa-tstr1
resr1:         LD   (HL),&FF
               INC  HL
               DEC  BC
               LD   A,B
               OR   C
               JR   NZ,resr1
               RET


;COMMAND CODE TABLE

samhk:         DEFW init      ;128
               DEFW hgthd     ;129
               DEFW hload     ;130
               DEFW hvery     ;131
               DEFW hsave     ;132
               DEFW s         ;133
               DEFW hopen     ;134
               DEFW hclos     ;135
               DEFW init      ;136
               DEFW hdir      ;137
               DEFW s         ;138
               DEFW hvar      ;139
               DEFW heof      ;140
               DEFW hptr      ;141
               DEFW hpath     ;142
               DEFW s         ;143
               DEFW s         ;144
               DEFW s         ;145
               DEFW s         ;146
               DEFW hofle     ;147
               DEFW sbyt      ;148
               DEFW hwsad     ;149
               DEFW hsvbk     ;150
               DEFW s         ;151
               DEFW cfsm      ;152
               DEFW s         ;153
               DEFW pntp      ;154
               DEFW cops1     ;155
               DEFW cops2     ;156
               DEFW s         ;157
               DEFW hgfle     ;158
               DEFW lbyt      ;159
               DEFW hrsad     ;160
               DEFW hldbk     ;161
               DEFW s         ;162
               DEFW s         ;163
               DEFW rest      ;164
               DEFW pcat      ;165
               DEFW heraz     ;166
               DEFW s         ;167
               DEFW s         ;168

setbit:        PUSH AF
               LD   A,1
               CALL nrwr
               DEFW var2+&01C3
               POP  AF
               RET

