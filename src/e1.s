
;DISC FORMAT ROUTINE

dfmt:          DI
               CALL gtixd
               CALL ckdrv
               CALL seld

               LD   B,10
dfmta:         PUSH BC
               CALL instp
               POP  BC
               DJNZ dfmta

               CALL restx

               CALL getscr

fmt1:          LD   HL,ftadd
               CALL dmt1

               PUSH DE
               CALL cmr
               DEFW clslow
               CALL pmoa
               POP  DE
               PUSH DE
               CALL sctrk
               POP  DE

fmt3:          LD   C,wtrk
               CALL precmp
               LD   HL,ftadd
               CALL wrdata
               CALL stpdel
               INC  D

               CALL tstd
               CP   D
               JR   Z,fmt7
               AND  &7F
               CP   D
               JR   Z,fmt6
               CALL instp
               DEC  E
               JR   NZ,fmt4
               LD   E,10
fmt4:          DEC  E
               JR   NZ,fmt5
               LD   E,10
fmt5:          JP   fmt1

fmt6:          CALL restx
               LD   D,&80
               CALL seld
               JP   fmt1

fmt7:          CALL rest

               LD   A,(dstr2)
               CP   &FF
               JR   Z,fmt11a

               LD   HL,(buf)
               LD   (svbuf),HL

fmt8:          PUSH DE
               CALL cmr
               DEFW clslow
               CALL pmob
               POP  DE
               PUSH DE
               CALL sctrk
               POP  DE

               LD   A,(dstr2)
               CALL ckdrx
               LD   HL,ftadd

fmt9:          LD   (buf),HL
               CALL rsad
               LD   BC,512
               ADD  HL,BC
               CALL isect
               JR   NZ,fmt9

               LD   A,(dstr1)
               CALL ckdrx
               LD   HL,ftadd

fmt10:         LD   (buf),HL
               CALL wsad
               LD   BC,512
               ADD  HL,BC
               CALL isect
               JR   NZ,fmt10

               CALL itrck
               JR   NZ,fmt8

               LD   HL,(svbuf)
               LD   (buf),HL
               JR   fmt12


fmt11:         CALL rsad
               CALL isect
               JR   NZ,fmt11

fmt11a:        PUSH DE
               CALL cmr
               DEFW clslow
               CALL pmoc
               POP  DE
               PUSH DE
               CALL sctrk
               POP  DE
               CALL itrck
               JR   NZ,fmt11

fmt12:         CALL cmr
               DEFW clslow
               LD   HL,ftadd
               CALL putscr
               EI
               JP   rest


;PRINT TRACK ON SCREEN

sctrk:         LD   A,D
               BIT  7,A
               JR   Z,strk1
               AND  &7F
               LD   B,A
               CALL tstd
               AND  &7F
               ADD  B

strk1:         LD   L,A
               LD   H,0
               LD   A,&20
               JP   pnum3


;INCREMENT TRACK

itrck:         INC  D
               CALL tstd
               CP   D
               RET  Z
               AND  &7F
               CP   D
               RET  NZ
               CALL rest
               LD   D,&80
               CP   D
               RET


;DOUBLE DENSITY FORMAT

dmt1:          LD   BC,&3C4E
               CALL wfm
               LD   B,10
dmt2:          PUSH BC
               LD   BC,&C00
               CALL wfm
               LD   BC,&03F5
               CALL wfm
               LD   BC,&01FE
               CALL wfm
               LD   A,D
               AND  &7F
               LD   C,A
               LD   B,&01
               CALL wfm
               LD   A,D
               AND  &80
               RLCA
               LD   C,A
               LD   B,1
               CALL wfm
               LD   C,E
               CALL isect
               LD   B,&01
               CALL wfm
               LD   BC,&0102
               CALL wfm
               LD   BC,&01F7
               CALL wfm
               LD   BC,&164E
               CALL wfm
               LD   BC,&C00
               CALL wfm
               LD   BC,&03F5
               CALL wfm
               LD   BC,&01FB
               CALL wfm
               LD   BC,0
               CALL wfm
               CALL wfm
               LD   BC,&01F7
               CALL wfm
               LD   BC,&184E
               CALL wfm
               POP  BC
               DEC  B
               JP   NZ,dmt2
               LD   BC,&004E
               CALL wfm
               CALL wfm
               JP   wfm



;WRITE FORMAT IN MEMORY

wfm:           LD   (HL),C
               INC  HL
               DJNZ wfm
               RET


;INCREMENT SECTOR ROUTINE

isect:         INC  E
               LD   A,E
               CP   11
               RET  NZ
               LD   E,1
               RET


;PRINT TYPE OF FILE

pntyp:         AND  &1F
               PUSH AF
               LD   HL,drtab
               LD   BC,drtbx-drtab
               CPIR

pnty1:         LD   A,(HL)
               CP   32
               JR   C,pnty2
               CALL pnt
               INC  HL
               JR   pnty1

pnty2:         POP  AF
               CP   16
               JR   NZ,pnty3
               LD   (IX+rptl),242
               CALL grpnt
               LD   A,(HL)
               AND  &C0
               JR   NZ,pnty5
               INC  HL
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               EX   DE,HL
               CALL pnum5
               JR   pnty5

pnty3:         CP   19
               JR   NZ,pnty4
               LD   (IX+rptl),236
               CALL grpnt
               CALL gtval
               INC  C
               EX   DE,HL
               PUSH DE
               LD   A,&20
               CALL pnum6
               LD   A,","
               CALL pnt
               POP  HL
               CALL gtval
               EX   DE,HL
               XOR  A
               CALL pnum6

pnty4:         CP   4
               JR   NZ,pnty5
               LD   (IX+rptl),215
               CALL grpnt
               LD   D,(HL)
               DEC  HL
               LD   E,(HL)
               EX   DE,HL
               PUSH DE
               CALL pnum5
               LD   A,","
               CALL pnt
               POP  HL
               DEC  HL
               LD   D,(HL)
               DEC  HL
               LD   E,(HL)
               EX   DE,HL
               XOR  A
               CALL pnum5x

pnty5:         LD   A,&D
               JP   pnt


;GET NUMBER FROM HEADER

gtval:         LD   A,(HL)
               AND  &1F
               LD   C,A
               INC  HL
               LD   E,(HL)
               INC  HL
               LD   A,(HL)
               AND  &7F
               LD   D,A
               INC  HL
               RET


drtab:         DEFB 1
               DEFM "ZX BASIC"
               DEFB 16
               DEFM "BASIC "
               DEFB 2
               DEFM "ZX D.ARRAY"
               DEFB 17
               DEFM "D.ARRAY"
               DEFB 3
               DEFM "ZX $.ARRAY"
               DEFB 18
               DEFM "$.ARRAY"
               DEFB 4
               DEFM "ZX "
               DEFB 19
               DEFM "c "
               DEFB 5
               DEFM "ZX SNP 48k"
               DEFB 6
               DEFM "MD.FILE"
               DEFB 7
               DEFM "ZX SCREEN$"
               DEFB 20
               DEFM "SCREEN$"
               DEFB 8
               DEFM "SPECIAL"
               DEFB 9
               DEFM "ZX SNP 128k"
               DEFB 10
               DEFM "OPENTYPE"
               DEFB 11
               DEFM "N/A EXECUTE"
               DEFB 12
drtbx:         DEFM "WHAT?"
               DEFB 0


;PRINT NUMBER IN HL

pnum6:         LD   (sva),A
               XOR  A
               LD   DE,0

               RR   C
               RR   D
               RR   C
               RR   D
               LD   A,D
               ADD  H
               LD   H,A
               LD   A,C
               ADC  E
               LD   B,A
               LD   DE,34464
               LD   C,1       ;65536
               LD   A,(sva)
               CALL pnm2
               JR   pnum5y

pnum5:         LD   A,&20

pnum5x:        LD   B,0
pnum5y:        LD   C,0
               LD   DE,10000
               CALL pnm2
pnum4:         LD   DE,1000
               CALL pnm1
pnum3:         LD   DE,100
               CALL pnm1
pnum2:         LD   DE,10
               CALL pnm1
pnum1:         LD   A,L
               ADD  &30
               JR   pnt

pnm1:          LD   BC,0
pnm2:          PUSH AF
               LD   A,B
               LD   B,0
               AND  A

pnm3:          SBC  HL,DE
               SBC  A,C
               JR   C,pnm4
               INC  B
               JR   pnm3
pnm4:          ADD  HL,DE
               ADC  A,C
               LD   C,A
               LD   A,B
               LD   B,C
               AND  A
               JR   NZ,pnm5

               POP  DE
               ADD  D
               RET  Z
               JR   pnt

pnm5:          ADD  &30
               CALL pnt
               POP  DE
               LD   A,&30
               RET



;PRINT TEXT MESSAGE

ptm:           POP  HL
ptm2:          LD   A,(HL)
               AND  &7F
               CALL pnt
               BIT  7,(HL)
               RET  NZ
               INC  HL
               JR   ptm2

;SEND A SPACE CHARACTER

spc:           LD   A,&20

;OUTPUT A CHAR TO CURRENT CHAN

pnt:           PUSH AF
               PUSH BC
               PUSH DE
               PUSH HL
               PUSH IX

               CALL cmr
               DEFW &0010

               POP  IX
               POP  HL
               POP  DE
               POP  BC
               POP  AF
               RET


;SCREEN ROUTINES

pmo1:          CALL ptm
               DEFM "Tape ready ? "
               DEFM "press SPACE ."
               DEFB "."+128

pmo2:          CALL ptm
               DEFM " - DIRECTORY *"
               DEFW &8D0D

pmo3:          CALL ptm
               DEFW &D0D
               DEFM "Number of Free "
               DEFM "K-Bytes ="
               DEFB &A0

pmo4:          CALL ptm
               DEFB &7F
               DEFM " Miles Gordon Tec"
               DEFM "hnology Plc  1"
               DEFW &8D0D

pmo5:          CALL ptm
               DEFM "OVERWRITE "
               DEFB "'"+128

pmo6:          CALL ptm
               DEFM "Are you SURE ?"
               DEFM " (y/n"
               DEFB ")"+128

pmo7:          CALL ptm
               DEFM "' (y/n"
               DEFB ")"+128

pmo8:          CALL ptm
               DEFM "  * SAM DRIVE "
               DEFB &A0

pmo9:          CALL ptm
               DEFB &D
               DEFM "Enter source disk "
               DEFM "press any ke"
               DEFB "y"+&80

pmoa:          CALL ptm
               DEFM "Format disk at "
               DEFM "track "
               DEFB " "+&80

pmob:          CALL ptm
               DEFM "Copy   disk at "
               DEFM "track "
               DEFB " "+&80

pmoc:          CALL ptm
               DEFM "Verify disk at "
               DEFM "track "
               DEFB " "+&80

pmod:          CALL ptm
               DEFM "Enter target disk "
               DEFM "press any ke"
               DEFB "y"+&80


tspce1:        CALL cmr
               DEFW clslow
               CALL pmod
tspc1:         CALL cmr
               DEFW rdkey
               JR   NC,tspc1
tspc2:         CALL cmr
               DEFW rdkey
               JR   C,tspc2
               CALL cmr
               DEFW clslow
               RET

tspce2:        CALL cmr
               DEFW clslow
               CALL pmo9
               JR   tspc1

