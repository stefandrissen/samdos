
ftadd:         EQU  &A280
rdkey:         EQU  &0169
clslow:        EQU  &0151


commp:         PUSH AF
               LD   A,(dsc)
               LD   C,A
               POP  AF
               RET

trckp:         CALL commp
               INC  C
               RET

commr:         PUSH BC
               CALL commp
               IN   A,(C)
               POP  BC
               RET

commt:         PUSH BC
               CALL commp
               OUT  (C),A
               POP  BC
               RET

ckde:          LD   A,D
               OR   E
               RET  NZ

               LD   A,(pges1)
               AND  A
               RET  Z

               DEC  A
               LD   (pges1),A
               LD   DE,16384
               JR   ckde


;SAVE INTERRUPT STATUS

svint:         PUSH AF
               LD   A,I
               JP   PE,svin1
               LD   A,I
svin1:         PUSH AF
               DI
               EX   (SP),HL
               LD   (hldi),HL
               POP  HL
               POP  AF
               RET


;LOAD INTERRUPT STATUS

ldint:         PUSH AF
               PUSH HL
               LD   HL,(hldi)
               EX   (SP),HL
               POP  AF
               JP   PO,ldin1
               EI
ldin1:         POP  AF
               RET


;PRECOMPENSATION CALCULATOR

precmx:        LD   C,dwsec

precmp:        CALL tstd
               LD   B,A
               SRL  B
               LD   A,D
               AND  &7F
               SUB  B
               JR   NC,prec1
               RES  1,C
prec1:         JP   sadc

;WRITE SECTOR AT DE

wsad:          XOR  A
               LD   (dct),A
wsa1:          CALL ctas
               CALL precmx
               CALL gtbuf
               CALL wrdata
               CALL cdec
               JR   wsa1

wrdata:        CALL svint
               CALL commp
               JR   wsa3

wsa2:          INC  C
               INC  C
               INC  C

               OUTI

               DEC  C
               DEC  C
               DEC  C

wsa3:          IN   A,(C)
               BIT  1,A
               JR   NZ,wsa2

               IN   A,(C)
               BIT  1,A
               JR   NZ,wsa2

               IN   A,(C)
               BIT  1,A
               JR   NZ,wsa2

               IN   A,(C)
               BIT  1,A
               JR   NZ,wsa2

               IN   A,(C)
               BIT  1,A
               JR   NZ,wsa2

               IN   A,(C)
               BIT  1,A
               JR   NZ,wsa2

               BIT  0,A
               JR   NZ,wsa3

               CALL ldint
               BIT  6,A
               RET  Z
               CALL decsam
               JP   rep23


;READ SECTOR AT DE

rsad:          XOR  A
               LD   (dct),A
rsa1:          CALL ctas
               LD   C,drsec
               CALL sadc
               CALL gtbuf
               CALL rddata
               CALL cdec
               JR   rsa1


rddata:        CALL svint
               CALL commp
               JR   rsa3

rsa2:          INC  C
               INC  C
               INC  C

               INI

               DEC  C
               DEC  C
               DEC  C

rsa3:          IN   A,(C)
               BIT  1,A
               JR   NZ,rsa2

               IN   A,(C)
               BIT  1,A
               JR   NZ,rsa2

               IN   A,(C)
               BIT  1,A
               JR   NZ,rsa2

               IN   A,(C)
               BIT  1,A
               JR   NZ,rsa2

               IN   A,(C)
               BIT  1,A
               JR   NZ,rsa2

               IN   A,(C)
               BIT  1,A
               JR   NZ,rsa2

               BIT  0,A
               JR   NZ,rsa3

               JP   ldint


;CHECK DISC ERR COUNT

cdec:          AND  &1C
               JR   NZ,cde1
               CALL clrrpt
               POP  HL
               JP   gtbuf

cde1:          PUSH AF
               LD   A,(dct)
               INC  A
               LD   (dct),A
               CP   10
               JP   NC,rep4

               POP  AF
               BIT  4,A
               JR   NZ,ctsl

               CALL instp
               CALL outstp
               CALL outstp
               JP   instp


;CONFIRM TRACK/SECTOR LOCATION

ctsl:          LD   C,radd
               CALL sadc
               LD   HL,dst
               CALL rddata
               AND  &1C
               JR   NZ,cts1
               CALL trckp
               LD   A,(dst)
               OUT  (C),A
               RET

cts1:          LD   A,(dct)
               INC  A
               LD   (dct),A
               CP   8
               JP   NC,rep5

               AND  2
               JR   Z,cts2

               PUSH DE
               CALL restx
               POP  DE
               JR   ctsl

cts2:          CALL instp
               JR   ctsl


;CONFIRM TRACK AND SEEK

ctas:          LD   A,D
               OR   E
               JR   NZ,cta1
               CALL bitf2
               JP   Z,rep27

               LD   SP,(entsp)
               XOR  A
               LD   E,A
               RET

cta1:          CALL seld
               LD   A,(dsc)
               INC  A
               INC  A
               LD   C,A
               OUT  (C),E
               CALL bcc

cta2:          LD   A,D
               AND  &7F
               LD   B,A
               CALL busy
               CALL trckp
               IN   A,(C)
               CP   B
               RET  Z


;CHECK IF PAGE OVER C000h

               PUSH AF

               CALL bitf6
               JR   Z,cta3

               LD   HL,(svhl)
               LD   A,H
               CP   &C0
               JR   C,cta3
               RES  6,H
               LD   (svhl),HL
               IN   A,(251)
               PUSH AF
               AND  %11100000
               LD   B,A
               POP  AF
               INC  A
               AND  %00011111
               OR   B
               LD   (port1),A
               OUT  (251),A

cta3:          POP  AF
               CALL NC,outstp
               CALL C,instp
               JR   cta2


;STEP DELAY ROUTINE

stpdel:        PUSH HL
               LD   HL,stprat
               LD   A,(dsc)
               BIT  4,A
               JR   Z,stpd1
               INC  HL
stpd1:         LD   A,(HL)
               POP  HL
               AND  A

stpd2:         RET  Z

stpd3:         PUSH AF
               LD   BC,150
stpd4:         DEC  BC
               LD   A,B
               OR   C
               JR   NZ,stpd4
               POP  AF
               DEC  A
               JR   stpd2


;RESTORE DISC DRIVE

restx:         NOP

rest:          LD   DE,&0001
               CALL seld

;RESET DISC CHIP

               LD   C,&D0
               CALL sdcx
               LD   B,0
rslp1:         DJNZ rslp1

;TEST FOR INDEX HOLE

               LD   HL,0
rslp2:         CALL commr
               BIT  1,A
               CALL NZ,rslpx
               JR   NZ,rslp2

rslp3:         CALL commr
               CPL
               BIT  1,A
               CALL NZ,rslpx
               JR   NZ,rslp3

;TEST FOR TRACK 00

rslp4:         CALL commr
               BIT  2,A
               JR   NZ,busy

;STEP OUT ONE TRACK

               CALL outstp
               JR   rslp4

;TEST FOR CHIP BUSY

busy:          CALL commr
               BIT  0,A
               RET  Z
               CALL brktst
               JR   busy

rslpx:         DEC  HL
               LD   A,H
               OR   L
               RET  NZ

               JP   rep6


;SEND A DISC COMMAND

sadc:          CALL busy
sdcx:          LD   A,C
               CALL commt
               LD   B,20
sdc1:          DJNZ sdc1
               RET


;CHECK DRIVE NUMBER

ckdrv:         LD   A,(dstr1)

ckdrx:         CP   1
               JR   Z,ckdv1
               CP   2
               JP   NZ,rep22
               LD   A,(rbcc+2)
               CP   0
               JP   Z,rep22
               LD   A,2
ckdv1:         LD   (drive),A
               RET

sze:           PUSH HL
               LD   (szea),A
               LD   HL,size1
               LD   A,(drive)
               CP   1
               JR   Z,sze1
               INC  HL
sze1:          LD   A,(HL)
               CP   2
               LD   A,(szea)
               POP  HL
               RET


;SELECT DISC AND SIDE

seld:          LD   A,(drive)
               CP   2
               LD   B,%11100000
               JR   NZ,sel1
               LD   B,%11110000

sel1:          LD   A,D
               AND  &80
               JR   Z,sel2
               LD   A,%00000100
sel2:          OR   B
               LD   (dsc),A
               RET


;CONVERT DE INTO NUMBER

conm:          PUSH DE
               POP  BC
               XOR  A
               DEC  B
               JP   M,con2
con1:          ADD  10
               DEC  B
               JP   P,con1
con2:          LD   B,A
               SLA  B
               SLA  C
               DEC  C
               LD   A,(IX+rpth)
               ADD  C
               ADD  B
               RET


;TEST FOR BUFFER FULL

tfbf:          CALL grpnt
               LD   A,C
               CP   254
               RET  NZ
               CALL sze
               JR   Z,tfbf1
               LD   A,B
               CP   3
               RET

tfbf1:         LD   A,B
               CP   1
               RET

;SAVE A BYTE ON DISC

sbyt:          PUSH BC
               PUSH DE
               PUSH HL
               PUSH AF
               CALL tfbf
               JR   NZ,sbt1
sbt2:          CALL fnfs
               LD   (HL),D
               INC  HL
               LD   (HL),E
               EX   DE,HL
               CALL swpnsr
               CALL wsad
sbt1:          POP  AF
               LD   (HL),A
               POP  HL
               POP  DE
               POP  BC
               JP   incrpt



;LOAD BYTE FROM DISC

lbyt:          PUSH BC
               PUSH DE
               PUSH HL
               CALL tfbf
               JR   NZ,lbt1
               LD   D,(HL)
               INC  HL
               LD   E,(HL)
               CALL rsad
lbt1:          LD   A,(HL)
               POP  HL
               POP  DE
               POP  BC
               JP   incrpt


;LOAD BLOCK DATA from disc

ldblk:         CALL setf6
               JP   lblok


ldb1:          LD   A,(HL)
               CALL incrpt
               LD   HL,(svhl)
               LD   (HL),A
               INC  HL
               DEC  DE
lblok:         LD   (svhl),HL
               CALL ckde
               RET  Z

ldb2:          CALL tfbf
               JR   NZ,ldb1

               LD   (svde),DE
               LD   D,(HL)
               INC  HL
               LD   E,(HL)
               CALL svint

ldb3:          CALL ccnt
               JP   C,ldb8

               INC  HL
               LD   (svde),HL
               XOR  A
               LD   (dct),A
               CALL svnsr
ldb4:          CALL ctas
               LD   C,drsec
               CALL sadc
               EXX
               CALL commp
               LD   DE,2
               CALL gtbuf
               EXX
               CALL commp
               LD   DE,510
               CALL sze
               JR   Z,ldb4a
               LD   DE,1022
ldb4a:         LD   HL,(svhl)
               JR   ldb6

ldb5:          INC  C
               INC  C
               INC  C
               INI
               DEC  C
               DEC  C
               DEC  C

               DEC  DE
               LD   A,D
               OR   E
               JR   NZ,ldb6
               EXX

ldb6:          IN   A,(C)
               BIT  1,A
               JR   NZ,ldb5

               IN   A,(C)
               BIT  1,A
               JR   NZ,ldb5

               IN   A,(C)
               BIT  1,A
               JR   NZ,ldb5

               IN   A,(C)
               BIT  1,A
               JR   NZ,ldb5

               IN   A,(C)
               BIT  1,A
               JR   NZ,ldb5

               IN   A,(C)
               BIT  1,A
               JR   NZ,ldb5

               BIT  0,A
               JR   NZ,ldb6

               AND  &1C
               JR   Z,ldb7

               CALL gtnsr
               CALL cde1
               JR   ldb4

ldb7:          LD   (svhl),HL
               CALL gtbuf
               LD   D,(HL)
               INC  HL
               LD   E,(HL)
               JP   ldb3

ldb8:          CALL ldint
               CALL rsad
               LD   DE,(svde)
               JP   ldb2


;CALCULATE COUNT

ccnt:          LD   HL,(svde)
               LD   BC,510
               CALL sze
               JR   Z,ccnta
               LD   BC,1022
ccnta:         SCF
               SBC  HL,BC
               RET  NC
               LD   A,(pges1)
               AND  A
               JR   NZ,ccnt1
               SCF
               RET

ccnt1:         DEC  A
               LD   (pges1),A
               LD   HL,(svde)
               LD   BC,16384
               ADD  HL,BC
               LD   (svde),HL
               JR   ccnt


;GET SCREEN MEMORY AND POINTER

getscr:        IN   A,(251)
               LD   (port1),A
               LD   A,(port2)
               OUT  (251),A
               LD   HL,(ptrscr)
               RET

;PUT SCREEN MEMORY AND POINTER

putscr:        LD   (ptrscr),HL
               LD   A,(port1)
               OUT  (251),A
               RET


;SAVE DATA BLOCK ON DISC

svblk:         CALL setf6
               JP   sblok

svb1:          LD   (HL),D
               CALL incrpt
               LD   HL,(svhl)
               INC  HL
               POP  DE
               DEC  DE

;TEST FOR ZERO BLOCK COUNT

sblok:         CALL ckde
               RET  Z

;SAVE CHAR IN REGISTER D

               PUSH DE
               LD   D,(HL)
               LD   (svhl),HL

;TEST FOR BUFFER FULL

               CALL tfbf
               JR   NZ,svb1

;SAVE BUFFER TO DISC

               POP  DE
               LD   (svde),DE
               CALL fnfs
               LD   (HL),D
               INC  HL
               LD   (HL),E
               EX   DE,HL
               CALL swpnsr
               CALL wsad

               CALL svint
               CALL ccnt
               JP   C,svb8

               CALL getscr
               LD   HL,ftadd
               LD   BC,0
               JR   svb2a


svb2:          PUSH HL
               CALL ccnt
               PUSH HL
               POP  DE
               POP  HL
               JR   C,svb3
               INC  DE
               LD   (svde),DE
               CALL fnfs
               LD   (HL),D
               INC  HL
               LD   (HL),E
               INC  HL
               LD   BC,(svcnt)
               INC  BC
svb2a:         LD   (svcnt),BC
               JR   svb2

svb3:          LD   HL,ftadd
               CALL putscr

svb3a:         XOR  A
               LD   (dct),A
               CALL gtnsr

svb4:          CALL ctas
               CALL precmx
               EXX
               LD   HL,(ptrscr)
               LD   DE,2
               CALL commp
               EXX
               LD   HL,(svhl)
               LD   DE,510
               CALL sze
               JR   Z,svb4a
               LD   DE,1022
svb4a:         CALL commp
               JR   svb6

svb5:          INC  C
               INC  C
               INC  C

               OUTI

               DEC  C
               DEC  C
               DEC  C

               DEC  DE
               LD   A,D
               OR   E
               JR   NZ,svb6

               EXX
               LD   A,(port2)
               OUT  (251),A

svb6:          IN   A,(C)
               BIT  1,A
               JR   NZ,svb5

               IN   A,(C)
               BIT  1,A
               JR   NZ,svb5

               IN   A,(C)
               BIT  1,A
               JR   NZ,svb5

               IN   A,(C)
               BIT  1,A
               JR   NZ,svb5

               IN   A,(C)
               BIT  1,A
               JR   NZ,svb5

               IN   A,(C)
               BIT  1,A
               JR   NZ,svb5

               BIT  0,A
               JR   NZ,svb6

               PUSH AF
               LD   A,(port1)
               OUT  (251),A
               POP  AF

               AND  &1C
               JR   Z,svb7

               CALL gtnsr
               CALL cde1
               JR   svb4

svb7:          LD   (svhl),HL

               EXX
               LD   A,(port2)
               OUT  (251),A
               LD   (ptrscr),HL
               DEC  HL
               LD   E,(HL)
               DEC  HL
               LD   D,(HL)
               LD   A,(port1)
               OUT  (251),A
               CALL svnsr
               EXX

               LD   BC,(svcnt)
               DEC  BC
               LD   (svcnt),BC
               LD   A,B
               OR   C
               JP   NZ,svb3a

svb8:          CALL ldint
               CALL clrrpt
               LD   DE,(svde)
               LD   HL,(svhl)
               JP   svblk


;FIND NEXT FREE SECTOR

fnfs:          PUSH HL
               PUSH BC
               LD   HL,sam
               LD   DE,&0401
               LD   C,0

fns1:          LD   A,(HL)
               CP   &FF
               JR   NZ,fns3
               LD   A,E
               ADD  8
               LD   E,A
               CALL sze
               JR   Z,fns1a
               SUB  5
               JR   fns1b
fns1a:         SUB  10
fns1b:         JR   C,fns2
               JR   Z,fns2
               LD   E,A
               CALL fns5
fns2:          INC  C
               INC  HL
               JR   fns1

fns3:          LD   B,1
fns4:          LD   A,(HL)
               AND  B
               JR   Z,fns6
               CALL isect
               CALL Z,fns5
               RLC  B
               JR   fns4

fns5:          INC  D
               CALL tstd
               CP   D
               CALL Z,decsam
               JP   Z,rep24
               AND  &7F
               CP   D
               RET  NZ
               LD   D,&80
               RET

fns6:          LD   A,(HL)
               OR   B
               LD   (HL),A
               LD   A,B
               LD   B,0
               PUSH IX
               ADD  IX,BC
               OR   (IX+fsam)
               LD   (IX+fsam),A
               POP  IX
               INC  (IX+cntl)
               JR   NZ,fns7
               INC  (IX+cnth)
fns7:          POP  BC
               POP  HL
               RET


;TEST TRACKS ON DISC

tstd:          PUSH HL
               LD   HL,traks1
               LD   A,(dsc)
               BIT  4,A
               JR   Z,tsd1
               INC  HL
tsd1:          LD   A,(HL)
               POP  HL
               RET


;PRINT FILE NAME

pfnme:         LD   (IX+rptl),1
               CALL grpnt
               LD   B,10
pfnm1:         LD   A,(HL)
               CP   &20
               JR   NZ,pfnm2
               LD   A,(chdir)
pfnm2:         CALL pnt
               INC  HL
               DJNZ pfnm1
               RET


;FILE DIR HAND/ROUT.

fdhr:          LD   IX,dchan
               LD   (IX+4),A
               XOR  A
               LD   (svdpt),A
               CALL rest
fdh1:          CALL rsad
fdh2:          CALL point
               LD   A,(HL)
               AND  A
               JP   Z,fdhf

;TEST FOR 'P' NUMBER

               BIT  0,(IX+4)
               JR   Z,fdh3
               CALL conm
               LD   B,A
               LD   A,(fstr1)
               CP   B
               RET  Z
               JP   fdhd

;GET TRACK AND SECTOR COUNT

fdh3:          BIT  1,(IX+4)
               JR   NZ,fdh4
               BIT  2,(IX+4)
               JP   Z,fdh9
fdh4:          LD   (IX+rptl),11
               CALL grpnt
               LD   B,(HL)
               INC  HL
               LD   C,(HL)
               LD   (svbc),BC
               LD   HL,(cnt)
               ADD  HL,BC
               LD   (cnt),HL

;TEST IF WE SHOULD PRINT NAME

               BIT  7,A
               JP   NZ,fdhd

               CALL cknam
               JP   NZ,fdhd
               BIT  1,(IX+4)
               JR   NZ,fdh5

               CALL point
               LD   A,(HL)
               BIT  6,A
               JR   Z,fdh4a
               CALL spc
               LD   A,"*"
               CALL pnt
               CALL spc
               JR   fdh5

fdh4a:         CALL conm
               PUSH DE
               LD   H,0
               LD   L,A
               LD   A,&20
               CALL pnum2
               POP  DE
               CALL spc

;PRINT FILE NAME

fdh5:          CALL pfnme

;FORMAT FOR ! PRINTOUT

               BIT  1,(IX+4)
               JR   Z,fdh7
               LD   B,3
               IN   A,(252)
               AND  %01100000
               CP   %01000000
               JR   NZ,fdh6a
               SLA  B
               DEC  B
fdh6a:         LD   A,(svdpt)
               INC  A
               CP   B
               JR   Z,fdh6b
               LD   (svdpt),A
               LD   A,32
               JR   fdh6c

fdh6b:         XOR  A
               LD   (svdpt),A
               LD   A,13
fdh6c:         CALL pnt
               JR   fdhd

;PRINT SECTOR COUNT

fdh7:          PUSH DE
               LD   HL,(svbc)
               LD   A,&20
               CALL pnum3
               CALL spc

;PRINT TYPE OF FILE

               CALL point
               LD   A,(HL)
               CALL pntyp
               POP  DE
               JR   fdhd

;TEST FOR SPECIFIC FILE NAME

fdh9:          BIT  3,(IX+4)
               JR   NZ,fdha

;TEST FOR FILE NAME ONLY

               BIT  4,(IX+4)
               JR   Z,fdhb

fdha:          CALL cknam
               RET  Z

;LOAD SAM FROM FILES USED

fdhb:          BIT  5,(IX+4)
               JR   Z,fdhd
               PUSH IX
               LD   (IX+rptl),15
               CALL grpnt
               LD   IX,sam
               LD   B,195

fdhc:          LD   A,(IX)
               OR   (HL)
               LD   (IX),A
               INC  IX
               INC  HL
               DJNZ fdhc
               POP  IX

;CALCULATE NEXT DIRECTORY ENTRY

fdhd:          LD   A,(IX+rpth)
               CALL sze
               JR   Z,fdhd1
               CP   3
               JR   fdhd2
fdhd1:         CP   1
fdhd2:         JR   Z,fdhe
               INC  (IX+rpth)
               JP   fdh2

fdhe:          CALL clrrpt
               CALL isect
               JP   NZ,fdh1
               INC  D
               LD   A,D
               CP   4
               JP   NZ,fdh1
               AND  A
               RET

;TEST FOR FREE DIRECTORY SPACE

fdhf:          LD   A,(IX+4)
               CPL
               BIT  6,A
               RET  Z
               JR   fdhd


;CHECK FILE NAME IN DIR

cknam:         PUSH IX
               CALL point
               LD   B,11
               BIT  3,(IX+4)
               LD   IX,nstr1
               JR   Z,cknm2

cknm1:         LD   A,(IX)
               CP   "*"
               JR   Z,cknm5
               CP   "?"
               JR   Z,cknm2
               XOR  (HL)
               AND  &DF
               JR   NZ,cknm4
cknm2:         INC  IX
               INC  HL
               DJNZ cknm1
cknm3:         XOR  A
cknm4:         POP  IX
               RET

cknm5:         INC  IX
               INC  HL
               LD   A,(IX)
               CP   "."
               JR   NZ,cknm3
cknm6:         LD   A,(HL)
               CP   "."
               JR   Z,cknm2
               INC  HL
               DJNZ cknm6
               JR   cknm4


;OPEN FILE SECTOR ADDRESS MAP

ofsm:          PUSH IX

;TEST FOR SAM ALREADY OPEN

;      LD   A,(SAMCNT)
;      CP   0
;      LD   A,10h
;      JR   NZ,OFM2

;SAM NOT OPEN SO CLEAR IT

               LD   HL,sam
               LD   B,195
ofm1:          LD   (HL),0
               INC  HL
               DJNZ ofm1
               LD   A,&30

ofm2:          CALL fdhr
               JR   NZ,ofm4

;FILE NAME ALREADY USED

               PUSH DE
               CALL nrrd
               DEFW overf
               AND  A
               JR   Z,ofm3

               CALL cmr
               DEFW clslow
               CALL pmo5
               CALL pfnme
               CALL pmo7
               CALL cyes
               JR   Z,ofm3

               JP   ends

ofm3:          POP  DE
               CALL point
               LD   (HL),0
               CALL wsad
               POP  IX
               JR   ofsm

;NOW CLEAR FILE SECTOR AREA

ofm4:          POP  IX

               PUSH IX
               LD   B,0
ofm5:          LD   (IX+ffsa),0
               INC  IX
               DJNZ ofm5
               POP  IX

               PUSH IX
               LD   HL,nstr1
               LD   B,11
               CALL ofm6
               POP  IX

               PUSH IX
               LD   BC,220
               ADD  IX,BC
               LD   HL,uifa+15
               LD   B,48-15
               CALL ofm6
               POP  IX

               CALL fnfs
               CALL svnsr
               LD   (IX+ftrk),D
               LD   (IX+fsct),E
               CALL clrrpt
               LD   A,(samcnt)
               INC  A
               LD   (samcnt),A
               XOR  A
               RET

ofm6:          LD   A,(HL)
               LD   (IX+ffsa),A
               INC  HL
               INC  IX
               DJNZ ofm6
               RET

decsam:        PUSH AF
               LD   A,(samcnt)
               DEC  A
               LD   (samcnt),A
               POP  AF
               RET


;COMPARE FOR Y or N

cyes:          CALL beep
cyes1:         CALL cmr
               DEFW rdkey
               JR   NC,cyes1
               AND  &DF
               CP   "Y"
               PUSH AF
               CALL cmr
               DEFW clslow
               POP  AF
               RET

beep:          PUSH HL
               PUSH DE
               PUSH BC
               PUSH IX
               LD   HL,&036A
               LD   DE,&0085
               CALL cmr
               DEFW beepr
               POP  IX
               POP  BC
               POP  DE
               POP  HL
               RET


;CLOSE FILE SECTOR MAP

cfsm:          CALL grpnt
               LD   A,C
               AND  A
               JR   NZ,cfm1
               LD   A,B
               CALL sze
               JR   Z,cfsm1
               CP   4
               JR   cfsm2
cfsm1:         CP   2
cfsm2:         JR   Z,cfm2
cfm1:          LD   (HL),0
               CALL incrpt
               JR   cfsm

cfm2:          CALL gtnsr
               CALL wsad
               CALL decsam
               PUSH IX
               LD   A,&40
               CALL fdhr

               JP   NZ,rep25

;UPDATE DIRECTORY

               CALL point
               LD   (svix),IX
               POP  IX
               PUSH IX

               LD   B,0
cfm3:          LD   A,(IX+ffsa)
               LD   (HL),A
               INC  IX
               INC  HL
               DJNZ cfm3

               LD   IX,(svix)
               CALL wsad
               POP  IX
               RET


;GET A FILE FROM DISC

gtfle:         LD   A,(fstr1)

;TEST FOR FILE NUMBER

               CP   &FF
               JR   Z,gtfl3

               LD   A,1
               CALL fdhr
               JP   NZ,rep26

gtflx:         CALL point
               LD   A,(HL)
               AND  &1F
               CP   &10
               JR   NC,gtfl1
               DEC  A
               OR   &10
               CALL setf7

gtfl1:         LD   (HL),A
               LD   DE,nstr1
               LD   BC,11
               LDIR
               LD   B,4
               LD   A,0
               CALL lcntb

               LD   (IX+rptl),211
               CALL grpnt
               LD   BC,9
               LDIR

               CALL point
               LD   DE,uifa
               LD   BC,11
               LDIR

               LD   B,15
               CALL lcnta

               LD   B,48-26
               LD   A,&FF
               CALL lcntb

               JR   gtfl4

gtfl3:         LD   A,&10
               CALL fdhr
               JP   NZ,rep26

gtfl4:         CALL point
               LD   A,(HL)
               AND  &1F
               CP   &10
               JR   NC,gtfl5
               DEC  A
               OR   &10
               CALL setf7

gtfl5:         LD   (HL),A
               LD   A,(nstr1)
               CP   &13
               JR   NZ,gtfl5a
               LD   A,(HL)
               CP   &14
               JR   NZ,gtfl5a
               DEC  A
               LD   (HL),A

gtfl5a:        LD   A,(nstr1)
               CP   (HL)
               JP   NZ,rep26

               LD   DE,hd002
               LD   (IX+rptl),211
               CALL grpnt
               LD   BC,9
               LDIR

               LD   DE,str-20
               LD   BC,22
               LDIR

               CALL point
               LD   DE,difa
               LD   BC,11
               LDIR
               LD   B,4
               CALL lcnta

               CALL bitf7
               JR   Z,gtfl7

               LD   B,11
               CALL lcnta

               LD   B,48-26
               LD   A,&FF
               CALL lcntb

               LD   HL,(hd0b2)
               CALL conp
               LD   (difa+34),A
               LD   (pges2),A
               LD   (difa+35),HL
               LD   (hd0b2),HL

               LD   HL,(hd0d2)
               CALL conp
               DEC  A
               AND  &1F
               LD   (difa+31),A
               LD   (page2),A
               SET  7,H
               LD   (difa+32),HL
               LD   (hd0d2),HL

               JR   gtfl8

gtfl7:         LD   (IX+rptl),220
               CALL grpnt
               LD   BC,48-15
               LDIR

gtfl8:         LD   (IX+rptl),13
               CALL grpnt
               LD   D,(HL)
               INC  HL
               LD   E,(HL)
               LD   (svde),DE
               RET

;CONVERT TO REAL PAGE AND MOD

conp:          XOR  A
               RL   H
               RLA
               RL   H
               RLA
               RR   H
               RR   H
               RET


;LOAD DE WITH COUNT B

lcnta:         LD   A,&20

lcntb:         LD   (DE),A
               INC  DE
               DJNZ lcntb
               RET


;GET BUFFER ADDRESS

gtixd:         LD   IX,dchan
               LD   HL,dram
               LD   (buf),HL

;CLEAR RAM POINTER

clrrpt:        LD   (IX+rptl),0
               LD   (IX+rpth),0
               RET


gtbuf:         LD   L,(IX+bufl)
               LD   H,(IX+bufh)
               RET


;GET RAM AND POINTER TO BUFFER

point:         LD   (IX+rptl),0

grpnt:         CALL gtbuf
               LD   B,(IX+rpth)
               LD   C,(IX+rptl)
               ADD  HL,BC
               RET


;INCREMENT RAM POINTER

incrpt:        INC  (IX+rptl)
               RET  NZ
               INC  (IX+rpth)
               RET


;GET THE NEXT TRACK/SECTOR

gtnsr:         LD   D,(IX+nsrh)
               LD   E,(IX+nsrl)
               RET

;SAVE THE NEXT TRACK/SECTOR

svnsr:         LD   (IX+nsrh),D
               LD   (IX+nsrl),E
               RET

;SWAP THE NEXT TRACK/SECTOR

swpnsr:        CALL gtnsr
               LD   (IX+nsrh),H
               LD   (IX+nsrl),L
               RET

outstp:        LD   C,stpout
               JR   step

instp:         LD   C,stpin

step:          CALL sadc
               JP   stpdel

