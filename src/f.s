
;ZX ENTRY INTO RAM SPACE

calll:         CALL gtnc
               CP   &AA      ;mode
               JP   NZ,rep2
               CALL gtnc
               CALL evnum
               CALL ceos

               LD   A,C
               CP   0
               JR   NZ,call1
               LD   A,3
               OUT  (251),A
               LD   A,4
               OUT  (252),A
               DI
               JP   &B914

call1:         CP   1
               JP   NZ,rep2
               LD   A,4
               OUT  (252),A
               JP   snap7


;DISC COPY ROUTINE

copy:          CALL gtnc
               CALL evnam
               CP   &8E       ;to
               JP   NZ,rep2
               CALL gtnc
               CALL evnam2
               CALL ceos

               LD   HL,nstr1+1
               CALL evfile
               CALL ckdisc

               CALL exdat
               LD   HL,nstr1+1
               CALL evfile
               CALL ckdisc
               LD   HL,nstr1+1
               LD   DE,nstr3+1
               LD   BC,14
               LDIR
               LD   A,(dstr1)
               LD   B,A
               LD   A,(dstr2)
               CP   B
               CALL Z,setf5

copy1:         CALL exdat
               CALL ckdrv
               CALL fndfl
               JR   C,copy2

               CALL bitf0
               JP   Z,rep26
               JP   ends

copy2:         CALL gdifa
               CALL rsad
               CALL gtcop
               CALL ldblk

               CALL exdat
               CALL ckdrv
               CALL bitf5
               CALL NZ,tspce1

               CALL trx
               CALL ofsm
               CALL gtcop
               CALL svblk
               CALL cfsm
               CALL setf0
               CALL bitf5
               CALL NZ,tspce2
               JR   copy1


gtcop:         LD   A,3
               OUT  (251),A
               LD   HL,&8000
               LD   A,(difa+34)
               LD   (pges1),A
               LD   DE,(difa+35)
               RET


;CALL UP DIRECTORY

dirx:          CALL gtdef
               LD   A,"*"
               LD   (nstr1+1),A
               LD   A,2
               LD   (sstr1),A
               RET


dir:           CALL gtixd
               CALL dirx

               CALL gtnc
               CALL ciel
               JR   Z,cat1a

               CP   "#"
               JR   NZ,cat1
               CALL evsrm
               CALL separx

cat1:          CALL evdnm

               CALL separ
               CALL Z,evnam
               CP   "!"
               JR   NZ,cat2
               CALL gtnc

cat1a:         CALL ceos

               CALL ckdrv
               LD   A,2
               JR   cat3

cat2:          CALL ceos

               XOR  A
               CALL cmr
               DEFW &014E  ;cls in sam
               CALL ckdrv
               LD   A,4

cat3:          CALL pcat
               JP   ends

;SETUP FOR DIRECTORY
;NSTR1+1,*
;SSTR1  ,2
;A WITH ,2 OR 4

pcat:          PUSH AF
               LD   A,(sstr1)
               CALL cmr
               DEFW stream
               LD   A,&D
               CALL pnt

               CALL pmo8
               LD   A,(drive)
               AND  3
               OR   &30
               CALL pnt
               CALL pmo2
               LD   HL,0
               LD   (cnt),HL
               POP  AF
               CALL fdhr
               CALL pmo3
               CALL tstd
               LD   HL,360
               LD   DE,400
               CP   40
               JR   Z,trk2
               CP   80
               JR   Z,trk1
               CP   168
               JR   Z,trk1
               LD   HL,1160
trk1:          ADD  HL,DE
trk2:          LD   DE,(cnt)
               XOR  A
               SBC  HL,DE
               JR   NC,pct1

               ADD  HL,DE
               EX   DE,HL
               SBC  HL,DE
               LD   A,"-"
               CALL pnt

pct1:          SRL  H
               RR   L
               XOR  A
               CALL pnum4
               LD   A,&D
               CALL pnt
               RET


;ERASE A FILE

eraz:          CALL gtnc
               CP   &A6      ;over
               JR   NZ,eraz1
               CALL setf1
               CALL gtnc

eraz1:         CALL evnam
               CALL ceos

               LD   HL,nstr1+1
               CALL evfile
               CALL ckdisc

eraz3:         CALL fndfl
               JR   NC,eraz5
               CALL bitf1
               JR   NZ,eraz4

               CALL point
               LD   A,(HL)
               BIT  6,A
               JR   Z,eraz4
               CALL beep
               JR   eraz3

eraz4:         CALL point
               LD   (HL),0
               CALL wsad
               CALL setf0
               JR   eraz3

eraz5:         CALL bitf0
               JP   Z,rep26
               JP   ends


;FIND A FILE IN THE DIRECTORY

fndfl:         CALL bitf2
               JR   NZ,fndf4
               CALL setf2
               LD   IX,dchan
               CALL rest

fndf1:         XOR  A
               LD   (IX+4),A
               LD   (fndfr),A

fndf2:         CALL rsad
               LD   (fndts),DE

fndf3:         CALL clrrpt
               LD   A,(fndfr)
               LD   (IX+rpth),A
               CALL grpnt
               LD   A,(HL)
               AND  A
               JR   Z,fndf4

               CALL cknam
               JR   NZ,fndf4
               SCF
               RET

fndf4:         LD   DE,(fndts)
               LD   A,(fndfr)
               CP   1
               JR   Z,fndf5

               INC  A
               LD   (fndfr),A
               JR   fndf2

fndf5:         CALL isect
               JR   NZ,fndf1
               INC  D
               LD   A,D
               CP   4
               RET  NC
               JR   fndf1


;RENAME A FILE

renam:         CALL gtnc
               CALL evnam
               CP   &8E       ;to
               JP   NZ,rep2
               CALL gtnc
               CALL evnam2
               CALL ceos

               LD   HL,nstr1+1
               CALL evfile
               CALL ckdisc

               CALL exdat
               LD   HL,nstr1+1
               CALL evfile
               CALL ckdisc

               CALL findc
               JP   Z,rep28
               CALL exdat

               CALL findc
               JP   NZ,rep26
               INC  HL
               PUSH DE
               LD   DE,nstr2+1
               EX   DE,HL
               LD   BC,10
               LDIR
               POP  DE
               CALL wsad
               JP   ends


;FIND A FILE ROUTINE

findc:         LD   A,&10
               CALL fdhr
               JP   point


;PROTECT ROUTINE

prot:          LD   A,&40
               JR   sfbt

;HIDE FILE ROUTINE

hide:          LD   A,&80

sfbt:          LD   (hstr1),A
               CALL gtnc
               CP   &89       ;off
               JR   NZ,sfb1
               CALL setf1
               CALL gtnc

sfb1:          CALL evnam
               CALL ceos

               LD   HL,nstr1+1
               CALL evfile
               CALL ckdisc

sfb2:          CALL fndfl
               JR   C,sfb3
               CALL bitf0
               JP   Z,rep26
               JP   ends

sfb3:          LD   (IX+rptl),0
               CALL grpnt
               LD   A,(hstr1)
               LD   C,A
               CPL
               LD   B,A
               LD   A,(HL)
               AND  B
               CALL bitf1
               JR   NZ,sfb4
               OR   C
sfb4:          LD   (HL),A
               CALL wsad
               CALL setf0
               JR   sfb2



;SEPARATOR REPORT ROUTINE

separx:        CALL separ
               RET  Z
               JP   rep2


;THE SEPARATOR SUBROUTINE

separ:         CP   ","
               JR   Z,sepa1
               CP   ";"
               JR   Z,sepa1
               CP   ""
               RET

sepa1:         CALL gtnc
               LD   (sva),A
               XOR  A
               LD   A,(sva)
               RET



;EVALUATE PARAMETERS IN SYNTAX

evprm:         CALL gtnc

               CP   &87       ;at
               JP   NZ,rep2

;GET DRIVE NUMBER

               CALL gtnc
               CALL evdnm

;GET TRACK NUMBER

               CALL separx
               CALL evnum
               JR   Z,evpr1

               LD   D,C
               LD   (hkde),DE

;GET SECTOR NUMBER

evpr1:         CALL separx
               CALL evnum
               JR   Z,evpr2

               LD   DE,(hkde)
               LD   E,C
               LD   (hkde),DE

;GET ADDRESS

evpr2:         CALL separx
               CALL evnum
               JR   Z,evpr3

               LD   (hkhl),BC

evpr3:         CALL ceos

               LD   A,(dstr1)
               LD   (hka),A
               RET


;SAVE HEADER INFORMATION

svhd:          LD   HL,hd001
               LD   DE,fsa+211
               LD   B,9
svhd1:         LD   A,(HL)
               LD   (DE),A
               CALL sbyt
               INC  HL
               INC  DE
               DJNZ svhd1
               RET


;WRITE AT A TRACK AND SECTOR

write:         CALL gtixd
               CALL evprm

               CALL hwsad
               JP   ends


;READ AT A TRACK AND SECTOR

read:          CALL gtixd
               CALL evprm

               CALL hrsad
               JP   ends


;LOAD HEADER INFORMATION

ldhd:          LD   B,9
ldhd1:         CALL lbyt
               DJNZ ldhd1
               RET


;LOAD SYNTAX COMMAND

load:          CALL gtixd
               CALL gtnc
               CALL evnum

               PUSH AF
               LD   A,C
               LD   (fstr1),A
               CALL gtdef
               POP  AF
               CALL ceos

               CALL gtfle
autox:         LD   A,(difa)
               CP   &14
               JR   NZ,dlvm1
               CALL bitf7
               JR   Z,dlvm1

;48k SNAPSHOT IS FOUND

               LD   DE,(svde)
               CALL rsad

               IN   A,(250)
               LD   (snprt0),A
               IN   A,(251)
               LD   (snprt1),A
               IN   A,(252)
               LD   (snprt2),A

               LD   A,%00000100
               OUT  (251),A
               OUT  (252),A

               LD   SP,str-20
               LD   HL,&8000
               LD   DE,16384
               LD   A,2
               LD   (pges1),A
               CALL ldblk
               JP   snap7

dlvm1:         CP   &10
               JR   NZ,dlvm2
               CALL bitf7
               JP   NZ,rep13

               CALL nrrdd
               DEFW prog
               PUSH BC
               POP  HL

               CALL nrrd
               DEFW progp
               LD   (uifa+31),A
               LD   (uifa+32),HL
               EX   DE,HL
               LD   C,A

               PUSH BC
               CALL nrrdd
               DEFW eline
               PUSH BC
               POP  HL

               CALL nrrd
               DEFW elinp

               POP  BC
               DEC  HL
               BIT  7,H
               JR   NZ,lab2
               DEC  A
lab2:          PUSH BC
               PUSH DE
               CALL ahln
               PUSH AF
               EX   DE,HL
               LD   A,C
               CALL ahln
               EX   DE,HL
               LD   C,A
               POP  AF
               AND  A
               SBC  HL,DE
               SBC  A,C
               POP  DE
               POP  BC
               RL   H
               RLA
               RL   H
               RLA
               RR   H
               SCF
               RR   H
               LD   (uifa+34),A
               LD   (uifa+35),HL
               XOR  A
               LD   (uifa+15),A

dlvm2:         CALL txinf
               CALL txhed
               JP   endsx


ahln:          RLC  H
               RLC  H
               RRA
               RR   H
               RRA
               RR   H
               AND  7
               RET


;WRITE FORMAT ON DISC

wfod:          CALL gtnc
               CP   &8E       ;to
               JR   Z,wfod1
               CALL ciel
               JR   Z,wfod2

               CALL evnam

               CP   &8E       ;to
               JR   NZ,wfod2
wfod1:         LD   (hstr1),A

               CALL gtnc
               CALL evnam2

wfod2:         CALL ceos

               LD   HL,nstr1+1
               CALL evfile
               CALL ckdisc

               LD   A,(hstr1)
               CP   &8E
               JR   NZ,wfod3

               CALL exdat
               LD   HL,nstr1+1
               CALL evfile
               CALL ckdisc
               CALL exdat


wfod3:         CALL pmo6
               CALL cyes
               JP   NZ,ends

               CALL dfmt
               JP   ends


;CHECK VALID SPECIFIER DISC

ckdisc:        LD   A,(lstr1)
               CP   "D"
               RET  Z
               JP   rep10

;EVALUATE DRIVE NUMBER

evdnm:         CALL evnum
               RET  Z

               PUSH AF
               LD   A,C
               LD   (dstr1),A
               POP  AF
               RET


;EVALUATE STREAM INFORMATION

evsrm:         CALL gtnc
evsrmx:        CALL evnum
               RET  Z

               PUSH AF
               LD   A,C
               CP   16
               JP   NC,rep9
               LD   (sstr1),A
               POP  AF
               RET


;EVALUATE NUMBER ROUTINE

evnum:         CALL cmr
               DEFW expnum
               CALL cfso
               RET  Z

               PUSH AF
               CALL cmr
               DEFW getint
               POP  AF
               RET


               ORG  &5BC8
               DUMP gnd.bank,&1BC8


;EVALUATE CHANNEL SPECIFIER

evsp:          CALL gtnc
evspx:         CALL evstr
               JR   Z,evsp1

               PUSH AF
               LD   A,C
               DEC  A
               OR   B
               JP   NZ,rep10

               EX   DE,HL
               CALL cmr
               DEFW nrread
               CALL alpha
               JP   NC,rep10

               LD   (lstr1),A
               POP  AF

evsp1:         CP   ";"
               RET  Z
               CP   ","
               RET  Z
               JP   rep0




;EVALUATE SECOND FILE NAME

evnam2:        CALL exdat
               CALL evnam

exdat:         PUSH AF
               PUSH BC
               PUSH DE
               PUSH HL

               LD   B,28
               LD   DE,dstr1
               LD   HL,dstr2
exdt1:         LD   A,(DE)
               LD   C,(HL)
               EX   DE,HL
               LD   (DE),A
               LD   (HL),C
               INC  DE
               INC  HL
               DJNZ exdt1

               POP  HL
               POP  DE
               POP  BC
               POP  AF
               RET


;EVALUATE FILE NAME

evnam:         CALL evstr
               RET  Z

               PUSH AF
               LD   A,C
               OR   B
               JP   Z,rep8

               LD   HL,14
               SBC  HL,BC
               JP   C,rep8

               LD   HL,nstr1
               LD   A,15
evnm1:         LD   (HL),&20
               INC  HL
               DEC  A
               JR   NZ,evnm1

               LD   HL,nstr1+1
               EX   DE,HL

               IN   A,(251)
               PUSH AF
               LD   A,(svc)
               AND  &1F
               OUT  (251),A
               LDIR
               POP  AF
               OUT  (251),A

               POP  AF
               RET


;EVALUATE STRING EXPRESSION

evstr:         CALL cmr
               DEFW expstr
               CALL cfso
               RET  Z

               PUSH AF
               CALL cmr
               DEFW getstr
               LD   (svc),A
               POP  AF
               RET


;CHECK FOR ALPHA CHAR

alpha:         CP   &41
               CCF
               RET  NC
               CP   &5B
               RET  C
               CP   &61
               CCF
               RET  NC
               CP   &7B
               RET


;CHECK FOR NUMBER

number:        SUB  &30
               CP   10
               RET  NC
               JP   rep11


;TRANSFER FILE NAMES IN COPY

trx:           LD   HL,difa
               LD   DE,nstr1
               LD   BC,15
               LDIR

               LD   HL,nstr3+1
               LD   DE,nstr1+1
               LD   B,10

trx1:          LD   A,(HL)
               CP   "*"
               JR   Z,trx3
               CP   "?"
               JR   Z,trx2
               LD   (DE),A
trx2:          INC  HL
               INC  DE
               DJNZ trx1
               RET

trx3:          INC  HL
               LD   A,(HL)
               CP   "."
               RET  NZ
trx4:          LD   A,(DE)
               CP   "."
               JR   Z,trx2
               INC  DE
               DJNZ trx4
               RET

gdifa:         CALL point
               LD   DE,difa
               LD   BC,11
               LDIR
               LD   B,4
               CALL lcnta
               LD   (IX+rptl),220
               CALL grpnt
               LD   BC,33
               LDIR

               LD   HL,difa
               LD   DE,uifa
               LD   BC,48
               LDIR

               LD   (IX+rptl),13
               CALL grpnt
               LD   D,(HL)
               INC  HL
               LD   E,(HL)
               RET

