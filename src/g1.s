
min:           EQU  191
mto:           EQU  223


;MOVE COMMAND

move:          CALL evmov
               CP   204
               JP   NZ,rep0
               CALL exdat
               CALL evmov
               CALL exdat
               CALL ceos

               CALL setf2

               LD   A,min
               LD   (fstr1),A
               CALL opmov
               LD   HL,(chans)
               PUSH HL
               LD   A,(fstr1)
               LD   (nstr2),A
               CALL exdat
               LD   A,mto
               LD   (fstr1),A
               LD   IX,dchan
               CALL opmov
               JR   NC,mova

               LD   IX,(nstr2)
               CALL rclmx
               POP  HL
               POP  HL
               JP   ends

mova:          CALL exdat
               POP  DE
               LD   HL,(chans)
               OR   A
               SBC  HL,DE
               LD   DE,(nstr1)
               ADD  HL,DE
               LD   (nstr1),HL

move1:         LD   HL,(nstr1)
               LD   (curchl),HL
move2:         CALL cmr
               DEFW &15E6
               JR   C,move3
               JR   Z,move2
               JR   move4

move3:         LD   HL,(nstr2)
               LD   (curchl),HL
               CALL cmr
               DEFW &15F2
               JR   move1

move4:         XOR  A
               LD   (flag3),A
               LD   HL,(chans)
               PUSH HL
               CALL exdat
               CALL clmov
               CALL exdat
               POP  DE
               LD   HL,(chans)
               OR   A
               SBC  HL,DE
               LD   DE,(nstr1)
               ADD  HL,DE
               LD   (nstr1),HL
               CALL clmov
               CALL cltemp
               JP   ends


;RECLAIM A CHANNEL

rclmx:         LD   C,(IX+9)
               LD   B,(IX+10)
               PUSH BC
               PUSH IX
               POP  HL
               CALL cmr
               DEFW &19E8
               POP  BC
               RET

;EVALUATE A MOVE SYNTAX

evmov:         CALL gtnc
               CP   "#"
               JP   Z,evsrm

evsyn:         LD   (lstr1),A
               AND  &DF
               CP   "D"
               CALL NZ,evspx
               CALL evnum
               CALL separ
               JP   Z,evnam
               CALL cfso
               RET  Z
               PUSH AF
               LD   A,(lstr1)
               AND  &DF
               CP   "D"
               JP   Z,rep2
               CP   "M"
               JP   Z,rep2
               POP  AF
               RET


;OPEN A MOVE CHANNEL

opmov:         LD   A,(sstr1)
               INC  A
               JR   Z,opmv1
               DEC  A
               CALL cmr
               DEFW &1601
               LD   HL,(curchl)
               LD   (nstr1),HL
               RET

opmv1:         LD   A,(lstr1)
               AND  &DF
               CP   "M"
               JR   Z,opmv2
               CP   "D"
               JR   NZ,opmv3
opmv2:         CALL ckdrv
               CALL opend
               LD   A,(nstr1)
               LD   (fstr1),A
               LD   (nstr1),IX
               RET

opmv3:         CP   "N"
               JP   NZ,rep0
               CALL setf3
               RET


;CLOSE A MOVE CHANNEL

clmov:         LD   A,(sstr1)
               INC  A
               RET  NZ

               LD   A,(lstr1)
               AND  &DF
               CP   "N"
               JR   Z,clmv1
               LD   IX,(nstr1)
               JP   deld

clmv1:         RET


;RECLAIM TEMPORARY CHANNELS

cltemp:        LD   IX,(chans)
               LD   DE,&0014
               ADD  IX,DE
cltm1:         LD   A,(IX)
               CP   &80
               RET  Z

               LD   A,(IX+4)
               CP   "D"+&80
               JR   NZ,cltm2
               CALL deld
               JR   cltemp

cltm2:         CALL bitf1
               JR   Z,cltm3
               CALL rclmx
               JR   cltemp

cltm3:         LD   E,(IX+9)
               LD   D,(IX+10)
               ADD  IX,DE
               JR   cltm1


deld:          PUSH IX
               POP  HL
               LD   DE,(chans)
               OR   A
               SBC  HL,DE
               INC  HL
               LD   (svtrs),HL
               JP   clrchd


;OPEN# COMMAND SYNTAX ROUTINE

open:          CALL evsrm
               CALL separ
               JP   NZ,rep0
               CALL evsyn
               CP   &D
               JR   Z,open2
               CP   min
               JR   Z,open1
               CP   mto
               JP   NZ,rep2
open1:         LD   (fstr1),A
               CALL gtnc

open2:         CALL ceos
               LD   A,(sstr1)
               CALL cmr
               DEFW &1727
               LD   HL,&0011
               AND  A
               SBC  HL,BC
               JP   C,rep30
               LD   A,(lstr1)
               AND  &DF
               CP   "D"
               JR   Z,open3
               CP   "M"
               JP   NZ,rep0
open3:         CALL ckdrv
               LD   A,10
               LD   (nstr1),A
               CALL opdst
               JP   ends


;OPEN A STREAM TO 'D' CHANNEL

opdst:         LD   A,(sstr1)
               ADD  A,A
               LD   HL,&5C16
               LD   E,A
               LD   D,0
               ADD  HL,DE
               PUSH HL
               CALL opend
               POP  DE
               RET  C

               BIT  0,(IX+mflg)
               JR   Z,opdst1
;      IN   A,(COMM)
;      BIT  6,A
               JR   Z,opdst1
               CALL rclmx
               JP   rep23

opdst1:        RES  7,(IX+4)
               EX   DE,HL
               LD   (HL),E
               INC  HL
               LD   (HL),D
               RET


;OPEN A  'D' DISC CHANNEL

opend:         LD   IX,(chans)
               LD   DE,&14
               ADD  IX,DE
opnd1:         LD   A,(IX)
               CP   &80
               JR   Z,opnd4

;FOUND AN OPEN CHANNEL

               LD   A,(IX+4)
               AND  &5F
               CP   "D"
               JR   NZ,opnd3
               LD   A,(dstr1)
               CP   (IX+mdrv)
               JR   NZ,opnd3

;CHECK NAME OF CHANNEL

               PUSH IX
               POP  HL
               LD   DE,name
               ADD  HL,DE
               EX   DE,HL
               LD   HL,nstr1+1
               LD   B,10
opnd2:         LD   A,(DE)
               XOR  (HL)
               AND  &DF
               JR   NZ,opnd3
               INC  HL
               INC  DE
               DJNZ opnd2

               JP   rep31

;GET THE LENGTH OF CHANNEL

opnd3:         LD   E,(IX+9)
               LD   D,(IX+10)
               ADD  IX,DE
               JR   opnd1

;TEST DIRECTORY FOR FILENAME

opnd4:         PUSH IX
               LD   A,&10
               CALL fdhr
               LD   A,(fstr1)
               JP   NZ,opnd5

;FILE FOUND

               CP   mto
               JP   Z,opnd6

;OPEN A READ CHANNEL

               LD   BC,551
               CALL crmch

;SET HL TO FILE IN DRAM

               CALL point

;RESTORE CHANNEL ADDRESS

               POP  IX
               LD   A,(dstr1)
               LD   (IX+mdrv),A

               LD   A,0
               LD   (IX+mflg),A

               LD   BC,rram
               LD   (IX+bufl),C
               LD   (IX+bufh),B

;TRANSFER NAME OF FILE

               PUSH HL

               PUSH IX
               POP  HL
               LD   DE,ffsa
               ADD  HL,DE
               EX   DE,HL
               POP  HL
               LD   BC,11
               LD   A,(HL)
               LD   (nstr1),A
               LDIR

               INC  HL
               INC  HL
               LD   B,(HL)
               INC  HL
               LD   C,(HL)
               PUSH BC

               LD   BC,196
               ADD  HL,BC
               LD   A,(HL)
               LD   (IX+18),A
               INC  HL

               LD   BC,9
               LDIR

               LD   DE,str-20
               LD   BC,22
               LDIR

               POP  DE
               CALL rsad
               JR   opnd7


;FILE NOT FOUND IS IT A READ?

opnd5:         CP   min
               JP   Z,rep26

;OPEN A WRITE FILE

opnd6:         LD   BC,787
               CALL crmch

               POP  IX
               LD   A,(dstr1)
               LD   (IX+mdrv),A

               LD   A,1
               LD   (IX+mflg),A

               LD   BC,wram
               LD   (IX+bufl),C
               LD   (IX+bufh),B

               CALL ofsm
               JR   Z,opnd6a

               LD   BC,787
               PUSH IX
               POP  HL
               CALL cmr
               DEFW &19E8
               SCF
               RET

opnd6a:        CALL bitf2
               JR   Z,opnd7

               PUSH IX
               POP  HL
               LD   DE,230
               ADD  HL,DE
               EX   DE,HL
               LD   HL,(nstr2)
               LD   BC,rtyp
               ADD  HL,BC
               LD   BC,9
               LDIR

               LD   HL,str-20
               LD   BC,22
               LDIR

;ENTER CHANNEL DATA

opnd7:         PUSH IX
               POP  DE
               LD   HL,mtbls
               LD   BC,11
               LDIR


;CALCULATE STREAM OFFSET

               PUSH IX
               POP  HL
               LD   DE,(chans)
               OR   A
               SBC  HL,DE
               INC  HL
               RET


;CREATE THE 'D' CHANNEL

crmch:         LD   (mlen),BC
               LD   HL,(prog)
               DEC  HL
               PUSH HL
               PUSH BC
               CALL cmr
               DEFW &1655
               POP  BC
               POP  HL

;CLEAR THE NEW CHANNEL AREA

crmc1:         LD   (HL),0
               INC  HL
               DEC  BC
               LD   A,B
               OR   C
               JR   NZ,crmc1
               RET


;DISC 'D' CHANNEL DATA

mtbls:         DEFW &0008
               DEFW &0008
               DEFB "D"+&80
               DEFW mchwr
               DEFW mchrd
mlen:          DEFW 0


;CLOSE# STREAMS COMMAND

close:         CALL gtnc
               CP   "*"
               JP   NZ,rep0
               CALL gtnc
               CP   &D
               JR   Z,clos1
               CP   &3A
               JR   Z,clos1
               CALL evsrmx
               CALL ceos
               LD   A,(sstr1)
               CALL clsrm
               JP   ends

clos1:         CALL ceos
               JR   clrs1


;CLEAR# STREAMS COMMAND

clear:         CALL gtnc
               CP   "#"
               JP   NZ,rep0
               CALL gtnc
               CALL ceos
               CALL setf1

clrs1:         XOR  A
clrs2:         PUSH AF
               CALL clsrm
               POP  AF
               INC  A
               CP   16
               JR   C,clrs2

               CALL cltemp
               XOR  A
               LD   (samcnt),A
               LD   (flag3),A
               JP   ends


;CLEAR STREAM AND CLOSE CHANNEL

clsrm:         CALL cmr
               DEFW &1727
               LD   A,C
               OR   B
               RET  Z

;CLOSE THE STREAM

               LD   (svtrs),BC
               PUSH HL
               LD   HL,(chans)
               DEC  HL
               ADD  HL,BC
               EX   (SP),HL
               CALL cmr
               DEFW &16EB
               POP  IX
               LD   A,B
               OR   C
               RET  NZ

;TEST FOR DISC 'D' CHANNEL

               LD   A,(IX+4)
               AND  &5F
               CP   "D"
               JR   NZ,rclaim
clrchd:        BIT  0,(IX+mflg)
               JR   Z,rclaim
               CALL bitf1
               JR   NZ,rclaim

;AN OPEN WRITE CHANNEL

               CALL cfsm

;RECLAIM THE CHANNEL

rclaim:        CALL rclmx

;CLOSE AND UPDATE STREAM DISP

               XOR  A
               LD   HL,&5C16
rclm1:         LD   (svhl),HL
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               LD   HL,(svtrs)
               AND  A
               SBC  HL,DE
               JR   NC,rclm4
               EX   DE,HL
               AND  A
               SBC  HL,BC
               EX   DE,HL
               LD   HL,(svhl)
               LD   (HL),E
               INC  HL
               LD   (HL),D
rclm4:         LD   HL,(svhl)
               INC  HL
               INC  HL
               INC  A
               CP   16
               JR   C,rclm1
               RET


;CLS ROUTINE

cls:           CALL gtnc
               CP   "#"
               JP   NZ,rep0
               CALL gtnc
               CALL ceos

               LD   HL,&0038
               LD   (attrp),HL
               LD   (attrt),HL
               LD   (IY+14),L
               LD   (IY+87),H
               LD   A,7
               OUT  (ula),A
               CALL cmr
               DEFW &D6B
               JP   ends


;DISC 'D' CHANNEL INPUT

mchrd:         LD   IX,(curchl)
               LD   HL,mchin


;MAIN INPUT ROUTINE

cinput:        RES  3,(IY+2)
               PUSH HL
               LD   HL,(errsp)
               LD   E,(HL)
               INC  HL
               LD   D,(HL)
               AND  A
               LD   HL,&107F
               SBC  HL,DE
               JR   NZ,inkey

;INPUT# COMMAND

               POP  HL
               LD   SP,(errsp)
               POP  DE
               POP  DE
               LD   (errsp),DE
inp1:          PUSH HL
               LD   DE,inp2
               PUSH DE
               JP   (HL)

inp2:          JR   C,inp3
               JP   NZ,rep27
               POP  HL
               JR   inp1

inp3:          CP   &D
               JR   Z,inp4
               CALL cmr
               DEFW &F85
               POP  HL
               JR   inp1

inp4:          POP  HL
;      JP   RTMP

;INKEY$ INPUT FUNCTION

inkey:         POP  HL
               LD   DE,ink1
               PUSH DE
               JP   (HL)

ink1:          RET  C
               RET  Z
               CALL bitf2
               JP   Z,rep27
               OR   1
               RET


;DISC 'D' CHANNEL READ

mchin:         BIT  0,(IX+mflg)
               JP   NZ,rep18

;DECREMENT THE FILE COUNT

               LD   A,(IX+31)
               SUB  1
               LD   (IX+31),A
               JR   NC,mchn1

               LD   A,(IX+32)
               SUB  1
               LD   (IX+32),A
               JR   NC,mchn1

               LD   A,(IX+18)
               SUB  1
               LD   (IX+18),A
               JR   NC,mchn1

               XOR  A
               ADD  &D
               RET

mchn1:         CALL lbyt
               CALL bcr
               SCF
               RET



;DISC 'D' CHANNEL OUTPUT

mchwr:         LD   IX,(curchl)
               BIT  0,(IX+mflg)
               JP   Z,rep19

               CALL sbyt
               CALL bcr

;UPDATE BLOCK SIZE COUNT

               CALL bitf2
               RET  NZ

               PUSH IX
               LD   BC,229
               ADD  IX,BC
               INC  (IX+2)
               JR   NZ,mchwr1
               INC  (IX+3)
               JR   NZ,mchwr1
               INC  (IX)

mchwr1:        POP  IX
               RET

