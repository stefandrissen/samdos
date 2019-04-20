

;HOOK CODE ROUTINES


;INPUT A HEADER FROM IX

rxhed:         PUSH BC
               PUSH DE
               PUSH HL

               LD   HL,&4B00
               LD   B,48
               LD   DE,uifa

rxhd1:         CALL cmr
               DEFW nrread
               LD   (DE),A
               INC  HL
               INC  DE
               DJNZ rxhd1

               POP  HL
               POP  DE
               POP  BC
               JP   hconr



;OUTPUT DIFA TO ROMUIFA

txinf:         LD   A,0
               LD   DE,uifa
               JR   txrom

;OUTPUT DIFA TO ROMDIFA

txhed:         LD   A,80
               LD   DE,difa

;OUTPUT A HEADER

txrom:         LD   HL,&4B00
               LD   B,0
               LD   C,A
               ADD  HL,BC
               LD   B,48

txrm1:         LD   A,(DE)
               CALL cmr
               DEFW nrrite
               INC  HL
               INC  DE
               DJNZ txrm1

               RET


init:          JP   hauto


hgthd:         CALL rxhed
               CALL ckdrv
               CALL gtixd
               CALL gtfle
               CALL txhed
               RET


hload:         CALL dschd
               JP   ldblk


dschd:         CALL gtixd
               LD   DE,(svde)
               CALL rsad
               CALL ldhd

               LD   HL,(hkhl)
               LD   (hd0d1),HL

               LD   BC,(hkbc)
               LD   A,C
               LD   (pges1),A

               LD   DE,(hkde)
               RES  7,D
               LD   (hd0b1),DE

               RET


;VERIFY FILE

hvery:         CALL dschd

               LD   (IX+rptl),9
hver1:         LD   A,D
               OR   E
               JR   NZ,hver2

               LD   A,C
               AND  A
               RET  Z

               DEC  C
               LD   DE,16384

hver2:         CALL lbyt

               CP   (HL)
               JP   NZ,rep12

               DEC  DE
               INC  HL
               LD   A,H
               CP   &C0
               JR   C,hver1
               RES  6,H
               IN   A,(251)
               PUSH AF
               AND  %11100000
               LD   B,A
               POP  AF
               INC  A
               AND  %00011111
               OR   B
               OUT  (251),A
               JR   hver1


hsave:         CALL setf3
               CALL rxhed
               CALL ckdrv

               IN   A,(251)
               LD   (port3),A
               AND  %11100000
               LD   B,A
               LD   A,(uifa+31)
               AND  %00011111
               OR   B
               OUT  (251),A

               CALL gtixd
               CALL ofsm
               CALL svhd
               LD   HL,(hd0d1)
               LD   DE,(hd0b1)
               CALL svblk
               CALL cfsm

               LD   A,(port3)
               OUT  (251),A
               RET


hdir:          RET

hopen:         RET

hclos:         RET

heof:          RET

hptr:          RET

hpath:         RET

hvar:          CALL cmr
               DEFW getint
               LD   HL,dvar
               ADD  HL,BC
               CALL nrrd
               DEFW var2+&01C2
               INC  A
               ADD  HL,HL
               ADD  HL,HL
               LD   B,&96

hvar1:         DEC  B
               ADD  HL,HL
               RLA
               BIT  7,A
               JR   Z,hvar1

               RES  7,A
               LD   E,A
               LD   A,B
               LD   D,H
               LD   C,L
               LD   B,0
               CALL cmr
               DEFW &0127

               RET


autnam:        DEFB 1
               DEFB &FF
               DEFB &FF
               DEFB "D"
               DEFB &10
               DEFM "AUTO*     "
               DEFM "    "
               DEFB 0
               DEFW &FFFF
               DEFW &FFFF
               DEFW &FFFF
               DEFW &FFFF


;LOOK FOR AN AUTO FILE


hauto:         LD   HL,autnam
               LD   DE,dstr1
               LD   BC,28
               LDIR
               CALL gtdef
               CALL ckdrv
               CALL gtixd

               LD   A,&10
               CALL fdhr
               JP   NZ,rep20

               CALL gtflx
               JP   autox


;HOOK OPEN FILE

hofle:         CALL rxhed
               CALL ofsm
               CALL svhd
               RET


hsvbk:         JP   svblk


hgfle:         CALL rxhed
               CALL gtfle
               LD   DE,(svde)
               CALL rsad
               CALL ldhd
               RET

hldbk:         JP   ldblk


heraz:         CALL rxhed
               CALL ckdrv
               CALL findc
               JP   NZ,rep26
               LD   (HL),0
               JP   wsad


;HOOK READ SECTOR AT DE

hrsad:         LD   A,(hka)
               CALL ckdrx
               CALL gtixd
               LD   DE,(hkde)
               CALL rsad
               LD   DE,dram
               LD   BC,512
               CALL sze
               JR   Z,hrsd1
               LD   BC,1024
hrsd1:         LD   HL,(hkhl)
               CALL cals
               EX   DE,HL
               LDIR
               LD   A,(port1)
               OUT  (251),A
               RET



;HOOK WRITE SECTOR AT DE

hwsad:         LD   A,(hka)
               CALL ckdrx
               LD   DE,dram
               LD   BC,512
               CALL sze
               JR   Z,hwsd1
               LD   BC,1024
hwsd1:         LD   HL,(hkhl)
               CALL cals
               LDIR
               LD   A,(port1)
               OUT  (251),A
               CALL gtixd
               LD   DE,(hkde)
               CALL wsad
               RET


;CALCULATE ADDRESS SECTION

cals:          IN   A,(251)
               LD   (port1),A
               LD   A,H
               AND  %11000000
               JP   Z,rep0
               SUB  %01000000
               RLCA
               RLCA
               OUT  (251),A
               LD   A,H
               AND  %00111111
               OR   %10000000
               LD   H,A
               RET


pntp:          RET

cops1:         RET

cops2:         RET

s:             RET



;CONVERT NEW HDR TO OLD

hconr:         CALL resreg

               LD   HL,uifa+1

               CALL evfile

               LD   A,(uifa)
               LD   (nstr1),A
               LD   (hd001),A

               LD   A,(uifa+31)
               LD   (page1),A

               LD   HL,(uifa+32)
               LD   (hd0d1),HL

               LD   A,(uifa+34)
               AND  &1F
               LD   (pges1),A

               LD   HL,(uifa+35)
               RES  7,H
               LD   (uifa+35),HL
               LD   (hd0b1),HL

               RET


;GET DEFAULTS IN VARIABLE AREA

gtdef:         CALL nrrd
               DEFW devl
               LD   (lstr1),A
               CALL nrrd
               DEFW devn
               LD   (dstr1),A
               RET


;EVALUATE FILE INFORMATION

evfile:        CALL gtdef

               LD   (svhl),HL
               LD   A,(HL)
               AND  &DF

;CHECK FOR FIRST DIGIT

evfl1:         LD   C,A
               INC  HL
               LD   A,(HL)
               CP   ":"
               JR   Z,evfl3
               SUB  &30
               CP   10
               JR   NC,evfl4

;CHECK FOR SECOND DIGIT

               LD   D,A
               INC  HL
               LD   A,(HL)
               CP   ":"
               JR   Z,evfl2
               SUB  &30
               CP   10
               JR   NC,evfl4

;EVALUATE NUMBER

               LD   E,A

               LD   A,D
               ADD  A,A
               ADD  A,A
               ADD  A,D
               ADD  A,A
               ADD  A,E
               LD   D,A
               INC  HL

;CHECK FOR ':'

               LD   A,(HL)
               CP   ":"
               JR   NZ,evfl4

evfl2:         LD   A,D
               LD   (dstr1),A
evfl3:         LD   A,C
               LD   (lstr1),A
               INC  HL
               JR   evfl5

evfl4:         LD   HL,(svhl)

;FILE NAME START

evfl5:         LD   BC,10
               LD   DE,nstr1+1

evfl6:         LDIR

               LD   B,4
               CALL lcnta

               LD   A,(lstr1)
               CP   "D"
               RET  Z
               JP   rep10

;this code not working

               CALL nrwr
               DEFW &5BB7
               LD   A,(dstr1)
               CP   3
               JR   NC,evfl8
               LD   A,112

evfl8:         CALL nrwr
               DEFW &5BB8

               LD   HL,nstr1+1
               LD   A,(HL)
               CP   &20
               JR   NZ,evfl8a
               LD   A,&FF
               LD   (uifa+1),A
               JR   evfl8b

evfl8a:        LD   DE,uifa+1
               LD   BC,14
               LDIR

evfl8b:        CALL txinf

               CALL bitf3
               JR   NZ,cspc1
               LD   SP,(entsp)
               POP  BC
               POP  HL
               PUSH HL
               PUSH BC
               SET  7,H
               RES  6,H
               XOR  A
               OUT  (251),A
               LD   (HL),8
               INC  HL
               LD   (HL),&E3
               JP   ends

cspc1:         CALL nrrd
               DEFW &5A33
               RRA
               RRA
               JR   C,cspc4

               CALL cmr
               DEFW clslow
               CALL pmo1
               CALL beep

cspc2:         CALL cmr
               DEFW rdkey
               JR   NC,cspc2
cspc3:         CALL cmr
               DEFW rdkey
               JR   C,cspc3
               CALL cmr
               DEFW clslow

cspc4:         LD   E,2
               JP   end1


zzend:         EQU  $
