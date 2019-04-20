
;CHECKSUM CALCULATE ON DOS

smchk:         CALL sum
               LD   (HL),A
               RET


;CHECKSUM CHECK ON DOS

chksm:         CALL sum
               CP   (HL)
               RET


sum:           XOR  A
               PUSH AF
               LD   HL,gnd+&0220
               LD   BC,size

sum1:          POP  AF
               ADD  (HL)
               PUSH AF
               INC  HL
               DEC  BC
               LD   A,B
               OR   C
               JR   NZ,sum1
               POP  AF
               RET



;CHECK IS IT END OF LINE

ciel:          CALL gchr
               CP   &D    ;cr
               RET  Z
               CP   &3A    ;:
               RET


;CHECK FOR SYNTAX ONLY

cfso:          LD   (sva),A
               CALL nrrd
               DEFW flags
               AND  &80
               LD   A,(sva)
               RET


;CHECK FOR END OF SYNTAX

ceos:          CALL ciel
               JP   NZ,rep2
               CALL cfso
               RET  NZ


;END OF STATEMENT

ends:          LD   E,0
end1:          XOR  A
               CALL nrwr
               DEFW xptr+1

               CALL nrwr
               DEFW var2+&01C3

               LD   SP,(entsp)
               JP   bcr

endsx:         LD   E,1
               JR   end1


;TEST FOR BREAK ROUTINE

brktst:        LD   A,&F7
               IN   A,(249)
               AND  &20
               RET  NZ

               JP   rep3



;GET THE NEXT CHAR.

gtnc:          CALL cmr
               DEFW &0020
               RET


;GET THE CHAR UNDER POINTER

gchr:          CALL cmr
               DEFW &0018
               RET


;READ A DOUBLE ROM WORD IN BC

nrrdd:         EX   (SP),HL
               PUSH DE
               CALL gthl
               PUSH DE
               CALL cmr
               DEFW nrread
               LD   C,A
               INC  HL
               CALL cmr
               DEFW nrread
               LD   B,A
               POP  HL
               POP  DE
               EX   (SP),HL
               RET


;READ A ROM SYSTEM VARIABLE

nrrd:          EX   (SP),HL
               PUSH DE
               CALL gthl
               PUSH DE
               CALL cmr
               DEFW nrread
               POP  HL
               POP  DE
               EX   (SP),HL
               RET


;WRITE A DOUBLE WORD IN BC

nrwrd:         EX   (SP),HL
               PUSH DE
               CALL gthl
               PUSH DE
               LD   A,C
               CALL cmr
               DEFW nrrite
               INC  HL
               LD   A,B
               CALL cmr
               DEFW nrrite
               POP  HL
               POP  DE
               EX   (SP),HL
               RET


;WRITE ROM SYSTEM VARIABLE

nrwr:          EX   (SP),HL
               PUSH DE
               CALL gthl
               PUSH DE
               CALL cmr
               DEFW nrrite
               POP  HL
               POP  DE
               EX   (SP),HL
               RET


gthl:          LD   E,(HL)
               INC  HL
               LD   D,(HL)
               INC  HL
               EX   DE,HL
               RET


;GET THE HALF FLAG3

hlfg:          EX   (SP),HL
               PUSH HL
               LD   HL,flag3
               RET



;SET FLAG3 SUBROUTINE

setf0:         CALL hlfg
               SET  0,(HL)
               POP  HL
               RET

setf1:         CALL hlfg
               SET  1,(HL)
               POP  HL
               RET

setf2:         CALL hlfg
               SET  2,(HL)
               POP  HL
               RET

setf3:         CALL hlfg
               SET  3,(HL)
               POP  HL
               RET

setf4:         CALL hlfg
               SET  4,(HL)
               POP  HL
               RET

setf5:         CALL hlfg
               SET  5,(HL)
               POP  HL
               RET

setf6:         CALL hlfg
               SET  6,(HL)
               POP  HL
               RET

setf7:         CALL hlfg
               SET  7,(HL)
               POP  HL
               RET



;BIT TEST OF FLAG3 ROUTS.

bitf0:         CALL hlfg
               BIT  0,(HL)
               POP  HL
               RET

bitf1:         CALL hlfg
               BIT  1,(HL)
               POP  HL
               RET

bitf2:         CALL hlfg
               BIT  2,(HL)
               POP  HL
               RET

bitf3:         CALL hlfg
               BIT  3,(HL)
               POP  HL
               RET

bitf4:         CALL hlfg
               BIT  4,(HL)
               POP  HL
               RET

bitf5:         CALL hlfg
               BIT  5,(HL)
               POP  HL
               RET

bitf6:         CALL hlfg
               BIT  6,(HL)
               POP  HL
               RET

bitf7:         CALL hlfg
               BIT  7,(HL)
               POP  HL
               RET

;BORDER COLOUR CHANGE

bcc:           LD   A,(rbcc)
               AND  &F
               RET  Z
               AND  7
               AND  E
               OUT  (ula),A
               RET

;BORDER COLOUR RESTORE

bcr:           PUSH AF
               CALL nrrd
               DEFW &5C4B
               OUT  (ula),A
               POP  AF
               RET


;ERROR REPORT MESSAGES

rep0:          CALL derr
               DEFB 81

rep1:          CALL derr
               DEFB 82

rep2:          CALL derr
               DEFB 83

rep3:          CALL derr
               DEFB 84

rep4:          CALL derr
               DEFB 85

rep5:          CALL derr
               DEFB 86

rep6:          CALL derr
               DEFB 87

rep7:          CALL derr
               DEFB 88

rep8:          CALL derr
               DEFB 89

rep9:          CALL derr
               DEFB 90

rep10:         CALL derr
               DEFB 91

rep11:         CALL derr
               DEFB 92

rep12:         CALL derr
               DEFB 93

rep13:         CALL derr
               DEFB 94

rep14:         CALL derr
               DEFB 95

rep15:         CALL derr
               DEFB 96

rep16:         CALL derr
               DEFB 97

rep17:         CALL derr
               DEFB 98

rep18:         CALL derr
               DEFB 99

rep19:         CALL derr
               DEFB 100

rep20:         CALL derr
               DEFB 101

rep21:         CALL derr
               DEFB 102

rep22:         CALL derr
               DEFB 103

rep23:         CALL derr
               DEFB 104

rep24:         CALL derr
               DEFB 105

rep25:         CALL derr
               DEFB 106

rep26:         CALL derr
               DEFB 107

rep27:         CALL derr
               DEFB 108

rep28:         CALL derr
               DEFB 109

rep29:         CALL derr
               DEFB 110

rep30:         CALL derr
               DEFB 111

rep31:         CALL derr
               DEFB 112


;CONVERT NUMBER IN A

conr:          PUSH DE
               LD   H,0
               LD   L,A
               LD   DE,100
               CALL conr1
               PUSH AF
               LD   DE,10
               CALL conr1
               PUSH AF
               LD   A,L
               ADD  &30
               LD   B,A
               POP  AF
               LD   C,A
               POP  AF
               POP  DE
               RET


conr1:         XOR  A
conr2:         SBC  HL,DE
               JR   C,conr3
               INC  A
               JR   conr2
conr3:         ADD  HL,DE
               AND  A
               JR   NZ,conr4
               LD   A,&20
               RET
conr4:         ADD  &30
               RET



;SAMDOS ERROR PRINT
;DE HOLDS TRACK AND SECTOR

derr:          CALL bcr

               LD   A,D
               CALL conr
               LD   (prtrk),A
               LD   (fmtrk),A
               LD   (prtrk+1),BC
               LD   (fmtrk+1),BC
               LD   A,E
               CALL conr
               LD   (prsec),BC

               CALL nrrdd
               DEFW chadd
               CALL nrwrd
               DEFW xptr

               XOR  A
               LD   (flag3),A
               LD   E,A
               POP  HL
               LD   A,(HL)
               LD   SP,(entsp)
               RET



errtbl:        DEFM "Nonsense in "
               DEFM "SAMDOS 1."
               DEFB "1"+&80

               DEFM "Nonsense in "
               DEFM "SNOS 1."
               DEFB "1"+&80

               DEFM "Statement "
               DEFM "end erro"
               DEFB "r"+&80

               DEFM "Escape requeste"
               DEFB "d"+&80

               DEFM "TRK-"
prtrk:         DEFB &20
               DEFB &20
               DEFB &20
               DEFM ",SCT-"
prsec:         DEFB &20
               DEFB &20
               DEFM ",Erro"
               DEFB "r"+&80

               DEFM "Format TRK-"
fmtrk:         DEFB &20
               DEFB &20
               DEFB &20
               DEFM " los"
               DEFB "t"+&80

               DEFM "Check disc in "
               DEFM "driv"
               DEFB "e"+&80

               DEFM "No 'BOOT' fil"
               DEFB "e"+&80

               DEFM "Invalid file nam"
               DEFB "e"+&80

               DEFM "Invalid statio"
               DEFB "n"+&80

               DEFM "Invalid devic"
               DEFB "e"+&80

               DEFM "Variable not foun"
               DEFB "d"+&80

               DEFM "Verify faile"
               DEFB "d"+&80

               DEFM "Wrong file typ"
               DEFB "e"+&80

               DEFM "Merge erro"
               DEFB "r"+&80

               DEFM "Code erro"
               DEFB "r"+&80

               DEFM "Pupil se"
               DEFB "t"+&80

               DEFM "Invalid cod"
               DEFB "e"+&80

               DEFM "Reading "
               DEFM "a write fil"
               DEFB "e"+&80

               DEFM "Writing "
               DEFM "a read fil"
               DEFB "e"+&80

               DEFM "No AUTO* fil"
               DEFB "e"+&80

               DEFM "Network of"
               DEFB "f"+&80

               DEFM "No such driv"
               DEFB "e"+&80

               DEFM "Disc is write "
               DEFM "protecte"
               DEFB "d"+&80

               DEFM "Not enough space "
               DEFM "on dis"
               DEFB "c"+&80

               DEFM "Directory ful"
               DEFB "l"+&80

               DEFM "File not foun"
               DEFB "d"+&80

               DEFM "End of fil"
               DEFB "e"+&80

               DEFM "File name use"
               DEFB "d"+&80

               DEFM "No SAMDOS loade"
               DEFB "d"+&80

               DEFM "Stream use"
               DEFB "d"+&80

               DEFM "Channel use"
               DEFB "d"+&80

ertabx:        DEFB 0



;NON MASK INT ROUT.

nmi:           LD   (str),SP
               LD   SP,str
               LD   A,I
               PUSH AF
               PUSH HL
               PUSH BC
               PUSH DE
               EX   AF,AF'
               EXX
               PUSH AF
               PUSH HL
               PUSH BC
               PUSH DE
               PUSH IX
               PUSH IY


;PUSH RETURN ADDRESS ON STACK

               LD   HL,snap7
               PUSH HL
               LD   (hksp),SP

;TEST FOR SNAPSHOT TYPE

               LD   A,4
               OUT  (251),A
               LD   HL,&8000
               IM   1

snap3:         LD   BC,&F7FE
               IN   E,(C)

               BIT  1,E       ;save scr
               RET  Z

               BIT  2,E       ;save scr
               JR   NZ,snap3a
               LD   A,&13
               LD   DE,6912
               JR   snap4

snap3a:        BIT  3,E       ;snp 48k
               JR   NZ,snap3b
               LD   A,5
               LD   DE,49152
               JR   snap4

snap3b:        INC  A
               AND  7
               OUT  (C),A
               LD   B,&FE
               IN   E,(C)
               BIT  2,E
               JR   NZ,snap3

snap3c:        IN   E,(C)
               BIT  2,E
               JR   Z,snap3c


               LD   A,(snprt0)
               OUT  (251),A
               LD   A,(snprt2)
               OUT  (252),A
               EI
               JP   ends

;SAVE VARIABLES OF FILE

snap4:         LD   (snme),A
               LD   (snlen),DE
               LD   (snadd),HL

;TEST FOR DIRECTORY SPACE

               LD   IX,dchan
               LD   B,&FE
               IN   A,(C)
               BIT  0,A
               LD   A,1
               JR   NZ,snap4a
               LD   A,2
snap4a:        CALL ckdrx
               LD   A,&40
               CALL fdhr
               RET  NZ

;FORM SNAPSHOT FILE NAME

               LD   A,D
               AND  7
               JR   Z,snap5
               ADD  &30
               LD   (snme+5),A
snap5:         LD   L,E
               SLA  L
               DEC  L
               LD   A,(IX+rpth)
               ADD  L
               ADD  &40
               LD   (snme+6),A

;TRANSFER NAME TO FILE AREA

               LD   HL,snme
               LD   DE,nstr1
               LD   BC,24
               LDIR

;OPEN A FILE

               XOR  A
               LD   (flag3),A
               LD   (pges1),A
               CALL ofsm

;SAVE REGISTERS IN DIRECTORY

               LD   HL,str-20
               LD   BC,22
               LD   A,(nstr1)
               CP   5
               JR   Z,snap6

               CALL svhd
               LD   HL,snptab
               LD   BC,33

snap6:         LD   DE,fsa+220
               LDIR
               LD   HL,(snadd)
               LD   DE,(snlen)
               CALL svblk

               JP   cfsm


snptab:        DEFB &20,&20,&20,&20,&20
               DEFB &20,&20,&20,&20,&20
               DEFB &20
               DEFB 255,255,255,255,255
               DEFB &6E,&00,&80,&00,&00
               DEFB &1B,255,255,255,255
               DEFB 255,255,255,255,255
               DEFB 255,255


;RETURN ADDRESS OF SNAPSHOT

snap7:         DI
               LD   A,3
               OUT  (251),A

               LD   HL,0
               LD   (hksp),HL
               LD   SP,str-20
               POP  IY
               POP  IX
               POP  DE
               POP  BC
               POP  HL
               POP  AF
               EX   AF,AF'
               EXX

               LD   HL,nmi
               LD   (&B8F6),HL
               LD   A,(snprt0)
               LD   (&B8F8),A
               LD   A,(snprt1)
               LD   (&B8F9),A
               LD   A,(snprt2)
               LD   (&B8FA),A

               POP  DE
               POP  BC
               POP  HL
               POP  AF
               LD   I,A
               CP   0
               JR   Z,snap8
               CP   &3F
               JR   Z,snap8
               IM   2
snap8:         LD   SP,(str)

               JP   &8000+&3900

