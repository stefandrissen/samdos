
;CHECKSUM CALCULATE ON DOS

smchk:         call sum
               ld (hl),a
               ret


;CHECKSUM CHECK ON DOS

chksm:         call sum
               cp (hl)
               ret


sum:           xor a
               push af
               ld hl,gnd+&0220
               ld bc,size

sum1:          pop af
               add (hl)
               push af
               inc hl
               dec bc
               ld a,b
               or c
               jr nz,sum1
               pop af
               ret



;CHECK IS IT END OF LINE

ciel:          call gchr
               cp &0d         ;cr
               ret z
               cp &3a         ;:
               ret


;CHECK FOR SYNTAX ONLY

cfso:          ld (sva),a
               call nrrd
               defw flags
               and &80
               ld a,(sva)
               ret


;CHECK FOR END OF SYNTAX

ceos:          call ciel
               jp nz,rep2
               call cfso
               ret nz


;END OF STATEMENT

ends:          ld e,0
end1:          xor a
               call nrwr
               defw xptr+1

               call nrwr
               defw var2+&01c3

               ld sp,(entsp)
               jp bcr

endsx:         ld e,1
               jr end1


;TEST FOR BREAK ROUTINE

brktst:        ld a,&f7
               in a,(249)
               and &20
               ret nz

               jp rep3



;GET THE NEXT CHAR.

gtnc:          call cmr
               defw &0020
               ret


;GET THE CHAR UNDER POINTER

gchr:          call cmr
               defw &0018
               ret


;READ A DOUBLE ROM WORD IN BC

nrrdd:         ex (sp),hl
               push de
               call gthl
               push de
               call cmr
               defw nrread
               ld c,a
               inc hl
               call cmr
               defw nrread
               ld b,a
               pop hl
               pop de
               ex (sp),hl
               ret


;READ A ROM SYSTEM VARIABLE

nrrd:          ex (sp),hl
               push de
               call gthl
               push de
               call cmr
               defw nrread
               pop hl
               pop de
               ex (sp),hl
               ret


;WRITE A DOUBLE WORD IN BC

nrwrd:         ex (sp),hl
               push de
               call gthl
               push de
               ld a,c
               call cmr
               defw nrrite
               inc hl
               ld a,b
               call cmr
               defw nrrite
               pop hl
               pop de
               ex (sp),hl
               ret


;WRITE ROM SYSTEM VARIABLE

nrwr:          ex (sp),hl
               push de
               call gthl
               push de
               call cmr
               defw nrrite
               pop hl
               pop de
               ex (sp),hl
               ret


gthl:          ld e,(hl)
               inc hl
               ld d,(hl)
               inc hl
               ex de,hl
               ret


;GET THE HALF FLAG3

hlfg:          ex (sp),hl
               push hl
               ld hl,flag3
               ret



;SET FLAG3 SUBROUTINE

setf0:         call hlfg
               set 0,(hl)
               pop hl
               ret

setf1:         call hlfg
               set 1,(hl)
               pop hl
               ret

setf2:         call hlfg
               set 2,(hl)
               pop hl
               ret

setf3:         call hlfg
               set 3,(hl)
               pop hl
               ret

setf4:         call hlfg
               set 4,(hl)
               pop hl
               ret

setf5:         call hlfg
               set 5,(hl)
               pop hl
               ret

setf6:         call hlfg
               set 6,(hl)
               pop hl
               ret

setf7:         call hlfg
               set 7,(hl)
               pop hl
               ret



;BIT TEST OF FLAG3 ROUTS.

bitf0:         call hlfg
               bit 0,(hl)
               pop hl
               ret

bitf1:         call hlfg
               bit 1,(hl)
               pop hl
               ret

bitf2:         call hlfg
               bit 2,(hl)
               pop hl
               ret

bitf3:         call hlfg
               bit 3,(hl)
               pop hl
               ret

bitf4:         call hlfg
               bit 4,(hl)
               pop hl
               ret

bitf5:         call hlfg
               bit 5,(hl)
               pop hl
               ret

bitf6:         call hlfg
               bit 6,(hl)
               pop hl
               ret

bitf7:         call hlfg
               bit 7,(hl)
               pop hl
               ret

;BORDER COLOUR CHANGE

bcc:           ld a,(rbcc)
               and &0f
               ret z
               and 7
               and e
               out (ula),a
               ret

;BORDER COLOUR RESTORE

bcr:           push af
               call nrrd
               defw &5c4b
               out (ula),a
               pop af
               ret


;ERROR REPORT MESSAGES

rep0:          call derr
               defb 81

rep1:          call derr
               defb 82

rep2:          call derr
               defb 83

rep3:          call derr
               defb 84

rep4:          call derr
               defb 85

rep5:          call derr
               defb 86

rep6:          call derr
               defb 87

rep7:          call derr
               defb 88

rep8:          call derr
               defb 89

rep9:          call derr
               defb 90

rep10:         call derr
               defb 91

rep11:         call derr
               defb 92

rep12:         call derr
               defb 93

rep13:         call derr
               defb 94

rep14:         call derr
               defb 95

rep15:         call derr
               defb 96

rep16:         call derr
               defb 97

rep17:         call derr
               defb 98

rep18:         call derr
               defb 99

rep19:         call derr
               defb 100

rep20:         call derr
               defb 101

rep21:         call derr
               defb 102

rep22:         call derr
               defb 103

rep23:         call derr
               defb 104

rep24:         call derr
               defb 105

rep25:         call derr
               defb 106

rep26:         call derr
               defb 107

rep27:         call derr
               defb 108

rep28:         call derr
               defb 109

rep29:         call derr
               defb 110

rep30:         call derr
               defb 111

rep31:         call derr
               defb 112


;CONVERT NUMBER IN A

conr:          push de
               ld h,0
               ld l,a
               ld de,100
               call conr1
               push af
               ld de,10
               call conr1
               push af
               ld a,l
               add &30
               ld b,a
               pop af
               ld c,a
               pop af
               pop de
               ret


conr1:         xor a
conr2:         sbc hl,de
               jr c,conr3
               inc a
               jr conr2
conr3:         add hl,de
               and a
               jr nz,conr4
               ld a,&20
               ret
conr4:         add &30
               ret



;SAMDOS ERROR PRINT
;DE HOLDS TRACK AND SECTOR

derr:          call bcr

               ld hl,(hksp)
               ld a,h
               or l
               jr z,derr1
               ld sp,hl
               ret

derr1:         ld a,d
               call conr
               ld (prtrk),a
               ld (fmtrk),a
               ld (prtrk+1),bc
               ld (fmtrk+1),bc
               ld a,e
               call conr
               ld (prsec),bc

               call nrrdd
               defw chadd
               call nrwrd
               defw xptr

               xor a
               ld (flag3),a
               ld e,a
               pop hl
               ld a,(hl)
               ld sp,(entsp)
               ret



errtbl:        defm "Nonsense in "
               defm "SAMDOS 1."
               defb "1"+&80

               defm "Nonsense in "
               defm "SNOS 1."
               defb "1"+&80

               defm "Statement "
               defm "end erro"
               defb "r"+&80

               defm "Escape requeste"
               defb "d"+&80

               defm "TRK-"
prtrk:         defb &20
               defb &20
               defb "0"
               defm ",SCT-"
prsec:         defb &20
               defb "5"
               defm ",Erro"
               defb "r"+&80

               defm "Format TRK-"
fmtrk:         defb &20
               defb &20
               defb "0"
               defm " los"
               defb "t"+&80

               defm "Check disk in "
               defm "driv"
               defb "e"+&80

               defm "No ""BOOT"" fil"
               defb "e"+&80

               defm "Invalid file nam"
               defb "e"+&80

               defm "Invalid statio"
               defb "n"+&80

               defm "Invalid devic"
               defb "e"+&80

               defm "Variable not foun"
               defb "d"+&80

               defm "Verify faile"
               defb "d"+&80

               defm "Wrong file typ"
               defb "e"+&80

               defm "Merge erro"
               defb "r"+&80

               defm "Code erro"
               defb "r"+&80

               defm "Pupil se"
               defb "t"+&80

               defm "Invalid cod"
               defb "e"+&80

               defm "Reading "
               defm "a write fil"
               defb "e"+&80

               defm "Writing "
               defm "a read fil"
               defb "e"+&80

               defm "no AUTO* fil"
               defb "e"+&80

               defm "Network of"
               defb "f"+&80

               defm "No such driv"
               defb "e"+&80

               defm "Disk is write "
               defm "protecte"
               defb "d"+&80

               defm "Not enough space "
               defm "on dis"
               defb "k"+&80

               defm "Directory ful"
               defb "l"+&80

               defm "File not foun"
               defb "d"+&80

               defm "End of fil"
               defb "e"+&80

               defm "File name use"
               defb "d"+&80

               defm "No SAMDOS loade"
               defb "d"+&80

               defm "Stream use"
               defb "d"+&80

               defm "Channel use"
               defb "d"+&80

ertabx:        defb 0



;NON MASK INT ROUT.

nmi:           ld (str),sp
               ld sp,str
               ld a,i
               push af
               push hl
               push bc
               push de
               ex af,af'
               exx
               push af
               push hl
               push bc
               push de
               push ix
               push iy


;PUSH RETURN ADDRESS ON STACK

               ld hl,snap7
               push hl
               ld (hksp),sp

;TEST FOR SNAPSHOT TYPE

               ld a,4
               out (251),a
               ld hl,&8000
               im 1

snap3:         ld bc,&f7fe
               in e,(c)

               bit 1,e        ;save scr
               ret z

               bit 2,e        ;save scr
               jr nz,snap3a
               ld a,&13
               ld de,6912
               jr snap4

snap3a:        bit 3,e        ;snp 48k
               jr nz,snap3b
               ld a,5
               ld de,49152
               jr snap4

snap3b:        inc a
               and 7
               out (c),a
               ld b,&fe
               in e,(c)
               bit 2,e
               jr nz,snap3

snap3c:        in e,(c)
               bit 2,e
               jr z,snap3c

               ld bc,0
snap3d:        dec bc
               ld a,b
               or c
               jr nz,snap3d

               ld a,(snprt0)
               out (251),a
               ld a,(snprt2)
               out (252),a
               ei
               jp ends

;SAVE VARIABLES OF FILE

snap4:         ld (snme),a
               ld (snlen),de
               ld (snadd),hl

;TEST FOR DIRECTORY SPACE

               ld ix,dchan
               ld b,&fe
               in a,(c)
               bit 0,a
               ld a,1
               jr nz,snap4a
               ld a,2
snap4a:        call ckdrx
               ld a,&40
               call fdhr
               ret nz

;FORM SNAPSHOT FILE NAME

               ld a,d
               and 7
               jr z,snap5
               add &30
               ld (snme+5),a
snap5:         ld l,e
               sla l
               dec l
               ld a,(ix+rpth)
               add l
               add &40
               ld (snme+6),a

;TRANSFER NAME TO FILE AREA

               ld hl,snme
               ld de,nstr1
               ld bc,24
               ldir

;OPEN A FILE

               xor a
               ld (flag3),a
               ld (pges1),a
               call ofsm

;SAVE REGISTERS IN DIRECTORY

               ld hl,str-20
               ld bc,22
               ld a,(nstr1)
               cp 5
               jr z,snap6

               call svhd
               ld hl,snptab
               ld bc,33

snap6:         ld de,fsa+220
               ldir
               ld hl,(snadd)
               ld de,(snlen)
               call svblk

               jp cfsm


snptab:        defb &20,&20,&20,&20,&20
               defb &20,&20,&20,&20,&20
               defb &20
               defb 255,255,255,255,255
               defb &6e,&00,&80,&00,&00
               defb &1b,255,255,255,255
               defb 255,255,255,255,255
               defb 255,255


;RETURN ADDRESS OF SNAPSHOT

snap7:         di
               ld a,3
               out (251),a

               ld hl,0
               ld (hksp),hl
               ld sp,str-20
               pop iy
               pop ix
               pop de
               pop bc
               pop hl
               pop af
               ex af,af'
               exx

               ld hl,nmi
               ld (&b8f6),hl
               ld a,(snprt0)
               ld (&b8f8),a
               ld a,(snprt1)
               ld (&b8f9),a
               ld a,(snprt2)
               ld (&b8fa),a

               pop de
               pop bc
               pop hl
               pop af
               ld i,a
               cp 0
               jr z,snap8
               cp &3f
               jr z,snap8
               im 2
snap8:         ld sp,(str)

               jp &8000+&3900

