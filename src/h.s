

;HOOK CODE ROUTINES


;INPUT A HEADER FROM IX

rxhed:         push bc
               push de
               push hl

               ld hl,&4b00
               ld b,48
               ld de,uifa

rxhd1:         call cmr
               defw nrread
               ld (de),a
               inc hl
               inc de
               djnz rxhd1

               pop hl
               pop de
               pop bc
               jp hconr



;OUTPUT DIFA TO ROMUIFA

txinf:         ld a,0
               ld de,uifa
               jr txrom

;OUTPUT DIFA TO ROMDIFA

txhed:         ld a,80
               ld de,difa

;OUTPUT A HEADER

txrom:         ld hl,&4b00
               ld b,0
               ld c,a
               add hl,bc
               ld b,48

txrm1:         ld a,(de)
               call cmr
               defw nrrite
               inc hl
               inc de
               djnz txrm1

               ret


init:          jp hauto


hgthd:         call rxhed
               call ckdrv
               call gtixd
               call gtfle
               call txhed
               ret


hload:         call dschd
               jp ldblk


dschd:         call gtixd
               ld de,(svde)
               call rsad
               call ldhd

               ld hl,(hkhl)
               ld (hd0d1),hl

               ld bc,(hkbc)
               ld a,c
               ld (pges1),a

               ld de,(hkde)
               res 7,d
               ld (hd0b1),de

               ret


;VERIFY FILE

hvery:         call dschd

               ld (ix+rptl),9
hver1:         ld a,d
               or e
               jr nz,hver2

               ld a,c
               and a
               ret z

               dec c
               ld de,16384

hver2:         call lbyt

               cp (hl)
               jp nz,rep12

               dec de
               inc hl
               ld a,h
               cp &c0
               jr c,hver1
               res 6,h
               in a,(251)
               push af
               and %11100000
               ld b,a
               pop af
               inc a
               and %00011111
               or b
               out (251),a
               jr hver1


hsave:         call setf3
               call rxhed
               call ckdrv

               in a,(251)
               ld (port3),a
               and %11100000
               ld b,a
               ld a,(uifa+31)
               and %00011111
               or b
               out (251),a

               call gtixd
               call ofsm
               call svhd
               ld hl,(hd0d1)
               ld de,(hd0b1)
               call svblk
               call cfsm

               ld a,(port3)
               out (251),a
               ret


hdir:          ret

hopen:         ret

hclos:         ret

heof:          ret

hptr:          ret

hpath:         ret

hvar:          call cmr
               defw getint
               ld hl,dvar
               add hl,bc
               call nrrd
               defw var2+&01c2
               inc a
               add hl,hl
               add hl,hl
               ld b,&96

hvar1:         dec b
               add hl,hl
               rla
               bit 7,a
               jr z,hvar1

               res 7,a
               ld e,a
               ld a,b
               ld d,h
               ld c,l
               ld b,0
               call cmr
               defw &0127

               ret


autnam:        defb 1
               defb &ff
               defb &ff
               defb "D"
               defb &10
               defm "AUTO*     "
               defm "    "
               defb 0
               defw &ffff
               defw &ffff
               defw &ffff
               defw &ffff


;LOOK FOR AN AUTO FILE


hauto:         ld hl,autnam
               ld de,dstr1
               ld bc,28
               ldir
               call gtdef
               call ckdrv
               call gtixd

               ld a,&10
               call fdhr
               jp nz,rep20

               call gtflx
               jp autox


;HOOK OPEN FILE

hofle:         call rxhed
               call ofsm
               call svhd
               ret


hsvbk:         jp svblk


hgfle:         call rxhed
               call gtfle
               ld de,(svde)
               call rsad
               call ldhd
               ret

hldbk:         jp ldblk


heraz:         call rxhed
               call ckdrv
               call findc
               jp nz,rep26
               ld (hl),0
               jp wsad


;HOOK READ SECTOR AT DE

hrsad:         ld a,(hka)
               call ckdrx
               call gtixd
               ld de,(hkde)
               call rsad
               ld de,dram
               ld bc,512
               call sze
               jr z,hrsd1
               ld bc,1024
hrsd1:         ld hl,(hkhl)
               call cals
               ex de,hl
               ldir
               ld a,(port1)
               out (251),a
               ret



;HOOK WRITE SECTOR AT DE

hwsad:         ld a,(hka)
               call ckdrx
               ld de,dram
               ld bc,512
               call sze
               jr z,hwsd1
               ld bc,1024
hwsd1:         ld hl,(hkhl)
               call cals
               ldir
               ld a,(port1)
               out (251),a
               call gtixd
               ld de,(hkde)
               call wsad
               ret


;CALCULATE ADDRESS SECTION

cals:          in a,(251)
               ld (port1),a
               ld a,h
               and %11000000
               jp z,rep0
               sub %01000000
               rlca
               rlca
               out (251),a
               ld a,h
               and %00111111
               or %10000000
               ld h,a
               ret


pntp:          ret

cops1:         ret

cops2:         ret

s:             ret



;CONVERT NEW HDR TO OLD

hconr:         call resreg

               ld hl,uifa+1

               call evfile

               ld a,(uifa)
               ld (nstr1),a
               ld (hd001),a

               ld a,(uifa+31)
               ld (page1),a

               ld hl,(uifa+32)
               ld (hd0d1),hl

               ld a,(uifa+34)
               and &1f
               ld (pges1),a

               ld hl,(uifa+35)
               res 7,h
               ld (uifa+35),hl
               ld (hd0b1),hl

               ret


;GET DEFAULTS IN VARIABLE AREA

gtdef:         call nrrd
               defw devl
               ld (lstr1),a
               call nrrd
               defw devn
               ld (dstr1),a
               ret


;EVALUATE FILE INFORMATION

evfile:        call gtdef

               ld (svhl),hl
               ld a,(hl)
               and &df

;CHECK FOR FIRST DIGIT

evfl1:         ld c,a
               inc hl
               ld a,(hl)
               cp ":"
               jr z,evfl3
               sub &30
               cp 10
               jr nc,evfl4

;CHECK FOR SECOND DIGIT

               ld d,a
               inc hl
               ld a,(hl)
               cp ":"
               jr z,evfl2
               sub &30
               cp 10
               jr nc,evfl4

;EVALUATE NUMBER

               ld e,a

               ld a,d
               add a,a
               add a,a
               add a,d
               add a,a
               add a,e
               ld d,a
               inc hl

;CHECK FOR ':'

               ld a,(hl)
               cp ":"
               jr nz,evfl4

evfl2:         ld a,d
               ld (dstr1),a
evfl3:         ld a,c
               ld (lstr1),a
               inc hl
               jr evfl5

evfl4:         ld hl,(svhl)

;FILE NAME START

evfl5:         ld bc,10
               ld de,nstr1+1

evfl6:         ldir

               ld b,4
               call lcnta

               ld a,(lstr1)
               cp "D"
               ret z
               jp rep10

;this code not working

               call nrwr
               defw &5bb7
               ld a,(dstr1)
               cp 3
               jr nc,evfl8
               ld a,112

evfl8:         call nrwr
               defw &5bb8

               ld hl,nstr1+1
               ld a,(hl)
               cp &20
               jr nz,evfl8a
               ld a,&ff
               ld (uifa+1),a
               jr evfl8b

evfl8a:        ld de,uifa+1
               ld bc,14
               ldir

evfl8b:        call txinf

               call bitf3
               jr nz,cspc1
               ld sp,(entsp)
               pop bc
               pop hl
               push hl
               push bc
               set 7,h
               res 6,h
               xor a
               out (251),a
               ld (hl),8
               inc hl
               ld (hl),&e3
               jp ends

cspc1:         call nrrd
               defw &5a33
               rra
               rra
               jr c,cspc4

               call cmr
               defw clslow
               call pmo1
               call beep

cspc2:         call cmr
               defw rdkey
               jr nc,cspc2
cspc3:         call cmr
               defw rdkey
               jr c,cspc3
               call cmr
               defw clslow

cspc4:         ld e,2
               jp end1


zzend:         equ $
