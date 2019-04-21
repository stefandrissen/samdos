
;ZX ENTRY INTO RAM SPACE

calll:         call gtnc
               cp &aa         ;mode
               jp nz,rep2
               call gtnc
               call evnum
               call ceos

               ld a,c
               cp 0
               jr nz,call1
               ld a,3
               out (251),a
               ld a,4
               out (252),a
               di
               jp &b914

call1:         cp 1
               jp nz,rep2
               ld a,4
               out (252),a
               jp snap7


;DISC COPY ROUTINE

copy:          call gtnc
               call evnam
               cp &8e         ;to
               jp nz,rep2
               call gtnc
               call evnam2
               call ceos

               ld hl,nstr1+1
               call evfile
               call ckdisc

               call exdat
               ld hl,nstr1+1
               call evfile
               call ckdisc
               ld hl,nstr1+1
               ld de,nstr3+1
               ld bc,14
               ldir
               ld a,(dstr1)
               ld b,a
               ld a,(dstr2)
               cp b
               call z,setf5

copy1:         call exdat
               call ckdrv
               call fndfl
               jr c,copy2

               call bitf0
               jp z,rep26
               jp ends

copy2:         call gdifa
               call rsad
               call gtcop
               call ldblk

               call exdat
               call ckdrv
               call bitf5
               call nz,tspce1

               call trx
               call ofsm
               call gtcop
               call svblk
               call cfsm
               call setf0
               call bitf5
               call nz,tspce2
               jr copy1


gtcop:         ld a,3
               out (251),a
               ld hl,&8000
               ld a,(difa+34)
               ld (pges1),a
               ld de,(difa+35)
               ret


;CALL UP DIRECTORY

dirx:          call gtdef
               ld a,"*"
               ld (nstr1+1),a
               ld a,2
               ld (sstr1),a
               ret


dir:           call gtixd
               call dirx

               call gtnc
               call ciel
               jr z,cat1a

               cp "#"
               jr nz,cat1
               call evsrm
               call separx

cat1:          call evdnm

               call separ
               call z,evnam
               cp "!"
               jr nz,cat2
               call gtnc

cat1a:         call ceos

               call ckdrv
               ld a,2
               jr cat3

cat2:          call ceos

               xor a
               call cmr
               defw &014e     ;cls in sam
               call ckdrv
               ld a,4

cat3:          call pcat
               jp ends

;SETUP FOR DIRECTORY
;NSTR1+1,*
;SSTR1  ,2
;A WITH ,2 OR 4

pcat:          push af
               ld a,(sstr1)
               call cmr
               defw stream
               ld a,&0d
               call pnt

               call pmo8
               ld a,(drive)
               and 3
               or &30
               call pnt
               call pmo2
               ld hl,0
               ld (cnt),hl
               pop af
               call fdhr
               call pmo3
               call tstd
               ld hl,360
               ld de,400
               cp 40
               jr z,trk2
               cp 80
               jr z,trk1
               cp 168
               jr z,trk1
               ld hl,1160
trk1:          add hl,de
trk2:          ld de,(cnt)
               xor a
               sbc hl,de
               jr nc,pct1

               add hl,de
               ex de,hl
               sbc hl,de
               ld a,"-"
               call pnt

pct1:          srl h
               rr l
               xor a
               call pnum4
               ld a,&0d
               call pnt
               ret


;ERASE A FILE

eraz:          call gtnc
               cp &a6         ;over
               jr nz,eraz1
               call setf1
               call gtnc

eraz1:         call evnam
               call ceos

               ld hl,nstr1+1
               call evfile
               call ckdisc

eraz3:         call fndfl
               jr nc,eraz5
               call bitf1
               jr nz,eraz4

               call point
               ld a,(hl)
               bit 6,a
               jr z,eraz4
               call beep
               jr eraz3

eraz4:         call point
               ld (hl),0
               call wsad
               call setf0
               jr eraz3

eraz5:         call bitf0
               jp z,rep26
               jp ends


;FIND A FILE IN THE DIRECTORY

fndfl:         call bitf2
               jr nz,fndf4
               call setf2
               ld ix,dchan
               call rest

fndf1:         xor a
               ld (ix+4),a
               ld (fndfr),a

fndf2:         call rsad
               ld (fndts),de

fndf3:         call clrrpt
               ld a,(fndfr)
               ld (ix+rpth),a
               call grpnt
               ld a,(hl)
               and a
               jr z,fndf4

               call cknam
               jr nz,fndf4
               scf
               ret

fndf4:         ld de,(fndts)
               ld a,(fndfr)
               cp 1
               jr z,fndf5

               inc a
               ld (fndfr),a
               jr fndf2

fndf5:         call isect
               jr nz,fndf1
               inc d
               ld a,d
               cp 4
               ret nc
               jr fndf1


;RENAME A FILE

renam:         call gtnc
               call evnam
               cp &8e         ;to
               jp nz,rep2
               call gtnc
               call evnam2
               call ceos

               ld hl,nstr1+1
               call evfile
               call ckdisc

               call exdat
               ld hl,nstr1+1
               call evfile
               call ckdisc

               call findc
               jp z,rep28
               call exdat

               call findc
               jp nz,rep26
               inc hl
               push de
               ld de,nstr2+1
               ex de,hl
               ld bc,10
               ldir
               pop de
               call wsad
               jp ends


;FIND A FILE ROUTINE

findc:         ld a,&10
               call fdhr
               jp point


;PROTECT ROUTINE

prot:          ld a,&40
               jr sfbt

;HIDE FILE ROUTINE

hide:          ld a,&80

sfbt:          ld (hstr1),a
               call gtnc
               cp &89         ;off
               jr nz,sfb1
               call setf1
               call gtnc

sfb1:          call evnam
               call ceos

               ld hl,nstr1+1
               call evfile
               call ckdisc

sfb2:          call fndfl
               jr c,sfb3
               call bitf0
               jp z,rep26
               jp ends

sfb3:          ld (ix+rptl),0
               call grpnt
               ld a,(hstr1)
               ld c,a
               cpl
               ld b,a
               ld a,(hl)
               and b
               call bitf1
               jr nz,sfb4
               or c
sfb4:          ld (hl),a
               call wsad
               call setf0
               jr sfb2



;SEPARATOR REPORT ROUTINE

separx:        call separ
               ret z
               jp rep2


;THE SEPARATOR SUBROUTINE

separ:         cp ","
               jr z,sepa1
               cp ";"
               jr z,sepa1
               cp ""
               ret

sepa1:         call gtnc
               ld (sva),a
               xor a
               ld a,(sva)
               ret



;EVALUATE PARAMETERS IN SYNTAX

evprm:         call gtnc

               cp &87         ;at
               jp nz,rep2

;GET DRIVE NUMBER

               call gtnc
               call evdnm

;GET TRACK NUMBER

               call separx
               call evnum
               jr z,evpr1

               ld d,c
               ld (hkde),de

;GET SECTOR NUMBER

evpr1:         call separx
               call evnum
               jr z,evpr2

               ld de,(hkde)
               ld e,c
               ld (hkde),de

;GET ADDRESS

evpr2:         call separx
               call evnum
               jr z,evpr3

               ld (hkhl),bc

evpr3:         call ceos

               ld a,(dstr1)
               ld (hka),a
               ret


;SAVE HEADER INFORMATION

svhd:          ld hl,hd001
               ld de,fsa+211
               ld b,9
svhd1:         ld a,(hl)
               ld (de),a
               call sbyt
               inc hl
               inc de
               djnz svhd1
               ret


;WRITE AT A TRACK AND SECTOR

write:         call gtixd
               call evprm

               call hwsad
               jp ends


;READ AT A TRACK AND SECTOR

read:          call gtixd
               call evprm

               call hrsad
               jp ends


;LOAD HEADER INFORMATION

ldhd:          ld b,9
ldhd1:         call lbyt
               djnz ldhd1
               ret


;LOAD SYNTAX COMMAND

load:          call gtixd
               call gtnc
               call evnum

               push af
               ld a,c
               ld (fstr1),a
               call gtdef
               pop af
               call ceos

               call gtfle
autox:         ld a,(difa)
               cp &14
               jr nz,dlvm1
               call bitf7
               jr z,dlvm1

;48k SNAPSHOT IS FOUND

               ld de,(svde)
               call rsad

               in a,(250)
               ld (snprt0),a
               in a,(251)
               ld (snprt1),a
               in a,(252)
               ld (snprt2),a

               ld a,%00000100
               out (251),a
               out (252),a

               ld sp,str-20
               ld hl,&8000
               ld de,16384
               ld a,2
               ld (pges1),a
               call ldblk
               jp snap7

dlvm1:         cp &10
               jr nz,dlvm2
               call bitf7
               jp nz,rep13

               call nrrdd
               defw prog
               push bc
               pop hl

               call nrrd
               defw progp
               ld (uifa+31),a
               ld (uifa+32),hl
               ex de,hl
               ld c,a

               push bc
               call nrrdd
               defw eline
               push bc
               pop hl

               call nrrd
               defw elinp

               pop bc
               dec hl
               bit 7,h
               jr nz,lab2
               dec a
lab2:          push bc
               push de
               call ahln
               push af
               ex de,hl
               ld a,c
               call ahln
               ex de,hl
               ld c,a
               pop af
               and a
               sbc hl,de
               sbc a,c
               pop de
               pop bc
               rl h
               rla
               rl h
               rla
               rr h
               scf
               rr h
               ld (uifa+34),a
               ld (uifa+35),hl
               xor a
               ld (uifa+15),a

dlvm2:         call txinf
               call txhed
               jp endsx


ahln:          rlc h
               rlc h
               rra
               rr h
               rra
               rr h
               and 7
               ret


;WRITE FORMAT ON DISC

wfod:          call gtnc
               cp &8e         ;to
               jr z,wfod1
               call ciel
               jr z,wfod2

               call evnam

               cp &8e         ;to
               jr nz,wfod2
wfod1:         ld (hstr1),a

               call gtnc
               call evnam2

wfod2:         call ceos

               ld hl,nstr1+1
               call evfile
               call ckdisc

               ld a,(hstr1)
               cp &8e
               jr nz,wfod3

               call exdat
               ld hl,nstr1+1
               call evfile
               call ckdisc
               call exdat


wfod3:         call pmo6
               call cyes
               jp nz,ends

               call dfmt
               jp ends


;CHECK VALID SPECIFIER DISC

ckdisc:        ld a,(lstr1)
               cp "D"
               ret z
               jp rep10

;EVALUATE DRIVE NUMBER

evdnm:         call evnum
               ret z

               push af
               ld a,c
               ld (dstr1),a
               pop af
               ret


;EVALUATE STREAM INFORMATION

evsrm:         call gtnc
evsrmx:        call evnum
               ret z

               push af
               ld a,c
               cp 16
               jp nc,rep9
               ld (sstr1),a
               pop af
               ret


;EVALUATE NUMBER ROUTINE

evnum:         call cmr
               defw expnum
               call cfso
               ret z

               push af
               call cmr
               defw getint
               pop af
               ret


               org &5bc8
               dump gnd.bank,&1bc8


;EVALUATE CHANNEL SPECIFIER

evsp:          call gtnc
evspx:         call evstr
               jr z,evsp1

               push af
               ld a,c
               dec a
               or b
               jp nz,rep10

               ex de,hl
               call cmr
               defw nrread
               call alpha
               jp nc,rep10

               ld (lstr1),a
               pop af

evsp1:         cp ";"
               ret z
               cp ","
               ret z
               jp rep0




;EVALUATE SECOND FILE NAME

evnam2:        call exdat
               call evnam

exdat:         push af
               push bc
               push de
               push hl

               ld b,28
               ld de,dstr1
               ld hl,dstr2
exdt1:         ld a,(de)
               ld c,(hl)
               ex de,hl
               ld (de),a
               ld (hl),c
               inc de
               inc hl
               djnz exdt1

               pop hl
               pop de
               pop bc
               pop af
               ret


;EVALUATE FILE NAME

evnam:         call evstr
               ret z

               push af
               ld a,c
               or b
               jp z,rep8

               ld hl,14
               sbc hl,bc
               jp c,rep8

               ld hl,nstr1
               ld a,15
evnm1:         ld (hl),&20
               inc hl
               dec a
               jr nz,evnm1

               ld hl,nstr1+1
               ex de,hl

               in a,(251)
               push af
               ld a,(svc)
               and &1f
               out (251),a
               ldir
               pop af
               out (251),a

               pop af
               ret


;EVALUATE STRING EXPRESSION

evstr:         call cmr
               defw expstr
               call cfso
               ret z

               push af
               call cmr
               defw getstr
               ld (svc),a
               pop af
               ret


;CHECK FOR ALPHA CHAR

alpha:         cp &41
               ccf
               ret nc
               cp &5b
               ret c
               cp &61
               ccf
               ret nc
               cp &7b
               ret


;CHECK FOR NUMBER

number:        sub &30
               cp 10
               ret nc
               jp rep11


;TRANSFER FILE NAMES IN COPY

trx:           ld hl,difa
               ld de,nstr1
               ld bc,15
               ldir

               ld hl,nstr3+1
               ld de,nstr1+1
               ld b,10

trx1:          ld a,(hl)
               cp "*"
               jr z,trx3
               cp "?"
               jr z,trx2
               ld (de),a
trx2:          inc hl
               inc de
               djnz trx1
               ret

trx3:          inc hl
               ld a,(hl)
               cp "."
               ret nz
trx4:          ld a,(de)
               cp "."
               jr z,trx2
               inc de
               djnz trx4
               ret

gdifa:         call point
               ld de,difa
               ld bc,11
               ldir
               ld b,4
               call lcnta
               ld (ix+rptl),220
               call grpnt
               ld bc,33
               ldir

               ld hl,difa
               ld de,uifa
               ld bc,48
               ldir

               ld (ix+rptl),13
               call grpnt
               ld d,(hl)
               inc hl
               ld e,(hl)
               ret

