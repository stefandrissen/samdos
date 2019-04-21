
min:           equ 191
mto:           equ 223


;MOVE COMMAND

move:          call evmov
               cp 204
               jp nz,rep0
               call exdat
               call evmov
               call exdat
               call ceos

               call setf2

               ld a,min
               ld (fstr1),a
               call opmov
               ld hl,(chans)
               push hl
               ld a,(fstr1)
               ld (nstr2),a
               call exdat
               ld a,mto
               ld (fstr1),a
               ld ix,dchan
               call opmov
               jr nc,mova

               ld ix,(nstr2)
               call rclmx
               pop hl
               pop hl
               jp ends

mova:          call exdat
               pop de
               ld hl,(chans)
               or a
               sbc hl,de
               ld de,(nstr1)
               add hl,de
               ld (nstr1),hl

move1:         ld hl,(nstr1)
               ld (curchl),hl
move2:         call cmr
               defw &15e6
               jr c,move3
               jr z,move2
               jr move4

move3:         ld hl,(nstr2)
               ld (curchl),hl
               call cmr
               defw &15f2
               jr move1

move4:         xor a
               ld (flag3),a
               ld hl,(chans)
               push hl
               call exdat
               call clmov
               call exdat
               pop de
               ld hl,(chans)
               or a
               sbc hl,de
               ld de,(nstr1)
               add hl,de
               ld (nstr1),hl
               call clmov
               call cltemp
               jp ends


;RECLAIM A CHANNEL

rclmx:         ld c,(ix+9)
               ld b,(ix+10)
               push bc
               push ix
               pop hl
               call cmr
               defw &19e8
               pop bc
               ret

;EVALUATE A MOVE SYNTAX

evmov:         call gtnc
               cp "#"
               jp z,evsrm

evsyn:         ld (lstr1),a
               and &df
               cp "D"
               call nz,evspx
               call evnum
               call separ
               jp z,evnam
               call cfso
               ret z
               push af
               ld a,(lstr1)
               and &df
               cp "D"
               jp z,rep2
               cp "M"
               jp z,rep2
               pop af
               ret


;OPEN A MOVE CHANNEL

opmov:         ld a,(sstr1)
               inc a
               jr z,opmv1
               dec a
               call cmr
               defw &1601
               ld hl,(curchl)
               ld (nstr1),hl
               ret

opmv1:         ld a,(lstr1)
               and &df
               cp "M"
               jr z,opmv2
               cp "D"
               jr nz,opmv3
opmv2:         call ckdrv
               call opend
               ld a,(nstr1)
               ld (fstr1),a
               ld (nstr1),ix
               ret

opmv3:         cp "N"
               jp nz,rep0
               call setf3
               ret


;CLOSE A MOVE CHANNEL

clmov:         ld a,(sstr1)
               inc a
               ret nz

               ld a,(lstr1)
               and &df
               cp "N"
               jr z,clmv1
               ld ix,(nstr1)
               jp deld

clmv1:         ret


;RECLAIM TEMPORARY CHANNELS

cltemp:        ld ix,(chans)
               ld de,&0014
               add ix,de
cltm1:         ld a,(ix)
               cp &80
               ret z

               ld a,(ix+4)
               cp "D"+&80
               jr nz,cltm2
               call deld
               jr cltemp

cltm2:         call bitf1
               jr z,cltm3
               call rclmx
               jr cltemp

cltm3:         ld e,(ix+9)
               ld d,(ix+10)
               add ix,de
               jr cltm1


deld:          push ix
               pop hl
               ld de,(chans)
               or a
               sbc hl,de
               inc hl
               ld (svtrs),hl
               jp clrchd


;OPEN# COMMAND SYNTAX ROUTINE

open:          call evsrm
               call separ
               jp nz,rep0
               call evsyn
               cp &0d
               jr z,open2
               cp min
               jr z,open1
               cp mto
               jp nz,rep2
open1:         ld (fstr1),a
               call gtnc

open2:         call ceos
               ld a,(sstr1)
               call cmr
               defw &1727
               ld hl,&0011
               and a
               sbc hl,bc
               jp c,rep30
               ld a,(lstr1)
               and &df
               cp "D"
               jr z,open3
               cp "M"
               jp nz,rep0
open3:         call ckdrv
               ld a,10
               ld (nstr1),a
               call opdst
               jp ends


;OPEN A STREAM TO 'D' CHANNEL

opdst:         ld a,(sstr1)
               add a,a
               ld hl,&5c16
               ld e,a
               ld d,0
               add hl,de
               push hl
               call opend
               pop de
               ret c

               bit 0,(ix+mflg)
               jr z,opdst1
;      IN   A,(COMM)
;      BIT  6,A
               jr z,opdst1
               call rclmx
               jp rep23

opdst1:        res 7,(ix+4)
               ex de,hl
               ld (hl),e
               inc hl
               ld (hl),d
               ret


;OPEN A  'D' DISC CHANNEL

opend:         ld ix,(chans)
               ld de,&14
               add ix,de
opnd1:         ld a,(ix)
               cp &80
               jr z,opnd4

;FOUND AN OPEN CHANNEL

               ld a,(ix+4)
               and &5f
               cp "D"
               jr nz,opnd3
               ld a,(dstr1)
               cp (ix+mdrv)
               jr nz,opnd3

;CHECK NAME OF CHANNEL

               push ix
               pop hl
               ld de,name
               add hl,de
               ex de,hl
               ld hl,nstr1+1
               ld b,10
opnd2:         ld a,(de)
               xor (hl)
               and &df
               jr nz,opnd3
               inc hl
               inc de
               djnz opnd2

               jp rep31

;GET THE LENGTH OF CHANNEL

opnd3:         ld e,(ix+9)
               ld d,(ix+10)
               add ix,de
               jr opnd1

;TEST DIRECTORY FOR FILENAME

opnd4:         push ix
               ld a,&10
               call fdhr
               ld a,(fstr1)
               jp nz,opnd5

;FILE FOUND

               cp mto
               jp z,opnd6

;OPEN A READ CHANNEL

               ld bc,551
               call crmch

;SET HL TO FILE IN DRAM

               call point

;RESTORE CHANNEL ADDRESS

               pop ix
               ld a,(dstr1)
               ld (ix+mdrv),a

               ld a,0
               ld (ix+mflg),a

               ld bc,rram
               ld (ix+bufl),c
               ld (ix+bufh),b

;TRANSFER NAME OF FILE

               push hl

               push ix
               pop hl
               ld de,ffsa
               add hl,de
               ex de,hl
               pop hl
               ld bc,11
               ld a,(hl)
               ld (nstr1),a
               ldir

               inc hl
               inc hl
               ld b,(hl)
               inc hl
               ld c,(hl)
               push bc

               ld bc,196
               add hl,bc
               ld a,(hl)
               ld (ix+18),a
               inc hl

               ld bc,9
               ldir

               ld de,str-20
               ld bc,22
               ldir

               pop de
               call rsad
               jr opnd7


;FILE NOT FOUND IS IT A READ?

opnd5:         cp min
               jp z,rep26

;OPEN A WRITE FILE

opnd6:         ld bc,787
               call crmch

               pop ix
               ld a,(dstr1)
               ld (ix+mdrv),a

               ld a,1
               ld (ix+mflg),a

               ld bc,wram
               ld (ix+bufl),c
               ld (ix+bufh),b

               call ofsm
               jr z,opnd6a

               ld bc,787
               push ix
               pop hl
               call cmr
               defw &19e8
               scf
               ret

opnd6a:        call bitf2
               jr z,opnd7

               push ix
               pop hl
               ld de,230
               add hl,de
               ex de,hl
               ld hl,(nstr2)
               ld bc,rtyp
               add hl,bc
               ld bc,9
               ldir

               ld hl,str-20
               ld bc,22
               ldir

;ENTER CHANNEL DATA

opnd7:         push ix
               pop de
               ld hl,mtbls
               ld bc,11
               ldir


;CALCULATE STREAM OFFSET

               push ix
               pop hl
               ld de,(chans)
               or a
               sbc hl,de
               inc hl
               ret


;CREATE THE 'D' CHANNEL

crmch:         ld (mlen),bc
               ld hl,(prog)
               dec hl
               push hl
               push bc
               call cmr
               defw &1655
               pop bc
               pop hl

;CLEAR THE NEW CHANNEL AREA

crmc1:         ld (hl),0
               inc hl
               dec bc
               ld a,b
               or c
               jr nz,crmc1
               ret


;DISC 'D' CHANNEL DATA

mtbls:         defw &0008
               defw &0008
               defb "D"+&80
               defw mchwr
               defw mchrd
mlen:          defw 0


;CLOSE# STREAMS COMMAND

close:         call gtnc
               cp "*"
               jp nz,rep0
               call gtnc
               cp &0d
               jr z,clos1
               cp &3a
               jr z,clos1
               call evsrmx
               call ceos
               ld a,(sstr1)
               call clsrm
               jp ends

clos1:         call ceos
               jr clrs1


;CLEAR# STREAMS COMMAND

clear:         call gtnc
               cp "#"
               jp nz,rep0
               call gtnc
               call ceos
               call setf1

clrs1:         xor a
clrs2:         push af
               call clsrm
               pop af
               inc a
               cp 16
               jr c,clrs2

               call cltemp
               xor a
               ld (samcnt),a
               ld (flag3),a
               jp ends


;CLEAR STREAM AND CLOSE CHANNEL

clsrm:         call cmr
               defw &1727
               ld a,c
               or b
               ret z

;CLOSE THE STREAM

               ld (svtrs),bc
               push hl
               ld hl,(chans)
               dec hl
               add hl,bc
               ex (sp),hl
               call cmr
               defw &16eb
               pop ix
               ld a,b
               or c
               ret nz

;TEST FOR DISC 'D' CHANNEL

               ld a,(ix+4)
               and &5f
               cp "D"
               jr nz,rclaim
clrchd:        bit 0,(ix+mflg)
               jr z,rclaim
               call bitf1
               jr nz,rclaim

;AN OPEN WRITE CHANNEL

               call cfsm

;RECLAIM THE CHANNEL

rclaim:        call rclmx

;CLOSE AND UPDATE STREAM DISP

               xor a
               ld hl,&5c16
rclm1:         ld (svhl),hl
               ld e,(hl)
               inc hl
               ld d,(hl)
               ld hl,(svtrs)
               and a
               sbc hl,de
               jr nc,rclm4
               ex de,hl
               and a
               sbc hl,bc
               ex de,hl
               ld hl,(svhl)
               ld (hl),e
               inc hl
               ld (hl),d
rclm4:         ld hl,(svhl)
               inc hl
               inc hl
               inc a
               cp 16
               jr c,rclm1
               ret


;CLS ROUTINE

cls:           call gtnc
               cp "#"
               jp nz,rep0
               call gtnc
               call ceos

               ld hl,&0038
               ld (attrp),hl
               ld (attrt),hl
               ld (iy+14),l
               ld (iy+87),h
               ld a,7
               out (ula),a
               call cmr
               defw &0d6b
               jp ends


;DISC 'D' CHANNEL INPUT

mchrd:         ld ix,(curchl)
               ld hl,mchin


;MAIN INPUT ROUTINE

cinput:        res 3,(iy+2)
               push hl
               ld hl,(errsp)
               ld e,(hl)
               inc hl
               ld d,(hl)
               and a
               ld hl,&107f
               sbc hl,de
               jr nz,inkey

;INPUT# COMMAND

               pop hl
               ld sp,(errsp)
               pop de
               pop de
               ld (errsp),de
inp1:          push hl
               ld de,inp2
               push de
               jp (hl)

inp2:          jr c,inp3
               jp nz,rep27
               pop hl
               jr inp1

inp3:          cp &0d
               jr z,inp4
               call cmr
               defw &0f85
               pop hl
               jr inp1

inp4:          pop hl
;      JP   RTMP

;INKEY$ INPUT FUNCTION

inkey:         pop hl
               ld de,ink1
               push de
               jp (hl)

ink1:          ret c
               ret z
               call bitf2
               jp z,rep27
               or 1
               ret


;DISC 'D' CHANNEL READ

mchin:         bit 0,(ix+mflg)
               jp nz,rep18

;DECREMENT THE FILE COUNT

               ld a,(ix+31)
               sub 1
               ld (ix+31),a
               jr nc,mchn1

               ld a,(ix+32)
               sub 1
               ld (ix+32),a
               jr nc,mchn1

               ld a,(ix+18)
               sub 1
               ld (ix+18),a
               jr nc,mchn1

               xor a
               add &0d
               ret

mchn1:         call lbyt
               call bcr
               scf
               ret



;DISC 'D' CHANNEL OUTPUT

mchwr:         ld ix,(curchl)
               bit 0,(ix+mflg)
               jp z,rep19

               call sbyt
               call bcr

;UPDATE BLOCK SIZE COUNT

               call bitf2
               ret nz

               push ix
               ld bc,229
               add ix,bc
               inc (ix+2)
               jr nz,mchwr1
               inc (ix+3)
               jr nz,mchwr1
               inc (ix)

mchwr1:        pop ix
               ret

