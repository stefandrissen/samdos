
;DISC FORMAT ROUTINE

dfmt:          di
               call gtixd
               call ckdrv
               call seld

               ld b,10
dfmta:         push bc
               call instp
               pop bc
               djnz dfmta

               call restx

               call getscr

fmt1:          ld hl,ftadd
               call dmt1

               push de
               call cmr
               defw clslow
               call pmoa
               pop de
               push de
               call sctrk
               pop de

fmt3:          ld c,wtrk
               call precmp
               ld hl,ftadd
               call wrdata
               call stpdel
               inc d

               call tstd
               cp d
               jr z,fmt7
               and &7f
               cp d
               jr z,fmt6
               call instp
               dec e
               jr nz,fmt4
               ld e,10
               call sze
               jr z,fmt4
               ld e,5
fmt4:          dec e
               jr nz,fmt5
               ld e,10
               call sze
               jr z,fmt5
               ld e,5
fmt5:          jp fmt1

fmt6:          call restx
               ld d,&80
               call seld
               jp fmt1

fmt7:          call rest

               ld a,(dstr2)
               cp &ff
               jr z,fmt11a

               ld hl,(buf)
               ld (svbuf),hl

fmt8:          push de
               call cmr
               defw clslow
               call pmob
               pop de
               push de
               call sctrk
               pop de

               ld a,(dstr2)
               call ckdrx
               ld hl,ftadd

fmt9:          ld (buf),hl
               call rsad
               ld bc,512
               call sze
               jr z,fmt9a
               ld bc,1024
fmt9a:         add hl,bc
               call isect
               jr nz,fmt9

               ld a,(dstr1)
               call ckdrx
               ld hl,ftadd

fmt10:         ld (buf),hl
               call wsad
               ld bc,512
               call sze
               jr z,fmt10a
               ld bc,1024
fmt10a:        add hl,bc
               call isect
               jr nz,fmt10

               call itrck
               jr nz,fmt8

               ld hl,(svbuf)
               ld (buf),hl
               jr fmt12


fmt11:         call rsad
               call isect
               jr nz,fmt11

fmt11a:        push de
               call cmr
               defw clslow
               call pmoc
               pop de
               push de
               call sctrk
               pop de
               call itrck
               jr nz,fmt11

fmt12:         call cmr
               defw clslow
               ld hl,ftadd
               call putscr
               ei
               jp rest


;PRINT TRACK ON SCREEN

sctrk:         ld a,d
               bit 7,a
               jr z,strk1
               and &7f
               ld b,a
               call tstd
               and &7f
               add b

strk1:         ld l,a
               ld h,0
               ld a,&20
               jp pnum3


;INCREMENT TRACK

itrck:         inc d
               call tstd
               cp d
               ret z
               and &7f
               cp d
               ret nz
               call rest
               ld d,&80
               cp d
               ret


;DOUBLE DENSITY FORMAT

dmt1:          ld bc,&3c4e
               call wfm
               ld b,10
               call sze
               jr z,dmt2
               ld b,5
dmt2:          push bc
               ld bc,&0c00
               call wfm
               ld bc,&03f5
               call wfm
               ld bc,&01fe
               call wfm
               ld a,d
               and &7f
               ld c,a
               ld b,&01
               call wfm
               ld a,d
               and &80
               rlca
               ld c,a
               ld b,1
               call wfm
               ld c,e
               call isect
               ld b,&01
               call wfm
               ld bc,&0102
               call sze
               jr z,dmt3
               ld bc,&0103
dmt3:          call wfm
               ld bc,&01f7
               call wfm
               ld bc,&164e
               call wfm
               ld bc,&0c00
               call wfm
               ld bc,&03f5
               call wfm
               ld bc,&01fb
               call wfm
               ld bc,0
               call wfm
               call wfm
               call sze
               jr z,dmt4
               ld bc,0
               call wfm
               call wfm
dmt4:          ld bc,&01f7
               call wfm
               ld bc,&184e
               call wfm
               pop bc
               dec b
               jp nz,dmt2
               ld bc,&004e
               call wfm
               call wfm
               jp wfm



;WRITE FORMAT IN MEMORY

wfm:           ld (hl),c
               inc hl
               djnz wfm
               ret


;INCREMENT SECTOR ROUTINE

isect:         inc e
               call sze
               jr z,isect1
               ld a,e
               cp 6
               ret nz
               ld e,1
               ret

isect1:        ld a,e
               cp 11
               ret nz
               ld e,1
               ret


;PRINT TYPE OF FILE

pntyp:         and &1f
               push af
               ld hl,drtab
               ld bc,drtbx-drtab
               cpir

pnty1:         ld a,(hl)
               cp 32
               jr c,pnty2
               call pnt
               inc hl
               jr pnty1

pnty2:         pop af
               cp 16
               jr nz,pnty3
               ld (ix+rptl),242
               call grpnt
               ld a,(hl)
               and &c0
               jr nz,pnty5
               inc hl
               ld e,(hl)
               inc hl
               ld d,(hl)
               ex de,hl
               call pnum5
               jr pnty5

pnty3:         cp 19
               jr nz,pnty4
               ld (ix+rptl),236
               call grpnt
               call gtval
               inc c
               ex de,hl
               push de
               ld a,&20
               call pnum6
               ld a,","
               call pnt
               pop hl
               call gtval
               ex de,hl
               xor a
               call pnum6

pnty4:         cp 4
               jr nz,pnty5
               ld (ix+rptl),215
               call grpnt
               ld d,(hl)
               dec hl
               ld e,(hl)
               ex de,hl
               push de
               call pnum5
               ld a,","
               call pnt
               pop hl
               dec hl
               ld d,(hl)
               dec hl
               ld e,(hl)
               ex de,hl
               xor a
               call pnum5x

pnty5:         ld a,&0d
               jp pnt


;GET NUMBER FROM HEADER

gtval:         ld a,(hl)
               and &1f
               ld c,a
               inc hl
               ld e,(hl)
               inc hl
               ld a,(hl)
               and &7f
               ld d,a
               inc hl
               ret


drtab:         defb 1
               defm "ZX BASIC"
               defb 16
               defm "BASIC "
               defb 2
               defm "ZX D.ARRAY"
               defb 17
               defm "D.ARRAY"
               defb 3
               defm "ZX $.ARRAY"
               defb 18
               defm "$.ARRAY"
               defb 4
               defm "ZX "
               defb 19
               defm "C "
               defb 5
               defm "ZX SNP 48k"
               defb 6
               defm "MD.FILE"
               defb 7
               defm "ZX SCREEN$"
               defb 20
               defm "SCREEN$"
               defb 8
               defm "SPECIAL"
               defb 9
               defm "ZX SNP 128k"
               defb 10
               defm "OPENTYPE"
               defb 11
               defm "N/A EXECUTE"
               defb 12
drtbx:         defm "WHAT?"
               defb 0


;PRINT NUMBER IN HL

pnum6:         ld (sva),a
               xor a
               ld de,0

               rr c
               rr d
               rr c
               rr d
               ld a,d
               add h
               ld h,a
               ld a,c
               adc e
               ld b,a
               ld de,34464
               ld c,1         ;65536
               ld a,(sva)
               call pnm2
               jr pnum5y

pnum5:         ld a,&20

pnum5x:        ld b,0
pnum5y:        ld c,0
               ld de,10000
               call pnm2
pnum4:         ld de,1000
               call pnm1
pnum3:         ld de,100
               call pnm1
pnum2:         ld de,10
               call pnm1
pnum1:         ld a,l
               add &30
               jr pnt

pnm1:          ld bc,0
pnm2:          push af
               ld a,b
               ld b,0
               and a

pnm3:          sbc hl,de
               sbc a,c
               jr c,pnm4
               inc b
               jr pnm3
pnm4:          add hl,de
               adc a,c
               ld c,a
               ld a,b
               ld b,c
               and a
               jr nz,pnm5

               pop de
               add d
               ret z
               jr pnt

pnm5:          add &30
               call pnt
               pop de
               ld a,&30
               ret



;PRINT TEXT MESSAGE

ptm:           pop hl
ptm2:          ld a,(hl)
               and &7f
               call pnt
               bit 7,(hl)
               ret nz
               inc hl
               jr ptm2

;SEND A SPACE CHARACTER

spc:           ld a,&20

;OUTPUT A CHAR TO CURRENT CHAN

pnt:           push af
               push bc
               push de
               push hl
               push ix

               call cmr
               defw &0010

               pop ix
               pop hl
               pop de
               pop bc
               pop af
               ret


;SCREEN ROUTINES

pmo1:          call ptm
               defm "Tape ready ? "
               defm "press SPACE ."
               defb "."+128

pmo2:          call ptm
               defm " - DIRECTORY *"
               defw &8d0d

pmo3:          call ptm
               defw &0d0d
               defm "Number of Free "
               defm "K-Bytes ="
               defb &a0

pmo4:          call ptm
               defb &7f
               defm " Sam Computers Lt"
               defm "d.  Version  1"
               defw &8d0d

pmo5:          call ptm
               defm "OVERWRITE "
               defb "'"+128

pmo6:          call ptm
               defm "Are you SURE ?"
               defm " (y/n"
               defb ")"+128

pmo7:          call ptm
               defm "' (y/n"
               defb ")"+128

pmo8:          call ptm
               defm "  * SAM DRIVE "
               defb &a0

pmo9:          call ptm
               defb &0d
               defm "Enter source disk "
               defm "press any ke"
               defb "y"+&80

pmoa:          call ptm
               defm "ALL FORMAT  AT "
               defm "TRACK "
               defb " "+&80

pmob:          call ptm
               defm "Copy   disk at "
               defm "track "
               defb " "+&80

pmoc:          call ptm
               defm "Verify disk at "
               defm "track "
               defb " "+&80

pmod:          call ptm
               defm "Enter target disk "
               defm "press any ke"
               defb "y"+&80


tspce1:        call cmr
               defw clslow
               call pmod
tspc1:         call cmr
               defw rdkey
               jr nc,tspc1
tspc2:         call cmr
               defw rdkey
               jr c,tspc2
               call cmr
               defw clslow
               ret

tspce2:        call cmr
               defw clslow
               call pmo9
               jr tspc1

