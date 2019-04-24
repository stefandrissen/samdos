
ftadd:         equ &a280
rdkey:         equ &0169
clslow:        equ &0151


commp:         push af
               ld a,(dsc)
               ld c,a
               pop af
               ret

trckp:         call commp
               inc c
               ret

commr:         push bc
               call commp
               in a,(c)
               pop bc
               ret

commt:         push bc
               call commp
               out (c),a
               pop bc
               ret

ckde:          ld a,d
               or e
               ret nz

               ld a,(pges1)
               and a
               ret z

               dec a
               ld (pges1),a
               ld de,16384
               jr ckde


;SAVE INTERRUPT STATUS

svint:         push af
               ld a,i
               jp pe,svin1
               ld a,i
svin1:         push af
               di
               ex (sp),hl
               ld (hldi),hl
               pop hl
               pop af
               ret


;LOAD INTERRUPT STATUS

ldint:         push af
               push hl
               ld hl,(hldi)
               ex (sp),hl
               pop af
               jp po,ldin1
               ei
ldin1:         pop af
               ret


;PRECOMPENSATION CALCULATOR

precmx:        ld c,dwsec

precmp:        call tstd
               rra
               and &3f
               ld b,a
               ld a,d
               and &7f
               sub b
               jp c,sadc
               res 1,c
               jp sadc

;WRITE SECTOR AT DE

wsad:          xor a
               ld (dct),a
wsa1:          call ctas
               call svint
               call commp
               push bc
               call gtbuf
               call precmx
               pop bc
               call wsa3
               call cdec
               jr wsa1

wsa2:          inc c
               inc c
               inc c

               outi

               dec c
               dec c
               dec c

wsa3:          in a,(c)
               bit 1,a
               jr nz,wsa2

               in a,(c)
               bit 1,a
               jr nz,wsa2

               in a,(c)
               bit 1,a
               jr nz,wsa2

               in a,(c)
               bit 1,a
               jr nz,wsa2

               in a,(c)
               bit 1,a
               jr nz,wsa2

               in a,(c)
               bit 1,a
               jr nz,wsa2

               bit 0,a
               jr nz,wsa3

               call ldint
               bit 6,a
               ret z
               jp rep23


;READ SECTOR AT DE

rsad:          xor a
               ld (dct),a
rsa1:          call ctas
               ld c,drsec
               call sadc
               call gtbuf
               call rddata
               call cdec
               jr rsa1


rddata:        call svint
               call commp
               jr rsa3

rsa2:          inc c
               inc c
               inc c

               ini

               dec c
               dec c
               dec c

rsa3:          in a,(c)
               bit 1,a
               jr nz,rsa2

               in a,(c)
               bit 1,a
               jr nz,rsa2

               in a,(c)
               bit 1,a
               jr nz,rsa2

               in a,(c)
               bit 1,a
               jr nz,rsa2

               in a,(c)
               bit 1,a
               jr nz,rsa2

               in a,(c)
               bit 1,a
               jr nz,rsa2

               bit 0,a
               jr nz,rsa3

               jp ldint

; called by FILE DIR HAND/ROUT instead of rsad

rsadx:         xor a
               ld (dct),a
rsadx1:        call ctas            ; confirm track and seek
               ld c,drsec
               call sadc            ; send a disk command
               call gtbuf           ; -> hl
               exx
               ld l,255
               ld d,&77
               exx
               push de
               call rsadx2
               call ldint           ; load interrupt status
               pop de
               rlca
               call cdec            ; check disc err count
               jr rsadx1

rsadx2:        call svint           ; save interrupt status
               call commp           ; put disk command in c
               ld a,c
               ld (comm.port.1+1),a
               ld (comm.port.2+1),a
               add a,3
               ld (dtrq.port+1),a
               ld b,2
               jr comm.port.2

rsadx3:        ld h,d
               and a
               jr nz,rsadx4

               ld h,a
rsadx4:        exx
comm.port.1:   in a,(comm)
               and b
               jr z,comm.port.2

dtrq.port:     in a,(dtrq)
               ld (hl),a
               inc hl
               exx
               inc l
               jr z,rsadx3

               or (hl)
               ld (hl),a
               exx

comm.port.2:   in a,(comm)
               rrca
               ret nc
               rra
               jp nc,comm.port.2

               jp dtrq.port


;CHECK DISC ERR COUNT

cdec:          and &1c
               jr nz,cde1
               call clrrpt
               pop hl
               jp gtbuf

cde1:          push af
               ld a,(dct)
               inc a
               ld (dct),a
               cp 10
               jp nc,rep4

               pop af
               bit 4,a
               jr nz,ctsl

               call instp
               call outstp
               call outstp
               jp instp


;CONFIRM TRACK/SECTOR LOCATION

ctsl:          ld c,radd
               call sadc
               ld hl,dst
               call rddata
               and &1c
               jr nz,cts1
               call trckp
               ld a,(dst)
               out (c),a
               ret

cts1:          ld a,(dct)
               inc a
               ld (dct),a
               cp 8
               jp nc,rep5

               and 2
               jr z,cts2

               push de
               call restx
               pop de
               jr ctsl

cts2:          call instp
               jr ctsl


;CONFIRM TRACK AND SEEK

ctas:          ld a,d
               or e
               jr nz,cta1
               call bitf2
               jp z,rep27

               ld sp,(entsp)
               xor a
               ld e,a
               ret

cta1:          call seld
               ld a,(dsc)
               inc a
               inc a
               ld c,a
               out (c),e
               call bcc

cta2:          ld a,d
               and &7f
               ld b,a
               call busy
               call trckp
               in a,(c)
               cp b
               ret z


;CHECK IF PAGE OVER C000h

               push af

               call bitf6
               jr z,cta3

               ld hl,(svhl)
               ld a,h
               cp &c0
               jr c,cta3
               res 6,h
               ld (svhl),hl
               in a,(251)
               push af
               and %11100000
               ld b,a
               pop af
               inc a
               and %00011111
               or b
               ld (port1),a
               out (251),a

cta3:          pop af
               call nc,outstp
               call c,instp
               jr cta2


;STEP DELAY ROUTINE

stpdel:        push hl
               ld hl,stprat
               ld a,(dsc)
               bit 4,a
               jr z,stpd1
               inc hl
stpd1:         ld a,(hl)
               pop hl
               and a

stpd2:         ret z

stpd3:         push af
               ld bc,150
stpd4:         dec bc
               ld a,b
               or c
               jr nz,stpd4
               pop af
               dec a
               jr stpd2


;RESTORE DISC DRIVE

restx:         nop

rest:          ld de,&0001
               call seld

;RESET DISC CHIP

               ld c,&d0
               call sdcx
               ld b,0
rslp1:         djnz rslp1

;TEST FOR INDEX HOLE

               ld hl,0
rslp2:         call commr
               bit 1,a
               call nz,rslpx
               jr nz,rslp2

rslp3:         call commr
               cpl
               bit 1,a
               call nz,rslpx
               jr nz,rslp3

;TEST FOR TRACK 00

rslp4:         call commr
               bit 2,a
               jr nz,busy

;STEP OUT ONE TRACK

               call outstp
               jr rslp4

;TEST FOR CHIP BUSY

busy:          call commr
               bit 0,a
               ret z
               call brktst
               jr busy

rslpx:         dec hl
               ld a,h
               or l
               ret nz

               jp rep6


;SEND A DISC COMMAND

sadc:          call busy
sdcx:          ld a,c
               call commt
               ld b,20
sdc1:          djnz sdc1
               ret


;CHECK DRIVE NUMBER

ckdrv:         ld a,(dstr1)

ckdrx:         cp 1
               jr z,ckdv1
               cp 2
               jp nz,rep22
               ld a,(rbcc+2)
               cp 0
               jp z,rep22
               ld a,2
ckdv1:         ld (drive),a
               ret


;SELECT DISC AND SIDE

seld:          ld a,(drive)
               cp 2
               ld b,%11100000
               jr nz,sel1
               ld b,%11110000

sel1:          ld a,d
               and &80
               jr z,sel2
               ld a,%00000100
sel2:          or b
               ld (dsc),a
               ret


;CONVERT DE INTO NUMBER

conm:          push de
               pop bc
               xor a
               dec b
               jp m,con2
con1:          add 10
               dec b
               jp p,con1
con2:          ld b,a
               sla b
               sla c
               dec c
               ld a,(ix+rpth)
               add c
               add b
               ret


;TEST FOR BUFFER FULL

tfbf:          call grpnt
               ld a,c
               cp 254
               ret nz

tfbf1:         ld a,b
               cp 1
               ret

;SAVE A BYTE ON DISC

sbyt:          push bc
               push de
               push hl
               push af
               call tfbf
               jr nz,sbt1
sbt2:          call fnfs
               ld (hl),d
               inc hl
               ld (hl),e
               ex de,hl
               call swpnsr
               call wsad
sbt1:          pop af
               ld (hl),a
               pop hl
               pop de
               pop bc
               jp incrpt



;LOAD BYTE FROM DISC

lbyt:          push bc
               push de
               push hl
               call tfbf
               jr nz,lbt1
               ld d,(hl)
               inc hl
               ld e,(hl)
               call rsad
lbt1:          ld a,(hl)
               pop hl
               pop de
               pop bc
               jp incrpt


;LOAD BLOCK DATA from disc

ldblk:         call setf6
               jp lblok


ldb1:          ld a,(hl)
               call incrpt
               ld hl,(svhl)
               ld (hl),a
               inc hl
               dec de
lblok:         ld (svhl),hl
               call ckde
               ret z

ldb2:          call tfbf
               jr nz,ldb1

               ld (svde),de
               ld d,(hl)
               inc hl
               ld e,(hl)
               call svint

ldb3:          call ccnt
               jp c,ldb8

               inc hl
               ld (svde),hl
               xor a
               ld (dct),a
               call svnsr
ldb4:          call ctas
               ld c,drsec
               call sadc
               exx
               call commp
               ld de,2
               call gtbuf
               exx
               call commp
               ld de,510
ldb4a:         ld hl,(svhl)
               jr ldb6

ldb5:          inc c
               inc c
               inc c
               ini
               dec c
               dec c
               dec c

               dec de
               ld a,d
               or e
               jr nz,ldb6
               exx

ldb6:          in a,(c)
               bit 1,a
               jr nz,ldb5

               in a,(c)
               bit 1,a
               jr nz,ldb5

               in a,(c)
               bit 1,a
               jr nz,ldb5

               in a,(c)
               bit 1,a
               jr nz,ldb5

               in a,(c)
               bit 1,a
               jr nz,ldb5

               in a,(c)
               bit 1,a
               jr nz,ldb5

               bit 0,a
               jr nz,ldb6

               and &1c
               jr z,ldb7

               call gtnsr
               call cde1
               jr ldb4

ldb7:          ld (svhl),hl
               call gtbuf
               ld d,(hl)
               inc hl
               ld e,(hl)
               jp ldb3

ldb8:          call ldint
               call rsad
               ld de,(svde)
               jp ldb2


;CALCULATE COUNT

ccnt:          ld hl,(svde)
               ld bc,510
ccnta:         scf
               sbc hl,bc
               ret nc
               ld a,(pges1)
               and a
               jr nz,ccnt1
               scf
               ret

ccnt1:         dec a
               ld (pges1),a
               ld hl,(svde)
               ld bc,16384
               add hl,bc
               ld (svde),hl
               jr ccnt


;GET SCREEN MEMORY AND POINTER

getscr:        in a,(251)
               ld (port1),a
               ld a,(port2)
               out (251),a
               ld hl,(ptrscr)
               ret

;PUT SCREEN MEMORY AND POINTER

putscr:        ld (ptrscr),hl
               ld a,(port1)
               out (251),a
               ret


;SAVE DATA BLOCK ON DISC

svblk:         call setf6
               jp sblok

svb1:          ld (hl),d
               call incrpt
               ld hl,(svhl)
               inc hl
               pop de
               dec de

;TEST FOR ZERO BLOCK COUNT

sblok:         call ckde
               ret z

;SAVE CHAR IN REGISTER D

               push de
               ld d,(hl)
               ld (svhl),hl

;TEST FOR BUFFER FULL

               call tfbf
               jr nz,svb1

;SAVE BUFFER TO DISC

               pop de
               ld (svde),de
               call fnfs
               ld (hl),d
               inc hl
               ld (hl),e
               ex de,hl
               call swpnsr
               call wsad

               call svint
               call ccnt
               jp c,svb8

               call getscr
               ld hl,ftadd
               ld bc,0
               jr svb2a


svb2:          push hl
               call ccnt
               push hl
               pop de
               pop hl
               jr c,svb3
               inc de
               ld (svde),de
               call fnfs
               ld (hl),d
               inc hl
               ld (hl),e
               inc hl
               ld bc,(svcnt)
               inc bc
svb2a:         ld (svcnt),bc
               jr svb2

svb3:          ld hl,ftadd
               call putscr

svb3a:         xor a
               ld (dct),a
               call gtnsr

svb4:          call ctas
               call precmx
               exx
               ld hl,(ptrscr)
               ld de,2
               call commp
               exx
               ld hl,(svhl)
               ld de,510
svb4a:         call commp
               jr svb6

svb5:          inc c
               inc c
               inc c

               outi

               dec c
               dec c
               dec c

               dec de
               ld a,d
               or e
               jr nz,svb6

               exx
               ld a,(port2)
               out (251),a

svb6:          in a,(c)
               bit 1,a
               jr nz,svb5

               in a,(c)
               bit 1,a
               jr nz,svb5

               in a,(c)
               bit 1,a
               jr nz,svb5

               in a,(c)
               bit 1,a
               jr nz,svb5

               in a,(c)
               bit 1,a
               jr nz,svb5

               in a,(c)
               bit 1,a
               jr nz,svb5

               bit 0,a
               jr nz,svb6

               push af
               ld a,(port1)
               out (251),a
               pop af

               and &1c
               jr z,svb7

               call gtnsr
               call cde1
               jr svb4

svb7:          ld (svhl),hl

               exx
               ld a,(port2)
               out (251),a
               ld (ptrscr),hl
               dec hl
               ld e,(hl)
               dec hl
               ld d,(hl)
               ld a,(port1)
               out (251),a
               call svnsr
               exx

               ld bc,(svcnt)
               dec bc
               ld (svcnt),bc
               ld a,b
               or c
               jp nz,svb3a

svb8:          call ldint
               call clrrpt
               ld de,(svde)
               ld hl,(svhl)
               jp svblk


;FIND NEXT FREE SECTOR

fnfs:          push hl
               push bc
               ld hl,sam
               ld de,&0401
               ld c,0

fns1:          ld a,(hl)
               cp &ff
               jr nz,fns3
               ld a,e
               add 8
               ld e,a
fns1a:         sub 10
fns1b:         jr c,fns2
               jr z,fns2
               ld e,a
               call fns5
fns2:          inc c
               inc hl
               jr fns1

fns3:          ld b,1
fns4:          ld a,(hl)
               and b
               jr z,fns6
               call isect
               call z,fns5
               rlc b
               jr fns4

fns5:          inc d
               call tstd
               cp d
               call z,decsam
               jp z,rep24
               and &7f
               cp d
               ret nz
               ld d,&80
               ret

fns6:          ld a,(hl)
               or b
               ld (hl),a
               ld a,b
               ld b,0
               push ix
               add ix,bc
               or (ix+fsam)
               ld (ix+fsam),a
               pop ix
               inc (ix+cntl)
               jr nz,fns7
               inc (ix+cnth)
fns7:          pop bc
               pop hl
               ret


;TEST TRACKS ON DISC

tstd:          push hl
               ld hl,traks1
               ld a,(dsc)
               bit 4,a
               jr z,tsd1
               inc hl
tsd1:          ld a,(hl)
               pop hl
               ret


;PRINT FILE NAME

pfnme:         ld (ix+rptl),1
               call grpnt
               ld b,10
pfnm1:         ld a,(hl)
               cp &20
               jr nz,pfnm2
               ld a,(chdir)
pfnm2:         call pnt
               inc hl
               djnz pfnm1
               ret


;FILE DIR HAND/ROUT.

fdhr:          ld ix,dchan
               ld (ix+4),a
               xor a
               ld (svdpt),a
               call rest
fdh1:          call rsadx
fdh2:          call point
               ld a,(hl)
               and a
               jp z,fdhf

;TEST FOR 'P' NUMBER

               bit 0,(ix+4)
               jr z,fdh3
               call conm
               ld b,a
               ld a,(fstr1)
               cp b
               ret z
               jp fdhd

;GET TRACK AND SECTOR COUNT

fdh3:          bit 1,(ix+4)
               jr nz,fdh4
               bit 2,(ix+4)
               jp z,fdh9
fdh4:          ld (ix+rptl),11
               call grpnt
               ld b,(hl)
               inc hl
               ld c,(hl)
               ld (svbc),bc
               ld hl,(cnt)
               add hl,bc
               ld (cnt),hl

;TEST IF WE SHOULD PRINT NAME

               bit 7,a
               jp nz,fdhd

               call cknam
               jp nz,fdhd
               bit 1,(ix+4)
               jr nz,fdh5

               call point
               ld a,(hl)
               bit 6,a
               jr z,fdh4a
               call spc
               ld a,"*"
               call pnt
               call spc
               jr fdh5

fdh4a:         call conm
               push de
               ld h,0
               ld l,a
               ld a,&20
               call pnum2
               pop de
               call spc

;PRINT FILE NAME

fdh5:          call pfnme

;FORMAT FOR ! PRINTOUT

               bit 1,(ix+4)
               jr z,fdh7
               ld b,3
               in a,(252)
               and %01100000
               cp %01000000
               jr nz,fdh6a
               sla b
               dec b
fdh6a:         ld a,(svdpt)
               inc a
               cp b
               jr z,fdh6b
               ld (svdpt),a
               ld a,32
               jr fdh6c

fdh6b:         xor a
               ld (svdpt),a
               ld a,13
fdh6c:         call pnt
               jr fdhd

;PRINT SECTOR COUNT

fdh7:          push de
               ld hl,(svbc)
               ld a,&20
               call pnum3
               call spc

;PRINT TYPE OF FILE

               call point
               ld a,(hl)
               call pntyp
               pop de
               jr fdhd

;TEST FOR SPECIFIC FILE NAME

fdh9:          bit 3,(ix+4)
               jr nz,fdha

;TEST FOR FILE NAME ONLY

               bit 4,(ix+4)
               jr z,fdhb

fdha:          call cknam
               ret z

;LOAD SAM FROM FILES USED

fdhb:          nop

;CALCULATE NEXT DIRECTORY ENTRY

fdhd:          ld a,(ix+rpth)
fdhd1:         cp 1
fdhd2:         jr z,fdhe
               call clrrpt
               inc (ix+rpth)
               jp fdh2

fdhe:          call isect
               jp nz,fdh1
               inc d
               ld a,d
               cp 4
               jp nz,fdh1
               and a
               ret

;TEST FOR FREE DIRECTORY SPACE

fdhf:          ld a,(ix+4)
               cpl
               bit 6,a
               ret z

               inc hl
               ld a,(hl)
               and a
               jr nz,fdhd
               inc a
               ret


;CHECK FILE NAME IN DIR

cknam:         push ix
               call point
               ld b,11
               bit 3,(ix+4)
               ld ix,nstr1
               jr z,cknm2

cknm1:         ld a,(ix)
               cp "*"
               jr z,cknm5
               cp "?"
               jr z,cknm2
               xor (hl)
               and &df
               jr nz,cknm4
cknm2:         inc ix
               inc hl
               djnz cknm1
cknm3:         xor a
cknm4:         pop ix
               ret

cknm5:         inc ix
               inc hl
               ld a,(ix)
               cp "."
               jr nz,cknm3
cknm6:         ld a,(hl)
               cp "."
               jr z,cknm2
               inc hl
               djnz cknm6
               jr cknm4


;OPEN FILE SECTOR ADDRESS MAP

ofsm:          push ix
               ld hl,sam
               ld b,195
ofm1:          ld (hl),0
               inc hl
               djnz ofm1
               ld a,&30

ofm2:          call fdhr
               jr nz,ofm4

;FILE NAME ALREADY USED

               push de
               call nrrd
               defw overf
               and a
               jr z,ofm3

               push ix
               call cmr
               defw clslow
               call pmo5
               pop ix

               push ix
               call pfnme
               call pmo7
               call cyes
               pop ix
               jr z,ofm3

               pop de
               pop ix
               scf
               ret

ofm3:          pop de
               call point
               ld (hl),0
               call wsad
               pop ix
               jr ofsm

;NOW CLEAR FILE SECTOR AREA

ofm4:          pop ix

               push ix
               ld b,0
ofm5:          ld (ix+ffsa),0
               inc ix
               djnz ofm5
               pop ix

               push ix
               ld hl,nstr1
               ld b,11
               call ofm6
               pop ix

               push ix
               ld bc,220
               add ix,bc
               ld hl,uifa+15
               ld b,48-15
               call ofm6
               pop ix

               call fnfs
               call svnsr
               ld (ix+ftrk),d
               ld (ix+fsct),e
               call clrrpt
               xor a
               ret

ofm6:          ld a,(hl)
               ld (ix+ffsa),a
               inc hl
               inc ix
               djnz ofm6
               ret

decsam:        ret


;COMPARE FOR Y or N

cyes:          call beep
cyes1:         call cmr
               defw rdkey
               jr nc,cyes1
               and &df
               cp "Y"
               push af
cyes2:         call cmr
               defw rdkey
               jr c,cyes2
               call cmr
               defw clslow
               pop af
               ret

beep:          push hl
               push de
               push bc
               push ix
               ld hl,&036a
               ld de,&0085
               call cmr
               defw beepr
               pop ix
               pop bc
               pop de
               pop hl
               ret


;CLOSE FILE SECTOR MAP

cfsm:          call grpnt
               ld a,c
               and a
               jr nz,cfm1
               ld a,b
cfsm1:         cp 2
cfsm2:         jr z,cfm2
cfm1:          ld (hl),0
               call incrpt
               jr cfsm

cfm2:          call gtnsr
               call wsad
               call decsam
               push ix
               ld a,&40
               call fdhr

               jp nz,rep25

;UPDATE DIRECTORY

               call point
               ld (svix),ix
               pop ix
               push ix

               ld b,0
cfm3:          ld a,(ix+ffsa)
               ld (hl),a
               inc ix
               inc hl
               djnz cfm3

               ld ix,(svix)
               call wsad
               pop ix
               ret


;GET A FILE FROM DISC

gtfle:         ld a,(fstr1)

;TEST FOR FILE NUMBER

               cp &ff
               jr z,gtfl3

               ld a,1
               call fdhr
               jp nz,rep26

gtflx:         call point
               ld a,(hl)
               and &1f
               cp &10
               jr nc,gtfl1
               dec a
               or &10
               call setf7

gtfl1:         ld (hl),a
               ld de,nstr1
               ld bc,11
               ldir
               ld b,4
               ld a,0
               call lcntb

               ld (ix+rptl),211
               call grpnt
               ld bc,9
               ldir

               call point
               ld de,uifa
               ld bc,11
               ldir

               ld b,15
               call lcnta

               ld b,48-26
               ld a,&ff
               call lcntb

               jr gtfl4

gtfl3:         ld a,&10
               call fdhr
               jp nz,rep26

gtfl4:         call point
               ld a,(hl)
               and &1f
               cp &10
               jr nc,gtfl5
               dec a
               or &10
               call setf7

gtfl5:         ld (hl),a
               ld a,(nstr1)
               cp &13
               jr nz,gtfl5a
               ld a,(hl)
               cp &14
               jr nz,gtfl5c
               dec a
               jr gtfl5b

gtfl5a:        cp &14
               jr nz,gtfl5c
               ld a,(hl)
               cp &13
               jr nz,gtfl5c
               inc a
gtfl5b:        ld (hl),a

gtfl5c:        ld a,(nstr1)
               cp (hl)
               jp nz,rep13

               ld de,hd002
               ld (ix+rptl),211
               call grpnt
               ld bc,9
               ldir

               ld de,str-20
               ld bc,22
               ldir

               call point
               ld de,difa
               ld bc,11
               ldir
               ld b,4
               call lcnta

               call bitf7
               jr z,gtfl7

               ld b,11
               call lcnta

               ld b,48-26
               ld a,&ff
               call lcntb

               ld hl,(hd0b2)
               call conp
               ld (difa+34),a
               ld (pges2),a
               ld (difa+35),hl
               ld (hd0b2),hl

               ld hl,(hd0d2)
               call conp
               dec a
               and &1f
               ld (difa+31),a
               ld (page2),a
               set 7,h
               ld (difa+32),hl
               ld (hd0d2),hl

               jr gtfl8

gtfl7:         ld (ix+rptl),220
               call grpnt
               ld bc,48-15
               ldir

gtfl8:         ld (ix+rptl),13
               call grpnt
               ld d,(hl)
               inc hl
               ld e,(hl)
               ld (svde),de
               ret

;CONVERT TO REAL PAGE AND MOD

conp:          xor a
               rl h
               rla
               rl h
               rla
               rr h
               rr h
               ret


;LOAD DE WITH COUNT B

lcnta:         ld a,&20

lcntb:         ld (de),a
               inc de
               djnz lcntb
               ret


;GET BUFFER ADDRESS

gtixd:         ld ix,dchan
               ld hl,dram
               ld (buf),hl

;CLEAR RAM POINTER

clrrpt:        ld (ix+rptl),0
               ld (ix+rpth),0
               ret


gtbuf:         ld l,(ix+bufl)
               ld h,(ix+bufh)
               ret


;GET RAM AND POINTER TO BUFFER

point:         ld (ix+rptl),0

grpnt:         call gtbuf
               ld b,(ix+rpth)
               ld c,(ix+rptl)
               add hl,bc
               ret


;INCREMENT RAM POINTER

incrpt:        inc (ix+rptl)
               ret nz
               inc (ix+rpth)
               ret


;GET THE NEXT TRACK/SECTOR

gtnsr:         ld d,(ix+nsrh)
               ld e,(ix+nsrl)
               ret

;SAVE THE NEXT TRACK/SECTOR

svnsr:         ld (ix+nsrh),d
               ld (ix+nsrl),e
               ret

;SWAP THE NEXT TRACK/SECTOR

swpnsr:        call gtnsr
               ld (ix+nsrh),h
               ld (ix+nsrl),l
               ret

outstp:        ld c,stpout
               jr step

instp:         ld c,stpin

step:          call sadc
               jp stpdel

