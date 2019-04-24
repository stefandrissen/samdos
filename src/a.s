
;(C) SAM Computers Limited

;     COPYRIGHT 1990

;     SAMDOS Ver 1.2

;     BRUCE B GORDON

;     START 29.11.89

;     DATE  24.01.90


;     DATE  10.07.91

gnd:           equ &4000
gnd.bank:      equ 1

var2:          equ &5a00
chadd:         equ var2+&97
cstat:         equ var2+&7b
nrread:        equ &00ac
nrrite:        equ &000d
beepr:         equ &016f
flags:         equ &5c3b
bordcr:        equ &5c4b
cmr:           equ &0103
xptr:          equ var2+&a3
curcmd:        equ var2+&174
stream:        equ &0112
devl:          equ var2+&06
devn:          equ var2+&07
pomsg:         equ &0115
expnum:        equ &0118
expstr:        equ &011b
expexp:        equ &011e
getint:        equ &0121
getstr:        equ &0124
stkstr:        equ &0127
progp:         equ var2+&9f
prog:          equ progp+1
elinp:         equ var2+&93
eline:         equ elinp+1
dosflg:        equ var2+&1c2

lodtok:        equ &95
newtok:        equ &af
savtok:        equ &94
dirtok:        equ &90
ovrtok:        equ &a6
cmdv:          equ &5af4
overf:         equ &5bb9
comad:         equ &5bda
insbf:         equ &4f00

lastk:         equ &5c08
defadd:        equ &5c54
tvdata:        equ var2+&01c0
errnr:         equ &5c3a
errsp:         equ &5c3d
newppc:        equ &5c42
nsppc:         equ &5c44
chans:         equ &5c4f
curchl:        equ &5c51

vars:          equ 23627
datadd:        equ 23639
worksp:        equ 23649
stkbot:        equ 23651
stkend:        equ 23653
flagx:         equ 23665
tadl:          equ 23668
attrp:         equ 23693
attrt:         equ 23695
membot:        equ 23698
trap:          equ 23728
dstr1m:        equ 23766
nstr1m:        equ 23770
nstr2m:        equ 23778

chbtlo:        equ 11
chbthi:        equ 12
chrec:         equ 13
chname:        equ 14
chflag:        equ 24
chdriv:        equ 25
recflg:        equ 67
recnum:        equ 68
rclnlo:        equ 69
rclnhi:        equ 70


dres:          equ %00001001
seek:          equ %00011011
stpin:         equ %01011011
stpout:        equ %01111011

drsec:         equ %10000000
dwsec:         equ %10100010
radd:          equ %11000000
rtrk:          equ %11100000
wtrk:          equ %11110000

resp:          equ 233
pport:         equ 232
ula:           equ 254

mdrv:          equ 11
mflg:          equ 12
rptl:          equ 13
rpth:          equ 14
bufl:          equ 15
bufh:          equ 16
nsrl:          equ 17
nsrh:          equ 18
ffsa:          equ 19
name:          equ 20
rtyp:          equ 30
cnth:          equ 30
cntl:          equ 31
ftrk:          equ 32
fsct:          equ 33
fsam:          equ 34
rram:          equ 39
wtyp:          equ 230
wram:          equ 275

str:           equ gnd+&3efe

sam:           equ gnd+&370f

dchan:         equ sam+241
svbc:          equ dchan
svde:          equ dchan+2
rfdh:          equ dchan+4
svhl:          equ dchan+5
svix:          equ dchan+7
reg1:          equ dchan+9
drive:         equ dchan+11
flag3:         equ dchan+12
rpt:           equ dchan+13
buf:           equ dchan+15
nsr:           equ dchan+17
fsa:           equ dchan+19

dram:          equ fsa+256

fndfr:         equ dram+1024
fndts:         equ fndfr+1
next:          equ fndts+2

