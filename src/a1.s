
;(C) SAM Computers Limited

;     COPYRIGHT 1990

;     SAMDOS Ver 1.2

;     BRUCE B GORDON

;     START 29.11.89

;     DATE  24.01.90


;     DATE   4.07.91


gnd:           EQU  &4000
gnd.bank:      EQU  1

var2:          EQU  &5A00
chadd:         EQU  var2+&97
cstat:         EQU  var2+&7B
nrread:        EQU  &00AC
nrrite:        EQU  &000D
beepr:         EQU  &016F
flags:         EQU  &5C3B
bordcr:        EQU  &5C4B
cmr:           EQU  &0103
xptr:          EQU  var2+&A3
curcmd:        EQU  var2+&174
stream:        EQU  &0112
devl:          EQU  var2+&06
devn:          EQU  var2+&07
pomsg:         EQU  &0115
expnum:        EQU  &0118
expstr:        EQU  &011B
expexp:        EQU  &011E
getint:        EQU  &0121
getstr:        EQU  &0124
stkstr:        EQU  &0127
progp:         EQU  var2+&9F
prog:          EQU  progp+1
elinp:         EQU  var2+&93
eline:         EQU  elinp+1
dosflg:        EQU  var2+&1C2

lodtok:        EQU  &95
newtok:        EQU  &AF
savtok:        EQU  &94
dirtok:        EQU  &90
ovrtok:        EQU  &A6
cmdv:          EQU  &5AF4
overf:         EQU  &5BB9
comad:         EQU  &5BDA
insbf:         EQU  &4F00

lastk:         EQU  &5C08
defadd:        EQU  &5C54
tvdata:        EQU  var2+&01C0
errnr:         EQU  &5C3A
errsp:         EQU  &5C3D
newppc:        EQU  &5C42
nsppc:         EQU  &5C44
chans:         EQU  &5C4F
curchl:        EQU  &5C51

vars:          EQU  23627
datadd:        EQU  23639
worksp:        EQU  23649
stkbot:        EQU  23651
stkend:        EQU  23653
flagx:         EQU  23665
tadl:          EQU  23668
attrp:         EQU  23693
attrt:         EQU  23695
membot:        EQU  23698
trap:          EQU  23728
dstr1m:        EQU  23766
nstr1m:        EQU  23770
nstr2m:        EQU  23778

chbtlo:        EQU  11
chbthi:        EQU  12
chrec:         EQU  13
chname:        EQU  14
chflag:        EQU  24
chdriv:        EQU  25
recflg:        EQU  67
recnum:        EQU  68
rclnlo:        EQU  69
rclnhi:        EQU  70


dres:          EQU  %00001001
seek:          EQU  %00011011
stpin:         EQU  %01011011
stpout:        EQU  %01111011

drsec:         EQU  %10000000
dwsec:         EQU  %10100010
radd:          EQU  %11000000
rtrk:          EQU  %11100000
wtrk:          EQU  %11110000

resp:          EQU  233
pport:         EQU  232
ula:           EQU  254

mdrv:          EQU  11
mflg:          EQU  12
rptl:          EQU  13
rpth:          EQU  14
bufl:          EQU  15
bufh:          EQU  16
nsrl:          EQU  17
nsrh:          EQU  18
ffsa:          EQU  19
name:          EQU  20
rtyp:          EQU  30
cnth:          EQU  30
cntl:          EQU  31
ftrk:          EQU  32
fsct:          EQU  33
fsam:          EQU  34
rram:          EQU  39
wtyp:          EQU  230
wram:          EQU  275

str:           EQU  gnd+&3EFE

sam:           EQU  gnd+&3A00

dchan:         EQU  sam+195
svbc:          EQU  dchan
svde:          EQU  dchan+2
rfdh:          EQU  dchan+4
svhl:          EQU  dchan+5
svix:          EQU  dchan+7
reg1:          EQU  dchan+9
drive:         EQU  dchan+11
flag3:         EQU  dchan+12
rpt:           EQU  dchan+13
buf:           EQU  dchan+15
nsr:           EQU  dchan+17
fsa:           EQU  dchan+19

dram:          EQU  fsa+256

fndfr:         EQU  dram+512
fndts:         EQU  fndfr+1
next:          EQU  fndts+2
