# SAMDOS

Source code of [samdos2](https://www.worldofsam.org/products/samdos), the standard disk operating system for the SAM Coupé.

### Procedure
The starting point was the source code [SamDos2InCometFormatMasterv1.2.zip](http://ftp.nvg.ntnu.no/pub/sam-coupe/sources/) which contains five versions (comp1.s through comp5.s).  These five versions have been uploaded over each other with the version numbers in the files being renamed to provide source history.

comp5.s is not the final "samdos2" as was publicly distributed.

I disassembled the samdos2 binary using [dZ80](http://www.inkland.org.uk/dz80/) and merged it into the last source release. Prior to merging I standardised (lowercased) the sources making the changes and additions from samdos2 visible.

### Triva
- The final build of samdos2 contains various pieces of noise from assembling and / or using the DOS before saving it. This noise, some of which looks like assembly source, is surrounded by <noise> comments.
- The unused copyright message at label pmo4 changed from 'Miles Gordon Technology Plc  1' (e1.s) to 'Sam Computers Ltd. Version  1' (e2.s & e3.s) to 'MILES GORDON TECHNOLOGY plc  1' (final)
