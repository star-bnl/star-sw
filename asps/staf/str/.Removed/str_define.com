$ !
$ !CSA definitions:
$ !
$ !The user must define this logical (no default), eg:
$ !DEFINE/NOLOG STARSTR DISK$RNC0:[HACKENBURG.STAR.STR]
$ !
$ !CSA Reference library definitions come from DISK$RNC0:[RHIC]LOGIN_STAR
$ !
$ !No INCLUDE files exist at the moment, but eventually they will:
$ !@STR_REF:STR_INCLUDE_DEFINE.COM
$ !
$ !
$ !BNLHEP definitions:
$ !DEFINE/NOLOG STARSTR DUA2:[HACKENBURG.STAR.STR]
$ !
$ !BNLHEP reference library definitions:
$ !
$ !DEFINE/NOLOG STR_REF DUA2:[HACKENBURG.STAR.STR]
$ !
$ !
$ !BNLA00 definitions:
$ !DEFINE/NOLOG STARSTR DUB1:[HACKENBURG.STAR.STR]
$ !
$ !BNLA00 reference library definitions:
$ !
$ !DEFINE/NOLOG STR_REF DUB1:[HACKENBURG.STAR.STR]
$ !
$ !
$ !
$ @STR_REF:STR_INCLUDE_DEFINE.COM
