* $Id: dzeindex.inc,v 1.1 2000/01/06 01:01:11 nevski Exp $
* $Log: dzeindex.inc,v $
* Revision 1.1  2000/01/06 01:01:11  nevski
* dzdoc for agi
*
      PARAMETER (MAXBNK=2000)
      CHARACTER*4 CINDEX(MAXBNK)
      COMMON/DZEIDC/CINDEX
      INTEGER IORD(MAXBNK),IPINDX(MAXBNK)
      COMMON/DZEIDI/IORD,IPINDX

