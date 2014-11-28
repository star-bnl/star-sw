*
* $Id: rbbank.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
*
* $Name:  $
*
* ------------------------------------------------------------------
*
* rbbank.inc
*
C   - combined DETM + Reconstruction bank access variables - AGI version
      CHARACTER         CNAM*4
      INTEGER           LU,BIG,MM,INAM,II,KK,IADR,LENG,IOD,IOX,IrbDIV,
     >                  LDD,NDDMAX,NDD,DDL,NFR,FRTAB,IrMode
      PARAMETER         (LDD=5,NDDMAX=100,MM=2,BIG=99999)
      COMMON /RBXBANK/  LU, CNAM,II,KK,IADR,LENG,IOD,IOX,IrbDIV,IrMode
      COMMON /RBXLIST/  NDD,DDL(LDD,NDDMAX)
      COMMON /RBXFORM/  NFR,FRTAB(NDDMAX)
      EQUIVALENCE       (CNAM,INAM)
*
