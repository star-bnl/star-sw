*
* $Id: rbbank.inc,v 1.1 1998/04/10 15:30:48 fisyak Exp $
*
* $Log: rbbank.inc,v $
* Revision 1.1  1998/04/10 15:30:48  fisyak
* Move gstar into CVS
*
*
* ------------------------------------------------------------------
*
* rbbank.inc
*
*CMZ :          22/03/98  21.52.54  by  Pavel Nevski
*CMZ :  1.30/00 02/04/97  18.53.56  by  Pavel Nevski
*CMZ :  1.00/00 07/03/95  21.11.16  by  Pavel Nevski
*-- Author :  by Pavel Nevski
C   - combined DETM + Reconstruction bank access variables - AGI version
      CHARACTER         CNAM*4
      INTEGER           LU,BIG,MM,INAM,II,KK,IADR,LENG,IOD,IOX,IrbDIV,
     >                  LDD,NDDMAX,NDD,DDL,NFR,FRTAB,IrMode
      PARAMETER         (LDD=5,NDDMAX=100,MM=2,BIG=99999)
      COMMON /RBXBANK/  LU, CNAM,II,KK,IADR,LENG,IOD,IOX,IrbDIV
      COMMON /RBXLIST/  NDD,DDL(LDD,NDDMAX)
      COMMON /RBXFORM/  NFR,FRTAB(NDDMAX)
      COMMON /RBCBANK/  IrMode
      EQUIVALENCE       (CNAM,INAM)
*
