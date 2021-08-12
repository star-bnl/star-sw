* $Id: curpar.inc,v 1.1.1.1 2004/01/12 23:49:40 potekhin Exp $
* $Log: curpar.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
* Revision 1.1  2001/02/27 10:15:32  nevski
*  first working release
*
      PARAMETER (MXGKCU=MXGKGH)
      COMMON/CURPAR/WEIGHT(10),DDELTN,IFILE,IRUN,NEVT,NEVENT,SHFLAG,
     *              ITHST,ITTOT,ITLST,IFRND,TOFCUT,CMOM(5),CENG(5),
     *              RS,S,ENP(10),NP,NM,NN,NR,NO,NZ,IPA(MXGKCU),
     *              ATNO2,ZNO2
C
