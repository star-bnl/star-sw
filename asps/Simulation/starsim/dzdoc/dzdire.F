* $Id: dzdire.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: dzdire.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:23  nevski
*  first working release
*
* Revision 1.2  2000/01/06 23:57:24  nevski
* cvs header record corrected
*
* Revision 1.1  2000/01/06 01:01:09  nevski
* dzdoc for agi
*
      subroutine dzdire
      implicit   none
      INTEGER    NLPATM,MXFILES,NLCDIR,NLNDIR,NLPAT,ICDIR,
     >                          NCHTOP,ICHTOP,ICHTYP,ICHLUN
      PARAMETER (NLPATM=100, MXFILES=50)
      COMMON /HCDIRN/NLCDIR,NLNDIR,NLPAT,ICDIR,NCHTOP,ICHTOP(MXFILES)
     +              ,ICHTYP(MXFILES),ICHLUN(MXFILES)
      CHARACTER*16   CHNDIR,    CHCDIR,    CHPAT    ,CHTOP
      COMMON /HCDIRC/CHCDIR(NLPATM),CHNDIR(NLPATM),CHPAT(NLPATM),
     +               CHTOP(NLPATM)
      CHARACTER*80   HFNAME
      COMMON /HCFILE/HFNAME(MXFILES)
      Integer           LRZDOC
      Common /AgCZDOC/  LRZDOC
*
*     NLPAT  = NLPAT +1
*     NLCDIR = NLCDIR+1
*     CHCDIR(NLCDIR) = 'RZDOC'

      NCHTOP = NCHTOP+1
      CHTOP (NCHTOP) = 'RZDOC'
      HFNAME(NCHTOP) = 'bank documentation system'
      ICHTOP(NCHTOP) = -LRZDOC   ! -LOCF(LUN)
      ICHTYP(NCHTOP) = 256
      ICHLUN(NCHTOP) = -1
*
      end
