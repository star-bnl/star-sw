$Id: dzdire.F,v 1.1 2000/01/06 01:01:09 nevski Exp $
$Log: dzdire.F,v $
Revision 1.1  2000/01/06 01:01:09  nevski
dzdoc for agi

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
