* $Id: gtmany.F,v 1.1.1.1 2004/01/12 23:49:40 potekhin Exp $
* $Log: gtmany.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
* Revision 1.3  2001/06/14 00:10:01  nevski
* remove unused variable
*
* Revision 1.2  2001/03/02 21:17:21  nevski
* commons changed as in atlas version
*
* Revision 1.1  2001/02/27 10:15:44  nevski
*  first working release
*
      subroutine gtmany
*     return to many-alternatives
#include "commons/typing.inc"
#include "geant321/gcflag.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvdma.inc"
#include "geant321/gcvolu.inc"
      Integer i,ier

        if (mycoun.gt.1.and.nfmany.gt.0) then
*          no need to check when mycoun is set !
           do i=1,nlevel
              names(i) =manyna(nfmany,i)
              number(i)=manynu(nfmany,i)
           enddo
           call glvolu(nlevel,names,number,ier)
           if(ier.ne.0) print *,'Fatal error in GLVOLU'
           ingoto=0
         endif

      end

