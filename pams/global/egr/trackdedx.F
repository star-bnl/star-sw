      INTEGER*4 FUNCTION trackdedx(r,dedx)

*****************************************************************
*                                                               *
* This code returns the energy loss dedx between the primary    *
* vertex and the track point at r. This is simply a fetch from  *
* a table precalculated for the current track using current     *
* GSTAR geometry.                                               *
*                                                               *
*****************************************************************

       IMPLICIT NONE

       Integer ns,i
       Real e, de, dedx, r, rs
       common /mymate/ ns, rs(100), de(100), e(100)

*       print *,' r,ns,rs(1),rs(2),e(1),e(2)= ',r,ns,rs(1),rs(2),e(1),e(2)

       dedx = 0.0
       trackdedx = 0
       if(ns.le.0) return
       if(r.lt.rs(1))then
        dedx = e(1)*r/rs(1)
        trackdedx = 1
        return
       endif
       if(r.gt.rs(ns))then
        dedx = 0.0
        return
       endif
       do i = 1,ns-1
*        print *,' i,r,rs(i),rs(i+1),e(i),de(i)= ',i,r,rs(i),rs(i+1),e(i),de(i)
        if((r.gt.rs(i)).and.(r.lt.rs(i+1)))then
         dedx = e(i+1) - de(i+1)*(rs(i+1)-r)/(rs(i+1)-rs(i))
         trackdedx = 1
        endif
       enddo

       END
  

