      INTEGER*4 FUNCTION trackradl(r,radl)

*****************************************************************
*                                                               *
* This code returns the % radiation length between the primary  *
* vertex and the track point at r. This is simply a fetch from  *
* a table precalculated for the current track using current     *
* GSTAR geometry.                                               *
*                                                               *
*****************************************************************

       IMPLICIT NONE

       Integer ns,i
       Real e, de, radl, r, rs, rl, drl
       common /mymate/ns,rs(100),de(100),e(100),rl(100),drl(100)

*       print *,' r,ns,rs(1),rs(2),e(1),e(2)= ',r,ns,rs(1),rs(2),e(1),e(2)

       radl = 0.0
       trackradl = 0
       if(ns.le.0) return
       if(r.lt.rs(1))then
        radl = rl(1)*r/rs(1)
        trackradl = 1
        return
       endif
       if(r.gt.rs(ns))then
        radl = 0.0
        return
       endif
       do i = 1,ns-1
*        print *,' i,r,rs(i),rs(i+1),rl(i),drl(i)= ',i,r,rs(i),rs(i+1),rl(i),drl(i)
        if((r.gt.rs(i)).and.(r.lt.rs(i+1)))then
         radl = rl(i+1) - drl(i+1)*(rs(i+1)-r)/(rs(i+1)-rs(i))
         trackradl = 1
        endif
       enddo

       END
  

