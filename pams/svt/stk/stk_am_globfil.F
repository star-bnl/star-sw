      INTEGER*4 FUNCTION STK_AM_GLOBFIL(
     1            stkpar_h,            stkpar ,
     3             track_h,             track ,
     5           mctrack_h,           mctrack ,
     6           globtrk_h,           globtrk ) 

C     temporary module by Ken Wilson to fill globtrk from SVT
C     Jun 8,1994

      IMPLICIT NONE
#include "stk_am_globfil.inc"
      
      integer itrack,i,ntrack

      WRITE(*,*) 'STK_AM_GLOBFIL: Starting ############################'

      if (stkpar(1).globfil.eq.2) then
         do itrack=1,track_h.nok
            do i=1,15
               globtrk(itrack).cov(i)=track(itrack).cov(i)
            enddo
            globtrk(itrack).chisq=track(itrack).chisq(1)+
     1           track(itrack).chisq(2)
            globtrk(itrack).id=track(itrack).id_mctrack
            globtrk(itrack).icharge=track(itrack).invpt/
     1           abs(track(itrack).invpt)
            globtrk(itrack).r0=track(itrack).r0
            globtrk(itrack).phi0=track(itrack).phi0
            globtrk(itrack).z0=track(itrack).z0
            globtrk(itrack).invpt=abs(track(itrack).invpt)
            globtrk(itrack).psi=track(itrack).psi
            globtrk(itrack).tanl=track(itrack).tanl
            globtrk(itrack).id_global_pid=track(itrack).pid
         enddo         
         globtrk_h.nok=track_h.nok
      else if (stkpar(1).globfil.eq.1) then
         ntrack=0
         do itrack=1,mctrack_h.nok
            if (mctrack(itrack).flag.ne.0) then
               ntrack=ntrack+1
               do i=1,15
                  globtrk(ntrack).cov(i)=mctrack(itrack).cov(i)
               enddo
               globtrk(ntrack).chisq=mctrack(itrack).chisq(1)+
     1              mctrack(itrack).chisq(2)
               globtrk(ntrack).id=track(itrack).id_mctrack
               globtrk(ntrack).icharge=mctrack(itrack).invpt/
     1              abs(mctrack(itrack).invpt)
               globtrk(ntrack).r0=mctrack(itrack).r0
               globtrk(ntrack).phi0=mctrack(itrack).phi0
               globtrk(ntrack).z0=mctrack(itrack).z0
               globtrk(ntrack).invpt=abs(mctrack(itrack).invpt)
               globtrk(ntrack).psi=mctrack(itrack).psi
               globtrk(ntrack).tanl=mctrack(itrack).tanl
               globtrk(ntrack).id_global_pid=mctrack(itrack).pid
            endif
         enddo
         globtrk_h.nok=ntrack
      endif


      WRITE(*,*) 'STK_AM_GLOBFIL: Stopping ############################'
      STK_AM_GLOBFIL=STAFCV_OK
      RETURN
      END
