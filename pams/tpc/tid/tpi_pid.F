* $Id: tpi_pid.F,v 1.7 1999/07/19 19:15:51 genevb Exp $
* $Log: tpi_pid.F,v $
* Revision 1.7  1999/07/19 19:15:51  genevb
* Use StMessMgr
*
* Revision 1.6  1999/02/19 21:14:38  ogilvie
* added new method 3 = use weights to calc de/dx
*
* Revision 1.5  1998/11/17 15:29:48  ogilvie
* corrected foreign key pointing to global tracks
*
* Revision 1.4  1998/11/12 16:13:59  ogilvie
* resolution of dedx updated from mdc1 info
*
* Revision 1.3  1998/11/03 15:14:18  ogilvie
* tpi_pid.F now reads in dst_dedx tables
*
* Revision 1.2  1998/01/27 01:54:48  fisyak
* Split sources
*
       INTEGER FUNCTION tpi_pid(parm_h,parm,gl_h,gl,dedx_h,dedx,hypo_h,hypo)
       IMPLICIT  NONE
C       
C        
C Input arguments
C
C Output arguments:
C	hypo-pid filled for tpc dedx vs p, i.e.
C	how far the measured dE/dx is from the
c       the mean dE/dx expected for an e, pi, K, p
C
C Functional description:
C      A module that takes the dE/dx from tde and fills the deviates
C	of various pid hypotheses.
c      Cureently limited to Z=1 particles by hard-wiring
C
C Contact: C. Ogilvie, MIT, (617) 253-4184 Ogilvie@mit.edu
C-----------------------------------------------------------------------

#include "tpi_pid.inc" 
#include "phys_constants.inc"
#include "math_constants.inc"
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

      INTEGER  i,j,igl,ii,igl_use
      REAL     pt,pz,p,invpt,tanl,dedx1,dedx2,vert
      INTEGER  nrec

      REAL     mass(4),gpid_pos(4),gpid_neg(4)
      REAL     dedxpar(4,3),sigmapar(4,2)

      REAL     z,x,satura,beta,gamma,bethe,rise
      REAL     ave_dedx(4),sigma(4),t


      INTEGER  nid,num_deg,method,nid1,nid2,n
      INTEGER  id,highest_id, current_id
      INTEGER  use_this_id,icharge,istart
      REAL     prob_of_meas(4),nsigma(4),prob_of_hypo(4)
      REAL     sum, weight, large1, large2
      REAL     prob_species(4), sep

      REAL     prob(4)
      CHARACTER*132 m132
  


      LOGICAL  no_dedx,primary,found
C-----------------------------------------------------------------------


C***********************************************************************

      istart = parm(1).method(1)
      method = parm(1).method(2)

c      method = 0  ! test
      write(m132,*) 'TPI_PID - temporary check on method', method
      call StInfo(m132)
c
c a quick loop to see if hypo_h is long enough
c
      n=0
      do j=1, dedx_h.nok    ! Loop over dst dedx valuse
         if (dedx(j).det_id.eq.1) n=n+1 ! tpc    
      enddo
      if (hypo_h.nok+4*n.gt.hypo_h.maxlen) then
             write (m132,10)
 10          format(2x,'adding in tpc dedx deviants would exceed size of hypo')
             call StWarning(m132)
             tpi_pid = STAFCV_BAD
             return
      endif
      n=0  
         
c
c find the highest identifier in the hypo-pid table
c
      highest_id=0
      do i=1, hypo_h.nok
        id = hypo(i).id
        if (id.gt.highest_id) then
          highest_id = id
        endif
      enddo

      current_id = highest_id +1


      mass(1) = M_ELECTRON   ! electron
      gpid_pos(1) = 2
      gpid_neg(1) = 3
      mass(2) = M_PION_PLUS    ! pion
      gpid_pos(2) = 8
      gpid_neg(2) = 9
      mass(3) = M_KAON_PLUS     ! kaon
      gpid_pos(3) = 11
      gpid_neg(3) = 12
      mass(4) = M_PROTON     ! proton
      gpid_pos(4) = 14
      gpid_neg(4) = 15
c
c temporary location of fits to mean dedx fit to Bethe-Block form
c but with one normalisizing parameter, and one offset. 
c logartimic term fixed. These should be in a calibration
c database
C
C     dedx = dE/dx                            (in Gev/cm)
C     p = momentum                              (in Gev/c)
C     mass = rest mass of the particle          (in Gev/c**2)
C     z = charge                            
C     satura = assigned saturation value of dE/dx (in Gev/cm)

     
       dedxpar(1,1) = parm(1).mean1(1)   
       dedxpar(2,1) = parm(1).mean1(2)
       dedxpar(3,1) = parm(1).mean1(3)    
       dedxpar(4,1) = parm(1).mean1(4)


       dedxpar(1,2) = parm(1).mean2(1)
       dedxpar(2,2) = parm(1).mean2(2)
       dedxpar(3,2) = parm(1).mean2(3)  
       dedxpar(4,2) = parm(1).mean2(4)


       dedxpar(1,3) = parm(1).mean3(1)
       dedxpar(2,3) = parm(1).mean3(2)
       dedxpar(3,3) = parm(1).mean3(3)  
       dedxpar(4,3) = parm(1).mean3(4)

c       write(6,*) 1,dedxpar(1,1),dedxpar(1,2),dedxpar(1,3)
c       write(6,*) 2,dedxpar(2,1),dedxpar(2,2),dedxpar(2,3)
c       write(6,*) 3,dedxpar(3,1),dedxpar(3,2),dedxpar(3,3)
c       write(6,*) 4,dedxpar(4,1),dedxpar(4,2),dedxpar(4,3)
      
c    
c     sigma = sigmapar(i,1) + sigmapar(i,2)*<dedx>/sqrt(ndedx)
c
       sigmapar(1,1) = parm(1).sigma1(1)
       sigmapar(2,1) = parm(1).sigma1(2)      
       sigmapar(3,1) = parm(1).sigma1(3)
       sigmapar(4,1) = parm(1).sigma1(4)

       sigmapar(1,2) = parm(1).sigma2(1)
       sigmapar(2,2) = parm(1).sigma2(2)      
       sigmapar(3,2) = parm(1).sigma2(3)
       sigmapar(4,2) = parm(1).sigma2(4)

c
c hardwired z =1
c
      z=1.0
      satura=5.0e-4

c
c
C***********************************************************************

      do j=1, dedx_h.nok    ! Loop over dst dedx valuse

         if (dedx(j).det_id.eq.1) then ! tpc 
c          if (dedx(j).det_id.eq.2) then ! svt 
           igl= dedx(j).id_track
c
c find the global track with this index, note that igl is not
c necessarily the igl'th record, because it may have been sorted
c
            found=.false.
c
c check for simplest possibility to save time first
c
            if (gl(igl).id.eq.igl) then
               found=.true.
               igl_use = igl
            else
               igl_use = 1
            endif
            do while (.not.found.and.igl_use.le.gl_h.nok)
               if (gl(igl_use).id.eq.igl) then
                 found=.true.
               else
                 igl_use = igl_use+1
               endif
            enddo
            if (found.eq..false.) then
              write (m132,20)
 20           format(2x,'global track indexed by dst_dedx.id_track not found')
              call StWarning(m132)
              tpi_pid = STAFCV_BAD
              return
             endif         
c
c id_hypo_pid only becomes non-zero if a partition is looked at the
c PID for that track
c
           if (gl(igl_use).id_hypo_pid.eq.0) then
		use_this_id = current_id
                gl(igl_use).id_hypo_pid = use_this_id
                current_id = current_id + 1
           else
                write (m132,*) gl(igl_use).id_hypo_pid
                call StInfo(m132)
		use_this_id = gl(igl_use).id_hypo_pid
	   endif
c
C     Extract momentum values of the track
c
           icharge = gl(igl_use).icharge
           invpt = gl(igl_use).invpt
           tanl = gl(igl_use).tanl
           if (invpt.gt.0.0) then
             pt = 1.0/invpt
           else
             pt = 0.0
           endif
           pz = pt*tanl
           p = sqrt(pt*pt+pz*pz)
c
c rough cut on primary
c
           primary=.true.
           vert=sqrt(gl(igl_use).x_first(1)**2+gl(igl_use).x_first(2)**2)

            if (vert.gt.20) then
               primary=.false.
            endif        
            if (abs(gl(igl_use).x_first(3)).gt.20) then
               primary=.false.
            endif
           
 
           dedx1 = dedx(j).dedx(1)
           dedx2 = dedx(j).dedx(2)
           nrec = dedx(j).ndedx

c
c method = 0 is a testing feature
c          
          if (method.eq.0) then    
c
c temporary fill dedx table
c
             dedx(j).dedx(2)= p
             if (.not.primary) then
                dedx(j).iflag=1
             endif
          endif
c
c compare to Bethe-Block
c
             do nid=1,4
 
C     Calculate the mean dedx, and sigma 
c
c Bethe-Bloch
c
                gamma=sqrt((p/mass(nid))**2+1.)
                beta=sqrt(abs(-1./(gamma**2)+1.))
                rise=abs(dedxpar(nid,3))*(beta*gamma)**2
                bethe=dedxpar(nid,1)/(beta**2)*
     >               (0.5*log(rise)-beta**2-dedxpar(nid,2))
                ave_dedx(nid)=bethe
 
                if (ave_dedx(nid).gt.satura) ave_dedx(nid)=satura

c
c  protect against nrec=0
c
           if (nrec.gt.0) then
c
c width of dE/dx
c
                sigma(nid)=sigmapar(nid,1)+
     >             sigmapar(nid,2)*ave_dedx(nid)/sqrt(real(nrec))

            else
                sigma(nid) = -1
c
c              flag this as an error somehow
c                
            endif
c
c method=0 is a testing feature
c
                if (method.eq.0) sigma(nid)=1
   
c
c calc number of sigma measurement is away from mean, 
c	+ve=above mean, -ve = below mean
c
                nsigma(nid) = (dedx1-ave_dedx(nid))/sigma(nid)
                if (abs(nsigma(nid)).gt.1000) nsigma(nid)=1000.  

            enddo ! hypothe    

c
c fill deviants
c
          do nid=1,4 
             n = hypo_h.nok+1
 
             hypo(n).id = use_this_id
             if (icharge.gt.0) then   
                hypo(n).gid = gpid_pos(nid)
             else
                hypo(n).gid = gpid_neg(nid)
             endif
             hypo(n).prob = 0.
           
             hypo(n).nsigma = nsigma(nid)
             hypo(n).method = method
             hypo(n).method = nrec
             if (.not.primary) then
              hypo(n).weight = 0.
             else
              hypo(n).weight = p
             endif
             hypo(n).det    = 1
c
c method =0 is a test feature
c
             if (method.eq.0) then
                hypo(n).weight = p
                if (.not.primary) then
                  hypo(n).method=0
                else 
                 hypo(n).method=nrec
                endif
             endif          
c
c end test feature
c
             hypo_h.nok = hypo_h.nok + 1
           enddo ! hypothes
        endif    ! tpc dedx 
      enddo ! loop ove dst-dedx


      tpi_pid = STAFCV_ok

      return
      end



