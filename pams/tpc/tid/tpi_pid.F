* $Id: tpi_pid.F,v 1.3 1998/11/03 15:14:18 ogilvie Exp $
* $Log: tpi_pid.F,v $
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

      INTEGER  i,j,igl,ii
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
  


      LOGICAL  no_dedx,primary
C-----------------------------------------------------------------------


C***********************************************************************

      istart = parm(1).method(1)
      method = parm(1).method(2)

c      method = 0  ! test

c
c a quick loop to see if hypo_h is long enough
c
      n=0
      do j=1, dedx_h.nok    ! Loop over dst dedx valuse
         if (dedx(j).det_id.eq.1) n=n+1 ! tpc    
      enddo
      if (hypo_h.nok+4*n.gt.hypo_h.maxlen) then
             write (6,10)
 10          format(2x,'adding in tpc dedx deviants would exceed size of hypo')
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

c       write(6,*) 1,dedxpar(1,1),dedxpar(1,2)
c       write(6,*) 2,dedxpar(2,1),dedxpar(2,2)
c       write(6,*) 3,dedxpar(3,1),dedxpar(3,2)
c       write(6,*) 4,dedxpar(4,1),dedxpar(4,2)
       
c    
c     sigma = sigmapar(i,1) + 1/p*sigmapar(i,2)
c
c old, better will be to parameterize resolution
c as a function of number of hits in sample
c currently sigma2 = 0

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
           igl= dedx(j).id_track
c
c id_hypo_pid only becomes non-zero if a partition is looked at the
c PID for that track
c
           if (gl(igl).id_hypo_pid.eq.0) then
		use_this_id = current_id
                gl(igl).id_hypo_pid = use_this_id
                current_id = current_id + 1
           else
		use_this_id = gl(igl).id_hypo_pid
	   endif
c
C     Extract momentum values of the track
c
           icharge = gl(igl).icharge
           invpt = gl(igl).invpt
           tanl = gl(igl).tanl
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
           vert=sqrt(gl(igl).x_first(1)**2+gl(igl).x_first(2)**2)

            if (vert.gt.20) then
               primary=.false.
            endif        
            if (abs(gl(igl).x_first(3)).gt.20) then
               primary=.false.
            endif
           
 
           dedx1 = dedx(j).dedx(1)
           dedx2 = dedx(j).dedx(2)
           nrec = dedx(j).ndedx
          
          if (method.eq.0) then    ! test
c
c temporary fill dedx table
c
             dedx(j).dedx(2)= p
             if (.not.primary) then
                dedx(j).iflag=1
             endif
          endif
          if (method.ne.0) then
c
c cmpare to Bethe-Block

             do nid=1,4
 
C     Calculate the mean dedx, and sigma 

c Bethe-Bloch form 1997
c
                gamma=sqrt((p/mass(nid))**2+1.)
                beta=sqrt(abs(-1./(gamma**2)+1.))
                rise=abs(dedxpar(nid,3))*(beta*gamma)**2
                bethe=dedxpar(nid,1)/(beta**2)*
     >               (0.5*log(rise)-beta**2+dedxpar(nid,2))
                ave_dedx(nid)=bethe
 
                if (ave_dedx(nid).gt.satura) ave_dedx(nid)=satura

c
c form of calculation 1997, constant percentage of dedx (temporary
c
                sigma(nid) = sigmapar(nid,1)*ave_dedx(nid)
            
                if (sigma(nid).lt.0.01e-05) sigma(nid)=0.01e-05
c
c calc number of sigma measurement is away from mean, 
c	+ve=above mean, -ve = below mean
c
                nsigma(nid) = (dedx1-ave_dedx(nid))/sigma(nid)
            
            enddo ! hypothe    

         endif   ! method ne 0
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
             if (.not.primary) then
              hypo(n).weight = 1.
             else
              hypo(n).weight = 0.
             endif
              
             hypo(n).det    = 2
          
             hypo_h.nok = hypo_h.nok + 1
           enddo ! hypothes
        endif    ! tpc dedx 
      enddo ! loop ove dst-dedx


      tpi_pid = STAFCV_ok

      return
      end



