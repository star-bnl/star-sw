* $Id: tpi_pid.F,v 1.2 1998/01/27 01:54:48 fisyak Exp $
* $Log: tpi_pid.F,v $
* Revision 1.2  1998/01/27 01:54:48  fisyak
* Split sources
*
       INTEGER FUNCTION  tpi_pid(parm_h,parm,trk_h,trk,gl_h,gl,
     >                   hypo_h,hypo)
       IMPLICIT  NONE
C       
C        
C Input arguments
C      track_h = header for the tptrack table (reconstructed tracks)
C      track = rows of the tptrack table
C
C Output arguments:
C	hypo-pid filled for tpc dedx vs p, i.e.
C	the probability  each global track is an e, pi, K, p
C
C Functional description:
C      A module that takes the dE/dx from tde and evaluates the probality
C	of various pid hypotheses.
C
C Contact: C. Ogilvie, MIT, (617) 253-4184 Ogilvie@mitlns.mit.edu
C-----------------------------------------------------------------------

#include "tpi_pid.inc" 

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

      INTEGER  i,j
      REAL     pt,pz,p,invpt,tanl,dedx1,dedx2
      INTEGER  nrec

      REAL     mass(4),gpid_pos(4),gpid_neg(4)
      REAL     dedxpar(4,3),sigmapar(4,2)

      REAL     z,x,satura,beta,gamma,bethe,rise
      REAL     ave_dedx(4),sigma(4),t
      REAL     pi

      INTEGER  nid,num_deg,method,nid1,nid2,n
      INTEGER  id,highest_id, current_id
      INTEGER  use_this_id,icharge,istart
      REAL     prob_of_meas(4),nsigma(4),prob_of_hypo(4)
      REAL     sum, weight, large1, large2
      REAL     prob_species(4), sep

      REAL     prob(4)
  


      LOGICAL  no_track
C-----------------------------------------------------------------------

      write(6,*) 'In the probability tpi module. '
      write(6,*) ' # global, tpc tracks ', gl_h.nok,trk_h.nok

C***********************************************************************

      istart = parm(1).method(1)
      method = parm(1).method(2)
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
      write(6,*) '# entries in hypo-pid table', hypo_h.nok
      write(6,*) 'current id for hypo-pid', current_id
 
      pi = 3.14159
      mass(1) = 0.000511   ! electron
      gpid_pos(1) = 2
      gpid_neg(1) = 3
      mass(2) = 0.13957    ! pion
      gpid_pos(2) = 8
      gpid_neg(2) = 9
      mass(3) = 0.4936     ! kaon
      gpid_pos(3) = 11
      gpid_neg(3) = 12
      mass(4) = 0.9383     ! proton
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

       write(6,*) 1,dedxpar(1,1),dedxpar(1,2)
       write(6,*) 2,dedxpar(2,1),dedxpar(2,2)
       write(6,*) 3,dedxpar(3,1),dedxpar(3,2)
       write(6,*) 4,dedxpar(4,1),dedxpar(4,2)
       
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



      z=1.0
      satura=5.0e-4


C***********************************************************************

      do j=1, gl_h.nok    ! Loop over global tracks

c
c this requires that id_hypo_pid is initialised to zero, 
c and only becomes non-zero if a partition is looked at the
c PID for that track
         if (gl(j).id_hypo_pid.eq.0) then
		use_this_id = current_id
                gl(j).id_hypo_pid = use_this_id
                current_id = current_id + 1
         else
c correct
c		use_this_id = gl(j).id_hypo_pid
c
c the next lines are a 
c temporary hack that presumes no-one else has done any PID yet
c
            	use_this_id = current_id
                gl(j).id_hypo_pid = use_this_id
                current_id = current_id + 1
	 endif

c         write (6,*) gl(j).id_hypo_pid
c         pause
C     Extract momentum values of the track
c
         icharge = gl(j).icharge
         invpt = gl(j).invpt
         tanl = gl(j).tanl
         if (invpt.gt.0.0) then
            pt = 1.0/invpt
         else
            pt = 0.0
         endif
         pz = pt*tanl
         p = sqrt(pt*pt+pz*pz)
c
c find the tptrack (i) that corresponds to this global track (j)
c
         i = 1
         no_track = .false.
         do while (trk(i).id_globtrk.ne.gl(j).id)
           i=i+1
           if (i.gt.trk_h.nok) then
c
c do something drastic
c
               no_track = .true.
               goto 2
           endif
         enddo
         
c look at only those global tracks with a TPC track
 2     continue
       if (.not.no_track) then
      
c	  write(6,*) 'matched a tpc track with global track '
c          write(6,*) 'trk.id_globtk', i

 
          dedx1 = trk(i).dedx(1)
          dedx2 = trk(i).dedx(2)
          nrec = trk(i).nrec

c	  write(6,*) 'info dedx, p, nrec', dedx1, p,nrec
     
c       
c loop thru the 4 hypotheses, e,pi,K, p
c	
          do nid=istart,4
 
C     Calculate the mean dedx, and sigma 


c Bethe-Bloch form 1997
c
	    gamma=sqrt((p/mass(nid))**2+1.)
            beta=sqrt(abs(-1./(gamma**2)+1.))
            rise=abs(dedxpar(nid,3))*(beta*gamma)**2
         bethe=dedxpar(nid,1)/(beta**2)*
     >         (0.5*log(rise)-beta**2+dedxpar(nid,2))
             ave_dedx(nid)=bethe
 
            if (ave_dedx(nid).gt.satura) ave_dedx(nid)=satura

c
c form of calculation 1997, constant percentage of dedx (temporary
c
	    sigma(nid) = sigmapar(nid,1)*ave_dedx(nid)
            
            if (sigma(nid).lt.0.01e-05) sigma(nid)=0.01e-05

c 
c calculate the probability that this measurement occured
c
	    x = ((dedx1-ave_dedx(nid))**2)/2./(sigma(nid)**2)
            prob_of_meas(nid) = 
     >      1./(sigma(nid)*sqrt(2.*pi))*exp(-x)
c
c calc number of sigma measurement is away from mean, 
c	+ve=above mean, -ve = below mean
c
            nsigma(nid) = (dedx1-ave_dedx(nid))/sigma(nid)
c	  write(6,*) 'info dedx, p, nid, nsigma', dedx1, p,nid,nsigma(nid)
c          pause
            
c
c     calculate t-statistic , and f(t)
c 
           if (dedx2.ne.0.) then
             t = sqrt(real(nrec))*(dedx1-ave_dedx(nid))/dedx2
             num_deg = nrec - 1
c
c          calc f(t)
c          prob_of_hypo(nid) = f
c
           endif
         enddo  ! end loop over hypotheses
         
c
c choose option for pid
c
         if (method.eq.2) then
           do nid=istart,4
            prob(nid) = prob_of_meas(nid)
           enddo
         elseif (method.eq.3) then
           do nid=istart,4
            prob(nid) = prob_of_hypo(nid)
           enddo
         endif
c
c calculate probability that measurement is a particular species by forming
c prob_species = prob / sum of prob
c
         sum = 0.
         do nid=istart,4
          sum = sum + prob(nid)
         enddo
         if (sum.ne.0.) then
            do nid=istart,4
              prob_species(nid) = prob(nid)/sum
            enddo  
         endif
c
c calculate weight for this track. This is used by global pid when
c combining info from each detector.
c first find the two most probable hypotheses, find the separation between
c them , weight = separation/sqrt(dedx2**2+0.5*sigma1**2+0.5*sigma2*2)
c
         large1 = prob_species(istart)
         large2 = prob_species(istart)
         nid1 = istart
         nid2 = istart
         do nid=istart+1,4
          if (prob_species(nid).gt.large2) then
            if (prob_species(nid).gt.large1) then
               nid2 = nid1
               nid1 = nid
               large2 = prob_species(nid2)
               large1 = prob_species(nid1)
            else
               nid2 = nid
               large2 = prob_species(nid2)
            endif
          endif
         enddo
         if (nid1.gt.0.and.nid2.gt.0) then
           sep = abs(ave_dedx(nid1)-ave_dedx(nid2))
           weight = sep /sqrt(dedx2**2 + 0.5*sigma(nid1)**2 + 
     1                      0.5*sigma(nid2)**2)
         else
           weight =0.
         endif
c        write (6,*) weight
c
c temp
c
c        weight = dedx1 - ave_dedx(nid1)
c
        do nid=istart,4 
c
c fill hypo pid table
c
          n = hypo_h.nok+1
          hypo(n).id = use_this_id
          if (icharge.gt.0) then   
            hypo(n).gid = gpid_pos(nid)
          else
            hypo(n).gid = gpid_neg(nid)
          endif
          hypo(n).prob = prob_species(nid)
          hypo(n).nsigma = nsigma(nid)
          hypo(n).method = method
          hypo(n).weight = weight
          hypo(n).det    = 2
c
c temporary filling for debugging
cc
c          hypo(n).det = int(prob_species(nid)*100.+0.5)
c          hypo(n).weight = dedx1 
c	  hypo(n).nsigma = p
c          hypo(n).method = nrec
          hypo_h.nok = hypo_h.nok + 1
         enddo

c
        endif   ! end if there was a tpc track associated with global track

      enddo   ! End loop over tracks

*      write (6,*) 'finished tpi pid'

      tpi_pid = STAFCV_ok

      return
      end
