C----------------------------------------------------------------------
C
C                               *
C                              ***
C                             ** **
C                            **   **
C                    *********     *********
C                      ****           ****
C                        ***         ***
C                        **     *     **
C                       **    *****    **
C                      **   **     **   **
C                     **                 **
C
C----------------------------------------------------------------------
C
      INTEGER FUNCTION TPEAM( 
     +                        tphith, tphit 
     +,                       mctrlh, mctrl
     +,                       mkineh, mkine
     +,                       mhits_tpch, mhits_tpc 
     +,                       tssparh, tsspar 
     +,			      tpclusterh, tpcluster
     +,			      tpseqh, tpseq
     +,			      tppixelh, tppixel 
     +,			      tpmcpixh, tpmcpix 
     +,			      pad_planeh, pad_plane
     +,			      detectorh, detector )

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tphith:
C     tphit:
C
C   Output arguments : 
C     None: this module fills a hit data ntuple for subsequent
C           analysis
C
C   Functional Description : 
C
C   evaluation module converting cluster data into an ntuple...
C
C   Moved into TAS 15-Apr-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   1 Dec MA Lisa
C	- added ntuple 500+event# to show pixel information for each hit
C
C   14Dec MA Lisa
C	- account for the fact that # Geant volumes over each row may vary
C	  through use of numbv_row array
C
C   24mar95 malisa
C       - now unpack data in the new way datum = adc+'1000'X*tdc+'1000000'X*numseq
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tcl_tphit_pars.inc"
#include "tcl_tphit_st.inc"

#include "tss_tppixel_pars.inc"
#include "tss_tppixel_st.inc"

#include "tss_tpmcpix_pars.inc"
#include "tss_tpmcpix_st.inc"

#include "tcl_tpcluster_pars.inc"
#include "tcl_tpcluster_st.inc"

#include "tcl_tpseq_pars.inc"
#include "tcl_tpseq_st.inc"

#include "mct_mctrl_pars.inc"
#include "mct_mctrl_st.inc"
#include "mct_mkine_pars.inc"
#include "mct_mkine_st.inc"
#include "mct_mhits_tpc_pars.inc"
#include "mct_mhits_tpc_st.inc"

#include "tss_tsspar_pars.inc"
#include "tss_tsspar_st.inc"

#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

#include "tpg_detector_pars.inc"
#include "tpg_detector_st.inc"

C-----------------------------------------------------------------------
      RECORD/ table_head_st/ tphith
      RECORD/ tphit_row_st/ tphit(*)

      RECORD/ table_head_st/ mctrlh
      RECORD/ mctrl_row_st/ mctrl(*)
      RECORD/ table_head_st/ mkineh
      RECORD/ mkine_row_st/ mkine(*)
      RECORD/ table_head_st/ mhits_tpch
      RECORD/ mhits_tpc_row_st/ mhits_tpc(*)

      RECORD/ table_head_st/ tssparh
      RECORD/ tsspar_row_st/ tsspar(*)

	RECORD/ table_head_st/ tpclusterh
	RECORD/ tpcluster_row_st/ tpcluster(*)

	RECORD/ table_head_st/ tpseqh
	RECORD/ tpseq_row_st/ tpseq(*)

	RECORD/ table_head_st/ tppixelh
	RECORD/ tppixel_row_st/ tppixel(*)

	RECORD/ table_head_st/ tpmcpixh
	RECORD/ tpmcpix_row_st/ tpmcpix(*)

	RECORD/ table_head_st/ pad_planeh
	RECORD/ pad_plane_row_st/ pad_plane(*)

	RECORD/ table_head_st/ detectorh
	RECORD/ detector_row_st/ detector(*)

C-----------------------------------------------------------------------
c
c        functions
c
c        integer lochar        ! Obsolete
         integer lenocc,icloc
c
C-----------------------------------------------------------------------
c
c        HBOOK variables...
c
         common/quest/iquest
         integer iquest(100)

         integer nquota,nvar,icycle,nvar2,nvar3
         character*4 chtags(9),chtags2(13),chtags3(5)
C-----------------------------------------------------------------------
         logical lstat,first

         real vec(17)

         integer istat
         integer indx, ihit
         integer isect,irow,iwire,nseq,numbv
         integer kdummy

         integer mxhits
         parameter (mxhits = 45)
         
         integer nmctrk,nhitrk,itrk,nmchit,oldmchit
         integer nhitrk2,nmchit2
         character*80 outfile
         integer iper,ihisto
        
         integer nhitrow(mxhits),wire_hits,kwir
         real tphit_q(mxhits),mchit_q(mxhits)

         integer numpix_factor/'100000'X/
         integer tdc_factor/'400'X/
         integer nvar4
         character*4 chtags4(17)
         integer khit,kclus,nseq_hit,ktpc_row,kseq_count
         integer ksector,krow,kseq,kpix,kdatum,kpad
         integer numpix_seq,kadc,ktdc,kpix_count
         integer kmcid,krowtemp,ksectortemp
         logical kstatus,tpg_local_to_global,tpg_global_to_local
         double precision tpg_pad_to_x,tpg_row_to_y
         double precision invec(3),outvec(3)
         real time_bucket
         integer mxrow
         parameter(mxrow=45)
         integer id_pad(mxrow)
         integer imal

         real desum

c to account for the fact that the number or volumes per row has changed..

         integer numbv_row(mxrow),numbv_row_max
         integer numbv_row_2mm(mxrow),numbv_row_4mm(mxrow)
         data numbv_row_2mm/13,13,13,13,13,13,13, ! inner sector
     +        13,13,13,13,13,13,
     +        10,10,10,10,10,10,10,10,10,10,10,	! outer sector
     +        10,10,10,10,10,10,10,10,10,10,10,
     +        10,10,10,10,10,10,10,10,10,10/
         data numbv_row_4mm/9,9,9,9,9,9,9,9,9,9,9,9,9, ! inner sector
     +        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5, ! outer sector
     +        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5/
         parameter (numbv_row_max=13)
         logical testcond

         integer mcptr(numbv_row_max*mxhits),dtptr(mxhits)

C-----------------------------------------------------------------------
         data nvar/9/,nvar2/13/,nvar3/5/,nvar4/17/
         data chtags/'itrk','itcl','nrow','x','y','z','prf','zrf','q'/
         data chtags2/'itrk','mcx ','mcy ','mcz ','tpx ','tpy ','tpz '
     +,              'delx','dely','delz','nrow', 'clus','nseq'/
         data chtags3/'prec','mce','tpe','rde','pre2'/
	 data chtags4/'hit ','clus','sect','row ','pad ',
     +		      'tdc ','adc ','mcid','xpix','ypix','zpix',
     +		      'xloc','yloc','zloc','xh_l','yh_l','zh_l'/
         data first/.true./,ihisto/0/
C-----------------------------------------------------------------------
c
c        set status
c
         tpeam = tucod_ok

         if(first)then
            first = .false.

c	    set up for 2mm or 4mm geant volumes...
	    if (tsspar(1).geant_volm.eq.0.4) then
	      do irow=1,mxrow
		numbv_row(irow) = numbv_row_4mm(irow)
	      enddo
	    elseif (tsspar(1).geant_volm.eq.0.2) then
	      do irow=1,mxrow
		numbv_row(irow) = numbv_row_2mm(irow)
	      enddo
	    else
	      write(6,*)'TPEAM -- Nonsense Geant volumes!!'
	      do irow=1,mxrow
		numbv_row(irow) = numbv_row_4mm(irow)
	      enddo
	    endif
c
c           open rz file for ntuple...
c
            if(mctrl(1).infile .eq. '""')then
               outfile = 'tpe.rzd'
            else
               outfile = mctrl(1).infile
c              iper = lochar(1H.,1,mctrl(1).infile,80)
               iper = icloc('.',1,mctrl(1).infile,1,80)
               if(iper.gt.0)then
                  outfile(iper+1:iper+7) = 'tpe_rzd'
               else
                  outfile = 'tpe.rzd'
               endif
            endif

            open(unit=23,file=outfile,form='unformatted',
     +        recl=1024,access='direct',
     +        status='new',iostat=istat)
            nquota=10000
            if(istat.eq.0)then
               iquest(10)=nquota
               call hrfile(23,'NTUP','NQ')
            endif

c things needed for the ntuple 500+ihisto...
 	    time_bucket = detector(1).drift_length/tsspar(1).ntime
            id_pad(1) = 0
            do irow = 2, mxrow
               id_pad(irow) = id_pad(irow-1)
     +                        + pad_plane(1).npads(irow-1)
            enddo


         endif

         call hcdir('//NTUP',' ')

         ihisto = ihisto+1
c ntuple 200 as defined has no more information than the hits table...
*         call hbookn(200+ihisto,'Hit Info',nvar,'NTUP',8000,
*     +           chtags)
         call hbookn(300+ihisto,'Compare Info',nvar2,'NTUP',13000,
     +              chtags2)
         call hbookn(400+ihisto,'Track Info',nvar3,'NTUP',4000,
     +              chtags3)
         call hbookn(500+ihisto,'Hit Pixel Info',nvar4,'NTUP',17000,
     +              chtags4)
c        
c        proceed with evaluation...
c
*         do indx = 1, tphith.nok
c     
*            vec(1)=real(tphit(indx).track)
*            vec(2)=real(tphit(indx).cluster)
*            vec(3)=real(tphit(indx).row)
*            vec(4)=tphit(indx).x
*            vec(5)=tphit(indx).y
*            vec(6)=tphit(indx).z
*            vec(7)=tphit(indx).prf
*            vec(8)=tphit(indx).zrf
*            vec(9)=tphit(indx).q

*            call hfn(200+ihisto,vec)
*         enddo
c     
c->      close ntuple file...
c
*         call hrout(200+ihisto,icycle,' ')
c        call hrend('ntup')
c        close(23)
c
c        the following works only for sparse events!
c        more efficient and flexible would sort primary 
c        keys on itrk, etc...
c
c
         do itrk = 1, mkineh.nok
           nhitrk = 0
           nhitrk2 = 0
           nmctrk = 0
           nmchit = 0
           nmchit2 = 0
           oldmchit = 0
           call vzero(dtptr,45)
c           call vzero(mcptr,180)

           call vzero(tphit_q,45)
           call vzero(mchit_q,45)
           call vzero(mcptr,numbv_row_max*mxhits)
c
c          build pointers to reconstructed hits on itrk
c
           do ihit = 1, tphith.nok
              if(tphit(ihit).track.eq.itrk)then
                 nhitrk = nhitrk + 1
                 if(tphit(ihit).row.ge.40)then
                    nhitrk2 = nhitrk2 + 1
                 endif
c
c                stupidity for HPUX compiler
c
c                dtptr(tphit(ihit).row) = ihit
                 kdummy = tphit(ihit).row
		 kdummy = mod(kdummy,100)  ! now row=100*SECTOR+ROW mal 1mar95
                 dtptr(kdummy) = ihit
                 tphit_q(nhitrk)=tphit(ihit).q
              endif
           enddo
c          write(6,*)' ',nhitrk,' tphits on itrk = ',itrk
c
c          build pointers to mc hits on itrk
c
           do ihit = 1, mhits_tpch.nok
              if(mhits_tpc(ihit).itrk.eq.itrk)then
                 nmctrk = nmctrk + 1
                 numbv = mhits_tpc(ihit).numbv
c note that iwire no longer refers to wire# - it is subvolume over row...
                 iwire = numbv/10000
                 isect = (numbv-10000*iwire)/100
                 irow = (numbv-10000*iwire-100*isect)

                 mcptr((irow-1)*numbv_row_max+iwire) = ihit
              endif
           enddo
c          write(6,*)' ',nmctrk,' mchits on itrk = ',itrk
c
c          collect comparison statistics for all mc/rec hits on itrk
c
           do irow = 1, 45

c              if(mcptr((irow-1)*4+1).ne.0 
c     +       .or.mcptr((irow-1)*4+2).ne.0 
c     +       .or.mcptr((irow-1)*4+3).ne.0 
c     +       .or.mcptr((irow-1)*4+4).ne.0 )then

	      testcond = .false.
	      do iwire=1,numbv_row(irow)
		testcond = (testcond.or.
     +			(mcptr((irow-1)*numbv_row_max+iwire).ne.0))
	      enddo
	      if (testcond) then
                 nmchit = nmchit + 1
                 if(irow.ge.40)then
                    nmchit2 = nmchit2 + 1
                 endif
              endif

              if (nmchit .ne. oldmchit)then
                 oldmchit = nmchit

                 vec(11) = real(irow)
                 vec(1) = itrk
                 vec(2) = 0.
                 vec(3) = 0.
                 vec(4) = 0.

                 wire_hits = 0
c something of a philosophical point: could weight the x,y,z averages
c  by dE.  To do this, remove the *'s
		 desum = 0.0
                 do kwir=1,numbv_row(irow)
                  if(mcptr((irow-1)*numbv_row_max+kwir).ne.0)then
                   vec(2)=vec(2)
     +              +mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).x(1)
*     +              *mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).de
                   vec(3)=vec(3)
     +              +mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).x(2)
*     +              *mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).de
                   vec(4)=vec(4)
     +              +mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).x(3)
*     +              *mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).de
                   mchit_q(nmchit)=mchit_q(nmchit)
     +        +1.0e06*mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).de
                   wire_hits = wire_hits + 1
		   desum = desum + 
     +              mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).de
                  endif
                 enddo
*                 if(desum.gt.0.0)then
                 if(wire_hits.gt.0)then
                    vec(2)=vec(2)/float(wire_hits)
                    vec(3)=vec(3)/float(wire_hits)
                    vec(4)=vec(4)/float(wire_hits)
*                    vec(2)=vec(2)/desum
*                    vec(3)=vec(3)/desum
*                    vec(4)=vec(4)/desum
                 endif
c                if(irow .le. pad_plane(1).nrow_in)then
                 if(irow .le. 13)then
                    mchit_q(nmchit)=mchit_q(nmchit)*tsspar(1).gain_in
                 else
                    mchit_q(nmchit)=mchit_q(nmchit)*tsspar(1).gain_out
                 endif
c
c                correct for integral
c
                 mchit_q(nmchit) = 8.*mchit_q(nmchit)
                 
                 if(dtptr(irow).ne.0)then
                    vec(5) = tphit(dtptr(irow)).x
                    vec(6) = tphit(dtptr(irow)).y
                    vec(7) = tphit(dtptr(irow)).z
                    vec(8) = vec(2)-vec(5)
                    vec(9) = vec(3)-vec(6)
                    vec(10) = vec(4)-vec(7)
		    vec(12) = tphit(dtptr(irow)).cluster
                    vec(13) = tpcluster(vec(12)).nseq
                 else
                    vec(5) = 0.
                    vec(6) = 0.
                    vec(7) = 0.
                    vec(8) = 0.
                    vec(9) = 0.
                    vec(10) = 0.
                    vec(12) = 0.
                    vec(13) = 0.
                 endif
                 call hfn(300+ihisto,vec)
                 
              endif      ! if(nmchit .ne. oldmchit)
           enddo      ! do irow = 1, 45

           if(nmchit.gt.0)then
              vec(1) = float(nhitrk)/float(nmchit)
           else
              vec(1) = 0.
           endif
           if(nmchit2.gt.0)then
              vec(5) = float(nhitrk2)/float(nmchit2)
           else
              vec(5) = 0.
           endif
c
c          perform truncation try lower 60%
c
           do indx = 1, nint(0.6*float(nmchit))
              vec(2) = vec(2) + mchit_q(indx)
           enddo
           vec(2) = vec(2)/float(nint(0.6*float(nmchit)))

           do indx = 1, nint(0.6*float(nhitrk))
              vec(3) = vec(3) + tphit_q(indx)
           enddo
           vec(3) = vec(3)/float(nint(0.6*float(nhitrk)))

c
c          it really only makes sense to plot dE/E since 
c          noise is constant wrt the min. ion. peak
c
           vec(4) = (vec(2)-vec(3))/vec(2)

           call hfn(400+ihisto,vec)
         enddo        ! do itrk = 1, mkineh.nok

         call hrout(400+ihisto,icycle,' ')
         call hrout(300+ihisto,icycle,' ')

c now fill pixel data for each hit - added 28nov94 mal
*	goto 119

	do khit=1,tphith.nok
	  kclus = tphit(khit).cluster
	  invec(1)=dble(tphit(khit).x)
	  invec(2)=dble(tphit(khit).y)
	  invec(3)=dble(tphit(khit).z)
	  nseq_hit = tpcluster(kclus).nseq
	  kseq = tpcluster(kclus).jseq
	  ktpc_row = tpcluster(kclus).tpc_row
	  ksector = ktpc_row/100
	  krow = ktpc_row - ksector*100
	  if (krow.gt.pad_plane(1).nrow_in) then
	    krowtemp = krow-pad_plane(1).nrow_in
            ksectortemp = ksector*2
	  else
	    krowtemp = krow
            ksectortemp=ksector
	  endif
	  kstatus=tpg_global_to_local(ksectortemp,invec,outvec)
	  vec(15)=real(outvec(1))
	  vec(16)=real(outvec(2))
	  vec(17)=real(outvec(3))
	  vec(1)=khit
	  vec(2)=kclus
	  vec(3)=ksector
	  vec(4)=krow
	  invec(2)=tpg_row_to_y(krowtemp,ksectortemp)
	  vec(13) = real(invec(2))
	  do kseq_count=1,nseq_hit
	    kpad = tpseq(kseq).secpad - id_pad(krow)
	    kpix = tpseq(kseq).jpix
	    kdatum=tppixel(kpix).datum
c	    numpix_seq=ibits(kdatum,8,6)
            numpix_seq=kdatum/numpix_factor ! 24mar95 malisa
	    vec(5)=kpad
	    invec(1)=tpg_pad_to_x(kpad,krowtemp,ksectortemp)
	    vec(12) = real(invec(1))
	    do kpix_count=1,numpix_seq
	      kdatum=tppixel(kpix).datum
c	      kadc = ibits(kdatum,16,16)
c	      ktdc = ibits(kdatum,0,8) + 256*ibits(kdatum,14,2)
              ktdc = (kdatum - numpix_factor*numpix_seq)/tdc_factor
              kadc = kdatum - numpix_factor*numpix_seq - tdc_factor*ktdc
	      kmcid = tpmcpix(kpix).mcid
	      vec(6)=ktdc
	      vec(7)=kadc
	      vec(8)=kmcid
	      invec(3) = dble(ktdc*time_bucket)
	      vec(14) = real(invec(3))
	      kstatus=tpg_local_to_global(ksectortemp,invec,outvec)
	      if (.not.kstatus)
     +		write(6,*)'Global transform failed!!',ksector
	      vec(9)=real(outvec(1))
	      vec(10)=real(outvec(2))
	      vec(11)=real(outvec(3))
	      call hfn(500+ihisto,vec)
	      kpix=kpix+1
	    enddo
	    kseq = tpseq(kseq).next
	  enddo
	enddo

 119	continue

         call hrout(500+ihisto,icycle,' ')


c        if( mctrl(1).nevt .eq. mctrl(1).mxevt)then
c           call hrend('ntup')
c           close(23)
c        endif
c
         return
         end

C----------------------------------------------------------------------

        SUBROUTINE TPE_CLOSE

        Implicit None

C----------------------------------------------------------------------

        call hcdir('//NTUP',' ')
        call hrend('NTUP')
        close(23)

        return
        end


C----------------------------------------------------------------------
