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
     +,                       tssparh, tsspar )

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
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tcl_tphit_pars.inc"
#include "tcl_tphit_st.inc"

#include "mct_mctrl_pars.inc"
#include "mct_mctrl_st.inc"
#include "mct_mkine_pars.inc"
#include "mct_mkine_st.inc"
#include "mct_mhits_tpc_pars.inc"
#include "mct_mhits_tpc_st.inc"

#include "tss_tsspar_pars.inc"
#include "tss_tsspar_st.inc"

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

C-----------------------------------------------------------------------
c
c        functions
c
         integer lenocc,lochar
c
C-----------------------------------------------------------------------
c
c        HBOOK variables...
c
         common/quest/iquest
         integer iquest(100)

         integer nquota,nvar,icycle,nvar2,nvar3
         character*4 chtags(9),chtags2(11),chtags3(5)
C-----------------------------------------------------------------------
         logical lstat,first

         real vec(11)

         integer istat
         integer indx, ihit
         integer isect,irow,iwire,nseq,numbv

         integer mxhits
         parameter (mxhits = 45)
         integer mcptr(4*mxhits),dtptr(mxhits)
         
         integer nmctrk,nhitrk,itrk,nmchit,oldmchit
         integer nhitrk2,nmchit2
         character*80 outfile
         integer iper,ihisto
        
         integer nhitrow(mxhits),wire_hits,kwir
         real tphit_q(mxhits),mchit_q(mxhits)

C-----------------------------------------------------------------------
         data nvar/9/,nvar2/11/,nvar3/5/
         data chtags/'itrk','itcl','nrow','x','y','z','prf','zrf','q'/
         data chtags2/'itrk','mcx ','mcy ','mcz ','tpx ','tpy ','tpz '
     +,              'delx','dely','delz','nrow'/
         data chtags3/'prec','mce','tpe','rde','pre2'/
         data first/.true./,ihisto/0/
C-----------------------------------------------------------------------
c
c        set status
c
         tpeam = tucod_ok

         if(first)then
            first = .false.
c
c           open rz file for ntuple...
c
            if(mctrl(1).infile .eq. '""')then
               outfile = 'tpe.rzd'
            else
               outfile = mctrl(1).infile
               iper = lochar(1H.,1,mctrl(1).infile,80)
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
               call hrfile(23,'ntup','NQ')
            endif
         endif

         call hcdir('//ntup',' ')

         ihisto = ihisto+1
         call hbookn(200+ihisto,'Hit Info',nvar,'ntup',8000,
     +           chtags)
         call hbookn(300+ihisto,'Compare Info',nvar2,'ntup',11000,
     +              chtags2)
         call hbookn(400+ihisto,'Track Info',nvar3,'ntup',4000,
     +              chtags3)
c        
c        proceed with evaluation...
c
         do indx = 1, tphith.nok
c     
            vec(1)=real(tphit(indx).track)
            vec(2)=real(tphit(indx).cluster)
            vec(3)=real(tphit(indx).row)
            vec(4)=tphit(indx).x
            vec(5)=tphit(indx).y
            vec(6)=tphit(indx).z
            vec(7)=tphit(indx).prf
            vec(8)=tphit(indx).zrf
            vec(9)=tphit(indx).q

            call hfn(200+ihisto,vec)
         enddo
c     
c->      close ntuple file...
c
         call hrout(200+ihisto,icycle,' ')
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
           call vzero(mcptr,180)

           call vzero(tphit_q,45)
           call vzero(mchit_q,45)
           call vzero(mcptr,4*mxhits)
c
c          build pointers to reconstructed hits on itrk
c
           do ihit = 1, tphith.nok
              if(tphit(ihit).track.eq.itrk)then
                 nhitrk = nhitrk + 1
                 if(tphit(ihit).row.ge.40)then
                    nhitrk2 = nhitrk2 + 1
                 endif
                 dtptr(tphit(ihit).row) = ihit
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
                 
                 iwire = numbv/10000
                 isect = (numbv-10000*iwire)/100
                 irow = (numbv-10000*iwire-100*isect)

                 mcptr((irow-1)*4+iwire) = ihit
              endif
           enddo
c          write(6,*)' ',nmctrk,' mchits on itrk = ',itrk
c
c          collect comparison statistics for all mc/rec hits on itrk
c
           do irow = 1, 45

              if(mcptr((irow-1)*4+1).ne.0 
     +       .or.mcptr((irow-1)*4+2).ne.0 
     +       .or.mcptr((irow-1)*4+3).ne.0 
     +       .or.mcptr((irow-1)*4+4).ne.0 )then
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
                 do kwir=1,4
                    if(mcptr((irow-1)*4+kwir).ne.0)then
                       vec(2)=vec(2)
     +                           +mhits_tpc(mcptr((irow-1)*4+kwir)).x(1)
                       vec(3)=vec(3)
     +                           +mhits_tpc(mcptr((irow-1)*4+kwir)).x(2)
                       vec(4)=vec(4)
     +                           +mhits_tpc(mcptr((irow-1)*4+kwir)).x(3)
                       mchit_q(nmchit)=mchit_q(nmchit)
     +                       +1.e06*mhits_tpc(mcptr((irow-1)*4+kwir)).de
                       wire_hits = wire_hits + 1
                    endif
                 enddo
                 if(wire_hits.gt.0)then
                    vec(2)=vec(2)/float(wire_hits)
                    vec(3)=vec(3)/float(wire_hits)
                    vec(4)=vec(4)/float(wire_hits)
                 endif
c                if(irow .le. pad_plane(1).nrow_in)then
                 if(irow .le. 13)then
                    mchit_q(nmchit)=mchit_q(nmchit)*tsspar(1).scale_in
                 else
                    mchit_q(nmchit)=mchit_q(nmchit)*tsspar(1).scale_out
                 endif
                 
                 if(dtptr(irow).ne.0)then
                    vec(5) = tphit(dtptr(irow)).x
                    vec(6) = tphit(dtptr(irow)).y
                    vec(7) = tphit(dtptr(irow)).z
                    vec(8) = vec(2)-vec(5)
                    vec(9) = vec(3)-vec(6)
                    vec(10) = vec(4)-vec(7)
                 else
                    vec(5) = 0.
                    vec(6) = 0.
                    vec(7) = 0.
                    vec(8) = 0.
                    vec(9) = 0.
                    vec(10) = 0.
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

        call hcdir('//ntup',' ')
        call hrend('ntup')
        close(23)

        return
        end


C----------------------------------------------------------------------
