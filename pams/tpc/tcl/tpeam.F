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
     +,                       mhits_tpch, mhits_tpc )

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

C-----------------------------------------------------------------------
      RECORD/ table_head_st/ tphith
      RECORD/ tphit_row_st/ tphit(*)

      RECORD/ table_head_st/ mctrlh
      RECORD/ mctrl_row_st/ mctrl(*)
      RECORD/ table_head_st/ mkineh
      RECORD/ mkine_row_st/ mkine(*)
      RECORD/ table_head_st/ mhits_tpch
      RECORD/ mhits_tpc_row_st/ mhits_tpc(*)

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

         integer nquota,nvar,icycle,nvar2
         character*4 chtags(9),chtags2(11)
C-----------------------------------------------------------------------
         logical lstat,first

         real vec(11)

         integer istat
         integer indx, ihit
         integer isect,irow,iwire,nseq,numbv

         integer mxhits
         parameter (mxhits = 45)
         integer mcptr(4*mxhits),dtptr(mxhits)
         
         integer nmctrk,nhitrk,itrk
         character*80 outfile
         integer iper,ihisto

C-----------------------------------------------------------------------
         data nvar/9/,nvar2/11/
         data chtags/'itrk','itcl','nrow','x','y','z','prf','zrf','q'/
         data chtags2/'itrk','mcx ','mcy ','mcz ','tpx ','tpy ','tpz '
     +,              'delx','dely','delz','nrow'/
         data first/.true./,ihisto/0/
C-----------------------------------------------------------------------
c
c        set status
c
         tpeam = tucod_ok

         if(first)then
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

         ihisto = ihisto+1
         call hbookn(200+ihisto,'Hit Info',nvar,'ntup',8000,
     +           chtags)
         call hbookn(300+ihisto,'Compare Info',nvar2,'ntup',11000,
     +              chtags2)
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
           nmctrk = 0
           call vzero(dtptr,45)
           call vzero(mcptr,180)
           do ihit = 1, tphith.nok
              if(tphit(ihit).track.eq.itrk)then
                 nhitrk = nhitrk + 1
                 dtptr(tphit(ihit).row) = ihit
              endif
           enddo
           write(6,*)' ',nhitrk,' tphits on itrk = ',itrk
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
           write(6,*)' ',nmctrk,' mchits on itrk = ',itrk
           do irow = 1, 45
              vec(11) = real(irow)
              vec(1) = itrk
              vec(2) = (mhits_tpc(mcptr((irow-1)*4+1)).x(1)
     +                +mhits_tpc(mcptr((irow-1)*4+2)).x(1)
     +                +mhits_tpc(mcptr((irow-1)*4+3)).x(1)
     +                +mhits_tpc(mcptr((irow-1)*4+4)).x(1))/4.
              vec(3) = (mhits_tpc(mcptr((irow-1)*4+1)).x(2)
     +                +mhits_tpc(mcptr((irow-1)*4+2)).x(2)
     +                +mhits_tpc(mcptr((irow-1)*4+3)).x(2)
     +                +mhits_tpc(mcptr((irow-1)*4+4)).x(2))/4.
              vec(4) = (mhits_tpc(mcptr((irow-1)*4+1)).x(3)
     +                +mhits_tpc(mcptr((irow-1)*4+2)).x(3)
     +                +mhits_tpc(mcptr((irow-1)*4+3)).x(3)
     +                +mhits_tpc(mcptr((irow-1)*4+4)).x(3))/4.
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
           enddo
         enddo
         call hrout(300+ihisto,icycle,' ')
c        call hrend('ntup')
c        close(23)

c
         return
         end




