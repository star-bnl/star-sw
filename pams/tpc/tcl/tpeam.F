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

#include "mct_mkine_pars.inc"
#include "mct_mkine_st.inc"
#include "mct_mhits_tpc_pars.inc"
#include "mct_mhits_tpc_st.inc"

C-----------------------------------------------------------------------
      RECORD/ table_head_st/ tphith
      RECORD/ tphit_row_st/ tphit(*)

      RECORD/ table_head_st/ mkineh
      RECORD/ mkine_row_st/ mkine(*)
      RECORD/ table_head_st/ mhits_tpch
      RECORD/ mhits_tpc_row_st/ mhits_tpc(*)

C-----------------------------------------------------------------------
c
c        functions
c
C-----------------------------------------------------------------------
c
c        HBOOK variables...
c
         common/quest/iquest
         integer iquest(100)

         integer nquota,nvar,icycle,nvarp
         character*4 chtags(8)
C-----------------------------------------------------------------------
         logical lstat,first

         real vec(8)

         integer istat
         integer indx, ihit
         integer isect,irow,iwire,nseq,numbv

         integer mxhits
         parameter (mxhits = 45)
         integer mcptr(mxhits),dtptr(mxhits)
         
         integer nmctrk,nhitrk,itrk

C-----------------------------------------------------------------------
         data nvar/8/
         data chtags/'itrk','itcl','nrow','x','y','z','prf','zrf'/
         data first/.true./
C-----------------------------------------------------------------------
c
c        set status
c
         tpeam = tucod_ok

         if(first)then
c
c           open rz file for ntuple...
c
            open(unit=23,file='tpe.rzd',form='unformatted',
     +        recl=1024,access='direct',
     +        status='new',iostat=istat)
            nquota=10000
            if(istat.eq.0)then
               iquest(10)=nquota
               call hrfile(23,'ntup','NQ')
            endif
            call hbookn(102,'Hit Info',nvar,'ntup',8000,
     +              chtags)
         endif
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

            call hfn(102,vec)
         enddo
c     
c->      close ntuple file...
c
         call hrout(102,icycle,' ')
         call hrend('ntup')
         close(23)
c
c        the following works only for sparse events!
c
         do itrk = 1, mkineh.nok
           nhitrk = 0
           nmctrk = 0
           call vzero(dtptr,45)
           call vzero(mcptr,45)
           do ihit = 1, tphith.nok
              if(tphit(ihit).track.eq.itrk)then
                 nhitrk = nhitrk + 1
                 dtptr(tphit(ihit).row) = ihit
              endif
           enddo
           do ihit = 1, mhits_tpch.nok
              if(mhits_tpc(ihit).itrk.eq.itrk)then
                 nmctrk = nmctrk + 1
                 numbv = mhits_tpc(ihit).numbv
                 
                 iwire = numbv/10000
                 isect = (numbv-10000*iwire)/100
                 irow = (numbv-10000*iwire-100*isect)

                 mcptr(irow) = ihit
              endif
           enddo

         enddo

c
         return
         end




