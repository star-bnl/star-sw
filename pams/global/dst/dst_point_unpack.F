CC:>--------------------------------------------------------------------
CC: FILE:       dst_point_unpack.F
CC: HISTORY:
CC:          Created on August 21, 1998
CC:
CC:           Modified on January 21, 1999 - include FTPC data
CC:                       Janet Seyboth, jcs@mppmu.mpg.de
CC:
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION DST_POINT_UNPACK(
     1         tcl_tphit_h,         tcl_tphit ,
     2           scs_spt_h,           scs_spt ,
     2       fcl_fppoint_h,       fcl_fppoint ,
     3         dst_point_h,         dst_point ) 
      IMPLICIT NONE
#include "dst_point_unpack.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    DST_POINT_UNPACK
CC: DESCRIPTION:
CC:             This routine reads in the DST level space point table,
CC:             dst_point, and uncompresses the information and fills
CC:             the resulting space point information into tcl_tphit
CC:             (for TPC space points) and scs_spt (for SVT space points)
CC:             and fcl_fppoint (for FTPC space points).
CC:             This is intended for POST-DST physics analysis which
CC:             requires space points.
CC:             All unused variables in these latter three tables are
CC:             set to zero; these variables were not saved in the
CC:             DST space point table.
CC: AUTHOR:     Lanny Ray, ray@physics.utexas.edu
CC: ARGUMENTS:
CC:          IN:
CC:        dst_point     - DST space point table
CC:      dst_point_h     - Header Structure for dst_point
CC:       INOUT:
CC:         OUT:
CC:        tcl_tphit     - TPC space point table - for use in post-DST
CC:      tcl_tphit_h     - Header Structure for tcl_tphit
CC:          scs_spt     - SVT space point table - for use in post-DST
CC:        scs_spt_h     - Header Structure for scs_spt
CC:      fcl_fppoint     - FTPC reconstructed space points
CC:    fcl_fppoint_h     - Header Structure for fcl_fppoint
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

CC:  Local Variable Type Declarations:

      integer*4 i,detid,hwsave,a,b,c,pos1,pos2
      integer*4 two4,two5,two6,two9,two10,two11,two12
      integer*4 two16,two17,two18,two20,two21,two24,two25,two26
      integer*4 tpcx,tpcy,tpcz,tpcy10,tpcy11,tpcq,tpcdq
      integer*4 svtx,svty,svtz,svty10,svty11,svtq,svtdq
      integer*4 nbins,npads,ftpcx,ftpcy,ftpcz,ftpcy10,ftpcy11,ftpcq,ftpcdq

      real*4    tpc_fac, svt_fac

      parameter(tpc_fac = 2380.0)
      parameter(svt_fac = 23800.)

      real*4    ftpc_fac, ftpc_length
      parameter(ftpc_fac = 2380.0)
      parameter(ftpc_length = 270.0)

      parameter(two4  = 16)
      parameter(two5  = 32)
      parameter(two6  = 64)
      parameter(two9  = 512)
      parameter(two10 = 1024)
      parameter(two11 = 2048)
      parameter(two12 = 4096)
      parameter(two16 = 65536)
      parameter(two17 = 131072)
      parameter(two18 = 262144)
      parameter(two20 = 1048576)
      parameter(two21 = 2097152)
      parameter(two24 = 16777216)
      parameter(two25 = 33554432)
      parameter(two26 = 67108864)

CC:  Set TPC and SVT and FTPC post-DST space point table # rows to zero:

      tcl_tphit_h.nok = 0
      scs_spt_h.nok   = 0
      fcl_fppoint_h.nok = 0

CC:  Loop thru all space points in dst_point and unpack the information
CC:  and load into tcl_tphit and scs_spt and fcl_fppoint:

      IF (dst_point_h.nok .gt. 0 .and. tcl_tphit_h.maxlen .ge. 1
     1    .and. scs_spt_h.maxlen .ge. 1 
     1    .and. fcl_fppoint_h.maxlen .ge. 1) then
         do i = 1,dst_point_h.nok

CC:  Determine which detector this space point is located in>>>

         hwsave = dst_point(i).hw_position
         if ( hwsave .lt. two25 ) then
            c = hwsave/two20
            b = (hwsave - two20*c)/two4
            a = hwsave - two20*c - two4*b
            detid = a
         else
            nbins = hwsave/two25
            npads = (hwsave - two25*nbins)/two21
            c     = (hwsave - two25*nbins - two21*npads)/two11
            b     = (hwsave - two25*nbins - two21*npads - two11*c)/two4
            detid = hwsave - two25*nbins - two21*npads - two11*c - two4*b
         end if

         if(detid .eq. 1) then      !  Fill TPC space points

            if(tcl_tphit_h.nok .lt. tcl_tphit_h.maxlen) then
               tcl_tphit_h.nok = tcl_tphit_h.nok + 1
               tcl_tphit(tcl_tphit_h.nok).row  = b
               tcl_tphit(tcl_tphit_h.nok).nseq = c
              
               pos1 = dst_point(i).position(1)
               pos2 = dst_point(i).position(2)
               tpcy11 = pos1/two20
               tpcz   = pos2/two10
               tpcx   = pos1 - two20*tpcy11
               tpcy10 = pos2 - two10*tpcz
               tpcy   = tpcy11 + two10*tpcy10
               tcl_tphit(tcl_tphit_h.nok).x = (float(tpcx)/tpc_fac)-220.
               tcl_tphit(tcl_tphit_h.nok).y = (float(tpcy)/tpc_fac)-220.
               tcl_tphit(tcl_tphit_h.nok).z = (float(tpcz)/tpc_fac)-220.

               pos1 = dst_point(i).pos_err(1)
               pos2 = dst_point(i).pos_err(2)
               tpcy11 = pos1/two20
               tpcz   = pos2/two10
               tpcx   = pos1 - two20*tpcy11
               tpcy10 = pos2 - two10*tpcz
               tpcy   = tpcy11 + two10*tpcy10
               tcl_tphit(tcl_tphit_h.nok).dx = float(tpcx)/float(two17)
               tcl_tphit(tcl_tphit_h.nok).dy = float(tpcy)/float(two17)
               tcl_tphit(tcl_tphit_h.nok).dz = float(tpcz)/float(two17)

               tpcdq = dst_point(i).charge/two16
               tpcq  = dst_point(i).charge - two16*tpcdq
               tcl_tphit(tcl_tphit_h.nok).q  = float(tpcq)/float(two25)
               tcl_tphit(tcl_tphit_h.nok).dq  = 0.
               tcl_tphit(tcl_tphit_h.nok).flag = tpcdq

               tcl_tphit(tcl_tphit_h.nok).id_globtrk = 
     1            dst_point(i).id_track

               tcl_tphit(tcl_tphit_h.nok).cluster   = 0
c               tcl_tphit(tcl_tphit_h.nok).flag      = 0
               tcl_tphit(tcl_tphit_h.nok).id        = tcl_tphit_h.nok
               tcl_tphit(tcl_tphit_h.nok).track     =
     1            dst_point(i).id_track
               tcl_tphit(tcl_tphit_h.nok).alpha     = 0.0
               tcl_tphit(tcl_tphit_h.nok).dalpha    = 0.0
               tcl_tphit(tcl_tphit_h.nok).dlambda   = 0.0
               tcl_tphit(tcl_tphit_h.nok).lambda    = 0.0
               tcl_tphit(tcl_tphit_h.nok).phi       = 0.0
               tcl_tphit(tcl_tphit_h.nok).prf       = 0.0
               tcl_tphit(tcl_tphit_h.nok).zrf       = 0.0

            else if(tcl_tphit_h.nok .ge. tcl_tphit_h.maxlen) then
               write(6,20) tcl_tphit_h.nok
20             Format(2x,'Number of TPC space point entries = ',
     1                I10,', which is .ge. MAXLEN')
               DST_POINT_UNPACK = STAFCV_BAD
               Return
            end if

         else if (detid .eq. 2) then   !  Fill SVT space points

            if(scs_spt_h.nok .lt. scs_spt_h.maxlen) then
               scs_spt_h.nok = scs_spt_h.nok + 1
               scs_spt(scs_spt_h.nok).id_wafer  = b
              
               pos1 = dst_point(i).position(1)
               pos2 = dst_point(i).position(2)
               svty11 = pos1/two20
               svtz   = pos2/two10
               svtx   = pos1 - two20*svty11
               svty10 = pos2 - two10*svtz
               svty   = svty11 + two10*svty10
               scs_spt(scs_spt_h.nok).x(1) = (float(svtx)/svt_fac)-22.
               scs_spt(scs_spt_h.nok).x(2) = (float(svty)/svt_fac)-22.
               scs_spt(scs_spt_h.nok).x(3) = (float(svtz)/svt_fac)-22.

               pos1 = dst_point(i).pos_err(1)
               pos2 = dst_point(i).pos_err(2)
               svty11 = pos1/two20
               svtz   = pos2/two10
               svtx   = pos1 - two20*svty11
               svty10 = pos2 - two10*svtz
               svty   = svty11 + two10*svty10
               scs_spt(scs_spt_h.nok).cov(1) = float(svtx)/float(two26)
               scs_spt(scs_spt_h.nok).cov(2) = float(svty)/float(two26)
               scs_spt(scs_spt_h.nok).cov(3) = float(svtz)/float(two26)

               svtdq = dst_point(i).charge/two16
               svtq  = dst_point(i).charge - two16*svtdq
               scs_spt(scs_spt_h.nok).de(1) = float(svtq)/float(two21)
               scs_spt(scs_spt_h.nok).de(2) = 0.
               scs_spt(scs_spt_h.nok).flag = svtdq

               scs_spt(scs_spt_h.nok).id_globtrk = 
     1            dst_point(i).id_track

c               scs_spt(scs_spt_h.nok).flag      = 0
               scs_spt(scs_spt_h.nok).id        = scs_spt_h.nok
               scs_spt(scs_spt_h.nok).id_cluster = 0
               scs_spt(scs_spt_h.nok).id_match   = 0
               scs_spt(scs_spt_h.nok).id_mchit   = 0
               scs_spt(scs_spt_h.nok).id_mctrack = 0
               scs_spt(scs_spt_h.nok).id_track   =
     1            dst_point(i).id_track
               scs_spt(scs_spt_h.nok).res(1)     = 0.0
               scs_spt(scs_spt_h.nok).res(2)     = 0.0
               scs_spt(scs_spt_h.nok).res(3)     = 0.0
               scs_spt(scs_spt_h.nok).xl(1)      = 0.0
               scs_spt(scs_spt_h.nok).xl(2)      = 0.0
               scs_spt(scs_spt_h.nok).xl(3)      = 0.0
               scs_spt(scs_spt_h.nok).mom2(1)    = 0.0
               scs_spt(scs_spt_h.nok).mom2(2)    = 0.0

            else if(scs_spt_h.nok .ge. scs_spt_h.maxlen) then
               write(6,30) scs_spt_h.nok
30             Format(2x,'Number of SVT space point entries = ',
     1                I10,', which is .ge. MAXLEN')
               DST_POINT_UNPACK = STAFCV_BAD
               Return
            end if
  

         else if(detid.eq.4 .or. detid.eq.5) then  !  Fill FTPC space points

            if(fcl_fppoint_h.nok .lt. fcl_fppoint_h.maxlen) then
               fcl_fppoint_h.nok = fcl_fppoint_h.nok + 1
               fcl_fppoint(fcl_fppoint_h.nok).row  = b
               fcl_fppoint(fcl_fppoint_h.nok).sector = c
               fcl_fppoint(fcl_fppoint_h.nok).n_pads = npads
               fcl_fppoint(fcl_fppoint_h.nok).n_bins = nbins

               fcl_fppoint(fcl_fppoint_h.nok).max_adc = 
     1               dst_point(i).charge/two16
               fcl_fppoint(fcl_fppoint_h.nok).charge  = 
     1               dst_point(i).charge 
     2                 - two16*fcl_fppoint(fcl_fppoint_h.nok).max_adc
             
               fcl_fppoint(fcl_fppoint_h.nok).flags      = 0

               pos1 = dst_point(i).position(1)
               pos2 = dst_point(i).position(2)
               ftpcy11 = pos1/two20
               ftpcz   = pos2/two10
               ftpcx   = pos1 - two20*ftpcy11
               ftpcy10 = pos2 - two10*ftpcz
               ftpcy   = ftpcy11 + two10*ftpcy10
               fcl_fppoint(fcl_fppoint_h.nok).x = 
     1              (float(ftpcx)/ftpc_fac)-ftpc_length
               fcl_fppoint(fcl_fppoint_h.nok).y = 
     1              (float(ftpcy)/ftpc_fac)-ftpc_length
               fcl_fppoint(fcl_fppoint_h.nok).z = 
     1              (float(ftpcz)/ftpc_fac)-ftpc_length

               pos1 = dst_point(i).pos_err(1)
               pos2 = dst_point(i).pos_err(2)
               ftpcy11 = pos1/two20
               ftpcz   = pos2/two10
               ftpcx   = pos1 - two20*ftpcy11
               ftpcy10 = pos2 - two10*ftpcz
               ftpcy   = ftpcy11 + two10*ftpcy10
               fcl_fppoint(fcl_fppoint_h.nok).x_err = float(ftpcx)/float(two17)
               fcl_fppoint(fcl_fppoint_h.nok).y_err = float(ftpcy)/float(two17)
               fcl_fppoint(fcl_fppoint_h.nok).z_err = float(ftpcz)/float(two17)


               fcl_fppoint(fcl_fppoint_h.nok).s_phi = 0.0
               fcl_fppoint(fcl_fppoint_h.nok).s_r   = 0.0

            else if(fcl_fppoint_h.nok .ge. fcl_fppoint_h.maxlen) then
               write(6,40) fcl_fppoint_h.nok
40             Format(2x,'Number of FTPC space point entries = ',
     1                I10,', which is .ge. MAXLEN')
               DST_POINT_UNPACK = STAFCV_BAD
               Return
            end if

         Else
            write(6,10) detid
10          Format(2x,'In DST_POINT_UNPACK detid = ',I10,
     1             'is .ne. 1 or 2 - NO UNPACKING DONE')
               DST_POINT_UNPACK = STAFCV_BAD
               Return

         End If
         End Do     !   End loop thru all rows in dst_point
      END IF

CC:  Successful completion of module

      DST_POINT_UNPACK=STAFCV_OK
      RETURN
      END

