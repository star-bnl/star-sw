CC:>--------------------------------------------------------------------
CC: FILE:       dst_point_filler.F
CC: HISTORY:
CC:             Created on August 20, 1998
CC:             Modified on June 4, 1999 to load all space points - R.L.Ray
CC:             Modified on Nov 25, 1999 
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION DST_POINT_FILLER(
     1         tcl_tphit_h,         tcl_tphit ,
     2           scs_spt_h,           scs_spt ,
     3         dst_point_h,         dst_point ) 
      IMPLICIT NONE
#include "dst_point_filler.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    DST_POINT_FILLER
CC: DESCRIPTION:
CC:             This module reads thru the TPC space point table,
CC:             tcl_tphit and the SVT space point table, scs_spt,
CC:             and collects the hardware position, coordinate
CC:             positions and errors, the charge deposition and
CC:             errors, and the foreign key to the dst_track
CC:             table and loads this information into the DST
CC:             Space Point Table, dst_point.
CC:             Compression algorithms are used to load these
CC:             quantities into the dst_point variables.
CC:             In this new version of this module all space
CC:             points are loaded into dst_point.
CC:
CC: AUTHOR:     Lanny Ray - ray@physics.utexas.edu -- 8/20/98
CC: ARGUMENTS:
CC:          IN:
CC:        tcl_tphit     - TPC space point table
CC:      tcl_tphit_h     - Header Structure for tcl_tphit
CC:          scs_spt     - SVT space point table
CC:        scs_spt_h     - Header Structure for scs_spt
CC:       INOUT:
CC:         OUT:
CC:        dst_point     - DST level space point table
CC:      dst_point_h     - Header Structure for dst_point
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

CC:  Local Variable Type Declarations:

      integer*4 i,detid_tpc,detid_svt,detid_ftpc,detid_ssd
      integer*4 two4,two5,two6,two9,two10,two13,two15,two16,two17
      integer*4 two20,two21,two22,two23,two24,two25,two26,two28,two30
      integer*4 tpcx,tpcy,tpcz,tpcy10,tpcy11,tpcq,tpcdq
      integer*4 svtx,svty,svtz,svty10,svty11,svtq,svtdq
      integer*4 iwaf,ilad,numwaf
      integer*4 istripn,istripp,icsizen,icsizep

      parameter(detid_tpc  = 1)
      parameter(detid_svt  = 2)
      parameter(detid_ftpc = 3)
      parameter(detid_ssd  = 8)

      real*4    tpc_fac, svt_fac, ssd_fac
      parameter(tpc_fac = 2380.0)
      parameter(svt_fac = 23800.)
      parameter(ssd_fac = 16000.)

      parameter(two4  = 16)
      parameter(two5  = 32)
      parameter(two6  = 64)
      parameter(two9  = 512)
      parameter(two10 = 1024)
      parameter(two13 = 8192)
      parameter(two15 = 32768)
      parameter(two16 = 65536)
      parameter(two17 = 131072)
      parameter(two20 = 1048576)
      parameter(two21 = 2097152)
      parameter(two22 = 4194304)
      parameter(two23 = 8388608)
      parameter(two24 = 16777216)
      parameter(two25 = 33554432)
      parameter(two26 = 67108864)
      parameter(two28 = 268435456)
      parameter(two30 = 1073741824)

CC:  Set dst_point number of used rows to zero:

      dst_point_h.nok = 0

CC:  Loop thru TPC space points; store all points

      if(tcl_tphit_h.nok .gt. 0 .and. dst_point_h.maxlen .ge. 1) then
         do i = 1,tcl_tphit_h.nok
            if(dst_point_h.nok .lt. dst_point_h.maxlen) then
               dst_point_h.nok = dst_point_h.nok + 1

CC:  Fill hardware position for hit>>>

               dst_point(dst_point_h.nok).hw_position =
     1            detid_tpc 
     2            + two4*int(tcl_tphit(i).row/100)
     3            + two9*mod(tcl_tphit(i).row,100)
     4            + two15*tcl_tphit(i).npads
     5            + two22*tcl_tphit(i).ntmbk
        

CC:  Fill space point position coordinates in {x,y,z}>>>

               if(tcl_tphit(i).x .gt. (-220.0) .and.
     1            tcl_tphit(i).x .lt. 220.0) then
                  tpcx = int(tpc_fac*(tcl_tphit(i).x + 220.0))
               else
                  tpcx = 0
               end if
               if(tcl_tphit(i).y .gt. (-220.0) .and.
     1            tcl_tphit(i).y .lt. 220.0) then
                  tpcy = int(tpc_fac*(tcl_tphit(i).y + 220.0))
               else
                  tpcy = 0
               end if
               if(tcl_tphit(i).z .gt. (-220.0) .and.
     1            tcl_tphit(i).z .lt. 220.0) then
                  tpcz = int(tpc_fac*(tcl_tphit(i).z + 220.0))
               else
                  tpcz = 0
               end if
               tpcy10 = int(tpcy/two10)
               tpcy11 = tpcy - two10*tpcy10
               dst_point(dst_point_h.nok).position(1) =
     1            tpcx + two20*tpcy11
               dst_point(dst_point_h.nok).position(2) =
     1            tpcy10 + two10*tpcz

CC:  Fill space point position errors>>>

               if(tcl_tphit(i).dx .ge. 0.0 .and.
     1            tcl_tphit(i).dx .lt. 8.0) then
                  tpcx = int(two17*tcl_tphit(i).dx)
               else
                  tpcx = 0
               end if
               if(tcl_tphit(i).dy .ge. 0.0 .and.
     1            tcl_tphit(i).dy .lt. 8.0) then
                  tpcy = int(two17*tcl_tphit(i).dy)
               else
                  tpcy = 0
               end if
               if(tcl_tphit(i).dz .ge. 0.0 .and.
     1            tcl_tphit(i).dz .lt. 8.0) then
                  tpcz = int(two17*tcl_tphit(i).dz)
               else
                  tpcz = 0
               end if
               tpcy10 = int(tpcy/two10)
               tpcy11 = tpcy - two10*tpcy10
               dst_point(dst_point_h.nok).pos_err(1) =
     1            tpcx + two20*tpcy11
               dst_point(dst_point_h.nok).pos_err(2) =
     1            tpcy10 + two10*tpcz

CC:  Fill charge deposition and errors>>>

               if(tcl_tphit(i).q .ge. 0.0 .and. tcl_tphit(i).q
     1            .lt. (1.0/float(two9))) then
                  tpcq = int(two25*tcl_tphit(i).q)
               else
                  tpcq = 0
               end if
               if(tcl_tphit(i).dq .ge. 0.0 .and. tcl_tphit(i).dq
     1            .lt. (1.0/float(two9))) then
                  tpcdq = int(two24*tcl_tphit(i).dq)
               else
                  tpcdq = 0
               end if
               dst_point(dst_point_h.nok).charge = 
     1            tpcq + two16*tpcdq

CC:  Fill Foreign Key to dst_track table>>>

               dst_point(dst_point_h.nok).id_track = 
     1            tcl_tphit(i).id_globtrk

            else if (dst_point_h.nok .ge. dst_point_h.maxlen) then
               write(6,10) dst_point_h.nok
10             Format(2x,'Number DST POINT entries = ',I10,
     1                ', which is .ge. MAXLEN')
               DST_POINT_FILLER = STAFCV_BAD
               Return
            end if
         end do
      end if        ! END TPC SPACE POINT FILLING LOOP




CC:  Loop thru SVT and SSD space points; store all points.
      if(scs_spt_h.nok .gt. 0 .and. dst_point_h.maxlen .ge. 1) then
         do i = 1,scs_spt_h.nok
            if(dst_point_h.nok .lt. dst_point_h.maxlen) then
               dst_point_h.nok = dst_point_h.nok + 1

CC:  Fill hardware position for hit>>>
CC:            Following line checks for SSD hits:
               if(scs_spt(i).id_wafer .lt. 7000) then ! svt hits
                  if (scs_spt(i).id_wafer .lt. two28) then
                     dst_point(dst_point_h.nok).hw_position =
     1                    detid_svt 
     2                    + two4*scs_spt(i).id_wafer
                  else
                     dst_point(dst_point_h.nok).hw_position =
     1                    detid_svt
                  endif
               else             ! ssd hits
                  if (scs_spt(i).id_wafer .lt. two28) then
CC : lm dec-21-1999
CC : we should transform id_wafer(7101-8620) in to numwaf(0-319)
CC : the other information is hardwired for debugging purposes
CC : they will be read from the SSD cluster table
CC : the current coding scheme of hw_position is the following
CC : bits 0-3   detector ID number 
CC : bits 4-12  num_wafer (0-319) 
CC : bits 13-22 central strip of the n-side cluster (1-768) 
CC : bits 23-27 strip of the p-side cluster relat. to n-side (-15,+16) 
CC : bits 28-29 n-side cluster size (1-4) 
CC : bits 30-31 p-side cluster size (1-4)
CC : this scheme will changed once the num_wafer is removed
                  iwaf=(scs_spt(i).id_wafer-7000)/100
                  ilad=scs_spt(i).id_wafer-7000-iwaf*100
                  numwaf=(ilad-1)*16+iwaf-1
                  istripn=500
                  istripp=0
                  icsizen=1
                  icsizep=1
                  dst_point(dst_point_h.nok).hw_position =
     1               detid_ssd
     2                 + two4*numwaf
     3                 + two13*istripn
     4                 + two23*istripp
     5                 + two28*icsizen
     6                 + two30*icsizep

                  else
                     dst_point(dst_point_h.nok).hw_position =
     1                    detid_ssd
                  endif
               end if

CC:  Fill space point position coordinates in {x,y,z}>>>
CC: the coordinates of the SVT hits should always be <22cm
CC: the coordinates of the SSD hits should always be <40cm
CC: the packing factor is different for the SSD (ssd_fac)
               if(scs_spt(i).id_wafer .lt. 7000) then ! svt hits
                  if(scs_spt(i).x(1) .gt. (-22.0) .and.
     1                 scs_spt(i).x(1) .lt. 22.0) then
                     svtx = int(svt_fac*(scs_spt(i).x(1) + 22.0))
                  else
                     svtx = 0
                  end if
                  if(scs_spt(i).x(2) .gt. (-22.0) .and.
     1                 scs_spt(i).x(2) .lt. 22.0) then
                     svty = int(svt_fac*(scs_spt(i).x(2) + 22.0))
                  else
                     svty = 0
                  end if
                  if(scs_spt(i).x(3) .gt. (-22.0) .and.
     1                 scs_spt(i).x(3) .lt. 22.0) then
                     svtz = int(svt_fac*(scs_spt(i).x(3) + 22.0))
                  else
                     svtz = 0
                  end if
               else  ! ssd hits
                  if(scs_spt(i).x(1) .gt. (-40.0) .and.
     1                 scs_spt(i).x(1) .lt. 40.0) then
                     svtx = int(ssd_fac*(scs_spt(i).x(1) + 40.0))
                  else
                     svtx = 0
                  end if
                  if(scs_spt(i).x(2) .gt. (-40.0) .and.
     1                 scs_spt(i).x(2) .lt. 40.0) then
                     svty = int(ssd_fac*(scs_spt(i).x(2) + 40.0))
                  else
                     svty = 0
                  end if
                  if(scs_spt(i).x(3) .gt. (-40.0) .and.
     1                 scs_spt(i).x(3) .lt. 40.0) then
                     svtz = int(ssd_fac*(scs_spt(i).x(3) + 40.0))
                  else
                     svtz = 0
                  end if
               end if
               svty10 = int(svty/two10)
               svty11 = svty - two10*svty10
               dst_point(dst_point_h.nok).position(1) =
     1            svtx + two20*svty11
               dst_point(dst_point_h.nok).position(2) =
     1            svty10 + two10*svtz

CC:  Fill space point position errors>>>

               if(scs_spt(i).cov(1) .ge. 0.0 .and.
     1            scs_spt(i).cov(1) .lt. (1.0/float(two6))) then
                  svtx = int(two26*scs_spt(i).cov(1))
               else
                  svtx = 0
               end if
               if(scs_spt(i).cov(2) .ge. 0.0 .and.
     1            scs_spt(i).cov(2) .lt. (1.0/float(two6))) then
                  svty = int(two26*scs_spt(i).cov(2))
               else
                  svty = 0
               end if
               if(scs_spt(i).cov(3) .ge. 0.0 .and.
     1            scs_spt(i).cov(3) .lt. (1.0/float(two6))) then
                  svtz = int(two26*scs_spt(i).cov(3))
               else
                  svtz = 0
               end if
               svty10 = int(svty/two10)
               svty11 = svty - two10*svty10
               dst_point(dst_point_h.nok).pos_err(1) =
     1            svtx + two20*svty11
               dst_point(dst_point_h.nok).pos_err(2) =
     1            svty10 + two10*svtz

CC:  Fill charge deposition and errors>>>

               if(scs_spt(i).de(1).ge.0.0 .and. scs_spt(i).de(1)
     1            .lt. (1.0/float(two5))) then
                  svtq = int(two21*scs_spt(i).de(1))
               else
                  svtq = 0
               end if
               if(scs_spt(i).de(2).ge.0.0 .and. scs_spt(i).de(2)
     1            .lt. (1.0/float(two5))) then
                  svtdq = int(two20*scs_spt(i).de(2))
               else
                  svtdq = 0
               end if
               dst_point(dst_point_h.nok).charge = 
     1            svtq + two16*svtdq

CC:  Fill Foreign Key to dst_track table>>>

               dst_point(dst_point_h.nok).id_track = 
     1            scs_spt(i).id_globtrk

            else if (dst_point_h.nok .ge. dst_point_h.maxlen) then
               write(6,10) dst_point_h.nok
               DST_POINT_FILLER = STAFCV_BAD
               Return
            end if
         end do
      end if        ! END SVT AND SSD SPACE POINT FILLING LOOP

CC:  Successful completion of module:

      DST_POINT_FILLER=STAFCV_OK
      RETURN
      END
