CC:>--------------------------------------------------------------------
CC: FILE:       vpv.F.template
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION VPV(
     1            vpar_h,                vpar ,
     2            vraw_h,                vraw ,
     4            vver_h,                vver ) 
      IMPLICIT NONE
#include "vpv.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    VPV
CC: DESCRIPTION: Simulates VPD vertex locator.
CC:        
CC:        
CC:        
CC:           
CC:            
CC: AUTHOR:     zm  - Z. Milosevich, milosevich@cmchem.chem.cmu.edu
CC: ARGUMENTS:
CC:          IN:
CC:       INOUT:
CC:           vpar        - parameter settings vps_par table
CC:         vpar_h        - Header Structure for vps_par
CC:           vraw        - vpd raw data table from vps
CC:         vraw_h        - Header Structure for vps raw data table
CC:           vver        - determined z position of vertex
CC:         vver_h        - Header Structure for vpv_ver
CC:         OUT:
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------
      INTEGER ihit, test, nok
      INTEGER ierr

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     =====================
      INTEGER I,J,K

*
*
*     npmt       is the total number of photo-tubes
*     pmt         labels phototubes, inner ring first, then outer
*                 ring, 1,n_pmt/2 eta<0 n_pmt/2 +1,n_pmt for eta>0
*
      INTEGER npmt, ipmt
*
*     variables associated with time response
*
*     neg_t      minimum time associated with VPD eta < 0
*     pos_t      minimum time associated with VPD eta > 0
*
      REAL neg_t, pos_t, nt, pt, time, mtime

      REAL time_difference, time_sum, distance, vertex
C
C     Data Statements:
C     ================
C
C     Executable Statements:
C=======================================================================
C
      if ( vpar(1).n_pmt .eq. 0 ) vpar(1).n_pmt = 48
      if ( vpar(1).max_time .eq. 0 ) vpar(1).max_time = 100e-9
      if ( vpar(1).dist .eq. 0 ) vpar(1).dist = 600.
      npmt    = vpar(1).n_pmt
      mtime   = vpar(1).max_time

      vver_h.nok = 0
*
      vver_h.nok = vver_h.nok + 1
*    
*     ----------------------------------------------------
*     fill table
*     ----------------------------------------------------
*       
      neg_t = mtime
      pos_t = mtime
      nt    = 0
      pt    = 0

      if ( vraw_h.nok .gt. 0 ) then
         do i = 1, vraw_h.nok
*    
            ipmt = vraw(i).pmt
            time = vraw(i).vpd_tdc 
            if(time.eq.0)time = mtime
            if(ipmt.le.npmt/2)then
                nt    = time
                neg_t = min(neg_t,nt)
            else
                pt    = time
                pos_t = min(pos_t,pt)
            endif
*              
         end do
      else
         vpv = STAFCV_BAD
         return
      endif  
*      
      time_difference =     neg_t - pos_t
      time_sum        =     neg_t + pos_t
      distance        =     vpar(1).dist
*
      vertex = 0.5*distance*time_difference/time_sum

      if(vpar(1).dist .eq. 0) then
*         print*,'***************************************************** '
*         print*,'* You need to enter a value for vpd_dst(1).dist !!! * '
*         print*,'*                                                   * '
*         print*,'* vpd_dst(1).dist is the distance between           * '
*         print*,'* the two VPDs in centimeters !!!!!                 * '
*         print*,'***************************************************** '
         vpv = STAFCV_BAD
         return
      endif
         
      if(vpar(1).dist .le. 100) then
*         print*,'***************************************************** '
*         print*,'* You have entered a value for vpd_dst(1).dist<100  * '
*         print*,'*                                                   * '
*         print*,'* Restart with a value for vpd_dst(1).dist in       * '
*         print*,'* units of centimeters !!!!!                        * '
*         print*,'***************************************************** '
         vpv = STAFCV_BAD
         return
      endif

      if(neg_t.ge.1000.or.pos_t.ge.1000)then  !no hits in one of VPD's
*         print*,' ************************************************ '
*         print*,' * No hit in one of the VPDs!                   * '
*         print*,' ************************************************ '
         vpv = STAFCV_BAD
         return
      endif

      if(time_sum.le.0)then
*        print*,' ************************************************ '
*        print*,' * Sum of times is zero (or less)!              * '
*        print*,' ************************************************ '
        vpv = STAFCV_BAD
        return
      endif   

*    
*     -----------------------------------------------------------
*     fill table
*     -----------------------------------------------------------
*       
      vver(vver_h.nok).id           = vver_h.nok
      vver(vver_h.nok).neg_time     = neg_t
      vver(vver_h.nok).pos_time     = pos_t    
      vver(vver_h.nok).vert         = vertex
*
      VPV=STAFCV_OK
999   RETURN
      END

C=======================================================================
