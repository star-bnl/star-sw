* $Id: tfs_g2t_hit_rd.F,v 1.4 1998/04/23 19:12:03 sakrejda Exp $
* $Log: tfs_g2t_hit_rd.F,v $
* Revision 1.4  1998/04/23 19:12:03  sakrejda
* modified to print out an estimate of needed tphit size if the structure is not big enough
*
* Revision 1.3  1998/04/07 20:56:32  hardtke
* AZZZZ
* A
* B
* C
* B
* B
* B
*
* A
* A
* B
* B
*
*
* C
* add error message if tphit table too small
*
* Revision 1.2  1998/04/02 20:52:20  sakrejda
* tphit.dq initialised to avoid Infinities
*
* Revision 1.1  1998/01/27 01:47:22  fisyak
* Split sources
*
      SUBROUTINE TFS_G2T_HIT_RD( g2t_hit_h,        g2t_hit,
     >                           g2t_track_h,      g2t_track,
     >                           ctrl_h,           ctrl,
     >                           par_h,            par,
     >                           tphit_h,          tphit,
     >                           ix,              nxtrig,
     >                           tpc_index_h,      tpc_index,
     >                           tpc_index_type_h, tpc_index_type,
     >                           ierr)


      IMPLICIT NONE
C-----------------------------------------------------------------------
C
C     Input arguments:
C       ix = current crossing number
C
C     Output arguments: ierr = 0 all ok, ierr = 1 tphit too small
C
C     Functional Description :
C       Extracts hit information from the tables translation of the
C       GEANT banks.
C
C     Author:
C       Peter G. Jones  LBL  (510)-486-5436
C
C     Creation Date:
C       10-Mar-1993
C
C     Modified:
C	     By		     	      Date	 	  History
C            --                       ----                ------- 
C       Iwona Sakrejda(LBL) 	   02/18/1996      To use the GSTAR tables
C	Dhammika Weerasundara(UW)  03/27/1996      Added TPC_INDEX tables
C       Dhammika W.                04/18/1996      Updated to conform with g2t
C                                                  tpc volume_id.
C                                                  Updated tpc central membrane
C                                                  thickness.
C       David Hardtke              04/02/1998      return error if tphit table
C                                                  is too small
C
C-----------------------------------------------------------------------
#include "PAM.inc"
#include "g2t_tpc_hit.inc"
      RECORD / TABLE_HEAD_ST    / g2t_hit_h
      RECORD / g2t_tpc_hit_st   / g2t_hit(*)
#include "g2t_track.inc"
#include "tfs_fsctrl.inc"
#include "tfs_fspar.inc"
#include "tcl_tphit.inc"
#include "tcl_tpc_index.inc"
#include "tcl_tpc_index_type.inc"
C-----------------------------------------------------------------------

      RECORD / table_head_st        / g2t_track_h
      RECORD / g2t_track_st         / g2t_track(*)
      RECORD / table_head_st        / ctrl_h
      RECORD / tfs_fsctrl_st        / ctrl(*)
      RECORD / table_head_st        / par_h
      RECORD / tfs_fspar_st         / par(*)
      RECORD / table_head_st        / tphit_h
      RECORD / tcl_tphit_st         / tphit(*)
      RECORD / table_head_st        / tpc_index_h
      RECORD / tcl_tpc_index_st     / tpc_index(*)
      RECORD / table_head_st        / tpc_index_type_h
      RECORD / tcl_tpc_index_type_st/ tpc_index_type(*)
C-----------------------------------------------------------------------

      INTEGER MXROW
      PARAMETER (MXROW=99)
      INTEGER MXSECTOR
      PARAMETER (MXSECTOR=24)
      INTEGER IROW(2,MXROW,MXSECTOR)
      INTEGER ICOUNT
      COMMON /TPHPNT/ IROW
C     IROW  - 1 pointer to a row in a sector
C     IROW  - 2 number of hits


C Local variables
      integer ih, jrow, jsect, nok, ix, nxtrig, ierr

      real x, dx, y, dy

C     central membrane width = 25 microns -- Aleph Handbook V.13
C     central membrane width = 76 microns -- TPC geometry proposal by Iwona S.
      real central_membrane
      parameter (central_membrane=0.0076)

C=========================== Begin Executable Code =======================

C     set row counter for tphit
      nok = tphit_h.nok
      ierr = 0
      icount = 0

C     loop over MC hits and extract sector-row information
      do 100 ih = 1, g2t_hit_h.nok


C     In GSTAR/g2t TPC volume id is a packed word which contains,  both, the 
C     physical and pseudo pad-row numbers. 
C     volume_id = 100000*isdet      (idset = 0 ; physical  & = 1,2 ; pseudo)   
C               +    100*n_sector   (n_sector  = 1 - 24 )
C               +        n_pad_row  (n_pad_row = 1 - 45 )
C
C     For physical pad-rows, max volume_id = 100*24 + 45 = 2445. For the TPC 
C     Fast Simulator, reject GEANT hits with voulme_id > 2445.    
C     ---  Dhammika W.  April 18, 1996 ---

         if(g2t_hit(ih).volume_id.gt.2445)goto 100

         if( nok .lt. tphit_h.maxlen ) then

C     increment row counter
            nok = nok + 1
            
            
C     Initialize tphit id/flags for this hit to their default value.
C     DSW 03/13/97
            tphit(nok).flag 	  = 0 
            tphit(nok).id 	  = 0 
            tphit(nok).id_globtrk = 0 
            tphit(nok).track	  = 0 
C     IMS 04/01/98
C     Add initialisation of dq to that
            tphit(nok).dq         = 0 


C     code the crossing number into the quality flag
            if( ctrl(1).ppflag .ne. 0 ) then
               tphit(nok).flag = 100 * ix
               if(ix.eq.nxtrig) then
                  tphit(nok).cluster=nxtrig
               else
                  tphit(nok).cluster=0
               endif
            endif

            if( abs( g2t_hit(ih).x(3) ) .le. central_membrane ) then
C     this hit is in the central membrane
               tphit(nok).flag = tphit(nok).flag + 6
            endif
            
C     a unique pointer for this hit ( offset = 0 for single event )
            tphit(nok).id = nok

CCC     g2t_hit(ih).recdig = ih + tphit_h.nok  
C     >>>use index table<<< ; DSW 03/27/96
C     Store  GEANT hit id in tpc_index table
            tpc_index_h.nok = tpc_index_h.nok+1
            tpc_index(tpc_index_h.nok).type = 
     >           tpc_index_type(1).tphit_mhitstpc
            tpc_index(tpc_index_h.nok).key1 =
     >           g2t_hit(ih).id
            tpc_index(tpc_index_h.nok).key2 = 
     >           tphit(nok).id
            

            tphit(nok).row = g2t_hit(ih).volume_id
            tphit(nok).x   = g2t_hit(ih).x(1)
            tphit(nok).y   = g2t_hit(ih).x(2)
            tphit(nok).z   = g2t_hit(ih).x(3)
            tphit(nok).q   = abs(g2t_hit(ih).de)
            
C     store local momentum in place of position errors
            tphit(nok).dx  = g2t_hit(ih).p(1)
            tphit(nok).dy  = g2t_hit(ih).p(2)
            tphit(nok).dz  = g2t_hit(ih).p(3)
            
C     check event folding flag
            if( ctrl(1).fold .eq. 2 ) then
C     fold two halves of the drift volume
               if( tphit(ih).z .lt. 0. ) then
                  jrow  = mod( tphit(ih).row, 100 )
                  jsect = tphit(ih).row - jrow
                  jsect = jsect - 50*par(1).nsect
                  tphit(ih).row = jsect + jrow
                  tphit(ih).z   = -1. * tphit(ih).z
                  tphit(ih).dz  = -1. * tphit(ih).dz
               endif
            elseif( ctrl(1).fold .eq. 4 ) then
               jrow  = mod( tphit(ih).row, 100 )
               jsect = tphit(ih).row - jrow
               jsect = mod( jsect, 300 )
               if( jsect .eq. 0 ) jsect = 300
               tphit(ih).row = jsect + jrow
C     rotate sectors into one quadrant
               if( tphit(ih).x .lt.0..and.tphit(ih).y.lt.0) then
                  tphit(ih).x  = abs( tphit(ih).x )
                  tphit(ih).dx = -1. * tphit(ih).dx
                  tphit(ih).y  = abs( tphit(ih).y )
                  tphit(ih).dy = -1. * tphit(ih).dy
               elseif ( tphit(ih).x .lt. 0 ) then
                  y  = abs( tphit(ih).x )
                  dy = -1. * tphit(ih).dx
                  x  = tphit(ih).y
                  dx = tphit(ih).dy
                  tphit(ih).x  = x
                  tphit(ih).y  = y
                  tphit(ih).dx = dx
                  tphit(ih).dy = dy
               elseif( tphit(ih).y .lt. 0. ) then
                  x  = abs( tphit(ih).y )
                  dx = -1. * tphit(ih).dy
                  y  = tphit(ih).x
                  dy = tphit(ih).dx
                  tphit(ih).x  = x
                  tphit(ih).y  = y
                  tphit(ih).dx = dx
                  tphit(ih).dy = dy
               endif
            elseif( ctrl(1).fold .eq. 8 ) then
C     fold two havles of the drift volume
               if( tphit(ih).z .lt. 0. ) then
                  tphit(ih).z = -1. * tphit(ih).z
                  tphit(ih).dz = -1. * tphit(ih).dz
               endif
               jrow  = mod( tphit(ih).row, 100 )
               jsect = tphit(ih).row - jrow
               jsect = mod( jsect, 300 )
               if( jsect .eq. 0 ) jsect = 300
               tphit(ih).row = jsect + jrow
C     rotate sectors into one quadrant
               if( tphit(ih).x .lt. 0. .and. tphit(ih).y .lt. 0  ) then 
                  tphit(ih).x  = abs( tphit(ih).x )
                  tphit(ih).dx = -1. * tphit(ih).dx
                  tphit(ih).y  = abs( tphit(ih).y )
                  tphit(ih).dy = -1. * tphit(ih).dy
               elseif ( tphit(ih).x .lt. 0 ) then
                  y  = abs( tphit(ih).x )
                  dy = -1. * tphit(ih).dx
                  x  = tphit(ih).y
                  dx = tphit(ih).dy
                  tphit(ih).x  = x
                  tphit(ih).y  = y
                  tphit(ih).dx = dx
                  tphit(ih).dy = dy
               elseif( tphit(ih).y .lt. 0. ) then
                  x  = abs( tphit(ih).y )
                  dx = -1. * tphit(ih).dy
                  y  = tphit(ih).x
                  dy = tphit(ih).dx
                  tphit(ih).x  = x
                  tphit(ih).y  = y
                  tphit(ih).dx = dx
                  tphit(ih).dy = dy
               endif
            endif
         
         else  
          icount=icount+1            ! not enough space in tphit

         endif                       ! nok < tphit.maxlen
         
 100  enddo

C     set the row counter
      if(icount.ne.0) ierr = nok+icount
      tphit_h.nok = nok

      RETURN
      END
