* $Id: tfs_g2t.F,v 1.8 1998/12/13 21:36:12 sakrejda Exp $
* $Log: tfs_g2t.F,v $
* Revision 1.8  1998/12/13 21:36:12  sakrejda
* parameters and constants cleaned up, bmpar removed
*
* Revision 1.7  1998/12/03 23:03:56  sakrejda
* Akio's changes to create pileup events added
*
* Revision 1.6  1998/04/23 19:12:00  sakrejda
* modified to print out an estimate of needed tphit size if the structure is not big enough
*
* Revision 1.5  1998/04/09 22:59:40  hardtke
* change write statement to message call
*
* Revision 1.4  1998/04/07 20:56:30  hardtke
* add error message if tphit table too small
*
* Revision 1.3  1998/04/02 20:53:39  sakrejda
* message calls initialised
*
* Revision 1.2  1998/01/27 01:47:21  fisyak
* Split sources
*
      INTEGER FUNCTION TFS_G2T(  g2t_hit_h,       g2t_hit,
     >                           g2t_track_h,     g2t_track,
     >                           g2t_vertex_h,    g2t_vertex,
     >                           par_h,            par,
     >                           ctrl_h,           ctrl,
     >                           tpc_index_h,      tpc_index,
     >                           tpc_index_type_h, tpc_index_type,
     >                           tphit_h,          tphit)
C=======================================================================
C
C                               *
C                              ***
C                             ** **
C                            **   **
C                    *********     *********
C                      ****           ****
C                        ***         ***
C                        **     *     **
C                       **   *******    **
C                      **  ***     ***  **
C                     **                 **
C
C=======================================================================
C23456789012345678901234567890123456789012345678901234567890123456789012
      IMPLICIT NONE
C
C A history of modifications starts today 04-10-97
C
C A bug in TFS_HIT_SMEAR was spotted by Bill Love and fixd following
C his advice. The error was set to err_pad instead of st_dev_xy. 
C-----------------------------------------------------------------------
C     Input Arguments:
C       g2t_hit_h        = header for g2t g2t_tpc_hit table
C       g2t_hit         = rows of g2t g2t_tpc_hit table
C       g2t_track_h      = header for g2t g2t_track table
C       g2t_track       = rows of g2t_track table
C       par_h            = header for TFS fspar table
C       par             = rows of TFS fspar table
C       ctrl_h           = header for TFS ctrl table
C       ctrl            = rows of TFS ctrl table
C       tpc_index_h      = header of TPC INDEX table  
C       tpc_index       = rows of TPC INDEX table
C       tpc_index_type_h = header of TPC INDEX TYPE table
C       tpc_index_type  = rows of TPC INDEX TYPE table
C
C     Output Arguments:
C       values in tphit table
C       tphit_h          = header of TCL tphit table
C       tphit           = rows of TCL tphit table
C
C     Functional Description:
C       The TPC fast simulator modified to use the new GSTAR tables. This TAS
C       module parameterizes the response of the STAR central TPC.
C       Not all the functionality of the original program could be
C       preserved in this implementation. The headers for the
C       subroutines called by this function contain more details
C       about the functionlity of this module.
C
C     Author:
C       Peter G. Jones  LBL  (510)-486-5436
C
C     Creation Date:
C       04-Mar-1993
C       Copyright 1993    Lawrence Berkeley Laboratory
C
C     Modified:
C	     By		     	      Date	 	  History
C           ----                      ----                -------
C     Iwona Sakrejda(LBL) 	   02/18/1996      To use the GSTAR tables
C     Dhammika Weerasundara(UW)    03/27/1996      
C     Dhammika W.                  12/18/1996      Protect against no geant
C                                                  hits in TPC.
C     Dhammika W.                  04/16/1997      fix a bug in tpc_index
C                                                  table updating in 
C                                                  tfs_tpchit_merge.
C     Akio O.                      10/29/1998      modify for high L pp.

C-----------------------------------------------------------------------

#include "tfs_g2t.inc"

C Local variables

      LOGICAL   NEW_EVENT, EVENT_FLAG, LAST_EVENT
      logical   new_xing, goloop, errflg
      INTEGER   NEVENT_OUT, NOK
      integer   ind, ierr, iev, nevent,i
      REAL      TIME_NOW, TIME_RD, TIME_POINT, TIME_DISPL
      REAL      TIME_MERGE, TIME_ALL, TIME_SORT
      character*132 m132       ! for messages
      integer idI1
      data idI1 / 0 /
      save idI1
      real      seed
      integer   iseed
      equivalence (iseed,seed)

C=========================== Begin Executable Code =======================

C     Protect against no Geant hits in the chamber.  DSW  12/18/96
      if ( g2t_hit_h.nok.le.0 )then
         write (6,'(A)')' No Geant hits in TPC'
         tfs_g2t = STAFCV_BAD
         goto 999
      endif
      call Msg_Enable( 'TFS-MERGE-I1' )
      call Msg_Enable( 'TFS-MERGE-I2' )
      call Msg_Enable( 'TFS-MERGE-I3' )
      call Msg_Enable( 'TFS-G2T-I1' )
C     Initialize tpc_index_h.nok to zero. This is valid  for the fast
C     simulator chain only. It is assumed that TFS is the first PAM
C     to be called in the fast simulator chain discounting geant and g2t.
C     DSW  03/13/97
      tpc_index_h.nok = 0

C     initialize the random number generator if needed
      if(ctrl(1).rndm.gt.0) then
         iseed=12345
         call rdmin(seed)
      endif
C     start the clock
      CALL TIMED(TIME_NOW)

C     initialise the recdig in the mc table
C     this is essential for the evaluation
CCCC      do i=1,g2t_hit_h.nok
CCCC         g2t_hit(i).recdig = 0
CCCC      end do

C     initialize
      if( ctrl(1).init .eq. 0 ) then
         CALL TFS_INI( par_h, par, ctrl_h, ctrl, errflg )
         if (errflg)then
            tfs_g2t = STAFCV_BAD
            goto 999
         endif
         NEVENT_OUT = 0
         TIME_RD    = 0.
         TIME_SORT  = 0.
         TIME_POINT = 0.
         TIME_DISPL = 0.
         TIME_MERGE = 0.
         TIME_ALL   = 0.
         new_event = .true.
         new_xing  = .true.
         ctrl(1).init = 1
C
C        obsolete in STAF
C        ctrl_h.mod_count = ctrl_h.mod_count + 1
C
      endif

C     book-keeping
      if( new_event ) then
         iev = 0                ! sub-event counter
         nok = 0                ! nok rows in tphit table
         new_event = .false.
      endif

C     (re)set running count of OK rows in tphit table
      if( ctrl(1).evmerg .ne. 1 ) tphit_h.nok = nok

C     update current beam crossing
      if( new_xing ) then
         iev = 0
         nevent = 1
         new_xing = .false.
      endif
      
C     read-in the Monte-Carlo hit data
      if( nevent.gt.0 ) then    ! may be zero for last xing
         iev = iev + 1
         CALL TFS_G2T_HIT_RD( g2t_hit_h, g2t_hit, 
     >        g2t_track_h, g2t_track, 
     >        ctrl_h, ctrl,par_h, par, tphit_h, tphit,
     >        tpc_index_h, tpc_index, tpc_index_type_h,
     >        tpc_index_type,ierr)
         If (ierr.gt.0) then
          write(m132,*) 'TFS-G2T-I1 tphit table too small, needs',
     >    ierr
          call Message (m132, 1, idI1 )
C          write(6,*) 'TFS-G2T-I1 tphit table too small'
*          tfs_g2t = STAFCV_BAD
         endif
         CALL TIMED(TIME_NOW)
         TIME_ALL = TIME_ALL + TIME_NOW
         TIME_RD = TIME_RD + TIME_NOW
      endif
      
C     
C     check current sub-event (may be more than one per beam crossing)
      if( iev .eq. nevent ) new_xing = .true.
      
C     abort TAS event loop if not yet done
      if( ctrl(1).bipass .ne. 0 ) then
C     bipass any processing of this event
         tfs_g2t = STAFCV_OK
         new_event = .true.
      elseif( iev .eq. nevent ) then
C     sort according to sector-row and set up pointer index
         CALL TFS_HIT_SORT( tphit_h, tphit )
         CALL TIMED(TIME_NOW)
         TIME_ALL = TIME_ALL + TIME_NOW
         TIME_SORT = TIME_SORT + TIME_NOW
         
C     generate pad response and spatial resolutions
         CALL TFS_GEN_PADRES(par_h, par,  tphit_h, tphit )
         CALL TIMED(TIME_NOW)
         TIME_ALL = TIME_ALL + TIME_NOW
         TIME_POINT = TIME_POINT + TIME_NOW
         
C     displace overlapping events  ---1998.10.29-akio
         CALL TFS_DISPL_EVENT2(par_h,par,tphit_h,tphit,
     +        tpc_index_h,tpc_index,g2t_hit_h,g2t_hit,
     +        g2t_track_h,g2t_track,g2t_vertex_h,g2t_vertex)
         CALL TIMED(TIME_NOW)
         TIME_ALL = TIME_ALL + TIME_NOW
         TIME_DISPL = TIME_DISPL + TIME_NOW

C     Merge hits that are close in space
         CALL TFS_TPCHIT_MERGE( ctrl_h, ctrl, par_h, par,
     >        g2t_hit_h, g2t_hit, tphit_h, tphit,tpc_index_h,
     >        tpc_index,
     >        tpc_index_type_h, tpc_index_type )
         CALL TIMED(TIME_NOW)
         TIME_ALL = TIME_ALL + TIME_NOW
         TIME_MERGE = TIME_MERGE + TIME_NOW
         
C     Increment counter and check if done
         NEVENT_OUT = NEVENT_OUT + 1
         CALL TIMED(TIME_NOW)
         TIME_ALL = TIME_ALL + TIME_NOW
         
         if( ctrl(1).print .ge. 1 ) then
            WRITE(6,*) ' EVENTS PROCESSED: ',NEVENT_OUT
            WRITE(6,*) ' TIME_ALL:   ',TIME_ALL,' CPU seconds'
            WRITE(6,*) ' TIME_RD:    ',TIME_RD,' CPU seconds'
            WRITE(6,*) ' TIME_SORT : ',TIME_SORT,' CPU seconds'
            WRITE(6,*) ' TIME_POINT: ',TIME_POINT,' CPU seconds'
            WRITE(6,*) ' TIME_DISPL: ',TIME_DISPL,' CPU seconds'
            WRITE(6,*) ' TIME_MERGE: ',TIME_MERGE,' CPU seconds'
         endif
         
C     write out this event - set flag for new event
         if( nevent_out .eq. ctrl(1).nevent ) then
            tfs_g2t = STAFCV_BAD
         else
            tfs_g2t = STAFCV_OK
         endif
         new_event = .true.
      else
C     keep track of rows, but return zero
         nok = tphit_h.nok
         tphit_h.nok = 0
         tfs_g2t = STAFCV_BAD
      endif
      
 999  Return
      End
