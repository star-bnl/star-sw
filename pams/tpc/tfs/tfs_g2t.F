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
      INTEGER FUNCTION TFS_G2T(  g2t_hit_h,       g2t_hit,
     >                           g2t_track_h,     g2t_track,
     >                           par_h,            par,
     >                           bmpar_h,          bmpar,
     >                           ctrl_h,           ctrl,
     >                           tpc_index_h,      tpc_index,
     >                           tpc_index_type_h, tpc_index_type,
     >                           tphit_h,          tphit)
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
C       bmpar_h          = header for TFS bmpar table
C       bmpar           = rows of TFS bmpar table
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

C-----------------------------------------------------------------------

#include "tfs_g2t.inc"

C-----------------------------------------------------------------------
      REAL DZXING, EVPXNG
      INTEGER NXINGS, NXTRIG
      COMMON /EVMERG/ DZXING, NXINGS, NXTRIG, EVPXNG
C     DZXING - z separation between beam bunches
C     NXINGS - number of beam crossings per TPC live time
C     NXTRIG - the number of the triggered crossing
C     EVPXNG - average number of events per beam crossing

C Local variables

      LOGICAL   NEW_EVENT, EVENT_FLAG, LAST_EVENT
      logical   new_xing, goloop, errflg
      INTEGER   NEVENT_OUT, IX, NOK
      integer   ind, ierr, iev, nevpx(5), nevent,i
      REAL      TIME_NOW, TIME_RD, TIME_POINT, TIME_DISPL
      REAL      TIME_MERGE, TIME_ALL, TIME_SORT
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
         CALL TFS_INIT( par_h, par, ctrl_h, ctrl, bmpar_h, bmpar,
     >        errflg )
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
         ix  = 0                ! bunch crossing counter
         iev = 0                ! sub-event counter
         nok = 0                ! nok rows in tphit table
         new_event = .false.
      endif

C     (re)set running count of OK rows in tphit table
      if( ctrl(1).evmerg .ne. 1 ) tphit_h.nok = nok

C     update current beam crossing
      if( new_xing ) then
         iev = 0

C     if this is a pp event, calculate events per bunch crossing
         if( ctrl(1).ppflag .ne. 0 ) then
            nevent = 0
            goloop = .true.
            do while ( goloop )
               ix  = ix + 1
               call poissn(evpxng,nevent,ierr)
               if( ierr .ne. 0 ) then
                  write(6,'(a,i6)') ' TFS_g2t - POISSN ierr: ',ierr
                  nevent = 1
               endif
C     make sure that triggered crossing has an event
               if( ix.eq.nxtrig .and. nevent.eq.0 ) then
                  do while ( nevent.eq.0 )
                     call poissn(evpxng,nevent,ierr)
                     if( ierr .ne. 0 ) then
                        write(6,'(a,i6)') 
     &                       ' TFS_g2t - POISSN ierr: ',ierr
                        nevent = 1
                     endif
                  enddo
               endif
C     keep statistics on the number of events per crossing
               ind = nevent + 1
               if( ind .gt. 5 ) ind = 5
               nevpx(ind) = nevpx(ind) + 1
               if(ix.eq.nxtrig-1.and.nevent.eq.0) nevent=1
               if( nevent.gt.0 .or. ix.eq.nxings ) goloop = .false.
            enddo
         else
C     single event
            ix  = ix + 1
            nevent = 1
         endif
         
         new_xing = .false.
      endif
      
C     read-in the Monte-Carlo hit data
      if( nevent.gt.0 ) then    ! may be zero for last xing
         iev = iev + 1
         CALL TFS_G2T_HIT_RD( g2t_hit_h, g2t_hit, 
     >        g2t_track_h, g2t_track, 
     >        ctrl_h, ctrl,par_h, par, tphit_h, tphit,
     >        ix, nxtrig,
     >        tpc_index_h, tpc_index, tpc_index_type_h,
     >        tpc_index_type)
         CALL TIMED(TIME_NOW)
         TIME_ALL = TIME_ALL + TIME_NOW
         TIME_RD = TIME_RD + TIME_NOW
      endif
      
C     
C     if the next is a trigger crossing ask for a switch in input files....
C     
CCC   if( ix .eq. nxtrig-1 .and. iev .eq. nevent ) then
CCC   mctrl(1).ifile=2 
CCC   else
CCC   if this is an end of any other event, get the next one from the 
CCC   regular input stream.,....
CCC   mctrl(1).ifile=1
CCC   endif
C     check current sub-event (may be more than one per beam crossing)
      if( iev .eq. nevent ) new_xing = .true.
      
C     abort TAS event loop if not yet done
      if( ctrl(1).bipass .ne. 0 ) then
C     bipass any processing of this event
         tfs_g2t = STAFCV_OK
         new_event = .true.
         
      elseif( ix .eq. nxings .and. iev .eq. nevent ) then
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
         
C     displace overlapping events
         IF( CTRL(1).PPFLAG .NE. 0 )
     >        CALL TFS_DISPL_EVENT( par_h, par, tphit_h, tphit )
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
            
            if( ctrl(1).ppflag .ne. 0 ) then
               WRITE(6,*) ' AVERAGE EVENTS PER BUNCH CROSSING: ',EVPXNG
               WRITE(6,*) ' 0 EVENT CROSSINGS: ',NEVPX(1)
               WRITE(6,*) ' 1 EVENT CROSSINGS: ',NEVPX(2)
               WRITE(6,*) ' 2 EVENT CROSSINGS: ',NEVPX(3)
               WRITE(6,*) ' 3 EVENT CROSSINGS: ',NEVPX(4)
               WRITE(6,*) ' > EVENT CROSSINGS: ',NEVPX(5)
            endif
            
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
C-----------------------------------------------------------------------
CDECK  ID>, TFSHRD. 
      SUBROUTINE TFS_G2T_HIT_RD( g2t_hit_h,        g2t_hit,
     >                           g2t_track_h,      g2t_track,
     >                           ctrl_h,           ctrl,
     >                           par_h,            par,
     >                           tphit_h,          tphit,
     >                           ix,              nxtrig,
     >                           tpc_index_h,      tpc_index,
     >                           tpc_index_type_h, tpc_index_type)


      IMPLICIT NONE
C-----------------------------------------------------------------------
C
C     Input arguments:
C       ix = current crossing number
C
C     Output arguments:
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
      COMMON /TPHPNT/ IROW
C     IROW  - 1 pointer to a row in a sector
C     IROW  - 2 number of hits


C Local variables
      integer ih, jrow, jsect, nok, ix, nxtrig

      real x, dx, y, dy

C     central membrane width = 25 microns -- Aleph Handbook V.13
C     central membrane width = 76 microns -- TPC geometry proposal by Iwona S.
      real central_membrane
      parameter (central_membrane=0.0076)

C=========================== Begin Executable Code =======================

C     set row counter for tphit
      nok = tphit_h.nok

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
            
         endif
         
 100  enddo

C     set the row counter
      tphit_h.nok = nok

 999  CONTINUE
      RETURN
      END

C-----------------------------------------------------------------------
CDECK  ID>, TFSORT. 
      SUBROUTINE TFS_HIT_SORT( tphit_h, tphit )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C
C     Input arguments:
C
C     Output arguments:
C
C     Functional Description :
C       Hits are sorted according to sector-row. Sets up a local
C       pointer index to the first and last hits in a given row.
C
C     Author:
C       Peter G. Jones  LBL  (510)-486-5436
C
C     Creation Date:
C       10-Mar-1993
C
C     Modified:
C	     By		     	      Date	 	  History
C       Iwona Sakrejda(LBL) 	   02/18/1996      To use the GSTAR tables
C	Dhammika Weerasundara(UW)  03/27/1996      
C
C-----------------------------------------------------------------------
#include "PAM.inc"
#include "tcl_tphit.inc"
C-----------------------------------------------------------------------
      RECORD / table_head_st / tphit_h
      RECORD / tcl_tphit_st / tphit(*)
C-----------------------------------------------------------------------

      INTEGER MXROW
      PARAMETER (MXROW=99)
      INTEGER MXSECTOR
      PARAMETER (MXSECTOR=24)
      INTEGER IROW(2,MXROW,MXSECTOR)
      COMMON /TPHPNT/ IROW
C     IROW  - 1 pointer to a row in a sector
C     IROW  - 2 number of hits

C Local variables
      integer ih, jrow, jsect, iret
      integer tls_Quick_Sort_i

C=========================== Begin Executable Code =======================

C     Sort hits according to the encoded sector-row information. Then
C     set up pointers to the beginning and end of each sector-row. This
C     is to be used later to sort each sector-row according to drift.

      iret = tls_Quick_Sort_i(tphit_h.nok,tphit(1).row,
     >     tphit(2).row,tphit)
      call vzero(irow,2*mxrow*mxsector)
      jrow = mod( tphit(1).row, 100 )
      jsect = (tphit(1).row - jrow)/100
      irow(1,jrow,jsect) = 1
      irow(2,jrow,jsect) = irow(2,jrow,jsect) + 1
      do ih = 2, tphit_h.nok
         if (tphit(ih).row .ne. tphit(ih-1).row) then
            jrow = mod( tphit(ih).row, 100 )
            jsect = (tphit(ih).row - jrow)/100
            irow(1,jrow,jsect) = ih
         endif
         irow(2,jrow,jsect) = irow(2,jrow,jsect) + 1
      enddo

      RETURN
      END

C-----------------------------------------------------------------------
CDECK  ID>, TFSINI. 
      SUBROUTINE TFS_INIT(par_h, par, ctrl_h, ctrl, bmpar_h, bmpar,
     >                    errflg)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C     
C     Input arguments:
C     par_h   = header for tfs fspar table
C     par     = rows of tfs fspar table
C     ctrl_h  = header for tfs ctrl table
C     ctrl    = rows of tfs ctrl table
C     bmpar_h = header for tfs bmpar table
C     bmpar   = rows of tfs bmpar table
C     errflg  = error flag -- .false. - INIT successful; .true. - INIT failed
C
C     Output arguments:
C
C     Functional Description:
C     
C     Author:
C     Peter G. Jones  LBL  (510)-486-5436
C     
C     Creation Date:
C     13-Feb-1992
C     
C     Modefied
C     By                Date                    Reason
C     --                ----                    ------
C     Dhammika W.      03/29/1997       Modefied the calculation of
C                                       diffusion coefficients in the
C                                       pad response function and the pad
C                                       resolution function  to accommodate
C                                       zero B-field case.
C-----------------------------------------------------------------------
#include "PAM.inc"
#include "tfs_fspar.inc"
#include "tfs_fsctrl.inc"
#include "tfs_bmpar.inc"
C-----------------------------------------------------------------------
      RECORD / table_head_st / par_h
      RECORD / tfs_fspar_st  / par(*)
      RECORD / table_head_st / ctrl_h
      RECORD / tfs_fsctrl_st / ctrl(*)
      RECORD / table_head_st / bmpar_h
      RECORD / tfs_bmpar_st  / bmpar(*)
C-----------------------------------------------------------------------
      INTEGER MXROW
      PARAMETER (MXROW=99)
      INTEGER MXSECTOR
      PARAMETER (MXSECTOR=24)

      REAL SIGMA02(4), SIGMAD2(4), SIGMAT2(4), SIGMA_L
      REAL PADL(2), PADL2(2), ANG_WIREF(4), ANG_PADF(4), ION_POT,
     >     WIRE_SPACE, WIRE_SPACE2, CLUS_DIS_P10
      COMMON /PADRES/ SIGMA02, SIGMAD2, SIGMAT2, SIGMA_L,
     >     PADL,PADL2,ANG_WIREF,ANG_PADF,ION_POT,WIRE_SPACE,
     >     WIRE_SPACE2,CLUS_DIS_P10

      REAL SPRF_02(2), SPRF_DR2(2), SPRF_TA2(2)
      REAL SIG_LDIFF2, SIG_SHAPE2
      COMMON /PADREF/ SPRF_02, SPRF_DR2, SPRF_TA2,
     >     SIG_LDIFF2, SIG_SHAPE2
      
      REAL ALOW, AHIGH, ASCALE
      COMMON /ASCALE/ ALOW, AHIGH, ASCALE
C     ALOW   - max value of crossing angle for pad response to be valid
C     AHIGH  - min value of crossing angle for projected width to be valid
C     ASCALE - is the linear scale factor in-between

      REAL RSEP(2), ZSEP, RPLIM(2), ZLIM
      COMMON /TPMERG/ RSEP, ZSEP, RPLIM, ZLIM
      
      REAL DZXING, EVPXNG
      INTEGER NXINGS, NXTRIG
      COMMON /EVMERG/ DZXING, NXINGS, NXTRIG, EVPXNG
C     DZXING - z separation between beam bunches
C     NXINGS - number of beam crossings per TPC live time
C     NXTRIG - the number of the triggered crossing
C     EVPXNG - average number of events per beam crossing
      
C     Local variables
      REAL CPI, C2PI, CPI2, CLIGHT, ELMASS, CROOT2, DTORAD
      REAL BCONV
C     
      PARAMETER ( 
     >     CPI    = 3.141592654, 
     >     C2PI   = 6.283185307,
     >     CPI2   = 1.570796327,
     >     CROOT2 = 1.414213562, 
     >     DTORAD = 0.017453293,
     >     BCONV  = 0.002998,
     >     CLIGHT = 2.99792458E+10, 
     >     ELMASS = 0.5110034)
C     
C     General physical constants
C     CPI = pi
C     C2PI = 2*pi
C     CPI2 = pi/2
C     CLIGHT = Velocity of light (cm/sec)
C     ELMASS = electron mass  (MeV)
C     CROOT2 = sqrt(2)
C     DTORAD = Conv. factor degrees to radians
C     BCFGEV = Conv. factor:     pt(GeV) = BCFGEV*radius(cm)
C     BCFMEV = Conv. factor:     pt(MeV) = BCFMEv*radius(cm)
      
      REAL RADEG
      REAL DRIVEL, TXING, NEVENT, DELTIM, EVRATE
      LOGICAL ERRFLG
      
      REAL  CONST, OMEGA_TAU
      REAL  CLUS_PER_CM_AR, CLUS_PER_CM_CH4, F_AR, F_CH4, PRESSURE

      INTEGER  n0

      PARAMETER(CLUS_PER_CM_AR  = 22.3,
     >          CLUS_PER_CM_CH4 = 24.3,
     >          F_AR = 0.90,
     >          F_CH4 = 0.10,
     >          PRESSURE = 1.0)

C=========================== Begin Executable Code =======================
C23456789012345678901234567890123456789012345678901234567890123456789012
      
      ERRFLG = .FALSE.
      if( par(1).nsect .gt. mxsector ) then
         WRITE(6,'(A)') ' TFS_INIT: Fatal error'
         WRITE(6,'(A)') ' Reason: Number of sectors exceeds MXSECTOR'
         ERRFLG = .TRUE.
         RETURN
      endif
      if( par(1).nrow .gt. mxrow ) then
         WRITE(6,'(A)') ' TFS_INIT: Fatal error'
         WRITE(6,'(A)') ' Reason: Number of rows exceeds MXROW'
         ERRFLG = .TRUE.
         RETURN
      endif
      
C     pad response function (prf)
      
C     modefy sprf_dr2 & sigmad2 to accomodate zero B_filed case.
C     In the new definition, sprf_dr0 in  tfs_fspar represents the
C     field free single electron diffusion;
C     now  sprf_dr2 = sprf_dr0^2/(C + (omega*tau)^2)
C     omega -- electron cyclotron frequency, tau -- mean collision time
C     For the time being (as of 03/29/1997), for non-zero B-filed, we use 
C     C=2.8 & omega*tau=2.34 (see STAR CDR p. 4C-15 & S.R. Amendolia et al. 
C     NIM A244 (1986) 516) and for zero B-field, C=1 & omega*tau=0. 
C     *** DSW  03/29/1997 ***

      if ( par(1).bmag .ne. 0. ) then
         Const     = 2.8
         omega_tau = 2.34
      else
         Const     = 1.0
         omega_tau = 0.0
      endif

      sprf_02(1)  = par(1).sprf_0(1)**2
      sprf_02(2)  = par(1).sprf_0(2)**2
      sprf_dr2(1) = par(1).sprf_dr0(1)**2/(Const + omega_tau**2)
      sprf_dr2(2) = par(1).sprf_dr0(2)**2/(Const + omega_tau**2)
      sprf_ta2(1) = par(1).sprf_ta(1)**2
      sprf_ta2(2) = par(1).sprf_ta(2)**2

       
C     longitudinal coefficient of diffusion (units :- sqrt(cm))
      sig_ldiff2 = par(1).sldiff**2
      
C     sigma of longitudinal pulse due to electronics shaping time -
C     TSHAPE is a FWHM in nanoseconds, DRIVEL is cm/microseconds

      sig_shape2 = ( par(1).drivel * par(1).tshape / 2.36 / 1000. )**2
      
C     spatial resolution

C     Modefy the calculation of  sigmad2 to accommodate the changes to
C     the calculation of sprf_dr2, above (see STAR CDR p. 4C-28).
C     ***  DSW  03/29/1997  ***

C     n0 -- Number of electrons liberated per cm.  04/17/1997  DSW
      n0 = par(1).n0_pcm
      
      wire_space   = 0.4
      wire_space2  = wire_space**2

      padL(1)      = par(1).padlen(1)
      padL(2)      = par(1).padlen(2)
      padL2(1)     = par(1).padlen(1)**2
      padL2(2)     = par(1).padlen(2)**2

C     Following calculation is needed to determine the  inter cluster
C     distance for P10.
C     See Roy B.'s mail to DSW dated May 7, 1997.
      clus_dis_P10 = pressure*
     >     (f_Ar*clus_per_cm_Ar + f_CH4*clus_per_cm_CH4)

C     P10 ionization potential  in GeV
      Ion_Pot = 28.5e-9 

      if (par(1).sys_test.eq.0) then
         sigma02(1)   = par(1).sigma0(1)**2
         sigma02(2)   = par(1).sigma0(2)**2
C     sigmad2(1)   = sprf_dr2(1)/(n0*par(1).padlen(1))
C     sigmad2(2)   = sprf_dr2(2)/(n0*par(1).padlen(2))
         sigmad2(1)   = sprf_dr2(1)
         sigmad2(2)   = sprf_dr2(2)
         sigmat2(1)   = par(1).sigmat(1)**2
         sigmat2(2)   = par(1).sigmat(2)**2
         ang_wireF(1) = par(1).ang_wireF(1)
         ang_wireF(2) = par(1).ang_wireF(2)
         ang_padF(1)  = par(1).ang_padF(1)
         ang_padF(2)  = par(1).ang_padF(2)
      elseif (par(1).sys_test.ge.1) then
         sigma02(3)   = par(1).sigma0(3)**2
         sigma02(4)   = par(1).sigma0(4)**2
         sigmad2(3)   = par(1).sprf_dr0(3)**2
         sigmad2(4)   = par(1).sprf_dr0(4)**2
         sigmat2(3)   = par(1).sigmat(1)**2
         sigmat2(4)   = par(1).sigmat(2)**2
         ang_wireF(3) = par(1).ang_wireF(3)
         ang_wireF(4) = par(1).ang_wireF(4)
         ang_padF(3)  = par(1).ang_padF(3)
         ang_padF(4)  = par(1).ang_padF(4)
      endif

      sigma_l      = par(1).sigmal
      
C     calculate limits for the transition from prf to projected width
      radeg = cpi / 180.
      alow  = par(1).alpha_lo * radeg
      ahigh = par(1).alpha_hi * radeg
      ascale = ( ahigh - alow )
      
C     calculate merging criterion
      rsep(1) = par(1).drmin * par(1).padwid(1)
      rsep(2) = par(1).drmin * par(1).padwid(2)
      zsep    = par(1).dzmin * par(1).tpclen / par(1).zbmax
      
C     calculate merging limits
      rplim(1) = par(1).merge_r * par(1).padwid(1)
      rplim(2) = par(1).merge_r * par(1).padwid(2)
      zlim     = par(1).merge_z * par(1).tpclen / par(1).zbmax
      
C     calculate number of events per gate opening
      if (ctrl(1).ppflag .ne. 0) then
         drivel = par(1).drivel * 1.e+06
         deltim = 2.0*par(1).tpclen / drivel
         evrate = bmpar(1).xsec * 1.e-27 * bmpar(1).lumin
         nevent = evrate * deltim
         txing  = bmpar(1).txing * 1.e-09
         nxings = deltim / txing
         nxtrig = nint( nxings / 2. )
         evpxng = nevent / nxings
         dzxing = drivel * txing

         WRITE(6,'(A,F5.2)') ' average pp events per crossing: ',evpxng
      else
         nxings = 1
         nxtrig = 1
      endif
      
 999  return
      end
      
C-----------------------------------------------------------------------
CDECK  ID>, TPCPNT. 
      SUBROUTINE TFS_GEN_PADRES( par_h, par, tphit_h, tphit )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C     Input Arguments:
C       par_h   = header for TFS fspar table
C       par    = rows of TFS fspar table
C       tphit_h = header of TCL tphit table
C       tphit  = rows of TFC tphit table
C
C     Output Arguments:
C       values in tphit table
C
C     Functional Description:
C       This routine assigns a spatial width and a spatial resolution
C       to the TPC space-points gotten from GEANT. The spatial width
C       (aka the pad response) and the pad resolution are a function of
C       the drift length, pad crossing angle and the dip angle. The
C       parameterizations used here are described in STAR notes (#33
C       and #34). The values of the coefficients to these expressions
C       have been determined from EOS and ALEPH measurements. These
C       are contained in the parameter table.
C
C     Author:
C       Peter G. Jones  LBL  (510)-486-5436
C
C     Creation Date:
C       16-Mar-1993
C
C     Modefied
C     By                Date                    Reason
C     --                ----                    ------
C     Dhammika W.      03/29/1997       Modefied the calculation of
C                                       wmin & tlen  to accommodate
C                                       zero B-field case.
C-----------------------------------------------------------------------
#include "PAM.inc"
#include "tfs_fspar.inc"
#include "tcl_tphit.inc"

C-----------------------------------------------------------------------
      RECORD / table_head_st / par_h
      RECORD / tfs_fspar_st / par(*)
      RECORD / table_head_st / tphit_h
      RECORD / tcl_tphit_st / tphit(*)
C-----------------------------------------------------------------------
      INTEGER MXROW
      PARAMETER (MXROW=99)
      INTEGER MXSECTOR
      PARAMETER (MXSECTOR=24)
      INTEGER IROW(2,MXROW,MXSECTOR)
      COMMON /TPHPNT/ IROW
C     IROW  - 1 pointer to a row in a sector
C     IROW  - 2 number of hits
      
      REAL SIGMA02(4), SIGMAD2(4), SIGMAT2(4), SIGMA_L
      REAL PADL(2), PADL2(2), ANG_WIREF(4), ANG_PADF(4), ION_POT,
     >     WIRE_SPACE, WIRE_SPACE2, CLUS_DIS_P10
      COMMON /PADRES/ SIGMA02, SIGMAD2, SIGMAT2, SIGMA_L,
     >     PADL,PADL2,ANG_WIREF,ANG_PADF,ION_POT,WIRE_SPACE,
     >     WIRE_SPACE2,CLUS_DIS_P10

      REAL SPRF_02(2), SPRF_DR2(2), SPRF_TA2(2)
      REAL SIG_LDIFF2, SIG_SHAPE2
      COMMON /PADREF/ SPRF_02, SPRF_DR2, SPRF_TA2,
     >     SIG_LDIFF2, SIG_SHAPE2

      REAL ALOW, AHIGH, ASCALE
      COMMON /ASCALE/ ALOW, AHIGH, ASCALE
C     ALOW   - max value of crossing angle for pad response to be valid
C     AHIGH  - min value of crossing angle for projected width to be valid
C     ASCALE - is the linear scale factor in-between

      REAL RSEP(2), ZSEP, RPLIM(2), ZLIM
      COMMON /TPMERG/ RSEP, ZSEP, RPLIM, ZLIM
      
C     Local variables
      
      REAL CPI, C2PI, CPI2, CLIGHT, ELMASS, CROOT2, DTORAD
      REAL BCONV
C     
      PARAMETER ( CPI    = 3.141592654, 
     >     C2PI   = 6.283185307,
     >     CPI2   = 1.570796327,
     >     CROOT2 = 1.414213562, 
     >     DTORAD = 0.017453293,
     >     BCONV  = 0.002998,
     >     CLIGHT = 2.99792458E+10, 
     >     ELMASS = 0.5110034)
      
C     
C     General physical constants
C     CPI = pi
C     C2PI = 2*pi
C     CPI2 = pi/2
C     CLIGHT = Velocity of light (cm/sec)
C     ELMASS = electron mass  (MeV)
C     CROOT2 = sqrt(2)
C     DTORAD = Conv. factor degrees to radians
C     BCFGEV = Conv. factor:     pt(GeV) = BCFGEV*radius(cm)
C     BCFMEV = Conv. factor:     pt(MeV) = BCFMEv*radius(cm)
      
      integer i,j,k, isect, istart, iend, ipad, iret
      integer tgc_global_to_local_p
      
      real px, py, pz
      real xi, yi, zi
      real xo, yo, zo
      real aout, bout, xtemp, ytemp, atemp
      real ctheta, theta, pbend, diptan, rad, wcirc, alpha, lambda
      real sigma_x, sigma_y, sigma_tr, sigma_response
      real dlen, talpha2, calpha2, wproj, wmin, wprf, tlen, padlen
      real p_local(3),p_global(3)
      real theta_const
      real Nh,Neff_b,Neff_h,siga2(2),sigb2(2),sigc2(2),sigd2(2)
      real clus_per_cm_P10,rel_dedx,ds_ave
      real AA, BB, CC

C=========================== Begin Executable Code =======================

C     loop over sector-rows

      do i = 1, par(1).nsect
         do j = 1, par(1).nrow
            ipad = 2
            if( j .le. par(1).irmax ) ipad = 1
            istart = irow(1,j,i)
            iend   = irow(1,j,i) + irow(2,j,i) - 1
            do k = istart, iend
               
C     do not consider rejected hits ( flag = 1-99 )
               if( mod(tphit(k).flag,100) .ne. 0 ) go to 100
               
C     get space point
               
               xi = tphit(k).x
               yi = tphit(k).y
               zi = tphit(k).z
               
C     momenta were copied into dx, dy, dz for safe-keeping
               
               px = tphit(k).dx
               py = tphit(k).dy
               pz = tphit(k).dz
               tphit(k).dx = 0
               tphit(k).dy = 0
               tphit(k).dz = 0
               
CCCC  calculate the tangent to the padrow (y = a*x + b)
CCCC  
CCCC  Note to users: intercept value 'bout' is not currently
CCCC  used. Its correctness can not guarenteed.
CCCC  CALL TFS_SEGIVE( par_h, par i, j, aout, bout )
CCCC  
CCCC  calculate the crossing angle as dot-product of tangent
CCCC  to the padrow and local momentum at the center of pad
CCCC  
CCCC  xtemp  = 1.0
CCCC  ytemp  = xtemp*aout
CCCC  atemp = sqrt( (xtemp**2 + ytemp**2) * (px**2 + py**2) )
CCCC  if( abs(atemp) .lt. 0.000001) then
CCCC  flag hit for discarding
CCCC  tphit(k).flag = tphit(k).flag + 2
CCCC  go to 100
CCCC  endif
CCCC  ctheta = abs( xtemp*px + ytemp*py ) / atemp
CCCC  catch rounding errors
CCCC  if( ctheta .gt. 1. ) ctheta = 1.
CCCC  theta  = acos( ctheta ) ! angle wrt pad row
CCCC  alpha  = cpi2 - abs( theta ) ! angle wrt normal
               
C     tan(px/py) in the local sector coordinates immediately gives the 
C     pad-crossing angle.  -- Apr 10, 96  --- DSW ---
               
               p_global(1) = px 
               p_global(2) = py
               p_global(3) = pz
               iret  = tgc_global_to_local_p(i,p_global,p_local)
               alpha = atan(p_local(1)/p_local(2)) ! angle wrt normal
               theta = cpi2 - abs(alpha) ! angle wrt pad row
               tphit(k).alpha = alpha
               
C     calculate the dip angle
               
               pbend = sqrt( px**2 + py**2 )
               if( pbend .lt. 0.000001)  then
C     flag hit for discarding
                  tphit(k).flag = tphit(k).flag + 3
                  go to 100
               endif
C     Note: diptan = 1. / ( tangent of the dip angle )
C     it is coded this way to avoid overflow when pz = 0
               diptan          = abs(pz)/ pbend
               tphit(k).lambda = atan( diptan )
               lambda          = tphit(k).lambda

C     Modefy the code to accommodate zero B-filed magnitude.
C     Added a piece of code to handle zero B-field case.
C     ***  DSW  03/29/1997 ***
               
               if ( par(1).bmag .ne. 0. ) then
                  
C     evaluate the length of the track over the padrow
C     wmin - is the track length along the padrow
C     tlen  - is the approximated track arc-length
                  
C     1st -  approximate with a circle

                  rad = sqrt( px**2 + py**2 ) / (bconv*par(1).bmag)
                  if( rad .lt. par(1).padlen(ipad) ) then
C     this is a spiral - cannot calculate track length
                     wcirc = 2.0 * rad
                     if( wcirc .lt. par(1).padwid(ipad) ) then
                        wcirc = par(1).padwid(ipad)
                     endif
                     tlen = 0
                  else
                     padlen = par(1).padlen(ipad)/2.
                     wcirc = 2. * sqrt( 2.*rad*padlen - padlen**2.)
                     tlen = 2.*rad*atan(wcirc/(2.*(rad-padlen)))
                  endif
                  
C     2nd - calculate from a  linear geometrical projection
                  if( abs(theta) .gt. 0.0001) then
                     wproj = abs( par(1).padlen(ipad) / tan(theta) )
C     compare with the circle width and take smaller
                     wmin = min( wproj , wcirc )
                     if ( wmin .eq. wproj )

     >                    tlen = sqrt(wproj**2+par(1).padlen(ipad)**2)
                  else
                     wmin = wcirc
                  endif
               else
C     zero B-filed case.
C     For track lengths that go across an entire single pad length,
C     it is probably reasonable to approximate wmin=pad width & 
C     tlen=pad length.
C     *** DSW  03/29/1997 ***

                  theta_const = par(1).padlen(ipad)/par(1).padwid(ipad)
                  if ( tan(theta).gt.theta_const )then
                     wmin = par(1).padwid(ipad)
                     tlen = par(1).padlen(ipad)
                  else
                     wmin = abs( par(1).padlen(ipad) / tan(theta) )
                     tlen = sqrt(wmin**2+par(1).padlen(ipad)**2)
                  endif
               endif
C     calculate half-width to determine merging criterion
               wmin = wmin/2.
C     add something for diffusion
               dlen = abs( par(1).tpclen - abs(zi) ) ! in cm
               wmin = wmin + sqrt( sprf_dr2(ipad) * dlen )
               
C     for crossing angles close to 90 degrees wmin is a better
C     approximation for wprf - the spatial width calculated from
C     the pad response function.
               
C               if( theta .gt. 0.01) then ! 0.6 degree
               if( theta .gt. 0.0175) then ! 1 degree
                  talpha2 = tan(alpha)**2
                  wprf = sprf_02(ipad) +
     >                 sprf_dr2(ipad) * dlen * (1 + talpha2) +
     >                 sprf_ta2(ipad) * talpha2
                  wprf = sqrt(wprf)
C     scale wprf to meet wmin between 45.0 < alpha < 72.0 degrees
                  if( alpha .ge. alow .and. alpha .lt. ahigh ) then
                     sigma_response = wprf +
     >                    (alpha - alow)/ascale * abs(wmin - wprf)
                  elseif (alpha.lt.alow) then
                     sigma_response = wprf
                  else
                     sigma_response = wmin
                  endif
               else
                  sigma_response = wmin
               endif
               
C     store the pad/z-response width in tphit table
               
               tphit(k).prf = sigma_response
               if( tphit(k).prf .gt. par(1).reslim )
     >              tphit(k).prf = par(1).reslim
               if( tlen .eq. 0 ) then
                  tphit(k).zrf = par(1).reslim
               else
C     z-width (half width plus something for diffusion)
                  tphit(k).zrf = 0.5 * ( tlen*diptan ) + ! projected width
     >                 sqrt( sig_ldiff2 * dlen + ! diffusion
     >                 sig_shape2 ) ! shaping time
                  if( tphit(k).zrf .gt. par(1).reslim )
     >                 tphit(k).zrf = par(1).reslim
               endif
               

C     Calculate spatial resolution along the padrow
C     Spatial resolution calculation is updated to accomadate the fitting of
C     pad resolution function (in p184 of Blum & Rolandi) to Roy B.'s 
C     calculated widths.

               Nh     = tphit(k).q/Ion_Pot
C                  clus_per_cm_P10 = rel_dedx*clus_dis_P10
C                  ds_ave = 1./clus_per_cm_P10
C                  Neff_b = 2.7*wire_space/ds_ave
C     We need to find out how to calculate rel_dedx for any give geant track.
C     Until then use Neff_b = 24.84 which is a good approximation.
               Neff_b = 24.84
               Neff_h = Neff_b*((padL(ipad)/wire_space)**0.54)
               talpha2 = tan(alpha)**2
               calpha2 = cos(alpha)**2

               if( theta .lt. 0.0175 ) then
                  sigma_tr = sigma_response / sqrt(12.0)
               else
                  if(par(1).sys_test.eq.0)then
C     sigma_tr = sqrt( sigma02(ipad) +
C     >                    sigmad2(ipad) * dlen/calpha +
C     >                    sigmat2(ipad) * talpha2 * calpha )
                     
                     siga2(ipad)  = sigma02(ipad)/Nh**2
                     sigb2(ipad)  = sigmad2(ipad)/Nh
                     sigc2(ipad)  = ang_wireF(ipad)*wire_space2/
     >                    (12.*Neff_b)
                     sigd2(ipad)  = ang_padF(ipad)*
     >                    (padL2(ipad)-wire_space2)/
     >                    (12.*Neff_h)
C     Calculate pad resolution. Use Eq. 6.34 in Blum & Rolandi (p. 184)
C     General simulations.
                     sigma_tr = sqrt( siga2(ipad)       +
     >                    sigb2(ipad)*dlen/calpha2      +
     >                    (sigc2(ipad)+sigd2(ipad))*tan(alpha)**2 )
                     
                  elseif(par(1).sys_test.eq.1)then
                     siga2(ipad)  = sigma02(ipad+2)/Nh**2
                     sigb2(ipad)  = sigmad2(ipad+2)/Nh
                     sigc2(ipad)  = ang_wireF(ipad+2)*wire_space2/
     >                    (12.*Neff_b)
                     sigd2(ipad)  = ang_padF(ipad+2)*
     >                    (padL2(ipad)-wire_space2)/
     >                    (12.*Neff_h)
C     Calculate pad resolution. Use Eq. 6.34 in Blum & Rolandi (p. 184)
C     System test  simulations.
                     sigma_tr = sqrt( siga2(ipad)       +
     >                    sigb2(ipad)*dlen/calpha2      +
     >                    (sigc2(ipad)+sigd2(ipad))*
     >                    abs(tan(alpha))**1.2 )
                  elseif(par(1).sys_test.eq.2)then
C     Use a parametrization solely based on the  cosmic ray test data in the
C     limited crossing angle region  [-45, 45] degrees.  --- DSW  6/17/97 ---
                     sigma_tr = 0.04 + 0.09*(abs(alpha)/(DTORAD*45.))
     >                    + 0.04*(abs(alpha)/(DTORAD*45.))**2
     >                    + 0.32*(abs(alpha)/(DTORAD*45.))**8
                  endif
               endif
               
C     Calculate  the error in Z coordinate (sigma_l). Use Eq. 6.40 in
C     Blum & Roland (p. 188).   DSW  May 29, 1997.
               if (par(1).sys_test.eq.0) then
                  sigb2(ipad)  = sigmad2(ipad)/Nh
               elseif(par(1).sys_test.eq.1)then
                  sigb2(ipad)  = sigmad2(ipad+2)/Nh
               endif
               
               AA  = sigb2(ipad)*dlen/calpha2
               BB  = wire_space2*tan(alpha)**2/(12.*Neff_b)
               CC  = 0.64*wire_space2/(180.*Neff_b)
               
               if(par(1).sys_test.lt.2)then
                  if( theta .lt. 0.0175 ) then
                     sigma_l      = sqrt( CC )
                  else
                     sigma_l      = sqrt( AA + BB + CC )
                  endif
                  
               elseif(par(1).sys_test.eq.2)then
C     Use a parametrization solely based on the  cosmic ray test data in the
C     limited dip angle region  [-45, 45] degrees. --- DSW  6/17/97 ---
                  sigma_l = 0.04 + 0.09*(abs(lambda)/(DTORAD*45.))
     >                 + 0.04*(abs(lambda)/(DTORAD*45.))**2
     >                 + 0.32*(abs(lambda)/(DTORAD*45.))**8
                  sigma_l = 2.*sigma_l
                  
               endif

C     calculate the new smeared coords. and update tphit table
               
               isect = i
CCCC  if( i .gt. 12 )  isect = i - 12

C     Here we want use the new sector numbering scheme (STAR Note SN0229).
C     Proper conversion of isect ---> rotation angle is taken care of inside 
C     TFS_HIT_SMEAR.  -- 03 APR, 96  DSW --

               CALL TFS_HIT_SMEAR( isect, xi, yi, zi, xo, yo, zo,
     >              sigma_tr, sigma_l, sigma_x, sigma_y)
               tphit(k).x = xo
               tphit(k).y = yo
               tphit(k).z = zo
               tphit(k).dx = sigma_x
               tphit(k).dy = sigma_y
               tphit(k).dz = sigma_l
               
 100           continue
C     End loop over hits
            end do
C     End loop over rows
         end do
C     End loop over sectors
      end do
      
C     WRITE(6,*) ' TFS_GEN_PADRES: number hits discarded ', nh_discard
      RETURN
      end

C-----------------------------------------------------------------------
CDECK  ID>, TFSDIS. 
      SUBROUTINE TFS_DISPL_EVENT( par_h, par, tphit_h, tphit )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C     Input Arguments:
C       par_h   = header for TFS fspar table
C       par    = rows of TFS fspar table
C       tphit_h = header of TCL tphit table
C       tphit  = rows of TCL tphit table
C
C     Output Arguments:
C       values in tphit table
C
C     Functional Description:
C       Displaces separate events along the beam direction according
C       to the current bunch crossing number ix. This routine is used
C       to overlap multiple pp events in the high luminosity mode of
C       pp running at RHIC. The average number of events per crossing
C       is calculated in TFS_INIT form the beam parameter table.
C
C     Author:
C       Peter G. Jones  LBL  (510)-486-5436
C
C     Creation Date:
C       17-Mar-1992
C
C-----------------------------------------------------------------------
#include "PAM.inc"
#include "tfs_fspar.inc"
#include "tcl_tphit.inc"
C-----------------------------------------------------------------------
      RECORD / table_head_st / par_h
      RECORD / tfs_fspar_st / par(*)
      RECORD / table_head_st / tphit_h
      RECORD / tcl_tphit_st / tphit(*)
C-----------------------------------------------------------------------
      INTEGER MXROW
      PARAMETER (MXROW=99)
      INTEGER MXSECTOR
      PARAMETER (MXSECTOR=24)
      INTEGER IROW(2,MXROW,MXSECTOR)
      COMMON /TPHPNT/ IROW
C     IROW  - 1 pointer to a row in a sector
C     IROW  - 2 number of hits
      REAL DZXING, EVPXNG
      INTEGER NXINGS, NXTRIG
      COMMON /EVMERG/ DZXING, NXINGS, NXTRIG, EVPXNG
C     DZXING - z separation between beam bunches
C     NXINGS - number of beam crossings per TPC live time
C     NXTRIG - the number of the triggered crossing
C     EVPXNG - average number of events per beam crossing

C Local variables
      integer i, j, istart, iend, k, ix

      real dz

C=========================== Begin Executable Code =======================

C     now do the displacement
      do i = 1, par(1).nsect
         do j = 1, par(1).nrow
            istart = irow(1,j,i)
            iend   = irow(1,j,i) + irow(2,j,i) - 1
            do k = istart, iend

C     decode crossing number and compare to triggered event
               ix = tphit(k).flag / 100
               tphit(k).flag = mod( tphit(k).flag, 100)
               dz = (nxtrig - ix)*dzxing
               
               if( tphit(k).z .ge. 0. ) then
                  tphit(k).z = tphit(k).z + dz
C     discard if hit moved through membrane
                  if( tphit(k).z .lt. 0. ) then
C     flag hit for discarding
                     tphit(k).flag = 9
                  endif
               else
                  tphit(k).z = tphit(k).z - dz
C     discard if hit moved through membrane
                  if( tphit(k).z .gt. 0. ) then
C     flag hit for discarding
                     tphit(k).flag = 9
                  endif
               endif
C     discard if hit moved outside tpc
               if( abs( tphit(k).z ) .ge. par(1).tpclen ) then
C     flag hit for discarding
                  tphit(k).flag = 9
               endif
            enddo
         enddo
      enddo
C     
      return
      end
      
C-----------------------------------------------------------------------
CDECK  ID>, TPCMRG. 
      SUBROUTINE TFS_TPCHIT_MERGE( ctrl_h,           ctrl,
     >                             par_h,            par,
     >                             g2t_hit_h,        g2t_hit,
     >                             tphit_h,          tphit,
     >                             tpc_index_h,      tpc_index,
     >                             tpc_index_type_h, tpc_index_type)

      IMPLICIT NONE
C-----------------------------------------------------------------------
C     Input Arguments:
C       ctrl_h     = header for the TFS ctrl table
C       ctrl      = rows of TFS ctrl table
C       par_h      = header for TFS fspar table
C       par       = rows of TFS fspar table
C       g2t_hit_h  = header for g2t g2t_tpc_hit table
C       g2t_hit   = row of g2t g2t_tpc_hit table
C       tphit_h    = header of TCL tphit table
C       tphit     = rows of TCL tphit table
C
C     Output Arguments:
C       values in g2t_hit_h table (recdig points to surviving tphit)
C       values in tphit table
C
C     Functional Description:
C       Merges TPC hits that are close in space (xy) and time (z).
C
C     Author:
C       Peter G. Jones  LBL  (510)-486-5436
C
C     Creation Date:
C       27-Feb-1992
C
C     Modified:
C	     By		     	      Date	 	  History
C       Iwona Sakrejda(LBL) 	   02/18/1996      To use the GSTAR tables
C	Dhammika Weerasundara(UW)  03/27/1996      
C
C----------------------------------------------------------------------
#include "PAM.inc"
#include "tfs_fsctrl.inc"
#include "tfs_fspar.inc"
#include "g2t_tpc_hit.inc"
#include "tcl_tphit.inc"
#include "tcl_tpc_index.inc"
#include "tcl_tpc_index_type.inc"
C-----------------------------------------------------------------------
      RECORD / table_head_st / ctrl_h
      RECORD / tfs_fsctrl_st / ctrl(*)
      RECORD / table_head_st / par_h
      RECORD / tfs_fspar_st / par(*)
      RECORD / table_head_st / g2t_hit_h
      RECORD / g2t_tpc_hit_st / g2t_hit(*)
      RECORD / table_head_st / tphit_h
      RECORD / tcl_tphit_st / tphit(*)
      RECORD / table_head_st/ tpc_index_h
      RECORD / tcl_tpc_index_st/ tpc_index(*)
      RECORD / table_head_st/ tpc_index_type_h
      RECORD / tcl_tpc_index_type_st/ tpc_index_type(*)
C-----------------------------------------------------------------------
      INTEGER MXROW
      PARAMETER (MXROW=99)
      INTEGER MXSECTOR
      PARAMETER (MXSECTOR=24)
      INTEGER IROW(2,MXROW,MXSECTOR)
      COMMON /TPHPNT/ IROW
C     IROW  - 1 pointer to a row in a sector
C     IROW  - 2 number of hits
      REAL RSEP(2), ZSEP, RPLIM(2), ZLIM
      COMMON /TPMERG/ RSEP, ZSEP, RPLIM, ZLIM


C Local variables
      INTEGER I,J,K,L,ISTART,IEND,IPAD,NUM_MARKED,NOK

      REAL DZMIN, DRMIN
      REAL DZ, DR, ZLAP, RLAP, ZWID, RWID, ZSIZ, RSIZ
      REAL ZSIZK, RSIZK, ZSIZL, RSIZL
      REAL RMIN, RMAX, ZMIN, ZMAX, RMINK, RMINL
      REAL XOUTK(3),XOUTL(3),XGLOB(3)

      integer  tgc_global_to_local, tgc_local_to_global
      integer  iret,irowl,i_rows(10),ii_rows
      integer  tls_Quick_Sort_r, tls_Quick_Sort_i,tls_Search_i

      integer  max_hit_merge,n_hit_merge,imerge_hit_id
      parameter (max_hit_merge=50000)
      integer  imerge_id(max_hit_merge,2)

C=========================== Begin Executable Code =======================

C     Initialize imerge_id.  04/16/1997   DSW.
      call vzero(imerge_id,2*max_hit_merge)
      n_hit_merge = 0

C     Quick_sort tpc_index table for key2 so it's easy to find  right
C     geant hit corresponding to given tpc hit. This is needed to update
C     the tpc_index table after tpc hit merging.

      iret = tls_Quick_Sort_i(tpc_index_h.nok,tpc_index(1).key2,
     >     tpc_index(2).key2, tpc_index)


C     loop over sector-rows
      do i = 1, par(1).nsect
         do j = 1, par(1).nrow
            
C     chose pad size
            ipad = 2
            if (j .le. par(1).irmax) ipad = 1
            
C     Sort each sector-row in time
            istart = irow(1,j,i)
            iend   = irow(1,j,i) + irow(2,j,i) - 1
            iret   = tls_Quick_Sort_r(irow(2,j,i),tphit(istart).z,
     >           tphit(istart+1).z,tphit)
            
C     start of the merging itteration
            num_marked = 1
            do while(num_marked.ne.0)
               num_marked = 0
               
C     hits are sorted in time - merge them
               do 10 k = istart, iend - 1
                  
C     ignore special case hits
                  if( tphit(k).flag.gt.0 ) go to 10
                  rsizk = 2 * tphit(k).prf
                  zsizk = 2 * tphit(k).zrf
                  do 20 l = k+1, iend
                     
C     ignore special case hits
                     if( tphit(l).flag.gt.0 ) go to 20
C     ignore hits already merged
                     if( iabs( tphit(l).flag ) .eq. k ) go to 20
C     or two hits merged with a common third hit
                     if( tphit(k).flag .lt. 0 .and.
     >                    tphit(l).flag .lt. 0 .and.
     >                    tphit(k).flag .eq. tphit(l).flag ) go to 20
                     rsizl = 2 * tphit(l).prf
                     zsizl = 2 * tphit(l).zrf
                     
C     check separation in z
                     dzmin = tphit(k).zrf + tphit(l).zrf + zsep
                     dz    = abs( tphit(k).z - tphit(l).z )
C     hits are sorted in z, so can't merge in the limit ...
                     if ( dz .gt. 2*par(1).reslim + zsep ) go to 10
                     
C     check separation in r-phi
                     drmin = tphit(k).prf + tphit(l).prf + rsep(ipad)
                     dr    = sqrt( ( tphit(k).x - tphit(l).x )**2 +
     >                    ( tphit(k).y - tphit(l).y )**2 )
                     if ( dr.lt.drmin .and. dz.lt.dzmin ) then
                        
C     these hits are close enough to merge
                        num_marked = num_marked + 1
                        
C     mark the lth hit for merging (higher in order)
                        if( tphit(k).flag .ne. 0  .and.
     >                       tphit(l).flag .ne. 0 ) then
C     merged hits not common - share lowest order hit
                           if( tphit(k).flag .gt. tphit(l).flag ) then
                              tphit(l).flag = tphit(k).flag
                           else
                              tphit(k).flag = tphit(l).flag
                           endif
                        elseif( tphit(k).flag .ne. 0 ) then
C     kth merger must have value < k
                           tphit(l).flag = tphit(k).flag
                        elseif( tphit(l).flag .ne. 0 ) then
C     lth merger must also have value < k
                           tphit(k).flag = tphit(l).flag
                        else
C     flag with the table-row number of the lowest order hit
                           tphit(l).flag = -k
                        endif
                        
                     endif      ! if (dr.lt.drmin .and.
 20               continue      ! loop over lth hit
 10            continue         ! loop over kth hit
               if( ctrl(1).print .gt. 2 ) then
                  write(6,'(a,3i6)') ' sect, row, mergers: ',
     &                 i,j,num_marked
               endif
            enddo               ! end merging itteration
            
C     ------- final pass does the merging ----------------------------------
C     hits are sorted in time - merge them
            do 100 k = istart, iend
               
C     ignore special case hits
               if( tphit(k).flag .gt. 0 ) go to 100
               
C     flag large single hit clusters
               if( tphit(k).flag.eq.0 ) then
                  if( 2*tphit(k).prf .gt. rplim(ipad) .and.
     >                 2*tphit(k).zrf .gt .zlim ) then
C     flag hit for discarding
                     tphit(k).flag = 12
                  endif
                  go to 100
               endif
               
C     this is a merged hit
               rsizk = 2. * tphit(k).prf
               zsizk = 2. * tphit(k).zrf
               
C     find merged partner
               l = iabs( tphit(k).flag )
               rsizl = 2. * tphit(l).prf
               zsizl = 2. * tphit(l).zrf
               
C     calculate widths and overlaps
               dr    = sqrt( ( tphit(k).x - tphit(l).x )**2 +
     >              ( tphit(k).y - tphit(l).y )**2 )
               dz    = abs( tphit(k).z - tphit(l).z )
               zwid = tphit(k).zrf + tphit(l).zrf
               rwid = tphit(k).prf + tphit(l).prf
               zlap = zwid - dz
               rlap = rwid - dr
               
C     rotate into local sector coord system
               xglob(1) = tphit(k).x
               xglob(2) = tphit(k).y
               xglob(3) = tphit(k).z
               iret  = tgc_global_to_local(i,xglob,xoutk)
               xglob(1) = tphit(l).x
               xglob(2) = tphit(l).y
               xglob(3) = tphit(l).z
               iret  = tgc_global_to_local(i,xglob,xoutl)

C     In the new tgc routines, local Z coordinate is measured relative to
C     pad-plane and is alway positive. So Z-direction hit merging needs to be 
C     done in local sector coordinate system. Therefore hit merging in all
C     three directions are now done in local sector coordinate system.
C     -- Apr 15, 1996  Dhammika W. --

C     check overlap in the z-direction
               if ( zlap .ge. zsizk ) then
C     kth hit is completely overlapped
                  zsiz = zsizl
                  zmin = xoutl(3) - tphit(l).zrf
                  zmax = xoutl(3) + tphit(l).zrf
               elseif ( zlap.ge.zsizl ) then
C     lth hit is completely overlapped
                  zsiz = zsizk
                  zmin = xoutk(3) - tphit(k).zrf
                  zmax = xoutk(3) + tphit(k).zrf
               else
C     sum of the full widths minus the overlap
                  zsiz = 2 * zwid - zlap
C     remember coors is sorted in z-direction -
C     kth hit was first
                  zmin = xoutk(3) - tphit(k).zrf
                  zmax = xoutl(3) + tphit(l).zrf
               endif
               
               
C     check overlap in the r-phi direction
               if( rlap.ge.rsizk ) then
C     kth hit is completely overlapped
                  rsiz = rsizl
                  rmin = xoutl(2) - tphit(l).prf
                  rmax = xoutl(2) + tphit(l).prf
               elseif ( rlap.ge.rsizl ) then
C     lth hit is completely overlapped
                  rsiz = rsizk
                  rmin = xoutk(2) - tphit(k).prf
                  rmax = xoutk(2) + tphit(k).prf
               else
C     sum of the full widths minus the overlap
                  rsiz = 2*rwid - rlap
C     in this case hits aren't sorted in any order ...
                  rmink = xoutk(2) - tphit(k).prf
                  rminl = xoutl(2) - tphit(l).prf
                  if ( rmink .lt. rminl ) then
                     rmin = rmink
                     rmax = xoutl(2) + tphit(l).prf
                  else
                     rmin = rminl
                     rmax = xoutk(2) + tphit(k).prf
                  endif
               endif
               
C     discard the kth hit (with larger hit number)
               tphit(k).flag = 8
C     mark hit with the id of the surviving hit
CCC   tphit(k).flag = -1. * tphit(l).id

C     and adjust the lth hit
               if ( rsiz.gt.rplim(ipad) .and. zsiz.gt.zlim ) then
C     merged lth hit too large - flag for discarding
                  tphit(l).flag = 11
               else
C     flag the lth hit, but don't discard it
                  tphit(l).flag = 1
               endif
C     add the amplitudes and quadrature the errors
               tphit(l).q = tphit(l).q + tphit(k).q
               tphit(l).dq = sqrt(tphit(l).dq**2 + tphit(k).dq**2)
               xoutl(2) = (rmax + rmin) / 2.
               xoutl(3) = (zmax + zmin) / 2.
               iret  = tgc_local_to_global(i,xoutl,xglob) 
               tphit(l).x = xglob(1)
               tphit(l).y = xglob(2)
               tphit(l).z = xglob(3)
               tphit(l).prf = rsiz / 2.
               tphit(l).zrf = zsiz / 2.
               tphit(l).dx = sqrt(tphit(l).dx**2 + tphit(k).dx**2)
               tphit(l).dy = sqrt(tphit(l).dy**2 + tphit(k).dy**2)
               
C     update the g2t_hit table for this hit (instead)
CCC   g2t_hit(tphit(k).id).recdig = tphit(l).id
C     >>>>use index table<<<< ; DSW 03/27/96
               
C     Search for row index  with tpc_index.key2=tphit(k).id
CCC   Replaced 10 in tls_Search_i by ii_rows, it has to be a variable
CCC   IMS 12/15/96
CCC   
               ii_rows=10
               iret = tls_Search_i(tpc_index_h.nok,tpc_index(1).key2, 
     >              tpc_index(2).key2,tphit(k).id,i_rows,ii_rows)
               
C     store the tpc_index table row# & tphit(l).id for this hit
C     04/16/1997  DSW
               irowl = i_rows(1)
               n_hit_merge = n_hit_merge + 1 
               if(n_hit_merge.le.max_hit_merge)then
                  imerge_id(n_hit_merge,1) =  irowl
                  imerge_id(n_hit_merge,2) =  tphit(l).id
               else
                  write(*,*)
     >                 ' total number of merged hits exceeds  ',
     >                 max_hit_merge
               endif
               
 100        continue
            
         enddo                  ! loop over rows
      enddo                     ! loop over sectors
      

C     Update  tphit.id in the index table for merged hits.
C     04/16/1997  DSW

      do i = 1, n_hit_merge
         irowl         = imerge_id(i,1)
         imerge_hit_id = imerge_id(i,2)
         tpc_index(irowl).key2 =  imerge_hit_id 
      enddo

      return
      end


C-----------------------------------------------------------------------
CDECK  ID>, TFSERR. 
	SUBROUTINE TFS_HIT_SMEAR( SECT, XI, YI, ZI, XO, YO, ZO,
     >                          ST_DEV_XY, ST_DEV_Z, ST_DEV_X, ST_DEV_Y)
        IMPLICIT NONE
C-----------------------------------------------------------------------
C     Input Arguments:
C       sect      - sector number
C       xi        - GEANT X coordinate
C       yi        - GEANT Y coordinate
C       zi        - GEANT Z coordinate
C       st_dev_xy - width of the signal in the transverse direction
C       st_dev_z  - width of the signal in the longitudinal direction
C
C     Output Arguments:
C       x0       - smeared XI
C       y0       - smeared YI
C       z0       - smeared ZI
C       st_dev_x - width of the signal in the x direction
C       st_dev_y - width of the signal in the y direction
C
C     Functional Description:
C	This subroutine will take as input a sector number for the tpc
C	(sect) and three coordinates for a space point (xi,yi,zi) in units
C	of centimeters and  the resolution in the transverse and drift
C       direction. It will then add a normally distributed error to
C	the coordinates for the space point, along the pad row direction
C	for the particular sector and in the electron drift direction(z).
C
C     Author:
C       Bill Christie   LBL
C
C     Modified by:
C       Peter G. Jones  LBL  (510)-486-5436
C
C     Creation Date:
C       09-Apr-1991
C
C     History:
C     Apr 09, 1991           Original by Peter Jones.
C     Apr 10, 1997           Modefied by Bill Love to set correct errors
C                            in hit smearing.
C     Jun 18, 1997           Modified by Dhammika W. to fix a bug in 
C                            z-coordinate  smearing.
C
C-----------------------------------------------------------------------
      REAL CPI, C2PI, CPI2, CLIGHT, ELMASS, CROOT2, DTORAD
      REAL BCONV
C
      PARAMETER ( CPI    = 3.141592654, 
     >            C2PI   = 6.283185307,
     >            CPI2   = 1.570796327,
     >            CROOT2 = 1.414213562, 
     >            DTORAD = 0.017453293,
     >            BCONV  = 0.002998,
     >            CLIGHT = 2.99792458E+10, 
     >            ELMASS = 0.5110034)
C
C     General physical constants
C     CPI = pi
C     C2PI = 2*pi
C     CPI2 = pi/2
C     CLIGHT = Velocity of light (cm/sec)
C     ELMASS = electron mass  (MeV)
C     CROOT2 = sqrt(2)
C     DTORAD = Conv. factor degrees to radians
C     BCFGEV = Conv. factor:     pt(GeV) = BCFGEV*radius(cm)
C     BCFMEV = Conv. factor:     pt(MeV) = BCFMEv*radius(cm)

      INTEGER	SECT

      REAL	XI,YI,ZI,XO,YO,ZO
      REAL	ERR_PAD		!ERROR ALONG PAD ROW FOR SPACE POINT
      REAL	ERR_DRFT	!ERROR ALONG DRIFT DIRECTION FOR SPACE POINT
      REAL	ST_DEV_XY	!SIGMA FOR X_Y ERROR (cm)
      REAL      ST_DEV_X
      REAL      ST_DEV_Y
      REAL	ST_DEV_Z	!SIGMA FOR Z ERROR   (cm)
      REAL      SMEAR           !Random no. gaussian distrib. unit sigma
      REAL      RDUMMY
      REAL      PHI
      real      xlocal(3),xglobal(3)
      integer   iret
      integer   tgc_local_to_global, tgc_global_to_local

C=========================== Begin Executable Code =======================

        CALL RANNOR(SMEAR,RDUMMY)
        ERR_PAD = ST_DEV_XY*SMEAR
        CALL RANNOR(SMEAR,RDUMMY)
        ERR_DRFT = ST_DEV_Z*SMEAR
        xglobal(1)=xi
        xglobal(2)=yi
        xglobal(3)=zi
        iret=tgc_global_to_local(sect,xglobal,xlocal)
        xlocal(1)=xlocal(1)+err_pad
        iret=tgc_local_to_global(sect,xlocal,xglobal)
        xo=xglobal(1)
        yo=xglobal(2)
        zo=zi+err_drft
C     Make sure that z coordinate remains in the same hemisphere after the 
C     smearing to maintain the correct sector number corresponding this hit.
C     --- DSW Jun 18, 1997  --- 
        if(zi.gt.0.0.and.zo.lt.0.0)then
           zo = zi + abs(err_drft)
        elseif(zi.lt.0.0.and.zo.gt.0.0)then
           zo = zi - abs(err_drft)
        endif
        xlocal(1)=st_dev_xy
        xlocal(2)=0
        xlocal(3)=0
        iret=tgc_local_to_global(sect,xlocal,xglobal)
        ST_DEV_X = abs(xglobal(1))
        ST_DEV_Y = abs(xglobal(2))
	RETURN
	END



C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

