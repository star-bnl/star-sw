* $Id: tfs_tpchit_merge.F,v 1.6 1998/09/18 21:56:51 perev Exp $
* $Log: tfs_tpchit_merge.F,v $
* Revision 1.6  1998/09/18 21:56:51  perev
* WRONG calling sequence for tls_sort
*
* Revision 1.5  1998/07/30 12:34:01  sakrejda
* loop index taken out from calls to the tgc functions
*
* Revision 1.4  1998/07/28 12:54:47  sakrejda
* problem fixed following Herb's suggestion
*
* Revision 1.3  1998/04/07 20:56:34  hardtke
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
* Revision 1.2  1998/04/02 20:57:23  sakrejda
* check error condition on return from tls_Quick_Sort_i
*
* Revision 1.1  1998/01/27 01:47:24  fisyak
* Split sources
*
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
#include "tls_ecvalues.inc"
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
      integer IsectorDummy
      integer idI1, idI2, idI3
      character*132 m132  ! And some message-generating space.
      data idI1, idI2, idI3 /0, 0, 0/
      save idI1, idI2, idI3
C=========================== Begin Executable Code =======================

C     Initialize imerge_id.  04/16/1997   DSW.
      call vzero(imerge_id,2*max_hit_merge)
      n_hit_merge = 0

C     Quick_sort tpc_index table for key2 so it's easy to find  right
C     geant hit corresponding to given tpc hit. This is needed to update
C     the tpc_index table after tpc hit merging.

      iret = tls_Quick_Sort_i(tpc_index_h.nok,tpc_index(1).key2,
     >     tpc_index(2).key2, tpc_index)
      if(iret.ne.tls_sort_normal_cv)then
           write(m132,*) 'TFS-MERGE-I2 sorting failed ',iret
           call Message( m132, 1, idI2 )
           return
      else
           write(m132,*) 'TFS-MERGE-I2 sorting succesfull',iret
           call Message( m132, 1, idI2 )
      endif


C     loop over sector-rows
      do i = 1, par(1).nsect
         do j = 1, par(1).nrow
*
*     This is a very ugly patch.....
            if(i.gt.24) go to 666
C     chose pad size
            ipad = 2
            if (j .le. par(1).irmax) ipad = 1
            
C     Sort each sector-row in time
            istart = irow(1,j,i)
            iend   = irow(1,j,i) + irow(2,j,i) - 1
            iret   = tls_Quick_Sort_r(irow(2,j,i),tphit(istart).z,
     >           tphit(istart+1).z,tphit(istart))
            
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
                     dr    = ( tphit(k).x - tphit(l).x )**2 +
     >                    ( tphit(k).y - tphit(l).y )**2 
                     if (dr.ge.0.) then
                      dr    = sqrt( dr )
                     else
                      write(m132,*) 'TFS-MERGE-I3 SQRT of negative number '
     >                          //'set to 0.0  '  
                      call Message( m132, 1, idI3 )
                      dr = 0.
                     endif
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
               dr    = ( tphit(k).x - tphit(l).x )**2 +
     >                 ( tphit(k).y - tphit(l).y )**2 
                     if (dr.ge.0.) then
                      dr    = sqrt( dr )
                     else
                      write(m132,*) 'TFS-MERGE-I3 SQRT of negative number '
     >                          //'set to 0.0  '  
                      call Message( m132, 1, idI3 )
                      dr = 0.
                     endif
               dz    = abs( tphit(k).z - tphit(l).z )
               zwid = tphit(k).zrf + tphit(l).zrf
               rwid = tphit(k).prf + tphit(l).prf
               zlap = zwid - dz
               rlap = rwid - dr
               
C     rotate into local sector coord system
               xglob(1) = tphit(k).x
               xglob(2) = tphit(k).y
               xglob(3) = tphit(k).z
               iret  = tgc_global_to_local(IsectorDummy,xglob,xoutk)
               xglob(1) = tphit(l).x
               xglob(2) = tphit(l).y
               xglob(3) = tphit(l).z
               iret  = tgc_global_to_local(IsectorDummy,xglob,xoutl)

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
               tphit(l).dq = tphit(l).dq**2 + tphit(k).dq**2
                     if (tphit(l).dq.ge.0.) then
                      tphit(l).dq = sqrt( tphit(l).dq )
                     else
                      write(m132,*) 'TFS-MERGE-I3 SQRT of negative number '
     >                          //'set to 0.0  '  
                      call Message( m132, 1, idI3 )
                      tphit(l).dq = 0.
                     endif
               xoutl(2) = (rmax + rmin) / 2.
               xoutl(3) = (zmax + zmin) / 2.
               IsectorDummy=i
               iret  = tgc_local_to_global(IsectorDummy,xoutl,xglob) 
               tphit(l).x = xglob(1)
               tphit(l).y = xglob(2)
               tphit(l).z = xglob(3)
               tphit(l).prf = rsiz / 2.
               tphit(l).zrf = zsiz / 2.
               tphit(l).dx = tphit(l).dx**2 + tphit(k).dx**2
                     if (tphit(l).dx.ge.0.) then
                      tphit(l).dx = sqrt( tphit(l).dx )
                     else
                      write(m132,*) 'TFS-MERGE-I3 SQRT of negative number '
     >                          //'set to 0.0  '  
                      call Message( m132, 1, idI3 )
                      tphit(l).dx = 0.
                     endif
               tphit(l).dy = tphit(l).dy**2 + tphit(k).dy**2
                     if (tphit(l).dy.ge.0.) then
                      tphit(l).dy = sqrt( tphit(l).dy )
                     else
                      write(m132,*) 'TFS-MERGE-I3 SQRT of negative number '
     >                          //'set to 0.0  '  
                      call Message( m132, 1, idI3 )
                      tphit(l).dy = 0.
                     endif
               
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
                  write(m132,*) 'TFS-MERGE-I1 total number of merged '
     >                          //'hits exceeds  ' ,max_hit_merge 
                  call Message( m132, 1, idI1 )
               endif
               
 100        continue
            
         enddo                  ! loop over rows
      enddo                     ! loop over sectors
*     jump here if the index gets past the loop limit 
*     (the end of that ugly patch)
 666  continue

C     Update  tphit.id in the index table for merged hits.
C     04/16/1997  DSW

      do i = 1, n_hit_merge
         irowl                 = imerge_id(i,1)
         tpc_index(irowl).key2 = imerge_id(i,2) 
      enddo

      return
      end
