* $Id: tpt_row_sect_pnt.F,v 1.8 1998/09/04 14:44:19 sakrejda Exp $
* $Log: tpt_row_sect_pnt.F,v $
* Revision 1.8  1998/09/04 14:44:19  sakrejda
* sector assignement for poorly reconstructed hits around sector edges fixed
*
* Revision 1.7  1998/09/01 13:32:39  sakrejda
* restored sector numbering 1-24 in tphit.row
*
* Revision 1.6  1998/07/12 05:29:07  sakrejda
* space in local array increased
*
* Revision 1.5  1998/05/26 04:15:13  sakrejda
* message type set to E to limit printing
*
* Revision 1.4  1998/04/04 14:35:36  fisyak
* Increase message-generating space
*
* Revision 1.3  1998/02/11 19:32:56  sakrejda
* tpt protected agains overflows, tpt_find_slic.F,tpt_row_sect_pnt.F simplified (and tested)
*
* Revision 1.2  1998/01/27 00:35:53  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_ROW_SECT_PNT(hit_h,hit,loc_hit,tpstart,
     >                                  tpcount,tpar_h,tpar,
     >                                  next_hit,slice_loc,slice_pnt,
     >                                  ipass )
C>---------------------------------------------------------------------
C
C TPT_ROW_SECT_PNT
C
C DESCRIPTION:
C
C INPUT ARGUMENTS:
C
C OUTPUT ARGUMENTS:
C
C AUTHOR:
C
C<---------------------------------------------------------------------

      IMPLICIT NONE
#include "PAM.inc"

#include "tcl_tphit.inc"
#include "tpt_pars.inc"
#include "tpt_mxdim.inc"
#include "tpt_mxhit.inc"

      STRUCTURE /sort_hit/
          INTEGER*4 ipnt
          REAL*4    z
      END STRUCTURE

C_____________________________________________________________________

      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/    tpar(*)
      RECORD /sort_hit/         tpsort(4000)
C______________________________________________________________________
      INTEGER tpstart(mxrow,mxsector),tpcount(mxrow,mxsector),loc_hit(*)
      INTEGER irow,isect,i,inew,j,k,ipos,ic,itim,ntmp,ipnt,ipass
      INTEGER next_hit(tpt_mxhit),slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic),loc_hit_tmp(4000)
      INTEGER iret, tls_Index_Sort_r,nhalf,itim1,ipos1,kc

      INTEGER rhit(tpt_mxhit)
      integer isec2, isec3, isect
      INTEGER tls_Index_Sort_i
C             sort hits in the TPC table according to row/sec field
      real    xglobal(3)

      INTEGER tgc_global_to_sector
      INTEGER tgc_sec24_to_sec12
      INTEGER tgc_drift_volume_length
      INTEGER id1,idI1
      REAL    dzslic, z_hit, dvl, dummy
      character*256 m132  ! And some message-generating space.
      data id1, idI1 /2*0/
      save id1, idI1

C______________________________________________________________________
      do i=1,hit_h.nok
C        clean up sector assignement.
         xglobal(1)=hit(i).x
         xglobal(2)=hit(i).y
         xglobal(3)=hit(i).z
c        check what xy and z says about the sector.
         iret=tgc_global_to_sector(isec2,xglobal)
         isec3=int(hit(i).row/100)
         if(isec3.ne.isec2) then
           write(m132,*) 'TPTI1-E1 sector missmatch',isec3,isec2,xglobal
           call message(m132,1,idI1)
           isec3=isec2
c          repair it
           hit(i).row = isec3*100+mod(hit(i).row,100)
         endif
         if(xglobal(3).gt.0) then
            rhit(i)=hit(i).row
         else
            iret=tgc_sec24_to_sec12(isec3,isect)
            rhit(i) = isect*100+mod(hit(i).row,100)
         endif
      end do

      if(hit_h.nok.eq.1)then
c       if there is just one hit, sorting routine has problems,
c       so deal with it separately
        loc_hit(1)=1
      else
        iret=tls_Index_Sort_i(hit_h.nok,rhit(1),rhit(2),loc_hit,
     >  tpt_mxhit)
      endif

      call Msg_Enable( 'TPT-RSP' )
C     set up information about the slices
      ic = 0
      do i=1,mxsector
         do j=1,mxrow
            tpcount(j,i) = 0
            tpstart(j,i) = 0
            do k=1,mxslic
              ic = ic + 1
              slice_loc(i,j,k) = ic
              slice_pnt(1,ic)  = 0
              slice_pnt(2,ic)  = 0
            end do
         end do
      end do
*
*     make sure that not too many slices were asked for
      if( tpar(ipass).nzslic .lt. 2  ) tpar(ipass).nzslic = 2
      if( (2*tpar(ipass).nzslic +1).gt. mxslic )
     > tpar(ipass).nzslic = (mxslic-1)/2
*
*     estimate size of a z slice
*     first get the TPC drift volume length
      iret=tgc_drift_volume_length(dvl,dummy,dummy)
      ntmp = tpar(ipass).nzslic/2
      dzslic =dvl/real(ntmp)
      nhalf=ntmp*2

*
*     loop through the sorted list and establish pointers to the 
*     sorted pointer array.
      inew = -1
      do i=1,hit_h.nok
C       loop through row-sorted list
        ipnt = loc_hit(i)
        if(hit(ipnt).track.le.0) then
          isect = rhit(ipnt)/100
          irow = mod(rhit(ipnt),100)
          if( isect.lt.1 .or. isect.gt.mxsector .or. irow.lt.1 .or.
     >    irow.gt.mxrow ) then
          write(m132,*) ' TPTRSP-E1 row,sector info invalid ',
     >    isect,irow,mxsector
          call Message( m132, 1, id1 )
          else
            if( rhit(ipnt).eq. inew ) then
               tpcount(irow,isect) = tpcount(irow,isect) + 1
            else
               tpcount(irow,isect) = 1
               tpstart(irow,isect) = i
               inew = rhit(ipnt)
            endif
          endif
        endif
      end do

*     within one row/sector sort hits in time
      do i=1,mxsector
         do j=1,mxrow
           ipnt= tpstart(j,i)
           ic = tpcount(j,i)
           if(ic.gt.0.and.ic.le.4000) then
            k=0
            kc=0
            do while (k.lt.ic)
*              check whether this hit is available 
*              want to eliminate used hits in subsequent passes
               if(hit(loc_hit(ipnt+kc)).track.le.0) then
                  k=k+1
*
*                 tpsort(k).ipnt is a location of the hit in the hit table
*
                  tpsort(k).ipnt=loc_hit(ipnt+kc)
                  tpsort(k).z=hit(loc_hit(ipnt+kc)).z
*
*                 for the second pass hits are sorted in a reversed order
                  if(ipass.eq.2)tpsort(k).z=-hit(loc_hit(ipnt+kc)).z
               endif
               kc=kc+1
            end do
*
*           index_sort has trouble sorting a structure with 1 entry....
            if(ic.eq.1) then
              loc_hit_tmp(1)=1
            else
              iret=tls_Index_Sort_r(ic,tpsort(1).z,tpsort(2).z,
     >        loc_hit_tmp,ic)
            endif

*           now re-arange pointers in loc_hit so that hits are also sorted in z
*           for each row in each sector...
*
*           for each hit set a pointer directing  to the next one
*           the last in a row/sector does not point anywhere.
*
*           subdivide z into overlapping slices

            do k=1,ic
               loc_hit(ipnt+k-1)=tpsort(loc_hit_tmp(k)).ipnt
               if((k+1).gt.ic) then
                  next_hit(loc_hit(ipnt+k-1))=0
               else
                  next_hit(loc_hit(ipnt+k-1))=
     >            tpsort(loc_hit_tmp(k+1)).ipnt
               endif
*
*              divide z position by the slice width
               z_hit=(hit(loc_hit(ipnt+k-1)).z+dvl)/dzslic
*
*              take integer of that, add one and multiply by two
*              so we get only even numbers
               itim = 2*(int(abs(z_hit)) + 1.0)
*
*              create a set of slices shifted by half a slice width.
               if(abs(z_hit)-int(abs(z_hit)).lt.0.5) then
                  itim1=itim-1
               else if(itim.lt.mxslic) then
                  itim1=itim+1
               else
                  itim1=itim
               endif
*
               ipos = slice_loc(i,j,itim)
               ipos1=  slice_loc(i,j,itim1)
               slice_pnt(2,ipos) = slice_pnt(2,ipos) + 1
               slice_pnt(2,ipos1) = slice_pnt(2,ipos1) + 1
               if(slice_pnt(2,ipos).eq.1)slice_pnt(1,ipos)=
     >         loc_hit(ipnt+k-1)
               if(slice_pnt(2,ipos1).eq.1)slice_pnt(1,ipos1)=
     >         loc_hit(ipnt+k-1)
            end do
           else if(ic.gt.4000) then
               write(m132,*) 'TPTRSP-E1 not enough storage to sort data in z',ic
               call Message( m132, 1, id1 )
           endif
         end do
      end do
      write(m132,*) ' TPTRSP-E1 done setting up pointers'
      call Message( m132, 1, id1 )
      tpt_row_sect_pnt=0
      end
c
