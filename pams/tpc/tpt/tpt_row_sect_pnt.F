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
      RECORD /sort_hit/         tpsort(2000)
C______________________________________________________________________
      INTEGER tpstart(mxrow,mxsector),tpcount(mxrow,mxsector),loc_hit(*)
      INTEGER irow,isect,i,inew,j,k,ipos,ic,itim,ntmp,ipnt,ipass
      INTEGER next_hit(tpt_mxhit),slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic),loc_hit_tmp(2000)
      INTEGER iret, tls_Index_Sort_r,nhalf,itim1,ipos1,kc

      REAL    dzslic,z_hit

C______________________________________________________________________
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
      if( tpar(ipass).nzslic .lt. 2  ) tpar(ipass).nzslic = 2
      if( (2*tpar(ipass).nzslic +1).gt. mxslic )
     > tpar(ipass).nzslic = (mxslic-1)/2
      ntmp = tpar(ipass).nzslic/2
      dzslic =225.0/real(ntmp)
      nhalf=ntmp*2
      inew = -1
      do i=1,hit_h.nok
C       loop through row-sorted list
        ipnt = loc_hit(i)
        if(hit(ipnt).track.le.0) then
          isect = hit(ipnt).row / 100
          irow = hit(ipnt).row - isect*100
          if( isect.lt.1 .or. isect.gt.mxsector .or. irow.lt.1 .or.
     >    irow.gt.mxrow ) then
            write(6,*) ' mess in  tpt_row_sect_pnt'
          else
            if( hit(ipnt).row .eq. inew ) then
               tpcount(irow,isect) = tpcount(irow,isect) + 1
            else
               tpcount(irow,isect) = 1
               tpstart(irow,isect) = i
               inew = hit(ipnt).row
            endif
          endif
        endif
      end do
      do i=1,mxsector
         do j=1,mxrow
           ipnt= tpstart(j,i)
           ic = tpcount(j,i)
           if(ic.gt.0.and.ic.le.2000) then
            k=0
            kc=0
            do while (k.lt.ic)
               if(hit(loc_hit(ipnt+kc)).track.le.0) then
                  k=k+1
                  tpsort(k).ipnt=loc_hit(ipnt+kc)
                  tpsort(k).z=hit(loc_hit(ipnt+kc)).z
                  if(ipass.eq.2)tpsort(k).z=-hit(loc_hit(ipnt+kc)).z
               endif
               kc=kc+1
            end do
CCCCCCCCCCCC
CCCCCCCCCCCC      TEMPORARY FIX
CCCCCCCCCCCC
            if(ic.eq.1) then
            loc_hit_tmp(1)=1
            else
            iret=tls_Index_Sort_r(ic,tpsort(1).z,tpsort(2).z,
     >      loc_hit_tmp,ic)
            endif
            do k=1,ic
               loc_hit(ipnt+k-1)=tpsort(loc_hit_tmp(k)).ipnt
               if((k+1).gt.ic) then
                  next_hit(loc_hit(ipnt+k-1))=0
               else
                  next_hit(loc_hit(ipnt+k-1))=
     >            tpsort(loc_hit_tmp(k+1)).ipnt
               endif
               z_hit=hit(loc_hit(ipnt+k-1)).z/dzslic
               itim = 2*(int(abs(z_hit)) + 1.0)
               if( z_hit.lt.0.) itim = itim + nhalf
               if(abs(z_hit)-int(abs(z_hit)).lt.0.5) then
                  itim1=itim-1
               else
                  itim1=itim+1
               endif
               if(z_hit.lt.0.and.itim1.eq.nhalf+1) itim1=1
               ipos = slice_loc(i,j,itim)
               ipos1=  slice_loc(i,j,itim1)
               slice_pnt(2,ipos) = slice_pnt(2,ipos) + 1
               slice_pnt(2,ipos1) = slice_pnt(2,ipos1) + 1
               if(slice_pnt(2,ipos).eq.1)slice_pnt(1,ipos)=
     >         loc_hit(ipnt+k-1)
               if(slice_pnt(2,ipos1).eq.1)slice_pnt(1,ipos1)=
     >         loc_hit(ipnt+k-1)
            end do
           else if(ic.gt.2000) then
              write(6,*) 'we are in trouble'
           endif
         end do
      end do
      tpt_row_sect_pnt=0
      end
