      INTEGER FUNCTION TPT_MERGE_SEGMENTS(track_h,track,trk,hit_h,hit,
     >   tpar,tpar_h,ipass)
C>---------------------------------------------------------------------
C
C TPT_MERGE_SEGMENTS - merges pieces of a track
C
C DESCRIPTION:
C 
C INPUT ARGUMENTS:
C OUTPUT ARGUMENTS:
C<---------------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE
#include "PAM.inc"

#include "tpt_track.inc"
#include "tpt_track_pointers.inc"
#include "tpt_mxdim.inc"
#include "tcl_tphit.inc"
#include "tpt_pars.inc"
#include "math_constants.inc"
C______________________________________________________________________
c
c Define local structure to sort radius
      STRUCTURE /track_merge/
                integer lpnt           ! location of a track with given radius
                real    rad            ! radius of a track
                real    xclose(2)      ! extrapolation to position where a line
                real    yclose(2)      !   from vertex crosses perpendicular 
                real    z(2)           ! z(1) < z(2)
                real    xcenter        ! center of circle
                real    ycenter
                real    zcenter        ! center z piont on track not circle
                real    s(2)    ! xy distance between closest point and xclose
      END STRUCTURE
C______________________________________________________________________
      RECORD /table_head_st/    track_h
      RECORD /tpt_track_st/     track(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
      RECORD /tpt_track_pointers_st/   trk(*)
      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)

      RECORD /track_merge/      merge(mxtrack)
C_________________________________________________________________
      INTEGER ltot,tls_Quick_Sort_r,iret
      INTEGER i,j,k,l,m,count,ipass, lxi,lxj,hitpnt
      INTEGER lstep1, lstep2, lstart1, lstart2, lx1, lx2
      REAL    mins(8),cuta,cutb,cutc,cutd,z1,z2
      INTEGER tpt_fit_track, tpt_load_track
      REAL    temp,in_vec(2),out_vec(2),temp_vec(2),angle
      INTEGER radi,event, dummy,num_hits
      LOGICAL good_dpt,test_both 
      data    event /0/
      CHARACTER*132 m132
      INTEGER test1, test2
      INTEGER merged(1000,30)          ! merged initially -1 
C______________________________________________________________________

c Order the good tracks according to their helix radius.
c The array lpnt will point to each of these tracks in order.
C      write(17,*)"****************** event ",event," ***************"
      event = event+1
C      if(event.eq.4) return

	
      do i=1,1000
	merged(i,1)=-1
      enddo

      ltot = 0
      do i=1,track_h.nok
c only look at low pt tracks radius.lt.1.5 meters
         if(abs(trk(i).p(2)).lt.150.and.trk(i).flag.ge.0) then
            ltot=ltot+1
            if(ltot.gt.mxtrack) then
               write(6,*)"ltot too big",ltot,mxtrack
               return
            endif
            merge(ltot).rad=abs(trk(i).p(2))
            merge(ltot).lpnt=i
            merge(ltot).xclose(1)=trk(i).p(3)*
     >                            (1.0+merge(ltot).rad/sqrt(trk(i).p(3)**2+
     >                            trk(i).p(4)**2))
            merge(ltot).xclose(2)=trk(i).p(3)*
     >                            (1.0-merge(ltot).rad/sqrt(trk(i).p(3)**2+
     >                            trk(i).p(4)**2))
            merge(ltot).yclose(1)=trk(i).p(4)*
     >                            (1.0+merge(ltot).rad/sqrt(trk(i).p(3)**2+
     >                            trk(i).p(4)**2))
            merge(ltot).yclose(2)=trk(i).p(4)*
     >                            (1.0-merge(ltot).rad/sqrt(trk(i).p(3)**2+
     >                            trk(i).p(4)**2))
            if((merge(ltot).xclose(1)**2+merge(ltot).yclose(1)**2).gt.
     >      (merge(ltot).xclose(2)**2+merge(ltot).yclose(2)**2)) then
               temp=merge(ltot).xclose(1)
               merge(ltot).xclose(1)=merge(ltot).xclose(2)
               merge(ltot).xclose(2)=temp
               temp=merge(ltot).yclose(1)
               merge(ltot).yclose(1)=merge(ltot).yclose(2)
               merge(ltot).yclose(2)=temp
            endif

            merge(ltot).xcenter = trk(i).p(3)
            merge(ltot).ycenter = trk(i).p(4)

            in_vec(1)=(merge(ltot).xclose(1)-merge(ltot).xcenter)
            in_vec(2)=(merge(ltot).yclose(1)-merge(ltot).ycenter)
            out_vec(1)=(merge(ltot).xclose(2)-merge(ltot).xcenter)
            out_vec(2)=(merge(ltot).yclose(2)-merge(ltot).ycenter)
            num_hits =trk(i).nhit
            temp_vec(1)=hit(trk(i).ipnt(num_hits)).x-merge(ltot).xcenter
            temp_vec(2)=hit(trk(i).ipnt(num_hits)).y-merge(ltot).ycenter

            angle=sqrt((in_vec(1)**2+in_vec(2)**2)*(temp_vec(1)**2+temp_vec(2)**2))
            angle = asin((in_vec(1)*temp_vec(2) - in_vec(2)*temp_vec(1))/angle)
            angle=abs(angle)
            if((in_vec(1)*temp_vec(1) + in_vec(2)*temp_vec(2)).lt.0) angle = c_PI-angle
            merge(ltot).s(1)=merge(ltot).rad*angle
            merge(ltot).z(1)=hit(trk(i).ipnt(num_hits)).z - merge(ltot).s(1)*trk(1).p(7)

            temp_vec(1)=(hit(trk(i).ipnt(1)).x-merge(ltot).xcenter)
            temp_vec(2)=(hit(trk(i).ipnt(1)).y-merge(ltot).ycenter)
            
            angle=sqrt((out_vec(1)**2+out_vec(2)**2)*(temp_vec(1)**2+temp_vec(2)**2))
            angle=asin((out_vec(1)*temp_vec(2) - out_vec(2)*temp_vec(1))/angle)
            angle=abs(angle)
           if((out_vec(1)*temp_vec(1) + out_vec(2)*temp_vec(2)).lt.0) angle = c_PI-angle
            merge(ltot).s(2)=merge(ltot).rad*angle
            merge(ltot).z(2)=hit(trk(i).ipnt(1)).z + merge(ltot).s(2)*trk(i).p(7)

            if(merge(ltot).z(1).gt.merge(ltot).z(2)) then
               temp=merge(ltot).z(1)
               merge(ltot).z(1)=merge(ltot).z(2)
               merge(ltot).z(2)=temp
               temp=merge(ltot).s(1)
               merge(ltot).s(1)=merge(ltot).s(2)
               merge(ltot).s(2)=temp
            endif
            merge(ltot).zcenter=.5*(hit(trk(i).ipnt(1)).z+hit(trk(i).ipnt(num_hits)).z)
            
C            write(17,*) i,num_hits,merge(ltot).rad,angle,trk(i).p(7),
C     >          merge(ltot).xclose(1),merge(ltot).xclose(2),
C     >          merge(ltot).yclose(1),merge(ltot).yclose(2),
C     >          merge(ltot).xcenter,merge(ltot).ycenter,merge(ltot).zcenter,
C     >          merge(ltot).s(1),merge(ltot).s(2),merge(ltot).z(1),merge(ltot).z(2)

         endif
      enddo
C      go to 999
      iret=tls_Quick_Sort_r(ltot, merge(1).rad, merge(2).rad, merge)


c     Minimum difference values for merging tracks
c     Adjust these parameters to change the merging criteria.
      mins(1)=0.1   ! radius abs(a-b)/a
      mins(2)=0.2   ! dip abs(a-b)/a
      mins(3)=6.0   ! x and y in abs(a-b)
      mins(4)=6.0   ! x and y out abs(a-b)
      mins(5)=6.0   ! x, y and z center abs(a-b)
      mins(6)=6.0   ! z  abs(a-b)
      mins(7)=1     ! xy vertex spead
      mins(8)=10    ! z vertex spead

c     Loop over track radii in order. Check if radii match. Go on if yes.
c     radd is the larger of the radii being compared.

      count = 0
      do i=1,ltot-1
        j=i+1
        lxi=merge(i).lpnt
        radi = merge(i).rad
        good_dpt = abs((radi-merge(j).rad)/radi).lt.mins(1)

c      "test_both" is used to check both inner and outer 
c         extrapolations in x and y for primary vertex tracks
        test_both = .false.
        if(merge(i).xclose(1).lt.mins(7).and.merge(i).yclose(1).lt.mins(7).and.
     >    (abs(merge(i).z(1)).lt.mins(8).or.abs(merge(i).z(2)).lt.mins(8))) then
                   test_both=.true.
        endif


        do while(good_dpt)
           lxj=merge(j).lpnt
C           write(17,*)"trying to merge", lxi, lxj
           good_dpt = abs((radi-merge(j).rad)/radi).lt.mins(1)
           
           if(good_dpt) then
C              write(17,*)"trying to merg, pt cut passed", lxi, lxj
              if(trk(lxi).p(7)*trk(lxj).p(7).lt.0) then
                 cuta = abs(merge(i).z(1)-merge(j).z(2))
                 cutb = abs(merge(i).z(2)-merge(j).z(1))
              else
                  cuta = abs(merge(i).z(1)-merge(j).z(1))
                  cutb = abs(merge(i).z(2)-merge(j).z(2))
              endif


              if(cuta.lt.mins(6).or.cutb.lt.mins(6)) then
C                write(17,*)"z cut passed", lxi, lxj

                cuta = abs(merge(i).xclose(1)-merge(j).xclose(1))
                cutb = abs(merge(i).yclose(1)-merge(j).yclose(1))
                cutc = abs(merge(i).xclose(2)-merge(j).xclose(2))
                cutd = abs(merge(i).yclose(2)-merge(j).yclose(2))

                if( (((cuta.lt.mins(3).and.cutb.lt.mins(3)).or.
     >           (cutc.lt.mins(4).and.cutd.lt.mins(4))).and..not.test_both).or.
     >                  (cuta.lt.mins(3).and.cutb.lt.mins(3).and.
     >            cutc.lt.mins(4).and.cutd.lt.mins(4).and.test_both)) then
C                  write(17,*)"in and/or out cut passed", lxi, lxj

                  cuta = abs(merge(i).xcenter-merge(j).xcenter)
                  cutb = abs(merge(i).ycenter-merge(j).ycenter)
     
                  if(cuta.lt.mins(5).and.cutb.lt.mins(5)) then
C                     write(17,*)"xy center cut passed",lxi,lxj
c     Now compare their dip angles
                     cuta=abs(abs(trk(lxi).p(7)) - abs(trk(lxj).p(7)))/abs(trk(lxi).p(7))
                     if(cuta.lt.mins(2)) then
C                        write(17,*)" merging, dip cut passed", lxi, lxj
                        test1=0
		        test2=0
                        if(count.ne.0) then
                          do k=1,count
                            do l=2,merged(k,1)+1
	  		      if(merged(k,l).eq.lxi) test1=k
		              if(merged(k,l).eq.lxj) test2=k
		            enddo
			  enddo
                        endif
                        if(test1.eq.test2.and.test1.ne.0) then
C                          write(17,*)"they belong to the same merger,",
c     >                           test1,"equals", test2
c                          write(17,*) "merged vaules",
c     >                           merged(test1,1),merged(test1,2),
c     >                           merged(test1,3),merged(test1,4),
c     >                           merged(test1,5),merged(test1,6),
c     >                           merged(test1,7),merged(test1,8)
                        else
		          if(test1.eq.0) then
			    if(test2.eq.0) then
		    	      if(count.eq.1000) then
			        count=count - 1
c                                write(17,*)"over 1000 mergers"
 			      endif
                              z1=hit(trk(lxi).ipnt(1)).z +
     >                                    hit(trk(lxi).ipnt(trk(lxi).nhit)).z
                              z2=hit(trk(lxj).ipnt(1)).z +
     >                                    hit(trk(lxj).ipnt(trk(lxj).nhit)).z
                              if(z1.gt.z2) then
                                 temp = lxi
                                 lxi = lxj
                                 lxj = temp
                              endif
			      count = count + 1
			      merged(count,1)=2
			      merged(count,2)=lxi
			      merged(count,3)=lxj
c                              write(17,*) "new merge", count , lxi, lxj
c                              write(17,*) "merged vaules",
c     >                             merged(count,1),merged(count,2),
c     >                             merged(count,3)
			    else
			      if(merged(test2,1).eq.29) then
c			        write(17,*)"need larger arrays 29 track merged"
			        merged(test2,1)=merged(test2,1)-1
			      endif
                              l=1
                              z1=1
                              z2=0
                              do while(z1.gt.z2)
                                l=l+1
                                z1=hit(trk(lxi).ipnt(1)).z +
     >                                    hit(trk(lxi).ipnt(trk(lxi).nhit)).z
                                z2=hit(trk(merged(test2,l)).ipnt(1)).z +
     >                               hit(trk(merged(test2,l)).
     >                               ipnt(trk(merged(test2,l)).nhit)).z
                                if(l.gt.merged(test2,1)) then
                                   if(z1.gt.z2) l=l+1
                                   z2=z1+1
                                endif
                              enddo
c         sort in z
                              k=0
                              do while(l.le.(merged(test2,1)+1-k))
                                 merged(test2, merged(test2,1)+2-k)=
     >                               merged(test2,merged(test2,1)+1-k)
                                 k=k+1
                              enddo
			      merged(test2,1)=merged(test2,1) + 1
			      merged(test2,l)=lxi
c         done with l
c                              write(17,*) "adding", lxi, "to  merge", test2 ,
c     >                                   "which contains", lxj
c                              write(17,*) "merged vaules",
c     >                             merged(test2,1),merged(test2,2),
c     >                             merged(test2,3),merged(test2,4),
c     >                             merged(test2,5),merged(test2,6),
c     >                             merged(test2,7),merged(test2,8)
			    endif
		          else
		  	    if(test2.eq.0) then
			      if(merged(test1,1).eq.29) then
c			        write(17,*)"need larger arrays 29 track merged"
			        merged(test1,1)=merged(test1,1) - 1
			      endif
                              l=1
                              z1=1
                              z2=0
                              do while(z1.gt.z2)
                                l=l+1
                                z1=hit(trk(lxj).ipnt(1)).z +
     >                                    hit(trk(lxj).ipnt(trk(lxj).nhit)).z
                                z2=hit(trk(merged(test1,l)).ipnt(1)).z +
     >                               hit(trk(merged(test1,l)).
     >                               ipnt(trk(merged(test1,l)).nhit)).z
                                if(l.gt.merged(test1,1)) then
                                  if(z1.gt.z2) l=l+1
                                  z2=z1+1
                                endif
                              enddo
c         sort in z
c			      write(17,*)"l",l
                              k=0
                              do while(l.le.(merged(test1,1)+1-k))
                                 merged(test1, merged(test1,1)+2-k)=
     >                               merged(test1,merged(test1,1)+1-k)
                                 k=k+1
                              enddo
			      merged(test1,1)=merged(test1,1) + 1
			      merged(test1,l)=lxj
c         done with l                              
c                              write(17,*) "adding", lxj," to merge", test1 ,
c     >                                " which contains ", lxi
c                              write(17,*) "merged vaules",
c     >                                merged(test1,1),merged(test1,2),
c     >                                merged(test1,3),merged(test1,4),
c     >                                merged(test1,5),merged(test1,6),
c     >                                merged(test1,7),merged(test1,8)
			    else
			      do k=2 , merged(test2,1)+1
			        temp = 0
                                m=merged(test1,1)+2
			        do l=2 , merged(test1,1)+1
      			          if(merged(test1,1).eq.merged(test2,k)) then
		 	            temp = 1
                                  else
                                    z1=hit(trk(merged(test2,k)).ipnt(1)).z +
     >                                  hit(trk(merged(test2,k)).
     >                                  ipnt(trk(merged(test2,k)).nhit)).z
                                    z2=hit(trk(merged(test1,l)).ipnt(1)).z +
     >                                  hit(trk(merged(test1,l)).
     >                                  ipnt(trk(merged(test1,l)).nhit)).z
                                    if(z1.lt.z2) m=l
                                  endif
      		 	        enddo
			        if(temp.eq.0) then
 			          if(merged(test1,1).eq.29) then
c 			            write(17,*)"larger arrays 29 track merged"
			            merged(test1,1)=merged(test1,1)-1
			          endif
                                  l=0
                                  do while(m.le.(merged(test1,1)+1-l))
                                    merged(test1, merged(test1,1)+2-l)=
     >                                 merged(test1,merged(test1,1)+1-l)
                                    l=l+1
                                  enddo
			          merged(test1,1)=merged(test1,1)+1
			          merged(test1,m)=merged(test2,k)
c                                  write(17,*) "putting",merged(test2,k),"of"
c     >                                       ,test2,"into",test1
c done with k,m
			        endif
			      enddo
c                              write(17,*) "merged test1",
c     >                             merged(test1,1),merged(test1,2),
c     >                             merged(test1,3),merged(test1,4),
c     >                             merged(test1,5),merged(test1,6),
c     >                             merged(test1,7),merged(test1,8)
			      if(test2.lt.count) then
				do k=1 , merged(count,1)+1
			          merged(test2,k)=merged(count,k)
				enddo
			      endif
  			      merged(count,1)= -1
			      count = count - 1
c                              write(17,*) "test2",
c     >                             merged(test1,1),merged(test1,2),
c     >                             merged(test1,3),merged(test1,4),
c     >                             merged(test1,5),merged(test1,6),
c     >                             merged(test1,7),merged(test1,8)
c                              write(17,*) "count",
c     >                             merged(count+1,1),merged(count+1,2),
c     >                             merged(count+1,3),merged(count+1,4),
c     >                             merged(count+1,5),merged(count+1,6),
c     >                             merged(count+1,7),merged(count+1,8)
			    endif
		          endif
                        endif
                     endif    ! dip cut
                  endif       ! center cut
                endif          ! in or out cut 
              endif             ! z cut 
           endif                ! good_dpt=.false.     

           j=j+1
           if(j.gt.ltot) good_dpt=.false.
        enddo                   ! j
      enddo
                     ! i  
c      write(17,*) "merging complete"
c      do j=1 , 1000
c         if(merged(j,1).gt.0) then
c             write(17,*) "merger vaules",
c     >       merged(j,1),merged(j,2),
c     >       merged(j,3),merged(j,4),
c     >       merged(j,5),merged(j,6),
c     >       merged(j,7),merged(j,8)
c          endif
c       enddo

       i=0
       do while(merged(i+1,1).gt.0)
          i=i+1
c          write(17,*) "merge",i
          num_hits=1
          do j=1, merged(i,1)
             do k=1, trk(merged(i,j+1)).nhit
               if(num_hits.lt.mxpnt) then
               if(trk(merged(i,j+1)).p(7).lt.0) then
                 hitpnt= trk(merged(i,j+1)).ipnt(k)
                 trk(track_h.nok+1).ipnt(num_hits)= hitpnt
               else
                 hitpnt= trk(merged(i,j+1)).ipnt(trk(merged(i,j+1)).nhit-k+1)
                 trk(track_h.nok+1).ipnt(num_hits) = hitpnt
               endif
               num_hits = num_hits + 1
c               write(17,*)"hit",hit(hitpnt).x,hit(hitpnt).y,hit(hitpnt).z,sqrt((hit(hitpnt).x)**2+(hit(hitpnt).y)**2),j
               endif
             enddo
          enddo
          trk(track_h.nok+1).nhit=num_hits-1

          dummy=0
          iret = tpt_fit_track(track_h.nok+1,trk,hit_h,hit,tpar_h,tpar,dummy,ipass)
c          write(17,*)trk(track_h.nok+1).flag,track_h.nok+1, trk(track_h.nok+1).nhit
          if(trk(track_h.nok+1).flag.gt.0)then
c             write(17,*) "we are here"
            track_h.nok=track_h.nok+1
            iret = tpt_load_track(track_h.nok,track_h,track,trk,hit_h,hit,tpar_h,tpar,ipass)
C           set flag to mark tracks coming from merging
            track(track_h.nok).flag=55
C           flag the contributing track as "admerged"
c           one more line
            do j=1, merged(i,1)
              trk(merged(i,j+1)).flag = -i
              track(merged(i,j+1)).flag = -i
            enddo
          endif
       enddo

       

 999  continue
      tpt_merge_segments = STAFCV_OK
      end







