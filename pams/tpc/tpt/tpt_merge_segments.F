* $Id: tpt_merge_segments.F,v 1.4 1998/04/09 23:11:36 sakrejda Exp $
* $Log: tpt_merge_segments.F,v $
* Revision 1.4  1998/04/09 23:11:36  sakrejda
* changes from Joachim Nystrand added
*
* Revision 1.3  1998/04/06 19:20:01  sakrejda
* quiks replaced by tls_Quick_Sort_r in merging routine
*
* Revision 1.2  1998/01/27 00:35:50  fisyak
* Split sources
*
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
#include "tpt_pointers.inc"
#include "tcl_tphit.inc"
#include "tpt_pars.inc"
#include "tpt_mxhit.inc"
C______________________________________________________________________
c
c Define local structure to sort radius
      STRUCTURE /track_merge/
                integer lpnt           ! location of a track with given radius
                real    rad            ! radius of a track
      END STRUCTURE

C______________________________________________________________________
      RECORD /table_head_st/    track_h
      RECORD /tpt_track_st/     track(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
      RECORD /track_pointers/   trk(*)
      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)

      RECORD /track_merge/      merge(mxtrack)
C_________________________________________________________________
      INTEGER ltot,tls_Quick_Sort_r,iret
      INTEGER i,j,k,l,lxj,lxi,next,c,m,n,ipass
      REAL id,radd,long,tracks,hits,dis,min(5),cuta,cutb
      INTEGER firsti,lasti,firstj,lastj
      INTEGER hipnt,lowpnt,isort,ipoint,itrack,ips,isect
      INTEGER rfifj,rfilj,rlifj,rlilj,rowdiff
      INTEGER rfi,rli,rfj,rlj,mpnt,ncount
      REAL xfi,yfi,zfi,xli,yli,zli
      REAL xfj,yfj,zfj,xlj,ylj,zlj
      REAL dfifj,dfilj,dlifj,dlilj,mindist,mindist2
      REAL radi,radj
      REAL varb,varc,meanrad,qij
C______________________________________________________________________

c Order the good tracks according to their helix radius.
c The array lpnt will point to each of these tracks in order.
      mpnt = mxpnt
      ltot = 0
      do i=1,track_h.nok
         if(trk(i).flag.ge.0) then
            ltot=ltot+1
            merge(ltot).rad=abs(trk(i).p(2))
            merge(ltot).lpnt=i
         endif
      enddo
      iret=tls_Quick_Sort_r(track_h.nok, merge(1).rad, merge(2).rad, merge)


c Minimum difference values for merging tracks 
c (min(1)=rr,min(2)=average of acent & bcent, min(3)=ltan, min(4)=phi0):
c Adjust these parameters to change the merging criteria.
      min(1)=0.1   ! radius ratio
      min(2)=0.1   ! center average
      min(3)=0.05  ! tanl
      min(4)=0.5   ! phi0 (deactivated)
      min(5)=10.0  ! Cut for avoiding merging parallel tracks 

c Loop over track radii in order. Check if radii match. Go on if yes.
c radd is the larger of the radii being compared.
      do i=1,ltot-1
         j=i+1
         lxi=merge(i).lpnt
         if(trk(lxi).flag.gt.0) then
         radi = merge(i).rad
         radj = merge(j).rad
         do while ((abs(radj-radi)/radi.lt.min(1)).and.(j.le.ltot)) 
          lxj=merge(j).lpnt
          if(trk(lxj).flag.gt.0) then
            radd=merge(i).rad
c           Now compare the average of the differences of the 
c           centers of the helices
            cuta = abs(trk(lxi).p(3)-trk(lxj).p(3))
            cutb = abs(trk(lxi).p(4)-trk(lxj).p(4))
C           >> USE Eq. 2.1.29 of Tracking Memo. Do not try to merge tracks
C           >> with different curvature (charge). (JN 980406)
            qij=trk(lxi).p(2)*trk(lxj).p(2)
            meanrad=0.5*(radi+radj)
            varb=0.5*(cuta/meanrad + cutb/meanrad)
C            if (cuta.lt.6.0.and.cutb.lt.6.0) then
c           come back to it! sometimes merging tracks with different signs
c           makes some sence
c            if ( (varb.lt.min(2)) .and. (qij.gt.0.) ) then
            if ( (varb.lt.min(2))) then
c               Now compare their dip angles.
                varc=abs(abs(trk(lxi).p(7))-abs(trk(lxj).p(7)))
                if (abs(abs(trk(lxi).p(7))-abs(trk(lxj).p(7))).lt.
     +          min(3)) then
c                 For the tracks that survive, these two cuts, find 
c                 their endpoints.
c                 This is to make sure that two parallel tracks 
c                 are not merged.
                  firsti=0
                  lasti=0
                  firstj=0
                  lastj=0
                  hipnt=0
                  lowpnt=999
                  do isort=1,trk(lxi).nhit
                     ipoint = trk(lxi).ipnt(isort)
                     if (ipoint.ne.0) then
                           itrack = int(hit(ipoint).track/1000)
                           ips = hit(ipoint).track-itrack*1000
                           if (ips.lt.lowpnt) then
                              lowpnt = ips
                              firsti = ipoint
                           endif
                           if (ips.gt.hipnt) then
                              hipnt = ips
                              lasti = ipoint
                           endif
                     else
                           write(6,*) 'This is a bug.'
                     endif
                  enddo
                  lowpnt = 999
                  hipnt = 0
                  do isort=1,trk(lxj).nhit
                     ipoint = trk(lxj).ipnt(isort)
                     if (ipoint.ne.0) then
                           itrack = int(hit(ipoint).track/1000)
                           ips = hit(ipoint).track-itrack*1000
                           if (ips.lt.lowpnt) then
                              lowpnt = ips
                              firstj = ipoint
                           endif
                           if (ips.gt.hipnt) then
                              hipnt = ips
                              lastj = ipoint
                           endif
                     else
                           write(6,*) 'This is a bug.'
                     endif
                  enddo
c                 Now that the endpoint hits have been identified, 
c                 assign rows and coords.
                  xfi = hit(firsti).x
                  yfi = hit(firsti).y
                  zfi = hit(firsti).z
                  isect = hit(firsti).row/100
                  rfi = hit(firsti).row-isect*100
                  xli = hit(lasti).x
                  yli = hit(lasti).y
                  zli = hit(lasti).z
                  isect = hit(lasti).row/100
                  rli = hit(lasti).row-isect*100
                  xfj = hit(firstj).x
                  yfj = hit(firstj).y
                  zfj = hit(firstj).z
                  isect = hit(firstj).row/100
                  rfj = hit(firstj).row-isect*100
                  xlj = hit(lastj).x
                  ylj = hit(lastj).y
                  zlj = hit(lastj).z
                  isect = hit(lastj).row/100
                  rlj = hit(lastj).row-isect*100

c                 Compute the distances and row differences
c                 between endpoints.
C                 Only consider firsti-firstj, firsti-lastj, 
c                 lasti-firstj, lasti-lastj
                  dfifj = sqrt((xfi-xfj)**2+(yfi-yfj)**2+
     +                    (zfi-zfj)**2)
                  dfilj = sqrt((xfi-xlj)**2+(yfi-ylj)**2+
     +                    (zfi-zlj)**2)
                  dlifj = sqrt((xli-xfj)**2+(yli-yfj)**2+
     +                    (zli-zfj)**2)
                  dlilj = sqrt((xli-xlj)**2+(yli-ylj)**2+
     +                    (zli-zlj)**2)
                  rfifj = abs(rfi-rfj)
                  rfilj = abs(rfi-rlj)
                  rlifj = abs(rli-rfj)
                  rlilj = abs(rli-rlj)

c                 Need to find the closest endpoints and 
c                 corresponding row difference.
                  mindist=99999.99
                  mindist2=99999.99
                  if (dfifj.ne.0.0.and.dfifj.lt.mindist) then
                        mindist=dfifj
                        rowdiff=rfifj
                  endif
                  if (dfilj.ne.0.0.and.dfilj.lt.mindist) then
                        mindist=dfilj
                        rowdiff=rfilj
                  endif
                  if (dlifj.ne.0.0.and.dlifj.lt.mindist) then
                        mindist=dlifj
                        rowdiff=rlifj
                  endif
                  if (dlilj.ne.0.0.and.dlilj.lt.mindist) then
                        mindist=dlilj
                        rowdiff=rlilj
                  endif
                  if (dfifj.ne.0.0.and.dfifj.ne.mindist) 
     +                    mindist2=dfifj
                  if (dfilj.ne.0.0.and.dfilj.ne.mindist.and.
     +                    dfilj.lt.mindist2) mindist2=dfilj
                  if (dlifj.ne.0.0.and.dlifj.ne.mindist.and.
     +                    dlifj.lt.mindist2) mindist2=dlifj
                  if (dlilj.ne.0.0.and.dlilj.ne.mindist.and.
     +                    dlilj.lt.mindist2) mindist2=dlilj

c                 Finally got it, now make sure the tracks are not parallel
c                 >> USE PARAMETER min(5) (JN 980406)
                  if (mindist.lt.min(5).and.mindist2.lt.min(5).and.
     +                    tpar(ipass).debug(8).eq.1)
     +                    write(6,*) '*****PARALLEL******'
                  if (mindist.gt.min(5).or.mindist2.gt.min(5)) then 
                        if (tpar(ipass).debug(8).eq.1) then
                           write(6,*) '**Merged ','dist= ',mindist,
     +                          'drow= ',rowdiff
                           write(6,111) i,lxi,trk(lxi).nhit,merge(i).rad,
     +                          trk(lxi).p(3),trk(lxi).p(4),trk(lxi).
     +                          p(7),trk(lxi).p(5)
                           write(6,111) j,lxj,trk(lxj).nhit,merge(j).rad,
     +                          trk(lxj).p(3),trk(lxj).p(4),trk(lxj).
     +                          p(7),trk(lxj).p(5)
 111                       format(3(1x,i5),5(1x,f10.5))
                        endif

*                       Merge the two tracks:
                        m=trk(lxi).nhit+trk(lxj).nhit
                        c=1  
                        n=1
                        if (m.gt.mxpnt) m=mxpnt
                        do l=1,m
                           long=-1.0
*                          Find hit with the next largest radius:
                           do n=1,trk(lxj).nhit
                              id=trk(lxj).ipnt(n)
                              dis=hit(trk(lxj).ipnt(n)).x**2+
     +                             hit(trk(lxj).ipnt(n)).y**2+
     +                             hit(trk(lxj).ipnt(n)).z**2
                              if (id.gt.-1.and.(dis.ge.long)) then
                                 long=dis
                                 tracks=lxj
                                 hits=n
                                 next=id
                              endif
                           enddo 
                  
                           do n=1,trk(lxi).nhit
                              id=trk(lxi).ipnt(n)
                              dis=hit(trk(lxi).ipnt(n)).x**2+
     +                             hit(trk(lxi).ipnt(n)).y**2+
     +                             hit(trk(lxi).ipnt(n)).z**2
                              if (id.gt.-1.and.(dis.ge.long)) then
                                 long=dis
                                 tracks=lxi
                                 hits=n
                                 next=id
                              endif
                           enddo                   
*                          Write the hit to its new track:
                           if (long.gt.-1.0) then
                              hit(next).track=lxi*1000+c
                              c=c+1
                           endif
*                          Mark hit as used:
                           trk(tracks).ipnt(hits)=-abs(trk(tracks).
     +                          ipnt(hits))
                        enddo
*                       Update and fix the trk array & update tptrack table:
                        ncount = 0
                        do n=1,trk(lxi).nhit
                           if(trk(lxi).ipnt(n).gt.0) then
                               next=trk(lxi).ipnt(n)
                               hit(next).track=0
                           else 
                               ncount=ncount+1
                               trk(lxi).ipnt(ncount)=
     >                         abs(trk(lxi).ipnt(n))
                           endif
                        enddo
                        do k=1,trk(lxj).nhit
                           if(trk(lxj).ipnt(k).gt.0) then
                               next=trk(lxj).ipnt(k)
                               hit(next).track=0
                           else
                               ncount = ncount+1
                               trk(lxj).ipnt(k)=abs(trk(lxj).ipnt(k))
                               trk(lxi).ipnt(ncount)=
     >                         abs(trk(lxj).ipnt(k))
                           endif
                        enddo
                        trk(lxi).nhit = ncount
                        if (tpar(ipass).debug(8).eq.1) then
                           write(6,*) 'New track:'
                           write(6,*) 'trk(lxi).nhit= ',trk(lxi).nhit
                        endif
                        trk(lxj).p(1)=-100
                        trk(lxj).flag=-100
                        track(lxj).flag=-100 
                        trk(lxi).p(1) = 100
                        trk(lxi).flag = 100
                        track(lxi).flag=100
                        track(lxi).nrec=ncount
                  endif   ! mindist
                endif   ! p(7) or tanl
            endif   ! cuta,b
          endif! track that passes the radius criterion already merged 
          j=j+1
          if(j.le.ltot) radj = merge(j).rad
         enddo   ! rad
      endif ! .flag<0 (track already merged)
      enddo   ! i  
c
      tpt_merge_segments = STAFCV_OK
      end
