      function g2t_get_hits (
     >                          p_vert_h,             p_vert ,
     >                          p_track_h,            p_track,
     >                          p_hits_h,             p_hits)
C :>--------------------------------------------------------------------
C : FILE:        g2t_get_hits.g
C : ROUTINE:     G2T_GET_HITS
C : DESCRIPTION: general hit table filling
C : AUTHOR:      narod
C : ARGUMENTS:
C :       INOUT:
C :           p_vert     - description here
C :         p_vert_h     - Header Structure for p_vert
C :          p_track     - description here
C :        p_track_h     - Header Structure for p_track
C :           p_hits     - description here
C :         p_hits_h     - Header Structure for p_hits
C : RETURNS:    STAF Condition Value
C : HISTORY:
C :             31dec97-v000a-PLN
C :>--------------------------------------------------------------------
      IMPLICIT   NONE

      INTEGER*4  G2T_GET_HITS
      Integer    AgFHIT0, AgFHIT1, IH, ITRA, JTRA, NUMBV(15)
      integer    tpgv,tpss,tpad,sector,isdet/1/,nh,i,k, LenH
      Real       Hits(15),fhmin(15),fhmax(15),fhbin(15),x(3)
      Character*4  chit(15)

#include       "g2t_get_hits.inc"
#include       "geant321/gcnum.inc"

      G2T_GET_HITS = STAFCV_BAD
      nh = 0
      if (agfhit0('TPCE','TPAD').ne.0) Return
      Call AgFDPAR (hits,Chit,FHmin,FHmax,FHbin)
      print *,' chit=',chit
      LenH          = p_hits_h.rbytes/4

      DO WHILE (agfhit1(ih,jtra,numbv,hits).eq.0)
*         print *,' ih,jtra,nvl = ',ih,itra,(numbv(i),i=1,3)
         nh   = nh + 1
         Call VZERO (p_hits(nh),LenH)
         call vzero (x,3)
         itra = abs(jtra)
         p_hits(nh).track_p = itra

         if (abs(ih).eq.1) call Agfpath(numbv)
         do k=1,15
            if (chit(k).eq.' ')     go to 101
            if (chit(k).eq.'X')     x(1)=hits(k)
            if (chit(k).eq.'Y')     x(2)=hits(k)
            if (chit(k).eq.'Z')     x(3)=hits(k)
            if (chit(k).eq.'XX')    p_hits(nh).x(1)     = hits(k)
            if (chit(k).eq.'YY')    p_hits(nh).x(2)     = hits(k)
            if (chit(k).eq.'ZZ')    p_hits(nh).x(3)     = hits(k)
            if (chit(k).eq.'PX')    p_hits(nh).p(1)     = hits(k)
            if (chit(k).eq.'PY')    p_hits(nh).p(2)     = hits(k)
            if (chit(k).eq.'PZ')    p_hits(nh).p(3)     = hits(k)
            if (chit(k).eq.'TOF ')  p_hits(nh).tof      = hits(k)
            if (chit(k).eq.'SLEN')  p_hits(nh).s_track  = hits(k)
            if (chit(k).eq.'USER')  p_hits(nh).de       = hits(k)
            if (chit(k).eq.'ELOS')  p_hits(nh).de       = hits(k)
            if (chit(k).eq.'BIRK')  p_hits(nh).de       = hits(k)
            if (chit(k).eq.'STEP')  p_hits(nh).ds       = hits(k)
         enddo
 101     continue 
         if (x(1)+x(2)+x(3).ne.0) call GDTOM(x,p_hits(nh).x,1)
         p_hits(nh).id   = nh

            
*                  calculate and encode the sector and padrow
*                  -- arithmetics thanks to Peter M. Jacobs. 
                tpgv = numbv(1)
                tpss = numbv(2)
                tpad = numbv(3)
		sector = tpss + 12*(tpgv-1)  ! sector number 1 - 24
                p_hits(nh).volume_id = 100000*isdet+100*sector+tpad        


*                  add to track linked list 
*                p_hits(nh).next_tr_hit_p = Track(p_hits.track_p).hit_tpc_p
*                Track(p_hits.track_p).hit_tpc_p = p_hits(nh).id
*                Track(p_hits.track_p).n_tpc_hit += 1

      p_hits_h.nok = nh
      enddo
      print *,' something done in g2t_get_hits '
      G2T_GET_HITS = STAFCV_OK

      RETURN
      end


