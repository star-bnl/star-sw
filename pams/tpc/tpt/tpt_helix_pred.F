* $Id: tpt_helix_pred.F,v 1.7 2000/02/23 21:05:52 hardtke Exp $
* $Log: tpt_helix_pred.F,v $
* Revision 1.7  2000/02/23 21:05:52  hardtke
* replace tpg functions with tpc functions
*
* Revision 1.6  1999/09/14 00:55:48  liq
* use tpt_track_pointers.inc instead of tpt_pointers.inc in order to used by C++ codes
*
* Revision 1.5  1999/07/19 19:15:55  genevb
* Use StMessMgr
*
* Revision 1.4  1998/12/11 01:54:28  sakrejda
* a call to tgc_global_to_local replaced by tgc_global_to_sector - only the sector information was used
*
* Revision 1.3  1998/03/23 18:25:59  sakrejda
* tpg_pad-plane removed from the tpt module, it was not used there anyway
*
* Revision 1.2  1998/01/27 00:35:48  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_HELIX_PRED(itrk,trk,isect,
     >                                irow,tpar_h,tpar,np,xp,yp,ipass)
C>--------------------------------------------------------------------
C
C TPT_HELIX_PRED - calculates prediction for track crossing a row
C 
C DESCRIPTION:
C A prediction for a point where a helix crosses center of a padrow
C is calculated. First common points of a circle and a line are 
C calculated and then a check is prformed for the crossing points being
C within the sector bounds. Then z coordinates for points that pass 
C this check are calculated.
C 
C INPUT ARGUMENTS:
C  itrk     - track number for which the crossing is evaluated
C  isect    - sector number where the crossings are supposed to be
C  irow     - row number for the crossings
C  trk      - structure of reconstructed segments
C  tpar_h    - header for the tptpar (tracking parameters) table
C  tpar     - tracking parameters table
C
C OUTPUT ARGUMENTS:
C  np        - number of crossings within sector
C  xp        - vector of x coordinates for the crossings
C  yp        - vector of y coordinates for the crossings
C
C AUTHOR:
C Iwona sakrejda
C
C<--------------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variable.
C
      IMPLICIT NONE
#include "PAM.inc"

#include "tpt_track_pointers.inc"
#include "tpt_pars.inc"

      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)
      RECORD /tpt_track_pointers_st/   trk(*)
C______________________________________________________________________

      INTEGER tpc_row_par,tpt_circle_pred
      INTEGER isect,irow,np,iret,itrk,isec1,ipass
      integer tpc_global_to_sector
      REAL    xp(2),yp(2),as,bs,xav,yav,dr,xglobal(3)
      REAL    dummy
      CHARACTER*132 m132

      np=0
      iret = tpc_row_par(isect,real(irow),as,bs)
      if(tpar(ipass).debug(2).eq.1) then
       write(m132,*) 'TPT_HELIX_PRED - sector',isect,'row',irow,'a',as,'b',bs
       call StInfo(m132)
       write(m132,*) trk(itrk).p(3), trk(itrk).p(4), trk(itrk).p(2)
       call StInfo(m132)
      endif
      iret = tpt_circle_pred(as,bs,trk(itrk).p(3),trk(itrk).p(4),
     >                       abs(trk(itrk).p(2)),np,xp,yp)
      if(tpar(ipass).debug(2).eq.1) then
       write(m132,*) np,xp(1),yp(1),xp(2),yp(2)
       call StInfo(m132)
      endif
      if(iret.ne.0) then
           tpt_helix_pred = 1
      else
           np = 0
           xglobal(1)=xp(1)
           xglobal(2)=yp(1)
           xglobal(3)=10.0
           iret =  tpc_global_to_sector(isec1,xglobal)
           if(isec1.gt.12) isec1=24-isec1
           if(isec1.eq.0) isec1=12
           if(isec1.eq.isect) then
              np = np+1
           endif
           xglobal(1)=xp(2)
           xglobal(2)=yp(2)
           xglobal(3)=10.0
           iret =  tpc_global_to_sector(isec1,xglobal)
           if(isec1.gt.12) isec1=24-isec1
           if(isec1.eq.0) isec1=12
           if(isec1.eq.isect) then
              np = np+1
              xp(np) = xp(2)
              yp(np) = yp(2)
           endif
           if(np.ne.0) then
             if(np.eq.2) then
                   dr=sqrt((xp(1)-xp(2))**2+(yp(1)-yp(2))**2)
                   if(dr.lt.2.0) then
                         xav = 0.5*(xp(1)+xp(2))
                         yav = 0.5*(yp(1)+yp(2))
                         xp(1) = xav
                         yp(1) = yav
                         np = 1
                   endif
             endif
                  tpt_helix_pred = 0
           else
                  tpt_helix_pred = 1
           endif
      endif
      end
