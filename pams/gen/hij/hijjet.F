* $Id: hijjet.F,v 1.6 2000/01/22 17:37:41 nevski Exp $
* $Log: hijjet.F,v $
* Revision 1.6  2000/01/22 17:37:41  nevski
* clean up unused variables
*
* Revision 1.5  1999/08/14 21:49:00  fisyak
* eliminate include and source level
*
* Revision 1.4  1999/08/13 23:55:08  fisyak
* remove hepevt
*
* Revision 1.3  1999/01/16 20:53:22  didenko
* add some usefull histogram for jets
*
* Revision 1.2  1998/05/13 18:55:50  didenko
* modified version with full particles history
*
* Revision 1.1  1998/04/20 18:19:54  didenko
* version hijing1.34 released
*
*
      INTEGER*4 FUNCTION HIJJET(par_h, par) 
      IMPLICIT NONE
*:>--------------------------------------------------------------------
*: ROUTINE:    HIJING
*: generate one hijing event and put into table
*:>--------------------------------------------------------------------
      EXTERNAL  HIJEV
#include "hijjet.inc"
#include "headpss.inc"
#include "himevt2.inc"
*--
      INTEGER NATT, JATT, NT, NP, N0, N01, N10, N11
      REAL  EATT
      COMMON/HIMAIN1/NATT,EATT,JATT,NT,NP,N0,N01,N10,N11
      SAVE  /HIMAIN1/
*
      COMMON/WEVENT/IEVEF
      INTEGER IEVEF
      SAVE /WEVENT/
*
      DIMENSION pt(40000), eta(40000), idst(40000,2), ptjet(50)
      DIMENSION NUMJET(50)
      REAL PT, ETA, ptjet, px1, py1
      REAL px, py, pz, theta, HMEMOR 
      INTEGER I, J, IK, IP, idst, numjet
*
      INTEGER ii, nd1, nd2, kk, mm, ll, km
      INTEGER nd3, nd4, nd5, nd6, nd7, nd8
      INTEGER nd9, nd10, nd11, nd12, njets, njetp
      
      COMMON/PAWC/HMEMOR(20000)
      LOGICAL FIRST/.TRUE./
**
      IF (FIRST) THEN
       FIRST=.FALSE.
       CALL HBOOK1(100, 'NUMBER of JETS', 10, 0., 10., 0.)
       CALL HBOOK1(101, 'PT of JETS PT > 5 ', 50, 0., 50., 0.)
       CALL HBOOK1(102, 'ETHA of JETS', 50, -5., 5., 0.)
       CALL HBOOK1(103, 'PT of PARTICLES', 40, 0., 20., 0.)
       CALL HBOOK1(104, 'ETHA of PARTICLES', 50, -5., 5., 0.)
       CALL HBOOK1(106, 'NUMBER of PART in JET', 40, 0., 80., 0.)
       CALL HBOOK1(107, 'PT of  JETS', 50, 0., 50., 0.)
      ENDIF
C
       IEVEF = 0
       CALL HIJEV
       IF(IEVEF .eq. 0)then     
*
*-- Sanity check of header for table "par"...
*
      par_h.nok = 0
      IF(NATT .GT. par_h.maxlen) THEN
         HIJJET = STAFCV_BAD
         GO TO 999
      ENDIF
*
*-- Start event loop
*
*
        CALL DATIME(I,J)
        VSSHEP(2) = VSSHEP(2) + 1
        VSSHEP(3) = FLOAT(I)
        VSSHEP(4) = FLOAT(J)
C--
        do ii = 1, natt
          do ik = 1,2
         idst(ii,ik) = 0
        enddo
        enddo
C-- 
        do ii = 1, natt
             px = patt(ii,1)
             py = patt(ii,2)
             pz = patt(ii,3)
C
             pt(ii) = sqrt(px*px+py*py)
             theta = atan2 ( pt(ii), pz )
             eta(ii)   = -log(tan(theta/2.))
 
       if(abs(katt(ii,1)).ge.91.and.abs(katt(ii,1)).le.93) then
           if(pt(ii).ge. 5.) then
          call hfill(101, pt(ii), 0., 1.)
          call hfill(102, eta(ii), 0., 1.)
        nd1 = katt(ii,5)
        nd2 = katt(ii,6)
         do ip = nd1, nd2
            if(katt(ip,4) .eq. 1) then
              idst(ip,1) = katt(ii,1)
              idst(ip,2) = ii 
            else            
             nd3 = katt(ip,5)
             nd4 = katt(ip,6)
              do ik = nd3, nd4
                 if(katt(ik,4) .eq. 1) then
                   idst(ik,1) = katt(ii,1) 
                   idst(ik,2) = ii
                  else 
                  nd5 = katt(ik,5)
                  nd6 = katt(ik,6)
                   do kk = nd5, nd6
                      if(katt(kk,4) .eq. 1) then
                        idst(kk,1) = katt(ii,1)
                        idst(kk,2) = ii
                     else
                     nd7 = katt(kk,5)
                     nd8 = katt(kk,6)
                      do km = nd7, nd8
                        if(katt(km,4) .eq. 1) then
                         idst(km,1) = katt(ii,1)
                         idst(km,2) = ii
                       else 
                        nd9 = katt(km,5)
                        nd10 = katt(km,6)
                      do mm = nd9, nd10
                       if(katt(mm,4) .eq. 1) then
                         idst(mm,1) = katt(ii,1) 
                         idst(mm,2) = ii
                       else 
                        nd11 = katt(mm,5)
                        nd12 = katt(mm,6)
                      do ll = nd11, nd12
                        idst(ll,1) = katt(ii,1)
                        idst(ll,2) = ii
                       enddo
                      endif 
                     enddo
                    endif 
                    enddo
                   endif
                 enddo
               endif
              enddo
             endif
            enddo
            endif
           endif
          enddo 
C
          njets = 0
C 
          do ii = 1, natt
        if(abs(katt(ii,1)).ge.91.and.abs(katt(ii,1)).le.93) then
          if(pt(ii) .ge. 5.) then
           njetp = 0
           px1 = 0.
           py1 = 0. 
          do ik = 1,natt
           if(idst(ik,2) .eq. ii ) then
              njetp = njetp + 1
              px1 = px1 + patt(ik,1)
              py1 = py1 + patt(ik,2)
              call hfill(103, pt(ik), 0., 1.)
              call hfill(104, eta(ik), 0., 1.)
             endif
            enddo 
             if(njetp.ne.0) then
              njets = njets + 1 
             ptjet(njets) = sqrt(px1**2 + py1**2)
              numjet(njets) = njetp
             endif 
           endif
          endif
         enddo
C
          if(njets .ne. 0) then
            call hfill(100, float(njets), 0., 1.)
           do kk = 1, njets
            call hfill(106, float(numjet(kk)), 0., 1.)
            call hfill(107, ptjet(kk), 0., 1.)
           enddo
          endif 
C--
        par(1).isthep = 10
        par(1).idhep = 9999999
        par(1).jmohep(1) = njets
        par(1).jmohep(2) = njetp
        par(1).jdahep(1) = 0
        par(1).jdahep(2) = 0
        par(1).phep(1) = psshep(1) 
        par(1).phep(2) = psshep(2)
        par(1).phep(3) = psshep(3)
        par(1).phep(4) = psshep(4)
        par(1).phep(5) = psshep(5)
        par(1).vhep(1) = vsshep(1)
        par(1).vhep(2) = vsshep(2)
        par(1).vhep(3) = vsshep(3)
        par(1).vhep(4) = vsshep(4)
C
        ik = 1
        DO IP = 1, NATT
C        if(katt(ip,4) .eq. 1) then
        ik = ik + 1
        par(ik).isthep     = katt(ip,4)    ! status code
        par(ik).idhep      = katt(ip,1)    ! particle identity
        par(ik).jmohep(1)  = katt(ip,3)    ! pointer(s) to mother position
        par(ik).jmohep(2)  = 0             !                         
        par(ik).jdahep(1)  = katt(ip,5)    !  pointers to position of 
        par(ik).jdahep(2)  = katt(ip,6)    !  the first/last daughter  
        DO J = 1, 4
          par(ik).phep(j)  = patt(ip,j)    !  p4 
          par(ik).vhep(j)  = 0.            !  decay vertex (mm) and time(mm/c) 
        END DO
          par(ik).phep(5) = patt(ip,5)
C      endif
C  
      END DO
      par_h.nok = ik
*
      HIJJET=STAFCV_OK
C      endif
      ENDIF
C
  999 END


