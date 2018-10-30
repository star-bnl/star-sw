* $Id: hijjet.F,v 1.2 2009/08/19 18:55:20 jeromel Exp $
* $Log: hijjet.F,v $
* Revision 1.2  2009/08/19 18:55:20  jeromel
* Continuation FORtran character on column 7 (?? sigh!)
*
* Revision 1.1  2008/08/18 18:24:25  perev
* Rename .f to .F
*
* Revision 1.7  2003/05/06 15:30:34  longacre
* Pavels improved HEPTUP
*
* Revision 1.6  2003/05/05 20:25:39  longacre
* put bin. coll wounded proj wounded targ in ipdg=999996
*
* Revision 1.5  2003/05/05 16:20:13  longacre
* resonance istat code were 11 now are 2
*
* Revision 1.4  2003/05/02 18:34:50  longacre
*  added bin coll and wounded proj and targ
*
* Revision 1.3  2003/04/21 18:08:57  longacre
* take out debug print
*
* Revision 1.2  2003/04/21 16:55:47  longacre
* corrected random number seed input
*
* Revision 1.1  2003/01/15 18:00:42  longacre
* tagging 382 as 382
*
* Revision 1.1  2003/01/14 21:21:03  longacre
* forgot hijjet.f
*
* Revision 1.4  2000/06/23 18:39:51  longacre
*  remove more printout
*
* Revision 1.3  2000/06/22 22:53:54  longacre
*  comment out debug code
*
* Revision 1.2  2000/06/22 22:49:36  longacre
*  new hijing better parton code
*
* Revision 1.1  2000/06/16 14:58:04  longacre
*  hijing ntuple maker
*
*
      PROGRAM HIJJET
      IMPLICIT NONE
*:>--------------------------------------------------------------------
*: ROUTINE:    HIJING
*: generate one hijing event and put into table
*:>--------------------------------------------------------------------
      EXTERNAL  HIJEV
      EXTERNAL  MAKHVY
      COMMON/HVYIN/NHVY,KHVY(200,6),PHVY(200,5)
      REAL PHVY
      INTEGER NHVY, KHVY
      SAVE /HVYIN/
      COMMON/HVYJT1/NHVJ1,KHVJ1(100,6),PHVJ1(100,5)
      REAL PHVJ1
      INTEGER NHVJ1, KHVJ1
      SAVE /HVYJT1/
      COMMON/HVYJT2/NHVJ2,KHVJ2(100,6),PHVJ2(100,5)
      REAL PHVJ2
      INTEGER NHVJ2, KHVJ2
      SAVE /HVYJT2/
CCC#include "headpss.inc"
      COMMON/HEADPSS/PSSHEP(5),VSSHEP(4),IFIRST,IRUN
      REAL PSSHEP,VSSHEP
      INTEGER IFIRST, IRUN
      SAVE /HEADPSS/
CCCC#include "himevt2.inc"
      INTEGER    KATT
      REAL  PATT, VATT
      COMMON/HIMAIN2/KATT(130000,6),PATT(130000,5)
      SAVE  /HIMAIN2/
*--
      INTEGER NATT, JATT, NT, NP, N0, N01, N10, N11, jend, joff
      REAL  EATT
      COMMON/HIMAIN1/NATT,EATT,JATT,NT,NP,N0,N01,N10,N11
      SAVE  /HIMAIN1/
*
      COMMON/WEVENT/EVEF
      REAL EVEF
      SAVE /WEVENT/
*
      REAL PT, ETA, ptjet, px1, py1, P(3), V(3), E, AMASS, X4
      REAL px, py, pz, theta, HMEMOR, PP(4) 
      INTEGER I, J, IK, IP, id, imo(2), idau(2), matt,ksptt
*
      INTEGER ii, nd1, nd2, mm, ll, km, ISTAT, jj
      INTEGER nd3, nd4, nd5, nd6, nd7, nd8, idtst
      INTEGER nd9, nd10, nd11, nd12, njets, njetp
C
       EVEF = 0.
       X4=0.0
       CALL HIJEV
 999   EVEF=EVEF+1.
        
       IF(EVEF .GT. VSSHEP(4)) then     
         CALL HEPEnd(' ')
         STOP
       ENDIF
       IF(EVEF.GT.1) CALL HIJEV
*
*
*
*-- Start event loop
*
*
        njets = 0
        if(IFIRST.GT.1) then
        ik = 0
        DO IP = 1, NATT
        ik = ik + 1
        ISTAT=katt(ip,4)
        imo(1)=katt(ip,3)
        imo(2)=0
        idau(1)=katt(ip,5)
        idau(2)=katt(ip,6)
        IF(ik.eq.NATT)  ik = -1
        DO J = 1, 3
          P(j)  = patt(ip,j)    
          V(j)  = 0.        
        END DO
          E     = patt(ip,4)    
          AMASS = patt(ip,5)
CCCCC        WRITE(63,6116)ik,ISTAT,katt(ip,1),imo,idau
CCCCC     1,P,E,AMASS
      END DO
        DO ip = 1, NATT
        if(katt(ip,1).gt.90.and.katt(ip,1).lt.94) then 
        pt=sqrt(patt(ip,1)**2+patt(ip,2)**2)
        if(pt.gt.5.0.or.iabs(katt(katt(ip,3),1)).eq.5) then
        njets = njets +1
        if(iabs(katt(katt(ip,3),1)).eq.5) then
        njets = njets +1
        katt(ip-1,1)=920000-katt(katt(ip,3),1)
        katt(ip-1,3)=katt(ip,3)
        katt(ip-1,4)=katt(ip,4)
        NHVY=katt(ip,6)-katt(ip,5)+1
        IF(NHVY.GT.1) THEN
CCCCC        WRITE(6,433) ip, katt(ip,5), katt(ip,6), NHVY
 433    FORMAT(' ip, katt(ip,5), katt(ip,6), NHVY ',4I10)
        jend=nhvy-1
        joff=katt(ip,5)
        do j=0,jend
          khvy(j+1,1)=katt(joff+j,1) 
          khvy(j+1,2)=katt(joff+j,2) 
          khvy(j+1,3)=katt(joff+j,3) 
          khvy(j+1,4)=katt(joff+j,4) 
          khvy(j+1,5)=katt(joff+j,5) 
          khvy(j+1,6)=katt(joff+j,6) 
          phvy(j+1,1)=patt(joff+j,1) 
          phvy(j+1,2)=patt(joff+j,2) 
          phvy(j+1,3)=patt(joff+j,3) 
          phvy(j+1,4)=patt(joff+j,4) 
          phvy(j+1,5)=patt(joff+j,5) 
         enddo
         call makhvy
CCCCC        WRITE(6,434) NHVY, NHVJ1, NHVJ2
 434    FORMAT(' NHVY, NHVJ1, NHVJ2 ',3I10)
        idtst=iabs(KHVJ2(1,1))
        IF(idtst.gt.499.and.idtst.lt.599.or.
     1  idtst.gt.4999.and.idtst.lt.5999) THEN
         katt(ip-1,5)=katt(ip,5)
         katt(ip-1,6)=katt(ip,5)+NHVJ1-1
         katt(ip,5)=katt(ip,5)+NHVJ1
        jend=nhvj1-1
        joff=katt(ip-1,5)  
        patt(ip-1,1)=0.0
        patt(ip-1,2)=0.0
        patt(ip-1,3)=0.0
        do j=0,jend
          katt(joff+j,1)=KHVJ1(j+1,1)
          katt(joff+j,2)=KHVJ1(j+1,2)
          katt(joff+j,3)=KHVJ1(j+1,3)
          katt(joff+j,4)=KHVJ1(j+1,4)
          katt(joff+j,5)=KHVJ1(j+1,5)
          katt(joff+j,6)=KHVJ1(j+1,6)
          patt(joff+j,1)=PHVJ1(j+1,1)
          patt(joff+j,2)=PHVJ1(j+1,2)
          patt(joff+j,3)=PHVJ1(j+1,3)
          patt(joff+j,4)=PHVJ1(j+1,4)
          patt(joff+j,5)=PHVJ1(j+1,5)
          patt(ip-1,1)=patt(ip-1,1)+PHVJ1(j+1,1)
          patt(ip-1,2)=patt(ip-1,2)+PHVJ1(j+1,2)
          patt(ip-1,3)=patt(ip-1,3)+PHVJ1(j+1,3)
         enddo
        jend=nhvj2-1
        joff=katt(ip,5)  
        patt(ip,1)=0.0
        patt(ip,2)=0.0
        patt(ip,3)=0.0
        do j=0,jend  
          katt(joff+j,1)=KHVJ2(j+1,1)
          katt(joff+j,2)=KHVJ2(j+1,2)
          katt(joff+j,3)=KHVJ2(j+1,3)
          katt(joff+j,4)=KHVJ2(j+1,4)
          katt(joff+j,5)=KHVJ2(j+1,5)
          katt(joff+j,6)=KHVJ2(j+1,6)
          patt(joff+j,1)=PHVJ2(j+1,1)
          patt(joff+j,2)=PHVJ2(j+1,2)
          patt(joff+j,3)=PHVJ2(j+1,3)
          patt(joff+j,4)=PHVJ2(j+1,4)
          patt(joff+j,5)=PHVJ2(j+1,5)
          patt(ip,1)=patt(ip,1)+PHVJ2(j+1,1)
          patt(ip,2)=patt(ip,2)+PHVJ2(j+1,2)
          patt(ip,3)=patt(ip,3)+PHVJ2(j+1,3)
         enddo
        ELSE
         katt(ip-1,6)=katt(ip,6)
         katt(ip,6)=katt(ip,5)+NHVJ1-1
         katt(ip-1,5)=katt(ip,5)+NHVJ1
        jend=nhvj2-1
        joff=katt(ip-1,5)  
        patt(ip-1,1)=0.0
        patt(ip-1,2)=0.0
        patt(ip-1,3)=0.0
        do j=0,jend
          katt(joff+j,1)=KHVJ2(j+1,1)
          katt(joff+j,2)=KHVJ2(j+1,2)
          katt(joff+j,3)=KHVJ2(j+1,3)
          katt(joff+j,4)=KHVJ2(j+1,4)
          katt(joff+j,5)=KHVJ2(j+1,5)
          katt(joff+j,6)=KHVJ2(j+1,6)
          patt(joff+j,1)=PHVJ2(j+1,1)
          patt(joff+j,2)=PHVJ2(j+1,2)
          patt(joff+j,3)=PHVJ2(j+1,3)
          patt(joff+j,4)=PHVJ2(j+1,4)
          patt(joff+j,5)=PHVJ2(j+1,5)
          patt(ip-1,1)=patt(ip-1,1)+PHVJ2(j+1,1)
          patt(ip-1,2)=patt(ip-1,2)+PHVJ2(j+1,2)
          patt(ip-1,3)=patt(ip-1,3)+PHVJ2(j+1,3)
         enddo
        jend=nhvj1-1
        joff=katt(ip,5)  
        patt(ip,1)=0.0
        patt(ip,2)=0.0
        patt(ip,3)=0.0
        do j=0,jend  
          katt(joff+j,1)=KHVJ1(j+1,1)
          katt(joff+j,2)=KHVJ1(j+1,2)
          katt(joff+j,3)=KHVJ1(j+1,3)
          katt(joff+j,4)=KHVJ1(j+1,4)
          katt(joff+j,5)=KHVJ1(j+1,5)
          katt(joff+j,6)=KHVJ1(j+1,6)
          patt(joff+j,1)=PHVJ1(j+1,1)
          patt(joff+j,2)=PHVJ1(j+1,2)
          patt(joff+j,3)=PHVJ1(j+1,3)
          patt(joff+j,4)=PHVJ1(j+1,4)
          patt(joff+j,5)=PHVJ1(j+1,5)
          patt(ip,1)=patt(ip,1)+PHVJ1(j+1,1)
          patt(ip,2)=patt(ip,2)+PHVJ1(j+1,2)
          patt(ip,3)=patt(ip,3)+PHVJ1(j+1,3)
         enddo
        endif
        ELSE
         katt(ip-1,1)=910000
        endif
        endif
        katt(ip,1)=920000+katt(katt(ip,3),1)
        IF(katt(ip-1,1).gt.915000)  THEN
          idtst=iabs(khvy(1,1)) 
          IF(idtst.gt.499.and.idtst.lt.599.or.
     1    idtst.gt.4999.and.idtst.lt.5999) THEN
            idtst=iabs(khvy(nhvy,1)) 
            IF(idtst.gt.499.and.idtst.lt.599.or.
     1      idtst.gt.4999.and.idtst.lt.5999) THEN
            ELSE
              katt(ip-1,1)=910000
              katt(ip,1)=910000
            ENDIF
          ENDIF
        ENDIF
        patt(ip,5) =  patt(katt(ip,3),5)
        patt(ip,4)=sqrt(patt(ip,1)**2+patt(ip,2)**2+patt(ip,3)**2
     1  +patt(ip,5)**2)
        else
        katt(ip,1)=910000
        do I=katt(ip,5),katt(ip,6)
            katt(I,3)=0
        enddo
        endif
        endif
      END DO
CCCCC        ik = 0
CCCCC        DO IP = 1, NATT
CCCCC        ik = ik + 1
CCCCC        ISTAT=katt(ip,4)
CCCCC        imo(1)=katt(ip,3)
CCCCC        imo(2)=0
CCCCC        idau(1)=katt(ip,5)
CCCCC        idau(2)=katt(ip,6)
CCCCC        IF(ik.eq.NATT)  ik = -1
CCCCC        DO J = 1, 3
CCCCC          P(j)  = patt(ip,j)    
CCCCC          V(j)  = 0.        
CCCCC        END DO
CCCCC          E     = patt(ip,4)    
CCCCC          AMASS = patt(ip,5)
CCCCC        WRITE(62,6116)ik,ISTAT,katt(ip,1),imo,idau
CCCCC     1,P,E,AMASS
CCCCC      END DO
C
C 
          matt=0
          ksptt=0
CCCCC          WRITE(6,888) natt
 888      FORMAT(' natt ',I10)
          do ii = 1, natt
        if(katt(ii,4).lt.12)  THEN
        if(katt(ii,4).eq.11)  THEN
           if(katt(ii,1).eq.910000) go to 777
           if(iabs(katt(ii,1)).eq.1) go to 777
           if(iabs(katt(ii,1)).eq.2) go to 777
           if(iabs(katt(ii,1)).eq.1103) go to 777
           if(iabs(katt(ii,1)).eq.2101) go to 777
           if(iabs(katt(ii,1)).eq.2103) go to 777
           if(iabs(katt(ii,1)).eq.2203) go to 777
           if(iabs(katt(ii,1)).eq.3) go to 777
           if(iabs(katt(ii,1)).eq.3101) go to 777
           if(iabs(katt(ii,1)).eq.3103) go to 777
           if(iabs(katt(ii,1)).eq.3201) go to 777
           if(iabs(katt(ii,1)).eq.3203) go to 777
           if(iabs(katt(ii,1)).eq.3303) go to 777
           if(iabs(katt(ii,1)).eq.21) go to 777
           if(iabs(katt(ii,1)).eq.4) go to 777
           if(iabs(katt(ii,1)).eq.5) go to 777
        endif
           matt=matt+1
           katt(matt,1)=katt(ii,1)
           katt(matt,3)=katt(ii,3)-ksptt
           if(katt(matt,3).lt.0) katt(matt,3)=0
           katt(matt,4)=katt(ii,4)
      if(katt(matt,1).gt.914000.and.katt(matt,1).lt.930000) THEN
             katt(matt,1)=katt(matt,1)-920000
             katt(matt,3)=0
             katt(matt,4)=12
      endif
           katt(matt,5)=katt(ii,5)-ksptt
           if(katt(matt,5).lt.0) katt(matt,5)=0
           katt(matt,6)=katt(ii,6)-ksptt
           if(katt(matt,6).lt.0) katt(matt,6)=0
           patt(matt,1)=patt(ii,1)
           patt(matt,2)=patt(ii,2)
           patt(matt,3)=patt(ii,3)
           patt(matt,4)=patt(ii,4)
           patt(matt,5)=patt(ii,5)
           go to 787
 777       ksptt=ksptt+1
 787       continue
        else
           ksptt=ksptt+1
        endif
        enddo
        natt=matt
        else
        DO ip = 1, NATT
        if(katt(ip,1).gt.90.and.katt(ip,1).lt.94) then 
        pt=sqrt(patt(ip,1)**2+patt(ip,2)**2)
        if(pt.gt.5.0) then
        njets = njets +1
        endif
        endif
        END DO
        endif
CCCCC          WRITE(6,888) natt
         vsshep(3)=float(njets)
C--
       CALL HEPEvent('hijing',IRUN,NATT,vsshep(1),vsshep(2),psshep(5)
     1 ,vsshep(3),psshep(1),psshep(2),psshep(3),psshep(4))
       CALL HepInfo(N0+N01+N10+N11,NP,NT,njets)
        ik=0
        DO IP = 1, NATT
        ik = ik + 1
        ISTAT=katt(ip,4)
        IF(ISTAT.EQ.11) ISTAT=2
        imo(1)=katt(ip,3)
        imo(2)=0
        idau(1)=katt(ip,5)
        idau(2)=katt(ip,6)
        IF(ik.eq.NATT)  ik = -1
        DO J = 1, 3
          P(j)  = patt(ip,j)    
          V(j)  = 0.        
        END DO
          E     = patt(ip,4)    
          AMASS = patt(ip,5)
      CALL HEPPart(ik,ISTAT,katt(ip,1),imo,idau,P,E,AMASS,V,X4) 
CCCCCC        WRITE(61,6116)ik,ISTAT,katt(ip,1),imo,idau
CCCCCC     1,P,E,AMASS
 6116   FORMAT(7I6,5G12.5)
      END DO
      GO TO 999
      END


