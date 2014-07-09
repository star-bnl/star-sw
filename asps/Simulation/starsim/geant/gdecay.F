* $Id: gdecay.F,v 1.6 2014/07/09 18:27:55 jwebb Exp $
* $Name:  $
* $Log: gdecay.F,v $
* Revision 1.6  2014/07/09 18:27:55  jwebb
* Previous version of gdecay.F limited the decay products of a particle to
* particles with geant ID <= 99 or <= 999.
*
* This version removes that restriction for the case where the geant IDs of
* a single decay branch are placed within the user buffer of the particle.
*
* e.g. in gstar_part.g
*
*   PARTICLE P_N_XI     "(pnXi-) --> (hyper triton) pi-" _
*                       code   = 60004     pdg   = 0             ,
*                       trktyp = kGtNeut   mass  = 3.1975        ,
*                       charge = 0         tlife = 1.6378e-10
*
*      """ Append actual decay mode in user buffer.  Up to 3 body decay """
*      """ (phase space decay) supported """
*      uw = { 0, 61053, 09 }
*      Call GSPART( %code, %title, %trktyp, %mass, %charge, %tlife, uw, nw )
*
* Revision 1.5  2010/03/22 22:03:56  perev
* Dealing with \/afs/rhic.bnl.gov/star/packages/.DEV fixed
*
* Revision 1.4  2010/03/02 17:40:42  perev
* For year < 2009 setenv G3KrollWada YES switches on new Daltz decay
*
* Revision 1.3  2009/07/11 21:27:42  perev
* Remove <cr>
*
* Revision 1.2  2009/06/09 01:02:17  perev
* Dalitz decays in starsim T.Ullrich & P.Nevski
*
* Revision 1.2  2003/10/06 15:55:47  nevski
* allow for particles codes up to 999 to be packed in decay MODEs
*
* Revision 1.1  2000/01/06 22:09:14  nevski
* unlimited track/vertex version
*
* Revision 1.1.1.1  1995/10/24 10:21:23  cernlib
* Geant
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE GDECAY
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Control routine for generation of particle decays.            *
C.    *                                                                *
C.    *  MODE(I)        I'th decay mode of current particle            *
C.    *  BRATIO(I)      branching ratio for I'th decay mode of         *
C.    *                 current particle.                              *
C.    *                                                                *
C.    *    ==>Called by : GHSTOP,GTHADR,GTNEUT,GTMUON                  *
C.    *       Author    G.Patrick *********                            *
C.    *                                                                *
C.    *  Modifications history                                         *
C.    *  PN, 28.05.99:                                                 *
C     *      Allow "one body" decay - particle type transformation     *
C.    *  PN, 11.05.09:                                                 *
C.    *      in case of eeX and mumuX decays use HELIOS DALGEN code    *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gctrak.inc"
#include "geant321/gconsp.inc"
#include "geant321/gcking.inc"
#include "geant321/gckine.inc"
#include "geant321/gcunit.inc"
#include "geant321/gcphys.inc"
#if defined(CERNLIB_USRJMP)
#include "geant321/gcjump.inc"
#endif
      DIMENSION BAREA(7)
      DIMENSION BETA(4)
      DIMENSION BRATIO(6)
      DIMENSION MODE(6)
      DIMENSION NTYPE(3)
      DIMENSION PCM(4,3)
      DIMENSION XM(3)
      DIMENSION RNDM(1)
      LOGICAL   ROTATE
      Integer   Nba/100/
      Integer   nw   
      Real      uw(32) ! user buffer
      Real      P(5,5)
      Integer   nt,n1,n2,n3,i,idal,mytype
      character *8 cKrollWada
      character *80 cSTAR
c     character *20 napart
      integer      iKrollWada/-1/,iYear,ln,lnblnk
      save         iKrollWada,cKrollWada
      real mycharge, mytlife

c
c Call GFpart to retrieve user buffer.  Expect to overwrite everything
c with poorly designed direct access to zebra below...
c
      Call GFpart( ipart,napart,mytype,amass,mycharge,mytlife,uw,nw )

C.    ------------------------------------------------------------------
C.
C             Search for parent particle in decay list.
C
C
      KCASE   = NAMEC(5)
      NGKINE  = 0
      IF(IDCAY.EQ.2) THEN
         DESTEP = DESTEP+GETOT
         ISTOP  = 2
         GO TO 99
      ENDIF
      DMASS = AMASS
      JPA = LQ(JPART-IPART)
      JDK1 = LQ(JPA-1)
      JDK2 = LQ(JPA-2)

      IF (nw.gt.0)                                 GO TO 666

      IF (JDK1.LE.0)                               GO TO 90
      IF (JDK2.LE.0)                               GO TO 90
  
      DO 5 I=1,6
         BRATIO(I)=Q(JDK1+I)
         MODE(I)=IQ(JDK2+I)
   5  CONTINUE

C
C             Generate branching ratio and select decay mode.
C
      NBR      = 1
      BAREA(1) = 0.
      DO 10 I=2,7
         BRADD    = BRATIO(I-1)
         IF (BRADD.EQ.0.) GO TO 20
         NBR      = NBR+1
         BAREA(I) = BAREA(I-1)+BRADD
  10  CONTINUE
C
  20  CALL GRNDM(RNDM,1)
      BRAND    = 100.*RNDM(1)
      IF (BRAND.GE.BAREA(NBR)) GO TO 99
      ID       = IABS((LOCATF(BAREA,NBR,BRAND)))

C
C             Unpack decay mode.
C
      MXX      = MODE(ID)
      if (Mxx.lt.1 000 000) then
       nba=100
      else
       nba=1000
      endif

 666  CONTINUE ! Branch here to allow unpacking of single decay mode from user words

      IF (nw.eq.0) THEN

         NTYPE(1) = MOD(MXX,Nba)
         NTYPE(2) = MOD(MXX/Nba,Nba)
         NTYPE(3) = MOD(MXX/Nba/Nba,Nba)
         
      ELSE ! If user buffer is filled, unpack particle ids from there...
      
         NTYPE(1) = uw( 3 )
         NTYPE(2) = uw( 2 )
         NTYPE(3) = uw( 1 )

      ENDIF


      JP1 = LQ(JPART-NTYPE(1))
      JP2 = LQ(JPART-NTYPE(2))
      XM(1) = Q(JP1+7)
      XM(2) = Q(JP2+7)

      IF (Ntype(2).eq.0) THEN
C
C             "one body" decay - particle type transformation
C
         NGKINE  = 1
         CALL VZERO(PCM,4)
         PCM(4,1)=XM(1)

      ELSEIF (Ntype(3).eq.0) THEN
C
C             Two body decay.
C
         NGKINE  = 2
         IF (TLIFE.LT.1.E-15) THEN
            XMTOT = XM(1)+XM(2)
            DO 30 I=1,1000
C--  Create Lorentz distributed energy with FWHM HBAR/TLIFE.
C--  (via integral-transformation of Lorentz-distribution)
C--                 (M.Guckes)
              CALL GRNDM(RNDM,1)
              RMASS = DMASS
     1                + 3.291086E-25/TLIFE * TAN(PI*(RNDM(1)-0.5))
              IF (RMASS.GE.XMTOT) GO TO 40
 30         CONTINUE
            WRITE(CHMAIL,1000) IPART, NTYPE(1), NTYPE(2)
            CALL GMAIL(0,0)
            NGKINE=0
            GO TO 99
 40         DMASS = RMASS
         END IF
         CALL GDECA2(DMASS,XM(1),XM(2),PCM)
      ELSE
C
C             Three body decay.
C
        NGKINE  = 3
        JP3 = LQ(JPART-NTYPE(3))
        XM(3) = Q(JP3+7)
C-- a Dalitz decay is recognised by e+/e- or mu+/mu- pair
        n1=999
        n2=0
        n3=0
        DO i=1,3
           nt=ntype(i)
           if (nt.gt.1.and.nt.lt.7) then
              n1=min(n1,nt)
              n2=max(n2,nt)
           else
              n3=nt
           endif
        enddo
        if (iKrollWada .lt.0) then
          call getenv('G3KrollWada',cKrollWada)
          call getenv('STAR',cSTAR)
          ln = lnblnk(cSTAR)
          iYear = (ichar(cSTAR(ln-2:ln-2))-ichar('0'))*10
     1          + (ichar(cSTAR(ln-1:ln-1))-ichar('0'))
          if (iYear.le.0 .or. iYear .gt. 10) iYear = 10
          iKrollWada=1
          if (iYear.lt.9) iKrollWada=0

          if (cKrollWada(1:2) .eq. 'NO' ) iKrollWada=0
          if (cKrollWada(1:3) .eq. 'YES') iKrollWada=1
        endif

        if (iKrollWada .ne.0
     ..And.((n1.eq.2.or.n1.eq.5).and.n2.eq.n1+1.and.n3>0)) then
C        decay it via Kroll-Wada mecanism, no mass smearing
         CALL VZERO(P,25)
         P(4,1)=AMASS
         P(5,1)=AMASS
         IDAL=DALDEC(XM,P,P(1,2))
         DO 23 j=1,3
           DO 23 i=1,4
 23           PCM(i,j)=P(i,j+1)
        else
C        any other 3 body decay
         IF (TLIFE.LT.1.E-15) THEN
C--  Create Lorentz distributed energy with FWHM HBAR/TLIFE.
C--  (via integral-transformation of Lorentz-distribution)
            XMTOT = XM(1)+XM(2)+XM(3)
            DO 31 I=1,1000
              CALL GRNDM(RNDM,1)
              RMASS = DMASS
     1                + 3.291086E-25/TLIFE * TAN(PI*(RNDM(1)-0.5))
              IF (RMASS.GE.XMTOT) GO TO 41
 31         CONTINUE
            WRITE(CHMAIL,1000) IPART, NTYPE(1), NTYPE(2), NTYPE(3)
            CALL GMAIL(0,0)
            NGKINE=0
            GO TO 99
 41         DMASS = RMASS
         END IF
         CALL GDECA3(DMASS,XM(1),XM(2),XM(3),PCM)
        endif
      ENDIF
C
C             LORENTZ boost into LAB system defined along parent vector
C             followed by rotation back into GEANT system.
C
      P0       = VECT(7)
      E0       = SQRT(P0*P0+DMASS*DMASS)
      BETA(1)  = 0.
      BETA(2)  = 0.
      BETA(3)  = -P0/E0
      BETA(4)  = E0/DMASS
      CALL GFANG(VECT(4),COSTH,SINTH,COSPH,SINPH,ROTATE)
C
      DO 60 K=1,NGKINE
         IF (P0.LE.0.) THEN
            DO 59 I = 1,3
   59       GKIN(I,K) = PCM(I,K)
         ELSE
            CALL GLOREN (BETA, PCM(1,K), GKIN(1,K))
         ENDIF
         IF(ROTATE) CALL GDROT  (GKIN(1,K),COSTH,SINTH,COSPH,SINPH)
         GKIN(4,K)=SQRT(GKIN(1,K)**2+GKIN(2,K)**2+GKIN(3,K)**2+XM(K)**2)
         GKIN(5,K)=NTYPE(K)
         TOFD(K)=0.
         GPOS(1,K) = VECT(1)
         GPOS(2,K) = VECT(2)
         GPOS(3,K) = VECT(3)
  60  CONTINUE
      GO TO 99
C
C             No branching ratio defined. Call user routine
C
#if !defined(CERNLIB_USRJMP)
  90  CALL GUDCAY
#endif
#if defined(CERNLIB_USRJMP)
  90  CALL JUMPT0(JUDCAY)
#endif
C
  99  RETURN
 1000 FORMAT(' ***** GDECAY ERROR : Not enough energy available for ',
     +       'decay of resonance',I3,' to',3I3,'; no decay.')
      END

