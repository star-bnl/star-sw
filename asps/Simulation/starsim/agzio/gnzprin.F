* $Id: gnzprin.F,v 1.1.1.1 2004/01/12 23:49:37 potekhin Exp $
* $Log: gnzprin.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:37  potekhin
*
* Revision 1.2  2001/03/02 21:03:13  nevski
* agmer splitted, gnz restored
*
* Revision 1.1  2001/02/27 10:14:18  nevski
*  first working release
*
      SUBROUTINE GNZPRIN (ILINK,LEVL)
C------------------------------------------------------------------------------
C GNZPRIN - print out contents of gate on link set ILINK with print level LEVL.
C
C In this subroutine, I constantly have to make sure that the offset IGNOFF
C is applied to anything inside array LGN, except on the pointers IGNLKP(i),
C which are calculated with this already in mind.
C
C Input : ILINK  - link set specifier
C         LEVL   - print level
C                  <=0 no printing at all
C                  = 1 print summary
C                  = 2 list stable particles
C                  = 3 list all particles
C                  = 4  "    "     "     along with decay vtx information
C                  > 4  "    "     "        "
C Original Version.                                 R.DeWolf   18/02/91
C------------------------------------------------------------------------------
#include "commons/typing.inc"
#include "geant321/gcunit.inc"
#include "gncsto.inc"
#include "genz/gnccon.inc"
#include "gncpar.inc"
#include "genz/gncevt.inc"
#include "slac_do.inc"
#include "genp_do.inc"
#include "genz/gene_do.inc"
#include "genz/genr_do.inc"
      INTEGER  IGNLKP
      EXTERNAL IGNLKP
C Maximum number in particle decay stacks:
      INTEGER      NMAXS,ILINK,LEVL
      PARAMETER   (NMAXS=500)
      INTEGER      IPSTK(NMAXS)
      CHARACTER*8  CHSTK(NMAXS)
      CHARACTER*16 CPNAM
      CHARACTER*8  CSHOR
      LOGICAL LSTAB,LPRI
      INTEGER MODUL,LPQR,LPQE,ND,NDAUG,NEED,IGATE,ISTAT
      INTEGER NEVTS,NPART,NZBANK,NDAU,NMOTH,NSTA1,NSTA2,NLIN,NPERL
      INTEGER IE,IM,IP,IL,JM(2),JD(2),IDAU,IVENT,IPDG,J1,J2,J,IMVRT
      REAL    VERTX(4),PLAB(4),VMOD,VXYZT(4),PDUM(3),AROT,ROTMA(3,3)
      INTEGER LEVNT,LRUNT,LGENE,LGENR,LOFF,LMOTH,LDAU,LGENP,LD1,NGAP
      LGNPRI = LOUT
C
C If LEVL <= 0, just return
      IF (LEVL.LE.0) GOTO 999
C
C Assume that there are no ZEBRA calls in this routine. Pointers are static.

      IF(ILINK.EQ.1)THEN
         LPQR = LGN(IGNLKP(1))
         LPQE = LGN(IGNLKP(2))
      ELSE IF(ILINK.EQ.2)THEN
         LPQR = LGN(IGNLKP(3))
         LPQE = LGN(IGNLKP(4))
      ELSE
         WRITE(LGNPRI,10000)ILINK
10000    FORMAT(' GZNPRIN: Illegal link set specifier ',I8,';'/,
     +          10X,'Use 1 for primary set, 2 for secondary.')
         GOTO 999
      ENDIF
C
C Make these the correct pointers for LGN:
      LRUNT = LPQR+IGNOFF
      LEVNT = LPQE+IGNOFF
C
C Get pointers to GENZ banks:
C First GENR if the run is new:
      IF(LGNPRR.AND.LPQR.GT.0)THEN
         LGNPRR=.FALSE.
         LGENR = 0
         IF(IGN(LRUNT-2).GE.2.AND.LGN(LRUNT-2).GT.0)THEN
            LGENR = LGN(LRUNT-2)
         ELSE
            WRITE (LGNPRI,10600)LGN(LRUNT-2)
10600       FORMAT(' GNZPRIN: No GENR bank present, LGN(LRUNT-2)= ',I15)
         ENDIF
C
C Dump the run information:
C Check if the run header is present
         IF(LGENR.GT.0)THEN
            LGENR = LGENR + IGNOFF
            WRITE(LGNPRI,10400)
10400       FORMAT('0',76('='))
            WRITE(LGNPRI,10300)IGN(LGENR+JOGRNA),IGN(LGENR+JOGRRU),
     +      IGN(LGENR+JOGJID),IGN(LGENR+JOGDAT),IGN(LGENR+JOGTIM),
     +      IGN(LGENR+JOMACH)
10300       FORMAT(' GNZPRIN: Run Header: Generator ',A4,'   Run',I10,
     +             '   Generator JobID ',I10,/,
     +             10X,'Date= ',I10,'  Time= ',I10,'   Machine= ',A4)
            WRITE(LGNPRI,10500)
10500       FORMAT(1X, 76('-'))

         ENDIF
C
      ENDIF
C
C Then GENE:
C
      IF(LPQE.LE.0)THEN
         WRITE(LGNPRI,10700)
10700    FORMAT(' GNZPRIN: No event bank (EVNT) present. ')
         GOTO 90
      ENDIF
C
      LGENE = 0
      IF(IGN(LEVNT-2).GE.2.AND.LGN(LEVNT-2).GT.0)THEN
         LGENE = LGN(LEVNT-2)+IGNOFF
      ELSE
         WRITE(LGNPRI,10800)LGN(LEVNT-2)
10800    FORMAT(' GNZPRIN: No GENE bank present!!! LGENE=',I10)
         GOTO 90
      ENDIF
C
C Dump the event information:
C
C this uses the value relative to correct array:
      NEVTS = NZBANK(IGNSTO,LGENE-IGNOFF)
      IVENT = 0
      NSTA2 = 0
C
      WRITE(LGNPRI,10400)
      IGATE = IGN(LEVNT+6)
      WRITE(LGNPRI,10100)IGATE,NEVTS
10100 FORMAT(' GNZPRIN: Gate number',I7,' contains',I4,' events.')
C
      DO 70 IE = 1,NEVTS
         IVENT = IVENT + 1
         NSTA1=0
         IF(IE.GT.1)THEN
            IF(LGN(LGENE).LE.0)GOTO 80
            LGENE = LGN(LGENE)
C Make this the correct pointer for LGN:
            LGENE = LGENE + IGNOFF
         ENDIF
C
C
C Get particle data
         LGENP = 0
         IF(IGN(LGENE-2).GE.1.AND.LGN(LGENE-1).GT.0)THEN
C Make this the correct pointer for LGN:
            LGENP = LGN(LGENE-1) + IGNOFF
         ELSE
            WRITE(LGNPRI,10900)
10900       FORMAT(' GNZPRIN: No GENP bank present !!! ')
            GOTO 70
         ENDIF
C
C===>
         NPART = -1
*         print *,' ctyp=',ctyp
         CALL UHTOC(IGN(LGENP-4),4,CTYP,4)
         LENB =     IGN(LGENP-1)
         ND   =     IGN(LGENP-1)

         IF       (CTYP.eq.'GENP') then
            NPART = MOD(IGN(LGENP+JOSYS2),NPAKAC)
            MODUL = MOD(IGN(LGENP+JOSYS3),NPAKAC)
            NEED  = NPART * MODUL + NHDWAC
            NAGENP = 13
            JOSTAT = 1
            JOPDGC = 2
            JOMOT1 = 3
            JOMOT2 = 4
            JODAU1 = 5
            JOPXPX = 6
            JOPYPY = 7
            JOPZPZ = 8
            JOMASS = 9
            JOTIME = 10
            JOVXVX = 11
            JOVYVY = 12
            JOVZVZ = 13
            JOADIN = 14
            NHDWAC = 3
         ELSE IF  (CTYP.eq.'GENT') then
            MODUL = 15
            NPART = LENB/MODUL
            NEED  = NPART * MODUL
            NAGENP = 15
            JOSTAT = 1
            JOPDGC = 2
            JOMOT1 = 3
            JOMOT2 = 4
            JODAU1 = 5
            JOPXPX = 7
            JOPYPY = 8
            JOPZPZ = 9
            JOMASS = 11
            JOTIME = 15
            JOVXVX = 12
            JOVYVY = 13
            JOVZVZ = 14
            JOADIN = 10
            NHDWAC = 0
         ENDIF
C
C Event information:
C
         WRITE(LGNPRI,10500)
         WRITE(LGNPRI,11900)IVENT,IGN(LGENE+JOGRUN),IGN(LGENE+JOGEVT),
     +   IGN(LGENE+JOGNAM),NPART
11900    FORMAT(' Event',I4,' of gate: Run #',I11,' Evt #',I7,
     +             '   Gen ',A4,I8,' particles.')
         WRITE(LGNPRI,12000)(GN(LGENE+J),J=JOVRTX,JOVRTT),
     +     IGN(LGENE+JOWTFL),GN(LGENE+JOWEIG)
12000    FORMAT(' Interaction at ',3F9.4,' metres ',1X,E10.3,' seconds',
     +            /' Weight flag=',I2,'   Weight= ',E10.3)
         WRITE(LGNPRI,10500)
C
C Check NP against length of bank:
C

         IF(NEED.GT.ND)THEN
            WRITE(LGNPRI,10200)ND,NPART,NEED
10200       FORMAT(' GNZPRIN: Bank length,',I7,
     +      ', is less than that required for',I5,
     +      ' particles,(',I7,')')
            GOTO 90
         ENDIF
C
         DO 60 , IP = 1,NPART
C
C Offset for this particle
            LOFF = LGENP + MODUL*(IP-1) + NHDWAC
C
            IPDG  = IGN(LOFF+JOPDGC)
            ISTAT = IGN(LOFF+JOSTAT)
            IF(ISTAT.LE.0)GOTO 60
C
            CALL GNZPNAM(IPDG,CPNAM)
            CSHOR = CPNAM(1:8)
            CALL UCOPY(GN(LOFF+JOPXPX),PLAB(1),3)
C
            PLAB(4) = VMOD(GN(LOFF+JOPXPX),4)
C If far away enough from the vertex, and there is magnetic field
C calculate new momentum:
            IF(GN(LOFF+JOTIME).GT.TVTMIN.AND.LGNMAG)THEN
               CALL GNZGETV(ILINK,IE,IP,VERTX,IMVRT,PLAB,AROT,ROTMA)
            ENDIF

C get pointers, calculate number of mothers, stability...
            JM(1) = IGN(LOFF+JOMOT1)
            JM(2) = IGN(LOFF+JOMOT2)
            JD(1) = IGN(LOFF+JODAU1)
C
C Stable is JD(1) <= 0 and status =1
            LSTAB = .FALSE.
            IF(JD(1).LE.0.AND.ISTAT.EQ.1)LSTAB=.TRUE.
C Does this one get printed?
            LPRI = .FALSE.
            IF(LSTAB.AND.LEVL.GE.2)LPRI=.TRUE.
            IF(LEVL.GE.3)LPRI=.TRUE.
C
            NMOTH = 0
            NGAP = ABS(JM(2)) - JM(1)
            IF(NGAP.EQ.0)NGAP = 1
            IF(JM(2).LT.0)NGAP = 1
            DO 10 IM = JM(1),ABS(JM(2)),NGAP
               IF(IM.EQ.0)GOTO 20
               IF(NMOTH.EQ.NMAXS)GOTO 10
               LMOTH = LGENP + MODUL*(IM-1)+ NHDWAC
               NMOTH = NMOTH + 1
               IPSTK(NMOTH)= IM
               CALL GNZPNAM(IGN(LMOTH+JOPDGC),CPNAM)
               CHSTK(NMOTH)=CPNAM(1:8)
   10       CONTINUE
C
   20       CONTINUE
C
            IF(LPRI)THEN
               IF(NMOTH.LT.1)THEN
                  WRITE(LGNPRI,11500)IP,CSHOR,PLAB
11500             FORMAT(1X,I5,2X,A8,'  PxyzE',4F9.3,'  No Parents. ')
               ELSE IF(NMOTH.EQ.1)THEN
                  WRITE(LGNPRI,11600)IP,CSHOR,PLAB,IPSTK(1),CHSTK(1)
11600             FORMAT(I6,2X,A8,'  PxyzE',4F9.3,'  Mother ',I5,' ',A8)
               ELSE
                  WRITE(LGNPRI,11700)IP,CSHOR,PLAB,IPSTK(1),CHSTK(1)
11700             FORMAT(I6,2X,A8,'  PxyzE',4F9.3,'  Mothers',I5,' ',A8)
                  DO 30 , IM = 2,NMOTH
                     WRITE(LGNPRI,11800)IPSTK(IM),CHSTK(IM)
11800                FORMAT(67X,I5,' ',A8)
   30             CONTINUE
               ENDIF
            ENDIF
C
C Now handle the daughters ( having got rid of the parents !)
            IF(JD(1).GT.0)THEN
               LD1 = LGENP + MODUL*(JD(1)-1) + NHDWAC
C Write out decay point if far enough away in x,y:
               IF(LEVL.GE.4)THEN
                  IF(GN(LD1+JOTIME).GT.TVTMIN)THEN
                     CALL GNZGETV(ILINK,IE,JD(1),VXYZT,
     +                            IMVRT,PDUM,AROT,ROTMA)
                     WRITE(LGNPRI,11000) VXYZT
11000                FORMAT(9X,'|....decays at xyz:',3F11.6,
     >                            ', time: ',E10.3)
                  ENDIF
               ENDIF
C
C Arrange for stack of descendants:
               NDAUG = NMAXS
               CALL GNZDLIS(IGN(LGENP+1),IP,NDAUG,IPSTK)
               NDAU = NDAUG
C
C Stack the numbers and names of the daughters:
               DO 40 IDAU = 1,NDAU
                  J = IPSTK(IDAU)
                  LDAU = LGENP + MODUL*(J-1) + NHDWAC
                  CALL GNZPNAM(IGN(LDAU+JOPDGC),CPNAM)
                  CHSTK(IDAU)=CPNAM(1:8)
   40          CONTINUE
C
C Write them out, NPERL per line.
C
               NPERL = 4
               NLIN = (NDAU-1)/NPERL + 1
               DO 50 IL = 1,NLIN
                  J1 = (IL-1)*NPERL + 1
                  J2 = IL*NPERL
                  IF(J2.GT.NDAU)J2=NDAU
                  IF(J1.GT.NDAU)J1=NDAU
                  IF(LPRI)THEN
                     IF(IL.EQ.1)THEN
                        WRITE(LGNPRI,11100)(IPSTK(J),CHSTK(J),J=J1,J2)
11100                   FORMAT(9X,'+----->',5(I5,' ',A8,3X) )
                     ELSE
                        WRITE(LGNPRI,11200)(IPSTK(J),CHSTK(J),J=J1,J2)
11200                   FORMAT(16X,5(I5,' ',A8,3X) )
                     ENDIF
                  ENDIF
   50          CONTINUE
C
            ELSE
               NSTA1 = NSTA1 + 1
               NSTA2 = NSTA2 + 1
            ENDIF
C
C End of particle loop:
   60    CONTINUE
         WRITE(LGNPRI,11300)NSTA1,IVENT
11300    FORMAT(/,1X,I6,' stable particles in event',I5)
C
C End of event loop:
   70 CONTINUE
      WRITE(LGNPRI,10500)
   80 WRITE(LGNPRI,11400)IGATE,NSTA2,IVENT
11400 FORMAT(' Gate',I6,' has a total of',I6,' particles in',I5,
     +' events.')
C
   90 CONTINUE
      WRITE(LGNPRI,10400)
C
  999 END



