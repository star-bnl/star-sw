
      SUBROUTINE PHO_SETPCOMB
C*********************************************************************
 
C     find and set or initialize current beam/target combination index
 
C     input:   ipds1   PDG particle code side 1
C              ipds2   PDG particle code side 2
 
C     output:  in /POBEAM/
C              idxmpar   currently active index
C              mpmap   PDGids of original particle in case of mapping
C              mparisinv .True. if side1/2 need swapping
 
C*********************************************************************
      IMPLICIT NONE
 
      INCLUDE 'inc/pobeam'
      INCLUDE 'inc/podebg'
      INCLUDE 'inc/poinou'
      INCLUDE 'inc/pogcms'
      INCLUDE 'inc/pohdfl'
 
      INTEGER ipds1 , ipds2 , i , iside , iipar , ifound , k , ifast , 
     &        klim , irej , ip1 , ip2 , iremn1 , iremn2 , 
     &        IPHO_ISMAPPED , ihfldsav(2,2) , ihflssav(2) , ipavail , 
     &        IKNOWN
      DOUBLE PRECISION p1(4) , p2(4) , DEPS
      PARAMETER (DEPS=1.D-14)
C  (Anti-)Particle combination assumed to be known
      PARAMETER (IKNOWN=9)
      DIMENSION ipavail(IKNOWN)
      DATA ipavail/2212 , 211 , 111 , 22 , 2112 , 3122 , 3112 , 321 , 
     &     311/
 
#ifdef FOR_FLUKA
      DATA ifast/0/
#else
      DATA ifast/0/
#endif
#ifdef FOR_CORSIKA
      INTEGER ISTART
      DATA    ISTART / 0 /
#endif
      SAVE 
 
 
 100  iremn1 = 0
      iremn2 = 0
 
      ipds1 = IFPap(1)
      ipds2 = IFPap(2)
 
#ifdef FOR_CORSIKA
      if ( LPRI.GT.4) then
        if (istart .eq. 0 ) then
          write(LO,*) 'PHO_SETPCOMB: IFAST=',IFAST
          istart = 1
        endif
      endif
#endif
 
      IF ( (IDEqp(1).NE.0) .AND. (IDEqp(1).NE.ipds1) ) THEN
         iremn1 = -1
         MPAr(1) = IDEqp(1)
         ipds1 = IDEqp(1)
      END IF
 
      IF ( (IDEqp(2).NE.0) .AND. (IDEqp(2).NE.ipds2) ) THEN
         iremn2 = -1
         MPAr(2) = IDEqp(2)
         ipds2 = IDEqp(2)
      END IF
 
      ! Check if side1 or side2 need mapping
      MPAr(1) = IPHO_ISMAPPED(ipds1)
      MPAr(2) = IPHO_ISMAPPED(ipds2)
 
      IF ( LPRi.GT.4 ) THEN
         IF ( IDEb(90).GT.2 ) WRITE (LO,*)
     &         'PHO_SETPCOMB: searching for ' , 
     &        'mpar(1)/mpar(2) combination' , MPAr(1) , '/' , MPAr(2) , 
     &        ' ' , ipds1 , '/' , ipds2
      END IF
 
      ! Search for index in stored pairs
      IDXmpar = 0
      DO i = 1 , NMPar
         IF ( MPAr(1).EQ.MPArconf(1,i) .AND. MPAr(2).EQ.MPArconf(2,i) )
     &        THEN
            IDXmpar = i
            CALL PHO_SETPAR(1,ipds1,iremn1,0.D0)
            CALL PHO_SETPAR(2,ipds2,iremn2,0.D0)
            CALL PHO_RREGPAR
            GOTO 200
         END IF
      END DO
 
      ! If index not found, initialize new beam config
 200  IF ( IDXmpar.EQ.0 ) THEN
         IF ( LPRi.GT.4 ) THEN
            IF ( IDEb(90).GT.2 ) WRITE (LO,'(/1X,A,I5,A,I5,A)')
     &            'PHO_SETPCOMB: Beam index' , MPAr(1) , ' /' , MPAr(2)
     &           , ' not initialized.'
         END IF
 
        ! Search parameter file for combination
         ifound = 1
         DO iipar = 1 , 2
            k = 1
            IF ( ifast.NE.0 ) THEN
               klim = ifast
            ELSE
               klim = IKNOWN
            END IF
            DO i = 1 , klim
               IF ( (ifast.NE.0) .AND. (ipavail(k).EQ.MPAr(iipar)) )
     &              GOTO 220
               IF ( (ifast.EQ.0) .AND. (ipavail(k).EQ.ABS(MPAr(iipar)))
     &              ) GOTO 220
               k = k + 1
            END DO
 220        IF ( k.GT.klim ) THEN
               IF ( IDEb(90).GT.-1 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,*)
     &                  'PHO_FITPAR: no parameters available for ' , 
     &                 ABS(MPAr(iipar)) , 
     &                 '. Trying to map the combination.'
                ! Auto-map the particle to something else
               END IF
               CALL PHO_INITPMAP(1,MPAr(iipar),-1,ifast)
               ifound = 0
            END IF
         END DO
 
         IF ( ifound.EQ.0 ) GOTO 100
        ! Backup the flavors in case of an initialization with a remnant
         IF ( (iremn1.NE.0) .OR. (iremn2.NE.0) ) THEN
            DO iside = 1 , 2
               ihfldsav(iside,1) = IHFld(iside,1)
               ihfldsav(iside,2) = IHFld(iside,2)
               ihflssav(iside) = IHFls(iside)
            END DO
         END IF
 
        ! Initialize phojet for new particle combination
         CALL PHO_SETPAR(1,MPAr(1),0,0.D0)
         CALL PHO_SETPAR(2,MPAr(2),0,0.D0)
         IF ( SQSglobmax.LE.1.D0 ) THEN
            WRITE (LO,'(/1X,A,A)') 
     &       'PHO_SETPCOMB: Initialization energy too small to proceed.'
     &       , ' Aborting..'
            CALL PHO_ABORT
         END IF
         CALL PHO_GET4VECTORS(MPAr(1),MPAr(2),SQSglobmax,p1,p2)
         CALL PHO_FITPAR(1)
 
         NMPar = NMPar + 1
         MPArconf(1,NMPar) = MPAr(1)
         MPArconf(2,NMPar) = MPAr(2)
 
         IF ( LPRi.GT.4 ) THEN
            IF ( IDEb(90).GT.2 ) WRITE (LO,*)
     &            'PHO_SETPCOMB: particle configuration ' , MPAr(1) , 
     &           '/' , MPAr(2) , ' added to mparconf at ' , NMPar
         END IF
 
        !Set current index to new entry
         IDXmpar = NMPar
 
        !Search parameter file for copmbination
         CALL PHO_SREGPAR
         CALL PHO_MCINI
         CALL PHO_SAMPRO(1,IFPap(1),IFPap(2),ECM,PVIrt(1),PVIrt(2),0.D0,
     &                   -1)
         CALL PHO_PARTON(-1,0,0,p1,p2,irej)
 
         CALL PHO_EVEINI(1,p1,p2,ip1,ip2)
 
         IF ( LPRi.GT.4 ) THEN
            IF ( IDEb(90).GT.1 ) WRITE (LO,*)
     &            'PHO_SETPCOMB: Initialization complete for ' , MPAr(1)
     &           , '/' , MPAr(2)
         END IF
 
         IF ( (ipds1.NE.MPAr(1)) .OR. (ipds2.NE.MPAr(2)) .OR. 
     &        (iremn1.NE.0) .OR. (iremn2.NE.0) ) THEN
          ! Restore remnant flavors
            IF ( (iremn1.NE.0) .OR. (iremn2.NE.0) ) THEN
               IF ( LPRi.GT.4 ) THEN
                  IF ( IDEb(90).GT.1 ) WRITE (LO,'(/1X,A,I5,A,I5)')
     &                  'PHO_SETPCOMB: Restoring remnants.' , iremn1 , 
     &                 '/' , iremn2
               END IF
               DO iside = 1 , 2
                  IHFld(iside,1) = ihfldsav(iside,1)
                  IHFld(iside,2) = ihfldsav(iside,2)
                  IHFls(iside) = ihflssav(iside)
               END DO
            END IF
 
            CALL PHO_SETPAR(1,ipds1,iremn1,0.D0)
            CALL PHO_SETPAR(2,ipds2,iremn2,0.D0)
            CALL PHO_RREGPAR
         END IF
      END IF
 
      IF ( LPRi.GT.4 ) THEN
         IF ( IDEb(90).GT.10 ) WRITE (LO,'(1X,A,I2)')
     &         'PHO_SETPCOMB: Index found and set to' , IDXmpar
      END IF
 
      END SUBROUTINE
