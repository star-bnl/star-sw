
      SUBROUTINE PHO_INITPMAP(Imode,Mapfrom,Mapto,Ifast)
C*********************************************************************
 
C     maps more exotic hadrons to known particles
 
C     input:   imode    -2: print current mappings
C                       -1: initialize default selection
C                        1: add particle combination to list
C
C              mapfrom  PDG particle code of (exotic) hadron
C              mapto    PDG particle code of known hadron
C              ifast    Use simplified mapping scheme p,pi+,pi0
 
C     output:  in /pompmp/
C              mpmapp   table where mappings are stored
C              nmapp    number of mappings available
 
C*********************************************************************
      IMPLICIT NONE
      INTEGER i , ibar , ichar , idcpc , Ifast , ifl1 , ifl2 , Imode , 
     &        IPHO_BAR3 , IPHO_CHR3 , IPHO_FINDIDX , IPHO_PDG2ID , 
     &        Mapfrom , Mapto , maptonew , null
      DOUBLE PRECISION PHO_PMASS , pmass , pmass_k
 
      INCLUDE 'inc/pobeam'
      INCLUDE 'inc/poinou'
      INCLUDE 'inc/podebg'
      INCLUDE 'inc/popar2'
 
C This flag makes init. faster by reducing the number of particles to
C map
 
      DATA NMApp/0/
      DATA maptonew/ - 1/
 
      IF ( Imode.EQ.-2 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,30A/)')
     &         'PHO_INITPMAP: particle mappings'
         DO i = 1 , NMApp
            WRITE (LO,'(1X,I8,A,I8)') MPMapp(1,i) , '  -->' , 
     &             MPMapp(2,i)
         END DO
      ELSE IF ( Imode.EQ.-1 ) THEN
        ! Example how to force fixed particle assignments
        !omega -> pi0
        ! mpmapp(1,1) = 223
        ! mpmapp(2,1) = 111
        ! nmapp = nmapp + 1
        ! Call pho_setpdf(maptonew, NULL, mapfrom, NULL, NULL, NULL, 2)
 
         NMApp = 0
      ELSE IF ( Imode.EQ.1 ) THEN
 
         IF ( IPHO_FINDIDX(1,Mapfrom,MAX(1,NMApp)).NE.-1 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,I5)')
     &            "PHO_INITPMAP: Error, trying to overwrite mapping for"
     &           , Mapfrom
            CALL PHO_ABORT
         END IF
 
         IF ( Mapto.EQ.-1 ) THEN
C       Determine mapping partner
            idcpc = IPHO_PDG2ID(Mapfrom)
            ichar = IPHO_CHR3(idcpc,0)
            ibar = IPHO_BAR3(idcpc,0)
            pmass = PHO_PMASS(idcpc,0)
            pmass_k = PHO_PMASS(321,1)
            IF ( ibar.NE.0 ) THEN
               IF ( (ichar.NE.0) .OR. (Ifast.NE.0) ) THEN
                  maptonew = SIGN(2212,Mapfrom)
               ELSE
                  maptonew = SIGN(2112,Mapfrom)
               END IF
            ELSE IF ( ichar.EQ.0 ) THEN
               ifl1 = IQ_list(1,idcpc)
               ifl2 = IQ_list(2,idcpc)
              ! map to pi0 if unflavored
               IF ( (ifl1.EQ.-ifl2) .OR. (Ifast.NE.0) ) THEN
                  maptonew = 111
               ELSE
              !map to K0
                  maptonew = SIGN(311,Mapfrom)
               END IF
            ELSE IF ( (Ifast.NE.0) .AND. (pmass.LE.pmass_k) ) THEN
               maptonew = SIGN(211,Mapfrom)
            ELSE
               maptonew = SIGN(321,Mapfrom)
            END IF
         END IF
 
         IF ( Ifast.NE.0 ) maptonew = ABS(maptonew)
 
         MPMapp(1,NMApp+1) = Mapfrom
         MPMapp(2,NMApp+1) = maptonew
         NMApp = NMApp + 1
 
        ! Map PDF entry of intialized particle to the requested particle
         CALL PHO_SETPDF(maptonew,null,Mapfrom,null,null,null,2)
 
         IF ( LPRi.GT.4 ) THEN
            IF ( IDEb(91).GE.0 ) WRITE (LO,'(1X,A,1X,I8,A,I8)')
     &            'PHO_INITPMAP: added mapping: ' , MPMapp(1,NMApp) , 
     &           '     -->' , MPMapp(2,NMApp)
         END IF
      ELSE
         WRITE (LO,'(1X,A,I5)') 'PHO_INITPMAP: Error, invalid IMODE: ' , 
     &          Imode
         CALL PHO_ABORT
      END IF
 
      END SUBROUTINE
