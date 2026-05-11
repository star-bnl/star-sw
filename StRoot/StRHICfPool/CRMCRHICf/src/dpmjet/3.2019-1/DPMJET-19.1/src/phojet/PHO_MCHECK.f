
      SUBROUTINE PHO_MCHECK(J1,Irej)
C********************************************************************
C
C    check parton momenta for fragmentation
C
C    input:      J1      first  string number
C                        /POEVT1/
C                        /POSTRG/
C
C    output:             /POEVT1/
C                        /POSTRG/
C                IREJ    0  successful
C                        1  failure
C
C    in case of very small string mass:
C                NNCH    mass label of string
C                        0  string
C                       -1  octett baryon / pseudo scalar meson
C                        1  decuplett baryon / vector meson
C                IBHAD   hadron number according to CPC,
C                        string will be treated as resonance
C                        (sometimes far off mass shell)
C
C    constant WIDTH ( 0.01GeV ) determines range of acceptance
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION am10 , am102 , am8 , am82 , amps , amps2 , amve , 
     &                 amve2 , DEPS , strm , WIDTH
      INTEGER i10 , i8 , ips , Irej , ive , J1
      SAVE 
 
      PARAMETER (WIDTH=0.01D0,DEPS=1.D-15)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  color string configurations including collapsed strings and hadrons
      INCLUDE 'inc/postrg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
 
      Irej = 0
C  quark antiquark jet
      strm = PHEp(5,NPOs(1,J1))
      IF ( NCOde(J1).EQ.3 ) THEN
         CALL PHO_MEMASS(IPAr1(J1),IPAr2(J1),amps,amps2,amve,amve2,ips,
     &                   ive)
         IF ( IDEb(18).GE.5 .AND. LPRi.GT.4 )
     &         WRITE (LO,'(1X,A,/3X,I3,5E12.3)')
     &         'PHO_MCHECK:1.STRING NO, CHMASS,AMPS,AMPS2,AMVE,AMVE2 ' , 
     &        J1 , strm , amps , amps2 , amve , amve2
         IF ( strm.LT.amps ) THEN
            Irej = 1
            IFAil(20) = IFAil(20) + 1
            RETURN
         ELSE IF ( strm.LT.amps2 ) THEN
            IF ( strm.LT.(amve-WIDTH) ) THEN
               NNCh(J1) = -1
               IBHad(J1) = ips
            ELSE
               NNCh(J1) = 1
               IBHad(J1) = ive
            END IF
         ELSE
            NNCh(J1) = 0
            IBHad(J1) = 0
         END IF
C  quark diquark or v.s. jet
      ELSE IF ( (NCOde(J1).EQ.4) .OR. (NCOde(J1).EQ.6) ) THEN
         CALL PHO_BAMASS(IPAr1(J1),IPAr2(J1),IPAr3(J1),am8,am82,am10,
     &                   am102,i8,i10)
         IF ( IDEb(18).GE.5 .AND. LPRi.GT.4 )
     &         WRITE (LO,'(1X,A,/5X,I3,5E12.3)')
     &         'PHO_MCHECK:1.STRING NO, CHMASS,AM8,AM82,AM10,AM102 ' , 
     &        J1 , strm , am8 , am82 , am10 , am102
         IF ( strm.LT.am8 ) THEN
            Irej = 1
            IFAil(19) = IFAil(19) + 1
            RETURN
         ELSE IF ( strm.LT.am82 ) THEN
            IF ( strm.LT.(am10-WIDTH) ) THEN
               NNCh(J1) = -1
               IBHad(J1) = i8
            ELSE
               NNCh(J1) = 1
               IBHad(J1) = i10
            END IF
         ELSE
            NNCh(J1) = 0
            IBHad(J1) = 0
         END IF
C  diquark a-diquark string
      ELSE IF ( NCOde(J1).EQ.5 ) THEN
         CALL PHO_DQMASS(IPAr1(J1),IPAr2(J1),IPAr3(J1),IPAr4(J1),am82,
     &                   am102)
         IF ( IDEb(18).GE.5 .AND. LPRi.GT.4 )
     &         WRITE (LO,'(1X,A,/5X,I3,3E12.3)')
     &         'PHO_MCHECK:1.STRING NO, CHMASS,AM82,AM102 ' , J1 , 
     &        strm , am82 , am102
         IF ( strm.LT.am82 ) THEN
            Irej = 1
            IFAil(19) = IFAil(19) + 1
            RETURN
         ELSE
            NNCh(J1) = 0
            IBHad(J1) = 0
         END IF
      ELSE IF ( NCOde(J1).LT.0 ) THEN
         RETURN
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,2A,2I8)') 'PHO_MCHECK: ' , 
     &        'inconsistent flavours for string (NO,NCODE)' , J1 , 
     &        NCOde(J1)
         CALL PHO_ABORT
      END IF
      END SUBROUTINE
