
      SUBROUTINE PHO_SAMPRO(Ip,Ifp1,Ifp2,Ecm,Pvir1,Pvir2,Sprob,Iproc)
C***********************************************************************
C
C     routine to sample kind of process
C
C     input:   IP        particle combination
C              IFP1/2    PDG number of particle 1/2
C              ECM       c.m. energy (GeV)
C              PVIR1/2   virtuality of particle 1/2 (GeV**2, positive)
C              SPROB     suppression factor for processes 1-7
C                        due to rapidity gap survival probability
C              IPROC     mode
C                          -2     output of statistics
C                          -1     initialization
C                           0     sampling of process
C
C     output:  IPROC     kind of interaction process:
C                           1  non-diffractive resolved process
C                           2  elastic scattering
C                           3  quasi-elastic rho/omega/phi production
C                           4  central diffraction
C                           5  single diffraction according to IDIFF1
C                           6  single diffraction according to IDIFF2
C                           7  double diffraction
C                           8  single-resolved / direct processes
C
C***********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Ip , Ifp1 , Ifp2 , Iproc
      DOUBLE PRECISION Ecm , Pvir1 , Pvir2 , Sprob
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  cross sections
      INCLUDE 'inc/pocsec'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  general process information
      INCLUDE 'inc/poprcs'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
 
      DOUBLE PRECISION pro , xprob , sigsdi , calls , sigsum , ecmsum
      DIMENSION pro(8,4) , xprob(8) , sigsdi(2)
      DIMENSION calls(4) , sigsum(4) , ecmsum(4)
 
      INTEGER i , k , kmax
      DOUBLE PRECISION DT_RNDM
      DOUBLE PRECISION sigddi , sighd , sighr , signdr , xi
 
      IF ( LPRi.GT.4 .AND. IDEb(11).GE.15 )
     &      WRITE (LO,'(/,1X,A,/5X,I3,2I6,1P4E11.3)')
     &      'PHO_SAMPRO: called with IP,IFP1/2,ECM,PVIR1/2,SPROB' , Ip , 
     &     Ifp1 , Ifp2 , Ecm , Pvir1 , Pvir2 , Sprob
 
      IF ( Iproc.GE.0 ) THEN
 
C  interpolate cross sections
         CALL PHO_CSINT(Ip,Ifp1,Ifp2,-1,-1,Ecm,Pvir1,Pvir2)
 
C  cross check
         IF ( (Ip.EQ.1) .AND. ((Sprob.GT.1.D0) .OR. (Sprob.LT.0.D0)) )
     &        THEN
            IF ( LPRi.GT.4 )
     &            WRITE (LO,'(/,1X,A,/5X,I12,I3,2I6,1P4E11.3)')
     &            'PHO_SAMPRO: inconsistent gap survival probability' , 
     &           'EVENT,IP,IFP1/2,ECM,PVIR1/2,SPROB:' , KEVent , Ip , 
     &           Ifp1 , Ifp2 , Ecm , Pvir1 , Pvir2 , Sprob
         END IF
 
C  calculate cumulative probabilities
         IF ( ISWmdl(1).EQ.3 ) THEN
            IF ( ISWmdl(2).GE.1 ) THEN
               sigsdi(1) = SIGlsd(1) + SIGhsd(1)
               sigsdi(2) = SIGlsd(2) + SIGhsd(2)
               sigddi = SIGldd + SIGhdd
               signdr = SIGine - SIGvm(0,0) - SIGcdf(0) - SIGdir - 
     &                  sigsdi(1) - sigsdi(2) - sigddi
               xprob(1) = signdr*Sprob*DBLE(IPRon(1,Ip))
               xprob(2) = xprob(1) + SIGela*Sprob*DBLE(IPRon(2,Ip))
               xprob(3) = xprob(2) + SIGvm(0,0)*Sprob*DBLE(IPRon(3,Ip))
               xprob(4) = xprob(3) + SIGcdf(0)*Sprob*DBLE(IPRon(4,Ip))
               xprob(5) = xprob(4) + sigsdi(1)*Sprob*DBLE(IPRon(5,Ip))
               xprob(6) = xprob(5) + sigsdi(2)*Sprob*DBLE(IPRon(6,Ip))
               xprob(7) = xprob(6) + sigddi*Sprob*DBLE(IPRon(7,Ip))
               xprob(8) = xprob(7) + SIGdir*DBLE(IPRon(8,Ip))
            ELSE
               sighr = 0.D0
               IF ( IPRon(1,Ip).EQ.1 ) sighr = SIGhar
               sighd = 0.D0
               IF ( IPRon(8,Ip).EQ.1 ) sighd = SIGdir
               xprob(1) = sighr/(sighr+sighd)
               xprob(2) = xprob(1)
               xprob(3) = xprob(1)
               xprob(4) = xprob(1)
               xprob(5) = xprob(1)
               xprob(6) = xprob(1)
               xprob(7) = xprob(1)
               xprob(8) = xprob(1) + sighd/(sighr+sighd)
            END IF
 
            IF ( IDEb(11).GE.15 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3)')
     &               'PHO_SAMPRO: partial cross sections for IP' , Ip
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,I3,2X,1PE12.4)') 1 , 
     &              xprob(1)
               DO i = 2 , 8
                  IF ( LPRi.GT.4 ) WRITE (LO,'(5X,I3,2X,1PE12.4)') i , 
     &                 xprob(i) - xprob(i-1)
               END DO
            END IF
 
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,I4)')
     &            'PHO_SAMPRO:ERROR: unsupported model' , ISWmdl(1)
            CALL PHO_ABORT
         END IF
 
         IF ( xprob(8).LT.1.D-20 ) THEN
            IF ( IDEb(11).GE.2 .AND. LPRi.GT.4 )
     &            WRITE (LO,'(1X,2A,/10X,A,1P,I2,2E11.3)')
     &            'PHO_SAMPRO:ERROR: ' , 
     &           'activated processes have vanishing cross section sum'
     &           , 'IP,ECM,SIG_sum:' , Ip , Ecm , xprob(8)
            Iproc = 0
            RETURN
         END IF
 
C  sample process
         xi = DT_RNDM(xi)*xprob(8)
         DO i = 1 , 8
            IF ( xi.LE.xprob(i) ) GOTO 50
         END DO
 50      Iproc = MIN(i,8)
 
         calls(Ip) = calls(Ip) + 1.D0
         pro(Iproc,Ip) = pro(Iproc,Ip) + 1.D0
         ecmsum(Ip) = ecmsum(Ip) + Ecm
         IF ( ISWmdl(2).GE.1 ) THEN
            sigsum(Ip) = sigsum(Ip) + xprob(8)
         ELSE
            sigsum(Ip) = sigsum(Ip) + SIGgen(3)
         END IF
 
C  debug output
         IF ( LPRi.GT.4 .AND. IDEb(11).GE.5 )
     &         WRITE (LO,'(1X,A,I3,I12,I4)')
     &         'PHO_SAMPRO: IP,CALL,PROC-ID' , Ip , INT(calls(Ip)+0.1D0)
     &        , Iproc
 
C  statistics initialization
      ELSE IF ( Iproc.EQ.-1 ) THEN
         DO k = 1 , 4
            DO i = 1 , 8
               pro(i,k) = 0.D0
            END DO
            calls(k) = 0.D0
            sigsum(k) = 0.D0
            ecmsum(k) = 0.D0
         END DO
 
C  write out statistics
      ELSE IF ( Iproc.EQ.-2 ) THEN
         kmax = 4
         IF ( ISWmdl(2).EQ.0 ) kmax = 1
         DO k = 1 , kmax
            IF ( calls(k).GT.0.5D0 ) THEN
               sigsum(k) = sigsum(k)/calls(k)**2
               ecmsum(k) = ecmsum(k)/calls(k)
               IF ( IDEb(11).GE.0 ) THEN
                  IF ( LPRi.GT.4 )
     &                  WRITE (LO,'(/,1X,2A,I4,1PE12.3,/,1X,A)')
     &                  'PHO_SAMPRO: internal process statistics ' , 
     &                 '(IP,<Ecm>)' , k , ecmsum(k) , 
     &                 '---------------------------------------'
                  IF ( LPRi.GT.4 ) WRITE (LO,'(8X,A)')
     &                  '        process      sampled    cross section'
                  IF ( ISWmdl(2).GE.1 ) THEN
                     IF ( LPRi.GT.4 )
     &                     WRITE (LO,'(9(/5X,A,0PF12.0,5X,1PE12.3))')
     &                     '    all processes' , calls(k) , calls(k)
     &                    *sigsum(k) , ' nondif.inelastic' , pro(1,k) , 
     &                    pro(1,k)*sigsum(k) , '          elastic' , 
     &                    pro(2,k) , pro(2,k)*sigsum(k) , 
     &                    'vmeson production' , pro(3,k) , pro(3,k)
     &                    *sigsum(k) , '   double pomeron' , pro(4,k) , 
     &                    pro(4,k)*sigsum(k) , ' single diffr.(1)' , 
     &                    pro(5,k) , pro(5,k)*sigsum(k) , 
     &                    ' single diffr.(2)' , pro(6,k) , pro(6,k)
     &                    *sigsum(k) , ' double diffract.' , pro(7,k) , 
     &                    pro(7,k)*sigsum(k) , ' direct processes' , 
     &                    pro(8,k) , pro(8,k)*sigsum(k)
                  ELSE
                     IF ( LPRi.GT.4 )
     &                     WRITE (LO,'(3(/5X,A,0PF12.0,5X,1PE12.3))')
     &                     '    all processes' , calls(k) , calls(k)
     &                    *sigsum(k) , '  double resolved' , pro(1,k) , 
     &                    pro(1,k)*sigsum(k) , ' single res + dir' , 
     &                    pro(8,k) , pro(8,k)*sigsum(k)
                  END IF
               END IF
            END IF
         END DO
      END IF
 
      END SUBROUTINE
