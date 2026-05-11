
      SUBROUTINE PHO_QELAST(Iproc,Jm1,Jm2,Irej)
C**********************************************************************
C
C     sampling of quasi elastic processes
C
C     input:   IPROC  2   purely elastic scattering
C              IPROC  3   q-ela. omega/omega/phi/pi+pi- production
C              IPROC  4   double pomeron scattering
C              IPROC  -1  initialization
C              IPROC  -2  output of statistics
C              JM1/2      index of initial particle 1/2
C
C     output:  initial and final particles in /POEVT1/ involving
C              polarized resonances in /POEVT1/ and decay
C              products
C
C              IREJ    0  successful
C                      1  failure
C                     50  user rejection
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION am12 , am22 , anorf , DEPS , DT_RNDM , EPS , 
     &                 PHO_PMASS , PHO_XLAM , PIMASS , ptot1 , slwght , 
     &                 ss , tt , xi
      INTEGER i , i1 , i2 , icall , icpos , idpro , ifl , igen , ipos , 
     &        Iproc , Irej , isamel , isamqe , isamvm , isp , itry , j , 
     &        Jm1 , Jm2 , k
      INTEGER NTAB
      SAVE 
 
      PARAMETER (NTAB=20,EPS=1.D-10,PIMASS=0.13D0,DEPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  c.m. kinematics of diffraction
      INCLUDE 'inc/podcms'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  some constants
      INCLUDE 'inc/pocons'
C  cross sections
      INCLUDE 'inc/pocsec'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
      DOUBLE PRECISION p , pk1 , pk2 , pmi , rmass
      DIMENSION p(4,2) , pk1(5) , pk2(5) , pmi(2) , rmass(2)
      DIMENSION ifl(2) , idpro(4)
      CHARACTER*15 PHO_PNAME
      CHARACTER*8 vmesa(0:4) , vmesb(0:4)
      DIMENSION isamvm(4,4)
      DATA idpro/113 , 223 , 333 , 92/
      DATA vmesa/'vmeson  ' , 'rho     ' , 'omega   ' , 'phi     ' , 
     &     'pi+pi-  '/
      DATA vmesb/'vmeson  ' , 'rho     ' , 'omega   ' , 'phi     ' , 
     &     'pi+pi-  '/
 
C  sampling of elastic/quasi-elastic processes
      IF ( (Iproc.EQ.2) .OR. (Iproc.EQ.3) ) THEN
         Irej = 0
         NPOsd(1) = Jm1
         NPOsd(2) = Jm2
         DO i = 1 , 2
            pmi(i) = PHEp(5,NPOsd(i))
            IF ( pmi(i).LT.0.1D0 ) pmi(i) = 0.765D0
         END DO
C  get CM system
         pk1(1) = PHEp(1,Jm1) + PHEp(1,Jm2)
         pk1(2) = PHEp(2,Jm1) + PHEp(2,Jm2)
         pk1(3) = PHEp(3,Jm1) + PHEp(3,Jm2)
         pk1(4) = PHEp(4,Jm1) + PHEp(4,Jm2)
         ss = (pk1(4)+pk1(3))*(pk1(4)-pk1(3)) - pk1(1)**2 - pk1(2)**2
         ECMd = SQRT(ss)
 
         IF ( ECMd.LE.pmi(1)+pmi(2) ) THEN
            IF ( LPRi.GT.4 .AND. IDEb(34).GE.3 )
     &            WRITE (LO,'(1X,A,I12,3E12.4)')
     &            'PHO_QELAST: too small mass (EV,ECM,M1,M2)' , KEVent , 
     &           ECMd , pmi
            Irej = 5
            RETURN
         END IF
 
         DO i = 1 , 4
            GAMbed(i) = pk1(i)/ECMd
         END DO
         CALL PHO_ALTRA(GAMbed(4),-GAMbed(1),-GAMbed(2),-GAMbed(3),
     &                  PHEp(1,NPOsd(1)),PHEp(2,NPOsd(1)),
     &                  PHEp(3,NPOsd(1)),PHEp(4,NPOsd(1)),ptot1,pk1(1),
     &                  pk1(2),pk1(3),pk1(4))
C  rotation angles
         CODd = pk1(3)/ptot1
         SIDd = SQRT(pk1(1)**2+pk1(2)**2)/ptot1
         COFd = 1.D0
         SIFd = 0.D0
         IF ( ptot1*SIDd.GT.1.D-5 ) THEN
            COFd = pk1(1)/(SIDd*ptot1)
            SIFd = pk1(2)/(SIDd*ptot1)
            anorf = SQRT(COFd*COFd+SIFd*SIFd)
            COFd = COFd/anorf
            SIFd = SIFd/anorf
         END IF
C  get CM momentum
         am12 = pmi(1)**2
         am22 = pmi(2)**2
         PCMd = PHO_XLAM(ss,am12,am22)/(2.D0*ECMd)
 
C  production process of mother particles
         igen = IPHist(2,NPOsd(1))
         IF ( igen.EQ.0 ) igen = Iproc
 
         icall = icall + 1
C  main rejection label
C  determine process and final particles
 50      ifl(1) = IDHep(NPOsd(1))
         ifl(2) = IDHep(NPOsd(2))
         IF ( Iproc.EQ.3 ) THEN
            itry = 0
 60         itry = itry + 1
            IF ( itry.GT.50 ) THEN
               IF ( LPRi.GT.4 .AND. IDEb(34).GE.3 )
     &               WRITE (LO,'(1X,A,I12,I5,E12.4)')
     &               'PHO_QELAST: mass rejection (EV,ITRY,ECM)' , 
     &              KEVent , itry , ECMd
               Irej = 5
               RETURN
            END IF
            xi = DT_RNDM(PCMd)*SIGvm(0,0) - DEPS
            DO i = 1 , 4
               DO j = 1 , 4
                  xi = xi - SIGvm(i,j)
                  IF ( xi.LE.0.D0 ) GOTO 80
               END DO
            END DO
 80         IF ( ifl(1).EQ.22 ) ifl(1) = idpro(i)
            IF ( ifl(2).EQ.22 ) ifl(2) = idpro(j)
            isamvm(i,j) = isamvm(i,j) + 1
            isamqe = isamqe + 1
C  sample new masses
            CALL PHO_SAMASS(ifl(1),rmass(1))
            CALL PHO_SAMASS(ifl(2),rmass(2))
            IF ( rmass(1)+rmass(2).GE.ECMd ) GOTO 60
         ELSE IF ( Iproc.EQ.2 ) THEN
            i = 0
            j = 0
            isamel = isamel + 1
            rmass(1) = PHO_PMASS(NPOsd(1),2)
            rmass(2) = PHO_PMASS(NPOsd(2),2)
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I6)')
     &            'PHO_QELAST:ERROR: invalid IPROC' , Iproc
            CALL PHO_ABORT
         END IF
C  sample momentum transfer
         CALL PHO_DIFSLP(0,0,i,j,rmass(1),rmass(2),rmass(1),tt,slwght,
     &                   Irej)
C  calculate new momenta
         IF ( LPRi.GT.4 .AND. IDEb(34).GE.5 )
     &         WRITE (LO,'(1X,A,2I6,I3,3E11.3)')
     &         'PHO_QELAST: IF1,2,T,RM1,RM2' , ifl , Iproc , tt , rmass
         CALL PHO_DIFKIN(rmass(1),rmass(2),tt,pk1,pk2,Irej)
         IF ( Irej.NE.0 ) GOTO 50
         DO k = 1 , 4
            p(k,1) = pk1(k)
            p(k,2) = pk2(k)
         END DO
C  comment line for elastic/quasi-elastic scattering
         CALL PHO_REGPAR(35,Iproc,0,NPOsd(1),NPOsd(2),rmass(1),rmass(2),
     &                   tt,ECMd,ifl(1),ifl(2),IDHep(NPOsd(1)),
     &                   IDHep(NPOsd(2)),icpos,1)
 
         i1 = NHEp + 1
C  fill /POEVT1/
         DO i = 1 , 2
            k = 3 - i
            IF ( (ifl(i).EQ.92) .OR. (ifl(i).EQ.91) ) THEN
C  pi+/pi- isotropic background
               igen = 3
               CALL PHO_REGPAR(1,113,0,NPOsd(i),NPOsd(k),p(1,i),p(2,i),
     &            p(3,i),p(4,i),0,igen,0,0,ipos,1)
               ICOlor(i,icpos) = ipos
               CALL PHO_SDECAY(ipos,0,-2)
            ELSE
C  registration
               igen = 2
               IF ( ifl(i).NE.IDHep(NPOsd(i)) ) igen = 3
               CALL PHO_REGPAR(1,ifl(i),0,NPOsd(i),NPOsd(k),p(1,i),
     &            p(2,i),p(3,i),p(4,i),0,igen,0,0,ipos,1)
               ICOlor(i,icpos) = ipos
            END IF
         END DO
         i2 = NHEp
C  search for vector mesons
         DO i = i1 , i2
C  decay according to polarization
            IF ( (IDHep(JMOhep(1,i)).EQ.22) .AND. (ISWmdl(21).GE.0) )
     &           THEN
               isp = IPAmdl(3)
               IF ( ISWmdl(21).GE.1 ) isp = IPAmdl(4)
               CALL PHO_SDECAY(i,isp,2)
            END IF
         END DO
         i2 = NHEp
C  back transformation
         CALL PHO_LTRHEP(i1,i2,CODd,SIDd,COFd,SIFd,GAMbed(4),GAMbed(1),
     &                   GAMbed(2),GAMbed(3))
 
C  initialization of tables
      ELSE IF ( Iproc.EQ.-1 ) THEN
         DO i = 1 , 4
            DO j = 1 , 4
               isamvm(i,j) = 0
            END DO
         END DO
         isamel = 0
         isamqe = 0
         IF ( IFPap(1).NE.22 ) vmesa(1) = PHO_PNAME(IFPap(1),1)
         IF ( IFPap(2).NE.22 ) vmesb(1) = PHO_PNAME(IFPap(2),1)
         CALL PHO_SAMASS(-1,rmass(1))
         icall = 0
 
C  output of statistics
      ELSE IF ( Iproc.EQ.-2 ) THEN
         IF ( icall.LT.10 ) RETURN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,I10/,1X,A)')
     &         'PHO_QELAST: statistics of (quasi-)elastic processes' , 
     &        icall , 
     &        '---------------------------------------------------'
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I10)')
     &         'sampled elastic processes:' , isamel
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I10)')
     &         'sampled quasi-elastic vectormeson production:' , isamqe
         IF ( LPRi.GT.4 ) WRITE (LO,'(15X,4(4X,A))') (vmesb(i),i=1,4)
         DO i = 1 , 4
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,4I12)') vmesa(i) , 
     &           (isamvm(i,k),k=1,4)
         END DO
         CALL PHO_SAMASS(-2,rmass(1))
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,I3)')
     &        'PHO_QELAST:ERROR: ' , 'unknown process ID' , Iproc
         CALL PHO_ABORT
      END IF
 
      END SUBROUTINE
