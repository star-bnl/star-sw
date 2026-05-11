
      SUBROUTINE PHO_HARCOL(Mspr,V,Ip1,Ica1,Ica2,Ip2,Icb1,Icb2,Ip3,Icc1,
     &                      Icc2,Ip4,Icd1,Icd2)
C*********************************************************************
C
C     calculate color flow for hard resolved process
C
C     input:    IP1..4  flavour of partons (PDG convention)
C               V       parton subprocess Mandelstam variable  V = t/s
C                       (lightcone momenta assumed)
C               ICA,ICB color labels
C               MSPR    process number
C                       -1   initialization of statistics
C                       -2   output of statistics
C
C     output:   ICC,ICD color label of final partons
C
C     (it is possible to use the same variables for in and output)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , pc , pcs , u , V , xi
      INTEGER i , i1 , i2 , ic1 , ic2 , ic3 , ic4 , Ica1 , Ica2 , Icb1 , 
     &        Icb2 , Icc1 , Icc2 , Icd1 , Icd2 , iconf , Ip1 , Ip2 , 
     &        Ip3 , Ip4
      INTEGER irc , irecn , k , k1 , k2 , Mspr
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  names of hard scattering processes
      INCLUDE 'inc/pohpro'
 
      DIMENSION pc(3) , iconf(8,5) , irecn(8,2)
 
C  initialization
      IF ( Mspr.EQ.-1 ) THEN
         DO i = 1 , 8
            DO k = 1 , 5
               iconf(i,k) = 0
            END DO
            irecn(i,1) = 0
            irecn(i,2) = 0
         END DO
         RETURN
C  output of statistics
      ELSE IF ( Mspr.EQ.-2 ) THEN
         IF ( IDEb(26).LT.1 ) RETURN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A)')
     &         'PHO_HARCOL: sampled color configurations' , 
     &        '----------------------------------------'
         IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,15X,A)')
     &         'diagram                  color configurations (1-4)' , 
     &        'sum'
         DO i = 1 , 8
            DO k = 1 , 4
               iconf(i,5) = iconf(i,5) + iconf(i,k)
            END DO
            IF ( LPRi.GT.4 ) WRITE (LO,'(2X,A,4I11,I12)') PROc(i) , 
     &           (iconf(i,k),k=1,5)
         END DO
         IF ( ISWmdl(11).GE.2 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/6X,A)') 
     &      'diagram             with   /   without color re-connection'
            DO i = 1 , 8
               IF ( LPRi.GT.4 ) WRITE (LO,'(2X,A,2I11)') PROc(i) , 
     &              irecn(i,1) , irecn(i,2)
            END DO
         END IF
         RETURN
      END IF
C
C  gluons: first color positive, quarks second color zero
      IF ( Ip1.NE.0 ) THEN
         Ica2 = 0
      ELSE IF ( Ica1.LT.0 ) THEN
         i = Ica2
         Ica2 = Ica1
         Ica1 = i
      END IF
      IF ( Ip2.NE.0 ) THEN
         Icb2 = 0
      ELSE IF ( Icb1.LT.0 ) THEN
         i = Icb2
         Icb2 = Icb1
         Icb1 = i
      END IF
      ic2 = 0
      ic4 = 0
C  debug output
      IF ( IDEb(26).GE.15 .AND. LPRi.GT.4 )
     &      WRITE (LO,'(1X,A,I4,/,5X,A,3I5,2X,3I5)')
     &      'PHO_HARCOL: process' , Mspr , 
     &     'initial partons and colors' , Ip1 , Ica1 , Ica2 , Ip2 , 
     &     Icb1 , Icb2
C
      irc = 0
      IF ( IPAmdl(21).EQ.1 ) THEN
C
C  soft color re-connection option
C
         IF ( Mspr.EQ.1 ) THEN
C  hard g g final state, only g g --> g g
            IF ( (Ica1.NE.-Ica2) .AND. (Icb1.NE.-Icb2) ) THEN
               IF ( DT_RNDM(V).LT.PARmdl(140) ) THEN
                  ic1 = Ica1
                  ic2 = Ica2
                  ic3 = Icb1
                  ic4 = Icb2
                  irecn(Mspr,1) = irecn(Mspr,1) + 1
                  irc = 1
                  GOTO 100
               END IF
            END IF
         ELSE IF ( Mspr.EQ.3 ) THEN
C  hard q g final state
            IF ( (Ica1.NE.-Ica2) .AND. (Icb1.NE.-Icb2) ) THEN
               IF ( DT_RNDM(V).LT.PARmdl(141) ) THEN
                  ic1 = Ica1
                  ic2 = Ica2
                  ic3 = Icb1
                  ic4 = Icb2
                  irecn(Mspr,1) = irecn(Mspr,1) + 1
                  irc = 1
                  GOTO 100
               END IF
            END IF
         ELSE IF ( (Mspr.EQ.5) .OR. (Mspr.EQ.7) .OR. (Mspr.EQ.8) ) THEN
C  hard q q final state
            IF ( Ica1.NE.-Icb1 ) THEN
               IF ( DT_RNDM(V).LT.PARmdl(142) ) THEN
                  ic1 = Ica1
                  ic2 = Ica2
                  ic3 = Icb1
                  ic4 = Icb2
                  irecn(Mspr,1) = irecn(Mspr,1) + 1
                  irc = 1
                  GOTO 100
               END IF
            END IF
         END IF
         irecn(Mspr,2) = irecn(Mspr,2) + 1
      END IF
C
      IF ( (ISWmdl(11).NE.1) .OR. (Mspr.GE.10) ) THEN
C
C  color flow according to QCD leading order matrix element
C
         u = -(1.D0+V)
         IF ( Mspr.EQ.1 ) THEN
C  g g --> g g
            pc(1) = 1/V**2 + 2.D0/V + 3.D0 + 2.D0*V + V**2
            pc(2) = 1/u**2 + 2.D0/u + 3.D0 + 2.D0*u + u**2
            pc(3) = (V/u)**2 + 2.D0*(V/u) + 3.D0 + 2.D0*(u/V) + (u/V)**2
            xi = (pc(1)+pc(2)+pc(3))*DT_RNDM(u)
            pcs = 0.D0
            DO i = 1 , 3
               pcs = pcs + pc(i)
               IF ( xi.LT.pcs ) GOTO 20
            END DO
 20         IF ( i.EQ.1 ) THEN
               CALL PHO_SELCOL(0,0,i1,k1,i2,k2,1)
               IF ( DT_RNDM(V).GT.0.5D0 ) THEN
                  ic1 = i1
                  ic2 = Ica2
                  ic3 = Icb1
                  ic4 = i2
                  CALL PHO_HARCOR(-Icb2,Ica1)
                  IF ( Icb1.EQ.-Icb2 ) ic3 = Ica1
               ELSE
                  ic1 = Ica1
                  ic2 = i2
                  ic3 = i1
                  ic4 = Icb2
                  CALL PHO_HARCOR(-Icb1,Ica2)
                  IF ( Icb2.EQ.-Icb1 ) ic4 = Ica2
               END IF
            ELSE IF ( i.EQ.2 ) THEN
               CALL PHO_SELCOL(0,0,i1,k1,i2,k2,1)
               IF ( DT_RNDM(u).GT.0.5D0 ) THEN
                  ic1 = Icb1
                  ic2 = i2
                  ic3 = i1
                  ic4 = Ica2
                  CALL PHO_HARCOR(-Icb2,Ica1)
                  IF ( Icb1.EQ.-Icb2 ) ic1 = Ica1
               ELSE
                  ic1 = i1
                  ic2 = Icb2
                  ic3 = Ica1
                  ic4 = i2
                  CALL PHO_HARCOR(-Icb1,Ica2)
                  IF ( Icb2.EQ.-Icb1 ) ic2 = Ica2
               END IF
            ELSE IF ( DT_RNDM(V).GT.0.5D0 ) THEN
               ic1 = Icb1
               ic2 = Ica2
               ic3 = Ica1
               ic4 = Icb2
            ELSE
               ic1 = Ica1
               ic2 = Icb2
               ic3 = Icb1
               ic4 = Ica2
            END IF
            iconf(Mspr,i) = iconf(Mspr,i) + 1
         ELSE IF ( Mspr.EQ.2 ) THEN
C  q qb --> g g
            pc(1) = u/V - 2.D0*u**2
            pc(2) = V/u - 2.D0*V**2
            CALL PHO_SELCOL(0,0,i1,k1,i2,k2,1)
            xi = (pc(1)+pc(2))*DT_RNDM(u)
            IF ( xi.LT.pc(1) ) THEN
               IF ( Ica1.GT.0 ) THEN
                  ic1 = Ica1
                  ic2 = i2
                  ic3 = i1
                  ic4 = Icb1
                  iconf(Mspr,1) = iconf(Mspr,1) + 1
               ELSE
                  ic1 = i1
                  ic2 = Ica1
                  ic3 = Icb1
                  ic4 = i2
                  iconf(Mspr,2) = iconf(Mspr,2) + 1
               END IF
            ELSE IF ( Ica1.GT.0 ) THEN
               ic1 = i1
               ic2 = Icb1
               ic3 = Ica1
               ic4 = i2
               iconf(Mspr,3) = iconf(Mspr,3) + 1
            ELSE
               ic1 = Icb1
               ic2 = i2
               ic3 = i1
               ic4 = Ica1
               iconf(Mspr,4) = iconf(Mspr,4) + 1
            END IF
         ELSE IF ( Mspr.EQ.3 ) THEN
C  q g --> q g
            pc(1) = 2.D0*(u/V)**2 - u
            pc(2) = 2.D0/V**2 - 1.D0/u
            xi = (pc(1)+pc(2))*DT_RNDM(V)
            IF ( xi.LT.pc(1) ) THEN
               CALL PHO_SELCOL(0,0,i1,k1,i2,k2,1)
               IF ( Ip1.GT.0 ) THEN
                  ic1 = i1
                  ic3 = Icb1
                  ic4 = i2
                  CALL PHO_HARCOR(-Ica1,Icb2)
                  iconf(Mspr,1) = iconf(Mspr,1) + 1
               ELSE IF ( Ip1.LT.0 ) THEN
                  ic1 = i2
                  ic3 = i1
                  ic4 = Icb2
                  CALL PHO_HARCOR(-Ica1,Icb1)
                  iconf(Mspr,1) = iconf(Mspr,1) + 1
               ELSE IF ( Ip2.GT.0 ) THEN
                  ic1 = Ica1
                  ic2 = i2
                  ic3 = i1
                  CALL PHO_HARCOR(-Icb1,Ica2)
                  iconf(Mspr,2) = iconf(Mspr,2) + 1
               ELSE
                  ic1 = i1
                  ic2 = Ica2
                  ic3 = i2
                  CALL PHO_HARCOR(-Icb1,Ica1)
                  iconf(Mspr,2) = iconf(Mspr,2) + 1
               END IF
            ELSE IF ( Ip1.GT.0 ) THEN
               ic1 = Icb1
               ic3 = Ica1
               ic4 = Icb2
               iconf(Mspr,3) = iconf(Mspr,3) + 1
            ELSE IF ( Ip1.LT.0 ) THEN
               ic1 = Icb2
               ic3 = Icb1
               ic4 = Ica1
               iconf(Mspr,3) = iconf(Mspr,3) + 1
            ELSE IF ( Ip2.GT.0 ) THEN
               ic1 = Icb1
               ic2 = Ica2
               ic3 = Ica1
               iconf(Mspr,4) = iconf(Mspr,4) + 1
            ELSE
               ic1 = Ica1
               ic2 = Icb1
               ic3 = Ica2
               iconf(Mspr,4) = iconf(Mspr,4) + 1
            END IF
         ELSE IF ( Mspr.EQ.4 ) THEN
C  g g --> q qb
            pc(1) = u/V - 2.D0*u**2
            pc(2) = V/u - 2.D0*V**2
            xi = (pc(1)+pc(2))*DT_RNDM(u)
            IF ( xi.LT.pc(1) ) THEN
               IF ( Ip3.GT.0 ) THEN
                  ic1 = Ica1
                  ic3 = Icb2
                  CALL PHO_HARCOR(-Icb1,Ica2)
                  IF ( Icb2.EQ.-Icb1 ) ic3 = Ica2
                  iconf(Mspr,1) = iconf(Mspr,1) + 1
               ELSE
                  ic1 = Ica2
                  ic3 = Icb1
                  CALL PHO_HARCOR(-Icb2,Ica1)
                  IF ( Icb1.EQ.-Icb2 ) ic3 = Ica1
                  iconf(Mspr,2) = iconf(Mspr,2) + 1
               END IF
            ELSE IF ( Ip3.GT.0 ) THEN
               ic1 = Icb1
               ic3 = Ica2
               CALL PHO_HARCOR(-Icb2,Ica1)
               IF ( Icb1.EQ.-Icb2 ) ic1 = Ica1
               iconf(Mspr,3) = iconf(Mspr,3) + 1
            ELSE
               ic1 = Icb2
               ic3 = Ica1
               CALL PHO_HARCOR(-Icb1,Ica2)
               IF ( Icb2.EQ.-Icb1 ) ic1 = Ica2
               iconf(Mspr,4) = iconf(Mspr,4) + 1
            END IF
         ELSE IF ( Mspr.EQ.5 ) THEN
C  q qb --> q qb
            pc(1) = (1.D0+u**2)/V**2
            pc(2) = (V**2+u**2)
            xi = (pc(1)+pc(2))*DT_RNDM(V)
            IF ( xi.LT.pc(1) ) THEN
               CALL PHO_HARCOR(-Icb1,Ica1)
               CALL PHO_SELCOL(0,0,i1,k1,i2,k2,1)
               IF ( Ip3.GT.0 ) THEN
                  ic1 = i1
                  ic3 = i2
                  iconf(Mspr,1) = iconf(Mspr,1) + 1
               ELSE
                  ic1 = i2
                  ic3 = i1
                  iconf(Mspr,2) = iconf(Mspr,2) + 1
               END IF
            ELSE IF ( Ip3.GT.0 ) THEN
               ic1 = MAX(Ica1,Icb1)
               ic3 = MIN(Ica1,Icb1)
               iconf(Mspr,3) = iconf(Mspr,3) + 1
            ELSE
               ic1 = MIN(Ica1,Icb1)
               ic3 = MAX(Ica1,Icb1)
               iconf(Mspr,4) = iconf(Mspr,4) + 1
            END IF
         ELSE IF ( Mspr.EQ.6 ) THEN
C  q qb --> qp qpb
            IF ( Ip3.GT.0 ) THEN
               ic1 = MAX(Ica1,Icb1)
               ic3 = MIN(Ica1,Icb1)
               iconf(Mspr,1) = iconf(Mspr,1) + 1
            ELSE
               ic1 = MIN(Ica1,Icb1)
               ic3 = MAX(Ica1,Icb1)
               iconf(Mspr,2) = iconf(Mspr,2) + 1
            END IF
         ELSE IF ( Mspr.EQ.7 ) THEN
C  q q --> q q
            pc(1) = (1.D0+u**2)/V**2
            pc(2) = (1.D0+V**2)/u**2
            xi = (pc(1)+pc(2))*DT_RNDM(u)
            IF ( xi.LT.pc(1) ) THEN
               ic1 = Icb1
               ic3 = Ica1
               iconf(Mspr,1) = iconf(Mspr,1) + 1
            ELSE
               ic1 = Ica1
               ic3 = Icb1
               iconf(Mspr,2) = iconf(Mspr,2) + 1
            END IF
         ELSE IF ( Mspr.EQ.8 ) THEN
C  q qp --> q qp
            IF ( Ip1*Ip2.LT.0 ) THEN
               CALL PHO_HARCOR(-Icb1,Ica1)
               CALL PHO_SELCOL(0,0,i1,k1,i2,k2,1)
               IF ( Ip1.GT.0 ) THEN
                  ic1 = i1
                  ic3 = i2
                  iconf(Mspr,1) = iconf(Mspr,1) + 1
               ELSE
                  ic1 = i2
                  ic3 = i1
                  iconf(Mspr,2) = iconf(Mspr,2) + 1
               END IF
            ELSE
               ic1 = Icb1
               ic3 = Ica1
               iconf(Mspr,3) = iconf(Mspr,3) + 1
            END IF
 
         ELSE IF ( Mspr.EQ.10 ) THEN
C  gam q --> q g
            CALL PHO_SELCOL(Icb1,Icb2,ic1,ic2,ic3,ic4,2)
            IF ( Ip3.EQ.0 ) THEN
               CALL PHO_SWAPI(ic1,ic3)
               CALL PHO_SWAPI(ic2,ic4)
            END IF
         ELSE IF ( Mspr.EQ.11 ) THEN
C  gam g --> q q
            ic1 = Icb1
            ic3 = Icb2
            IF ( Ip3.LT.0 ) CALL PHO_SWAPI(ic1,ic3)
         ELSE IF ( Mspr.EQ.12 ) THEN
C  q gam --> q g
            CALL PHO_SELCOL(Ica1,Ica2,ic1,ic2,ic3,ic4,2)
            IF ( Ip3.EQ.0 ) THEN
               CALL PHO_SWAPI(ic1,ic3)
               CALL PHO_SWAPI(ic2,ic4)
            END IF
         ELSE IF ( Mspr.EQ.13 ) THEN
C  g gam --> q q
            ic1 = Ica1
            ic3 = Ica2
            IF ( Ip3.LT.0 ) CALL PHO_SWAPI(ic1,ic3)
         ELSE IF ( Mspr.EQ.14 ) THEN
            IF ( ABS(Ip3).GT.12 ) THEN
               ic1 = 0
               ic3 = 0
            ELSE
               CALL PHO_SELCOL(Ica1,Ica2,ic1,ic2,ic3,ic4,1)
               IF ( Ip3.LT.0 ) CALL PHO_SWAPI(ic1,ic3)
            END IF
         ELSE
C  unknown process
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I3)')
     &            'PHO_HARCOL:ERROR:invalid process number' , Mspr
            CALL PHO_ABORT
         END IF
C
C  large Nc limit of all graphs
C
      ELSE IF ( Mspr.EQ.1 ) THEN
C  g g --> g g
         IF ( DT_RNDM(V).GT.0.5D0 ) THEN
            ic1 = Icb1
            ic2 = Ica2
            ic3 = Ica1
            ic4 = Icb2
            iconf(Mspr,1) = iconf(Mspr,1) + 1
         ELSE
            ic1 = Ica1
            ic2 = Icb2
            ic3 = Icb1
            ic4 = Ica2
            iconf(Mspr,2) = iconf(Mspr,2) + 1
         END IF
      ELSE IF ( Mspr.EQ.2 ) THEN
C  q qb --> g g
         CALL PHO_SELCOL(0,0,i1,k1,i2,k2,1)
         IF ( Ica1.LT.0 ) THEN
            ic1 = i1
            ic2 = Ica1
            ic3 = Icb1
            ic4 = i2
            iconf(Mspr,2) = iconf(Mspr,2) + 1
         ELSE
            ic1 = Ica1
            ic2 = i2
            ic3 = i1
            ic4 = Icb1
            iconf(Mspr,1) = iconf(Mspr,1) + 1
         END IF
      ELSE IF ( Mspr.EQ.3 ) THEN
C  q g --> q g
         IF ( DT_RNDM(V).LT.0.5D0 ) THEN
            IF ( Ip1+Ip2.GT.0 ) THEN
               ic1 = Icb1
               ic2 = Ica2
               ic3 = Ica1
               ic4 = Icb2
            ELSE IF ( Ip1.LT.0 ) THEN
               ic1 = Icb2
               ic3 = Icb1
               ic4 = Ica1
            ELSE
               ic1 = Ica1
               ic2 = Icb1
               ic3 = Ica2
            END IF
            iconf(Mspr,1) = iconf(Mspr,1) + 1
         ELSE
            IF ( Ip1.GT.0 ) THEN
               CALL PHO_HARCOR(-Ica1,Icb2)
               ic1 = Ica1
               ic3 = Icb1
               ic4 = -Ica1
            ELSE IF ( Ip2.GT.0 ) THEN
               CALL PHO_HARCOR(-Icb1,Ica2)
               ic1 = Ica1
               ic2 = -Icb1
               ic3 = Icb1
            ELSE IF ( Ip1.LT.0 ) THEN
               CALL PHO_HARCOR(-Ica1,Icb1)
               ic1 = Ica1
               ic3 = -Ica1
               ic4 = Icb2
            ELSE IF ( Ip2.LT.0 ) THEN
               CALL PHO_HARCOR(-Icb1,Ica1)
               ic1 = -Icb1
               ic2 = Ica2
               ic3 = Icb1
            END IF
            iconf(Mspr,2) = iconf(Mspr,2) + 1
         END IF
      ELSE IF ( Mspr.EQ.4 ) THEN
C  g g --> q qb
         ic1 = Ica1
         ic3 = Icb2
         CALL PHO_HARCOR(-Icb1,Ica2)
         IF ( Icb2.EQ.-Icb1 ) ic3 = Ica2
         IF ( Ip3*ic1.LT.0 ) THEN
            i = ic1
            ic1 = ic3
            ic3 = i
         END IF
         iconf(Mspr,2) = iconf(Mspr,2) + 1
      ELSE IF ( Mspr.EQ.5 ) THEN
C  q qb --> q qb
         IF ( DT_RNDM(V).LT.0.5D0 ) THEN
            IF ( Ica1*Ip3.LT.0 ) THEN
               ic1 = Icb1
               ic3 = Ica1
            ELSE
               ic1 = Ica1
               ic3 = Icb1
            END IF
            iconf(Mspr,1) = iconf(Mspr,1) + 1
         ELSE
            IF ( Ica1*Ip3.LT.0 ) THEN
               ic1 = -Ica1
               ic3 = Ica1
            ELSE
               ic1 = Ica1
               ic3 = -Ica1
            END IF
            CALL PHO_HARCOR(-Ica1,Icb1)
            iconf(Mspr,2) = iconf(Mspr,2) + 1
         END IF
      ELSE IF ( Mspr.EQ.6 ) THEN
C  q qb --> qp qbp
         IF ( Ica1*Ip3.LT.0 ) THEN
            ic1 = Icb1
            ic3 = Ica1
            iconf(Mspr,1) = iconf(Mspr,1) + 1
         ELSE
            ic1 = Ica1
            ic3 = Icb1
            iconf(Mspr,2) = iconf(Mspr,2) + 1
         END IF
      ELSE IF ( Mspr.EQ.7 ) THEN
C  q q --> q q
         IF ( DT_RNDM(V).LT.0.5D0 ) THEN
            ic1 = Ica1
            ic3 = Icb1
            iconf(Mspr,1) = iconf(Mspr,1) + 1
         ELSE
            ic1 = Icb1
            ic3 = Ica1
            iconf(Mspr,2) = iconf(Mspr,2) + 1
         END IF
      ELSE IF ( Mspr.EQ.8 ) THEN
C  q qp --> q qp
         IF ( Ip1*Ip2.GT.0 ) THEN
            IF ( Ip3.EQ.Ip1 ) THEN
               ic1 = Icb1
               ic3 = Ica1
            ELSE
               ic1 = Ica1
               ic3 = Icb1
            END IF
            iconf(Mspr,1) = iconf(Mspr,1) + 1
         ELSE
            IF ( Ica1*Ip3.LT.0 ) THEN
               ic1 = -Ica1
               ic3 = Ica1
            ELSE
               ic1 = Ica1
               ic3 = -Ica1
            END IF
            CALL PHO_HARCOR(-Ica1,Icb1)
            iconf(Mspr,2) = iconf(Mspr,2) + 1
         END IF
      ELSE
C  unknown process
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I3)')
     &         'PHO_HARCOL:ERROR:invalid process number (MSPR)' , Mspr
         CALL PHO_ABORT
C
      END IF
C
C  debug output
 100  IF ( LPRi.GT.4 .AND. IDEb(26).GE.10 )
     &      WRITE (LO,'(5X,A,3I5,2X,3I5)') 'final partons and colors' , 
     &     Ip3 , ic1 , ic2 , Ip4 , ic3 , ic4
C  color connection?
C     IF(((IC1.NE.-IC3).AND.(IC1.NE.-IC4)).AND.
C    &  (((IC2.NE.0).AND.(IC2.NE.-IC3).AND.(IC2.NE.-IC4))
C    &  .OR.(IC2.EQ.0))) THEN
C  color exchange?
C       IF(((IP1.EQ.IP3).AND.(ICA1.EQ.IC1).AND.(ICA2.EQ.IC2))
C    &     .OR.((IP1.EQ.IP4).AND.(ICA1.EQ.IC3).AND.(ICA2.EQ.IC4))) THEN
C         IF(IRC.NE.1) THEN
C           WRITE(LO,'(1X,A,I10,I3)')
C    &        'PHO_HARCOL:unexp. re-connection (event/MSPR):',KEVENT,MSPR
C           WRITE(LO,'(5X,A,3I5,2X,3I5)')
C    &        'initial partons and colors',IP1,ICA1,ICA2,IP2,ICB1,ICB2
C           WRITE(LO,'(5X,A,3I5,2X,3I5)')
C    &        'final partons and colors  ',IP3,IC1,IC2,IP4,IC3,IC4
C         ENDIF
C         IRC = 0
C       ENDIF
C     ENDIF
C     IF(IRC.EQ.1) THEN
C           WRITE(LO,'(1X,A,I10,I3)')
C    &        'PHO_HARCOL:re-conn. failed (event/MSPR):',KEVENT,MSPR
C           WRITE(LO,'(5X,A,3I5,2X,3I5)')
C    &        'initial partons and colors',IP1,ICA1,ICA2,IP2,ICB1,ICB2
C           WRITE(LO,'(5X,A,3I5,2X,3I5)')
C    &        'final partons and colors  ',IP3,IC1,IC2,IP4,IC3,IC4
C     ENDIF
C
      Icc1 = ic1
      Icc2 = ic2
      Icd1 = ic3
      Icd2 = ic4
 
      END SUBROUTINE
