
      SUBROUTINE DT_PROFBI(Na,Nb,Ppn,Ntarg)
 
C***********************************************************************
C Integral over profile function (to be used for impact-parameter      *
C sampling during event generation).                                   *
C Fitted results are used.                                             *
C         NA / NB    mass numbers of proj./target nuclei               *
C         PPN        projectile momentum (for projectile nuclei:       *
C                    momentum per nucleon) in target rest system       *
C         NTARG      index of target material (i.e. kind of nucleus)   *
C This version dated 31.05.95 is revised by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION glafit , glappn , glasig , ONE , poly , Ppn , 
     &                 THREE , TWO , xx , ZERO
      INTEGER i , idxgla , ipoint , j , j1 , j1beg , j1end , k , Na , 
     &        natmp , Nb , nglip , nglit , NGLMAX , nglpar , nna , nnb , 
     &        Ntarg
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      SAVE 
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,THREE=3.0D0)
 
      LOGICAL lstart
      CHARACTER cname*80
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: parameters
      INCLUDE 'inc/dtglam'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
 
      PARAMETER (NGLMAX=8000)
      DIMENSION nglit(NGLMAX) , nglip(NGLMAX) , glappn(NGLMAX) , 
     &          glasig(NGLMAX) , glafit(5,NGLMAX)
 
      DATA lstart/.TRUE./
 
      IF ( lstart ) THEN
C read fit-parameters from file
         OPEN (47,FILE='inpdata/glpara.dat',STATUS='UNKNOWN')
         i = 0
 50      READ (47,'(A80)') cname
         IF ( cname.EQ.'STOP' ) THEN
            nglpar = i
            lstart = .FALSE.
         ELSE
            i = i + 1
            READ (cname,*) nglip(i) , nglit(i) , glappn(i) , glasig(i) , 
     &                     glafit(1,i) , glafit(2,i) , glafit(3,i) , 
     &                     glafit(4,i) , glafit(5,i)
            IF ( i+1.GT.NGLMAX ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010          FORMAT (1X,'PROFBI:    warning! array size exceeded - ',
     &                 'program stopped')
               STOP
            END IF
            GOTO 50
         END IF
      END IF
 
      nna = Na
      nnb = Nb
      IF ( Na.GT.Nb ) THEN
         nna = Nb
         nnb = Na
      END IF
      idxgla = 0
      DO j = 1 , nglpar
         IF ( (nnb.LT.nglit(j)) .OR. (j.EQ.nglpar) ) THEN
            IF ( nnb.NE.nglit(j-1) ) nnb = nglit(j-1)
            DO k = 1 , j - 1
               ipoint = j - k
               IF ( j.EQ.nglpar ) ipoint = j + 1 - k
               IF ( (nna.GT.nglip(ipoint)) .OR. (nnb.NE.nglit(ipoint))
     &              .OR. (ipoint.EQ.1) ) THEN
                  IF ( ipoint.EQ.1 ) ipoint = 0
                  natmp = nglip(ipoint+1)
                  IF ( Ppn.LT.glappn(ipoint+1) ) THEN
                     idxgla = ipoint + 1
                     GOTO 100
                  ELSE
                     j1beg = ipoint + 1
                     j1end = j
C                    IF (J.EQ.NGLPAR) THEN
C                       J1BEG = IPOINT
C                       J1END = J
C                    ENDIF
                     DO j1 = j1beg , j1end
                        IF ( nglip(j1).NE.natmp ) THEN
                           idxgla = j1 - 1
                           GOTO 100
                        ELSE IF ( Ppn.LT.glappn(j1) ) THEN
                           idxgla = j1
                           GOTO 100
                        END IF
                     END DO
                     IF ( (j.EQ.nglpar) .AND. (Ppn.GT.glappn(nglpar)) )
     &                    idxgla = nglpar
                  END IF
               END IF
            END DO
         END IF
      END DO
 
 100  IF ( idxgla.EQ.0 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99020) nna , nnb , Ppn
99020    FORMAT (1X,'PROFBI:   configuration (NA,NB,PPN = ',2I4,F6.0,
     &           ') not found ')
         STOP
      END IF
 
C no interpolation yet available
      XSPro(1,1,Ntarg) = glasig(idxgla)
 
      BSIte(1,1,Ntarg,1) = ZERO
      DO i = 2 , NSIteb
         xx = DBLE(i)
         poly = glafit(1,idxgla) + glafit(2,idxgla)
     &          *xx + glafit(3,idxgla)*xx**2 + glafit(4,idxgla)
     &          *xx**3 + glafit(5,idxgla)*xx**4
         IF ( ABS(poly).GT.35.0D0 ) poly = SIGN(35.0D0,poly)
         BSIte(1,1,Ntarg,i) = (1.0D0-EXP(-poly))
         IF ( BSIte(1,1,Ntarg,i).LT.ZERO ) BSIte(1,1,Ntarg,i) = ZERO
      END DO
 
      END SUBROUTINE
