
      SUBROUTINE PHO_DSIGDM(Rma,Xma,Nstep)
C**********************************************************************
C
C     differential cross section DSIG/DM of low mass enhancement
C
C     input:   RMA(4,NTABM)   mass values
C     output:  XMA(4,NTABM)   DSIG/DM of resonances
C                  1          rho production
C                  2          omega production
C                  3          phi production
C                  4          pi-pi continuum
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION EPS , gamma , pimass , qq , qres , Rma , Xma , 
     &                 xmass
      INTEGER i , k , Nstep , NTABM
      SAVE 
 
      PARAMETER (EPS=1.D-10)
 
      PARAMETER (NTABM=50)
      DIMENSION Xma(4,NTABM) , Rma(4,NTABM)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  parameters of the "simple" Vector Dominance Model
      INCLUDE 'inc/posvdm'
 
      pimass = 0.135
C  rho meson shape (mass dependent width)
      qres = SQRT(VMAs(1)**2-4.D0*pimass**2)
      DO i = 1 , Nstep
         xmass = Rma(1,i)
         qq = SQRT(xmass**2-4.D0*pimass**2)
         gamma = GAMm(1)*(qq/qres)**3
         Xma(1,i) = xmass*gamma*(VMAs(1)/xmass)**PARmdl(170)
     &              /((VMAs(1)**2-xmass**2)**2+VMAs(1)**2*gamma**2)
      END DO
C  omega/phi meson (constant width)
      DO k = 2 , 3
         DO i = 1 , Nstep
            xmass = Rma(k,i)
            Xma(k,i) = xmass*GAMm(k)
     &                 /((VMAs(k)**2-xmass**2)**2+VMAs(k)**2*GAMm(k)**2)
         END DO
      END DO
C  pi-pi continuum
      DO i = 1 , Nstep
         xmass = Rma(4,i)
         Xma(4,i) = (xmass-0.29D0)**2/xmass
      END DO
 
      END SUBROUTINE
