
      SUBROUTINE PHO_MASSAD(Iflmo,Ifl1,Ifl2,Pmass,Xmcon,Xmout,Idpdg,
     &                      Idcpc)
C***********************************************************************
C
C    fine-correction of low mass strings to mass of corresponding
C    resonance or two particle threshold
C
C    input:     IFLMO         PDG ID of mother particle
C               IFL1,2        requested parton flavours
C                             (not used at the moment)
C               PMASS         reference mass (mass of mother particle)
C               XMCON         conjecture of mass
C
C    output:    XMOUT         output mass (adjusted input mass)
C                             moved ot nearest mass possible
C               IDPDG         PDG resonance ID
C               IDcpc         CPC resonance ID
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ahi , alo , amdcy , amsum , DEPS , dm2 , 
     &                 DT_RNDM , gares , Pmass , pmassl , rga , rma , 
     &                 rprob , rwg , xi , Xmcon , xminp , xminp2 , 
     &                 Xmout , xmres
      DOUBLE PRECISION xmres2 , xwg , xwgsum
      INTEGER i , id , Idcpc , Idpdg , Ifl1 , Ifl2 , Iflmo , ik , 
     &        irbam , irpdg , iter , listl
      SAVE 
 
      PARAMETER (DEPS=1.D-8)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  general particle data
      INCLUDE 'inc/popar2'
C  particle decay data
      INCLUDE 'inc/popar3'
 
      DIMENSION xwg(10) , rma(10) , rga(10) , rwg(10) , irpdg(10) , 
     &          irbam(10)
 
      xminp = Xmcon
      Idpdg = 0
      Idcpc = 0
      Xmout = xminp
 
C  resonance treatment activated?
      IF ( ISWmdl(23).EQ.0 ) RETURN
 
      CALL PHO_DIFRES(Iflmo,Ifl1,Ifl2,irpdg,irbam,rma,rga,rwg,listl)
      IF ( listl.LT.1 ) THEN
         IF ( LPRi.GT.4 .AND. IDEb(7).GE.2 ) WRITE (LO,'(1X,A,3I7)')
     &         'PHO_MASSAD: no resonances for (IFMO,IF1,IF2)' , Iflmo , 
     &        Ifl1 , Ifl2
         GOTO 200
      END IF
C  mass small?
      pmassl = (Pmass+0.15D0)**2
      xminp2 = xminp**2
C  determine resonance probability
      dm2 = 1.1D0
      rprob = (pmassl+dm2)*(xminp2-pmassl)/(dm2*xminp2)
      IF ( rprob.LT.DT_RNDM(pmassl) ) THEN
C  sample new resonance
         xwgsum = 0.D0
         DO i = 1 , listl
            xwg(i) = rwg(i)/rma(i)**2
            xwgsum = xwgsum + xwg(i)
         END DO
 
         iter = 0
 50      iter = iter + 1
         IF ( iter.GE.5 ) THEN
            Idcpc = 0
            Idpdg = 0
            Xmout = xminp
            GOTO 200
         END IF
 
         i = 0
         xi = xwgsum*DT_RNDM(Xmout)
 100     i = i + 1
         xwgsum = xwgsum - xwg(i)
         IF ( (xi.LT.xwgsum) .AND. (i.LT.listl) ) GOTO 100
         Idpdg = irpdg(i)
         Idcpc = irbam(i)
         gares = rga(i)
         xmres = rma(i)
         xmres2 = xmres**2
C  sample new mass (from Breit-Wigner cross section)
         alo = ATAN((pmassl-xmres2)/(xmres*gares))
         ahi = ATAN((5.D0-xmres2)/(xmres*gares))
         xi = (ahi-alo)*DT_RNDM(xmres) + alo
         Xmout = xmres*gares*TAN(xi) + xmres2
         Xmout = SQRT(Xmout)
 
C  check mass for decay
         amdcy = 2.D0*xmres
         id = ABS(Idcpc)
         DO ik = IDEc_list(2,id) , IDEc_list(3,id)
            amsum = 0.D0
            DO i = 1 , 3
               IF ( ISEc_list(i,ik).NE.0 ) amsum = amsum + 
     &              XM_list(ABS(ISEc_list(i,ik)))
            END DO
            amdcy = MIN(amdcy,amsum)
         END DO
         IF ( amdcy.GE.Xmout ) GOTO 50
 
C  debug output
         IF ( IDEb(7).GE.10 .AND. LPRi.GT.4 )
     &         WRITE (LO,'(1X,2A,/1X,3I6,2E10.3,2I7,2E10.3)')
     &         'PHO_MASSAD: ' , 
     &        'IFMO,IF1,IF2,XMCON,XMOUT,IDPDG,IDcpc,RMA,RGA' , Iflmo , 
     &        Ifl1 , Ifl2 , Xmcon , Xmout , Idpdg , Idcpc , rma(i) , 
     &        rga(i)
         RETURN
      END IF
 
C  debug output
 
 200  IF ( IDEb(7).GE.15 .AND. LPRi.GT.4 )
     &      WRITE (LO,'(1X,A,/1X,3I6,2E10.3)')
     &      'PHO_MASSAD: string sampled: IFMO,IF1,IF2,XMCON,XMOUT' , 
     &     Iflmo , Ifl1 , Ifl2 , Xmcon , Xmout
      END SUBROUTINE
