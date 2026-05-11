
      SUBROUTINE DT_CKMT(X,Scale,Upv,Dnv,Usea,Dsea,Str,Chm,Bot,Top,Gl,
     &                   F2,Ipar)
 
C***********************************************************************
C This version dated 31.01.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION adq2 , Bot , Chm , Dnv , DQ2 , Dsea , F2 , f2p , 
     &                 f2pp , f2pq0 , f2pq1 , f2q0 , f2q1 , fx , Gl , 
     &                 glu , pd , Q02 , Q12 , q2
      DOUBLE PRECISION Scale , sea , smooth , Str , TINY10 , Top , TWO , 
     &                 Upv , Usea , val , X , ZERO
      INTEGER Ipar
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TWO=2.0D0,TINY10=1.0D-10)
 
      PARAMETER (Q02=2.0D0,DQ2=10.05D0,Q12=Q02+DQ2)
 
      DIMENSION pd(-6:6) , sea(3) , val(2)
 
      CALL DT_PDF0(Q02,X,f2q0,val,sea,glu,Ipar)
      CALL DT_PDF0(Q12,X,f2q1,val,sea,glu,Ipar)
      adq2 = LOG10(Q12) - LOG10(Q02)
      f2p = (f2q1-f2q0)/adq2
      CALL DT_CKMTX(Ipar,X,Q02,pd,f2pq0)
      CALL DT_CKMTX(Ipar,X,Q12,pd,f2pq1)
      f2pp = (f2pq1-f2pq0)/adq2
      fx = (f2p-f2pp)/(f2pp+LOG(DQ2)*f2pq0+TINY10)*Q02
 
      q2 = MAX(Scale**2.0D0,TINY10)
      smooth = 1.0D0 + fx*(q2-Q02)/q2**2
      IF ( q2.LT.Q02 ) THEN
         CALL DT_PDF0(q2,X,F2,val,sea,glu,Ipar)
         Upv = val(1)
         Dnv = val(2)
         Usea = sea(1)
         Dsea = sea(2)
         Str = sea(3)
         Chm = 0.0D0
         Bot = 0.0D0
         Top = 0.0D0
         Gl = glu
      ELSE
         CALL DT_CKMTX(Ipar,X,q2,pd,F2)
         F2 = F2*smooth
         Upv = pd(2) - pd(3)
         Dnv = pd(1) - pd(3)
         Usea = pd(3)
         Dsea = pd(3)
         Str = pd(3)
         Chm = pd(4)
         Bot = pd(5)
         Top = pd(6)
         Gl = pd(0)
C        UPV  = UPV*SMOOTH
C        DNV  = DNV*SMOOTH
C        USEA = USEA*SMOOTH
C        DSEA = DSEA*SMOOTH
C        STR  = STR*SMOOTH
C        CHM  = CHM*SMOOTH
C        GL   = GL*SMOOTH
      END IF
 
      END SUBROUTINE
