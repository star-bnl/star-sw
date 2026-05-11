
      SUBROUTINE DT_LM2RES(If1,If2,Xm,Idr,Idxr,Irej)
 
C***********************************************************************
C Check low-mass diffractive excitation for resonance mass.            *
C   (input)   IF1/2    PDG-indizes of valence partons                  *
C   (in/out)  XM       diffractive mass requested/corrected            *
C   (output)  IDR/IDXR id./BAMJET-index of resonance                   *
C This version dated 12.02.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER idch , Idr , IDT_IPDG2B , Idxr , If1 , if1a , if1b , If2 , 
     &        if2a , if2b , Irej , irej1
      DOUBLE PRECISION OHALF , ONE , Xm , xmi , xmn , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,OHALF=0.5D0,ONE=1.0D0)
 
C kinematics of diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtdiki'
 
      Irej = 0
      if1b = 0
      if2b = 0
      xmi = Xm
 
C BAMJET indices of partons
      if1a = IDT_IPDG2B(If1,1,2)
      IF ( ABS(If1).GE.1000 ) if1b = IDT_IPDG2B(If1,2,2)
      if2a = IDT_IPDG2B(If2,1,2)
      IF ( ABS(If2).GE.1000 ) if2b = IDT_IPDG2B(If2,2,2)
 
C get kind of chains (1 - q-aq, 2 - q-qq/aq-aqaq)
      idch = 2
      IF ( (if1b.EQ.0) .AND. (if2b.EQ.0) ) idch = 1
 
C check for resonance mass
      CALL DT_CH2RES(if1a,if1b,if2a,if2b,Idr,Idxr,xmi,xmn,idch,irej1)
      IF ( irej1.NE.0 ) THEN
 
         Irej = 1
         GOTO 99999
      END IF
 
      Xm = xmn
99999 END SUBROUTINE
