* $Id: rlu.F,v 1.1 2000/06/09 22:23:12 nevski Exp $
* $Log: rlu.F,v $
* Revision 1.1  2000/06/09 22:23:12  nevski
* rlu random number generator replaced by rndm
*
*********************************************************************
      FUNCTION RLU (IDUM)
      logical first/.true./    
      if (first) then
          first=.false.
          print *,' RLU replaced by rndm '
      endif
      rlu=rndm(idum)
      end
*********************************************************************
      FUNCTION RLU orig (IDUM)    
    
C...Purpose: to generate random numbers uniformly distributed between   
C...0 and 1, excluding the endpoints.   
      COMMON/LUDATR/MRLU(6),RRLU(100)   
      SAVE /LUDATR/ 
      EQUIVALENCE (MRLU1,MRLU(1)),(MRLU2,MRLU(2)),(MRLU3,MRLU(3)),  
     &(MRLU4,MRLU(4)),(MRLU5,MRLU(5)),(MRLU6,MRLU(6)),  
     &(RRLU98,RRLU(98)),(RRLU99,RRLU(99)),(RRLU00,RRLU(100))    
    
C...Initialize generation from given seed.  
      IF(MRLU2.EQ.0) THEN   
        IJ=MOD(MRLU1/30082,31329)   
        KL=MOD(MRLU1,30082) 
        I=MOD(IJ/177,177)+2 
        J=MOD(IJ,177)+2 
        K=MOD(KL/169,178)+1 
        L=MOD(KL,169)   
        DO 110 II=1,97  
        S=0.    
        T=0.5   
        DO 100 JJ=1,24  
        M=MOD(MOD(I*J,179)*K,179)   
        I=J 
        J=K 
        K=M 
        L=MOD(53*L+1,169)   
        IF(MOD(L*M,64).GE.32) S=S+T 
  100   T=0.5*T 
  110   RRLU(II)=S  
        TWOM24=1.   
        DO 120 I24=1,24 
  120   TWOM24=0.5*TWOM24   
        RRLU98=362436.*TWOM24   
        RRLU99=7654321.*TWOM24  
        RRLU00=16777213.*TWOM24 
        MRLU2=1 
        MRLU3=0 
        MRLU4=97    
        MRLU5=33    
      ENDIF 
    
C...Generate next random number.    
  130 RUNI=RRLU(MRLU4)-RRLU(MRLU5)  
      IF(RUNI.LT.0.) RUNI=RUNI+1.   
      RRLU(MRLU4)=RUNI  
      MRLU4=MRLU4-1 
      IF(MRLU4.EQ.0) MRLU4=97   
      MRLU5=MRLU5-1 
      IF(MRLU5.EQ.0) MRLU5=97   
      RRLU98=RRLU98-RRLU99  
      IF(RRLU98.LT.0.) RRLU98=RRLU98+RRLU00 
      RUNI=RUNI-RRLU98  
      IF(RUNI.LT.0.) RUNI=RUNI+1.   
      IF(RUNI.LE.0.OR.RUNI.GE.1.) GOTO 130  
    
C...Update counters. Random number to output.   
      MRLU3=MRLU3+1 
      IF(MRLU3.EQ.1000000000) THEN  
        MRLU2=MRLU2+1   
        MRLU3=0 
      ENDIF 
      RLU=RUNI  
    
      RETURN    
      END   
    
C*********************************************************************  
    
      SUBROUTINE RLUGET(LFN,MOVE)   
    
C...Purpose: to dump the state of the random number generator on a file 
C...for subsequent startup from this state onwards. 
      COMMON/LUDATR/MRLU(6),RRLU(100)   
      SAVE /LUDATR/ 
      CHARACTER CHERR*8 
    
C...Backspace required number of records (or as many as there are). 
      IF(MOVE.LT.0) THEN    
        NBCK=MIN(MRLU(6),-MOVE) 
        DO 100 IBCK=1,NBCK  
  100   BACKSPACE(LFN,ERR=110,IOSTAT=IERR)  
        MRLU(6)=MRLU(6)-NBCK    
      ENDIF 
    
C...Unformatted write on unit LFN.  
      WRITE(LFN,ERR=110,IOSTAT=IERR) (MRLU(I1),I1=1,5), 
     &(RRLU(I2),I2=1,100)   
      MRLU(6)=MRLU(6)+1 
      RETURN    
    
C...Write error.    
  110 WRITE(CHERR,'(I8)') IERR  
      CALL LUERRM(18,'(RLUGET:) error when accessing file, IOSTAT ='//  
     &CHERR)    
    
      RETURN    
      END   
    
C*********************************************************************  
    
      SUBROUTINE RLUSET(LFN,MOVE)   
    
C...Purpose: to read a state of the random number generator from a file 
C...for subsequent generation from this state onwards.  
      COMMON/LUDATR/MRLU(6),RRLU(100)   
      SAVE /LUDATR/ 
      CHARACTER CHERR*8 
    
C...Backspace required number of records (or as many as there are). 
      IF(MOVE.LT.0) THEN    
        NBCK=MIN(MRLU(6),-MOVE) 
        DO 100 IBCK=1,NBCK  
  100   BACKSPACE(LFN,ERR=120,IOSTAT=IERR)  
        MRLU(6)=MRLU(6)-NBCK    
      ENDIF 
    
C...Unformatted read from unit LFN. 
      NFOR=1+MAX(0,MOVE)    
      DO 110 IFOR=1,NFOR    
  110 READ(LFN,ERR=120,IOSTAT=IERR) (MRLU(I1),I1=1,5),  
     &(RRLU(I2),I2=1,100)   
      MRLU(6)=MRLU(6)+NFOR  
      RETURN    
    
C...Write error.    
  120 WRITE(CHERR,'(I8)') IERR  
      CALL LUERRM(18,'(RLUSET:) error when accessing file, IOSTAT ='//  
     &CHERR)    
    
      RETURN    
      END   
    
