      SUBROUTINE PHYFIX(NSTOP,NSTART)
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5) 
      SAVE /LUJETS/ 
C NSTOP NSTART : when PHYTIA history ends and event starts.
      NSTOP=0
      NSTART=1
      DO I=1, N
       IF(K(I,1).NE.21) THEN
           NSTOP = I-1
           NSTART= I
           GOTO 500
       ENDIF
      ENDDO
 500  CONTINUE
      END
