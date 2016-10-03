      PROGRAM SPINTAU
C     *****************
      IMPLICIT REAL(A-H,O-Z)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200) 
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5) 
      COMMON / INOUT / INUT,NOUT
 
C to prevent tau decays in jetset
       mdcy(15,1)=0
c------------------------------------>>>>>>
C-----output ident for TAUOLA. 
         NOUT2 = 6
         NOUT  = 6
c.....JETSET output will be writen on NOUT2
       MSTU(11) = NOUT2
c..... PHOTOS will write on 6
CC-------------------------------------->>>>
c      OPEN(0      ,file='tauola.out')
      OPEN(NOUT   ,file='demo.out')

c------------------------------------>>>>>>
    
c------------------------------------>>>>>>
C------number of requested events
       NEVT=99
C polarization switch
       KEYPOL=1

       CALL TAUOLA(-1,KEYPOL) 

C>>>>>>>>>>>>>>
         DO II= 1,NEVT
           IF(MOD(II,50).EQ.1) WRITE(6,*)'event no=',II
           mdcy(15,1)=0 
           CALL LUEEVT(5,200.0)
           CALL LUHEPC(1)
           n11=ihepdim(dum) ! no of entries in hepevt before tauola
           CALL TAUOLA(0,KEYPOL) 
           n12=ihepdim(dum) ! no of entries in hepevt after  tauola

           if (n12.gt.n11) then
             IF(II.LT.101) CALL LULIST(1)
                           CALL LUHEPC(2)
             IF(II.LT.101) CALL LULIST(1)
           endif
         ENDDO
CC>>>>>>>>>>>>>>
         CALL TAUOLA(1,KEYPOL) 

      CLOSE(NOUT2)
      CLOSE(NOUT)

      END 

