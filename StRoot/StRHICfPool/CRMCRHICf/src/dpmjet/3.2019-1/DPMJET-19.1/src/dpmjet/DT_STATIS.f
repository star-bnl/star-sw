
      SUBROUTINE DT_STATIS(Mode)
 
C***********************************************************************
C Initialization and output of run-statistics.                         *
C              MODE  = 1     initialization                            *
C                    = 2     output                                    *
C This version dated 23.01.94 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION fac , pp , pt , TINY3
      INTEGER i , irej1 , j , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY3=1.0D-3)
 
C statistics
      INCLUDE 'inc/dtsta1'
C rejection counter
      INCLUDE 'inc/dtrejc'
C central particle production, impact parameter biasing
      INCLUDE 'inc/dtimpa'
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C flags for particle decays
      INCLUDE 'inc/dtfrpa'
C diquark-breaking mechanism
      INCLUDE 'inc/dtdiqb'
 
      DIMENSION pp(4) , pt(4)
 
      IF ( Mode.EQ.2 ) THEN
 
C output
 
C   statistics counter
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010    FORMAT (/,/,1X,'STATIS:',20X,'statistics of the run',/,28X,
     &           '---------------------')
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99020) ICRequ , ICSamp , 
     &        DBLE(ICSamp)/DBLE(ICRequ)
99020    FORMAT (/,1X,'number of events requested / sampled',13X,I8,
     &           ' / ',I8,/,1X,'number of samp. evts per requested ',
     &           'event',11X,F9.1)
         IF ( ICDiff(1).NE.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99030) ICDiff
99030       FORMAT (/,1X,'diffractive events:    total   ',I8,/,49X,
     &              'low mass   high mass',/,24X,'single diffraction',
     &              7X,I8,4X,I8,/,24X,'double diffraction',7X,I8,4X,I8)
         END IF
         IF ( ICEntr.GT.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99040) DBLE(ICCpro)
     &           /DBLE(ICSamp) , DBLE(ICSamp)/DBLE(ICCpro)
99040       FORMAT (/,1X,'central production:',/,2X,'mean number',
     &              ' of sampled Glauber-events per event',9X,F9.1,/,2X,
     &              'fraction of production cross section',21X,F10.6)
         END IF
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99050) DBLE(ICDpr)/DBLE(ICSamp) , 
     &        DBLE(ICDta)/DBLE(ICSamp)
99050    FORMAT (/,54X,'proj.    targ.',/,1X,
     &           'average number of wounded',
     &           ' nucleons after x-sampling',2(4X,F6.2))
 
         IF ( MCGene.EQ.1 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99060) DBLE(ICRjss)
     &           /DBLE(ICSamp)
99060       FORMAT (/,1X,'mean number of sea-sea chain rejections per',
     &              ' event',3X,F9.1)
            IF ( ISIcha.EQ.1 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99070) DBLE(ICVv2s)
     &              /DBLE(ICSamp)
99070          FORMAT (/,1X,'Reggeon contribution:',/,1X,'mean number ',
     &                 'of single chains  per event',13X,F9.1)
            END IF
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99080)
99080       FORMAT (/,1X,'chain system statistics:  (per event)',/,23X,
     &              'mean number of chains      mean number of chains',
     &              /,23X,
     &              'sampled    hadronized      having mass of a reso.')
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99090)
     &           (DBLE(ICChai(1,j))/(2.0D0*DBLE(ICSamp)),
     &           DBLE(ICChai(2,j))/(2.0D0*DBLE(ICRequ)),DBLE(ICRes(j))
     &           /(2.0D0*DBLE(ICRequ)),j=1,8) , DBLE(ICChai(2,9))
     &           /MAX(DBLE(ICChai(1,9)),TINY3)
99090       FORMAT (1X,'sea     - sea     ',6X,F4.1,8X,F4.1,17X,F4.1,/,
     &              1X,'disea   - sea     ',6X,F4.1,8X,F4.1,17X,F4.1,/,
     &              1X,'sea     - disea   ',6X,F4.1,8X,F4.1,17X,F4.1,/,
     &              1X,'sea     - valence ',6X,F4.1,8X,F4.1,17X,F4.1,/,
     &              1X,'disea   - valence ',6X,F4.1,8X,F4.1,17X,F4.1,/,
     &              1X,'valence - sea     ',6X,F4.1,8X,F4.1,17X,F4.1,/,
     &              1X,'valence - disea   ',6X,F4.1,8X,F4.1,17X,F4.1,/,
     &              1X,'valence - valence ',6X,F4.1,8X,F4.1,17X,F4.1,/,
     &              1X,'fused chains      ',18X,F4.1,17X,F4.1,/)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99100)
     &           (DBLE(IRCron(i))/MAX(DBLE(IRCron(1)),TINY3),i=2,3) , 
     &           DBLE(IRPt)/DBLE(ICRequ) , 
     &           (DBLE(IRRes(i))/DBLE(ICRequ),i=1,2) , DBLE(LOMres)
     &           /DBLE(ICRequ) , DBLE(LOBres)/DBLE(ICRequ) , 
     &           (DBLE(IRChki(i))/DBLE(ICRequ),i=1,2) , 
     &           (DBLE(IRDiff(i))/DBLE(ICRequ),i=1,2) , DBLE(IRHha)
     &           /DBLE(ICRequ) , DBLE(IRFrag)/DBLE(ICRequ) , DBLE(IREvt)
     &           /DBLE(ICRequ) , (DBLE(IRExci(i))/DBLE(ICRequ),i=1,2) , 
     &           IRExci(3)
99100       FORMAT (/,1X,'Rejection counter:  (NEVT = no. of events)',/,
     &              /,1X,'Cronin-effect (CRONIN)',15X,
     &              'IRCRON(2)/IRCRON(1) = ',F7.2,/,38X,
     &              'IRCRON(3)/IRCRON(1) = ',F7.2,/,1X,
     &              'Intrins. p_t (GETSPT)',21X,'IRPT     /NEVT = ',
     &              F7.2,/,1X,'Chain mass corr. for resonances (EVTRES)'
     &              ,2X,'IRRES(1) /NEVT = ',F7.2,/,33X,
     &              '(CH2RES)  IRRES(2) /','NEVT = ',F7.2,/,43X,
     &              'LOMRES   /NEVT = ',F7.2,/,43X,'LOBRES   /NEVT = ',
     &              F7.2,/,1X,'Kinem. corr. of',
     &              ' 2-chain systems (CHKINE)  IRCHKI(1)/NEVT = ',F7.2,
     &              /,43X,'IRCHKI(2)/NEVT = ',F7.2,/,1X,'Diffraction',
     &              31X,'IRDIFF(1)/NEVT = ',F7.2,/,43X,
     &              'IRDIFF(2)/NEVT = ',F7.2,/,1X,'Total no. of rej.',
     &              ' in chain-systems treatment (GETCSY)',/,43X,
     &              'IRHHA    /NEVT = ',F7.2,/,1X,
     &              'Fragmentation (EVTFRA)',' (not yet used!)',4X,
     &              'IRFRAG   /NEVT = ',F7.2,/,1X,
     &              'Total no. of rej. in DPM-treatment of one event',
     &              ' (EVENTA)',/,43X,'IREVT    /NEVT = ',F7.2,/,1X,
     &              'Treatment of final nucleon conf.',10X,
     &              'IREXCI(1)/NEVT = ',F7.2,/,43X,'IREXCI(2)/NEVT = ',
     &              F7.2,/,48X,'IREXCI(3) = ',I5,/)
         ELSE IF ( MCGene.EQ.2 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99110) ELOjet
99110       FORMAT (/,/,1X,'PHOJET-treatment of chain systems above  ',
     &              F4.1,' GeV')
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99120)
99120       FORMAT (/,1X,'1. chain system statistics - total numbers:',
     &              /,30X,'--------------',/,/,12X,'s-s',5X,'d-s',5X,
     &              's-d',5X,'s-v',5X,'d-v',5X,'v-s',5X,'v-d',5X,'v-v')
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99130)
     &           ((ICEvtg(i,j),i=1,8),j=0,1) , 
     &           (INT(ICChai(2,i)/2.0D0),i=1,8) , (ICEvtg(i,2),i=1,8) , 
     &           (ICEvtg(i,29),i=1,8) , ((ICEvtg(i,j),i=1,8),j=3,7) , 
     &           ((ICEvtg(i,j),i=1,8),j=19,21) , (ICEvtg(i,8),i=1,8) , 
     &           ((ICEvtg(i,j),i=1,8),j=22,24) , (ICEvtg(i,9),i=1,8) , 
     &           ((ICEvtg(i,j),i=1,8),j=25,28) , 
     &           ((ICEvtg(i,j),i=1,8),j=10,18)
99130       FORMAT (/,1X,'req.to.',8I8,/,/,1X,'low rq.',8I8,/,1X,
     &              'low ac.',8I8,/,/,1X,'PHOJET ',8I8,/,'   sngl ',8I8,
     &              /,/,' no-dif.',8I8,/,' el-sca.',8I8,/,' qel-sc.',
     &              8I8,/,' dbl-Po.',8I8,/,' diff-1 ',8I8,/,'  low   ',
     &              8I8,/,'  high  ',8I8,/,'  h-diff',8I8,/,' diff-2 ',
     &              8I8,/,'  low   ',8I8,/,'  high  ',8I8,/,'  h-diff',
     &              8I8,/,' dbl-di.',8I8,/,'  lo-lo ',8I8,/,'  hi-hi ',
     &              8I8,/,'  lo-hi ',8I8,/,'  hi-lo ',8I8,/,' dir-ga.',
     &              8I8,/,/,' dir-1  ',8I8,/,' dir-2  ',8I8,/,
     &              ' dbl-dir',8I8,/,' s-Pom. ',8I8,/,' h-Pom. ',8I8,/,
     &              ' s-Reg. ',8I8,/,' enh-trg',8I8,/,' enh-log',8I8)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99140)
99140       FORMAT (/,1X,'2. chain system statistics -',
     &              ' mean numbers per evt:',/,30X,
     &              '---------------------',/,/,16X,'s-s',7X,'d-s',7X,
     &              's-d')
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99150)
     &           ((DBLE(ICEvtg(i,j))/DBLE(ICSamp),i=1,3),j=0,1) , 
     &           (DBLE(ICChai(2,i))/(2.0D0*DBLE(ICSamp)),i=1,3) , 
     &           ((DBLE(ICEvtg(i,j))/DBLE(ICSamp),i=1,3),j=2,18)
99150       FORMAT (/,1X,'req.to.    ',3E10.2,/,/,1X,'low rq.    ',
     &              3E10.2,/,1X,'low ac.    ',3E10.2,/,/,1X,
     &              'PHOJET     ',3E10.2,/,/,' no-dif.    ',3E10.2,/,
     &              ' el-sca.    ',3E10.2,/,' qel-sc.    ',3E10.2,/,
     &              ' dbl-Po.    ',3E10.2,/,' diff-1     ',3E10.2,/,
     &              ' diff-2     ',3E10.2,/,' dbl-di.    ',3E10.2,/,
     &              ' dir-ga.    ',3E10.2,/,/,' dir-1      ',3E10.2,/,
     &              ' dir-2      ',3E10.2,/,' dbl-dir    ',3E10.2,/,
     &              ' s-Pom.     ',3E10.2,/,' h-Pom.     ',3E10.2,/,
     &              ' s-Reg.     ',3E10.2,/,' enh-trg    ',3E10.2,/,
     &              ' enh-log    ',3E10.2)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99160)
99160       FORMAT (/,16X,'s-v',7X,'d-v',7X,'v-s',7X,'v-d',7X,'v-v')
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99170)
     &           ((DBLE(ICEvtg(i,j))/DBLE(ICSamp),i=4,8),j=0,1) , 
     &           (DBLE(ICChai(2,i))/(2.0D0*DBLE(ICSamp)),i=4,8) , 
     &           ((DBLE(ICEvtg(i,j))/DBLE(ICSamp),i=4,8),j=2,18)
99170       FORMAT (/,1X,'req.to.    ',5E10.2,/,/,1X,'low rq.    ',
     &              5E10.2,/,1X,'low ac.    ',5E10.2,/,/,1X,
     &              'PHOJET     ',5E10.2,/,/,' no-dif.    ',5E10.2,/,
     &              ' el-sca.    ',5E10.2,/,' qel-sc.    ',5E10.2,/,
     &              ' dbl-Po.    ',5E10.2,/,' diff-1     ',5E10.2,/,
     &              ' diff-2     ',5E10.2,/,' dbl-di.    ',5E10.2,/,
     &              ' dir-ga.    ',5E10.2,/,/,' dir-1      ',5E10.2,/,
     &              ' dir-2      ',5E10.2,/,' dbl-dir    ',5E10.2,/,
     &              ' s-Pom.     ',5E10.2,/,' h-Pom.     ',5E10.2,/,
     &              ' s-Reg.     ',5E10.2,/,' enh-trg    ',5E10.2,/,
     &              ' enh-log    ',5E10.2)
 
         END IF
         CALL DT_CHASTA(1)
 
         IF ( (PDBsea(1).GT.0.0D0) .OR. (PDBsea(2).GT.0.0D0) .OR. 
     &        (PDBsea(3).GT.0.0D0) ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'YGS1S,YGS2S,YUS1S,YUS2S' , 
     &           DBRka(1,1) + DBRka(2,1) , DBRka(1,2) + DBRka(2,2) , 
     &           DBRka(1,3) + DBRka(2,3) , DBRka(1,4) + DBRka(2,4)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'YGS1R,YGS2R,YUS1R,YUS2R' , 
     &           DBRkr(1,1) + DBRkr(2,1) , DBRkr(1,2) + DBRkr(2,2) , 
     &           DBRkr(1,3) + DBRkr(2,3) , DBRkr(1,4) + DBRkr(2,4)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &            'YGSA1S,YGSA2S,YUSA1S,YUSA2S' , DBRka(1,5)
     &           + DBRka(2,5) , DBRka(1,6) + DBRka(2,6) , DBRka(1,7)
     &           + DBRka(2,7) , DBRka(1,8) + DBRka(2,8)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &            'YGSA1R,YGSA2R,YUSA1R,YUSA2R' , DBRkr(1,5)
     &           + DBRkr(2,5) , DBRkr(1,6) + DBRkr(2,6) , DBRkr(1,7)
     &           + DBRkr(2,7) , DBRkr(1,8) + DBRkr(2,8)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'YG31S,YG32S,YU31S,YU32S' , 
     &           DBRka(3,1) , DBRka(3,2) , DBRka(3,3) , DBRka(3,4)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'YG31R,YG32R,YU31R,YU32R' , 
     &           DBRkr(3,1) , DBRkr(3,2) , DBRkr(3,3) , DBRkr(3,4)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &            'YG3A1S,YG3A2S,YU3A1S,YU3A2S' , DBRka(3,5) , 
     &           DBRka(3,6) , DBRka(3,7) , DBRka(3,8)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &            'YG3A1R,YG3A2R,YU3A1R,YU3A2R' , DBRkr(3,5) , 
     &           DBRkr(3,6) , DBRkr(3,7) , DBRkr(3,8)
         END IF
 
         fac = 1.0D0
 
C        CALL PHO_PHIST(-2,SIGMAX)
 
         IF ( MCGene.EQ.2 ) CALL PHO_EVENT(-2,pp,pt,fac,irej1)
 
         CALL DT_XTIME
         GOTO 99999
      END IF
 
C initialization
 
C   initialize statistics counter
      ICRequ = 0
      ICSamp = 0
      ICCpro = 0
      ICDpr = 0
      ICDta = 0
      ICRjss = 0
      ICVv2s = 0
      DO i = 1 , 9
         ICRes(i) = 0
         ICChai(1,i) = 0
         ICChai(2,i) = 0
      END DO
C   initialize rejection counter
      IRPt = 0
      IRHha = 0
      LOMres = 0
      LOBres = 0
      IRFrag = 0
      IREvt = 0
      IRRes(1) = 0
      IRRes(2) = 0
      IRChki(1) = 0
      IRChki(2) = 0
      IRCron(1) = 0
      IRCron(2) = 0
      IRCron(3) = 0
      IRDiff(1) = 0
      IRDiff(2) = 0
      IRInc = 0
      DO i = 1 , 5
         ICDiff(i) = 0
      END DO
      DO i = 1 , 8
         DO j = 0 , 30
            ICEvtg(i,j) = 0
         END DO
      END DO
 
 
99999 END SUBROUTINE
