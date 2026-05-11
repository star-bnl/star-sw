
      DOUBLE PRECISION FUNCTION PHO_CT14ALPHAS(Qq)
 
      IMPLICIT NONE
      DOUBLE PRECISION ALScteq , q , QBAse , QINi , QMAx , Qq , tt , 
     &                 TV , UPD , XMIn , XV
      INTEGER IPDsformat , IPDsset , ISEtch , jlq , jm , jq , ju , lo , 
     &        lpri , MAXVAL , MXF , MXPQX , MXQ , MXVal , MXX , NFMx , 
     &        NT , NX
 
      PARAMETER (MXX=201,MXQ=40,MXF=6,MAXVAL=4)
      PARAMETER (MXPQX=(MXF+1+MAXVAL)*MXQ*MXX)
      DOUBLE PRECISION alsout
 
      COMMON /CTQPAR1/ QBAse , XV(0:MXX) , TV(0:MXQ) , UPD(MXPQX) , 
     &                 ALScteq(0:MXQ)/CTQPAR2/ NX , NT , NFMx , 
     &                 MXVal/XQRANGE/ QINi , QMAx , 
     &                 XMIn/SETCHANGE/ ISEtch , IPDsset , IPDsformat
 
      DATA q , jq/ - 1D0 , 0/
      SAVE 
 
      IF ( IPDsset.NE.1 )
     &      STOP 'pho_CT14Alphas: the PDF table was not initialized'
 
 
      IF ( IPDsformat.LT.11 ) THEN
         IF ( lpri.GT.4 ) WRITE (lo,*) 
     &  'STOP in pho_CT14alphas: the PDF table file has an older format'
         IF ( lpri.GT.4 ) WRITE (lo,*)
     &         'and does not include the table of QCD coupling values.'
         IF ( lpri.GT.4 ) WRITE (lo,*)
     &         'You can still compute the PDFs, but do not call'
         IF ( lpri.GT.4 ) WRITE (lo,*) 
     &   'the pho_CT14alphas function for the interpolation of alpha_s.'
         STOP
      END IF
 
      q = Qq
      tt = LOG(LOG(q/QBAse))
 
C         --------------   Find lower end of interval containing Q, i.e.,
C                          get jq such that qv(jq) .le. q .le. qv(jq+1)...
      jlq = -1
      ju = NT + 1
 100  IF ( ju-jlq.GT.1 ) THEN
         jm = (ju+jlq)/2
         IF ( tt.GE.TV(jm) ) THEN
            jlq = jm
         ELSE
            ju = jm
         END IF
         GOTO 100
      END IF
 
      IF ( jlq.LE.0 ) THEN
         jq = 0
      ELSE IF ( jlq.LE.NT-2 ) THEN
C                                  keep q in the middle, as shown above
         jq = jlq - 1
      ELSE
C                         JLq .GE. Nt-1 case:  Keep at least 4 points >= Jq.
         jq = NT - 3
 
      END IF
C                                 This is the interpolation variable in Q
      CALL PHO_POLINT4F(TV(jq),ALScteq(jq),tt,alsout)
 
      PHO_CT14ALPHAS = alsout
 
C                                       ********************
      END FUNCTION
