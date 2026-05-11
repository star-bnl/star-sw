
      SUBROUTINE DT_CH2RES(If1,If2,If3,If4,Idr,Idxr,Am,Amn,Imode,Irej)
 
C***********************************************************************
C Check chains for resonance production.                               *
C This subroutine replaces COMCMA/COBCMA/COMCM2                        *
C    input:                                                            *
C          IF1,2,3,4    input flavors (q,aq in any order)              *
C          AM           chain mass                                     *
C          MODE = 1     check q-aq chain for meson-resonance           *
C               = 2     check q-qq, aq-aqaq chain for baryon-resonance *
C               = 3     check qq-aqaq chain for lower mass cut         *
C    output:                                                           *
C          IDR = 0      no resonances found                            *
C              = -1     pseudoscalar meson/octet baryon                *
C              = 1      vector-meson/decuplet baryon                   *
C          IDXR         BAMJET-index of corresponding resonance        *
C          AMN          mass of corresponding resonance                *
C                                                                      *
C          IREJ         rejection flag                                 *
C This version dated 06.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Am , am10 , am8 , amhi , amlob , amlom , Amn , 
     &                 amps , amv , amx
      INTEGER i , Idr , Idxr , if , If1 , If2 , If3 , If4 , ifaq , 
     &        ifps , ifq , ifv , Imode , Irej , jb10 , jb8 , jf , mode , 
     &        nf
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C quark-content to particle index conversion (DTUNUC 1.x)
      INCLUDE 'inc/dtq2id'
C rejection counter
      INCLUDE 'inc/dtrejc'
C flags for input different options
      INCLUDE 'inc/dtflg1'
 
 
C*sr 4.7. test
C     DATA AMLOM,AMLOB /0.08D0,0.2D0/
      DIMENSION if(4) , jf(4)
      DATA amlom , amlob/0.1D0 , 0.7D0/
C*
C     DATA AMLOM,AMLOB /0.001D0,0.001D0/
 
      mode = ABS(Imode)
 
      IF ( (mode.LT.1) .OR. (mode.GT.3) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) mode
99010    FORMAT (1X,'CH2RES: MODE ',I4,' not supported!',/,1X,
     &           '        program stopped')
         STOP
      END IF
 
      amx = Am
      Irej = 0
      Idr = 0
      Idxr = 0
      Amn = amx
      IF ( (Am.LE.0.0D0) .AND. (mode.EQ.1) ) amx = amlom
      IF ( (Am.LE.0.0D0) .AND. (mode.EQ.2) ) amx = amlob
 
      if(1) = If1
      if(2) = If2
      if(3) = If3
      if(4) = If4
      nf = 0
      DO i = 1 , 4
         IF ( if(i).NE.0 ) THEN
            nf = nf + 1
            jf(nf) = if(i)
         END IF
      END DO
      IF ( nf.LE.mode ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99020) mode , if
99020    FORMAT (1X,'CH2RES: inconsistent input flavors in MODE ',I4,
     &           ' IF1 = ',I4,' IF2 = ',I4,' IF3 = ',I4,' IF4 = ',I4)
         GOTO 100
      END IF
 
      IF ( mode.EQ.2 ) THEN
 
C check for baryon resonance
         CALL DT_DBKLAS(jf(1),jf(2),jf(3),jb8,jb10)
         am8 = AAM(jb8)
         am10 = AAM(jb10)
         amhi = am10 + 0.3D0
         IF ( amx.LT.am10 ) THEN
            IF ( amx.LT.am8 ) THEN
               IF ( Imode.GT.0 ) THEN
                  IF ( (IREsrj.EQ.1) .OR. (amx.LT.amlob) ) GOTO 100
               ELSE IF ( amx.LT.0.8D0*am8 ) THEN
                  GOTO 100
               END IF
               LOBres = LOBres + 1
            END IF
C    replace chain by oktet baryon
            Idr = -1
            Idxr = jb8
            Amn = am8
         ELSE IF ( amx.LT.amhi ) THEN
            Idr = 1
            Idxr = jb10
            Amn = am10
         END IF
         RETURN
      ELSE IF ( mode.EQ.3 ) THEN
 
C check qq-aqaq for lower mass cut
C   empirical definition of AMHI to allow for (b-antib)-pair prod.
         amhi = 2.5D0
         IF ( amx.GE.amhi ) RETURN
      ELSE
 
C check for meson resonance
         ifq = jf(1)
         ifaq = ABS(jf(2))
         IF ( jf(2).GT.0 ) THEN
            ifq = jf(2)
            ifaq = ABS(jf(1))
         END IF
         ifps = IMPs(ifaq,ifq)
         ifv = IMVe(ifaq,ifq)
         amps = AAM(ifps)
         amv = AAM(ifv)
         amhi = amv + 0.3D0
         IF ( amx.LT.amv ) THEN
            IF ( amx.LT.amps ) THEN
               IF ( Imode.GT.0 ) THEN
                  IF ( (IREsrj.EQ.1) .OR. (amx.LT.amlom) ) GOTO 100
               ELSE IF ( amx.LT.0.8D0*amps ) THEN
                  GOTO 100
               END IF
               LOMres = LOMres + 1
            END IF
C    replace chain by pseudoscalar meson
            Idr = -1
            Idxr = ifps
            Amn = amps
         ELSE IF ( amx.LT.amhi ) THEN
C    replace chain by vector-meson
            Idr = 1
            Idxr = ifv
            Amn = amv
         END IF
         RETURN
      END IF
 
 
 100  IF ( (IOUlev(1).GT.0) .AND. (Imode.GT.0) .AND. LPRi.GT.4 )
     &     WRITE (LOUt,*) 'rejected 1 in CH2RES' , Imode
      Irej = 1
      IRRes(2) = IRRes(2) + 1
      END SUBROUTINE
