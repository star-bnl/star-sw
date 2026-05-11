
      SUBROUTINE DT_EVTEMC(Pxio,Pyio,Pzio,Eio,Imode,Ipos,Irej)
 
C***********************************************************************
C This version dated 13.12.94 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION chklev , e , edev , Eio , px , pxdev , Pxio , 
     &                 py , pydev , Pyio , pz , pzdev , Pzio , TINY1 , 
     &                 TINY10 , TINY2 , TINY4 , ZERO
      INTEGER Imode , Ipos , Irej , mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY1=1.0D-1,TINY2=1.0D-2,TINY4=1.0D-4,TINY10=1.0D-10,
     &           ZERO=0.0D0)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C flags for input different options
      INCLUDE 'inc/dtflg1'
 
      Irej = 0
 
      mode = Imode
      chklev = TINY10
      IF ( mode.EQ.4 ) THEN
         chklev = TINY2
         mode = 3
      ELSE IF ( mode.EQ.5 ) THEN
         chklev = TINY1
         mode = 3
      ELSE IF ( mode.EQ.-1 ) THEN
         chklev = Eio
         mode = 3
      END IF
 
      IF ( ABS(mode).EQ.3 ) THEN
         pxdev = px
         pydev = py
         pzdev = pz
         edev = e
         IF ( (IFRag(1).EQ.2) .AND. (chklev.LT.TINY4) ) chklev = TINY4
         IF ( (ABS(pxdev).GT.chklev) .OR. (ABS(pydev).GT.chklev) .OR. 
     &        (ABS(pzdev).GT.chklev) .OR. (ABS(edev).GT.chklev) ) THEN
 
            IF ( LPRi.GT.4 .AND. IOUlev(2).GT.0 )
     &            WRITE (LOUt,'(1X,A,I4,A,I8,A,/,4G10.3)')
     &            'EVTEMC: energy-momentum cons. failure at pos. ' , 
     &           Ipos , '  event  ' , NEVhkk , ' ! ' , pxdev , pydev , 
     &           pzdev , edev
            px = 0.0D0
            py = 0.0D0
            pz = 0.0D0
            e = 0.0D0
 
            Irej = 1
            GOTO 99999
         END IF
         px = 0.0D0
         py = 0.0D0
         pz = 0.0D0
         e = 0.0D0
         RETURN
      END IF
 
      IF ( mode.EQ.1 ) THEN
         px = 0.0D0
         py = 0.0D0
         pz = 0.0D0
         e = 0.0D0
      END IF
 
      px = px + Pxio
      py = py + Pyio
      pz = pz + Pzio
      e = e + Eio
 
99999 END SUBROUTINE
