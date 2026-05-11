
      SUBROUTINE DT_EVTFLC(Id,Id1,Mode,Ipos,Irej)
 
C***********************************************************************
C Flavor conservation check.                                           *
C        ID       identity of particle                                 *
C        ID1 = 1  ID for q,aq,qq,aqaq in PDG-numbering scheme          *
C            = 2  ID for particle/resonance in BAMJET numbering scheme *
C            = 3  ID for particle/resonance in PDG    numbering scheme *
C        MODE = 1 initialization and add ID                            *
C             =-1 initialization and subtract ID                       *
C             = 2 add ID                                               *
C             =-2 subtract ID                                          *
C             = 3 check flavor cons.                                   *
C        IPOS     flag to give position of call of EVTFLC to output    *
C                 unit in case of violation                            *
C This version dated 10.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , Id , Id1 , idd , IDT_ICIHAD , IDT_IPDG2B , 
     &        IDT_IQUARK , ifbam , ifl , Ipos , Irej , Mode , nq
      DOUBLE PRECISION TINY10
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10)
 
      Irej = 0
 
      IF ( Mode.EQ.3 ) THEN
         IF ( ifl.NE.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,I3,A,I3)')
     &            'EVTFLC: flavor-conservation failure at pos. ' , 
     &           Ipos , ' !  IFL = ' , ifl
            ifl = 0
 
            Irej = 1
            GOTO 99999
         END IF
         ifl = 0
         RETURN
      END IF
 
      IF ( Mode.EQ.1 ) ifl = 0
 
      IF ( Id.EQ.0 ) RETURN
      IF ( Id1.EQ.1 ) THEN
         idd = ABS(Id)
         nq = 1
         IF ( (idd.GE.100) .AND. (idd.LT.1000) ) nq = 2
         IF ( idd.GE.1000 ) nq = 3
         DO i = 1 , nq
            ifbam = IDT_IPDG2B(Id,i,2)
            IF ( ABS(ifbam).EQ.1 ) THEN
               ifbam = SIGN(2,ifbam)
            ELSE IF ( ABS(ifbam).EQ.2 ) THEN
               ifbam = SIGN(1,ifbam)
            END IF
            IF ( Mode.GT.0 ) THEN
               ifl = ifl + ifbam
            ELSE
               ifl = ifl - ifbam
            END IF
         END DO
         RETURN
      END IF
 
      idd = Id
      IF ( Id1.EQ.3 ) idd = IDT_ICIHAD(Id)
      IF ( (Id1.EQ.2) .OR. (Id1.EQ.3) ) THEN
         DO i = 1 , 3
            IF ( Mode.GT.0 ) THEN
               ifl = ifl + IDT_IQUARK(i,idd)
            ELSE
               ifl = ifl - IDT_IQUARK(i,idd)
            END IF
         END DO
      END IF
99999 END SUBROUTINE
