
      SUBROUTINE PHO_SETPAR(Iside,Idpdg,Idcpc,Pvir)
C**********************************************************************
C
C     assign a particle to either side 1 or 2
C     (including special treatment for remnants)
C
C     input:    Iside      1,2  side selected for the particle
C                          -2   output of current settings
C               IDpdg      PDG number
C               IDcpc      CPC number
C                          0     CPC determination in subroutine
C                          -1    special particle remnant, IDPDG
C                                is the particle number the remnant
C                                corresponds to (see /POHDFL/)
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Iside , Idpdg , Idcpc
      DOUBLE PRECISION Pvir
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  particle ID translation table
      INCLUDE 'inc/popar1'
C  general particle data
      INCLUDE 'inc/popar2'
C  particle decay data
      INCLUDE 'inc/popar3'
 
C  external functions
      INTEGER IPHO_PDG2ID , IPHO_CHR3 , IPHO_BAR3
      DOUBLE PRECISION PHO_PMASS
 
C  local variables
      INTEGER i , idcpcn , idcpcr , idpdgn , idpdgr , idb , ifl1 , 
     &        ifl2 , ifl3
 
      IF ( IDEb(87).GE.15 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I2,/5X,A,2I6)')
     &         'PHO_SETPAR: called for side' , Iside , 'IDPDG, IDCPC' , 
     &        Idpdg , Idcpc
      END IF
 
      IF ( (Iside.EQ.1) .OR. (Iside.EQ.2) ) THEN
         idcpcn = Idcpc
C  remnant?
         IF ( Idcpc.EQ.-1 ) THEN
            IF ( Iside.EQ.1 ) THEN
               idpdgr = 81
            ELSE
               idpdgr = 82
            END IF
            idcpcr = IPHO_PDG2ID(idpdgr)
            IDEqb(Iside) = IPHO_PDG2ID(Idpdg)
            IDEqp(Iside) = Idpdg
C  copy particle properties
            idb = ABS(IDEqb(Iside))
            XM_list(idcpcr) = XM_list(idb)
            TAU_list(idcpcr) = TAU_list(idb)
            GAM_list(idcpcr) = GAM_list(idb)
            IF ( IHFls(Iside).EQ.1 ) THEN
               ICH3_list(idcpcr) = IPHO_CHR3(IDEqb(Iside),0)
               IBA3_list(idcpcr) = IPHO_BAR3(IDEqb(Iside),0)
            ELSE
               ICH3_list(idcpcr) = 0
               IBA3_list(idcpcr) = 0
            END IF
C  quark content
            ifl1 = IHFld(Iside,1)
            ifl2 = IHFld(Iside,2)
            ifl3 = 0
            IF ( IHFls(Iside).EQ.1 ) THEN
               IF ( ABS(IHFld(Iside,1)).GT.1000 ) THEN
                  ifl1 = IHFld(Iside,1)/1000
                  ifl2 = MOD(IHFld(Iside,1)/100,10)
                  ifl3 = IHFld(Iside,2)
               ELSE IF ( ABS(IHFld(Iside,2)).GT.1000 ) THEN
                  ifl1 = IHFld(Iside,1)
                  ifl2 = IHFld(Iside,2)/1000
                  ifl3 = MOD(IHFld(Iside,2)/100,10)
               END IF
            END IF
            IQ_list(1,idcpcr) = ifl1
            IQ_list(2,idcpcr) = ifl2
            IQ_list(3,idcpcr) = ifl3
 
            idcpcn = idcpcr
            idpdgn = idpdgr
 
            IF ( IDEb(87).GE.5 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I2,/5X,A,I7,4I6)')
     &               'pho_setpar: remnant assignment side' , Iside , 
     &              'IDPDG,IFL1,2,3,IVAL' , idpdgn , ifl1 , ifl2 , 
     &              ifl3 , IHFls(Iside)
            END IF
         ELSE IF ( Idcpc.EQ.0 ) THEN
C  ordinary hadron
            IHFls(Iside) = 1
            IHFld(Iside,1) = 0
            IHFld(Iside,2) = 0
            idcpcn = IPHO_PDG2ID(Idpdg)
            idpdgn = Idpdg
            IDEqp(Iside) = 0
         END IF
 
C initialize /POGCMS/
         IFPap(Iside) = idpdgn
         IFPab(Iside) = idcpcn
         PMAss(Iside) = PHO_PMASS(idcpcn,0)
         IF ( IFPap(Iside).EQ.22 ) THEN
            PVIrt(Iside) = ABS(Pvir)
         ELSE
            PVIrt(Iside) = 0.D0
         END IF
 
      ELSE IF ( Iside.EQ.-2 ) THEN
C  output of current settings
         DO i = 1 , 2
            IF ( LPRi.GT.4 )
     &            WRITE (LO,'(1X,A,I2,1X,A,I7,I4,1X,1P2E10.3)')
     &            'PHO_SETPAR: side' , i , 'IDPDG,IDcpc,PMASS,PVIRT' , 
     &           IFPap(i) , IFPab(i) , PMAss(i) , PVIrt(i)
            IF ( (IFPap(i).EQ.81) .OR. (IFPap(i).EQ.82) ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I7,I4,I2,3I5)')
     &               'remnant:IDPDG,IDcpc,IVAL,IFLA1,2' , IDEqp(i) , 
     &              IDEqb(i) , IHFls(i) , IHFld(i,1) , IHFld(i,2)
            END IF
         END DO
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I8)')
     &         'pho_setpar: invalid argument (Iside)' , Iside
      END IF
 
      END SUBROUTINE
