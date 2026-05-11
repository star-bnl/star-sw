
      SUBROUTINE DT_KKEVNT(Kkmat,Irej)
 
C***********************************************************************
C Treatment of complete nucleus-nucleus or hadron-nucleus scattering   *
C without nuclear effects (one event).                                 *
C This subroutine is an update of the previous version (KKEVT) written *
C by J. Ranft/ H.-J. Moehring.                                         *
C This version dated 20.04.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , eprold , frac , rr , TINY10 , ZERO
      INTEGER iback , ipold , Irej , irej1 , itold , jjpold , jjproj , 
     &        Kkmat , MAXINT , MAXNCL , MAXSQU , MAXVQU , mode , nc , 
     &        nevold , nn , np , nt
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10)
 
 
      PARAMETER (MAXNCL=260,MAXVQU=MAXNCL,MAXSQU=20*MAXVQU,
     &           MAXINT=MAXVQU+MAXSQU)
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C rejection counter
      INCLUDE 'inc/dtrejc'
C statistics
      INCLUDE 'inc/dtsta1'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C flags for diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtflg3'
C interface HADRIN-DPM
      INCLUDE 'inc/hnthre'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C coordinates of nucleons
      INCLUDE 'inc/dtnuco'
C interface between Glauber formalism and DPM
      INCLUDE 'inc/dtglif'
C Glauber formalism: collision properties
      INCLUDE 'inc/dtglcp'
C central particle production, impact parameter biasing
      INCLUDE 'inc/dtimpa'
C*temporary
C statistics: Glauber-formalism
      INCLUDE 'inc/dtsta3'
C*
#ifdef FOR_FLUKA
C     COMMON / DBGPRE / LDBGPR
C     LOGICAL LDBGPR
#endif
 
      DATA nevold , ipold , itold , jjpold , eprold/4*0 , 0.0D0/
 
      Irej = 0
      ICRequ = ICRequ + 1
      nc = 0
 
#ifdef FOR_FLUKA
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,3I6)')
C    &       ' KKEVNT-1:',IREJ,ICREQU,NC
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
#endif
 
 100  ICSamp = ICSamp + 1
      nc = nc + 1
      IF ( MOD(nc,10).EQ.0 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) NEVhkk
99010    FORMAT (1X,'KKEVNT: event ',I8,' rejected!')
 
         Irej = 1
         GOTO 99999
      END IF
 
C initialize DTEVT1/DTEVT2
      CALL DT_EVTINI
#ifdef FOR_FLUKA
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,I6)')
C    &       ' KKEVNT EVTINI:',NHKK
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
#endif
 
C We need the following only in order to sample nucleon coordinates.
C However we don't have parameters (cross sections, slope etc.)
C for neutrinos available. Therefore switch projectile to proton
C in this case.
      IF ( MCGene.EQ.4 ) THEN
         jjproj = 1
      ELSE
         jjproj = IJProj
      END IF
 
 
C*anfe 20.08.2016
C Obsolete, since DT_PHOXS has been patched
C **anfe 10.10.2015
C * since the Glauber formalism uses cross-section tables from PHOJET,
C * one needs to tell phojet to switch to the desired combination now
 
C       CALL PHO_SETPAR(1,IDT_IPDGHA(IJPROJ),0,ZERO)
C       CALL PHO_SETPAR(2,IDT_IPDGHA(IJTARG),0,ZERO)
C       CALL PHO_SETPCOMB
C make sure that Glauber-formalism is called each time the interaction
C configuration changed
 200  IF ( (NEVhkk.NE.nevold) .OR. (ICEntr.GT.0) .OR. (IP.NE.ipold) .OR. 
     &     (IT.NE.itold) .OR. (jjproj.NE.jjpold) .OR. 
     &     (ABS(EPRoj-eprold).GT.TINY10) ) THEN
C sample number of nucleon-nucleon coll. according to Glauber-form.
         CALL DT_GLAUBE(IP,IT,jjproj,BIMpac,nn,np,nt,JSSh,JTSh,Kkmat)
#ifdef FOR_FLUKA
C        IF ( LDBGPR ) THEN
C           WRITE (77,'(A,6I6)')
C    &          ' KKEVNT GLAUBE:',IP,IT,JJPROJ,NN,NP,NT
C           CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C           WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C        END IF
#endif
 
         NWTsam = nn
         NWAsam = np
         NWBsam = nt
         nevold = NEVhkk
         ipold = IP
         itold = IT
         jjpold = jjproj
         eprold = EPRoj
      END IF
 
C force diffractive particle production in h-K interactions
      IF ( ((ABS(ISIngd).GT.1) .OR. (ABS(IDOubd).GT.1)) .AND. (IP.EQ.1)
     &     .AND. (nn.NE.1) ) THEN
         nevold = 0
         GOTO 200
      END IF
 
C check number of involved proj. nucl. (NP) if central prod.is requested
      IF ( ICEntr.GT.0 ) THEN
         CALL DT_CHKCEN(IP,IT,np,nt,iback)
         IF ( iback.GT.0 ) GOTO 200
      END IF
 
C get initial nucleon-configuration in projectile and target
C rest-system (including Fermi-momenta if requested)
      CALL DT_ININUC(IJProj,IP,IPZ,PKOo,JSSh,1)
#ifdef FOR_FLUKA
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,3I6,(/,10I6))')
C    &       ' KKEVNT ININUC-1:',IP,IPZ,MODE,(JSSH(IJK),IJK=1,IP)
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
#endif
 
      mode = 2
      IF ( EPRoj.LE.EHAdth ) mode = 3
      CALL DT_ININUC(IJTarg,IT,ITZ,TKOo,JTSh,mode)
#ifdef FOR_FLUKA
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,3I6,(/,10I6))')
C    &       ' KKEVNT ININUC-2:',IT,ITZ,MODE,(JTSH(IJK),IJK=1,IT)
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
#endif
 
      IF ( (MCGene.NE.3) .AND. (MCGene.NE.4) ) THEN
 
C activate HADRIN at low energies (implemented for h-N scattering only)
         IF ( EPRoj.LE.EHAdhi ) THEN
            IF ( EHAdth.LT.ZERO ) THEN
C   smooth transition btwn. DPM and HADRIN
               frac = (EPRoj-EHAdlo)/(EHAdhi-EHAdlo)
               rr = DT_RNDM(frac)
               IF ( rr.GT.frac ) THEN
                  IF ( IP.EQ.1 ) THEN
                     CALL DT_HADCOL(IJProj,PPRoj,IDXta,irej1)
                     IF ( irej1.GT.0 ) GOTO 100
                     RETURN
                  ELSE
 
                     IF ( LPRi.GT.4 ) WRITE (LOUt,99030) IP , IT , 
     &                    EPRoj , EHAdth
                  END IF
               END IF
C   fixed threshold for onset of production via HADRIN
            ELSE IF ( EPRoj.LE.EHAdth ) THEN
               IF ( IP.EQ.1 ) THEN
                  CALL DT_HADCOL(IJProj,PPRoj,IDXta,irej1)
                  IF ( irej1.GT.0 ) GOTO 100
                  RETURN
               ELSE
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,99030) IP , IT , EPRoj , 
     &                 EHAdth
               END IF
            END IF
         END IF
 
C sampling of momentum-x fractions & flavors of chain ends
         CALL DT_SPLPTN(nn)
#ifdef FOR_FLUKA
C        IF ( LDBGPR ) THEN
C           WRITE (77,'(A,2I6)')
C    &          ' KKEVNT SPLPTN:',NN,NHKK
C           CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C           WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C        END IF
#endif
 
C Lorentz-transformation of wounded nucleons into nucl.-nucl. cms
         CALL DT_NUC2CM
#ifdef FOR_FLUKA
C        IF ( LDBGPR ) THEN
C           WRITE (77,'(A,I6)')
C    &          ' KKEVNT NUC2CM:',NHKK
C           CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C           WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C        END IF
#endif
 
C collect momenta of chain ends and put them into DTEVT1
         CALL DT_GETPTN(IP,nn,NCSy,irej1)
#ifdef FOR_FLUKA
C        IF ( LDBGPR ) THEN
C           WRITE (77,'(A,3I6)')
C    &          ' KKEVNT GETPTN:',NN,IREJ1,NHKK
C           CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C           WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C        END IF
#endif
         IF ( irej1.NE.0 ) GOTO 100
 
      END IF
 
C handle chains including fragmentation (two-chain approximation)
      IF ( MCGene.EQ.1 ) THEN
C  two-chain approximation
         CALL DT_EVENTA(IJProj,IP,IT,NCSy,irej1)
         IF ( irej1.NE.0 ) THEN
 
            IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &            'rejected 1 in KKEVNT'
            GOTO 100
         END IF
      ELSE IF ( MCGene.EQ.2 ) THEN
C  multiple-Po exchange including minijets
         CALL DT_EVENTB(NCSy,irej1)
         IF ( irej1.NE.0 ) THEN
 
            IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &            'rejected 2 in KKEVNT'
            GOTO 100
         END IF
      ELSE IF ( MCGene.EQ.3 ) THEN
 
         STOP ' This version does not contain LEPTO !'
 
      ELSE IF ( MCGene.EQ.4 ) THEN
C  quasi-elastic neutrino scattering
         CALL DT_EVENTD(irej1)
         IF ( irej1.NE.0 ) THEN
 
            IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &            'rejected 4 in KKEVNT'
            GOTO 100
         END IF
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99020) MCGene
99020    FORMAT (1X,'KKEVNT:   warning! event-generator',I4,
     &           ' not available - program stopped')
         STOP
      END IF
#ifdef FOR_FLUKA
C     IF ( LDBGPR ) THEN
C        WRITE (77,'(A,2I6)')
C    &       ' KKEVNT EVENTx:',MCGENE,NHKK
C        CALL FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
C        WRITE(77,'(2X,2Z8)')ISEED1,ISEED2
C     END IF
#endif
      RETURN
99030 FORMAT (1X,'KKEVNT:   warning! interaction of proj. (m=',I3,
     &        ') with target (m=',I3,')',/,11X,'at E_lab=',F5.1,
     &        'GeV (threshold-energy: ',F8.1,'GeV) cannot be handled')
99999 END SUBROUTINE
