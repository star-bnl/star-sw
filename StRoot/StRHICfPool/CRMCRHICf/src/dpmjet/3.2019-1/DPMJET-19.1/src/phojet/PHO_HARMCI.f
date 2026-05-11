
      SUBROUTINE PHO_HARMCI(Ip,Emaxf)
C**********************************************************************
C
C     initialize MC sampling and calculate hard cross section
C
C     input:  IP       particle combination (neg. number for user cut)
C             EMAXF    maximum CMS energy for
C                      interpolation table in reference to PTCUT(1..4)
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION delta , DEPS , dspt , ee , ellow , Emaxf , 
     &                 PHO_PTCUT , PLARGE , ptc , pv1 , pv2 , q2a , q2b
      INTEGER i , ia , ib , idp1 , idp2 , ie , il , Ip , k , m
      SAVE 
 
      PARAMETER (DEPS=1.D-10,PLARGE=1.D20)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  some constants
      INCLUDE 'inc/pocons'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  scale parameters for parton model calculations
      INCLUDE 'inc/pohscl'
C  names of hard scattering processes
      INCLUDE 'inc/pohpro'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  interpolation tables for hard cross section and MC selection weights
#ifndef FOR_CORSIKA
      INCLUDE 'inc/pohtab'
#else
      INCLUDE 'inc/pohtab50'
#endif
C  event weights and generated cross section
      INCLUDE 'inc/powght'
 
      COMPLEX*16 dsig
      DIMENSION dsig(0:MAX_PRO_2) , dspt(0:MAX_PRO_2)
 
C  initialization for all pt cutoffs
      i = ABS(Ip)
      il = i
      IF ( Ip.LT.0 ) THEN
         il = 0
         ptc = HSWcut(4+i)
      ELSE
         ptc = PHO_PTCUT(PARmdl(19),i)
      END IF
 
C  skip unassigned PTCUT
      IF ( ptc.GE.0.5D0 ) THEN
 
         IH_q2a_up(i,IDXmpar) = 1
         IH_q2b_up(i,IDXmpar) = 1
         DO ib = 1 , MAX_TAB_Q2
            DO ia = 1 , MAX_TAB_Q2
               DO ie = 1 , MAX_TAB_E
                  DO m = -1 , MAX_PRO_2
                     HFAc_tab(m,ie,ia,ib,i,IDXmpar) = 0.D0
                     HWGx_tab(m,ie,ia,ib,i,IDXmpar) = 0.D0
                     HSIg_tab(m,ie,ia,ib,i,IDXmpar) = 0.D0
                     HDPt_tab(m,ie,ia,ib,i,IDXmpar) = 0.D0
                  END DO
               END DO
            END DO
         END DO
 
         ellow = LOG(2.05*ptc)
         delta = (LOG(Emaxf)-ellow)/DBLE(IH_ecm_up(i,IDXmpar)-1)
C  energy too low
         IF ( delta.GT.0.D0 ) THEN
 
C  switch between external particles and Pomeron
            IF ( i.EQ.4 ) THEN
               idp1 = 990
               pv1 = 0.D0
               idp2 = 990
               pv2 = 0.D0
            ELSE IF ( i.EQ.3 ) THEN
               idp1 = IFPap(2)
               pv1 = PVIrt(2)
               idp2 = 990
               pv2 = 0.D0
            ELSE IF ( i.EQ.2 ) THEN
               idp1 = IFPap(1)
               pv1 = PVIrt(1)
               idp2 = 990
               pv2 = 0.D0
            ELSE
               idp1 = IFPap(1)
               pv1 = PVIrt(1)
               idp2 = IFPap(2)
               pv2 = PVIrt(2)
            END IF
 
C  initialize PT scales
            IF ( (IDPdg1.EQ.22) .OR. (IDPdg1.EQ.990) ) THEN
               IF ( (IDPdg2.EQ.22) .OR. (IDPdg2.EQ.990) ) THEN
                  FPS(i) = PARmdl(105)
                  FPH(i) = PARmdl(106)
               ELSE
                  FPS(i) = PARmdl(103)
                  FPH(i) = PARmdl(104)
               END IF
            ELSE IF ( (IDPdg2.EQ.22) .OR. (IDPdg2.EQ.990) ) THEN
               FPS(i) = PARmdl(103)
               FPH(i) = PARmdl(104)
            ELSE
               FPS(i) = PARmdl(101)
               FPH(i) = PARmdl(102)
            END IF
 
C  initialize hard scattering
            IF ( Ip.GT.0 ) THEN
               CALL PHO_HARINI(i,idp1,idp2,pv1,pv2,6,IDEb(8)+1)
            ELSE
               CALL PHO_HARINI(i,idp1,idp2,pv1,pv2,6,IDEb(8))
            END IF
 
C  energy/virtuality grid
            DO ie = 1 , IH_ecm_up(il,IDXmpar)
               HECm_tab(ie,il,IDXmpar) = EXP(ellow+delta*(ie-1))
            END DO
            DO ia = 1 , IH_q2a_up(il,IDXmpar)
               HQ2a_tab(ia,il,IDXmpar) = 0.D0
            END DO
            DO ib = 1 , IH_q2b_up(il,IDXmpar)
               HQ2b_tab(ib,il,IDXmpar) = 0.D0
            END DO
 
C  initialization for several energies and particle virtualities
            DO ie = 1 , IH_ecm_up(il,IDXmpar)
               DO ia = 1 , IH_q2a_up(il,IDXmpar)
                  DO ib = 1 , IH_q2b_up(il,IDXmpar)
 
                     ee = HECm_tab(ie,il,IDXmpar)
                     q2a = HQ2a_tab(ia,il,IDXmpar)
                     q2b = HQ2b_tab(ib,il,IDXmpar)
                     CALL PHO_HARINT(Ip,ee,0.D0,0.D0,0,-2,0,0,1)
                     IF ( LPRi.GT.4 .AND. IDEb(8).GE.5 )
     &                     WRITE (LO,'(1X,A,2E10.3,2I7)')
     &                     'PHO_HARMCI: initialization PT,ECM,ID1,ID2:'
     &                    , PTCut(i) , ee , IDPdg1 , IDPdg2
                     HFAc_tab(0,ie,ia,ib,il,IDXmpar) = PTCut(i)
 
                     CALL PHO_HARFAC(PTCut(i),ee)
                     CALL PHO_HARWGX(PTCut(i),ee)
C            Write(6,*) 'benchmark Harxto:'
C            call cpu_time(start)
                     CALL PHO_HARXTO(ee,PTCut(i),PTCut(i),dsig,dspt)
C            call cpu_time(finish)
C            print '("Time = ",f6.3," seconds.")',finish-start
 
                     IF ( IDEb(8).GE.10 ) THEN
                        IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,1X,A)') 
     &                'hard cross sections SIGH(mb),DSIG/DPT(mb/GeV**2)'
     &                , 
     &                '------------------------------------------------'
                        DO m = 0 , MAX_PRO_2
                           IF ( LPRi.GT.4 )
     &                        WRITE (LO,'(10X,A,1P2E17.9)') PROc(m) , 
     &                        DREAL(dsig(m)) , dspt(m)
                        END DO
                     END IF
 
C  store in interpolation tables
                     HFAc_tab(-1,ie,ia,ib,il,IDXmpar) = HFAc(-1,IDXmpar)
                     HWGx_tab(-1,ie,ia,ib,il,IDXmpar) = HWGx(-1,IDXmpar)
                     DO m = 0 , MAX_PRO_2
                        HFAc_tab(m,ie,ia,ib,il,IDXmpar)
     &                     = HFAc(m,IDXmpar)
                        HWGx_tab(m,ie,ia,ib,il,IDXmpar)
     &                     = HWGx(m,IDXmpar)
                        HSIg_tab(m,ie,ia,ib,il,IDXmpar) = DREAL(dsig(m))
     &                     *MH_pro_on(m,i,IDXmpar)
                        HDPt_tab(m,ie,ia,ib,il,IDXmpar) = dspt(m)
     &                     *MH_pro_on(m,i,IDXmpar)
                     END DO
 
C  summed quantities
                     HSIg_tab(9,ie,ia,ib,il,IDXmpar) = 0.D0
                     HDPt_tab(9,ie,ia,ib,il,IDXmpar) = 0.D0
                     DO m = 1 , 8
                        IF ( MH_pro_on(m,i,IDXmpar).GT.0 ) THEN
                           HSIg_tab(9,ie,ia,ib,il,IDXmpar)
     &                        = HSIg_tab(9,ie,ia,ib,il,IDXmpar)
     &                        + HSIg_tab(m,ie,ia,ib,il,IDXmpar)
                           HDPt_tab(9,ie,ia,ib,il,IDXmpar)
     &                        = HDPt_tab(9,ie,ia,ib,il,IDXmpar)
     &                        + HDPt_tab(m,ie,ia,ib,il,IDXmpar)
                        END IF
                     END DO
                     HSIg_tab(15,ie,ia,ib,il,IDXmpar) = 0.D0
                     HDPt_tab(15,ie,ia,ib,il,IDXmpar) = 0.D0
                     DO m = 10 , 14
                        IF ( MH_pro_on(m,i,IDXmpar).GT.0 ) THEN
                           HSIg_tab(15,ie,ia,ib,il,IDXmpar)
     &                        = HSIg_tab(15,ie,ia,ib,il,IDXmpar)
     &                        + HSIg_tab(m,ie,ia,ib,il,IDXmpar)
                           HDPt_tab(15,ie,ia,ib,il,IDXmpar)
     &                        = HDPt_tab(15,ie,ia,ib,il,IDXmpar)
     &                        + HDPt_tab(m,ie,ia,ib,il,IDXmpar)
                        END IF
                     END DO
                     HSIg_tab(0,ie,ia,ib,il,IDXmpar)
     &                  = HSIg_tab(9,ie,ia,ib,il,IDXmpar)
     &                  + HSIg_tab(15,ie,ia,ib,il,IDXmpar)
                     HDPt_tab(0,ie,ia,ib,il,IDXmpar)
     &                  = HDPt_tab(9,ie,ia,ib,il,IDXmpar)
     &                  + HDPt_tab(15,ie,ia,ib,il,IDXmpar)
 
                  END DO
               END DO
            END DO
         END IF
      END IF
 
C  debug output of weights
      IF ( IDEb(8).GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,5X,2I7,I3,F7.2,/1X,A)')
     &         'PHO_HARMCI: weights, maxima (ID1/2,IP,PTC)' , IDPdg1 , 
     &        IDPdg2 , Ip , PTCut(i) , 
     &        '------------------------------------------'
         DO m = -1 , MAX_PRO_2
            IF ( (m.NE.0) .AND. (m.NE.9) .AND. (m.NE.15) ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(2X,A,I3,2I7)') 
     &         'PHO_HARMCI: ECM Hfac, HWgx, HSig, Hdpt for MSTR,ID1,ID2'
     &         , m , IDPdg1 , IDPdg2
               DO k = 1 , IH_ecm_up(il,IDXmpar)
                  DO ia = 1 , IH_q2a_up(il,IDXmpar)
                     DO ib = 1 , IH_q2b_up(il,IDXmpar)
                        IF ( LPRi.GT.4 ) WRITE (LO,'(3X,1p,7E10.3)')
     &                       HECm_tab(k,il,IDXmpar) , 
     &                       HQ2a_tab(ia,il,IDXmpar) , 
     &                       HQ2b_tab(ib,il,IDXmpar) , 
     &                       HFAc_tab(m,k,ia,ib,il,IDXmpar) , 
     &                       HWGx_tab(m,k,ia,ib,il,IDXmpar) , 
     &                       HSIg_tab(m,k,ia,ib,il,IDXmpar) , 
     &                       HDPt_tab(m,k,ia,ib,il,IDXmpar)
                     END DO
                  END DO
               END DO
            END IF
         END DO
      END IF
 
      END SUBROUTINE
