
      SUBROUTINE PHO_HARREM(Jm1,Jm2,Igen,Ihpos,Ival,Indxs,Ic1,Ic2,Iused,
     &                      Irej)
C***********************************************************************
C
C     sample color structure for initial quark/gluon of hard scattering
C     and write hadron remnant to /POEVT1/
C
C     input:    JM1,2   index of mother particle in POEVT1
C               IGEN    mother particle production process
C               IHPOS   hard pomeron number
C               INDXH   index of hard parton
C                       positive for labels 1
C                       negative for labels 2
C               IVAL     1  hard valence parton
C                        0  hard sea parton connected by color flow with
C                           valence quarks
C                       -1  hard sea parton independent off valence
C                           quarks
C               INDXS   index of soft partons needed
C
C     output:   IC1,IC2 color label of initial parton
C               IUSED   number of soft X values used
C               IREJ    rejection flag
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , e1 , e2 , p1 , p2 , p3 , p4 , TINY
      INTEGER i , Ic1 , Ic2 , ica1 , ica2 , icb1 , icb2 , icc1 , icc2 , 
     &        ifl1 , ifl2 , Igen , ihp , Ihpos , ijh , indxh , Indxs , 
     &        IPHO_CNV1 , ipos , Irej
      INTEGER irem , Iused , Ival , ivfl1 , ivfl2 , ivsw , Jm1 , Jm2
      SAVE 
 
      PARAMETER (TINY=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  light-cone x fractions and c.m. momenta of soft cut string ends
      INCLUDE 'inc/posoft'
C  hard scattering data
      INCLUDE 'inc/pohslt'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  internal rejection counters
      INCLUDE 'inc/poloop'
 
      Irej = 0
 
      indxh = SIGN(LSIdx(ABS(Ihpos)),Ihpos)
 
      IF ( indxh.GT.0 ) THEN
         ijh = IPHO_CNV1(NINhd(indxh,1))
      ELSE
         ijh = IPHO_CNV1(NINhd(-indxh,2))
      END IF
C  direct process (photon or pomeron)
      Iused = 0
      Ic1 = 0
      Ic2 = 0
      IF ( (ijh.EQ.22) .OR. (ijh.EQ.990) ) RETURN
 
      ihp = 100*ABS(Ihpos)
      ivsw = 1
C**************************************
C     IF((IDHEP(JM1).EQ.22).OR.(IDHEP(JM1).EQ.990)) IVSW = 0
C**************************************
 
      IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &      WRITE (LO,'(1X,A,2I3,1X,5I4)')
     &      'PHO_HARREM: JM1,JM2,IHPOS,INDXH,IFLH,IVAL,INDXS:' , Jm1 , 
     &     Jm2 , Ihpos , indxh , ijh , Ival , Indxs
 
C  quark
C****************************************************************
 
      IF ( ijh.NE.21 ) THEN
 
C  valence quark engaged in hard scattering
         IF ( Ival.EQ.1 ) THEN
            CALL PHO_PARREM(Jm1,ijh,irem,Irej)
            IF ( Irej.NE.0 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,2I6)')
     &              'PHO_HARREM: ' , 
     &              'invalid valence flavour requested JM,IFLA' , Jm1 , 
     &              ijh
               RETURN
            END IF
            CALL PHO_SELCOL(0,0,ica1,ica2,icb1,icb2,1)
            IF ( ((ABS(irem).GT.6) .AND. (irem.GT.0)) .OR. 
     &           ((ABS(irem).LE.6) .AND. (irem.LT.0)) ) THEN
               i = ica1
               ica1 = icb1
               icb1 = i
            END IF
C  remnant of hadron
            IF ( indxh.GT.0 ) THEN
               p1 = PSOft1(1,Indxs)
               p2 = PSOft1(2,Indxs)
               p3 = PSOft1(3,Indxs)
               p4 = PSOft1(4,Indxs)
               IJSi1(Indxs) = irem
            ELSE
               p1 = PSOft2(1,Indxs)
               p2 = PSOft2(2,Indxs)
               p3 = PSOft2(3,Indxs)
               p4 = PSOft2(4,Indxs)
               IJSi2(Indxs) = irem
            END IF
C  registration
            CALL PHO_REGPAR(-1,irem,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,ica1,
     &                      ivsw,ipos,1)
            IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &           WRITE (LO,'(1X,A,3I5)')
     &            'PHO_HARREM: val.spectator:(IFL,IPOS,INDXS)' , irem , 
     &           ipos , SIGN(Indxs,indxh)
 
            Iused = 1
 
C  sea quark engaged in hard scattering, valence quarks treated
         ELSE IF ( Ival.EQ.0 ) THEN
            IF ( indxh.GT.0 ) THEN
               e1 = PSOft1(4,Indxs)
               e2 = PSOft1(4,Indxs+1)
            ELSE
               e1 = PSOft2(4,Indxs)
               e2 = PSOft2(4,Indxs+1)
            END IF
            CALL PHO_VALFLA(Jm1,ivfl1,ivfl2,e1,e2)
            CALL PHO_SELCOL(0,0,ica1,ica2,icb1,icb2,1)
            IF ( DT_RNDM(p1).LT.0.5D0 ) THEN
               CALL PHO_SELCOL(icb1,icb2,icb1,icb2,icc1,icc2,2)
            ELSE
               CALL PHO_SELCOL(ica1,ica2,ica1,ica2,icc1,icc2,2)
            END IF
            IF ( ((ABS(ivfl1).GT.6) .AND. (ivfl1.GT.0)) .OR. 
     &           ((ABS(ivfl1).LE.6) .AND. (ivfl1.LT.0)) ) THEN
               i = ica1
               ica1 = icb1
               icb1 = i
            END IF
            IF ( indxh.GT.0 ) THEN
               p1 = PSOft1(1,Indxs)
               p2 = PSOft1(2,Indxs)
               p3 = PSOft1(3,Indxs)
               p4 = PSOft1(4,Indxs)
               IJSi1(Indxs) = ivfl1
            ELSE
               p1 = PSOft2(1,Indxs)
               p2 = PSOft2(2,Indxs)
               p3 = PSOft2(3,Indxs)
               p4 = PSOft2(4,Indxs)
               IJSi2(Indxs) = ivfl1
            END IF
C  registration
            CALL PHO_REGPAR(-1,ivfl1,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,
     &                      ica1,ivsw,ipos,1)
            IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &           WRITE (LO,'(1X,A,3I5)')
     &            'PHO_HARREM: val.spectator:(IFL,IPOS,INDXS)' , ivfl1 , 
     &           ipos , SIGN(Indxs,indxh)
 
C
            IF ( indxh.GT.0 ) THEN
               p1 = PSOft1(1,Indxs+1)
               p2 = PSOft1(2,Indxs+1)
               p3 = PSOft1(3,Indxs+1)
               p4 = PSOft1(4,Indxs+1)
               IJSi1(Indxs+1) = ivfl2
            ELSE
               p1 = PSOft2(1,Indxs+1)
               p2 = PSOft2(2,Indxs+1)
               p3 = PSOft2(3,Indxs+1)
               p4 = PSOft2(4,Indxs+1)
               IJSi2(Indxs+1) = ivfl2
            END IF
C  registration
            CALL PHO_REGPAR(-1,ivfl2,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,
     &                      icb1,ivsw,ipos,1)
            IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &           WRITE (LO,'(1X,A,3I5)')
     &            'PHO_HARREM: val.spectator:(IFL,IPOS,INDXS)' , ivfl2 , 
     &           ipos , SIGN(Indxs+1,indxh)
 
C
            IF ( ijh.LT.0 ) THEN
               icb1 = icc2
               ica1 = icc1
            ELSE
               icb1 = icc1
               ica1 = icc2
            END IF
            IF ( indxh.GT.0 ) THEN
               p1 = PSOft1(1,Indxs+2)
               p2 = PSOft1(2,Indxs+2)
               p3 = PSOft1(3,Indxs+2)
               p4 = PSOft1(4,Indxs+2)
               IJSi1(Indxs+2) = -ijh
            ELSE
               p1 = PSOft2(1,Indxs+2)
               p2 = PSOft2(2,Indxs+2)
               p3 = PSOft2(3,Indxs+2)
               p4 = PSOft2(4,Indxs+2)
               IJSi2(Indxs+2) = -ijh
            END IF
C  registration
            CALL PHO_REGPAR(-1,-ijh,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,ica1,
     &                      0,ipos,1)
            IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &           WRITE (LO,'(1X,A,3I5)')
     &            'PHO_HARREM: sea spectator:(IFL,IPOS,INDXS)' , -ijh , 
     &           ipos , SIGN(Indxs+2,indxh)
            Iused = 3
C
C  sea quark engaged in hard scattering, valences treated separately
         ELSE IF ( Ival.EQ.-1 ) THEN
            CALL PHO_SELCOL(0,0,ica1,ica2,icb1,icb2,1)
            IF ( ijh.GT.0 ) THEN
               icc1 = icb1
               icb1 = ica1
               ica1 = icc1
            END IF
            IF ( indxh.GT.0 ) THEN
               p1 = PSOft1(1,Indxs)
               p2 = PSOft1(2,Indxs)
               p3 = PSOft1(3,Indxs)
               p4 = PSOft1(4,Indxs)
               IJSi1(Indxs) = -ijh
            ELSE
               p1 = PSOft2(1,Indxs)
               p2 = PSOft2(2,Indxs)
               p3 = PSOft2(3,Indxs)
               p4 = PSOft2(4,Indxs)
               IJSi2(Indxs) = -ijh
            END IF
C  registration
            CALL PHO_REGPAR(-1,-ijh,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,ica1,
     &                      0,ipos,1)
            IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &           WRITE (LO,'(1X,A,3I5)')
     &            'PHO_HARREM: sea spectator:(IFL,IPOS,INDXS)' , -ijh , 
     &           ipos , SIGN(Indxs,indxh)
 
            Iused = 1
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I5)')
     &            'PHO_HARREM:ERROR:unsupported combination of IVAL,IJH'
     &           , Ival , ijh
            CALL PHO_ABORT
         END IF
C
         Ic1 = icb1
         Ic2 = 0
C
C  gluon
C****************************************************************
C
C  gluon from valence quarks
      ELSE
         IF ( Ival.EQ.1 ) THEN
C  purely gluonic pomeron remnant
            IF ( (IDHep(Jm1).EQ.990) .AND. (IPAmdl(20).GT.0) ) THEN
               IF ( indxh.GT.0 ) THEN
                  p1 = PSOft1(1,Indxs) + PSOft1(1,Indxs+1)
                  p2 = PSOft1(2,Indxs) + PSOft1(2,Indxs+1)
                  p3 = PSOft1(3,Indxs) + PSOft1(3,Indxs+1)
                  p4 = PSOft1(4,Indxs) + PSOft1(4,Indxs+1)
                  IJSi1(Indxs) = 0
               ELSE
                  p1 = PSOft2(1,Indxs) + PSOft2(1,Indxs+1)
                  p2 = PSOft2(2,Indxs) + PSOft2(2,Indxs+1)
                  p3 = PSOft2(3,Indxs) + PSOft2(3,Indxs+1)
                  p4 = PSOft2(4,Indxs) + PSOft2(4,Indxs+1)
                  IJSi2(Indxs) = 0
               END IF
               ifl1 = 21
               CALL PHO_SELCOL(0,0,ica1,ica2,icb1,icb2,1)
               IF ( DT_RNDM(p2).LT.0.5D0 ) THEN
                  CALL PHO_SELCOL(ica1,ica2,ica1,ica2,icc1,icc2,2)
               ELSE
                  CALL PHO_SELCOL(icb1,icb2,icb1,icb2,icc1,icc2,2)
               END IF
C  registration
               CALL PHO_REGPAR(-1,ifl1,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,
     &            ica1,icb1,ipos,1)
 
               IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &               WRITE (LO,'(1X,A,3I5)')
     &               'PHO_HARREM: val.gluon:(IFL,IPOS,INDXS)' , ifl1 , 
     &              ipos , SIGN(Indxs,indxh)
               Iused = 2
C  valence quark remnant
            ELSE
               IF ( indxh.GT.0 ) THEN
                  e1 = PSOft1(4,Indxs)
                  e2 = PSOft1(4,Indxs+1)
               ELSE
                  e1 = PSOft2(4,Indxs)
                  e2 = PSOft2(4,Indxs+1)
               END IF
               CALL PHO_VALFLA(Jm1,ifl1,ifl2,e1,e2)
               CALL PHO_SELCOL(0,0,ica1,ica2,icb1,icb2,1)
               IF ( ((ABS(ifl1).LE.6) .AND. (ifl1.LT.0)) .OR. 
     &              ((ABS(ifl1).GT.6) .AND. (ifl1.GT.0)) ) THEN
                  i = ica1
                  ica1 = icb1
                  icb1 = i
               END IF
               IF ( DT_RNDM(p2).LT.0.5D0 ) THEN
                  CALL PHO_SELCOL(ica1,ica2,ica1,ica2,icc1,icc2,2)
               ELSE
                  CALL PHO_SELCOL(icb1,icb2,icb1,icb2,icc1,icc2,2)
               END IF
C  remnant of hadron
               IF ( indxh.GT.0 ) THEN
                  p1 = PSOft1(1,Indxs)
                  p2 = PSOft1(2,Indxs)
                  p3 = PSOft1(3,Indxs)
                  p4 = PSOft1(4,Indxs)
                  IJSi1(Indxs) = ifl1
               ELSE
                  p1 = PSOft2(1,Indxs)
                  p2 = PSOft2(2,Indxs)
                  p3 = PSOft2(3,Indxs)
                  p4 = PSOft2(4,Indxs)
                  IJSi2(Indxs) = ifl1
               END IF
C  registration
               CALL PHO_REGPAR(-1,ifl1,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,
     &            ica1,ivsw,ipos,1)
 
C
               IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &               WRITE (LO,'(1X,A,3I5)')
     &               'PHO_HARREM: val.spectator:(IFL,IPOS,INDXS)' , 
     &              ifl1 , ipos , SIGN(Indxs,indxh)
               IF ( indxh.GT.0 ) THEN
                  p1 = PSOft1(1,Indxs+1)
                  p2 = PSOft1(2,Indxs+1)
                  p3 = PSOft1(3,Indxs+1)
                  p4 = PSOft1(4,Indxs+1)
                  IJSi1(Indxs+1) = ifl2
               ELSE
                  p1 = PSOft2(1,Indxs+1)
                  p2 = PSOft2(2,Indxs+1)
                  p3 = PSOft2(3,Indxs+1)
                  p4 = PSOft2(4,Indxs+1)
                  IJSi2(Indxs+1) = ifl2
               END IF
C  registration
               CALL PHO_REGPAR(-1,ifl2,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,
     &            icb1,ivsw,ipos,1)
 
               IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &               WRITE (LO,'(1X,A,3I5)')
     &               'PHO_HARREM: val.spectator:(IFL,IPOS,INDXS)' , 
     &              ifl2 , ipos , SIGN(Indxs+1,indxh)
               Iused = 2
            END IF
C
C  gluon from sea quarks connected with valence quarks
         ELSE IF ( Ival.EQ.0 ) THEN
            IF ( indxh.GT.0 ) THEN
               e1 = PSOft1(4,Indxs)
               e2 = PSOft1(4,Indxs+1)
            ELSE
               e1 = PSOft2(4,Indxs)
               e2 = PSOft2(4,Indxs+1)
            END IF
            CALL PHO_VALFLA(Jm1,ifl1,ifl2,e1,e2)
            CALL PHO_SELCOL(0,0,ica1,ica2,icb1,icb2,1)
            IF ( ((ABS(ifl1).LE.6) .AND. (ifl1.LT.0)) .OR. 
     &           ((ABS(ifl1).GT.6) .AND. (ifl1.GT.0)) ) THEN
               i = ica1
               ica1 = icb1
               icb1 = i
            END IF
            IF ( DT_RNDM(p3).LT.0.5D0 ) THEN
               CALL PHO_SELCOL(ica1,ica2,ica1,ica2,icc1,icc2,2)
            ELSE
               CALL PHO_SELCOL(icb1,icb2,icb1,icb2,icc1,icc2,2)
            END IF
C  remnant of hadron
            IF ( indxh.GT.0 ) THEN
               p1 = PSOft1(1,Indxs)
               p2 = PSOft1(2,Indxs)
               p3 = PSOft1(3,Indxs)
               p4 = PSOft1(4,Indxs)
               IJSi1(Indxs) = ifl1
            ELSE
               p1 = PSOft2(1,Indxs)
               p2 = PSOft2(2,Indxs)
               p3 = PSOft2(3,Indxs)
               p4 = PSOft2(4,Indxs)
               IJSi2(Indxs) = ifl1
            END IF
C  registration
            CALL PHO_REGPAR(-1,ifl1,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,ica1,
     &                      ivsw,ipos,1)
 
C
            IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &           WRITE (LO,'(1X,A,3I5)')
     &            'PHO_HARREM: val.spectator:(IFL,IPOS,INDXS)' , ifl1 , 
     &           ipos , SIGN(Indxs,indxh)
            IF ( indxh.GT.0 ) THEN
               p1 = PSOft1(1,Indxs+1)
               p2 = PSOft1(2,Indxs+1)
               p3 = PSOft1(3,Indxs+1)
               p4 = PSOft1(4,Indxs+1)
               IJSi1(Indxs+1) = ifl2
            ELSE
               p1 = PSOft2(1,Indxs+1)
               p2 = PSOft2(2,Indxs+1)
               p3 = PSOft2(3,Indxs+1)
               p4 = PSOft2(4,Indxs+1)
               IJSi2(Indxs+1) = ifl2
            END IF
C  registration
            CALL PHO_REGPAR(-1,ifl2,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,icb1,
     &                      ivsw,ipos,1)
 
            IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &           WRITE (LO,'(1X,A,3I5)')
     &            'PHO_HARREM: val.spectator:(IFL,IPOS,INDXS)' , ifl2 , 
     &           ipos , SIGN(Indxs+1,indxh)
            IF ( IPAmdl(18).EQ.0 ) THEN
C  sea quark pair
               CALL PHO_SEAFLA(Jm1,ifl1,ifl2,PARmdl(161))
               IF ( icc1.GT.0 ) THEN
                  ifl1 = ABS(ifl1)
                  ifl2 = -ifl1
               ELSE
                  ifl1 = -ABS(ifl1)
                  ifl2 = -ifl1
               END IF
               IF ( DT_RNDM(p4).LT.0.5D0 ) THEN
                  icb1 = icc2
                  CALL PHO_SELCOL(icc1,0,ica1,ica2,icc1,icc2,2)
               ELSE
                  ica1 = icc1
                  CALL PHO_SELCOL(icc2,0,icb1,icb2,icc1,icc2,2)
               END IF
               IF ( indxh.GT.0 ) THEN
                  p1 = PSOft1(1,Indxs+2)
                  p2 = PSOft1(2,Indxs+2)
                  p3 = PSOft1(3,Indxs+2)
                  p4 = PSOft1(4,Indxs+2)
                  IJSi1(Indxs+2) = ifl1
               ELSE
                  p1 = PSOft2(1,Indxs+2)
                  p2 = PSOft2(2,Indxs+2)
                  p3 = PSOft2(3,Indxs+2)
                  p4 = PSOft2(4,Indxs+2)
                  IJSi2(Indxs+2) = ifl1
               END IF
C  registration
               CALL PHO_REGPAR(-1,ifl1,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,
     &            ica1,0,ipos,1)
 
C
               IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &               WRITE (LO,'(1X,A,3I5)')
     &               'PHO_HARREM: sea spectator:(IFL,IPOS,INDXS)' , 
     &              ifl1 , ipos , SIGN(Indxs+2,indxh)
               IF ( indxh.GT.0 ) THEN
                  p1 = PSOft1(1,Indxs+3)
                  p2 = PSOft1(2,Indxs+3)
                  p3 = PSOft1(3,Indxs+3)
                  p4 = PSOft1(4,Indxs+3)
                  IJSi1(Indxs+3) = ifl2
               ELSE
                  p1 = PSOft2(1,Indxs+3)
                  p2 = PSOft2(2,Indxs+3)
                  p3 = PSOft2(3,Indxs+3)
                  p4 = PSOft2(4,Indxs+3)
                  IJSi2(Indxs+3) = ifl2
               END IF
C  registration
               CALL PHO_REGPAR(-1,ifl2,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,
     &            icb1,0,ipos,1)
 
               IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &               WRITE (LO,'(1X,A,3I5)')
     &               'PHO_HARREM: sea spectator:(IFL,IPOS,INDXS)' , 
     &              ifl2 , ipos , SIGN(Indxs+3,indxh)
               Iused = 4
            ELSE
               Iused = 2
            END IF
C
C  gluon from independent sea quarks
         ELSE IF ( Ival.EQ.-1 ) THEN
            IF ( IPAmdl(18).EQ.0 ) THEN
               CALL PHO_SELCOL(0,0,ica1,ica2,icb1,icb2,1)
               CALL PHO_SEAFLA(Jm1,ifl1,ifl2,PARmdl(161))
               IF ( ((ABS(ifl1).LE.6) .AND. (ifl1.LT.0)) .OR. 
     &              ((ABS(ifl1).GT.6) .AND. (ifl1.GT.0)) ) THEN
                  i = ica1
                  ica1 = icb1
                  icb1 = i
               END IF
               IF ( DT_RNDM(p1).LT.0.5D0 ) THEN
                  CALL PHO_SELCOL(ica1,ica2,ica1,ica2,icc1,icc2,2)
               ELSE
                  CALL PHO_SELCOL(icb1,icb2,icb1,icb2,icc1,icc2,2)
               END IF
C  remainder of hadron
               IF ( indxh.GT.0 ) THEN
                  p1 = PSOft1(1,Indxs)
                  p2 = PSOft1(2,Indxs)
                  p3 = PSOft1(3,Indxs)
                  p4 = PSOft1(4,Indxs)
                  IJSi1(Indxs) = ifl1
               ELSE
                  p1 = PSOft2(1,Indxs)
                  p2 = PSOft2(2,Indxs)
                  p3 = PSOft2(3,Indxs)
                  p4 = PSOft2(4,Indxs)
                  IJSi2(Indxs) = ifl1
               END IF
C  registration
               CALL PHO_REGPAR(-1,ifl1,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,
     &            ica1,ica2,ipos,1)
 
C  remnant of sea
               IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &               WRITE (LO,'(1X,A,3I5)')
     &               'PHO_HARREM: sea spectator:(IFL,IPOS,INDXS)' , 
     &              ifl1 , ipos , SIGN(Indxs,indxh)
               IF ( indxh.GT.0 ) THEN
                  p1 = PSOft1(1,Indxs-1)
                  p2 = PSOft1(2,Indxs-1)
                  p3 = PSOft1(3,Indxs-1)
                  p4 = PSOft1(4,Indxs-1)
                  IJSi1(Indxs-1) = ifl2
               ELSE
                  p1 = PSOft2(1,Indxs-1)
                  p2 = PSOft2(2,Indxs-1)
                  p3 = PSOft2(3,Indxs-1)
                  p4 = PSOft2(4,Indxs-1)
                  IJSi2(Indxs-1) = ifl2
               END IF
C  registration
               CALL PHO_REGPAR(-1,ifl2,0,Jm1,Jm2,p1,p2,p3,p4,ihp,Igen,
     &            icb1,icb2,ipos,1)
 
               IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &               WRITE (LO,'(1X,A,3I5)')
     &               'PHO_HARREM: sea spectator:(IFL,IPOS,INDXS)' , 
     &              ifl2 , ipos , SIGN(Indxs-1,indxh)
               Iused = 2
            ELSE
               CALL PHO_SELCOL(0,0,icc1,ica2,icc2,icb2,1)
               IF ( LPRi.GT.4 .AND. IDEb(28).GE.20 )
     &               WRITE (LO,'(1X,A,I5)')
     &               'PHO_HARREM: no spectator added:(INDXS)' , 
     &              SIGN(Indxs,indxh)
               Iused = 0
            END IF
C
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I5)') 
     &           'PHO_HARREM:ERROR: unsupported combination of IVAL,IJH'
     &           , Ival , ijh
            CALL PHO_ABORT
         END IF
         Ic1 = icc1
         Ic2 = icc2
      END IF
      END SUBROUTINE
