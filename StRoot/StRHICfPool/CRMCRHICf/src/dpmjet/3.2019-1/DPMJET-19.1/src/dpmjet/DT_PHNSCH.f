
      DOUBLE PRECISION FUNCTION DT_PHNSCH(Kp,Ktarg,Plab)
 
C***********************************************************************
C                                                                      *
C     Probability for Hadron Nucleon Single CHain interactions:        *
C                                                                      *
C     Created on 30 december 1993  by    Alfredo Ferrari & Paola Sala  *
C                                                   Infn - Milan       *
C                                                                      *
C     Last change on 04-jan-94     by    Alfredo Ferrari               *
C                                                                      *
C             modified by J.R.for use in DTUNUC  6.1.94                *
C                                                                      *
C     Input variables:                                                 *
C                      Kp = hadron projectile index (Part numbering    *
C                           scheme)                                    *
C                   Ktarg = target nucleon index (1=proton, 8=neutron) *
C                    Plab = projectile laboratory momentum (GeV/c)     *
C     Output variable:                                                 *
C                  Phnsch = probability per single chain (particle     *
C                           exchange) interactions                     *
C                                                                      *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION acof , algpla , am , ampsq , amtsq , bcof , 
     &                 ccof , dcof , DT_RNDM , DT_SCHQUA , enne , 
     &                 eproj , FIVFIV , HLFHLF , ONEONE , ONEPLS , 
     &                 pddbar , pla , Plab , puubar
      DOUBLE PRECISION rnchen , sappel , sappin , sapptt , sgtco1 , 
     &                 sgtco2 , sgtco3 , sgtcoe , shncel , shncin , 
     &                 shnctt , sigdia , skmpel , skmpin , skmptt , 
     &                 skppel , skppin , skpptt , spmpel , spmpin
      DOUBLE PRECISION spmptt , sppela , sppine , spppel , spppin , 
     &                 sppptt , spptot , TWOTWO , umo50 , umorat , 
     &                 umosq , ZERZER
      INTEGER ichrge , IDMAXP , ihlp , ip , iqbchc , iqbsc1 , iqbsc2 , 
     &        iqechc , iqfsc1 , iqfsc2 , iqschc , iqspro , Jqbsc1 , 
     &        Jqbsc2 , Jqfsc1 , Jqfsc2 , jreac , k2hlp , khelp , Kp
      INTEGER kptoip , Ktarg , LUNERR , LUNOUT , NALLWP , ndiagr
      SAVE 
 
      PARAMETER (LUNOUT=6)
      PARAMETER (LUNERR=6)
      PARAMETER (ONEPLS=1.000000000000001D+00)
      PARAMETER (ZERZER=0.D+00)
      PARAMETER (ONEONE=1.D+00)
      PARAMETER (TWOTWO=2.D+00)
      PARAMETER (FIVFIV=5.D+00)
      PARAMETER (HLFHLF=0.5D+00)
 
      PARAMETER (NALLWP=39)
      PARAMETER (IDMAXP=210)
 
      DIMENSION ichrge(39) , am(39)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
 
      DIMENSION kptoip(210)
C auxiliary common for reggeon exchange (DTUNUC 1.x)
      INCLUDE 'inc/dtquar'
 
      DIMENSION sgtcoe(5,33) , ihlp(NALLWP)
      DIMENSION sgtco1(5,10) , sgtco2(5,8) , sgtco3(5,15)
      EQUIVALENCE (sgtco1(1,1),sgtcoe(1,1))
      EQUIVALENCE (sgtco2(1,1),sgtcoe(1,11))
      EQUIVALENCE (sgtco3(1,1),sgtcoe(1,19))
 
C Conversion from part to paprop numbering
      DATA kptoip/1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 
     &     13 , 14 , 15 , 16 , 17 , 18 , 19 , 20 , 21 , 22 , 23 , 24 , 
     &     25 , 26 , 27 , 28 , 29 , 30 , 66*0 , 34 , 36 , 31 , 32 , 33 , 
     &     35 , 37 , 5*0 , 38 , 5*0 , 39 , 19*0 , 27 , 28 , 74*0/
 
C  1=baryon, 2=pion, 3=kaon, 4=antibaryon:
      DATA ihlp/1 , 4 , 5*0 , 1 , 4 , 2*0 , 3 , 2*2 , 2*3 , 1 , 4 , 3 , 
     &     3*1 , 2 , 2*3 , 2 , 4*0 , 3*4 , 1 , 4 , 1 , 4 , 1 , 4/
C     DATA ( ( SGTCOE (J,I), J=1,5 ), I=1,10 ) /
C 1st reaction: gamma p total
C 2nd reaction: gamma d total
C 3rd reaction: pi+ p total
C 4th reaction: pi- p total
C 5th reaction: pi+/- d total
C 6th reaction: K+ p total
C 7th reaction: K+ n total
C 8th reaction: K+ d total
C 9th reaction: K- p total
C 10th reaction: K- n total
      DATA sgtco1/0.147D+00 , ZERZER , ZERZER , 0.0022D+00 , 
     &     -0.0170D+00 , 0.300D+00 , ZERZER , ZERZER , 0.0095D+00 , 
     &     -0.057D+00 , 16.4D+00 , 19.3D+00 , -0.42D+00 , 0.19D+00 , 
     &     ZERZER , 33.0D+00 , 14.0D+00 , -1.36D+00 , 0.456D+00 , 
     &     -4.03D+00 , 56.8D+00 , 42.2D+00 , -1.45D+00 , 0.65D+00 , 
     &     -5.39D+00 , 18.1D+00 , ZERZER , ZERZER , 0.26D+00 , 
     &     -1.0D+00 , 18.7D+00 , ZERZER , ZERZER , 0.21D+00 , 
     &     -0.89D+00 , 34.2D+00 , 7.9D+00 , -2.1D+00 , 0.346D+00 , 
     &     -0.99D+00 , 32.1D+00 , ZERZER , ZERZER , 0.66D+00 , 
     &     -5.6D+00 , 25.2D+00 , ZERZER , ZERZER , 0.38D+00 , -2.9D+00/
C     DATA ( ( SGTCOE (J,I), J=1,5 ), I=11,18 ) /
C 11th reaction: K- d total
C 12th reaction: p p total
C 13th reaction: p n total
C 14th reaction: p d total
C 15th reaction: pbar p total
C 16th reaction: pbar n total
C 17th reaction: pbar d total
C 18th reaction: Lamda p total
      DATA sgtco2/57.6D+00 , ZERZER , ZERZER , 1.17D+00 , -9.5D+00 , 
     &     48.0D+00 , ZERZER , ZERZER , 0.522D+00 , -4.51D+00 , 
     &     47.30D+00 , ZERZER , ZERZER , 0.513D+00 , -4.27D+00 , 
     &     91.3D+00 , ZERZER , ZERZER , 1.05D+00 , -8.8D+00 , 38.4D+00 , 
     &     77.6D+00 , -0.64D+00 , 0.26D+00 , -1.2D+00 , ZERZER , 
     &     133.6D+00 , -0.70D+00 , -1.22D+00 , 13.7D+00 , 112.D+00 , 
     &     125.D+00 , -1.08D+00 , 1.14D+00 , -12.4D+00 , 30.4D+00 , 
     &     ZERZER , ZERZER , ZERZER , 1.6D+00/
C     DATA ( ( SGTCOE (J,I), J=1,5 ), I=19,33 ) /
C 19th reaction: pi+ p elastic
C 20th reaction: pi- p elastic
C 21st reaction: K+ p elastic
C 22nd reaction: K- p elastic
C 23rd reaction: p p elastic
C 24th reaction: p d elastic
C 25th reaction: pbar p elastic
C 26th reaction: pbar p elastic bis
C 27th reaction: pbar n elastic
C 28th reaction: Lamda p elastic
C 29th reaction: K- p ela bis
C 30th reaction: pi- p cx
C 31st reaction: K- p cx
C 32nd reaction: K+ n cx
C 33rd reaction: pbar p cx
      DATA sgtco3/ZERZER , 11.4D+00 , -0.4D+00 , 0.079D+00 , ZERZER , 
     &     1.76D+00 , 11.2D+00 , -0.64D+00 , 0.043D+00 , ZERZER , 
     &     5.0D+00 , 8.1D+00 , -1.8D+00 , 0.16D+00 , -1.3D+00 , 
     &     7.3D+00 , ZERZER , ZERZER , 0.29D+00 , -2.40D+00 , 11.9D+00 , 
     &     26.9D+00 , -1.21D+00 , 0.169D+00 , -1.85D+00 , 16.1D+00 , 
     &     ZERZER , ZERZER , 0.32D+00 , -3.4D+00 , 10.2D+00 , 52.7D+00 , 
     &     -1.16D+00 , 0.125D+00 , -1.28D+00 , 10.6D+00 , 53.1D+00 , 
     &     -1.19D+00 , 0.136D+00 , -1.41D+00 , 36.5D+00 , ZERZER , 
     &     ZERZER , ZERZER , -11.9D+00 , 12.3D+00 , ZERZER , ZERZER , 
     &     ZERZER , -2.4D+00 , 7.24D+00 , 46.0D+00 , -4.71D+00 , 
     &     0.279D+00 , -2.35D+00 , ZERZER , 0.912D+00 , -1.22D+00 , 
     &     ZERZER , ZERZER , ZERZER , 3.39D+00 , -1.75D+00 , ZERZER , 
     &     ZERZER , ZERZER , 7.18D+00 , -2.01D+00 , ZERZER , ZERZER , 
     &     ZERZER , 18.8D+00 , -2.01D+00 , ZERZER , ZERZER/
C
C  +-------------------------------------------------------------------*
      ichrge(Ktarg) = IICh(Ktarg)
      am(Ktarg) = AAM(Ktarg)
C  |  Check for pi0 (d-dbar)
      IF ( Kp.NE.26 ) THEN
         ip = kptoip(Kp)
         IF ( ip.EQ.0 ) ip = 1
         ichrge(ip) = IICh(Kp)
         am(ip) = AAM(Kp)
C  |
C  +-------------------------------------------------------------------*
C  |
      ELSE
         ip = 23
         ichrge(ip) = 0
C  |
C  +-------------------------------------------------------------------*
C  +-------------------------------------------------------------------*
C  |  No such interactions for baryon-baryon
      END IF
      IF ( IIBar(Kp).GT.0 ) THEN
         DT_PHNSCH = ZERZER
         RETURN
C  |
C  +-------------------------------------------------------------------*
C  |  No "annihilation" diagram possible for K+ p/n
      ELSE IF ( ip.EQ.15 ) THEN
         DT_PHNSCH = ZERZER
         RETURN
C  |
C  +-------------------------------------------------------------------*
C  |  No "annihilation" diagram possible for K0 p/n
      ELSE IF ( ip.EQ.24 ) THEN
         DT_PHNSCH = ZERZER
         RETURN
C  |
C  +-------------------------------------------------------------------*
C  |  No "annihilation" diagram possible for Omebar p/n
      ELSE IF ( ip.GE.38 ) THEN
         DT_PHNSCH = ZERZER
         RETURN
C  |
C  +-------------------------------------------------------------------*
C  +-------------------------------------------------------------------*
C  |  If the momentum is larger than 50 GeV/c, compute the single
C  |  chain probability at 50 GeV/c and extrapolate to the present
C  |  momentum according to 1/sqrt(s)
C  |  sigma = sigma_sch (50) * sqrt (s(50)/s) + sigma_dch
C  |  P_sch (50) = sigma_sch (50) / ( sigma_dch + sigma_sch (50) )
C  |  sigma_dch / sigma_sch (50) = 1 / P_sch (50) - 1
C  |  sigma_dch / sigma_sch = 1 / P_sch - 1 = ( 1 / P_sch (50) - 1 )
C  |                        x sqrt(s/s(50))
C  |  P_sch = 1 / [ ( 1 / P_sch (50) - 1 ) x sqrt(s/s(50)) + 1 ]
      END IF
      IF ( Plab.GT.50.D+00 ) THEN
         pla = 50.D+00
         ampsq = am(ip)**2
         amtsq = am(Ktarg)**2
         eproj = SQRT(Plab**2+ampsq)
         umosq = ampsq + amtsq + TWOTWO*am(Ktarg)*eproj
         eproj = SQRT(pla**2+ampsq)
         umo50 = ampsq + amtsq + TWOTWO*am(Ktarg)*eproj
         umorat = SQRT(umosq/umo50)
C  |
C  +-------------------------------------------------------------------*
C  |  P < 3 GeV/c
      ELSE IF ( Plab.LT.3.D+00 ) THEN
         pla = 3.D+00
         ampsq = am(ip)**2
         amtsq = am(Ktarg)**2
         eproj = SQRT(Plab**2+ampsq)
         umosq = ampsq + amtsq + TWOTWO*am(Ktarg)*eproj
         eproj = SQRT(pla**2+ampsq)
         umo50 = ampsq + amtsq + TWOTWO*am(Ktarg)*eproj
         umorat = SQRT(umosq/umo50)
C  |
C  +-------------------------------------------------------------------*
C  |  P < 50 GeV/c
      ELSE
         pla = Plab
         umorat = ONEONE
C  |
C  +-------------------------------------------------------------------*
      END IF
      algpla = LOG(pla)
C  +-------------------------------------------------------------------*
C  |  Pions:
      IF ( ihlp(ip).EQ.2 ) THEN
         acof = sgtcoe(1,3)
         bcof = sgtcoe(2,3)
         enne = sgtcoe(3,3)
         ccof = sgtcoe(4,3)
         dcof = sgtcoe(5,3)
C  |  Compute the pi+ p total cross section:
         sppptt = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
         acof = sgtcoe(1,19)
         bcof = sgtcoe(2,19)
         enne = sgtcoe(3,19)
         ccof = sgtcoe(4,19)
         dcof = sgtcoe(5,19)
C  |  Compute the pi+ p elastic cross section:
         spppel = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
C  |  Compute the pi+ p inelastic cross section:
         spppin = sppptt - spppel
         acof = sgtcoe(1,4)
         bcof = sgtcoe(2,4)
         enne = sgtcoe(3,4)
         ccof = sgtcoe(4,4)
         dcof = sgtcoe(5,4)
C  |  Compute the pi- p total cross section:
         spmptt = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
         acof = sgtcoe(1,20)
         bcof = sgtcoe(2,20)
         enne = sgtcoe(3,20)
         ccof = sgtcoe(4,20)
         dcof = sgtcoe(5,20)
C  |  Compute the pi- p elastic cross section:
         spmpel = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
C  |  Compute the pi- p inelastic cross section:
         spmpin = spmptt - spmpel
         sigdia = spmpin - spppin
C  |  +----------------------------------------------------------------*
C  |  |  Charged pions: besides isospin consideration it is supposed
C  |  |                 that (pi+ n)el is almost equal to (pi- p)el
C  |  |                 and  (pi+ p)el "    "     "    "  (pi- n)el
C  |  |                 and all are almost equal among each others
C  |  |                 (reasonable above 5 GeV/c)
         IF ( ichrge(ip).NE.0 ) THEN
            khelp = Ktarg/8
            jreac = 3 + ip - 13 + ichrge(ip)*khelp
            acof = sgtcoe(1,jreac)
            bcof = sgtcoe(2,jreac)
            enne = sgtcoe(3,jreac)
            ccof = sgtcoe(4,jreac)
            dcof = sgtcoe(5,jreac)
C  |  |  Compute the total cross section:
            shnctt = acof + bcof*pla**enne + ccof*algpla**2 + 
     &               dcof*algpla
            jreac = 19 + ip - 13 + ichrge(ip)*khelp
            acof = sgtcoe(1,jreac)
            bcof = sgtcoe(2,jreac)
            enne = sgtcoe(3,jreac)
            ccof = sgtcoe(4,jreac)
            dcof = sgtcoe(5,jreac)
C  |  |  Compute the elastic cross section:
            shncel = acof + bcof*pla**enne + ccof*algpla**2 + 
     &               dcof*algpla
C  |  |  Compute the inelastic cross section:
            shncin = shnctt - shncel
C  |  |  Number of diagrams:
            ndiagr = 1 + ip - 13 + ichrge(ip)*khelp
C  |  |  Now compute the chain end (anti)quark-(anti)diquark
            iqfsc1 = 1 + ip - 13
            iqfsc2 = 0
            iqbsc1 = 1 + khelp
            iqbsc2 = 1 + ip - 13
C  |  |
C  |  +----------------------------------------------------------------*
C  |  |  pi0: besides isospin consideration it is supposed that the
C  |  |       elastic cross section is not very different from
C  |  |       pi+ p and/or pi- p (reasonable above 5 GeV/c)
         ELSE
            khelp = Ktarg/8
            k2hlp = (Kp-23)/3
C  |  |  Number of diagrams:
C  |  |  For u ubar (k2hlp=0):
C           NDIAGR = 2 - KHELP
C  |  |  For d dbar (k2hlp=1):
C           NDIAGR = 2 + KHELP - K2HLP
            ndiagr = 2 + khelp*(2*k2hlp-1) - k2hlp
            shncin = HLFHLF*(spppin+spmpin)
C  |  |  Now compute the chain end (anti)quark-(anti)diquark
            iqfsc1 = 1 + k2hlp
            iqfsc2 = 0
            iqbsc1 = 1 + khelp
            iqbsc2 = 2 - k2hlp
C  |  |
C  |  +----------------------------------------------------------------*
C  |                                                   end pi's
C  +-------------------------------------------------------------------*
C  |  Kaons:
         END IF
      ELSE IF ( ihlp(ip).EQ.3 ) THEN
         acof = sgtcoe(1,6)
         bcof = sgtcoe(2,6)
         enne = sgtcoe(3,6)
         ccof = sgtcoe(4,6)
         dcof = sgtcoe(5,6)
C  |  Compute the K+ p total cross section:
         skpptt = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
         acof = sgtcoe(1,21)
         bcof = sgtcoe(2,21)
         enne = sgtcoe(3,21)
         ccof = sgtcoe(4,21)
         dcof = sgtcoe(5,21)
C  |  Compute the K+ p elastic cross section:
         skppel = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
C  |  Compute the K+ p inelastic cross section:
         skppin = skpptt - skppel
         acof = sgtcoe(1,9)
         bcof = sgtcoe(2,9)
         enne = sgtcoe(3,9)
         ccof = sgtcoe(4,9)
         dcof = sgtcoe(5,9)
C  |  Compute the K- p total cross section:
         skmptt = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
         acof = sgtcoe(1,22)
         bcof = sgtcoe(2,22)
         enne = sgtcoe(3,22)
         ccof = sgtcoe(4,22)
         dcof = sgtcoe(5,22)
C  |  Compute the K- p elastic cross section:
         skmpel = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
C  |  Compute the K- p inelastic cross section:
         skmpin = skmptt - skmpel
         sigdia = HLFHLF*(skmpin-skppin)
C  |  +----------------------------------------------------------------*
C  |  |  Charged Kaons: actually only K-
         IF ( ichrge(ip).NE.0 ) THEN
            khelp = Ktarg/8
C  |  |  +-------------------------------------------------------------*
C  |  |  |  Proton target:
            IF ( khelp.EQ.0 ) THEN
               shncin = skmpin
C  |  |  |  Number of diagrams:
               ndiagr = 2
C  |  |  |
C  |  |  +-------------------------------------------------------------*
C  |  |  |  Neutron target: besides isospin consideration it is supposed
C  |  |  |              that (K- n)el is almost equal to (K- p)el
C  |  |  |              (reasonable above 5 GeV/c)
            ELSE
               acof = sgtcoe(1,10)
               bcof = sgtcoe(2,10)
               enne = sgtcoe(3,10)
               ccof = sgtcoe(4,10)
               dcof = sgtcoe(5,10)
C  |  |  |  Compute the total cross section:
               shnctt = acof + bcof*pla**enne + ccof*algpla**2 + 
     &                  dcof*algpla
C  |  |  |  Compute the elastic cross section:
               shncel = skmpel
C  |  |  |  Compute the inelastic cross section:
               shncin = shnctt - shncel
C  |  |  |  Number of diagrams:
               ndiagr = 1
C  |  |  |
C  |  |  +-------------------------------------------------------------*
C  |  |  Now compute the chain end (anti)quark-(anti)diquark
            END IF
            iqfsc1 = 3
            iqfsc2 = 0
            iqbsc1 = 1 + khelp
            iqbsc2 = 2
C  |  |
C  |  +----------------------------------------------------------------*
C  |  |  K0's: (actually only K0bar)
         ELSE
            khelp = Ktarg/8
C  |  |  +-------------------------------------------------------------*
C  |  |  |  Proton target: (K0bar p)in supposed to be given by
C  |  |  |                 (K- p)in - Sig_diagr
            IF ( khelp.EQ.0 ) THEN
               shncin = skmpin - sigdia
C  |  |  |  Number of diagrams:
               ndiagr = 1
C  |  |  |
C  |  |  +-------------------------------------------------------------*
C  |  |  |  Neutron target: (K0bar n)in supposed to be given by
C  |  |  |                 (K- n)in + Sig_diagr
C  |  |  |              besides isospin consideration it is supposed
C  |  |  |              that (K- n)el is almost equal to (K- p)el
C  |  |  |              (reasonable above 5 GeV/c)
            ELSE
               acof = sgtcoe(1,10)
               bcof = sgtcoe(2,10)
               enne = sgtcoe(3,10)
               ccof = sgtcoe(4,10)
               dcof = sgtcoe(5,10)
C  |  |  |  Compute the total cross section:
               shnctt = acof + bcof*pla**enne + ccof*algpla**2 + 
     &                  dcof*algpla
C  |  |  |  Compute the elastic cross section:
               shncel = skmpel
C  |  |  |  Compute the inelastic cross section:
               shncin = shnctt - shncel + sigdia
C  |  |  |  Number of diagrams:
               ndiagr = 2
C  |  |  |
C  |  |  +-------------------------------------------------------------*
C  |  |  Now compute the chain end (anti)quark-(anti)diquark
            END IF
            iqfsc1 = 3
            iqfsc2 = 0
            iqbsc1 = 1
            iqbsc2 = 1 + khelp
C  |  |
C  |  +----------------------------------------------------------------*
C  |                                                   end Kaon's
C  +-------------------------------------------------------------------*
C  |  Antinucleons:
         END IF
      ELSE IF ( ihlp(ip).EQ.4 .AND. ip.LE.9 ) THEN
C  |  For momenta between 3 and 5 GeV/c the use of tabulated data
C  |  should be implemented!
         acof = sgtcoe(1,15)
         bcof = sgtcoe(2,15)
         enne = sgtcoe(3,15)
         ccof = sgtcoe(4,15)
         dcof = sgtcoe(5,15)
C  |  Compute the pbar p total cross section:
         sapptt = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
         IF ( pla.LT.FIVFIV ) THEN
            jreac = 26
         ELSE
            jreac = 25
         END IF
         acof = sgtcoe(1,jreac)
         bcof = sgtcoe(2,jreac)
         enne = sgtcoe(3,jreac)
         ccof = sgtcoe(4,jreac)
         dcof = sgtcoe(5,jreac)
C  |  Compute the pbar p elastic cross section:
         sappel = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
C  |  Compute the pbar p inelastic cross section:
         sappin = sapptt - sappel
         acof = sgtcoe(1,12)
         bcof = sgtcoe(2,12)
         enne = sgtcoe(3,12)
         ccof = sgtcoe(4,12)
         dcof = sgtcoe(5,12)
C  |  Compute the p p total cross section:
         spptot = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
         acof = sgtcoe(1,23)
         bcof = sgtcoe(2,23)
         enne = sgtcoe(3,23)
         ccof = sgtcoe(4,23)
         dcof = sgtcoe(5,23)
C  |  Compute the p p elastic cross section:
         sppela = acof + bcof*pla**enne + ccof*algpla**2 + dcof*algpla
C  |  Compute the K- p inelastic cross section:
         sppine = spptot - sppela
         sigdia = (sappin-sppine)/FIVFIV
         khelp = Ktarg/8
C  |  +----------------------------------------------------------------*
C  |  |  Pbar:
         IF ( ichrge(ip).NE.0 ) THEN
            ndiagr = 5 - khelp
C  |  |  +-------------------------------------------------------------*
C  |  |  |  Proton target:
            IF ( khelp.EQ.0 ) THEN
C  |  |  |  Number of diagrams:
               shncin = sappin
               puubar = 0.8D+00
C  |  |  |
C  |  |  +-------------------------------------------------------------*
C  |  |  |  Neutron target: it is supposed that (ap n)el is almost equal
C  |  |  |                  to (ap p)el (reasonable above 5 GeV/c)
            ELSE
               acof = sgtcoe(1,16)
               bcof = sgtcoe(2,16)
               enne = sgtcoe(3,16)
               ccof = sgtcoe(4,16)
               dcof = sgtcoe(5,16)
C  |  |  |  Compute the total cross section:
               shnctt = acof + bcof*pla**enne + ccof*algpla**2 + 
     &                  dcof*algpla
C  |  |  |  Compute the elastic cross section:
               shncel = sappel
C  |  |  |  Compute the inelastic cross section:
               shncin = shnctt - shncel
               puubar = HLFHLF
C  |  |  |
C  |  |  +-------------------------------------------------------------*
C  |  |  Now compute the chain end (anti)quark-(anti)diquark
C  |  |  there are different possibilities, make a random choiche:
            END IF
            iqfsc1 = -1
            rnchen = DT_RNDM(puubar)
            IF ( rnchen.LT.puubar ) THEN
               iqfsc2 = -2
            ELSE
               iqfsc2 = -1
            END IF
            iqbsc1 = -iqfsc1 + khelp
            iqbsc2 = -iqfsc2
C  |  |
C  |  +----------------------------------------------------------------*
C  |  |  nbar:
         ELSE
            ndiagr = 4 + khelp
C  |  |  +-------------------------------------------------------------*
C  |  |  |  Proton target: (nbar p)in supposed to be given by
C  |  |  |                 (pbar p)in - Sig_diagr
            IF ( khelp.EQ.0 ) THEN
               shncin = sappin - sigdia
               pddbar = HLFHLF
C  |  |  |
C  |  |  +-------------------------------------------------------------*
C  |  |  |  Neutron target: (nbar n)el is supposed to be equal to
C  |  |  |                  (pbar p)el (reasonable above 5 GeV/c)
            ELSE
C  |  |  |  Compute the total cross section:
               shnctt = sapptt
C  |  |  |  Compute the elastic cross section:
               shncel = sappel
C  |  |  |  Compute the inelastic cross section:
               shncin = shnctt - shncel
               pddbar = 0.8D+00
C  |  |  |
C  |  |  +-------------------------------------------------------------*
C  |  |  Now compute the chain end (anti)quark-(anti)diquark
C  |  |  there are different possibilities, make a random choiche:
            END IF
            iqfsc1 = -2
            rnchen = DT_RNDM(rnchen)
            IF ( rnchen.LT.pddbar ) THEN
               iqfsc2 = -1
            ELSE
               iqfsc2 = -2
            END IF
            iqbsc1 = -iqfsc1 + khelp - 1
            iqbsc2 = -iqfsc2
C  |  |
C  |  +----------------------------------------------------------------*
C  |
C  +-------------------------------------------------------------------*
C  |  Others: not yet implemented
         END IF
      ELSE
         sigdia = ZERZER
         shncin = ONEONE
         ndiagr = 0
         DT_PHNSCH = ZERZER
         RETURN
C  |                                                   end others
C  +-------------------------------------------------------------------*
      END IF
      DT_PHNSCH = ndiagr*sigdia/shncin
      iqechc = IQEchr(iqfsc1) + IQEchr(iqfsc2) + IQEchr(iqbsc1)
     &         + IQEchr(iqbsc2)
      iqbchc = IQBchr(iqfsc1) + IQBchr(iqfsc2) + IQBchr(iqbsc1)
     &         + IQBchr(iqbsc2)
      iqechc = iqechc/3
      iqbchc = iqbchc/3
      iqschc = IQSchr(iqfsc1) + IQSchr(iqfsc2) + IQSchr(iqbsc1)
     &         + IQSchr(iqbsc2)
      iqspro = IQSchr(MQUark(1,ip)) + IQSchr(MQUark(2,ip))
     &         + IQSchr(MQUark(3,ip))
C  +-------------------------------------------------------------------*
C  |  Consistency check:
      IF ( DT_PHNSCH.LE.ZERZER .OR. DT_PHNSCH.GT.ONEONE ) THEN
         WRITE (LUNOUT,*) ' *** Phnsch,kp,ktarg,pla' , DT_PHNSCH , Kp , 
     &                    Ktarg , pla , ' ****'
         WRITE (LUNERR,*) ' *** Phnsch,kp,ktarg,pla' , DT_PHNSCH , Kp , 
     &                    Ktarg , pla , ' ****'
         DT_PHNSCH = MAX(DT_PHNSCH,ZERZER)
         DT_PHNSCH = MIN(DT_PHNSCH,ONEONE)
C  |
C  +-------------------------------------------------------------------*
C  +-------------------------------------------------------------------*
C  |  Consistency check:
      END IF
      IF ( iqspro.NE.iqschc .OR. ichrge(ip)+ichrge(Ktarg).NE.iqechc .OR. 
     &     IIBar(Kp)+IIBar(Ktarg).NE.iqbchc ) THEN
         WRITE (LUNOUT,*) 
     &       ' *** Phnsch,iqspro,iqschc,ichrge,iqechc,ibar,iqbchc,ktarg'
     &       , iqspro , iqschc , ichrge(ip) , iqechc , IIBar(Kp) , 
     &       iqbchc , Ktarg
         WRITE (LUNERR,*) 
     &       ' *** Phnsch,iqspro,iqschc,ichrge,iqechc,ibar,iqbchc,ktarg'
     &       , iqspro , iqschc , ichrge(ip) , iqechc , IIBar(Kp) , 
     &       iqbchc , Ktarg
C  |
C  +-------------------------------------------------------------------*
C  P_sch = 1 / [ ( 1 / P_sch (50) - 1 ) x sqrt(s/s(50)) + 1 ]
      END IF
      IF ( umorat.GT.ONEPLS )
     &     DT_PHNSCH = ONEONE/((ONEONE/DT_PHNSCH-ONEONE)*umorat+ONEONE)
      RETURN
C
      ENTRY DT_SCHQUA(Jqfsc1,Jqfsc2,Jqbsc1,Jqbsc2)
      DT_SCHQUA = ONEONE
      Jqfsc1 = iqfsc1
      Jqfsc2 = iqfsc2
      Jqbsc1 = iqbsc1
      Jqbsc2 = iqbsc2
C=== End of function Phnsch ===========================================*
      END FUNCTION
