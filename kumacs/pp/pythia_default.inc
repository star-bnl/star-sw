* LUDAT1        <**********************************
MSTU(21)=1       ! (D=2) check on possible errors during program excution
MSTU(41)=2       ! (D=2) partons/particles used in the event analysis
MSTJ(104)=6      ! allow top-antitop production
MSTJ(107)=1      ! include initial state radiation
* LUDAT2        <**********************************
PMAS(C6,1)=175.  ! top quark mass
PMAS(C25,1)=150. ! higgs mass
PMAS(C39,1)=200. ! Leptoquark mass
* LUDAT3        <**********************************
MSTP(111)=1      ! (D=1) (C) 0:no decay, 1:decay
mdcy(c111,1)=0   !=0: make pi0 stable, =1 let it decay
mdcy(c221,1)=0   !make eta stable
MDCY(C310,1)=0   !MAKE K_SHORT STABLE
MDCY(C3112,1)=0  !MAKE SIGMA- STABLE
MDCY(C3122,1)=0  !MAKE LAMBDA0 STABLE
MDCY(C3222,1)=0  !MAKE SIGMA+ STABLE
MDCY(C3312,1)=0  !MAKE CASCADE- STABLE
MDCY(C3322,1)=0  !MAKE CASCADE0 STABLE
MDCY(C3334,1)=0  !MAKE OMEGA- STABLE
*
CKIN(1)=0.0     !sqrt(s) min
CKIN(2)=-1.0    !sqrt(s) max
CKIN(7)=-10.0   !y min
CKIN(8)=10.0    !y max
*
MSTP(2)=2        !(D=1) calculation of alpha_strong at hard interaction
MSTP(31)=5       !(D=1) parametrization of total and elastic cross-sections,
*                ! nuclear slope parameter B  and curvature C [Blo85].
*                ! = 1 : Block-Cahn fit 1 for cross-section, fit 1 for slope
*                ! = 2 : Block-Cahn fit 2 for cross-section, fit 1 for slope
*                ! = 3 : Block-Cahn fit 3 for cross-section, fit 1 for slope
*                ! = 4 : Block-Cahn fit 6 for cross-section, fit 2 for slope
*                ! = 5 : Block-Cahn fit 8 for cross-section, fit 2 for slope
*                ! Note: sets 1-3 for cross-section and set 1 for slope
*                ! paramet
*                ! fits excluding recent measurements from Spp~S, whereas sets
*                ! cross-section and set 2 for slope parameter are fits
*                ! includi
*                ! Spp~S measurements.
*
MSTP(33)=3       ! (D=0) inclusion of K factors in hard cross-sections
PARP(31)=1.2     ! specifies a k factor = 1.2 (SDC)
*PARP(64)=0.0    !transverse momentum evolution kt^2
*
mstp(51)=5       ! choose pdf. 5:CTEQ NLO Best, 9:CTEQ LO best.
*mstp(51)=5005   ! choose PDF's
mstp(52)= 1      ! 1:internal, 2:use pdflib
*
MSTP(81) = 1     ! (D=1) Master switch for multipule interaction
MSTP(82) = 2     ! (D=1) structure of multiple interactions. (SDC)
PARP(82) = 1.3   ! p_T0, the p_T normalization scale
PARP(85) = 0.81  ! probability that an additional interaction gives two gl
PARP(86) = 0.90  ! PART(85) + probability that additional interaction give
*
MSTP(131)=0      ! off, i.e. only one event is generated at a time
*


