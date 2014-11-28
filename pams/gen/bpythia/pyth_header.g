module PYTH_HEADER  creates and fills EVNT
 author M.Potekhin
 created 08.27.03
* These is the variables for storing/accessing the subprocess ID --max--
       COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
       COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
* These is the variables for storing/accessing the subprocess ID --max--
 DOUBLE PRECISION PARP,PARI,PARU,PARJ
 Integer          MSTI,MSTP,MSTU,MSTJ
 Integer          IPRIN

* Maxim: adding extra bank
 structure MPAR {int Version, int SUBPR, int Nbin, int NWE, int NWW, int Njets, int NREJ }
 structure PYTH {int Version, int SUBPR,
                 real MANDS,  real MANDT,  real MANDU,
                 real HARDP,  real COSTH,
                 real BJOR1,  real BJOR2,
                 real MSTU72, real MSTU73, real MSTP111}

* Subprocess ID  --max--
     Fill /EVNT/PASS/MPAR  ! comment
        Version=3               ! version
        SUBPR = MSTI(1)         ! Pythia subprocess ID
     endfill

     Fill /EVNT/PASS/PYTH  ! comment
        Version=1               ! version
        SUBPR   = MSTI(1)         ! Pythia subprocess ID
        MANDS   = PARI(14)        ! Mandelstam s of the hard subprocess
        MANDT   = PARI(15)        ! Mandelstam t of the hard subprocess
        MANDU   = PARI(16)        ! Mandelstam u of the hard subprocess
        HARDP   = PARI(17)        ! pT of the hard  subprocess in partonic CM frame
        COSTH   = PARI(41)        ! COS theta of hard subprocess in partonic CM frame
        BJOR1   = PARI(33)        ! Bjorken x of 1st parton in after initial radiation
        BJOR2   = PARI(34)        ! Bjorken x of 2nd particle after inital radiation
        MSTU72  = MSTU(72)        ! MSTU72
        MSTU73  = MSTU(73)        ! MSTU73
        MSTP111 = MSTP(111)       ! MSTP111
     endfill
end


