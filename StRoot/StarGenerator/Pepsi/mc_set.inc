      common /common_mc_set/
     +  mcSet_EneBeam,
     +  mcSet_TarA,
     +  mcSet_TarZ,
     +  mcSet_Q2Min,
     +  mcSet_Q2Max,
     +  mcSet_YMin,
     +  mcSet_YMax,
     +  mcSet_NuMin,
     +  mcSet_NuMax,
     +  mcSet_W2Min,
     +  mcSet_W2Max,
     +  qedrad,
     +  iModel,
     +  genSet_FStruct,
     +  genSet_R,
     +  mcSet_PTarget,
     +  mcSet_PBeam,
     +  mcSet_PTValue,
     +  mcset_PBValue,
     +  mcSet_XMin,
     +  mcSet_XMax,
     +  mcSet_SFact,
     +  mcSet_SBFact,
     +  massp,
     +  masse,
     +  ebeam,
     +  pbeam
      save /common_mc_set/

        integer
     +  qedrad,
     +  iModel,
     +  mcSet_TarA,
     +  mcSet_TarZ,
     +  mcSet_SFact,
     +  mcSet_SBFact

        real
     +  mcSet_EneBeam,
     +  mcSet_Q2Min,
     +  mcSet_Q2Max,
     +  mcSet_YMin,
     +  mcSet_YMax,
     +  mcSet_XMin,
     +  mcSet_XMax,
     +  mcSet_NuMin,
     +  mcSet_NuMax,
     +  mcSet_W2Min,
     +  mcSet_W2Max,
     +  mcSet_PTValue,
     +  mcset_PBValue,
     +  massp,
     +  masse,
     +  ebeam,
     +  pbeam

        character*4
     +  genSet_FStruct,
     +  genSet_R,
     +  mcSet_PBeam,
     +  mcSet_PTarget

        common /mcevnt/
     +          weight,
     +          genq2, gennu, genx, geny, genw2,
     +          genthe, genphi, geneprim, genpprim, ebeamEnucl,
     +          genpx, genpy, genpz,
     +          genvx, genvy, genvz
       save /mcevnt/

        double precision
     +          weight,                                 ! event weight
     +          genq2, gennu, genx, geny, genw2,        ! vertex kinematics
     +          genthe, genphi, geneprim, genpprim,     ! scattered lepton
     +          ebeamEnucl,
     +          genpx, genpy, genpz,                    ! scat lepton 3-vector
     +          genvx, genvy, genvz
