// Define 2016 simualtion chains

void StarSimOpts2021() {

  { // 2021 Gun Photons eta 2.0 to 5.5 

    StarSimOpt_t job;

    job.name      = "rcf25000:y2021a:photons:G4:kinematics:mult1:eta2-5:pt9-11:v001";
    job.genopts   = "y2021a agml  stargen stargen:mk stargen stargen:stubs kinematics:mk noinput nooutput nodefault";
    job.generator = "StarKine";
    job.genDAttr = {
      { "ptlow", 9.0 },
      { "pthigh", 11.0 },
      { "etalow", 2.0 },
      { "etahigh", 5.0 }
    };
    job.genIAttr = {
      { "pid",    1 },
      { "ntrack", 1  }
    };
    job.genSAttr = {
      { "mode", "FlatPT" }
    };
    job.primDAttr = {
      { "XVERTEX",  0.1 },   { "XSIGMA", 0.01 },
      { "YVERTEX", -0.1 },   { "YSIGMA", 0.01 },
      { "ZVERTEX",  0.0 },   { "ZSIGMA", 15.0 }
    };
    //2021-05-19 06:24:57
    job.timestamps={{"sdt20210519.063000"}};
    job.runnumbers={22139005};

    job.simopts="y2021a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };
    job.recopts="P2021a StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess fcssim fcscluster nodefault";


    // Photons | G4 | 2 < eta < 5 | 9 < pt < 11 GeV
    job.name      = "rcf25000:y2021a:photons:G4:kinematics:mult1:eta2-5:pt9-11:v001";
    addjob(job);

    // Photons | G3 | 2 < eta < 5 | 9 < pt < 11 GeV
    job.name      = "rcf25000:y2021a:photons:G3:kinematics:mult1:eta2-5:pt9-11:v001";    
    job.simSAttr["application:engine"]="G3";
    addjob(job);


    // Electrons | G4 | 2 < eta < 5 | 9 < pt < 11 GeV
    job.name      = "rcf25000:y2021a:electrons:G4:kinematics:mult1:eta2-5:pt9-11:v001";
    job.genIAttr["pid"]=3;
    addjob(job);

    // Electrons | G3 | 2 < eta < 5 | 9 < pt < 11 GeV
    job.name      = "rcf25000:y2021a:electrons:G3:kinematics:mult1:eta2-5:pt9-11:v001";    
    job.genIAttr["pid"]=3;
    job.simSAttr["application:engine"]="G3";
    addjob(job);

    // Piplus | G4 | 2 < eta < 5 | 9 < pt < 11 GeV
    job.name      = "rcf25000:y2021a:piplus:G4:kinematics:mult1:eta2-5:pt9-11:v001";
    job.genIAttr["pid"]=8;
    addjob(job);

    // Piplus | G3 | 2 < eta < 5 | 9 < pt < 11 GeV
    job.name      = "rcf25000:y2021a:piplus:G3:kinematics:mult1:eta2-5:pt9-11:v001";    
    job.genIAttr["pid"]=8;
    job.simSAttr["application:engine"]="G3";
    addjob(job);
       
  }

};
