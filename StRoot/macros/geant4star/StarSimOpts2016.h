// Define 2016 simualtion chains

void StarSimOpts2016() {

  { // 2016 200 GeV AuAu Hijing 
    auto& job=addjob("rcf26000:y2016x:ideal:AuAu200:G3:hijing1.383:minbias:v001");
    job.genopts   = "y2016x agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault";
    job.generator = "Hijing";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "Au"  }, 
          {"BLUE",   "Au"  }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    //  17107009 
    //  Sat Apr 16 04:31:30 EDT [ Sat Apr 16 08:31:30 GMT ]
    job.timestamps={"sdt20160416.083200"};  // NOTE: need to update to correct timestamp and run numbers
    job.runnumbers={17107009};

    job.simopts="y2016x agml ideal stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };
    job.recopts="P2016x StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt      MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";


    auto job2=job;
    job2.name="rcf26000:y2016x:ideal:AuAu200:G4:hijing1.383:minbias:v001";
    job2.simSAttr = {{ "application:engine", "G4" }};
    addjob(job2);

    job2.name="rcf26000:y2016x:misalign:AuAu200:G3:hijing1.383:minbias:v001";
    job2.simSAttr = {{ "application:engine", "G3" }};
    job2.simopts="y2016x agml misalign stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    addjob(job2);

    job2.name="rcf26000:y2016x:misalign:AuAu200:G4:hijing1.383:minbias:v001";
    job2.simSAttr = {{ "application:engine", "G4" }};
    job2.simopts="y2016x agml misalign stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    addjob(job2);

  };

  { // 2016 200 GeV AuAu Hijing 
    auto& job=addjob("rcf26000:y2016a:ideal:AuAu200:G3:hijing1.383:minbias:v001");
    job.genopts   = "y2016a agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault";
    job.generator = "Hijing";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "Au"  }, 
          {"BLUE",   "Au"  }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    //  17107009 
    //  Sat Apr 16 04:31:30 EDT [ Sat Apr 16 08:31:30 GMT ]
    //    job.timestamps={{"sdt20160416.083500"}};  // NOTE: need to update to correct timestamp and run numbers
    job.timestamps={"sdt20160416.043500"};
    job.runnumbers={17107009};

    job.simopts="y2016a agml ideal stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };
    job.recopts="P2016a StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt      MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";


    auto job2=job;
    job2.name="rcf26000:y2016a:ideal:AuAu200:G4:hijing1.383:minbias:v001";
    job2.simSAttr = {{ "application:engine", "G4" }};
    addjob(job2);

    job2.name="rcf26000:y2016a:misalign:AuAu200:G3:hijing1.383:minbias:v001";
    job2.simSAttr = {{ "application:engine", "G3" }};
    job2.simopts="y2016a agml misalign stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    addjob(job2);

    job2.name="rcf26000:y2016a:misalign:AuAu200:G4:hijing1.383:minbias:v001";
    job2.simSAttr = {{ "application:engine", "G4" }};
    job2.simopts="y2016a agml misalign stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    addjob(job2);

  };

};
