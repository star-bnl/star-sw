#include <string>
#include <map>
#include <iostream>

#ifndef __StarSimOpts_h__
#define __StarSimOpts_h__

static bool StarSimOpts__init = false;

struct StarSimOpt_t {

  std::string name;                                       // name for the production
  std::string genopts;                                    // bfc options to run event generation
  std::string simopts;                                    // bfc options to run geant simulation
  std::vector<std::string> timestamps;                    // list of timestamps to be randomly sampled (possibly empty)
  std::vector<int> runnumbers;                      // list of runnumbers
  std::string generator;                                  // name of the event generator
  std::map<std::string,std::string> genSAttr;             // attributes appied to the event generator
  std::map<std::string,double>      genDAttr;
  std::map<std::string,double>      genIAttr;
  std::map<std::string,double>      primDAttr;      // set on primary maker
  std::map<std::string,std::string> simSAttr;             // attributes applied to the simulation
  std::map<std::string,double>      simDAttr;
  std::map<std::string,double>      simIAttr;

  std::string recopts;

};

std::map< std::string, StarSimOpt_t > jobmap;

bool addjob( StarSimOpt_t job ) {
  assert(StarSimOpts__init);
  jobmap[job.name] = job;  
  return true;
};

StarSimOpt_t& addjob( const char* name ) {
  StarSimOpt_t opt = {};
  opt.name = name;
  jobmap[name] = opt;
  return jobmap[name];
};

StarSimOpt_t GetStarSimOpts( std::string name ) {
  return jobmap[name];
};

// Defaults to no printout
void ListStarSimOpts( const int mode=999, bool attr=false ) {

  if ( mode==1 ) {
    std::cout << "# List production jobs" << std::endl;
    for ( auto result : jobmap ) {
      std::cout << "- Production tag " << result.first << std::endl;
    }
    std::cout << std::endl << std::endl;
    std::cout << "# Production job chains and attributes" << std::endl;
  }

  for ( auto result : jobmap ) {
    if ( mode== 0 )     std::cout << result.first << std::endl;
    if ( mode==-1 )     std::cout << result.first << " | " << result.second.genopts << std::endl;    
    if ( mode==-2 )     std::cout << result.first << " | " << result.second.simopts << std::endl;
    if ( mode==-3 )     std::cout << result.first << " | " << result.second.recopts << std::endl;

    if ( mode==1 ) {
      std::cout << "- Production tag " << result.first << std::endl;
      std::cout << std::endl;
      std::cout << "  - Event generator chain: `" << result.second.genopts << "`" << std::endl;
      std::cout << "  - Event generator attributes " << std::endl;
      for ( auto attr: result.second.genSAttr ) {
	std::cout << "       - " << attr.first << "=" << attr.second << std::endl;
      }
      for ( auto attr: result.second.genDAttr ) {
	std::cout << "       - " << attr.first << "=" << attr.second << std::endl;
      }
      std::cout << std::endl;
      std::cout << "  - Simulation chain:  `" << result.second.simopts << "`" << std::endl;
      std::cout << "  - Simulation attributes " << std::endl;
      for ( auto attr: result.second.simSAttr ) {
	std::cout << "       - " << attr.first << "=" << attr.second << std::endl;
      }
      for ( auto attr: result.second.simDAttr ) {
	std::cout << "       - " << attr.first << "=" << attr.second << std::endl;
      }
      for ( auto attr: result.second.simIAttr ) {
	std::cout << "       - " << attr.first << "=" << attr.second << std::endl;
      }
      std::cout << std::endl;
      std::cout << "  - Reconstruction chain:  `" << result.second.recopts << "`" << std::endl;

      std::cout << std::endl << std::endl;

    }

  };

};




void setupProductionJobs() {
  StarSimOpts__init = true;

  { // 2025 200 GeV AuAu Hijing 
    auto& job=addjob("rcf25000:y2025:AuAu200:G4:hijing1.383:minbias:v001");
    job.genopts   = "y2025 agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault";
    job.generator = "Hijing";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "Au"  }, 
          {"BLUE",   "Au"  }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    job.timestamps={{"sdt20251012.054800"}};  // NOTE: need to update to correct timestamp and run numbers
    job.runnumbers={26285007};

    job.simopts="y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };

//  job.recopts="P2025 mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";
    job.recopts="P2025 StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt      MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";

  };

  { // 2025 200 GeV pAu Hijing 
    auto& job=addjob("rcf25000:y2025:pAu200:G4:hijing1.383:minbias:v001");
    job.genopts   = "y2025 agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault";
    job.generator = "Hijing";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "p"  }, 
          {"BLUE",   "Au"  }
    };
    job.genDAttr={{ "Ecms", 200.0 }};

    //  Sun Jun 15 02:14:13 EDT [ Sun Jun 15 06:14:13 GMT ]
    job.timestamps={{"sdt20250615.061500"}};  // NOTE: need to update to correct timestamp and run numbers
    job.runnumbers={26166003};

    job.simopts="y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };

//  job.recopts="P2025 mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";
    job.recopts="P2025 StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt       MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";

  };

  { // 2025 200 GeV pp pythia8
    auto& job=addjob("rcf25000:y2025:pp200:G4:pythia8.2.35:minbias:v001");
    job.genopts   = "y2025 agml  stargen stargen:mk stargen stargen:stubs pythia8.2.35 pythia8:mk noinput nooutput nodefault";
    job.generator = "Pythia8";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "proton"  }, 
          {"BLUE",   "proton"  },
	  {"SET",    "HardQCD:all=on" }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    job.timestamps={{"sdt20250615.061500"}};  // NOTE: need to update to correct timestamp and run numbers
    job.runnumbers={26166003};

    job.simopts="y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };

//  job.recopts="P2025 mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";
    job.recopts="P2025 StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt       MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";

  };

  { // 2024 200 GeV AuAu Hijing 
    auto& job=addjob("rcf25000:y2024a:AuAu200:G4:hijing1.383:minbias:v001");
    job.genopts   = "y2024a agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault";
    job.generator = "Hijing";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "Au"  }, 
          {"BLUE",   "Au"  }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    // Sat Oct 19 00:01:35 EDT [ 2024-10-19 04:01:35 GMT ]  25293001
    job.timestamps={{"sdt20241019.040135"}};
    job.runnumbers={ 25293001 };

    job.simopts="y2024a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };

//  job.recopts="P2024a mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";
    job.recopts="P2024a stargen in -fzin emcy2 eefs eess StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk CMuDst -mtd -mtddat -mtdutil -mtdmatch nodefault  ";

  };


  { // 2024 200 GeV pp Pythia8
    auto& job=addjob("rcf25000:y2024a:pp200:G4:pythia8.2.35:minbias:v001");
    job.genopts   = "y2024a agml  stargen stargen:mk stargen stargen:stubs pythia8.2.35 pythia8:mk noinput nooutput nodefault";
    job.generator = "Pythia8";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "proton"  }, 
          {"BLUE",   "proton"  },
	  {"SET",    "HardQCD:all=on" }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    job.timestamps={{"sdt20240510.080000"}};
    job.runnumbers={25131008};

    job.simopts="y2024a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };

//  job.recopts="P2024a          mysql                 StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess epc  event nodefault";
//  job.recopts="P2024a in -fzin mysql emcy2 eefs eess StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk CMuDst -mtd -mtddat -mtdutil -mtdmatch nodefault  ";
    job.recopts="P2024a stargen in -fzin emcy2 eefs eess StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  btofsim  vpdsim  MiniMcMk CMuDst -mtd -mtddat -mtdutil -mtdmatch nodefault  ";

  };

 
  { // 2023 200 GeV AuAu Hijing 
    auto& job=addjob("rcf25000:y2023a:AuAu200:G4:hijing1.383:minbias:v001");
    job.genopts   = "y2023a agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault";
    job.generator = "Hijing";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "Au"  }, 
          {"BLUE",   "Au"  }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    // Sat Jul 22 00:22:24 EDT [ Sat Jul 22 04:22:24 GMT ]
    job.timestamps={{"sdt20230722.042300"}};
    job.runnumbers={24203001};

    job.simopts="y2023a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };

    job.recopts="P2023a StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";

    // Version 2 will switch off itermediate vertices on tracks
    StarSimOpt_t job2 = job;
    job2.name = "rcf25000:y2023a:AuAu200:G4:hijing1.383:minbias:v002";
    addjob(job2);

  };



  { // 2025 200 GeV AuAu Hijing 
    auto& job=addjob("rcf25000:y2025:AuAu200:G3:hijing1.383:minbias:v001");
    job.genopts   = "y2025 agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault";
    job.generator = "Hijing";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "Au"  }, 
          {"BLUE",   "Au"  }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    job.timestamps={{"sdt20251012.054800"}};  // NOTE: need to update to correct timestamp and run numbers
    job.runnumbers={26285007};

    job.simopts="y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };

    job.recopts="P2025 StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";    

  };


  { // 2025 200 GeV pAu Hijing 
    auto& job=addjob("rcf25000:y2025:pAu200:G3:hijing1.383:minbias:v001");
    job.genopts   = "y2025 agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault";
    job.generator = "Hijing";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "p"  }, 
          {"BLUE",   "Au"  }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    //    job.timestamps={{"sdt20241210"}};
    // Sun Jun 15 02:14:13 EDT [ Sun Jun 15 06:14:13 GMT ]
    job.timestamps={{"sdt20250615.061500"}};
    job.runnumbers={26166003};

    job.simopts="y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };
    job.recopts="P2025 StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";    

  };

  { // 2025 200 GeV pp pythia8
    auto& job=addjob("rcf25000:y2025:pp200:G3:pythia8.2.35:minbias:v001");
    job.genopts   = "y2025 agml  stargen stargen:mk stargen stargen:stubs pythia8.2.35 pythia8:mk noinput nooutput nodefault";
    job.generator = "Pythia8";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "proton"  }, 
          {"BLUE",   "proton"  },
	  {"SET",    "HardQCD:all=on" }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    //    job.timestamps={{"sdt20241210"}};
    job.timestamps={{"sdt20250615.061500"}};
    job.runnumbers={26166003};

    job.simopts="y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };

//  job.recopts="P2024a mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";
//  job.recopts="P2024a stargen in -fzin emcy2 eefs eess StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk CMuDst -mtd -mtddat -mtdutil -mtdmatch nodefault  ";
    job.recopts="P2025 StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt       MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";

  };

  { // 2024 200 GeV AuAu Hijing 
    auto& job=addjob("rcf25000:y2024a:AuAu200:G3:hijing1.383:minbias:v001");
    job.genopts   = "y2024a agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault";
    job.generator = "Hijing";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "Au"  }, 
          {"BLUE",   "Au"  }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    // Sat Oct 19 00:01:35 EDT [ 2024-10-19 04:01:35 GMT ]  25293001
    job.timestamps={{"sdt20241019.040135"}};
    job.runnumbers={ 25293001 };

    job.simopts="y2024a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };

//  job.recopts="P2024a mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";
    //    job.recopts="P2024a in -fzin emcy2 eefs eess StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk CMuDst -mtd -mtddat -mtdutil -mtdmatch nodefault  ";
    job.recopts="P2024a stargen in -fzin emcy2 eefs eess StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk CMuDst -mtd -mtddat -mtdutil -mtdmatch nodefault  ";    

  };


  { // 2024 200 GeV pp Pythia8
    auto& job=addjob("rcf25000:y2024a:pp200:G3:pythia8.2.35:minbias:v001");
    job.genopts   = "y2024a agml  stargen stargen:mk stargen stargen:stubs pythia8.2.35 pythia8:mk noinput nooutput nodefault";
    job.generator = "Pythia8";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "proton"  }, 
          {"BLUE",   "proton"  },
	  {"SET",    "HardQCD:all=on" }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    job.timestamps={{"sdt20240510.080000"}};
    job.runnumbers={25131008};

    job.simopts="y2024a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };

//  job.recopts="P2024a mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess epc  event nodefault";
//    job.recopts="P2024a stargen in -fzin emcy2 eefs eess StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk CMuDst -mtd -mtddat -mtdutil -mtdmatch nodefault  ";
    job.recopts="P2024a stargen in -fzin emcy2 eefs eess StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt       MiniMcMk CMuDst -mtd -mtddat -mtdutil -mtdmatch nodefault  ";    

  };

  { // 2023 200 GeV AuAu Hijing 
    auto& job=addjob("rcf25000:y2023a:AuAu200:G3:hijing1.383:minbias:v001");
    job.genopts   = "y2023a agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault";
    job.generator = "Hijing";
    job.genSAttr={
	  {"FRAME",  "CMS" },
          {"YELL",   "Au"  }, 
          {"BLUE",   "Au"  }
    };
    job.genDAttr={{ "Ecms", 200.0 }};
    // Sat Jul 22 00:22:23 EDT [ 2023-07-22 04:22:23 GMT ] 24203001
    job.timestamps={{"sdt20230722.042300"}};
    job.runnumbers={24203001};

    job.simopts="y2023a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };

    job.recopts="P2023a StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";    

    // Version 2 will switch off itermediate vertices on tracks
    StarSimOpt_t job2 = job;
    job2.name = "rcf25000:y2023a:AuAu200:G3:hijing1.383:minbias:v002";
    addjob(job2);

  };


  { // 2021 Gun Alphas eta 2.0 to 5.5 
    auto& job=addjob("rcf25000:y2021a:alphas:G4:kinematics:mult10:eta2-5:pt9-11:v001");
    job.genopts   = "y2021a agml  stargen stargen:mk stargen stargen:stubs kinematics:mk noinput nooutput nodefault";
    job.generator = "StarKine";
    job.genDAttr = {
      { "ptlow", 9.0 },
      { "pthigh", 11.0 },
      { "etalow", 2.0 },
      { "etahigh", 5.0 }
    };
    job.genIAttr = {
      { "pid",    47 },
      { "ntrack", 10 }
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

    job.recopts="P2021a StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";


    StarSimOpt_t job_alpha = job;
    job_alpha.name = "rcf25000:y2021a:alphas:G4:kinematics:mult1:eta2-5:pt9-11:v001";
    job_alpha.genIAttr = {
      { "pid",    47 },
      { "ntrack",  1 }
    };    addjob(job_alpha);


    StarSimOpt_t job_mu = job;
    job_mu.name = "rcf25000:y2021a:muons:G4:kinematics:mult10:eta2-5:pt9-11:v001";
    job_mu.genIAttr = {
      { "pid",     6 },
      { "ntrack", 10 }
    };    addjob(job_mu);

    job_mu.name = "rcf25000:y2021a:muons:G4:kinematics:mult1:eta2-5:pt9-11:v001";
    job_mu.genIAttr = {
      { "pid",     6 },
      { "ntrack",  1 }
    };    addjob(job_mu);

    StarSimOpt_t job_n = job;
    job_n.name = "rcf25000:y2021a:neutron:G4:kinematics:mult10:eta2-5:pt9-11:v001";
    job_n.genIAttr = {
      { "pid",    13 },
      { "ntrack", 10 }
    };    addjob(job_n);

    job_n.name = "rcf25000:y2021a:neutron:G4:kinematics:mult1:eta2-5:pt9-11:v001";
    job_n.genIAttr = {
      { "pid",    13 },
      { "ntrack",  1 }
    };    addjob(job_n);



  };


  { // 2021 Gun Alphas eta 2.0 to 5.5 
    auto& job=addjob("rcf25000:y2021a:alphas:G3:kinematics:mult10:eta2-5:pt9-11:v001");
    job.genopts   = "y2021a agml  stargen stargen:mk stargen stargen:stubs kinematics:mk noinput nooutput nodefault";
    job.generator = "StarKine";
    job.genDAttr = {
      { "ptlow", 9.0 },
      { "pthigh", 11.0 },
      { "etalow", 2.0 },
      { "etahigh", 5.0 }
    };
    job.genIAttr = {
      { "pid",    47 },
      { "ntrack", 10 }
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
      { "application:engine", "G3" }
    };

    job.recopts="P2021a StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt  MiniMcMk,McAna ,useInTracker btofsim  vpdsim emcsim eefs eess nodefault";


    StarSimOpt_t job_alpha = job;
    job_alpha.name = "rcf25000:y2021a:alphas:G3:kinematics:mult1:eta2-5:pt9-11:v001";
    job_alpha.genIAttr = {
      { "pid",    47 },
      { "ntrack",  1 }
    };    addjob(job_alpha);

    StarSimOpt_t job_mu = job;
    job_mu.name = "rcf25000:y2021a:muons:G3:kinematics:mult10:eta2-5:pt9-11:v001";
    job_mu.genIAttr = {
      { "pid",     6 },
      { "ntrack", 10 }
    };  addjob(job_mu);

    job_mu.name = "rcf25000:y2021a:muons:G3:kinematics:mult1:eta2-5:pt9-11:v001";
    job_mu.genIAttr = {
      { "pid",     6 },
      { "ntrack",  1 }
    };  addjob(job_mu);

    StarSimOpt_t job_n = job;
    job_n.name = "rcf25000:y2021a:neutron:G3:kinematics:mult1:eta2-5:pt9-11:v001";
    job_n.genIAttr = {
      { "pid",    13 },
      { "ntrack",  1 }
    };  addjob(job_n);


  };



   
};  

void StarSimOpts(const int mode=9999){ 
  setupProductionJobs(); 
  //  setupDetectorJobs();
  ListStarSimOpts(mode); 
}
  
#endif

