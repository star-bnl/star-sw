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
      std::cout << "  - Simulation chain:  `" << result.second.genopts << "`" << std::endl;
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

  
  addjob({ 
      "rcf25000:y2025:AuAu200:G4:hijing1.383:minbias:v001",                                                               // name
       "y2025 agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault",    // event generator
       "y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault ", 
	// Sun Jun 15 02:14:13 EDT [ 2025-06-15 06:14:13 GMT ] 26166003
       { "sdt20250615" },
       { 26166003 },
      "Hijing",
	{
	  {"FRAME",  "CMS" },
          {"YELL",   "Au"  }, 
          {"BLUE",   "Au"  }
	},
	{
	  {"Ecms",   200.0 }
	},
	{
	  {"application:engine", "G4"}
	},
	{},
	{},
	"P2025 mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault"
  });

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
    job.timestamps={{"sdt20250615"}};  // NOTE: need to update to correct timestamp and run numbers
    job.runnumbers={26166003};

    job.simopts="y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };

    job.recopts="P2025 mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault";

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
    //    job.timestamps={{"sdt20241210"}};
    job.timestamps={{"sdt20250615"}};
    job.runnumbers={26166003};

    job.simopts="y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };

    job.recopts="P2025 mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault";

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
    job.timestamps={{"sdt20241019"}};
    job.runnumbers={ 25293001 };

    job.simopts="y2024a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };

    job.recopts="P2024a mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault";

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
    //  Sat May 18 01:23:53 EDT [ 2024-05-18 05:23:53 GMT ] 25139005
    job.timestamps={{"sdt20240518"}};
    job.runnumbers={25139005};

    job.simopts="y2024a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };

    job.recopts="P2024a mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault";

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
    // Sat Jul 22 00:22:23 EDT [ 2023-07-22 04:22:23 GMT ] 24203001
    job.timestamps={{"sdt20230722"}};
    job.runnumbers={24203001};

    job.simopts="y2023a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G4" }
    };

    job.recopts="P2023a mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault";

  };


  
  addjob({ 
      "rcf25000:y2025:AuAu200:G3:hijing1.383:minbias:v001",                                                               // name
       "y2025 agml  stargen stargen:mk stargen stargen:stubs hijing1.383 hijing:mk noinput nooutput nodefault",    // event generator
       "y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault ", 
       { "sdt20250615" },
       { 26166003 },
      "Hijing",
	{
	  {"FRAME",  "CMS" },
          {"YELL",   "Au"  }, 
          {"BLUE",   "Au"  }
	},
	{
	  {"Ecms",   200.0 }
	},
	{
	  {"application:engine", "G3"}
	},
	  {},{},
	       "P2025 mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault"
  });

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
    job.timestamps={{"sdt20250615"}};
    job.runnumbers={26166003};

    job.simopts="y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };
    job.recopts="P2025 mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault";

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
    job.timestamps={{"sdt20250615"}};
    job.runnumbers={26166003};

    job.simopts="y2025 agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };

    job.recopts="P2024a mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault";

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
    job.timestamps={{"sdt20241019"}};
    job.runnumbers={ 25293001 };

    job.simopts="y2024a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };

    job.recopts="P2024a mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault";

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
    job.timestamps={{"sdt20240518"}};
    job.runnumbers={25139005};

    job.simopts="y2024a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };

    job.recopts="P2024a mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault";

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
    job.timestamps={{"sdt20230722"}};
    job.runnumbers={24203001};

    job.simopts="y2023a agml stargen:mk genreader:mk simu g4star:mk noinput geant4out nodefault";
    job.simSAttr = {
      { "application:engine", "G3" }
    };

    job.recopts="P2023a mysql StiCA BEmcChkStat EbyET0 ODistoSmear VFMCE TpxClu TpcRS -VFMinuit -hitfilt Tpc23 MiniMcMk,McAna ,useInTracker emcsim eefs eess nodefault";

  };


   
};  

void StarSimOpts(const int mode=9999){ 
  setupProductionJobs(); 
  ListStarSimOpts(mode); 
}
  
#endif

