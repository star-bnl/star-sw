#include "StRoot/macros/bfc.C"
#include "StarSimOpts.h"

void run_reconstruction( int nevents=0, const char* jobtag=0, int index=0, std::string dir="./" ) {

  setupProductionJobs();

  std::string inpname = jobtag;
  std::replace( inpname.begin(), inpname.end(), ':', '_' );
  std::replace( inpname.begin(), inpname.end(), '.', 'p' );
  inpname += "_job";
  inpname += std::to_string(index);
  inpname = inpname + ".geant.root";

  
  std::string chopts=jobmap[jobtag].recopts;

  bfc(-1,chopts.c_str(),(dir+inpname).c_str());

  gInterpreter->ProcessLine("{Geometry* __hack = new Geometry(); delete __hack;}");

  chain->Init();

  chain->EventLoop(nevents,chain->Maker("outputStream"));

  std::cout << jobtag << std::endl;
  std::cout << jobmap[jobtag].recopts.c_str() << std::endl;
  std::cout << inpname.c_str() << std::endl;

}
