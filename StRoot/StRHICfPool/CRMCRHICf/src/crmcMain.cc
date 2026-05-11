#include <CRMCoptions.h>
#include <CRMC.h>
#ifdef WITH_ROOT
#include <OutputPolicyROOT.h>
#endif
#ifdef WITH_HEPMC3
#include <OutputPolicyHepMC3.h>
#endif
#ifdef WITH_HEPMC
#include <OutputPolicyHepMC.h>
#endif
#ifdef WITH_RIVET
#include <OutputPolicyRivet.h>
#endif
#include <OutputPolicyLHE.h>
#include <OutputPolicyNone.h>

#include <iostream>
#include <fstream>
#include <cmath>

using namespace std;



int
main(int argc, char **argv)
{

  const CRMCoptions cfg(argc, argv);
  if (cfg.OptionsError()){
    cout << "\nConfiguration Error\n" << endl;
    return 2;
  }

  OutputPolicyNone* output = 0;
  
  switch(cfg.GetOutputMode()) {


#ifdef WITH_ROOT
  case CRMCoptions::eROOT:
    output = new OutputPolicyROOT;
    break;
#endif

#ifdef WITH_HEPMC
  case CRMCoptions::eHepMC:
  case CRMCoptions::eHepMCGZ:
    output = new OutputPolicyHepMC;
    break;
#endif

#ifdef WITH_HEPMC3
  case CRMCoptions::eHepMC3:
  case CRMCoptions::eHepMC3GZ:
    output = new OutputPolicyHepMC3;
    break;
#endif

#ifdef WITH_RIVET
  case CRMCoptions::eRivet:
    output = new OutputPolicyRivet;
    break;
#endif

  case CRMCoptions::eLHE:
  case CRMCoptions::eLHEGZ:
    output = new OutputPolicyLHE;
    break;

  case CRMCoptions::eNone:
    output = new OutputPolicyNone;
    break;

  }
  if (!output) {
    std::cerr << "Invalid output policy specified" << std::endl;
    return 1;
  }
  
  CRMC crmc(cfg, *output);
  if (!crmc.init())   return 1;
  if (!crmc.run())    return 1;
  if (!crmc.finish()) return 1;
  
  return 0;
}
