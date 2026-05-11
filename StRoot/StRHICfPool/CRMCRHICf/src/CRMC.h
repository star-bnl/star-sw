#ifndef __CRMC_H
#define __CRMC_H
#include <OutputPolicyNone.h>
#include <CRMCinterface.h>
//#include <CRMCfilter.h>

// //////////
// //////////

class CRMCoptions;


class CRMC {
 public:
  CRMC(const CRMCoptions& cfg, OutputPolicyNone& output);

  bool init();
  bool run();
  bool finish();

  CRMCinterface& GetInterface() { return fInterface; }

 private:

  const CRMCoptions& fCfg;
  CRMCinterface fInterface;
  OutputPolicyNone& fOutput;
  //CRMCfilter fFilter;

};


#endif
