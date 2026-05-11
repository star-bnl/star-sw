#ifndef _OutputPolicyLHE_h_
#define _OutputPolicyLHE_h_
#include "OutputPolicyNone.h"


class CRMCoptions;

class OutputPolicyLHE : public OutputPolicyNone {

 public:
  OutputPolicyLHE();
  
  void InitOutput(const CRMCoptions& cfg) override;
  void FillEvent(const CRMCoptions& cfg,const int nEvent) override;
  void CloseOutput(const CRMCoptions& cfg) override;

};


#endif
