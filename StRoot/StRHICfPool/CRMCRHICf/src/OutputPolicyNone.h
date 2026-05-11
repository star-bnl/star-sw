#ifndef _OutputPolicyNone_h_
#define _OutputPolicyNone_h_

class CRMCoptions;

class OutputPolicyNone {

public:
  OutputPolicyNone();

  virtual void InitOutput(const CRMCoptions& cfg);
  virtual void FillEvent(const CRMCoptions& cfg, const int nEvent);
  virtual void FillRHICfEvent(const CRMCoptions& cfg, const int nEvent, int& passEventNum);
  virtual void CloseOutput(const CRMCoptions& cfg);

  virtual void PrintTestEvent(const CRMCoptions& cfg) {};
  virtual void PrintCrossSections(const CRMCoptions& cfg);
};


#endif
