#ifndef MiniChainParameters_H_INCLUDED
#define MiniChainParameters_H_INCLUDED

#include <iostream.h>
#include "TObject.h"

class MiniChainParameters : public TObject
{
 public:
  MiniChainParameters();
  virtual ~MiniChainParameters();

  // Detectors
  bool useEmc;
  bool useEemc;
  bool useSvt;
  bool useSsd;
  bool useTpc;
  bool useFtpc;

  bool useGui;
  bool doSimulation;
  bool doAssociation;
  bool doMiniMcEvent;
  bool doDst;
  bool doStEventOutput;
  bool doStEventInput;  

  friend ostream& operator<<(ostream& os, const MiniChainParameters&pars);
  
  ClassDef(MiniChainParameters, 1)

};

#endif
