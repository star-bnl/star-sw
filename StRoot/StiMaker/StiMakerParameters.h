#ifndef StiMakerParameters_H_INCLUDED
#define StiMakerParameters_H_INCLUDED

#include <iostream.h>
#include "TObject.h"

class StiMakerParameters : public TObject
{
 public:
  StiMakerParameters();
  virtual ~StiMakerParameters();

  // Detector loading
  bool useEmc;
  bool useEemc;
  bool useSvt;
  bool useSsd;
  bool useTpc;
  bool useFtpc;
  bool usePixel;

  // Hit loading 
  bool activeEmc;
  bool activeEemc;
  bool activeSvt;
  bool activeSsd;
  bool activeTpc;
  bool activeFtpc;
  bool activePixel;
  bool useResidualCalculator;

  bool useMcAsRec;
  bool useGui;
  bool doSimulation;
  bool doAssociation;
  bool doMiniMcEvent;
  bool doDst;
  bool doStEventOutput;
  bool doStEventInput;  
  bool doPlots;

  friend ostream& operator<<(ostream& os, const StiMakerParameters&pars);
  
  ClassDef(StiMakerParameters, 1)

};

#endif
