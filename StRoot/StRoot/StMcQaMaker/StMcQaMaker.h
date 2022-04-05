/**********************************************
 *
 * $Id: StMcQaMaker.h,v 1.2 2015/08/29 03:37:46 perev Exp $
 * $Log: StMcQaMaker.h,v $
 * Revision 1.2  2015/08/29 03:37:46  perev
 * __
 *
 * Revision 1.1  2007/03/21 16:48:49  fisyak
 * maker for side by side comparision of GEANT and VMC simulations
 *
 *
 * Creates Histograms to test functionality of
 * StMcEvent
 *
 **********************************************/

#ifndef StMcQaMaker_HH
#define StMcQaMaker_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif
class StMcEvent;

class StMcQaMaker : public StMaker {
  
 public:

  StMcQaMaker(const char* name = "StMcQaMaker",
	      const char* title = "event/StMcQaMaker") : StMaker(name,title) {}
  virtual ~StMcQaMaker() {}
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void  QAPlots(StMcEvent *ev=0);
  virtual const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StMcQaMaker.h,v 1.2 2015/08/29 03:37:46 perev Exp $ built " __DATE__ " " __TIME__; return cvs;}	
  ClassDef(StMcQaMaker,0)
};
#endif
