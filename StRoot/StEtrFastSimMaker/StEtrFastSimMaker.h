#ifndef STAR_StEtrFastSimMaker
#define STAR_StEtrFastSimMaker

//////////////////////////////////////////////////////////////////////////
// $Id: StEtrFastSimMaker.h,v 1.2 2015/08/29 03:24:43 perev Exp $		//
// $Log: StEtrFastSimMaker.h,v $
// Revision 1.2  2015/08/29 03:24:43  perev
// __DATE
//
// Revision 1.1  2012/03/22 01:19:12  perev
// *** empty log message ***
//					//
//                                                                      //
// StEtrFastSimMaker fast simu for Etr                            	//
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class StEtrFastSimMaker : public StMaker {
public: 
  StEtrFastSimMaker(const char *name="etr_hits");
  virtual       ~StEtrFastSimMaker() {}
  virtual int  InitRun(int RunNo);
  virtual int  Make();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEtrFastSimMaker.h,v 1.2 2015/08/29 03:24:43 perev Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
private:
  double mErr[3];

   ClassDef(StEtrFastSimMaker,0)   // EtrFastSim
};

#endif


