#ifndef STAR_StTpcFastSimMaker
#define STAR_StTpcFastSimMaker

//////////////////////////////////////////////////////////////////////////
// $Id: StTpcFastSimMaker.h,v 1.2 2014/07/27 13:28:06 fisyak Exp $
// $Log: StTpcFastSimMaker.h,v $
// Revision 1.2  2014/07/27 13:28:06  fisyak
// Add cast for c++11 option
//
// Revision 1.1  2009/11/10 21:15:33  fisyak
// pams clean up
//
//                                                                      //
// StTpcFastSimMaker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class StMagUtilities;

class StTpcFastSimMaker : public StMaker {
 public: 
  StTpcFastSimMaker(const char *name="tpc_hits") : StMaker(name){}
  virtual       ~StTpcFastSimMaker() {}
  virtual Int_t  Make();
 private:
  StMagUtilities*   mExB; //!
 public:
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcFastSimMaker.h,v 1.2 2014/07/27 13:28:06 fisyak Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
   ClassDef(StTpcFastSimMaker,0)   // chain virtual base class for Makers
};

#endif


