
#ifndef STFGTNAIVEDBMAKER_H
#define STFGTNAIVEDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StFgtDbMaker.h"
// $Id: StFgtNaiveDbMaker.h,v 1.2 2011/10/04 02:59:35 balewski Exp $


/* \class StFgtNaiveDbMaker        
\author Stephen Gliske

*/

class StFgtNaiveDbMaker : public StFgtDbMaker {

 public: 

  // tmp, FGT gains hardcoded as a function
  double gridAttenuation(float xLoc, float yLoc); // range [0,1]
  double PchargeFraction(float xLoc, float yLoc); // range [0,1]
  double PstripGain(int iStrip, int iQuad, int iDisc);
  double RstripGain(int iStrip, int iQuad, int iDisc);


  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFgtNaiveDbMaker.h,v 1.2 2011/10/04 02:59:35 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  ClassDef(StFgtNaiveDbMaker,0)   //StAF chain virtual base class for Makers
};

#endif

// $Log: StFgtNaiveDbMaker.h,v $
// Revision 1.2  2011/10/04 02:59:35  balewski
// added guestimates of gains, grid absorption, charge sharing
//
