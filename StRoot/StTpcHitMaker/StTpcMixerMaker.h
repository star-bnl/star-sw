/***************************************************************************
 *
 * $Id: StTpcMixerMaker.h,v 1.1 2008/07/31 20:45:27 fisyak Exp $
 * $Log: StTpcMixerMaker.h,v $
 * Revision 1.1  2008/07/31 20:45:27  fisyak
 * Add TpcMixer
 *
 **************************************************************************/

#ifndef STAR_StTpcMixerMaker
#define STAR_StTpcMixerMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
class StTpcMixerMaker : public StMaker {
 public:
  StTpcMixerMaker(const char *name="TpcMixer") : StMaker(name) {}
  virtual ~StTpcMixerMaker() {}
  virtual Int_t Make();
  virtual const char *GetCVS() const {
    static const char cvs[]=
      "Tag $Name:  $ $Id: StTpcMixerMaker.h,v 1.1 2008/07/31 20:45:27 fisyak Exp $ built "__DATE__" "__TIME__; 
    return cvs;}
  ClassDef(StTpcMixerMaker, 0)  // 
};
#endif
