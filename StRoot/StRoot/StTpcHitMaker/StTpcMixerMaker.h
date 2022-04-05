/***************************************************************************
 *
 * $Id: StTpcMixerMaker.h,v 1.2 2014/08/06 11:43:50 jeromel Exp $
 * $Log: StTpcMixerMaker.h,v $
 * Revision 1.2  2014/08/06 11:43:50  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
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
      "Tag $Name:  $ $Id: StTpcMixerMaker.h,v 1.2 2014/08/06 11:43:50 jeromel Exp $ built " __DATE__ " " __TIME__; 
    return cvs;}
  ClassDef(StTpcMixerMaker, 0)  // 
};
#endif
