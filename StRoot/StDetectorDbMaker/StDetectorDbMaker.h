#ifndef STAR_StDetectorDbMaker
#define STAR_StDetectorDbMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StDetectorDbMaker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: StDetectorDbMaker.h,v 1.2 2003/09/10 19:47:07 perev Exp $";
 
 protected:
 public: 
                   StDetectorDbMaker(const char *name="DetectorDb");
    virtual       ~StDetectorDbMaker();
    virtual Int_t Init();
    virtual Int_t  Make();
    virtual Int_t InitRun(int);
    
   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StDetectorDbMaker.h,v 1.2 2003/09/10 19:47:07 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StDetectorDbMaker,0)   //StAF chain virtual base class for Makers
};

#endif
