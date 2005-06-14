/*!
 * \class StMuDstVtxSeedMaker 
 * \author G. Van Buren, BNL
 * \version $Id: StMuDstVtxSeedMaker.h,v 1.1 2005/06/14 18:52:20 genevb Exp $
 *
 * calculates mean primary vertex positions from
 * suitable events to use as seeds in finding better       
 * primary vertex positions (helpful for low               
 * multiplicity events like pp collisions)                 
 * using MuDst
 */


#ifndef STAR_StMuDstVtxSeedMaker
#define STAR_StMuDstVtxSeedMaker

#ifndef STAR_StVertexSeedMaker
#include "StVertexSeedMaker.h"
#endif

class StMuDst;
class StMuEvent;


class StMuDstVtxSeedMaker : public StVertexSeedMaker {
 public: 
                  StMuDstVtxSeedMaker(const char *name="EvtVtxSeedMkr");
   virtual        ~StMuDstVtxSeedMaker() {}
   virtual Int_t  Make();
   virtual void   PrintInfo();

   virtual const char *GetCVS() const {
     static const char cvs[]="Tag $Name:  $ $Id: StMuDstVtxSeedMaker.h,v 1.1 2005/06/14 18:52:20 genevb Exp $ built "__DATE__" "__TIME__ ;
     return cvs;
   }

 protected:
   virtual Bool_t CheckTriggers();
   virtual Int_t  GetEventData();

   StMuDst* mudst;
   StMuEvent* event;

  ClassDef(StMuDstVtxSeedMaker,0)
};



#endif

// $Id: StMuDstVtxSeedMaker.h,v 1.1 2005/06/14 18:52:20 genevb Exp $
// $Log: StMuDstVtxSeedMaker.h,v $
// Revision 1.1  2005/06/14 18:52:20  genevb
// Introduction of code to use MuDst for beamline constraint
//
//


