/*!
 * \class StEvtVtxSeedMaker 
 * \author G. Van Buren, BNL
 * \version $Id: StEvtVtxSeedMaker.h,v 1.3 2012/08/17 22:57:33 genevb Exp $
 *
 * calculates mean primary vertex positions from
 * suitable events to use as seeds in finding better       
 * primary vertex positions (helpful for low               
 * multiplicity events like pp collisions)                 
 * using StEvent
 */


#ifndef STAR_StEvtVtxSeedMaker
#define STAR_StEvtVtxSeedMaker

#ifndef STAR_StVertexSeedMaker
#include "StVertexSeedMaker.h"
#endif

class StEvent;


class StEvtVtxSeedMaker : public StVertexSeedMaker {
 public: 
                  StEvtVtxSeedMaker(const char *name="EvtVtxSeedMkr");
   virtual        ~StEvtVtxSeedMaker() {}
   virtual Int_t  Make();
   virtual void   PrintInfo();

   virtual const char *GetCVS() const {
     static const char cvs[]="Tag $Name:  $ $Id: StEvtVtxSeedMaker.h,v 1.3 2012/08/17 22:57:33 genevb Exp $ built "__DATE__" "__TIME__ ;
     return cvs;
   }

 protected:
   virtual Bool_t CheckTriggers();
   virtual Int_t  GetEventData();

   StEvent* event;

  ClassDef(StEvtVtxSeedMaker,0)
};



#endif

// $Id: StEvtVtxSeedMaker.h,v 1.3 2012/08/17 22:57:33 genevb Exp $
// $Log: StEvtVtxSeedMaker.h,v $
// Revision 1.3  2012/08/17 22:57:33  genevb
// Add index of vertex within event to ntuple
//
// Revision 1.2  2006/09/01 22:27:16  genevb
// More detailed info in ntuple
//
// Revision 1.1  2005/06/14 18:52:04  genevb
// Introduction of code to use StEvent for beamline constraint
//
//
