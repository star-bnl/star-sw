/*!
 * \class StPicoDstVtxSeedMaker 
 * \author G. Van Buren, BNL
 * \version $Id: StPicoDstVtxSeedMaker.h,v 1.1 2017/08/08 03:58:20 genevb Exp $
 *
 * calculates mean primary vertex positions from
 * suitable events to use as seeds in finding better       
 * primary vertex positions (helpful for low               
 * multiplicity events like pp collisions)                 
 * using PicoDst
 */


#ifndef STAR_StPicoDstVtxSeedMaker
#define STAR_StPicoDstVtxSeedMaker

#ifndef STAR_StVertexSeedMaker
#include "StVertexSeedMaker.h"
#endif

class StPicoDst;
class StPicoEvent;


class StPicoDstVtxSeedMaker : public StVertexSeedMaker {
 public: 
                  StPicoDstVtxSeedMaker(const char *name="EvtVtxSeedMkr");
   virtual        ~StPicoDstVtxSeedMaker() {}
   virtual Int_t  Make();
   virtual void   PrintInfo();

   virtual const char *GetCVS() const {
     static const char cvs[]="Tag $Name:  $ $Id: StPicoDstVtxSeedMaker.h,v 1.1 2017/08/08 03:58:20 genevb Exp $ built " __DATE__ " " __TIME__ ;
     return cvs;
   }

 protected:
   virtual bool   CheckTriggers();
   virtual int    GetEventData();

   StPicoDst* picodst;
   StPicoEvent* event;

  ClassDef(StPicoDstVtxSeedMaker,0)
};



#endif

// $Id: StPicoDstVtxSeedMaker.h,v 1.1 2017/08/08 03:58:20 genevb Exp $
// $Log: StPicoDstVtxSeedMaker.h,v $
// Revision 1.1  2017/08/08 03:58:20  genevb
// Add vertex-seed-finding with picoDsts
//
//
//


