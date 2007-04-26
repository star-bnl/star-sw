//
// $Id: StBemcTrigger.h,v 1.4 2004/08/04 17:53:08 suaide Exp $
//
//    

#ifndef STAR_StBemcTrigger
#define STAR_StBemcTrigger
#include "TObject.h"

#define kNPatches 300
#define kNJet 10
#define kNTowers 4800
#define k12bits 4096

class StEmcGeom;
class StEmcDecoder;
class StEvent;

struct emcTrigger 
{ 
  short    TowerPedestal[kNTowers];      /* single tower pedestals used for trigger (ID -1)*/
  short    TowerStatus[kNTowers];        /* single tower masks used for trigger  (ID -1)*/

  short    HTBits;                       /* High tower bits selection */
  short    PatchStatus[kNPatches];       /* trigger tower status */
  short    PatchLUT[kNPatches][k12bits]; /* Patch LUT */       
    
  short    HT[kNPatches];
  short    Patch[kNPatches];
  short    Jet[kNJet];
  short    Et;
};

class StBemcTrigger: public TObject
{  
  private:
    char           mHighTower[kNPatches];
    char           mPatch[kNPatches];
    char           mJetPatch[kNJet];
    char           mEt;
    
    StEmcGeom*     mGeo;
    StEmcDecoder*  mDecoder;
    StEvent*       mEvent;

    emcTrigger     mTrigger;
    
    bool           mPrint;
  public: 
                   StBemcTrigger();
    virtual        ~StBemcTrigger();
    
    void           resetConf();
    void           makeTrigger();
    
    void           zero();
    void           setEvent(StEvent* e) { mEvent = e;}
    void           setPrint(bool a)     { mPrint = a;}
    
    emcTrigger     getTrigger()        { return mTrigger;}

  ClassDef(StBemcTrigger, 1) 
};
     
#endif

