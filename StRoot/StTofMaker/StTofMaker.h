/***************************************************************************
 *
 * $Id: StTofMaker.h,v 1.1 2001/04/24 20:27:37 wzhang Exp $ 
 * 
 * Author: Wei-Ming Zhang, April 2001
 *
 ***************************************************************************
 *
 * Description: TOF offline software
 *              StTofMaker.h - ROOT/STAR Maker for offline chain.
 ***************************************************************************
 *
 * $Log: StTofMaker.h,v $
 * Revision 1.1  2001/04/24 20:27:37  wzhang
 * First release
 * 
 *
 **************************************************************************/

#ifdef __ROOT__
#ifndef STAR_StTofMaker
#define STAR_StTofMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#define TOF_HISTOGRAM

#ifndef __CINT__
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#ifdef TOF_HISTOGRAM
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
// #include "TNtuple.h"
#endif

#include "StTofSlat.h"
#include "StTofHit.h"
#include "StTofCross.h"
#include "StTofCollection.h"
#include "StTofSlatCollection.h"
#include "StTofHitCollection.h"
#endif
// forward StEvent declaration
class StTofCollection;

class StTofSlat;
class StTofHit;

class TH2F;
class StEvent;
class StTofDatabase;
class StTofCross;
class StTofSlatCollection;
class StTofHitCollection;
class StTofPidTraits;

class StTofMaker : public StMaker {
private:
    Bool_t drawinit;
    StEvent*               mEvent;//!
    StTofDatabase*         mTofDb;//!
    StTofCross*            mTofCross;//!
    StTofCollection*       mTheTofCollection;//!
    StTofSlatCollection*   mSlatCollection;//!
    StTofHitCollection*    mHitCollection;//!

    // flags
    short mTofCollectionPresent ;
    short mSlatCollectionPresent;
    short mHitCollectionPresent;

protected:

#ifdef TOF_HISTOGRAM

    TH1F* mHitPerTrk;//!
    TH1F* mTrkDist;//!
    TH1F* mHitDist;//!
    TH1F* mMom1;//!
    TH1F* mMom2;//!
    TH1F* mMom3;//!
    TH1F* mPz;//!
    TH1F* mPz1;//!
    TH1F* mZ;//!
    TH1F* mZ1;//!
    TH1F* mCur1;//!
    TH1F* mCur2;//!
    TH1F* mCur3;//!

#endif

private:
   void   fillStEvent(); 

// method for testing classes of StTofPidMaker 
   void   fillPidTraits(); 

public: 
                   StTofMaker(const char *name="tof");
    virtual       ~StTofMaker();
    virtual Int_t  Init();
    virtual Int_t  Make();
    virtual Int_t  Finish();
    
    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StTofMaker.h,v 1.1 2001/04/24 20:27:37 wzhang Exp $ built "__DATE__" "__TIME__ ; return cvs;}

private:
    ClassDef(StTofMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
#endif /* __ROOT__ */
