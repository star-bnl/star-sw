/***************************************************************************
 *
 * $Id: StRichSpectraMaker.h,v 1.4 2001/08/22 19:33:35 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: RICH offline software
 *              StRchMaker.h - ROOT/STAR Maker for offline chain.
 ***************************************************************************
 * $Log: StRichSpectraMaker.h,v $
 * Revision 1.4  2001/08/22 19:33:35  lasiuk
 * remove trace of StPairD, and move some include files that
 * should ease parsing of CINT
 *
 * Revision 1.3  2001/08/21 17:58:34  lasiuk
 * for 2000 analysis
 *
 * Revision 1.2  2001/02/25 22:11:46  lasiuk
 * quality assessment
 *
 * Revision 1.1  2000/12/12 21:35:09  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef STAR_StRichSpectraMaker_hh
#define STAR_StRichSpectraMaker_hh

#ifndef StMaker_H
#include "StMaker.h"
#endif

#define RICH_SPECTRA_HISTOGRAM 1
//RICH_WITH_PAD_MONITOR 1

#ifdef RICH_SPECTRA_HISTOGRAM
#include "TFile.h"
#include "TH1.h"
#include "TNtuple.h"
#endif


// forward StEvent declaration
class StEvent;
class StTrack;
class StRichCollection;
class StRichHit;
class StRichSimpleHit;
class StRichSimpleHitCollection;
class StRichPidTraits;

// PID Maker
class StRichTrack;

// RRS
class StRichGeometryDb;
class StRichMomentumTransform;

// Pad Monitor
class StRichPadMonitor;

//
class StRichRayTracer;
class StRichCerenkovHistogram;

#include "StThreeVectorF.hh"
#include "StParticleDefinition.hh"

class StRichSpectraMaker : public StMaker {
    
public: 
    StRichSpectraMaker(const char *name="rchspec");
    virtual ~StRichSpectraMaker();

    Int_t  Init();
    Int_t  Make();
    void   PrintInfo();
    Int_t  Finish();

    StThreeVectorF normalRadiationPoint() const;

protected:
    void  initCutParameters();

    bool  checkTrack(StTrack*) const;
    bool  checkMomentumThreshold(StTrack*) const;
    bool  checkMomentumLimit(StTrack*) const;
    bool  checkMomentumWindow(StTrack*) const;
    float expectedNumberOfPhotons(float, int) const;

    void  printCutParameters(ostream& os=cout) const;
    
    // for use on .root files (not condensed)
    bool evaluateEvent(StRichTrack*, StRichPidTraits*);
    void lookAtPhotonInfo(StRichTrack*, StRichPidTraits*);

    void drawRichPixels(StRichCollection*) const;
    void drawRichHits(StRichCollection*)   const;
    void drawTracks()                      const;
    
    void qualityAssessment();
    void doIdentification(StTrack*);
    StThreeVectorF calculateRadiationPoint(StTrack*);
    
protected:
    StEvent*                    mEvent;//!
    StRichCollection*           mTheRichCollection;//!

    //
    // storage variables
    //
    
    StThreeVectorF          mVertexPos;
    unsigned int            mNumberOfPrimaries;
    float                   mIndex;
    float                   mMagField;

    unsigned int mNumberOfEvents;
    unsigned int mNumberOfGood2GevTracks;

    //
    // particles
    //
    StParticleDefinition* mPion;//!
    StParticleDefinition* mKaon;//!
    StParticleDefinition* mProton;//!

    // Cuts:
    //   Event level

    float mVertexWindow;
    
    //   Track level

    float mLastHitCut;
    float mDcaCut;
    int   mFitPointsCut;
    float mEtaCut;
    float mPtCut;
    float mPathCut;
    float mPadPlaneCut;
    float mRadiatorCut;

    float mMomentumThreshold;
    float mMomentumLimit;
    
    StRichMomentumTransform* mMomTform;//!
    StRichGeometryDb*        mGeometryDb;//!

    StRichPadMonitor*        mPadMonitor;//!

    //
    // Cerenkov Angle determination
    //

    StRichRayTracer*         mTracer;//!
    double mMeanWavelength;//!
    StRichCerenkovHistogram*  mHistogram;//!
    
    //
    // geometry
    //
    StThreeVectorF mNormalRadiationPoint;
    
#ifdef RICH_SPECTRA_HISTOGRAM
    TFile*   mFile; //!
    TNtuple* mPhotons;   //!
    TNtuple* mCorrected;   //!

    TNtuple* mEvt;//!
    TNtuple* mTrack;//!
    TNtuple* mCerenkov;//!
    TNtuple* mSim;//!
    
    int      mTupleSize;
    int      mTuple2Size;
#endif
    
virtual const char *GetCVS() const	{
    static const char cvs[]=
	"Tag $Name:  $ $Id: StRichSpectraMaker.h,v 1.4 2001/08/22 19:33:35 lasiuk Exp $ built "__DATE__" "__TIME__ ;
    return cvs;
}
public:
    virtual void SetMode(Int_t mode=0) {
	m_Mode = mode;
    }
private:
    // the following is a ROOT macro  that is needed in all ROOT code
    ClassDef(StRichSpectraMaker, 1)   //StAF chain virtual base class for Makers
	};

inline StThreeVectorF StRichSpectraMaker::normalRadiationPoint() const { return mNormalRadiationPoint;}

#endif
