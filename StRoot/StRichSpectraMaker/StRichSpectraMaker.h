/***************************************************************************
 *
 * $Id: StRichSpectraMaker.h,v 1.10 2002/02/19 04:26:49 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: RICH offline software
 *              StRchMaker.h - ROOT/STAR Maker for offline chain.
 ***************************************************************************
 * $Log: StRichSpectraMaker.h,v $
 * Revision 1.10  2002/02/19 04:26:49  lasiuk
 * addition of filling StEvent for inclusion in chain
 *
 * Revision 1.9  2002/02/12 15:31:35  lasiuk
 * changes to remove formatting of tuple structures
 *
 * Revision 1.8  2002/02/01 17:45:56  lasiuk
 * Mods for gcc(7.2)
 * outer helix usage
 * histo mods
 *
 * Revision 1.7  2002/01/12 00:10:23  lasiuk
 * debin addition; quartz cerenkov angle, tuple modification, shift
 * to 183 nm for ray tracing, no temperature effect yet
 *
 * Revision 1.6  2001/12/19 20:18:38  lasiuk
 * Changeover in algorithm of isolating the Cherenkov angle
 *
 * Revision 1.5  2001/11/21 20:36:07  lasiuk
 * azimuth angle calculation, trace and retracing algorithms, rotation
 * matrices, clean up intersection calculation.  Addition of quick
 * rings for graphics, change definition of ntuples, and
 * removal of old PID method
 *
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

#include <utility>
#ifndef ST_NO_NAMESPACES
using std::pair;
#endif

#define FOR_RECONSTRUCTION 1
#define RICH_SPECTRA_HISTOGRAM 0
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
class StRichCoordinateTransform;

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
    pair<double,double> expectedCerenkov(float, int) const;

protected:
    void  initCutParameters();

    bool  checkTrack(StTrack*) const;
    bool  checkMomentumThreshold(StTrack*) const;
    bool  checkMomentumLimit(StTrack*) const;
    bool  checkMomentumWindow(StTrack*) const;

    void  printCutParameters(ostream& os=cout) const;
    
    // for use on .root files (not condensed)
    bool evaluateEvent();

    bool evaluateRichTrack(StRichTrack*, StRichPidTraits*);

    bool assignMipResidual(StTrack*);
    
    void drawRichPixels(StRichCollection*) const;
    void drawRichHits(StRichCollection*)   const;
    void drawTracks()                      const;
    void drawQuickRing(StThreeVectorF&, StThreeVectorF&);
    
    void qualityAssessment();
    void doIdentification(StTrack*);
    StThreeVectorF calculateRadiationPoint(StTrack*, StThreeVectorF&);
    void calculateResidual(StTrack*);
    
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

    StThreeVectorF mMipResidual;
    StThreeVectorF mCalculatedResidual;
    StThreeVectorF mAssociatedMip;

    StThreeVectorF mRadPoint;
    StThreeVectorF mQuartzRadPoint;

    int            mUniqueRingHits;
    float          mD[3];
    float          mNpd[3];

    
    StRichCoordinateTransform* mTransform;//!
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
    //StThreeVectorF mNormalRadiationPoint;
    // average point where expect the radiation to eminate from
    StThreeVectorF mGlobalRichNormal;
    StThreeVectorF mAverageRadiationPlanePoint;
    StThreeVectorF mAverageQuartzRadiationPlanePoint;
    StThreeVectorF mTopRadiator;
    StThreeVectorF mBottomRadiator;


#ifdef RICH_SPECTRA_HISTOGRAM
    TFile*   mFile; //!

    TNtuple* mEvt;//!
    TNtuple* mTrack;//!
    TNtuple* mCerenkov;//!
    TNtuple* mSim;//!
    TNtuple* mSimEvent;//!
    
    int      mTupleSize;
    int      mTuple2Size;
#endif
    
virtual const char *GetCVS() const	{
    static const char cvs[]=
	"Tag $Name:  $ $Id: StRichSpectraMaker.h,v 1.10 2002/02/19 04:26:49 lasiuk Exp $ built "__DATE__" "__TIME__ ;
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

#endif
