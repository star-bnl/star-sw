/**********************************************************
 * $Id: StRichPIDMaker.h,v 2.9 2000/11/25 11:55:14 lasiuk Exp $
 *
 * Description:
 *  StRrsMaker is the main module
 *  StRichRawData. It has the standard Maker functions:
 *
 *  $Log: StRichPIDMaker.h,v $
 *  Revision 2.9  2000/11/25 11:55:14  lasiuk
 *  add reprocess Traits
 *
 *  Revision 2.8  2000/11/21 19:50:35  lasiuk
 *  add stthreevectorf
 *
 *  Revision 2.7  2000/11/21 16:24:22  horsley
 *  Major overhaul of StRichArea, introduced monte carlo integration cross check,
 *  all possible areas, angles calculated together. StRichRingCalculator, StRichPIDMaker modified to support new StRichArea. StRichPIDMaker's hit finder
 *  typo corrected.
 *
 *  Revision 2.6  2000/11/07 14:11:43  lasiuk
 *  initCutParameters() and diagnositis print added.
 *  bins for <d> and sigma_d added.
 *  TPC hits for RICH tracks written out.
 *  (file) ptr checked before writing ntuple.
 *  check flags on Hits instead of ADC value
 *
 *  Revision 2.5  2000/11/01 17:45:24  lasiuk
 *  MAJOR. hitFilter overhaul. members reordered, padplane dimension kept as
 *  a member.  addition of initTuple.  Additional dependencies of
 *  min/max algorithms
 *
 *  Revision 2.4  2000/10/19 01:13:23  horsley
 *  added member functions to StRichPIDMaker to make cuts on hits, tracks, events.
 *  added normal distance sigma cut on hits, quartz and radiator pathlengths
 *  for individual photons, modified minimization routine to correct boundary
 *  problems
 *
 *  Revision 2.3  2000/10/02 23:06:33  horsley
 *  *** empty log message ***
 *
 *  Revision 2.2  2000/09/29 01:35:37  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.4  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.2  2000/05/19 19:06:10  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 **********************************************************/

#ifndef StRichPIDMaker_HH
#define StRichPIDMaker_HH

#include "StMaker.h"
#include "TNtuple.h"
#include "TH3.h"
#include "TFile.h"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

// SCL
#include "StParticleTypes.hh"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"
#include "StThreeVector.hh"

// StRichPid
//#include "StRichTrackFilter.h"
#include "StRichTrackingControl.h"
#include "StRichMcSwitch.h"
#include "TpcHitVecUtilities.h"

#ifdef RICH_WITH_L3_TRACKS
#include "StDaqLib/L3/L3.Banks.hh"
#endif

// SCL
class StParticleDefinition;

// StRichPID
class StTrack;
class StRichTrack;
class StRichRingCalculator;
class StRichMaterialsDb;

// StRrs
class StRichGeometryDb;
class StRichCoordinateTransform;
class StRichMomentumTransform;

// StRch
class StRichSimpleHitCollection;

// StDisplay
class StRichPadMonitor;

// StEvent
class StEvent;
class StRichPidTraits;
class StSPtrVecRichHit;
class StSPtrVecRichPixel;
class StSPtrVecRichCluster;

class StMcTrack;
class StMcEvent;
class StRichMCHit;
class StRichHit;
class StRichMCTrack;
class St_g2t_track;

class StRichPIDMaker : public StMaker {

private:
    Bool_t drawinit;
    Char_t collectionName[256];  
    vector<StRichTrack* > mListOfStRichTracks; //!
    vector<StParticleDefinition* > mListOfParticles; //!

    bool  mRefit;
    bool  mDoGapCorrection;

    double mPionSaturatedArea;
    double mKaonSaturatedArea;
    double mProtonSaturatedArea;

    bool   mPrintThisEvent;

  //
  // monte carlo event
  //
  StMcEvent* mEvent; //!

    //
    // Event Parameters
    //
    StThreeVectorF mVertexPos;
    float          mMagField;
    int            mEventN;

    //
    // Tracks in the RICH
    //
    int mNumberOfPrimaries;
    int mNegativePrimaries;
    int mNumberOf1GeV;
    int mNumberOfRingHits;
    int mRichTracks;
  
    //
    // wavelength bounds
    //
    double mShortWave;
    double mLongWave;
    double mDefaultShortWave;
    double mDefaultLongWave;

    // hit filter
    double innerDistance,outerDistance,meanDistance;
    double innerAngle,outerAngle,meanAngle; 
    double ringWidth;

    double meanD[6][2];
    double sigmaD[6][2];
    
    //
    // pad plane dimensions
    StThreeVector<double>   mPadPlaneDimension; //!
    
    StRichPadMonitor*  mPadMonitor; //!
    StRichGeometryDb*  mGeometryDb; //!
    StRichMaterialsDb* mMaterialDb; //! 
    StRichCoordinateTransform* mCoordinateTransformation; // !  
    StRichMomentumTransform*   mMomentumTransformation; //!

    //
    // Cuts: parameters and types
    //
    // Event
    float mVertexWindow;
    
    // Hits
    int   mAdcCut;
    
    // Track
    float mLastHitCut;
    float mDcaCut;
    int   mFitPointsCut;
    float mEtaCut;
    float mPtCut;
    float mPathCut;
    float mPadPlaneCut;
    float mRadiatorCut;
    float mThresholdMomentum;

    // convergence limit for psi determination
    double mPrecision;
    
    char* fileName; //!
    char* mySaveDirectory; //!

    // particles
    StPionMinus*  pion;   //!
    StKaonMinus*  kaon;   //!
    StAntiProton* proton; //!
    
    TFile*   file;          //!
    TNtuple* trackNtuple;   //!
    TNtuple* geantTrackNtuple;   //!
    TNtuple* geantPhotonNtuple;   //!
    TNtuple* geantPixelNtuple;   //!
    TNtuple* geantCloseHitNtuple;   //!

    //
    // brian
    //
    TNtuple* distup; //!
    
    TNtuple* hitNtup;  //!
    TNtuple* evtNtup;  //!
    TNtuple* closeHitNtup;   //!
    
    TH3F*    pionResid;      //!
    TH3F*    pionResid_x;    //! 
    TH3F*    pionResid_y;    //!

    TH3F*    pionTheta;      //!
    TH3F*    pionTheta_x;    //! 
    TH3F*    pionTheta_y;    //!
    TH3F*    pionPsi;        //!  
    
    TH3F*    pionResidb;      //!
    TH3F*    pionResid_xb;    //! 
    TH3F*    pionResid_yb;    //!
    
    TH3F*    pionThetab;      //!
    TH3F*    pionTheta_xb;    //! 
    TH3F*    pionTheta_yb;    //!
    
    TH3F*    pionPsib;        //!
    
    TH3F*    pionRadius;     //!

    TNtuple* trackCorrectedNtuple;   //!

    TH3F*    pionCorrectedResid;      //!
    TH3F*    pionCorrectedResid_x;    //! 
    TH3F*    pionCorrectedResid_y;    //!
    
    TH3F*    pionCorrectedTheta;      //!
    TH3F*    pionCorrectedTheta_x;    //! 
    TH3F*    pionCorrectedTheta_y;    //!
    
    TH3F*    pionCorrectedPsi;        //!
    
    TH3F*    pionCorrectedResidb;      //!
    TH3F*    pionCorrectedResid_xb;    //! 
    TH3F*    pionCorrectedResid_yb;    //!

    TH3F*    pionCorrectedThetab;      //!
    TH3F*    pionCorrectedTheta_xb;    //! 
    TH3F*    pionCorrectedTheta_yb;    //!
    
    TH3F*    pionCorrectedPsib;        //!
 
    TNtuple*    overLap;      //!
    TpcHitVecUtilities* util; //!
    bool     kWriteNtuple;    //!

    TNtuple*    psiFixNtuple;      //!

  
public:
  
    StRichPIDMaker(const Char_t *name="RICHPID", bool writeNtuple=false);
    virtual ~StRichPIDMaker();
  
    Int_t Init();
    Int_t Make();
    Int_t Finish();

    void initCutParameters();
    void initNtuples();
    
    //
    // hit operations
    //
    void hitFilter(const StSPtrVecRichHit*, StRichRingCalculator*);
    void hitFilter(StRichRingCalculator* , StThreeVectorF&, float&, float&);

    float getHitSigma(float); 
    void  tagMips(StEvent*, StSPtrVecRichHit*);

    //
    // track operations
    //
    Int_t fillTrackList(StEvent*, const StSPtrVecRichHit*);

    //
    // functions to apply cuts
    // and set cut parameters
    //
    bool checkEvent(StEvent*);
    bool checkHit(StRichHit*);
    bool checkTrack(StRichTrack*);
    bool checkTrack(StTrack*);
    bool checkTrackMomentum(float);

    //
    // set/Print cut parameters at the macro Level
    //

    void printCutParameters(ostream& os=cout) const;
    
    // Event Level
    void setVertexWindow(float);

    // Hit Level
    void setAdcCut(int);

    // Track Level
    void setLastHitCut(float);
    void setEtaCut(float);
    void setDcaCut(float);
    void setPtCut(float);
    void setFitPointsCut(int);
    void setPathLengthCut(float);
    void setPadPlaneCut(float);
    void setRadiatorCut(float);

    void Clear(Option_t *option="");
    void DefineSaveDirectory(char*);

    //
    // analysis parameters and flags
    //
    void setFileName(char *);
    void setWavelengthRange(double, double);
    void setTrackRefit(bool);
    void drawPadPlane(StEvent*, bool );
  
    //
    // Diagnostic and output
    //
    void fillEvtNtuple(StEvent* , int, int, const StSPtrVecRichHit* , const StSPtrVecRichPixel* );
    void fillHitNtuple(const StSPtrVecRichHit* , const StSPtrVecRichCluster* );
    void fillPIDNtuple();

    void fillOverLapHist(const StSPtrVecRichHit*);

    void fillCorrectedNtuple();

    // StEvent
    void fillPIDTraits(StRichRingCalculator*);
    bool reprocessTheTraits(StRichPidTraits*);
    void fillRichSoftwareMonitor(StEvent*);

#ifdef  myRICH_WITH_MC
    void fillMcTrackNtuple(const StSPtrVecRichCluster*);
    void fillMcPhotonNtuple(StMcEvent*, const StSPtrVecRichCluster*, const StSPtrVecRichHit*);
    void fillMcPixelNtuple(const StSPtrVecRichPixel*);
    void fillGeantHitNtuple();

    void getGeantPhotonInfo(StRichMCTrack* richTrack, StMcTrack* photon, float& wave, float& gpsi, float& z);
  
    bool makeTrackAssociations(StMcEvent*, const StSPtrVecRichHit* hits); 

    StMcTrack*     getStMcTrack(StRichMCHit* , StMcEvent* , St_g2t_track*);
    StThreeVectorF getTheGeantHitOnPadPlane(StMcTrack*, StThreeVectorF&);
#endif

    void clearTrackList();
  
  
#ifdef RICH_WITH_L3_TRACKS
  double findL3ZVertex(globalTrack *,int);
#endif    
  
  ClassDef(StRichPIDMaker,1)
    };

#endif
