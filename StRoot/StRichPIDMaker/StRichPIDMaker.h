/**********************************************************
 * $Id: StRichPIDMaker.h,v 2.3 2000/10/02 23:06:33 horsley Exp $
 *
 * Description:
 *  StRrsMaker is the main module
 *  StRichRawData. It has the standard Maker functions:
 *
 *  $Log: StRichPIDMaker.h,v $
 *  Revision 2.3  2000/10/02 23:06:33  horsley
 *  *** empty log message ***
 *
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
#include "StRichMcSwitch.h"
// Millers Track Utilities
#include "TpcHitVecUtilities.h"
 *  many additions, added features to pad plane display (MIPS, rings, etc)

#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichMomentumTransform.h"
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 **********************************************************/

#include "StRichTrackingControl.h"
#define StRichPIDMaker_HH

#include "StMaker.h"
#include "TNtuple.h"
#include "TH3.h"
#include "TFile.h"
#include "TFile.h"
using std::vector;
#endif

//#include "StRichMcSwitch.h"
#include "StThreeVectorD.hh"
//#include "StRichTrackFilter.h"
#include "StRichTrackingControl.h"
#include "StRichMcSwitch.h"
class StParticleDefinition;
#include "StDaqLib/L3/L3.Banks.hh"
class StRichMaterialsDb;
// SCL

class StRichTrack;
// StRrs
class StRichGeometryDb;
class StEvent;
class StRichMomentumTransform;

// StRch
class StRichSimpleHitCollection;

// StDisplay
class StRichPadMonitor;



// StEvent
class StEvent;
class StSPtrVecRichHit;
  Bool_t drawinit;
  Char_t collectionName[256];  
  vector<StRichTrack* > mListOfStRichTracks; //!
  int id;
  
  bool   printThisEvent;
  double pionSaturatedArea,kaonSaturatedArea,protonSaturatedArea;

  float mMagField;
  int mEventN;
  double vz;
  StThreeVectorF mVertexPosition;
  int nprimaries;
  int nnegprimaries;
  int mNumberOf1GeV;
  int mNumberOfRingHits;
  int nrichtracks;
  
  
  double mShortWave;
  double mLongWave;
    // Track
  double mDefaultShortWave;
  double mDefaultLongWave;
    TFile*   file;          //!
  // hit filter
  double innerDistance,outerDistance,meanDistance;
  double innerAngle,outerAngle,meanAngle; 
  double ringWidth;
  


  int   evtN;
  float mPtCut;
  int mAdcCut;
  float mPathCut;
  float mPadPlaneCut;
  float mRadiatorCut;
  
  char* fileName; //!
  char* mySaveDirectory; //!
  bool  mRefit;

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

  TNtuple* hitNtup;  //!
  TNtuple* evtNtup;  //!
  TNtuple* closeHitNtup;   //!


  TH3F*    pionResid;      //!
  TH3F*    pionThetab;      //!
  TH3F*    pionPsi;        //!
  TH3F*    pionTheta_xb;    //! 
  TH3F*    pionTheta_yb;    //!
  TH3F*    pionPsib;        //!
  

  TH3F*    psiHist;        //!

  TNtuple* trackCorrectedNtuple;   //!


  TH3F*    pionCorrectedResid;      //!
  TH3F*    pionCorrectedResid_x;    //! 
  TH3F*    pionCorrectedResid_y;    //!
  TH3F*    pionCorrectedTheta_xb;    //! 
  TH3F*    pionCorrectedTheta_yb;    //!


  TH3F*    pionCorrectedPsib;        //!
    TH3F*    pionPsi;        //!  
  TpcHitVecUtilities* util; //!
  bool     kWriteNtuple; //!

  TNtuple*    psiFixNtuple;      //!
    TH3F*    pionCorrectedResidb;      //!
    TH3F*    pionCorrectedResid_yb;    //!
protected:

    TH3F*    pionCorrectedThetab;      //!
    TH3F*    pionCorrectedTheta_xb;    //! 
  StRichPIDMaker(const Char_t *name="RICHPID", bool writeNtuple=false);
  virtual ~StRichPIDMaker();
  virtual void setAdcCut(int);
  virtual void setPathLengthCut(float);
  virtual void setPadPlaneCut(float);
  virtual void setRadiatorCut(float);


  virtual void Clear(Option_t *option="");
  virtual void DefineSaveDirectory(char*);
  virtual void setTrackRefit(bool);
  virtual void drawPadPlane(StEvent*, bool );
  virtual void setWavelengthRange(double, double);
    TH3F*    pionCorrectedPsib;        //!
  virtual void hitFilter(const StSPtrVecRichHit*, StRichRingCalculator* );
  virtual void hitFilter(StRichRingCalculator* , StThreeVectorF&, float&, float&);
  
  virtual float getHitSigma(float ); 
  
  virtual void tagMips(StEvent*, const StSPtrVecRichHit*);
    bool     kWriteNtuple;    //!

  virtual void fillHitNtuple(const StSPtrVecRichHit* , const StSPtrVecRichCluster* );
  virtual void fillPIDNtuple();
  virtual void fillEvtNtuple(StEvent* , int, int, const StSPtrVecRichHit* , const StSPtrVecRichPixel* );

  virtual void fillOverLapHist(const StSPtrVecRichHit*);

  virtual void fillCorrectedNtuple();
  
  virtual void fillPIDTraits(StRichRingCalculator*);
  virtual void fillRichSoftwareMonitor(StEvent*);
    // Diagnostic and output
    //
  virtual void fillMcTrackNtuple(const StSPtrVecRichCluster*);
  virtual void fillMcPhotonNtuple(StMcEvent*, const StSPtrVecRichCluster*, const StSPtrVecRichHit*);
  virtual void fillMcPixelNtuple(const StSPtrVecRichPixel*);
  virtual void fillGeantHitNtuple();

  virtual void getGeantPhotonInfo(StRichMCTrack* richTrack, StMcTrack* photon, float& wave, float& gpsi, float& z);
  
  virtual bool makeTrackAssociations(StMcEvent*, const StSPtrVecRichHit* hits); 
  virtual StMcTrack*     getStMcTrack(StRichMCHit* , StMcEvent* , St_g2t_track*);
  virtual StThreeVectorF getTheGeantHitOnPadPlane(StMcTrack*, StThreeVectorF&);
    void fillRichSoftwareMonitor(StEvent*);
  
    

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
