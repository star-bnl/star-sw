/**********************************************************
 * $Id: StRichPIDMaker.h,v 2.1 2000/08/13 01:25:58 gans Exp $
 *
 * Description:
 *  StRrsMaker is the main module
 *  StRichRawData. It has the standard Maker functions:
 *
 *  $Log: StRichPIDMaker.h,v $
 *  Revision 2.1  2000/08/13 01:25:58  gans
 *  Added directory changing when using pidMaker->printCanvas("directory/")
 *
 *  Revision 2.0  2000/08/09 16:26:19  gans
 *  Naming Convention for TDrawable Ojects. All drawable objects now in StRichDisplayMaker
 *
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
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
#include "TpcHitVecUtilities.h"
 *  many additions, added features to pad plane display (MIPS, rings, etc)

#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichMomentumTransform.h"
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...

#ifdef RICH_WITH_L3_TRACKS
#include "StDaqLib/L3/L3.Banks.hh"
#endif

 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 **********************************************************/
#include "StRichTrackingControl.h"
#define StRichPIDMaker_HH

class StEvent;
#include "TFile.h"
using std::vector;
#endif

//#include "StRichMcSwitch.h"
#include "StRichTrackingControl.h"
#include "StRichMcSwitch.h"
class StRichSimpleHitCollection;

// StDisplay
class StRichPadMonitor;

  Char_t collectionName[256];

   
  // containers of particles, tracks
  vector<StParticleDefinition* > mListOfPositiveParticles; //!
  vector<StParticleDefinition* > mListOfNegativeParticles; //!
// StEvent
  vector<StRichTrack* > mListOfStRichTracks; //!
  vector<StTrack* >     mListOfStTracks; //!
  vector<StRichTrack* > mListOfStRichTracks; //!

  // track filter
  StRichTrackFilter trackFilter; //!
  int mNumberOfRingHits;
  double mShortWave;
  double innerDistance,outerDistance;
  double innerAngle,outerAngle; 
  double mDefaultShortWave;
  double mDefaultLongWave;
  int evtN;
  char * fileName; //!

    unsigned short mUseL3Tracking;
    unsigned short mUsePrintCanvas;
    char mUsePrintCanvasDir[200];
    unsigned short mUseResidNTup;
  // analysis 
  TNtuple* mPidNtuple;
  TFile*   file;
  float mPadPlaneCut;
  float mRadiatorCut;
    StPionMinus*  pionminus;//!
    StKaonMinus*  kaonminus;//!
    StAntiProton* antiproton;//!

    StPionPlus*  pionplus;//!
    StKaonPlus*  kaonplus;//!
    StProton*    proton;//!

    // primaryTrack bined in eta from less than -1 to greater than 1,
    // in steps of .25 eta
    long mPrimaryTracksVEta[10];
  TpcHitVecUtilities* util; //!



  StRichPIDMaker(const Char_t *name="RICHPID");
    TH3F*    pionCorrectedResid_yb;    //!
  virtual void  Clear(Option_t *option="");
    TH3F*    pionCorrectedThetab;      //!
    TH3F*    pionCorrectedTheta_xb;    //! 
  StRichPIDMaker(const Char_t *name="RICHPID", bool writeNtuple=false);

  Int_t hitFilter(StThreeVector<double>& hit, StRichRingCalculator* ringCalculator, double& ang, double& dist, double cut, double& meanD);
  
    void setFileName(char *);
    void useL3Tracking();
    void usePrintCanvas(const char * directory = "");
    void useResidNTup();
    long numStPrimaryTracks(StEvent*);
    void fillRichSoftwareMonitor(StEvent*);
    double findL3ZVertex(globalTrack *,int);
    
    vector<StRichTrack* >& getListOfStRichTracks();
    vector<StTrack* >&     getListOfStTracks();
    
    ClassDef(StRichPIDMaker,1)
	};
    void fillGeantHitNtuple();







#endif

    void clearTrackList();
  
  
#ifdef RICH_WITH_L3_TRACKS
  double findL3ZVertex(globalTrack *,int);
#endif    
  
  ClassDef(StRichPIDMaker,1)
    };

#endif
