/**********************************************************
 * $Id: StRichPIDMaker.h,v 1.4 2000/06/16 02:37:12 horsley Exp $
 *
 * Description:
 *  StRrsMaker is the main module
 *  StRichRawData. It has the standard Maker functions:
 *
 *  $Log: StRichPIDMaker.h,v $
 *  Revision 1.4  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
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
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 **********************************************************/
#include "StRichTrackingControl.h"
#define StRichPIDMaker_HH

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
   
  TpcHitVecUtilities* util; //!



  StRichPIDMaker(const Char_t *name="RICHPID");
    TH3F*    pionCorrectedResid_yb;    //!
  virtual void  Clear(Option_t *option="");
    TH3F*    pionCorrectedThetab;      //!
    TH3F*    pionCorrectedTheta_xb;    //! 
  StRichPIDMaker(const Char_t *name="RICHPID", bool writeNtuple=false);

  Int_t hitFilter(StThreeVector<double>& hit, StRichRingCalculator* ringCalculator, double& ang, double& dist, double cut, double& meanD);
  
    void setFileName(char *);
    
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
