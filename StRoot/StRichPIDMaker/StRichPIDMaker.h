/**********************************************************
 * $Id: StRichPIDMaker.h,v 1.1 2000/04/03 19:36:08 horsley Exp $
 *
 * Description:
 *  StRrsMaker is the main module
 *  StRichRawData. It has the standard Maker functions:
 *
 *  $Log: StRichPIDMaker.h,v $
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 *
 *  
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
 **********************************************************/
#include "StRichTrackingControl.h"
#define StRichPIDMaker_HH

#include "TFile.h"
using std::vector;
//#include "StRichMcSwitch.h"
#include "StRichTrackingControl.h"
#include "StRichMcSwitch.h"
class StRichSimpleHitCollection;

// StDisplay
class StRichPadMonitor;
  StThreeVectorD  mRichGlobalRadiatorPoint; 
  StThreeVectorD  mRichNormalVector; 

  StRichGeometryDb*  mRichGeometryDb; //!

  Char_t collectionName[256];
  // containers of particles, tracks
  vector<StTrack* > mListOfStTracks; //!
// StEvent
  vector<StRichTrack* > mListOfStRichTracks; //!

  // hit collection
  double mLongestPathLength;
  double mMinimumNumberOfTrackPoints;
  StRichTrackFilter trackFilter; //!
  int mNumberOfRingHits;
  double mShortWave;
   
  TNtuple* rings;
  TNtuple* photNtup;
  TFile*   file;
  float mPadPlaneCut;
  TpcHitVecUtilities* util; //!



  StRichPIDMaker(const Char_t *name="RICHPID");
    TH3F*    pionCorrectedResid_yb;    //!
  Int_t hitFilter( StThreeVector<double>& hit, StRichRingCalculator& ringCalculator);
  virtual void  Clear(Option_t *option="");

  vector<StTrack* >& getListOfStTracks();

ClassDef(StRichPIDMaker,1)
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
