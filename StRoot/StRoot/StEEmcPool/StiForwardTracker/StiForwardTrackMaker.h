// $Id: StiForwardTrackMaker.h,v 1.9 2014/08/06 11:43:02 jeromel Exp $

#ifndef STAR_StiForwardTrackMaker
#define STAR_StiForwardTrackMaker

/*!
 *                                                                     
 * \class  StiForwardTrackMaker
 * \author Adam & Jan

 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <vector>
class StiToolkit;
class StiKalmanTrack;
class StPrimaryVertex; //tmp

class StiHitContainer;
class StiTrackContainer;
class StiLocalTrackSeedFinder;
class StiDetectorContainer;
class StiKalmanTrackFinder;

class StiForwardTrackMaker : public StMaker {
 private:
  // Private method declaration if any
  // event realted info
  int eveID;
  
  // misc counter
  int mTotEve;
  int mTotMatchedSeeds;
  int mTotMatchedTracks;

  //.... params,cuts
  double mMaxTrkDcaRxy;   //DCA to nominal beam line for each track
  double mMaxZdca;     //zDCA cutoff for prim tracks in cm
  double mMinEta;		//eta cutoff for hits used in constructing forward tracks
 
  //..... util
  StiToolkit     *mToolkit;
  enum {mxHA=11};
  TH1F *hA[mxHA];
  class VertexV{public: float z,ez;};
  vector<VertexV> vertL;
  StiHitContainer* mAllHits;
  StiHitContainer* mForwardHits;
  StiTrackContainer* mTrackSeeds;
  StiLocalTrackSeedFinder* mSeedGenerator;
  StiKalmanTrackFinder* mTrackFinder;
  

  void initHisto();
  
  void getForwardHits(StiHitContainer* allHits, StiHitContainer* forwardHits, StiDetectorContainer* detector, double minEta);
  //stores hits from allHits with eta > minEta in forwardHits container.  anything previously in forwardHits is lost
  
  void buildTrackSeeds(const StiHitContainer* forwardHits, StiTrackContainer* trackSeeds, StiLocalTrackSeedFinder* seedGenerator);
  //takes forward hits and uses Mike's seed finder to store track segments in the trackSeeds container
  
  void extendTracks();
  //takes track seeds and extends them using Kalman
  
  void matchVertex(StiTrackContainer* tracks, vector<VertexV> &vertL, double &mMaxZdca, int &nV, TH1* h, int &totalMatched);

  bool examineTrackDca(const StiKalmanTrack *track,float &zDca, float &ezDca, float &rxyDca);
    
 public: 
  StiForwardTrackMaker(const char *name="forwTrack");
  virtual       ~StiForwardTrackMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  Int_t  MakeInSti(); // tmp, split of make on 2 parts
  Int_t  MakeAfterSti(); // tmp,
  virtual Int_t Finish();
  virtual void Clear(const char* opt);
  TObjArray * HList;
  void saveHisto(TString fname);
  void addVertex(float z, float ez); //tmp, hack Sti

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StiForwardTrackMaker.h,v 1.9 2014/08/06 11:43:02 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StiForwardTrackMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StiForwardTrackMaker.h,v $
// Revision 1.9  2014/08/06 11:43:02  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.8  2007/07/12 19:27:21  fisyak
// Add includes for TMath for ROOT 5.16
//
// Revision 1.7  2005/09/26 14:03:55  kocolosk
// added Kalman propagation for forward track seeds
//
// Revision 1.6  2005/09/21 15:37:06  kocolosk
// modified matchVertex() so that |deltaZ| could be plotted separately for old, new tracks
//
// Revision 1.5  2005/09/14 14:15:14  kocolosk
// restrict to TPC hits, added histos, moved code to matchVertex() function
//
// Revision 1.4  2005/09/13 14:24:16  kocolosk
// implemented getForwardHits() and buildTrackSeeds() functions
//
// Revision 1.3  2005/09/12 21:08:21  balewski
// split Make to InSti and AfterSti
//
// Revision 1.2  2005/09/09 15:55:00  balewski
// prototype with hardcoded hacks
//
// Revision 1.1  2005/09/08 21:42:03  balewski
// star
//
