/*!
 * \class StTrack2FastDetectorMatcher
 * \author Jan Balewski, July 2004
 * $Id: StTrack2FastDetectorMatcher.h,v 2.3 2013/01/18 15:03:37 fisyak Exp $
 *
 */
#include "TObject.h"
#include "StPhysicalHelixD.hh" // dongx
class StiTrack2FastDetector;
class VertexData;
class TGraphErrors;
class StEEmcDb;
class StEvent;
class EEmcGeomSimple;
class StBTofGeometry; // dongx
#include "StPhiEtaHitList.h"
struct StiTrack2FastDetector {
  StiTrack2FastDetector() {btofBin = ctbBin = bemcBin = eemcBin = -1; anyMatch=anyVeto=kFALSE; mBtof=mCtb=mBemc=mEemc=mTpc=0; weight = 1;}
  // 3-stat logic: 1=match, -1=veto, 0=dunno
  Int_t mBtof,mCtb,mBemc,mEemc,mTpc; 
  Bool_t anyMatch,anyVeto;
  Float_t weight; // compound from all maching tests
  Int_t btofBin; // >=0 if track passed through BTOF cell
  Int_t ctbBin;  // >=0 if track passed through CTB slat
  Int_t bemcBin; // >=0 if track passed through BTOW tower
  Int_t eemcBin; // >=0 if track passed through ETOW tower
  void updateAnyMatch(Bool_t match, Bool_t veto,Int_t & mXXX) {
    if(match) {
      anyMatch=kTRUE;
      anyVeto=kFALSE;
      mXXX=1;
      //  } else if(veto && (!anyMatch) ) {
    } else if(veto && (!match) ) { // dongx
      anyVeto=kTRUE;
      mXXX=-1;
    } else {
      mXXX=0;
    }
  }
};

class StTrack2FastDetectorMatcher: public TObject {
 public:
  StTrack2FastDetectorMatcher();
  virtual  ~StTrack2FastDetectorMatcher();
  void fillArrays(StEvent *);
  void matchTrack2BTOF(const StPhysicalHelixD* hlx, StiTrack2FastDetector *t);  // dongx
  void matchTrack2CTB(const StPhysicalHelixD* hlx, StiTrack2FastDetector *t);
  void matchTrack2EEMC(const StPhysicalHelixD* hlx, StiTrack2FastDetector *t, Float_t z);
  void matchTrack2BEMC(const StPhysicalHelixD* hlx, StiTrack2FastDetector *t, Float_t rxy);
  void matchTrack2FastDetectors(const StPhysicalHelixD *hlx,StiTrack2FastDetector *t);
  virtual void  Clear(const Char_t *opt=""); 
  enum {kSwitchOneHighPT=1}; 
 private:
  Int_t  mTotEve;
  Int_t  eveID;
  // params
  Float_t  mMinZBtof;       // BTOF local z min cut - dongx
  Float_t  mMaxZBtof;       // BTOF local z max cut - dongx
  Float_t  mMinAdcBemc;     // BEMC towers with MIP response
  Float_t  mMinAdcEemc;     // EEMC towers with MIP response
  Bool_t   isMC; 
  // util
  StBtofHitList    *btofList;  // dongx
  StCtbHitList     *ctbList;
  StBemcHitList    *bemcList;
  StEemcHitList    *eemcList;
  StBTofGeometry *btofGeom;  // dongx btofGeometry
  StEEmcDb       *eeDb;
  EEmcGeomSimple *geomE;
  ClassDef(StTrack2FastDetectorMatcher,0)
};

/***************************************************************************
 *
 * $Log: StTrack2FastDetectorMatcher.h,v $
 * Revision 2.3  2013/01/18 15:03:37  fisyak
 * Fix TrackData data name clash with StiPPVertexFinder
 *
 * Revision 2.2  2013/01/17 15:57:26  fisyak
 * Add handles for debugging
 *
 * Revision 2.1  2012/05/07 14:56:14  fisyak
 * Add StKFVertexMaker
 *
 **************************************************************************/

