//
// $Id: StTbyTMaker.h,v 1.2 2009/11/10 20:57:28 fisyak Exp $
//
#ifndef STAR_St_TrackMate_Maker
#define STAR_St_TrackMate_Maker
#include "Riostream.h"
#include "StMaker.h"
#include <map>
#include <vector>
#include "StThreeVectorF.hh"
#include "TH2.h"
#include "TFile.h"
#include "TTree.h"
#include "TString.h"
class StSPtrVecTrackNode;
class StTpcHitCollection;
class StTrack;
class StGlobalTrack;
class StTpcHit;
typedef multimap<const StTpcHit*,StGlobalTrack*> Hit2TrackMap;
typedef multimap<StGlobalTrack*,StGlobalTrack*>  Track2TrackMap;
typedef map<const StTpcHit*,const StTpcHit*>     Hit2HitMap;
typedef pair<const StTpcHit*,StGlobalTrack*>     HitTrackPair;
typedef pair<StGlobalTrack*,StGlobalTrack*>      TrackTrackPair;
typedef Hit2TrackMap::iterator                   Hit2TrackIter;
typedef pair<Hit2TrackIter, Hit2TrackIter>       Hit2Track2Iter;
typedef Track2TrackMap::iterator                 Track2TrackIter;
typedef pair<Track2TrackIter, Track2TrackIter>   Track2Track2Iter;
class TrackParameters : public TObject {
 public:
  TrackParameters() {set(); MatchStatus = -1;}
  virtual ~TrackParameters() {}
  void set() {memset(&begin, 0, &end-&begin);}
  Bool_t IsEmpty()    {return MatchStatus == 0;}
  Bool_t IsMatch()    {return MatchStatus & 1;}
  Bool_t IsClone()    {return MatchStatus & 2;}
  Bool_t IsSplitted() {return MatchStatus & 4;}
  Bool_t IsLost()     {return MatchStatus & 8;}
  Int_t  charge()     {return (Charge > 0) ? 0 : 1;}
  Char_t begin;
  Int_t   Id;
  Float_t PtGl;
  Float_t EtaGl;
  Float_t PhiGl;
  Float_t PGl;
  Float_t FitPtsGl;
  Float_t PtPr;
  Float_t EtaPr;
  Float_t PhiPr;
  Float_t PPr;
  Float_t FitPtsPr;
  Float_t Dedx;
  Float_t Charge;
  Float_t Prim;
  Float_t Chi2Gl0;
  Float_t Chi2Gl1;
  Float_t Chi2Pr0;
  Float_t Chi2Pr1;
  Float_t FirstPointX;
  Float_t FirstPointY;
  Float_t FirstPointZ;
  Float_t LastPointX;
  Float_t LastPointY;
  Float_t LastPointZ;
  Float_t PrimX;
  Float_t PrimY;
  Float_t PrimZ;
/*   Float_t SecF;  // first and last hit sector */
/*   Float_t SecL; */
  Int_t   hitMap; // HFT hits pxl + 10 * ist + 100 *Ssd
  Int_t   MatchStatus; // 1 - ReCo (1 to 1 match), 2 - CLone, 4 - Splitted, 8 - Lost 
  Char_t end;
  ClassDef(TrackParameters,3)
};
class TrackMatch : public TObject {
 public:
 TrackMatch() {}
 virtual ~TrackMatch() {}
  TrackParameters newP;
  TrackParameters oldP;
  ClassDef(TrackMatch,3)
};

class HitParameters : public TObject {
 public:
  HitParameters() {set();}
  virtual ~HitParameters() {}
  void set() {memset(&begin, 0, &end-&begin);}
  Char_t begin;
  Int_t    sector, row;
  Float_t  x,y,z,q;
  Int_t    adc;
  Float_t pad,timebucket;
  Int_t    npads, ntbks, IdTruth;
  Float_t  xL,yL,zL,dX;
  Int_t    trigId, us,fl;
  Float_t  time, timeb;
  Char_t end;
  virtual void Print(Option_t *option="") const {
    cout << Form("HitP s/r %3i/%3i ",sector,row);
    cout << Form(" fl%3i us %1i", fl, us)
	 << Form(" xyz:%10.3f%10.3f%10.3f xyzL:%10.3f%10.3f%10.3f",x,y,z,xL,yL,zL)
	 << Form(" dX %5.2f",dX)
	 << Form(" tm %6.2f pad %6.2f adc %5i",timebucket,pad, adc)
	 << Form(" IdT%6i,npad%3i,ntbks%3i",IdTruth,npads,ntbks)
	 << endl;
  }
  ClassDef(HitParameters,3)
};
class HitMatch : public TObject {
 public:
 HitMatch() {}
 virtual ~HitMatch() {}
  HitParameters newP;
  HitParameters oldP;
  ClassDef(HitMatch,3)
};

struct StTrackPing {
StTrackPing(StTrack *t=0, UInt_t ping=0) : mTrack(t), mNPings(ping) {};
  StTrack*  mTrack;
  unsigned int    mNPings;
};

bool compStTrackPing(const StTrackPing& rhs,const StTrackPing& lhs);
class StTbyTMaker : public StMaker {
 public: 
 StTbyTMaker(const char *name="TrackMate") : StMaker(name),
    trackTree(0), trackBr(0), fTrackMatch(0), hitTree(0), hitBr(0), fHitMatch(0)  {}
  ~StTbyTMaker() {}
  Int_t  Init();
  virtual void Clear(const char* opt="");
  Int_t  Make();
  size_t buildRecHitTrackMap(const StSPtrVecTrackNode& nodes,Hit2TrackMap& htMap);
  void   buildHit2HitMaps(const StTpcHitCollection *tpchitcoll1, const StTpcHitCollection *tpchitcoll2,
			  Hit2HitMap        &Hit1ToHit2,Hit2HitMap        &Hit2ToHit1);
  void   buildTrack2TrackMap(const StSPtrVecTrackNode &trackNodes1, 
			     const StSPtrVecTrackNode &trackNodes2, 
			     Hit2HitMap &Hit1ToHit2, 
			     Hit2TrackMap &hitTrackMap2,
			     Track2TrackMap &Track1ToTrack2);
  void    checkConsistency(Track2TrackMap &Track1ToTrack2, Track2TrackMap &Track2ToTrack1);
  Bool_t  GoodTrack(StTrack* trk);
  Bool_t  GoodMatch(StTrack* trk1, StTrack* trk2, UInt_t NPings);
  TrackParameters TrackParametersFill(const StGlobalTrack *gTrack = 0);
  HitParameters HitParametersFill(const StTpcHit *tpcHit = 0);
 private:
  void FillMatch(const StGlobalTrack* trk1, const StGlobalTrack* trk2 = 0);
  void FillMatch(const StTpcHit* hit1, const StTpcHit* hit2 = 0);
  TTree* trackTree;
  TBranch* trackBr;
  TrackMatch *fTrackMatch;
  TTree* hitTree;
  TBranch* hitBr;
  HitMatch *fHitMatch;
  virtual const char *GetCVS() const {
    static const char cvs[]= "Tag $Name:  $ $Id: StTbyTMaker.h,v 1.2 2009/11/10 20:57:28 fisyak Exp $ built __DATE__ __TIME__" ; 
    return cvs;
  }
  ClassDef(StTbyTMaker, 1)   //StAF chain virtual base class for Makers
};
#endif
	

