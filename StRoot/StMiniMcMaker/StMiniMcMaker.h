/**
 * $Id: StMiniMcMaker.h,v 1.8 2004/01/26 13:59:26 calderon Exp $
 * \file  StMiniMcMaker.h
 * \brief Filling of StMiniMcEvent classes from StMcEvent, StEvent, StAssociationMaker
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 *  
 * Fills the mDst of the association maker results.
 * basically an amalgamation of the flow maker and
 * manuel calderon de la barca's code.
 *
 * $Log: StMiniMcMaker.h,v $
 * Revision 1.8  2004/01/26 13:59:26  calderon
 * Added the code to fill the global track matches of StMiniMcEvent.
 *
 * Revision 1.7  2003/09/10 19:47:23  perev
 * ansi corrs
 *
 * Revision 1.6  2003/07/09 01:07:23  calderon
 * Addition of FTPC reference multiplicity
 * Addition of other multiplicity values for StMiniMcEvent
 * Changes to reflect the use of the setters and getters, no longer
 * access the data members directly.
 *
 * Revision 1.5  2002/06/28 22:15:12  calderon
 * Changes to deal with seg. faults in the file name handling:
 * Conventions:
 * StMiniMcMaker looks for the input file from the IO maker to figure out
 * if the file has changed.  This is done using TString::Contains() in Make().
 * Usually we will run one file at a time, but in order not to break Bum's scheme of being
 * able to process several files in one go, this is left as is.  However, for
 * embedding, the file name is not enough, in Eric's new scheme there are repeated
 * file names.  This is resolved by adding a prefix to the output file name.  However,
 * this prefix should not be overwritten, so the current code only replaces the
 * string inside the output file name pertaining to the input file name, and leaves
 * the prefix of the output file intact.  This was done for embedding looking for
 * st_physics, and here is where the problem arose: hijing files begin with a different
 * prefix.  To solve this problem, the input file name prefix is now an input parameter
 * in the macro.
 *
 * StMiniEmbed.C and StMiniHijing.C now conform to this convention.  StMiniEmbed.C
 * did not change its prototype, because all embedding files have st_phyics as prefix.
 * StMiniHijing.C changed its prototype, now it takes as an input argument the prefix,
 * but in order not to break Jenn's scripts if she was already using this macro,
 * this parameter was added at the end and defaults to "rcf", which is appropriate
 * for hijing files reconstructed in rcf.
 *
 * Revision 1.4  2002/06/07 02:22:00  calderon
 * Protection against empty vector in findFirstLastHit
 * $Log: StMiniMcMaker.h,v $
 * Revision 1.8  2004/01/26 13:59:26  calderon
 * Added the code to fill the global track matches of StMiniMcEvent.
 *
 * Revision 1.7  2003/09/10 19:47:23  perev
 * ansi corrs
 *
 * Revision 1.6  2003/07/09 01:07:23  calderon
 * Addition of FTPC reference multiplicity
 * Addition of other multiplicity values for StMiniMcEvent
 * Changes to reflect the use of the setters and getters, no longer
 * access the data members directly.
 *
 * Revision 1.5  2002/06/28 22:15:12  calderon
 * Changes to deal with seg. faults in the file name handling:
 * Conventions:
 * StMiniMcMaker looks for the input file from the IO maker to figure out
 * if the file has changed.  This is done using TString::Contains() in Make().
 * Usually we will run one file at a time, but in order not to break Bum's scheme of being
 * able to process several files in one go, this is left as is.  However, for
 * embedding, the file name is not enough, in Eric's new scheme there are repeated
 * file names.  This is resolved by adding a prefix to the output file name.  However,
 * this prefix should not be overwritten, so the current code only replaces the
 * string inside the output file name pertaining to the input file name, and leaves
 * the prefix of the output file intact.  This was done for embedding looking for
 * st_physics, and here is where the problem arose: hijing files begin with a different
 * prefix.  To solve this problem, the input file name prefix is now an input parameter
 * in the macro.
 *
 * StMiniEmbed.C and StMiniHijing.C now conform to this convention.  StMiniEmbed.C
 * did not change its prototype, because all embedding files have st_phyics as prefix.
 * StMiniHijing.C changed its prototype, now it takes as an input argument the prefix,
 * but in order not to break Jenn's scripts if she was already using this macro,
 * this parameter was added at the end and defaults to "rcf", which is appropriate
 * for hijing files reconstructed in rcf.
 * and $Id: StMiniMcMaker.h,v 1.8 2004/01/26 13:59:26 calderon Exp $ plus header comments for the macros
 *
 */

#ifndef StMiniMcMaker_H
#define StMiniMcMaker_H

#include "StMaker.h"

#include <vector>
#include <utility>
#include <map>

#include "TString.h"

#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"

class StMiniMcEvent;
class StMiniMcPair;
class StTinyMcTrack;
class StTinyRcTrack;
class StContamPair;

class TFile;
class TTree;
class StRun;
class StEvent;
class StMcEvent;
class StMcTrack;
class StTrack;
class StPrimaryTrack;
class StIOMaker;
class StThreeVectorF;
class StTpcDedxPidAlgorithm;
class StuProbabilityPidAlgorithm;
class StTpcHit;
class StDedxPidTraits;

// typdef's
#ifndef __CINT__

typedef map<UInt_t,Int_t> RCFOUNDMAP;
typedef map<long,Int_t> MCFOUNDMAP;
typedef vector<StTrackPairInfo*> PAIRVEC;
typedef pair<StTpcHit*,StTpcHit*> PAIRHIT;
inline bool pairCmp(StTrackPairInfo* p1, StTrackPairInfo* p2){
  return p1->commonTpcHits() < p2->commonTpcHits();
}
inline bool sortCmp(StTrackPairInfo* p1, StTrackPairInfo* p2){
  return p1->commonTpcHits() > p2->commonTpcHits();
}

#endif
bool hitCmp(StTpcHit* p1, StTpcHit* p2);


// check StMiniMcEvent for the track categories...

//____________________________________________

class StMiniMcMaker : public StMaker{
 public:
  StMiniMcMaker(const Char_t* name="StMiniMcMaker",
		const Char_t* title="event/StMiniMcMaker");
  virtual ~StMiniMcMaker();

  void  Clear(Option_t *option="");
  Int_t Init();
  Int_t InitRun(int runnumber);
  Int_t Make();
  Int_t Finish();

  //---- SETS -------

  void  setGhost(Bool_t doit=kTRUE)      { mGhost = doit; }
  void  setDebug(Bool_t debug=kTRUE)     { mDebug = debug; }
  void  setOutDir(const char* dir= "./") { mOutDir = dir; }  
  void  setPtCut(Float_t minPt=0, Float_t maxPt=9999) 
    { mMinPt=minPt; mMaxPt=maxPt; }
  void  setFileName(TString& val)      { mInFileName = val; }
  void  setFilePrefix(TString& val)      { mInFilePrefix = val; }

 private:
  //static const Float_t mSharedHitsCut = .5;

  // methods
  
  Bool_t           initAssociation();  // sets all the assoc maps
  Bool_t           initVertex(); // finds the primary vertex if it exists
  Bool_t           acceptRaw(StMcTrack*);
  Bool_t           accept(StMcTrack*);
  Bool_t           accept(StTrack*);
  Bool_t	   acceptGood20(StTrack*);
  Bool_t	   acceptGood20(StMcTrack*);
  Bool_t           acceptCentrality(StTrack*);
  Bool_t           acceptUncorrected(StTrack*);
  Bool_t           acceptFTPC(StTrack*);
  Bool_t           ok(StTrack*);
  Bool_t           isSameSign(StTrack*,StMcTrack*);
  Bool_t           acceptPt(StTrack*);
  Bool_t           acceptPt(StMcTrack*);
  Bool_t           acceptDebug(StMcTrack*);
  
  StTrackPairInfo* findBestMatchedGlobal(StMcTrack*);
  PAIRVEC          findMatchedRc(StMcTrack*);
  PAIRHIT          findFirstLastHit(const StTrack*);
  PAIRHIT          findFirstLastFitHit(const StTrack*);

  Float_t          computeXY(const StThreeVectorF*, const StTrack*);
  StDedxPidTraits* findDedxPidTraits(const StTrack*);
  //pair<Float_t,Float_t>  computeProj(const StThreeVectorF*,const StTrack*);
  Float_t          computeZDca(const StThreeVectorF*,const StTrack*);

  StPrimaryTrack*  isPrimaryTrack(StTrack*);
  Bool_t           isPrimaryTrack(StMcTrack*);
  Int_t            openFile();
  Int_t            closeFile();
  void             trackLoop();
  void             fillEventInfo(Int_t nGoodTrack, Int_t nRcGlobal, Int_t nRcGoodGlobal20,
				 //Int_t nAcceptedRaw,
				 Int_t nMcGlobal, Int_t nMcGoodGlobal20,
				 Int_t nMcNch, Int_t nMcHminus,
				 Int_t nMcFtpcENch, Int_t nMcFtpcWNch,
				 Int_t nFtpcEUncorrected, Int_t nFtpcWUncorrected);
  void             fillTrackPairInfo(StMiniMcPair*,
				     const StMcTrack*,
				     const StTrack* prTrack, 
				     const StTrack* glTrack,
				     Int_t commonHits, Int_t nAssMc,
				     Int_t nAssGl, Int_t nAssPr,
				     Bool_t isBestContam=kFALSE);
  
  void             fillRcTrackInfo(StTinyRcTrack*,
				   const StTrack* prTrack,
				   const StTrack* glTrack,
				   Int_t nAssMc);
  
  void             fillMcTrackInfo(StTinyMcTrack*,
				   const StMcTrack*,
				   Int_t nAssGl, Int_t nAssPr);

  void             checkMerged(StMcTrack* merged, Int_t mergedCommonHits,
			       StTrack* prTrack);
  void             checkSplit(StMcTrack*,StTrack*,Int_t);
  void             checkContam(StMcTrack*,StGlobalTrack*,Int_t);

  size_t           getIndex(size_t mult);
  // members
  StMiniMcEvent*   mMiniMcEvent; //! 
  StIOMaker*       mIOMaker;      //!
  TTree*           mMiniMcTree;   //!
  TFile*           mMiniMcDST;    //!
  TString          mInFileName;   //!
  TString          mInFilePrefix;   //!
  TString          mOutFileName;  //!
  TString          mOutDir;       //!
  TString          mParameterFileName; //!

  StEvent*         mRcEvent;      //!
  StMcEvent*       mMcEvent;      //!
  StRun*           mRun;          //!
  rcTpcHitMapType* mRcHitMap;     //!
  rcTrackMapType*  mRcTrackMap;   //!
  mcTrackMapType*  mMcTrackMap;   //!
  const StThreeVectorF*  mRcVertexPos;  //!
  const StThreeVectorF*  mMcVertexPos;  //!
  StTpcDedxPidAlgorithm* mTpcDedxAlgo; //!
  StuProbabilityPidAlgorithm* mPidAlgo; //!

  Bool_t           mGhost;        //!
  Bool_t           mDebug;        //!
  
  Float_t          mMinPt;        //!
  Float_t          mMaxPt;        //!

  Int_t            mNSplit; //!
  Int_t            mNRc; //!
  Int_t            mNGhost; //!
  Int_t            mNContam; //!
  Int_t            mNMatched; //!
  Int_t            mNMatGlob; //!

  ClassDef(StMiniMcMaker,0)
};
  
  

  
#endif  
//
// $Log $
//
