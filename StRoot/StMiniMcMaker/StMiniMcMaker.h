/**
 * $Id: StMiniMcMaker.h,v 1.20 2014/08/06 11:43:27 jeromel Exp $
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
 * Revision 1.20  2014/08/06 11:43:27  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.19  2012/03/15 23:37:36  perev
 * Uncorrected globals added(Chris)
 *
 * Revision 1.18  2011/07/19 19:18:23  perev
 * Error handling fixed
 *
 * Revision 1.17  2011/04/01 20:01:41  perev
 * const++
 *
 * Revision 1.16  2010/08/31 20:16:15  fisyak
 * Add track seedQuality
 *
 * Revision 1.15  2010/04/15 19:17:27  fisyak
 * Add corrections for AppendMCDaughterTrack from Masayuki Wada
 *
 * Revision 1.14  2009/02/02 19:30:50  fisyak
 * Set common Hit as no.Tpc + 100*no.Svt + 1000*no.Ssd hits, add protection against empty emcCollection
 *
 * Revision 1.13  2007/12/22 20:31:21  calderon
 * Storing of info of 3 EMC towers for each TinyRcTrack and TinyMcTrack.
 *
 * Revision 1.12  2007/02/23 17:07:41  fisyak
 * Resolve bug #682
 *
 * Revision 1.11  2005/07/06 22:23:49  fisyak
 * Use templated StThreeVectorF
 *
 * Revision 1.10  2004/03/30 03:16:15  calderon
 * Modifications for running in bfc.
 *  - Changed to use StiIOInterface (IOMaker in normal mode, TreeMaker in bfc)
 *  - Cleaned up Init(), InitRun() to handle the changing file names.
 *  - Initialize lots of variables and pointers in constructor.
 *  - Delete some pointers in Finish (deleting the TTree causes a seg fault, though.)
 *  - Note that currently the StHits in the ITTF chain don't have a usedInFit() flag,
 *    so there will be many messages complaining about this.
 *  - Removed the mDebug data member, every Maker already has one, so change
 *    to use that throughout the package.
 *
 * Revision 1.9  2004/03/15 18:59:47  calderon
 * - Added support for encoded common hits.  Now the common hits of the TPC and
 * the SVT are stored, with the corresponding methods to decode and return these
 * values.
 * - Added protection for tracks with no particle definition.
 * - Added () around call to mcMergedPair[i] to make Insure++ happy.
 *
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
 * Revision 1.20  2014/08/06 11:43:27  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.19  2012/03/15 23:37:36  perev
 * Uncorrected globals added(Chris)
 *
 * Revision 1.18  2011/07/19 19:18:23  perev
 * Error handling fixed
 *
 * Revision 1.17  2011/04/01 20:01:41  perev
 * const++
 *
 * Revision 1.16  2010/08/31 20:16:15  fisyak
 * Add track seedQuality
 *
 * Revision 1.15  2010/04/15 19:17:27  fisyak
 * Add corrections for AppendMCDaughterTrack from Masayuki Wada
 *
 * Revision 1.14  2009/02/02 19:30:50  fisyak
 * Set common Hit as no.Tpc + 100*no.Svt + 1000*no.Ssd hits, add protection against empty emcCollection
 *
 * Revision 1.13  2007/12/22 20:31:21  calderon
 * Storing of info of 3 EMC towers for each TinyRcTrack and TinyMcTrack.
 *
 * Revision 1.12  2007/02/23 17:07:41  fisyak
 * Resolve bug #682
 *
 * Revision 1.11  2005/07/06 22:23:49  fisyak
 * Use templated StThreeVectorF
 *
 * Revision 1.10  2004/03/30 03:16:15  calderon
 * Modifications for running in bfc.
 *  - Changed to use StiIOInterface (IOMaker in normal mode, TreeMaker in bfc)
 *  - Cleaned up Init(), InitRun() to handle the changing file names.
 *  - Initialize lots of variables and pointers in constructor.
 *  - Delete some pointers in Finish (deleting the TTree causes a seg fault, though.)
 *  - Note that currently the StHits in the ITTF chain don't have a usedInFit() flag,
 *    so there will be many messages complaining about this.
 *  - Removed the mDebug data member, every Maker already has one, so change
 *    to use that throughout the package.
 *
 * Revision 1.9  2004/03/15 18:59:47  calderon
 * - Added support for encoded common hits.  Now the common hits of the TPC and
 * the SVT are stored, with the corresponding methods to decode and return these
 * values.
 * - Added protection for tracks with no particle definition.
 * - Added () around call to mcMergedPair[i] to make Insure++ happy.
 *
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
 * and $Id: StMiniMcMaker.h,v 1.20 2014/08/06 11:43:27 jeromel Exp $ plus header comments for the macros
 *
 */

#ifndef StMiniMcMaker_H
#define StMiniMcMaker_H

#include "StMaker.h"

#include <vector>
#include <utility>
#include <algorithm>
#include <map>

#include "TString.h"

#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"

#include "StMiniMcEvent/StMiniMcEvent.h"
#include "StMiniMcEvent/StMiniMcPair.h"
#include "StMiniMcEvent/StContamPair.h"

class TFile;
class TTree;
class StEvent;
class StMcEvent;
class StMcTrack;
class StTrack;
class StPrimaryTrack;
class StIOInterFace;
#include "StThreeVectorF.hh"
class StTpcDedxPidAlgorithm;
class StuProbabilityPidAlgorithm;
class StTpcHit;
class StDedxPidTraits;
class StEmcRawHit;
class StEmcPoint;

// typdef's
#ifndef __CINT__

typedef map<UInt_t,Int_t> RCFOUNDMAP;
typedef map<long,Int_t> MCFOUNDMAP;
typedef map<Long_t,const StMcTrack*> MCMAP;
typedef vector<StTrackPairInfo*> PAIRVEC;
typedef pair<const StTpcHit*,const StTpcHit*> PAIRHIT;
inline bool pairCmp(StTrackPairInfo* p1, StTrackPairInfo* p2){
  return p1->commonTpcHits() < p2->commonTpcHits();
}
inline bool sortCmp(StTrackPairInfo* p1, StTrackPairInfo* p2){
  return p1->commonTpcHits() > p2->commonTpcHits();
}

#endif
//bool hitCmp(StTpcHit* p1, StTpcHit* p2);


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
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StMiniMcMaker.h,v 1.20 2014/08/06 11:43:27 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  //---- SETS -------

  void  setGhost(Bool_t doit=kTRUE)      	{ mGhost = doit; }
  void  setOutDir(const char* dir= "./") 	{ mOutDir = dir; }  
  void  setPtCut(Float_t minPt=0, Float_t maxPt=9999) 
    						{ mMinPt=minPt; mMaxPt=maxPt; }
  void  setFileName(TString& val)      		{ mInFileName = val; }
  void  setFilePrefix(TString& val)      	{ mInFilePrefix = val; }

 private:
  //static const Float_t mSharedHitsCut = .5;

  // methods
  
  Bool_t           initAssociation();  // sets all the assoc maps
  Bool_t           initVertex(); // finds the primary vertex if it exists
  Bool_t           acceptRaw(const StMcTrack*);
  Bool_t           accept(const StMcTrack*);
  Bool_t           accept(const StTrack*);
  Bool_t	   acceptGood20(const StTrack*);
  Bool_t	   acceptGood20(const StMcTrack*);
  Bool_t           acceptCentrality(const StTrack*);
  Bool_t           acceptUncorrected(const StTrack*);
  Bool_t           acceptGlobals(const StTrack* track);
  Bool_t           acceptFTPC(const StTrack*);
  Bool_t           ok(const StTrack*);
  Bool_t           isSameSign(const StTrack*,const StMcTrack*);
  Bool_t           acceptPt(const StTrack*);
  Bool_t           acceptPt(const StMcTrack*);
  Bool_t           acceptDebug(const StMcTrack*);
  
  StTrackPairInfo* findBestMatchedGlobal(const StMcTrack*);
  PAIRVEC          findMatchedRc(const StMcTrack*);
  PAIRHIT          findFirstLastHit(const StTrack*);
  PAIRHIT          findFirstLastFitHit(const StTrack*);

  Float_t          computeXY(const StThreeVectorF*, const StTrack*);
  StDedxPidTraits* findDedxPidTraits(const StTrack*);
  //pair<Float_t,Float_t>  computeProj(const StThreeVectorF*,const StTrack*);
  Float_t          computeZDca(const StThreeVectorF*,const StTrack*);

  const StPrimaryTrack*  isPrimaryTrack(const StTrack*);
  Bool_t           isPrimaryTrack(const StMcTrack*);
  Int_t            openFile();
  Int_t            closeFile();
  void             trackLoop();
  void             trackLoopIdT();
  void             buildEmcIndexArray();
  void             fillEventInfo(Int_t nGoodTrack, Int_t nRcGlobal, Int_t nRcGoodGlobal20,
				 //Int_t nAcceptedRaw,
				 Int_t nMcGlobal, Int_t nMcGoodGlobal20,
				 Int_t nMcNch, Int_t nMcHminus,
				 Int_t nMcFtpcENch, Int_t nMcFtpcWNch,
				 Int_t nFtpcEUncorrected, Int_t nFtpcWUncorrected, 
				 Int_t nUncorrectedGlobals);
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

  void             checkMerged(const StMcTrack* merged, Int_t mergedCommonHits,
			       const StTrack* prTrack);
  void             checkSplit (const StMcTrack*,const StTrack*,Int_t);
  void             checkContam(const StMcTrack*,const StGlobalTrack*,Int_t);

  size_t           getIndex(size_t mult);

 void             AppendMCDaughterTrack();
 static void      dominatTkInfo(const StTrack* recTrack,int &dominatrackKey ,int& dominatrackHits,float& avgQuality);
  // members
  StMiniMcEvent*   mMiniMcEvent; //! 
  StIOInterFace*   mIOMaker;      //!
  TTree*           mMiniMcTree;   //!
  TFile*           mMiniMcDST;    //!
  TString          mInFileName;   //!
  TString          mInFilePrefix;   //!
  TString          mOutFileName;  //!
  TString          mOutDir;       //!
  TString          mParameterFileName; //!

  StEvent*         mRcEvent;      //!
  StMcEvent*       mMcEvent;      //!
  rcTpcHitMapType* mRcHitMap;     //!
  rcTrackMapType*  mRcTrackMap;   //!
  mcTrackMapType*  mMcTrackMap;   //!
  const StThreeVectorF*  mRcVertexPos;  //!
  const StThreeVectorF*  mMcVertexPos;  //!
  StTpcDedxPidAlgorithm* mTpcDedxAlgo; //!
  StuProbabilityPidAlgorithm* mPidAlgo; //!
  vector<StEmcRawHit*> mEmcIndex; //! Array Indexed by Soft Id [1-4800], size is then 4801.
  Bool_t           mGhost;        //!
  
  Float_t          mMinPt;        //!
  Float_t          mMaxPt;        //!

  Int_t            mNSplit; 	//!
  Int_t            mNRc; 	//!
  Int_t            mNGhost; 	//!
  Int_t            mNContam; 	//!
  Int_t            mNMatched; 	//!
  Int_t            mNMatGlob; 	//!
  int              mMainVtx;
  ClassDef(StMiniMcMaker,0)
};
  
  

  
#endif  
//
// $Log $
//
