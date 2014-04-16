//! Muon Telescope Detector (MTD) Match Maker
/*!  \class StMtdMatchMaker
 *   \brief Match Maker for the MTD detector
 *   \author Bingchu Huang
 *   \date January 2013
 *
 * The MTD MatchMaker matches STAR tracks to the MTD MRPCs.
 * 
 * $Id: StMtdMatchMaker.h,v 1.5 2014/04/16 02:23:39 huangbc Exp $
 */
/*****************************************************************
 *
 * $Log: StMtdMatchMaker.h,v $
 * Revision 1.5  2014/04/16 02:23:39  huangbc
 * 1. fixed a bug of un-initialized variable nDedxPts in MtdTrack construction function.
 * 2. reoriganized project2Mtd function. Made it more readable.
 * 3. save pathlengths of extrapolated tracks.
 * 4. tot selection < 40 ns. drop off maximum tot selection.
 * 5. add hits <-> track index association in mMuDstIn=true mode.
 *
 * Revision 1.4  2013/12/09 22:53:25  geurts
 * update: enable filling of MTD Pid traits and include a few more protections against zero-pointers [Bingchu]
 *
 * Revision 1.3  2013/11/19 22:30:34  jeromel
 * Added name
 *
 * Revision 1.2  2013/03/21 11:21:45  jeromel
 * Reviewd version 2013/03
 *
 *
 *
 *******************************************************************/
#ifndef STMTDMATCHMAKER_H
#define STMTDMATCHMAKER_H

#include <vector>
#include <string>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::string;
#endif

#include "StMaker.h"
#include "TH2.h"
#include "TF1.h"
#include "TF2.h"
#include <StPhysicalHelixD.hh>
#include "TNtuple.h"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"

class StMuDstMaker;
class StMuDst;
class StEvent;
//class StMuBTofHit;
class StTrack; 
class StMuTrack; 
class StTriggerData;
class StMtdPidTraits;
class StMuMtdPidTraits;
#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
typedef vector<Int_t>  IntVec;
typedef vector<UInt_t>  UIntVec;
typedef vector<Double_t>  DoubleVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
typedef vector<UInt_t, allocator<UInt_t>>  UIntVec;
typedef vector<Double_t, allocator<Double_t>>  DoubleVec;
#endif

const Int_t kMaxHits = 20000;
const Int_t kMaxTriggerIds = 64;
const Int_t mNBacklegs = 30;
const Int_t mNStrips = 12;
const Int_t mNAllTrays = 150; 

/// MTD Event data structure
struct MtdEvtData {
	Int_t  run, evt,nTrgIds;
	Int_t  trgId[kMaxTriggerIds];
	Float_t  bField;
	Float_t vertexX, vertexY, vertexZ;

	Double_t  triggerTime[2];
	UShort_t  mtdVpdTacDiff;
	Float_t   vpdVz;
	Float_t   tStart;
	UShort_t  lastDSM[8];
	Int_t	  prepost;
	Int_t	  pre;
	Int_t	  post;
	Float_t	  fasteast[11];
	Float_t	  fastwest[11];
	Float_t	  fasteastHi[11];
	Float_t	  fastwestHi[11];
	Float_t   Vpd[11][64];
	Float_t   VpdHi[11][64];
	Float_t   MT001[11][32];
	Float_t   MT002[11][32];
	Float_t   MT003[11][32];
	Float_t   MT004[11][32];



	/// raw hits information
	Int_t  nMtdRawHits;
	Char_t    flag[kMaxHits];
	UChar_t   backlegRaw[kMaxHits];
	UChar_t   chn[kMaxHits]; //1-120
	Double_t  tdc[kMaxHits]; // bin

	/// sorted hits information
	Int_t  nMtdHits;
	UChar_t  backleg[kMaxHits];
	UChar_t  module[kMaxHits];
	UChar_t  cell[kMaxHits];

	Double_t leTimeWest[kMaxHits];
	Double_t leTimeEast[kMaxHits];
	Double_t totWest[kMaxHits];
	Double_t totEast[kMaxHits];

	/// global tracks information
	Int_t ngTrks;
	Float_t gpt[kMaxHits];
	Float_t geta[kMaxHits];
	Float_t gphi[kMaxHits];
	Float_t ppt[kMaxHits];
	Float_t peta[kMaxHits];
	Float_t pphi[kMaxHits];
	Short_t vertexIndex[kMaxHits];
	
	Float_t ghelixpx[kMaxHits];
	Float_t ghelixpy[kMaxHits];
	Float_t ghelixpz[kMaxHits];
	Float_t ghelixox[kMaxHits];
	Float_t ghelixoy[kMaxHits];
	Float_t ghelixoz[kMaxHits];

	Float_t gdedx[kMaxHits];
	Float_t gdca[kMaxHits];
	Float_t gnSigmaPi[kMaxHits];
	Float_t gnSigmaK[kMaxHits];
	Float_t gnSigmaP[kMaxHits];
	Float_t gnSigmaE[kMaxHits];
	Char_t  gq[kMaxHits];
	Int_t  gtrackId[kMaxHits];
	Int_t  gIndex2Primary[kMaxHits];
	Char_t  gnFtPts[kMaxHits];
	Char_t  gnDedxPts[kMaxHits];

	Int_t   gchannel[kMaxHits];
	Float_t gyLocal[kMaxHits];
	Float_t gzLocal[kMaxHits];
	Float_t gtdc[kMaxHits];
	Float_t gtot[kMaxHits];
	Float_t gtof[kMaxHits];
	Float_t gpathLength[kMaxHits];
	Float_t gbeta[kMaxHits];
	Float_t gtdiff[kMaxHits];

	/// projection information to TOF and MTD
	UChar_t  gprojMtdBackLeg[kMaxHits];
	UChar_t  gprojMtdModule[kMaxHits];
	UChar_t  gprojMtdCell[kMaxHits];
	Float_t  gprojMtdPhi[kMaxHits];
	Float_t  gprojMtdZ[kMaxHits];
	Float_t  gprojMtdLength[kMaxHits];
	Float_t  gprojTofPhi[kMaxHits];
	Float_t  gprojTofZ[kMaxHits];
	Float_t  gprojTofLength[kMaxHits];
	Float_t  gtof2Tof[kMaxHits]; // by projection
	Float_t  gtof2Mtd[kMaxHits];

	Int_t     gnMatchMtdHits[kMaxHits];
	Int_t     gmMtdHitIndex[kMaxHits];
	UChar_t   gmBackLeg[kMaxHits]; // 1-30
	UChar_t   gmModule[kMaxHits];  // 1-5
	UChar_t   gmCell[kMaxHits];    // 0-11
	Float_t   gmLeTimeWest[kMaxHits];
	Float_t   gmTotWest[kMaxHits];
	Float_t   gmLeTimeEast[kMaxHits];
	Float_t   gmTotEast[kMaxHits];
	Float_t   gmLocalZ[kMaxHits];
	Float_t   gmLocalY[kMaxHits];
};

/// MTD track class
class MtdTrack{
	public:
		MtdTrack():pt(-999.),eta(-999.),nFtPts(0),nDedxPts(0),flag(0),nHitsPoss(999){};
		MtdTrack(StTrack *stt);
		MtdTrack(StMuTrack *mut);
		~MtdTrack(){}

		Double_t pt;
		Double_t eta;
		Int_t	 nFtPts;   
		Int_t	 nDedxPts;   
		Int_t	 flag;   
		Int_t	 nHitsPoss;   

};

class StMtdMatchMaker: public StMaker 
{

	public:
                /// Default constructor
		StMtdMatchMaker(const char* name = "MtdMatch");
		virtual ~StMtdMatchMaker();
   
		virtual void  Clear(const char* opt="");
		/// initialize drifting velocity and histograms.
		virtual Int_t Init();
		Int_t  InitRun(int runnumber);

		/// associate tracks with mtd hits
		virtual Int_t Make();
		/// write QA histograms
		virtual Int_t Finish();
		Int_t  FinishRun(int runnumber);

		/// set event trigger comsmic ray event has a reversed direction from outer to inner for tracks in upper half TPC 
		void setCosmicEvent(const Bool_t val);
		/// save QA tree or not
		void setSaveTree(const Bool_t val);
		/// save QA histogram or not
		void setHisto(const Bool_t val);
		
		/// set minimum nHitsFit 
		void setMinFitPoints(Int_t val);
		/// set minimum ndEdx fit points 
		void setMindEdxFitPoints(Int_t val);
		/// set pseudo rapidity range 
		void setEtaRange(Float_t etaMin, Float_t etaMax);
		/// set minimum nHitsFit 
		void setMinPt(Float_t val);
		/// set maximum matching cells
		void setMaxNeighbors(Int_t val);
		
		/// check track quality 
		bool validTrack(StTrack *track);
		bool validTrack(StMuTrack *track);


	protected:
		string		 mOutName;
		StPhysicalHelixD* mBeamHelix;
		Float_t mBeamX, mBeamY;
		Bool_t  doPrintMemoryInfo;     
		Bool_t  doPrintCpuInfo;
		Bool_t  mCosmicFlag;

		Int_t   mMinFitPointsPerTrack;  //! minimum fit points per track
		Int_t   mMindEdxFitPoints;  //! minimum dE/dx fit points per track
		Float_t mMinFitPointsOverMax; //! minimum ratio
		Float_t mNSigReso; //! n sigma of z and y resolution of MTD
		Float_t mMinEta; //! minimum pseudorapidity 
		Float_t mMaxEta; //! maximum pseudorapidity 
		Float_t mMinPt;  //! minimum transverse momentum 


	private:
		Bool_t          mMuDstIn;          //! switch - default is to read in StEvent
		Bool_t			mSaveTree;
		Bool_t			mHisto;
		
		Double_t		mVDrift[mNAllTrays][mNStrips];  //! drifting velocity
		Int_t 			mnNeighbors; //! match with +- mnNeighbors cell
		Int_t 			ngTracks;


		///QA histograms
		TH1D* mEventCounterHisto;
		TH1D* mCellsMultInEvent;
		TH1D* mHitsMultInEvent;
		TH1D* mHitsPrimaryInEvent;   // ! primary tracks
		TH1D* mHitsGlobalInEvent;    // ! global tracks
		TH1D* mHitsMultPerTrack;
		TH2D* mHitsPosition;
		TH1D* mDaqOccupancy[mNBacklegs];
		TH1D* mDaqOccupancyProj[mNBacklegs];

		TH2D* mHitCorr[mNBacklegs];
		TH2D* mHitCorrModule[mNBacklegs];
		TH2D* mDeltaHitFinal[mNBacklegs];

		TH2D* mTrackPtEta;
		TH2D* mTrackPtPhi;
		TH1D* mTrackNFitPts;
		TH2D* mTrackdEdxvsp;
		TH2D* mNSigmaPivsPt;

		TH1D* mCellsPerEventMatch1;
		TH1D* mHitsPerEventMatch1;
		TH1D* mCellsPerTrackMatch1;
		TH1D* mTracksPerCellMatch1;
		TH1D* mDaqOccupancyMatch1;
		TH2D* mDeltaHitMatch1;

		TH1D* mCellsPerEventMatch2;
		TH1D* mHitsPerEventMatch2;
		TH1D* mCellsPerTrackMatch2;
		TH1D* mTracksPerCellMatch2;
		TH1D* mDaqOccupancyMatch2;
		TH2D* mDeltaHitMatch2;

		TH1D* mCellsPerEventMatch3;
		TH1D* mHitsPerEventMatch3;
		TH1D* mCellsPerTrackMatch3;
		TH1D* mTracksPerCellMatch3;
		TH1D* mDaqOccupancyMatch3;
		TH2D* mDeltaHitMatch3;

		TH1D* mCellsPrimaryPerEventMatch3;

		TH2F *hphivsz;
		TH2F *hTofPhivsProj;
		TH2F *hTofZvsProj;
		TH2F *hMtdZvsProj;
		TH2F *hMtdPhivsProj;
		TH2F *hMtddPhivsBackleg;
		TH2F *hMtddZvsBackleg;
		
		TF1 *fZReso;
		TF1 *fPhiReso;

		StEvent *mEvent;
		StMuDst *mMuDst;
  		StTriggerData *trgData;
#ifndef ST_NO_TEMPLATE_DEF_ARGS
		typedef vector<Int_t> idVector;
#else
		typedef vector<Int_t,allocator<Int_t>> idVector;
#endif
		typedef idVector::iterator idVectorIter;    
		struct StructCellHit{
			Int_t backleg;
			Int_t module;
			Int_t cell;
			StThreeVector<double> hitPosition;
			idVector trackIdVec;
			Int_t matchFlag;
			Float_t zhit;
			Float_t yhit;
			pair<Double_t,Double_t> tot;
			pair<Double_t,Double_t> leadingEdgeTime;
			Int_t index2MtdHit;
			Double_t theta;
			Double_t pathLength;
		};
		MtdEvtData  mMtdEvtData;

		TTree *mMtdEvt;

#ifndef ST_NO_TEMPLATE_DEF_ARGSA
		typedef vector<StructCellHit> mtdCellHitVector;
#else
		typedef vector<StructCellHit,allocator<StructCellHit>> mtdCellHitVector;
#endif
		typedef vector<StructCellHit>::iterator mtdCellHitVectorIter;
		typedef pair<Double_t,Double_t> pairD;


		/// set QA histograms
		void bookHistograms();
		/// set QA tree 
		void bookTree();
		/// read mtd hits from StMuDst and StEvent
		Bool_t readMtdHits(mtdCellHitVector& daqCellsHitVec,idVector& validModuleVec);
		/// project a track to MTD geometry, return the hit position and strip index
		void project2Mtd(mtdCellHitVector daqCellsHitVec,mtdCellHitVector& allCellsHitVec,Int_t& nPrimaryHits);
		/// compare track hit strip index with mtd hits, save the MTD fired hits in same strip as candidates
		void matchMtdHits(mtdCellHitVector& dapCellsHitVec,mtdCellHitVector& allCellsHitVec,mtdCellHitVector& matchHitCellsVec);
		/// save vectors of single and multi matched hits
		void sortSingleAndMultiHits(mtdCellHitVector& matchHitCellsVec,mtdCellHitVector& singleHitCellsVec,mtdCellHitVector& multiHitsCellsVec);
		/// select the closest hit from multi match hits
		void finalMatchedMtdHits(mtdCellHitVector& singleHitCellsVec,mtdCellHitVector& FinalMatchedCellsVec);
		/// fill the matched hit into PID traits
		void fillPidTraits(mtdCellHitVector& FinalMatchedCellsVec,Int_t& nValidSingleHitCells,Int_t& nValidSinglePrimHitCells);
		/// check track quality
		bool validTrack(MtdTrack mtt);

		/// used in project2Mtd() 
		bool matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,const StPhysicalHelixD &helix, Int_t gq, mtdCellHitVector& allCellsHitVec,unsigned int iNode, StThreeVectorD globalPos);


		/// convert module center to local coordinates
		void modulePos(int backleg,int module,StThreeVector<double>& local);		
		/// convert global position to local position 
		void global2Local(StThreeVector<double> global,int backleg,int module,StThreeVector<double>& local);
		/// reset QA tree data
		void initEventData();

		/// fill track branch to QA tree
		void fillTrackInfo(StTrack *t, float mField, UInt_t iNode);
		void fillTrackInfo(StMuTrack *t, float mField, UInt_t iNode);

		virtual const char *GetCVS() const
	 		{static const char cvs[]="Tag $Name:  $ $Id: StMtdMatchMaker.h,v 1.5 2014/04/16 02:23:39 huangbc Exp $ built "__DATE__" "__TIME__ ; return cvs;}
		ClassDef(StMtdMatchMaker,1)
};

inline void StMtdMatchMaker::setCosmicEvent(const Bool_t val) { mCosmicFlag= val; }
inline void StMtdMatchMaker::setSaveTree(const Bool_t val) { mSaveTree= val; }
inline void StMtdMatchMaker::setHisto(const Bool_t val) { mHisto = val; }
inline void StMtdMatchMaker::setMinFitPoints(Int_t val) { mMinFitPointsPerTrack = val; }
inline void StMtdMatchMaker::setMindEdxFitPoints(Int_t val) { mMindEdxFitPoints= val; }
inline void StMtdMatchMaker::setEtaRange(Float_t etaMin,Float_t etaMax) {mMinEta=etaMin;mMaxEta=etaMax; }
inline void StMtdMatchMaker::setMinPt(Float_t val) {mMinPt=val; }
inline void StMtdMatchMaker::setMaxNeighbors(Int_t val) {mnNeighbors=val; }
#endif
