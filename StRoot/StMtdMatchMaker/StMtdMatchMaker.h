
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
#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
typedef vector<Int_t>  IntVec;
typedef vector<UInt_t>  UIntVec;
typedef vector<Double_t>  DoubleVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
typedef vector<UInt_t, allocator<UInt_t>>  UIntVec;
typedef vector<Double_t, allocator<Double_t>>  DoubleVec;
#endif

const Int_t kMaxHits = 100000;
const Int_t mNBacklegs = 30;
const Int_t mNStrips = 30;
const Int_t mNAllTrays = 150; 

struct MtdEvtData {
	Int_t  run, evt;
	Int_t  trgId;
	Float_t  bField;
	Float_t vertexX, vertexY, vertexZ;

	Double_t  triggerTime;

	//raw hits
	Int_t  nMtdRawHits;
	Char_t    flag[kMaxHits];
	UChar_t   backlegRaw[kMaxHits];
	UChar_t   chn[kMaxHits]; //1-120
	Double_t  tdc[kMaxHits]; // bin

	//sorted hits
	Int_t  nMtdHits;
	UChar_t  backleg[kMaxHits];
	UChar_t  module[kMaxHits];
	UChar_t  cell[kMaxHits];

	Double_t leTimeWest[kMaxHits];
	Double_t leTimeEast[kMaxHits];
	Double_t totWest[kMaxHits];
	Double_t totEast[kMaxHits];

	//global tracks
	Int_t ngTracks;
	Float_t gpt[kMaxHits];
	Float_t geta[kMaxHits];
	Float_t gphi[kMaxHits];
	
	Float_t ghelixpx[kMaxHits];
	Float_t ghelixpy[kMaxHits];
	Float_t ghelixpz[kMaxHits];
	Float_t ghelixox[kMaxHits];
	Float_t ghelixoy[kMaxHits];
	Float_t ghelixoz[kMaxHits];

	Float_t gdedx[kMaxHits];
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

	//projection to TOF and MTD
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

class StMtdMatchMaker: public StMaker{

	public:
		StMtdMatchMaker(StMuDstMaker *,  const Char_t *);
		virtual ~StMtdMatchMaker();
		virtual void  Clear(const char* opt="");
		virtual Int_t Init();
		Int_t  InitRun(int runnumber);
		virtual Int_t Make();
		virtual Int_t Finish();
		Int_t  FinishRun(int runnumber);
		void setMuDstIn(const Bool_t val);
		void setCosmicEvent(const Bool_t val);
		void setSaveTree(const Bool_t val);
		void setHisto(const Bool_t val);

		StPhysicalHelixD* mBeamHelix;
		Float_t mBeamX, mBeamY;
		Bool_t  doPrintMemoryInfo;     
		Bool_t  doPrintCpuInfo;
		Bool_t  mCosmicFlag;

		Int_t mMinFitPointsPerTrack;
		Float_t mMinFitPointsOverMax;
		//Float_t mZLocalCut;
		Float_t mNSigReso;

		Int_t ngTracks;

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
		
		TF1 *fZReso;
		TF1 *fPhiReso;

	protected:
		StMuDstMaker *mMuDstMaker;
		string		 mOutName;
	private:
		Bool_t          mMuDstIn;          //! switch - default is to read in StEvent
		Bool_t			mSaveTree;
		Bool_t			mHisto;
		Double_t		mVDrift[mNAllTrays][mNStrips];
		Int_t 	mnNeighbors; // match with +- mnNeighbors cell

		///
		void processStEvent();
		///
		void processMuDst();


		StEvent *mEvent;
		StMuDst *mMuDst;
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
		};
		MtdEvtData  mMtdEvtData;

		TTree *mMtdEvt;
		TFile *fout;

#ifndef ST_NO_TEMPLATE_DEF_ARGSA
		typedef vector<StructCellHit> mtdCellHitVector;
#else
		typedef vector<StructCellHit,allocator<StructCellHit>> mtdCellHitVector;
#endif
		typedef vector<StructCellHit>::iterator mtdCellHitVectorIter;
		typedef pair<Double_t,Double_t> pairD;


		void bookHistograms();
		Bool_t readMtdHits(mtdCellHitVector& daqCellsHitVec,idVector& validModuleVec);
		void project2Mtd(mtdCellHitVector daqCellsHitVec,mtdCellHitVector& allCellsHitVec,Int_t& nPrimaryHits);
		void matchMtdHits(mtdCellHitVector& dapCellsHitVec,mtdCellHitVector& allCellsHitVec,mtdCellHitVector& matchHitCellsVec);
		void sortSingleAndMultiHits(mtdCellHitVector& matchHitCellsVec,mtdCellHitVector& singleHitCellsVec,mtdCellHitVector& multiHitsCellsVec);
		void finalMatchedMtdHits(mtdCellHitVector& singleHitCellsVec,mtdCellHitVector& FinalMatchedCellsVec);
		void fillPidTraits(mtdCellHitVector& FinalMatchedCellsVec,Int_t& nValidSingleHitCells,Int_t& nValidSinglePrimHitCells);
		bool validTrack(StTrack *track);
		bool validTrack(StMuTrack *track);
		bool matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,StTrack *theTrack, mtdCellHitVector& allCellsHitVec,unsigned int iNode, StThreeVectorD globalPos);
		bool matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,StMuTrack *theTrack, mtdCellHitVector& allCellsHitVec,unsigned int iNode, StThreeVectorD globalPos);
		bool matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,StPhysicalHelixD helix, Int_t gq, mtdCellHitVector& allCellsHitVec,unsigned int iNode, StThreeVectorD globalPos);
		void modulePos(int backleg,int module,StThreeVector<double>& local);		
		void global2Local(StThreeVector<double> global,int backleg,int module,StThreeVector<double>& local);
		int  decodeStripPhiZ(int backLeg, int trayId, int channel, double &phi, double &z);
		int  decodeTofPhiZ(int channel, double &phi, double &z);
		void initEventData();
		ClassDef(StMtdMatchMaker,1)
};

inline void StMtdMatchMaker::setMuDstIn(const Bool_t val) { mMuDstIn = val; }
inline void StMtdMatchMaker::setCosmicEvent(const Bool_t val) { mCosmicFlag= val; }
inline void StMtdMatchMaker::setSaveTree(const Bool_t val) { mSaveTree= val; }
inline void StMtdMatchMaker::setHisto(const Bool_t val) { mHisto = val; }
#endif
