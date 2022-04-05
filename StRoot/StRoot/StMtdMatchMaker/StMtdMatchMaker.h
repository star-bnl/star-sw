//! Muon Telescope Detector (MTD) Match Maker
/*!  \class StMtdMatchMaker
 *   \brief Match Maker for the MTD detector
 *   \author Bingchu Huang
 *   \date January 2013
 *
 * The MTD MatchMaker matches STAR tracks to the MTD MRPCs.
 * 
 * $Id: StMtdMatchMaker.h,v 1.20 2018/12/06 18:11:13 marr Exp $
 */
/*****************************************************************
 *
 * $Log: StMtdMatchMaker.h,v $
 * Revision 1.20  2018/12/06 18:11:13  marr
 * Improvement: extrapolate tracks to the proper primary vertex when available. This eliminates large negative dTof values
 *
 * Revision 1.19  2018/09/04 19:29:14  marr
 * Use the pairD definition in StHelixD.hh
 *
 * Revision 1.18  2017/03/08 20:48:54  marr
 * 1) Add a new data member mYear to indicate run year
 * 2) Invoke appropriate functions in StMtdGeometry class to calculate local y
 * to make the class backward compatible
 *
 * Revision 1.17  2016/08/05 16:12:24  marr
 * Add MTD hit IdTruth to avoid applying dy shift for BL 8 and 24 for MC hits
 *
 * Revision 1.16  2016/07/28 14:31:23  marr
 * Fix coverity check: initialization of data member
 *
 * Revision 1.15  2016/07/27 15:46:34  marr
 * Fix coverity check: initialization of data members
 *
 * Revision 1.14  2015/10/16 19:04:55  marr
 * Remove filling trees
 *
 * Revision 1.13  2015/07/10 16:07:40  marr
 * Add the distance along radius to the calculation of the distance between
 * a MTD hit and a projected track position
 *
 * Revision 1.12  2015/04/24 19:55:16  marr
 * Add a member function cleanUpMtdPidTraits() to clean up the MTD pidTraits for
 * all global and primary tracks before the matching process. This is needed when
 * running MuDst in afterburner mode.
 *
 * Revision 1.11  2015/04/10 18:21:38  marr
 * Comment on the meaning of different values of matchFlag
 *
 * Revision 1.10  2014/09/09 14:00:39  marr
 * Fill the expected time-of-flight calculated via track extrapolation
 *
 * Revision 1.9  2014/08/06 11:43:27  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.8  2014/07/24 02:53:04  marr
 * 1) Add log info of the matched track-hit pair
 * 2) Set DeltaY and DeltaZ in PidTraits
 *
 * Revision 1.7  2014/07/10 20:50:35  huangbc
 * Use new MTD geometry class. Load geometry volume from geant.
 * Choose closest one for multi-tracks which associated with same hit.
 *
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
class StMtdGeometry;

#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
typedef vector<Int_t>  IntVec;
typedef vector<UInt_t>  UIntVec;
typedef vector<Double_t>  DoubleVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
typedef vector<UInt_t, allocator<UInt_t>>  UIntVec;
typedef vector<Double_t, allocator<Double_t>>  DoubleVec;
#endif

const Int_t mNBacklegs = 30;
const Int_t mNStrips = 12;
const Int_t mNAllTrays = 150; 

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
		/// set energy loss in MTD 
		void setELossFlag(const Bool_t val);
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
		/// set matching neighbor modules
		void setMatNeighbors(Bool_t val);
		/// set n extra cells
		void setNExtraCells(Int_t val);
		/// set BField to FF, only use this option in simulation 
		void setLockBField(Bool_t val); 
		/// set geometry tag, only use this option in reading mudst mode 
		void setGeomTag(const char *tag);
		
		/// check track quality 
		bool validTrack(StTrack *track);
		bool validTrack(StMuTrack *track);

                // calcuate global z of MTD hit
                Float_t getMtdHitGlobalZ(Float_t leadingWestTime, Float_t leadingEastTime, Int_t module);
		Int_t   getProjModule(Float_t local_z, Float_t global_z);


	protected:
		StPhysicalHelixD* mBeamHelix;
		Bool_t  doPrintMemoryInfo;     
		Bool_t  doPrintCpuInfo;
		Bool_t  mCosmicFlag;

		Int_t   mMinFitPointsPerTrack;  //! minimum fit points per track
		Int_t   mMindEdxFitPoints;  //! minimum dE/dx fit points per track
		Float_t mMinFitPointsOverMax; //! minimum ratio
		Float_t mMinEta; //! minimum pseudorapidity 
		Float_t mMaxEta; //! maximum pseudorapidity 
		Float_t mMinPt;  //! minimum transverse momentum 


	private:
		Bool_t          mMuDstIn;          //! switch - default is to read in StEvent
		Bool_t			mHisto;
		
		Double_t		mVDrift[mNAllTrays][mNStrips];  //! drifting velocity
		Bool_t 			mnNeighbors; //! match with neighbor module
		Int_t 			mNExtraCells; //! match with N extra cells 
		Int_t 			ngTracks;

		map<Int_t, Int_t> index2Primary;

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
		Int_t   mYear;
		StMtdGeometry *mMtdGeom;
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
		        Int_t matchFlag;  // 1: one hit - one track 
		                          // 2: multiple hits - one track; only 1 hit left after tot cut 
		                          // 3: mulitple hits - one track; pick the cloest hit after tot cut 
		                          // 7: one hit - multiple tracks; pick the closest track
		                          // 8: multiple hits - multiple tracks; only 1 hit left after tot cut, pick the closest track
		                          // 9: mulitple hits - multiple tracks; pick the closest track and cloest hit after tot cut 
			Float_t zhit;
			Float_t yhit;
			pair<Double_t,Double_t> tot;
			pair<Double_t,Double_t> leadingEdgeTime;
			Int_t index2MtdHit;
			Double_t theta;
			Double_t pathLength;
			Double_t expTof2MTD;
		        Int_t idTruth;
		};
		Bool_t  mELossFlag;
		Bool_t  mLockBField;
		TString mGeomTag;

#ifndef ST_NO_TEMPLATE_DEF_ARGSA
		typedef vector<StructCellHit> mtdCellHitVector;
#else
		typedef vector<StructCellHit,allocator<StructCellHit>> mtdCellHitVector;
#endif
		typedef vector<StructCellHit>::iterator mtdCellHitVectorIter;


		/// set QA histograms
		void bookHistograms();
		/// clean up mtdPidTraits in MuDst when running afterburner mode
		void cleanUpMtdPidTraits();
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
		bool matchTrack2Mtd(mtdCellHitVector daqCellsHitVec,const StPhysicalHelixD &helix, Int_t gq, mtdCellHitVector& allCellsHitVec,unsigned int iNode, StThreeVectorD pVtx);

		virtual const char *GetCVS() const
	 		{static const char cvs[]="Tag $Name:  $ $Id: StMtdMatchMaker.h,v 1.20 2018/12/06 18:11:13 marr Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
		ClassDef(StMtdMatchMaker,2)
};

inline void StMtdMatchMaker::setCosmicEvent(const Bool_t val) { mCosmicFlag= val; }
inline void StMtdMatchMaker::setELossFlag(const Bool_t val) { mELossFlag= val; }
inline void StMtdMatchMaker::setHisto(const Bool_t val) { mHisto = val; }
inline void StMtdMatchMaker::setMinFitPoints(Int_t val) { mMinFitPointsPerTrack = val; }
inline void StMtdMatchMaker::setMindEdxFitPoints(Int_t val) { mMindEdxFitPoints= val; }
inline void StMtdMatchMaker::setEtaRange(Float_t etaMin,Float_t etaMax) {mMinEta=etaMin;mMaxEta=etaMax; }
inline void StMtdMatchMaker::setMinPt(Float_t val) {mMinPt=val; }
inline void StMtdMatchMaker::setMatNeighbors(Bool_t val) {mnNeighbors=val; }
inline void StMtdMatchMaker::setNExtraCells(Int_t val) {mNExtraCells=val; }
inline void StMtdMatchMaker::setLockBField(Bool_t val) {mLockBField=val; }
inline void StMtdMatchMaker::setGeomTag(const char *tag) {mGeomTag=tag; }
#endif
