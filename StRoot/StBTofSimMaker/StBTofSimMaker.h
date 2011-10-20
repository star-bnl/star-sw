/***************************************************************************
 *
 * $Id: StBTofSimMaker.h,v 1.1 2009/12/09 21:56:41 dthein Exp $
 *
 * Author:  Frank Geurts
 ***************************************************************************
 *
 * Description: StBTofSimMaker virtual base class for Barrel TOF Simulations
 *
 ***************************************************************************
 *
 * $Log: StBTofSimMaker.h,v $
 * Revision 1.1  2009/12/09 21:56:41  dthein
 * First version of StBTofSimMaker
 * 
 *
 **************************************************************************/
#ifndef STBTOFSIMMAKER_HH
#define STBTOFSIMMAKER_HH
#include "StMaker.h"

#include "St_DataSet.h"
class TH1F;
class TH2F;
class TNtuple;
class TNtuple;
class TProfile;

class StEvent;
class StBTofCollection;
class StTofSimParam;
class StBTofGeometry;
class StBTofDaqMap;
struct g2t_ctf_hit_st;

// g2t tables
#include "tables/St_g2t_ctf_hit_Table.h"
#include "tables/St_g2t_vpd_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"

#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcBTofHitCollection.hh"
#include "StMcEvent/StMcBTofHit.hh"
#include "StThreeVectorF.hh"
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StBTofSimMaker : public StMaker{
	protected:


		StBTofGeometry*      mGeomDb;         //! 
		StTofSimParam*      mSimDb;          //!
		StBTofDaqMap*       mDaqMap;         //! Tof Daq map
		StMcBTofHitCollection *mMcBTofHitCollection; //! barrel tof hit

		StEvent           *mEvent;            //!
		StMcEvent         *mMcEvent;
		StBTofCollection   *mBTofCollection;   

		//define some constants
		enum {
			mNTOF = 192,    //! TOF channels per tray
			mNVPD = 19,     //! VPD tubes per side
			mNTray = 120,   //! 120 TOF trays
			mNModule = 32,  //! 32 modules per tray
			mNCell = 6,     //! 6 cells per module
			mAMP = 50000,     
			mADCBINWIDTH = 25,
			mTDCBINWIDTH = 50
		};
		const static float mVHRBIN2PS = 24.4;  //! Very High resolution mode, ps/bin
		const static float mHRBIN2PS = 97.7;     //! High resolution mode, ps/bin
		const static float mBTofPadWidth = 3.45;        //! Pad Width                                                              


		Bool_t mCellXtalk;     //! switch for cell xtalk
		Bool_t mSlow;
		Bool_t mBookHisto;

		Int_t     mTofHitFlag[mNTray][mNTOF];   //! hit flag for tof geant hits
		Int_t   mVpdHitFlag[2*mNVPD];         //! hit flag for vpd geant hits

		struct TrackHit{
			Int_t          tray;
			Int_t          module;
			Int_t          cell;
			Int_t          trkId;
			Double_t       dE;
			Double_t       dQ;
			Double_t       dQdt[600];//this 600 (nTimebins) comes from the /TofUtil/StTofParam file
			Double_t       tof;
			Double_t       s_track;
			Double_t          t0;              //! t0 (in ps) as the start of this tof hit -- was ns - changed for consistency
			StThreeVectorF position;
		};


		typedef vector<TrackHit, allocator<TrackHit> > TrackVec;
		typedef vector<Int_t> IntVec;


		string mHistFile;//for QA histograms
		TH1F* mBetaHist;    //! speed of particles hitting tof
		TH1F* mPathLHist;    //! speed of particles hitting tof
		TH1F* mTofHist;    //! total time of flight of partilce
		TH1F* mRecMass;    //! reconstructed mass of particle

		TH2F* mCellGeant;    //! cellId of geant hit
		TH1F* mVpdGeant;     //! Vpd tubeId of geant hit
		TH2F* mNCellGeant;   //! # of cells of geant hit
		TH2F* mNVpdGeant;    //! # of vpd tubes of geant hit
		TH1F* mDeGeant;      //! deposited-energy in geant hit
		TH1F* mTofGeant;     //! tof in geant hit

		TH2F* mCellSeen;     //! cellId after DetectorResponse
		TH1F* mVpdSeen;      //! Vpd tubeId after DetectorResponse
		TH2F* mNCellSeen;    //! # of cells after DetectorResponse
		TH2F* mNVpdSeen;     //! # of vpd tubes after DetectorResponse
		TH1F* mDeSeen;       //! deposited-energy after DetectorResponse
		TH1F* mT0Seen;      //! 
		TH1F* mTofSeen;      //! smeared-tof after DetectorResponse
		TH1F* mTofResSeen;   //! time resolution after Detector Response
		TH1F* mVpdResSeen;   //! vpd time resolution after DetectorResponse

		TH2F* mCellReco;     //! cellId after recon
		TH1F* mVpdReco;      //! Vpd tubeId after recon
		TH2F* mNCellReco;    //! # of cells after recon
		TH2F* mNVpdReco;     //! # of vpd tubes after recon
		TH1F* mTDCReco;      //! TDC recon
		TH1F* mADCReco;      //! ADC recon -- empty
		TH1F* mT0Reco;   //! 
		TH1F* mTofResReco;   //! time resolution after recon
		TH1F* mVpdResReco;   //! vpd time resolution after recon
		TH2F* mTACorr;       //! T-A Slewing Correlation
		TH1F* mModHist;       //! T-A Slewing Correlation

		/// TOFp histograms
		TH1F* mdE;           //!
		TH1F* mdS;           //!
		TH1F* mNumberOfPhotoelectrons;  //!
		TH1F* mT;            //!
		TH1F* mTime;         //!
		TH1F* mTime1;        //!
		TH1F* mPMlength;     //!
		TH1F* mAdc;          //!
		TH1F* mTdc;          //!

		TVolume *starHall;

		Int_t CellResponse(g2t_ctf_hit_st* tof_hit,
				TrackVec& trackVec);   //! Slow simulation step one
		Int_t CellTimePassTh(TrackVec& trackVec);        //! Slow simulation step two

		Int_t FastCellResponse(g2t_ctf_hit_st* tof_hit);
		Int_t VpdResponse(g2t_vpd_hit_st* vpd_hit);

		IntVec    CalcCellId(Int_t volume_id, Float_t ylocal);
		Int_t CellXtalk(Int_t icell, Float_t ylocal, Float_t& wt, Int_t& icellx);
		Int_t      storeMcBTofHit(StMcBTofHit *mcCellHit);

		Int_t        fillRaw(void);
		Int_t        electronicNoise(void);
		Float_t       slatResponseExp(Float_t&);
		Double_t GammaRandom();


		Int_t        fillEvent();
		Int_t        bookHistograms();
		Int_t        writeHistograms();
		Int_t        ResetFlags();


	public:
		StBTofSimMaker(const char *name="TofSim");


		virtual ~StBTofSimMaker();

		void           Reset();
		virtual Int_t  Init();
		Int_t          InitRun(Int_t);
		Int_t          FinishRun(Int_t);
		virtual Int_t  Make();
		virtual Int_t  Finish();

		StBTofGeometry*    GetGeometry()       const { return mGeomDb; }
		StTofSimParam*    GetSimParam()       const { return mSimDb; }
		StBTofCollection*  GetBTofCollection()  const { return mBTofCollection; }
		StMcBTofHitCollection* GetMcBTofHitCollection() const { return mMcBTofHitCollection; }

		void   setCellXtalk(Bool_t val) { mCellXtalk = val; }
		void   setHistFileName(string s);

		virtual const char *GetCVS() const
		{static const char cvs[]="Tag $Name:  $ $Id: StBTofSimMaker.h,v 1.1 2009/12/09 21:56:41 dthein Exp $ built "__DATE__" "__TIME__ ; return cvs;}

		ClassDef(StBTofSimMaker,0)
};
#endif
