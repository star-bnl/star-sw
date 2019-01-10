/***************************************************************************
 *
 * $Id: StBTofSimMaker.cxx,v 1.16 2019/01/10 22:26:33 genevb Exp $
 *
 * Author: Frank Geurts
 ***************************************************************************
 *
 * Description: StBTofSimMaker class for Barrel TOF Simulations
 *
 ***************************************************************************
 *
 * VPD Removed by Nickolas Luttrell (Rice University)
 *
 **************************************************************************/
//! Time-of-Flight Simulator Maker
/*! \class StTofSimMaker
  \author Frank Geurts

  <p>TOF simulation software. This Maker further processes the simulated
  detector response from GSTAR's GEANT simulation. It takes the G2T tof
  hit tables and builds an StEvent Tof SlatCollection.</p>
 */
#include <Stiostream.h>
#include "StBTofSimMaker.h"

// SCL
#include <math.h>
#include <string>
#include "TRandom.h"
#include "SystemOfUnits.h"
#include "phys_constants.h"
#include "StThreeVectorD.hh"
#include "Random.h"
#include "RanluxEngine.h"
#include "RandGauss.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"

// g2t tables and collections
#include "tables/St_g2t_ctf_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "StMcTrack.hh"
#include "StVpdSimMaker/StVpdSimConfig.h"

#include "StBTofUtil/StBTofDaqMap.h"
#include "StTofUtil/tofPathLength.hh"
#include "StTofUtil/StTofSimParam.h"
#include "StBTofUtil/StBTofGeometry.h"
#include "StEventTypes.h"
#include "StEvent/StBTofCollection.h"
#include "StChain/StChainOpt.h"
#include "StVpdSimMaker/StVpdSimMaker.h"
#include "StVpdCalibMaker/StVpdCalibMaker.h"
#include "StBTofSimResParams.h"

static RanluxEngine engine;
static RandGauss ranGauss(engine);


const float StBTofSimMaker::mVHRBIN2PS = 24.4;    //! Very High resolution mode, ps/bin
const float StBTofSimMaker::mHRBIN2PS = 97.7;     //! High resolution mode, ps/bin
const float StBTofSimMaker::mBTofPadWidth = 3.45; //! Pad Width

	//_____________________________________________________________________________
StBTofSimMaker::StBTofSimMaker(const char *name):StMaker(name)
{
	//! set default values
	mBookHisto=kFALSE; //!< histograms
	mSlow=kFALSE;
	mCellXtalk=kTRUE;
	mWriteStEvent=kTRUE;
	mDaqMap=0;
	mMcBTofHitCollection = 0;
	mIsEmbedding = kSimulation;
	mVpdSim = kFALSE;
	mUseVpdStart = kTRUE;
	Reset();

}

//_____________________________________________________________________________
StBTofSimMaker::~StBTofSimMaker()
{
	delete mSimDb;
	delete mDaqMap;

}


//_____________________________________________________________________________
int StBTofSimMaker::Init()
{
	Reset();
	//mSimDb = new StTofSimParam(); //  Moved to InitRun
	//if (Debug()) mSimDb->print(); //  Moved to InitRun
	if(mBookHisto) bookHistograms();

	return StMaker::Init();
}

//_____________________________________________________________________________
void StBTofSimMaker::Reset()
{
	mBTofHeader = 0;
	mGeantData = 0;
	mEvent  = 0;
	mMcEvent = 0;
	delete mMcBTofHitCollection;
	mSimDb  = 0;

	if(mDaqMap){delete mDaqMap; mDaqMap = 0;}

	ResetFlags();
}

//_____________________________________________________________________________
int StBTofSimMaker::ResetFlags()
{
	//! reset TOF hit occupancy flag
	memset(mTofHitFlag, 0, sizeof(mTofHitFlag));
	return kStOk;
}

//_____________________________________________________________________________
int StBTofSimMaker::InitRun(int runnumber)
{

	/// MRPC-TOF DAQ map
	mDaqMap = new StBTofDaqMap();
	mDaqMap->Init(this);
	mSimDb = new StTofSimParam();
	mSimResDb = new StBTofSimResParams();
	mSimResDb->loadParams();
	//mSimDb->init();   // Only enable to pull calibration values from db
	if (Debug()) mSimDb->print();
	StVpdSimMaker *vpdSim = (StVpdSimMaker *)GetMaker("VpdSim");
	
	//! Determine if VpdSimMaker was run
	if (vpdSim) {
		mVpdSim = kTRUE;
	}
	mVpdSimConfig = new StVpdSimConfig;
	
	//! Determine if vpd start or startless
	StVpdCalibMaker *vpdCalib = (StVpdCalibMaker *)GetMaker("vpdCalib");
	if(vpdCalib) {
		mUseVpdStart = vpdCalib->useVpdStart();
	}

	return kStOK;
}

//_____________________________________________________________________________
int StBTofSimMaker::FinishRun(int runnumber)
{
	LOG_INFO << "StBTofSimMaker::FinishRun -- cleaning up BTOF DAQ map --" << endm;
	if (mDaqMap){delete mDaqMap; mDaqMap = 0;}
	if (mSimDb){delete mSimDb; mSimDb = 0;}
	if (mSimResDb){delete mSimResDb; mSimResDb = 0;}
	if (mVpdSimConfig){delete mVpdSimConfig; mVpdSimConfig = 0;}
	return kStOk;
}

//_____________________________________________________________________________
int StBTofSimMaker::Finish()
{
	if(mBookHisto){
		LOG_INFO << "StBTofSimMaker::Finish  writing BTofSim.root ..." << endm;
		mHistoFileName = setHistFileName();
		if (mHistoFileName == "") {
			LOG_INFO << "Nothing stored in mHistoFileName!" << endm;
			mHistoFileName = "BTofSim.root";
		}
		else {
			LOG_INFO << "The Filename is: " << mHistoFileName.c_str() << endm;
		}
		TFile aFile(mHistoFileName.c_str(),"RECREATE","tofsim");
		
		aFile.cd();
		// if ( ntuple )
		// 	ntuple->SetDirectory(aFile.CurrentDirectory());

		writeHistograms();
		aFile.Write();
		aFile.Close();
	}
	return kStOK;
}


//_____________________________________________________________________________
int StBTofSimMaker::Make()
{
	LOG_INFO << "StBTofSimMaker  Make() starts" << endm;

	ResetFlags();

	mMcBTofHitCollection = new StMcBTofHitCollection();

	//! Check to see that there are GEANT hits
	mGeantData = GetInputDS("geant"); //! in bfc chain
	if(!mGeantData) { //! when reading the geant.root file
		mGeantData = GetInputDS("geantBranch");
	}
	if(!mGeantData) {
		LOG_WARN << " No GEANT data loaded. Exit! " << endm;
		return kStWarn;
	}
	LOG_INFO << " Found GEANT data -- loading VPD/TOF hits... " << endm;    //! Note that it may still be loading VPD Hits!

	//! Initialize StEvent
	mEvent = (StEvent*)GetInputDS("StEvent");
	if (!mEvent) {
		LOG_ERROR << "No StEvent! Bailing out ..." << endm;
	}
	StBTofCollection *btofColl = mEvent->btofCollection();
	
	
	//! Look for TOF hits
	St_g2t_ctf_hit* g2t_tfr_hits = 0;
	g2t_tfr_hits = dynamic_cast<St_g2t_ctf_hit*> (mGeantData->Find("g2t_tfr_hit"));
	if(!g2t_tfr_hits) {
		LOG_WARN << " No TOF hits in GEANT" << endm; }
	else {
		int nhits = g2t_tfr_hits->GetNRows();
		LOG_INFO << " Found GEANT TOF hits: " << nhits << endm;
		g2t_ctf_hit_st* tofHitsFromGeant = g2t_tfr_hits->begin();

		if(mSlow) {
			//! fill this vector with particles that hit the tof and the tof's responses
			TrackVec tofResponseVec;
			tofResponseVec.clear();
			for (int i=0; i<nhits; i++, tofHitsFromGeant++) {
				//! for every tof hit possible add a response (and neighboring cell response if Xtalk is enabled)
				CellResponse(tofHitsFromGeant,tofResponseVec);
			}
			//! compute ToT for all responses saved in tofResponseVec
			CellTimePassTh(tofResponseVec);
		}
		else if(!mSlow) {
			for (int i=0; i<nhits; i++, tofHitsFromGeant++) FastCellResponse(tofHitsFromGeant, btofColl);
		}
		else
			LOG_WARN << " No TOF simulator specified " << endm;
	}
	LOG_INFO << " McBTofHit Size (TOF) = " << mMcBTofHitCollection->hits().size() << endm;


	fillEvent();

	return kStOK;
}


//_____________________________________________________________________________
/// MRPC-TOF slow simulator
int StBTofSimMaker::CellResponse(g2t_ctf_hit_st* tofHitsFromGeant,
		TrackVec& tofResponseVec)//slow sim part 1
{
	///
	/// Original author of slow simulator: Lijuan Ruan
	/// Simulate the single cell response for a geant hit
	///
	/// 1) Charged particle traverses ToF detector (a specific module)
	/// 2) Number of electron showers is determined
	/// 3) Size of each electron shower is established
	/// 4) Shower energy deposit (and such) is saved in data structures

	// accept TOF hit
	if(tofHitsFromGeant->s_track<=0.0 || tofHitsFromGeant->de <=0.0) {
		LOG_WARN << " No deposited energy in this TOF hit!" << endm;
		return kStWarn;
	}
	
	IntVec cellId   = CalcCellId(tofHitsFromGeant->volume_id, tofHitsFromGeant->x[1]);
	int icell, imodule, itray;
	itray   = cellId[0];
	imodule = cellId[1];
	icell   = cellId[2];
	if (itray==-1 || imodule==-1 || icell==-1) {
		LOG_WARN << " Not hit the sensitive MRPC volume!" << endm;
		return kStWarn;
	}
	if(mBookHisto) {
		mDeGeant->Fill(tofHitsFromGeant->de / keV);
		mTofGeant->Fill(tofHitsFromGeant->tof / nanosecond);
	}


	St_g2t_track *g2t_track = static_cast<St_g2t_track *>(mGeantData->Find("g2t_track"));
	if (!g2t_track) {
		LOG_WARN << " No G2T track table!" << endm;
		return kStWarn;
	};
	g2t_track_st *tof_track = g2t_track->GetTable();
	int no_tracks= g2t_track->GetNRows();

	double beta;
	int trackId = -1;
	for(int j=0;j<no_tracks;j++){
		if(tofHitsFromGeant->track_p==tof_track[j].id){
			trackId = j;//
			beta = tof_track[j].ptot/tof_track[j].e;
			break;
		}
	}

	double qtot=-1;
	double tof=-1;
	double t0 = tofHitsFromGeant->tof;
	float wt=1.0;

	double clusterDensity = mSimDb->nclus(beta);
	double gapLength  = mSimDb->dg();
	double alpha   = mSimDb->alpha();
	double ka  = mSimDb->ka();
	double kaa = ka/(alpha*gapLength);

	const int maxClusters=mSimDb->nmaxclus();
	const int nTimeBins = mSimDb->ndt();
	double driftVelocity[maxClusters],nElectrons[maxClusters],startPositionOfCluster[maxClusters],sa[maxClusters];
	double s[maxClusters][nTimeBins];

	double chargeDepositedPerTimeBin[nTimeBins];
	for(int j=0;j<nTimeBins;j++) {chargeDepositedPerTimeBin[j] = 0.0;}

	int nElectronClusters=-1;
	while(nElectronClusters<1) {nElectronClusters=gRandom->Poisson(gapLength*clusterDensity);}
	if(nElectronClusters>maxClusters) nElectronClusters = maxClusters;

	for(int m=0;m<nElectronClusters;m++) {
		driftVelocity[m] = mSimDb->vd_mean()*(0.90+0.20*gRandom->Rndm(1));  //!mm/ps

		int nElectrons_temp=-1;
		while(nElectrons_temp<1){nElectrons_temp = gRandom->Poisson(mSimDb->nmeane());}
		nElectrons[m] = double(nElectrons_temp);

		startPositionOfCluster[m]=gapLength+1;
		while(startPositionOfCluster[m]>gapLength) { startPositionOfCluster[m] = gRandom->Exp(1.0/clusterDensity);  }  //mm
	}

	double ytmp=0.0;
	for(int m=0;m<nElectronClusters;m++) {
		sa[m] = (exp(alpha*(gapLength-startPositionOfCluster[m]))-1)*nElectrons[m]*GammaRandom();
		if (sa[m]>mSimDb->nmaxe()) sa[m] = mSimDb->nmaxe();
		ytmp += kaa*sa[m];
	}
	qtot = ytmp*1.e+12*e_SI;  //pC

	t0 = tofHitsFromGeant->tof*1000/nanosecond;  //ps
	tof=tofHitsFromGeant->tof*1000/nanosecond;  //ps


	for(int j=0;j<nTimeBins;j++) {
		double ts = t0+ mSimDb->dt()*double(j);  //dt=25ps
		double ytmp1 = 0.;
		for(int m=0;m<nElectronClusters;m++) {
			double tx = (startPositionOfCluster[m])/(C_C_LIGHT*1.e-3*nanosecond/millimeter);
			double t_drift = (gapLength-startPositionOfCluster[m])/driftVelocity[m];
			if( ts>=t0 + tx  && ts<=t0+ tx+t_drift) {
				s[m][j]=(exp(alpha*driftVelocity[m]*(ts-t0-tx))-1)*nElectrons[m]*GammaRandom();
				if(s[m][j]>mSimDb->nmaxe()) { s[m][j] = mSimDb->nmaxe(); }
			} else {
				s[m][j]=0.0;
			}
			ytmp1 += kaa*s[m][j];
		}
		chargeDepositedPerTimeBin[j] = ytmp1*1.e+12*e_SI;  // pico-Coulomb
	}


	int icellx = -1;
	wt = 1.0;
	StThreeVectorF local(tofHitsFromGeant->x[0], tofHitsFromGeant->x[1], tofHitsFromGeant->x[2]);
	if(mCellXtalk){ CellXtalk(icell, local.y(), wt, icellx); }
	TrackHit trackhit;
	trackhit.tray    = itray;
	trackhit.module  = imodule;
	trackhit.cell    = icell;
	trackhit.trkId   = trackId;
	trackhit.dE      = tofHitsFromGeant->de * wt;
	trackhit.dQ      = qtot * wt;
	for(int j=0;j<nTimeBins;j++) {
		trackhit.dQdt[j] = chargeDepositedPerTimeBin[j] * wt;
	}
	trackhit.tof      = tof;//ps
	trackhit.s_track  = tofHitsFromGeant->s_track;// track length
	trackhit.position = local;
	trackhit.t0       = t0/1000.;//perfect simulation (ns)
	tofResponseVec.push_back(trackhit);
	mTofHitFlag[itray-1][(imodule-1)*mNCell+(icell-1)] = 1;

	if(icellx>0 && icellx<=mNCell){
		TrackHit trackhitx=trackhit;
		trackhitx.cell    = icellx;
		trackhitx.dE      = tofHitsFromGeant->de * (1.-wt);
		trackhitx.dQ      = qtot * (1.-wt);
		for(int j=0;j<nTimeBins;j++) {
			trackhitx.dQdt[j] = chargeDepositedPerTimeBin[j] * (1.-wt);
		}
		tofResponseVec.push_back(trackhitx);
		mTofHitFlag[itray-1][(imodule-1)*mNCell+(icellx-1)] = 1;
	}

	return kStOk;
}

//____________________________________________________________________________
int StBTofSimMaker::CellTimePassTh(TrackVec& tofResponseVec)
	//only for SLOW simulation (i.e. slow simulator part2)
	// Corrects response depending on which track is recorded (since electronics can only see first)
	// Stores the output into StBTof container

{
	TrackVec  trackSumVec;
	trackSumVec.clear();

	int eraseId[500000];
	int nhits = tofResponseVec.size();
	int nTimeBins = mSimDb->ndt();
	for(int i=0;i<nhits;i++) eraseId[i] = 0;


	for(int i=0;i<nhits;i++){
		if(eraseId[i]) continue;
		TrackHit sumhit=tofResponseVec[i];

		for(int j=i+1;j<nhits;j++) {
			if(eraseId[j]) continue;

			if(tofResponseVec[j].tray   != sumhit.tray ||
					tofResponseVec[j].module != sumhit.module ||
					tofResponseVec[j].cell   != sumhit.cell) continue;

			if(tofResponseVec[j].tof < sumhit.tof) {
				sumhit.tof      = tofResponseVec[j].tof;
				sumhit.s_track  = tofResponseVec[j].s_track;
				sumhit.position = tofResponseVec[j].position;
				if(tofResponseVec[j].trkId != sumhit.trkId) {
					LOG_WARN << " Two tracks match to one cell." << endm;
					sumhit.trkId = tofResponseVec[j].trkId;
				}
			}
			sumhit.dE   += tofResponseVec[j].dE;
			sumhit.dQ   += tofResponseVec[j].dQ;

			double dQdt[nTimeBins];  for(int aa=0;aa<nTimeBins;aa++){dQdt[aa]=sumhit.dQdt[aa];}

			if(sumhit.t0 == tofResponseVec[j].t0) {
				for(int m=0;m<nTimeBins;m++) { sumhit.dQdt[m] += tofResponseVec[j].dQdt[m];}
			}
			else if(sumhit.t0 > tofResponseVec[j].t0) {
				int nbinoffset = (int)((sumhit.t0 - tofResponseVec[j].t0)  / 25.);//pico seconds
				for(int m=0;m<nTimeBins;m++) {
					if(m<nbinoffset) {
						sumhit.dQdt[m] = tofResponseVec[j].dQdt[m];
					} else{
						sumhit.dQdt[m] = tofResponseVec[j].dQdt[m] + dQdt[m-nbinoffset];
					}
				}
				sumhit.t0 = tofResponseVec[j].t0;
			} else { //t0 < tofResponseVec[j].t0
				int nbinoffset = (int)((tofResponseVec[j].t0 - sumhit.t0)  / 25.);//pico seconds
				for(int m=0;m<nTimeBins;m++) {
					if(m<nbinoffset) {
						//do nothing
					} else{
						sumhit.dQdt[m] = tofResponseVec[j].dQdt[m-nbinoffset] + dQdt[m];
					}
				}
			}

			eraseId[j] = 1;
		}
		trackSumVec.push_back(sumhit);
	}



	/// store to McBTofHitCollection
	St_g2t_track *g2t_track = static_cast<St_g2t_track *>(mGeantData->Find("g2t_track"));
	if (!g2t_track) {
		LOG_WARN << " No g2t track table !!! " << endm;
		return kStWarn;
	}
	g2t_track_st *tof_track = g2t_track->GetTable();
	for(size_t i=0;i<trackSumVec.size();i++) {
		float tof = 0.;
		bool pass = kFALSE;
		for(int m=0;m<nTimeBins;m++) {
			if(trackSumVec[i].dQdt[m]>(mSimDb->adc_thre()*0.001) && !pass) {// pC
				tof = trackSumVec[i].tof;// ps
				pass = kTRUE;
				break;
			}
		}


		float deltaMRPC=ranGauss.shoot()*85.;  //!ps
		tof+=deltaMRPC;


		if(pass) {
			StMcBTofHit *mcHit = new StMcBTofHit();
			StMcTrack *partnerTrk = new StMcTrack(&(tof_track[trackSumVec[i].trkId]));
			int truthId=partnerTrk->key();
			mcHit->setTray(trackSumVec[i].tray);
			mcHit->setModule(trackSumVec[i].module);
			mcHit->setCell(trackSumVec[i].cell);
			mcHit->setdE(trackSumVec[i].dE);
			float pathLength=trackSumVec[i].s_track;
			mcHit->setPathLength(pathLength);//cm
			mcHit->setTime(trackSumVec[i].tof);
			mcHit->setTof(tof);//ps
			mcHit->setCharge(trackSumVec[i].dQ);
			mcHit->setPosition(trackSumVec[i].position);
			mcHit->setParentTrack(partnerTrk);
			mcHit->setParentTrackId(truthId);
			mMcBTofHitCollection->addHit(mcHit);


			if (mBookHisto){
				float beta=pathLength/tof/3e-2;
				mBetaHist->Fill(beta);
				mPathLHist->Fill(pathLength);
				mTofHist->Fill(tof);
				double momentum=partnerTrk->momentum().mag();
				double mass=sqrt(beta*beta*momentum*momentum/(1.-beta*beta));
				if(beta!=1.0 && pathLength>150){  mRecMass->Fill(mass);}
				mTofResReco->Fill( (tof - trackSumVec[i].t0*1000.) );//ps
			}
		}
	} //! end loop trackSumVec

	return kStOk;
}

//___________________________________________________________________________
int StBTofSimMaker::fillEvent()
{
	LOG_DEBUG << "Filling McEvent and Event"<<endm;

	// update histograms
	if(mBookHisto) {
		for(int i=0;i<mNTray;i++) {
			int ncell = 0;
			for(int j=0;j<mNTOF;j++) {
				if(mTofHitFlag[i][j]) {
					mCellGeant->Fill(j,i);
					ncell++;
				}
			}
			mNCellGeant->Fill(ncell,i);
		}
	}

	/// send off to StMcEvent
	mMcEvent = (StMcEvent*)GetInputDS("StMcEvent");
	if (!mMcEvent) {
		LOG_ERROR << "No StMcEvent! Bailing out ..." << endm;
	}
	else {
		mMcEvent->setBTofHitCollection(mMcBTofHitCollection);       //! Replaces existing collection with the passed argument
		LOG_INFO << " ... StMcBTofHitCollection stored in StMcEvent" << endm;
	}

	if (!mIsEmbedding) {  //! send off to StEvent

	  //! Store Collections
	  mBTofCollection = mEvent->btofCollection();
	  if(!mBTofCollection) {
		  LOG_INFO << "Creating new StBTofCollection" << endm;
		  mBTofCollection = new StBTofCollection();
		  mEvent->setBTofCollection(mBTofCollection);
	  }
	}
	else if (mIsEmbedding) { //! perform embedding
		
		LOG_INFO << "Creating new StBTofCollection locally" << endm;
		mBTofCollection = new StBTofCollection();
	}

	//! create StBTofHit / tofRawData / tofData collection
	for(int jj = 0; jj < (int)mMcBTofHitCollection->hits().size(); jj++) {
		StMcBTofHit *aMcBTofHit = mMcBTofHitCollection->hits()[jj];

		if(!aMcBTofHit) continue;

		int trayid = aMcBTofHit->tray();
		int moduleid = aMcBTofHit->module();
		int cellid = aMcBTofHit->cell();

		//! Efficiency
		float eff = 1.;
		if(trayid>0&&trayid<=120) eff = mSimDb->eff_tof(trayid, moduleid, cellid);
		if (gRandom->Uniform(1.0) > eff){LOG_DEBUG<<"Hit removed by inefficiency cut (at " << eff*100 << "%)"<<endm; continue; } //! inefficiency


		//! Fill the StBTofHit
		StBTofHit aBTofHit;
		aBTofHit.Clear();

		float mcTof=aMcBTofHit->tof()/1000.;      //! from picoseconds to nanoseconds

		aBTofHit.setHardwarePosition(kBTofId);
		aBTofHit.setTray((int)aMcBTofHit->tray());
		aBTofHit.setModule((unsigned char)aMcBTofHit->module());
		aBTofHit.setCell((int)aMcBTofHit->cell());
		aBTofHit.setLeadingEdgeTime((double)mcTof);
		aBTofHit.setTrailingEdgeTime((double)mcTof);
		aBTofHit.setAssociatedTrack(NULL);          //! done in StBTofMatchMaker
		aBTofHit.setIdTruth(aMcBTofHit->parentTrackId(), 1);
		mBTofCollection->addHit(new StBTofHit(aBTofHit));

		//! Fill the StBTofRawHit
		StBTofRawHit aBTofRawHit;
		aBTofRawHit.Clear();
		aBTofRawHit.setTray((int)aMcBTofHit->tray());
		aBTofRawHit.setChannel(6*(aMcBTofHit->module() - 1) + (int)aMcBTofHit->cell());
		aBTofRawHit.setFlag(1);
		mBTofCollection->addRawHit(new StBTofRawHit(aBTofRawHit));
	}

	if (!mIsEmbedding) {
		//! Fill StBTofHeader --

		StBTofHeader *tofHeader = mBTofCollection->tofHeader();
		StBTofHeader aHead;

		if(!tofHeader) {
			  LOG_INFO << " No StEvent/btofCollection, creating new... " << endm;
			  mBTofCollection->setHeader(new StBTofHeader(aHead));
		 }
		else {
			tofHeader = (StBTofHeader *) mBTofCollection->tofHeader();
		}

		LOG_INFO << "... StBTofCollection Stored in StEvent! " << endm;
	}
	
	//! check StMcEvent and StEvent
	if(Debug()) {
		LOG_DEBUG << " ==== Test McBTofHitCollection ==== " << endm;
		if (mMcEvent != nullptr) {
			StSPtrVecMcBTofHit& mcBTofHits = mMcEvent->btofHitCollection()->hits();
			int nCell[mNTray];
			for(int i=0;i<mNTray;i++) nCell[i] = 0;
			for(int i=0;i<(int)mcBTofHits.size();i++) {
				LOG_DEBUG << (*mcBTofHits[i]) << endm;

				if(mBookHisto) {
					int itray = mcBTofHits[i]->tray();
					int imodule = mcBTofHits[i]->module();
					int icell = mcBTofHits[i]->cell();
					float t0 = mcBTofHits[i]->time();
					float tof = mcBTofHits[i]->tof();
					float de = mcBTofHits[i]->dE();


					LOG_DEBUG << "tray# "<<itray << endm;

					//! fill BTOF histograms
					if(itray>0&&itray<=120) {
						mCellSeen->Fill((imodule-1)*mNCell+(icell-1),itray-1);
						mDeSeen->Fill( de / keV );
						mT0Seen->Fill( t0 /1000 ); // ns
						mTofSeen->Fill( tof / 1000 ); // ns
						mTofResSeen->Fill( (tof-t0) ); // ps
						nCell[itray-1]++;
					}
				}
			}
			if(mBookHisto) {
				for(int i=0;i<mNTray;i++) mNCellSeen->Fill(nCell[i],i);
			}

			LOG_INFO << " ==== Test TofRawDataCollection ==== " << endm;
			for(int i=0;i<mNTray;i++) nCell[i] = 0;

			if (mWriteStEvent){
				StSPtrVecBTofHit& bTofHits=mEvent->btofCollection()->tofHits();
				StBTofHit* bHit;
				for(int aa=0;aa<(int)bTofHits.size();aa++){
					bHit=bTofHits[aa];
					int itray=bHit->tray();
					int imodule=bHit->module();
					int icell=bHit->cell();
					if(mBookHisto) {mCellReco->Fill((imodule-1)*mNCell+(icell-1),itray-1);}
				}

				if(mBookHisto) {
					for(int i=0;i<mNTray;i++) mNCellReco->Fill(nCell[i],i);
				}
			}
		}
	}

	if(Debug()) cout<<"leaving fillevent"<<endl;

	return kStOk;
}



//_____________________________________________________________________________
IntVec StBTofSimMaker::CalcCellId(int volume_id, float ylocal)
{
	IntVec cellId;
	int ires    = volume_id;

	int rileft  = int(ires/10/100/100);   //! west (1) or east (2)
	ires          = ires-rileft*100*100*10;
	int itray   = int(ires/10/100);       //! tray id in half barrel
	ires          = ires-itray*100*10;
	int imodule = int(ires/10);           //! module id 1-32
	itray = itray + (rileft-1)*mNTray/2;    //! tray id 1-120

	int icell = int((ylocal + mBTofPadWidth * mNCell/2) / mBTofPadWidth) + 1;

	if(itray<=0 || itray>mNTray) itray = -1;
	if(imodule<=0 || imodule>mNModule) imodule = -1;
	if(icell<=0 || icell>mNCell) icell = -1;

	cellId.push_back(itray);
	cellId.push_back(imodule);
	cellId.push_back(icell);

	return cellId;
}

//_____________________________________________________________________________
double StBTofSimMaker::GammaRandom()
{
	double xmax,ymin,x,y,x1;
	xmax = 10.0;
	ymin = exp(-xmax);

back:
	y = ymin+(1-ymin)*gRandom->Rndm();
	x = -log(y);
	x1 = sqrt(xmax)*gRandom->Rndm();
	if(x1>sqrt(x)) goto back;
	return x/1.5;

}

//_____________________________________________________________________________
int StBTofSimMaker::CellXtalk(int icell, float ylocal, float& wt, int& icellx)
{
	float yc = (icell-1-2.5)*mBTofPadWidth;  //! y center in this pad
	float dy = ylocal - yc;

	wt = 1.;
	icellx = -1;
	float dyCut = mSimDb->dy_xtalk(); //! dyCut is by default set to 1
	if(fabs(dy)<dyCut) return kStOk;   //! no Xtalk when hit is in the cell center

	wt = 1. - (fabs(dy) - dyCut)/(mBTofPadWidth - 2.0*dyCut);

	if(dy>0) icellx = icell + 1;
	else     icellx = icell - 1;

	if(icellx>mNCell) icellx = -1;
	if(icellx<=0)    icellx = -1;

	return kStOk;
}


//_____________________________________________________________________________
int StBTofSimMaker::FastCellResponse(g2t_ctf_hit_st* tofHitsFromGeant, StBTofCollection* btofColl)
{
	//! Simulate the single cell response for a geant hit
	if((tofHitsFromGeant->s_track <= 0.0) || (tofHitsFromGeant->de / keV <= 0.0)) {
		return kStWarn;
	}
	
	if(mBookHisto) {
		mDeGeant->Fill(tofHitsFromGeant->de / keV);
		mTofGeant->Fill(tofHitsFromGeant->tof / nanosecond);
	}

	IntVec cellId   = CalcCellId(tofHitsFromGeant->volume_id, tofHitsFromGeant->x[1]);

	int icell, imodule, itray;
	itray   = cellId[0];
	imodule = cellId[1];
	icell   = cellId[2];
	if (itray==-1 || imodule==-1 || icell==-1) {
		LOG_WARN << " Not hit the sensitive MRPC volume !!! " << endm;
		return kStWarn;
	}

	StThreeVectorF local(tofHitsFromGeant->x[0], tofHitsFromGeant->x[1], tofHitsFromGeant->x[2]);

	St_g2t_track *g2t_track = static_cast<St_g2t_track *>(mGeantData->Find("g2t_track"));
	if (!g2t_track) {
		LOG_WARN << " No g2t track table !!! " << endm;
		return kStWarn;
	}
	g2t_track_st *tof_track = g2t_track->GetTable();
	int no_tracks= g2t_track->GetNRows();

	StMcTrack *partnerTrk = 0;
	int partnerTrkId;
	for(int j=0;j<no_tracks;j++){
		if(tofHitsFromGeant->track_p==tof_track[j].id){
			partnerTrk = new StMcTrack(&(tof_track[j]));
			partnerTrkId=partnerTrk->key();
		}
	}
	
	//! X-talk
	int icellx = -1;
	float wt = 1.0;
	if(mCellXtalk)   CellXtalk(icell, local.y(), wt, icellx);

	double de = tofHitsFromGeant->de * wt;
	double pathL = tofHitsFromGeant->s_track;
	double q = 0.;
	
	double Rawtof = tofHitsFromGeant->tof*1000./nanosecond;
	float Rawbeta=pathL/Rawtof/3e-2;
	double momentum=partnerTrk->momentum().mag();
	double mass=partnerTrk->fourMomentum().m();
	double calcTof=pathL/(3e-2)/sqrt(1 - mass*mass/(momentum*momentum + mass*mass));
	
	double time_blur = ranGauss.shoot()*mSimResDb->timeres_tof(itray, imodule, icell)*1e-9/nanosecond;
	double tof = tofHitsFromGeant->tof*1000./nanosecond + time_blur;    //! 85ps per channel

	if ( mVpdSim ) {    // VpdSimMaker present, assume vpdstart
		tof += mVpdSimConfig->getMcClock()*1000;
	}
	else {
		if ( mUseVpdStart && btofColl ) {   //!< VpdSimMaker not present, check for vpdstart, add vpd resolution to tof
			mBTofHeader = btofColl->tofHeader();

			if ( mBTofHeader != NULL ){
				int mNWest = mBTofHeader->numberOfVpdHits(west);
				int mNEast = mBTofHeader->numberOfVpdHits(east);
				
				tof += mVpdSimConfig->getVpdResolution(mNWest, mNEast);
			} else {
				tof -= 999;
			}
			
		}
		else {
			// do nothing
		}
	}
	
	//    tof = tof - mSimDb->toffset();  // Apply offset correction.
	double t0 = tofHitsFromGeant->tof*1000./nanosecond;
	float beta=pathL/tof/3e-2;

	StMcBTofHit *mcBTofHit = new StMcBTofHit(itray,imodule,icell,de,pathL,t0,tof,q);
	mcBTofHit->setPosition(local);
	mcBTofHit->setParentTrack(partnerTrk);
	

	storeMcBTofHit(mcBTofHit);
	mTofHitFlag[itray-1][(imodule-1)*mNCell+(icell-1)] = 1;
	

	if(icellx <= 0 || icellx > mNCell) return kStOk;  //! no X-talk
	///
	/// X talk signal
	///
	time_blur = ranGauss.shoot()*mSimResDb->timeres_tof(itray, imodule, icell)*1e-9/nanosecond;
	double tofx = tofHitsFromGeant->tof*1000./nanosecond + time_blur;    //! 85ps per channel
	double dex = tofHitsFromGeant->de * (1. - wt);
	double qx = 0.*(1.-wt);

	StMcBTofHit *mcBTofHitx = new StMcBTofHit(itray,imodule,icellx,dex,pathL,t0,tofx,qx);
	mcBTofHitx->setPosition(local);
	mcBTofHitx->setParentTrack(partnerTrk);
	mcBTofHitx->setParentTrackId(partnerTrkId);

	storeMcBTofHit(mcBTofHitx);
	mTofHitFlag[itray-1][(imodule-1)*mNCell+(icellx-1)] = 1;
	
	
	return kStOk;
}

//_____________________________________________________________________________
int StBTofSimMaker::storeMcBTofHit(StMcBTofHit* mcBTofHit)
{
	/** this function adds a hit to a previous hit (if they mactch the same cell location),
	* or it stores the new hit (the last part below)
	 */
	bool hitFound = kFALSE;

	//! this is primarily for VPD hits
	for(size_t j=0;j<mMcBTofHitCollection->hits().size();j++) {
		StMcBTofHit *tempHit = mMcBTofHitCollection->hits()[j];
		if(!tempHit) continue;
		if(mcBTofHit->sameCell(*tempHit)) {
			hitFound = kTRUE;
			float t1 = mcBTofHit->time();
			float t2 = tempHit->time();
			float tof1 = mcBTofHit->tof();
			float dE1 = mcBTofHit->dE();
			float dE2 = tempHit->dE();
			float s1 = mcBTofHit->pathLength();
			float q1 = mcBTofHit->charge();
			float q2 = tempHit->charge();
			StThreeVectorF x1 = mcBTofHit->position();
			StThreeVectorF x2 = tempHit->position();
			StMcTrack *trk1 = mcBTofHit->parentTrack();
			if(t1>t2) {
				//do nothing
			} else {
				tempHit->setTime(t1);
				tempHit->setTof(tof1);
				tempHit->setPathLength(s1);
				tempHit->setPosition(x1);
				tempHit->setParentTrack(trk1);
			}
			tempHit->setdE(dE1+dE2);
			tempHit->setCharge(q1+q2);
		}
	}

	if(!hitFound) {
		mMcBTofHitCollection->addHit(mcBTofHit);
	} else {
		delete mcBTofHit;
	}
	return kStOk;
}
//_____________________________________________________________________________
/// digitize to ADC and TDC entries (empty)
int StBTofSimMaker::fillRaw(){
	//not currently used
	//fill the adc and tdc entries.
	return kStOk;

}

//_____________________________________________________________________________
/// simulate electronic noise (empty)
int StBTofSimMaker::electronicNoise(){
	return kStOk;
}

//_____________________________________________________________________________
string  StBTofSimMaker::setHistFileName(){
	
	string extension = ".BTofSim.root";

	if (GetChainOpt()->GetFileOut() != NULL) {
		TString outFile = GetChainOpt()->GetFileOut();
		mHistoFileName = (string)outFile;
		size_t lastindex = mHistoFileName.find_last_of(".");
		mHistoFileName = mHistoFileName.substr(0, lastindex);
		lastindex = mHistoFileName.find_last_of("/");
		mHistoFileName = mHistoFileName.substr(lastindex+1, mHistoFileName.length());
		mHistoFileName = mHistoFileName + extension;
	}

	return mHistoFileName;
}
//_____________________________________________________________________________
int StBTofSimMaker::bookHistograms()
{
	//! only done if Histogram setting is turned on
	AddHist( mRawBetaHist=new TH1F("mRawBetaHist","mRawBetaHist", 400, -1, 1.5) );
	AddHist( mBetaHist=new TH1F("mBetaHist","mBetaHist", 400, -2, 2) );
	
	AddHist( mRawBetaVsMom=new TH2F("mRawBetaVsMom","mRawBetaVsMom; Momentum (GeV); 1/beta; counts", 1500, 0.1, 1.25, 1500, 0.6, 3) );
	AddHist( mCalcBetaVsMom=new TH2F("mCalcBetaVsMom","mCalcBetaVsMom; Momentum (GeV); 1/beta; counts", 1500, 0.1, 1.25, 1500, 0.6, 3) );
	AddHist( mBetaVsMom=new TH2F("mBetaVsMom","mBetaVsMom; Momentum (GeV); 1/beta; counts", 1500, 0.1, 1.25, 1500, 0.6, 3) );
	
	AddHist( Electron_BetaVsMom=new TH2F("Electron_BetaVsMom","Electron_BetaVsMom; Momentum (GeV); 1/beta; counts", 1500, 0.1, 1.25, 1500, 0.6, 3) );
	AddHist( Muon_BetaVsMom=new TH2F("Muon_BetaVsMom","Muon_BetaVsMom; Momentum (GeV); 1/beta; counts", 1500, 0.1, 1.25, 1500, 0.6, 3) );
	AddHist( Pion_BetaVsMom=new TH2F("Pion_BetaVsMom","Pion_BetaVsMom; Momentum (GeV); 1/beta; counts", 1500, 0.1, 1.25, 1500, 0.6, 3) );
	AddHist( Kaon_BetaVsMom=new TH2F("Kaon_BetaVsMom","Kaon_BetaVsMom; Momentum (GeV); 1/beta; counts", 1500, 0.1, 1.25, 1500, 0.6, 3) );
	AddHist( Proton_BetaVsMom=new TH2F("Proton_BetaVsMom","Proton_BetaVsMom; Momentum (GeV); 1/beta; counts", 1500, 0.1, 1.25, 1500, 0.6, 3) );
	
	AddHist( mPathLHist=new TH1F("mPathLHist","mPathLHist", 500, -2, 500) );//cm's
	AddHist( mRawTofHist=new TH1F("mRawTofHist","mRawTofHist",2000, -10, 25000) );
	AddHist( mTofHist=new TH1F("mTofHist","mTofHist; Tof (ps)", 2000, -10, 25000) );
	AddHist( mRecMass=new TH1F("mRecMass","mRecMass", 1000, -2, 4) );
	
	AddHist( massHist=new TH1F("massHist","Mass; Mass (GeV); Counts",200, 0, 4) );
	AddHist( m2VsP=new TH2F("m2VsP","Mass Sqared Vs Momentum; Momentum (P); Mass Squared (GeV^2)",2000, 0.1, 1.5, 100, 0, 4) );
	AddHist( mTofCalculated=new TH1F("mTofCalculated","Calculated Tof using mass and momentum; Tof (ps); Counts", 2000, -10, 25000) );
	
	AddHist( tof_RealVsCalc=new TH2F("tof_RealVsCalc","Resolution-smeared Tof Vs. Calculated Tof; Calculated Tof (ps); Given Tof (ps)", 2000, -10, 25000, 2000, -10, 25000) );
	
	AddHist( momBinRaw1 = new TH1F("Raw_0.15<P<0.2","Raw (No resolution smearing) 1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBinRaw2 = new TH1F("Raw_0.2<P<0.25","Raw (No resolution smearing) 1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBinRaw3 = new TH1F("Raw_0.35<P<0.4","Raw (No resolution smearing) 1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBinRaw4 = new TH1F("Raw_0.4<P<0.45","Raw (No resolution smearing) 1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBinRaw5 = new TH1F("Raw_0.55<P<0.6","Raw (No resolution smearing) 1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBinRaw6 = new TH1F("Raw_0.65<P<0.66","Raw (No resolution smearing) 1/beta in Momentum Bin; 1/beta; Counts", 400,0.5,2) );
	AddHist( momBinRaw7 = new TH1F("Raw_0.7<P<0.75","Raw (No resolution smearing) 1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBinRaw8 = new TH1F("Raw_0.23<P<0.24","Raw (No resolution smearing) 1/beta in Momentum Bin; 1/beta; Counts", 400,0.5,2) );
	
	AddHist( momBin1 = new TH1F("0.15<P<0.2","1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBin2 = new TH1F("0.2<P<0.25","1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBin3 = new TH1F("0.35<P<0.4","1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBin4 = new TH1F("0.4<P<0.45","1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBin5 = new TH1F("0.55<P<0.6","1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBin6 = new TH1F("0.65<P<0.66","1/beta in Momentum Bin; 1/beta; Counts", 400,0.5,2) );
	AddHist( momBin7 = new TH1F("0.7<P<0.75","1/beta in Momentum Bin; 1/beta; Counts", 600,0.5,2) );
	AddHist( momBin8 = new TH1F("0.23<P<0.24","1/beta in Momentum Bin; 1/beta; Counts", 400,0.5,2) );

	AddHist( mCellGeant  = new TH2F("CellGeant","CellGeant",192,0.,192.,120,1.,120.) );
	AddHist( mNCellGeant = new TH2F("NCellGeant","NCellGeant",192,0.,192.,120,1.,120.) );
	AddHist( mDeGeant    = new TH1F("DeGeant","DeGeant",1000,0.,10.) );      //! 10 keV
	AddHist( mTofGeant   = new TH1F("TofGeant","TofGeant",1000,0.,20.) );    //! 20 ns

	AddHist( mCellSeen   = new TH2F("CellSeen","CellSeen",192,0.,192.,120,1.,120.) );
	AddHist( mNCellSeen  = new TH2F("NCellSeen","NCellSeen",192,0.,192.,120,1.,120.) );
	AddHist( mDeSeen     = new TH1F("DeSeen","DeSeen",1000,0.,10.) );        //! 10 kev
	AddHist( mT0Seen    = new TH1F("T0Seen","T0Seen",1000,0.,20.) );      //! ns
	AddHist( mTofSeen    = new TH1F("TofSeen","TofSeen",1000,0.,20.) );      //! 20 ns

	AddHist( mTofResSeen = new TH1F("TofResSeen","TofResSeen",1001,-500.,500.) );  //! ps

	AddHist( mCellReco   = new TH2F("CellReco","CellReco",192,0.,192.,120,1.,120.) );
	AddHist( mNCellReco  = new TH2F("NCellReco","NCellReco",192,0.,192.,120,1.,120.) );
	AddHist( mTofResReco = new TH1F("TofResReco","TofResReco",1000,-300.,300.) );  //! ps
	
	return kStOk;

}

Int_t StBTofSimMaker::writeHistograms()
{
	//only done if Histogram setting is turned on

    mRawBetaHist->Write();
	mBetaHist->Write();
    
    mRawBetaVsMom->Write();
    mCalcBetaVsMom->Write();
    mBetaVsMom->Write();
    
    Electron_BetaVsMom->Write();
    Muon_BetaVsMom->Write();
    Pion_BetaVsMom->Write();
    Kaon_BetaVsMom->Write();
    Proton_BetaVsMom->Write();
    
	mPathLHist->Write();
    mRawTofHist->Write();
	mTofHist->Write();
	mRecMass->Write();
    
    massHist->Write();
    m2VsP->Write();
    mTofCalculated->Write();
    tof_RealVsCalc->Write();
    
    momBinRaw1->Write();
    momBinRaw2->Write();
    momBinRaw3->Write();
    momBinRaw4->Write();
    momBinRaw5->Write();
    momBinRaw6->Write();
    momBinRaw7->Write();
    momBinRaw8->Write();
    
    momBin1->Write();
    momBin2->Write();
    momBin3->Write();
    momBin4->Write();
    momBin5->Write();
    momBin6->Write();
    momBin7->Write();
    momBin8->Write();

	mCellGeant->Write();
	mNCellGeant->Write();
	mDeGeant->Write();
	mTofGeant->Write();

	mCellSeen->Write();
	mNCellSeen->Write();
	mDeSeen->Write();
	mT0Seen->Write();
	mTofSeen->Write();
	mTofResSeen->Write();

	mCellReco->Write();
	mNCellReco->Write();
	mTofResReco->Write();

	return kStOk;
}

// End StBTofSimMaker
