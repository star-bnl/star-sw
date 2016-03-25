/***************************************************************************
 *
 * $Id: StBTofSimMaker.cxx,v 1.9 2015/07/28 22:49:55 smirnovd Exp $
 *
 * Author: Frank Geurts
 ***************************************************************************
 *
 * Description: StBTofSimMaker class for Barrel TOF Simulations
 *
 ***************************************************************************
 *
 * $Log: StBTofSimMaker.cxx,v $
 * Revision 1.9  2015/07/28 22:49:55  smirnovd
 *  Initialize static constants outside of class definition
 *
 *  C++ forbids initialization of non-integral static const members within the class
 *  definition. The syntax is allowed only for integral type variables.
 *
 * Revision 1.8  2015/06/30 18:00:38  genevb
 * Enable MC hits in embedding (RT ticket 3116, Geurts approval)
 *
 * Revision 1.7  2013/06/13 14:00:51  geurts
 * improve log message for inefficiency cuts
 *
 * Revision 1.6  2012/05/07 14:16:40  fisyak
 * Write hit to StEvent, set kBTofId for hit
 *
 * Revision 1.5  2011/02/03 19:01:01  geurts
 * Introduce option to switch writing simulated hits to StEvent. Default behavior is OFF.
 *
 * Revision 1.4  2010/08/10 19:18:31  geurts
 * Look for geant data in bfc ("geant") or geant.root ("geantBranch"); Protect storing BTofMcHitCollection in case McEvent is NULL.  [Xin]
 *
 * Revision 1.3  2010/07/14 20:44:10  geurts
 * Correct application of vpd resolution smearing: The original values in the db (or ParSim) are in ps [Xin]
 *
 * Revision 1.2  2010/07/14 20:32:57  geurts
 * remove geometry initialization (not used)
 *
 * Revision 1.1  2009/12/09 21:56:41  dthein
 * First version of StBTofSimMaker
 * 
 *
 **************************************************************************/
//! Time-of-Flight Simulator Maker
/*! \class StTofSimMaker
  \author Frank Geurts

  <p>TOF simulation software. This Maker further processes the simulated
  detector response from GSTAR's GEANT simulation. It takes the G2T tof
  hit tables and build an StEvent Tof SlatCollection.</p>
 */
#include <Stiostream.h>
#include "StBTofSimMaker.h"

// SCL
#include <math.h>
#include "TRandom.h"
#include "SystemOfUnits.h"
#include "phys_constants.h"
#include "StThreeVectorD.hh"
#include "Random.h"
#include "RanluxEngine.h"
#include "RandGauss.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TMath.h"
// g2t tables and collections
#include "StMcTrack.hh"

#include "StBTofUtil/StBTofDaqMap.h"
#include "StTofUtil/tofPathLength.hh"
#include "StTofUtil/StTofSimParam.h"
#include "StBTofUtil/StBTofGeometry.h"
#include "StEventTypes.h"
#include "StEvent/StBTofCollection.h"
#include "StDetectorDbMaker/St_tofStatusC.h"
#include "StDetectorDbMaker/St_tofTOffsetC.h"
#include "StDetectorDbMaker/St_tofTotbCorrC.h"
#include "StDetectorDbMaker/St_tofZbCorrC.h"
#include "StDetectorDbMaker/St_vpdTotCorrC.h"

static RanluxEngine engine;
static RandGauss ranGauss(engine);

ClassImp(StBTofSimMaker)

float StBTofSimMaker::mVHRBIN2PS = 24.4;  //! Very High resolution mode, ps/bin
float StBTofSimMaker::mHRBIN2PS = 97.7;     //! High resolution mode, ps/bin
float StBTofSimMaker::mBTofPadWidth = 3.45;        //! Pad Width                                                              
//_____________________________________________________________________________
Bool_t StBTofSimMaker::TimeAtVertex(Int_t TrackId,  St_g2t_track *track, St_g2t_vertex *vertex, Double_t &tofV) {
  tofV = 0;
  if (TrackId <= 0 || TrackId > track->GetNRows()) return kFALSE;
  g2t_track_st *tof_track = track->GetTable();
  Int_t id3 = tof_track[TrackId-1].start_vertex_p;
  if (id3 <= 0 || id3 > vertex->GetNRows()) return kFALSE;
  g2t_vertex_st *gver = vertex->GetTable();
  tofV = gver[id3-1].ge_tof;
  return kTRUE;
}
//_____________________________________________________________________________
StBTofSimMaker::StBTofSimMaker(const char *name):StMaker(name)
{
	//set default values
	mBookHisto=kFALSE;// histograms 
	mSlow=kTRUE;
	mCellXtalk=kTRUE;
	mWriteStEvent=kTRUE;
	mDaqMap=0;
	mMcBTofHitCollection = 0;
	Reset();

}

//_____________________________________________________________________________
StBTofSimMaker::~StBTofSimMaker()
{
	delete mSimDb;
	delete mDaqMap;

}


//_____________________________________________________________________________
Int_t StBTofSimMaker::Init()
{  
	Reset();
	mSimDb = new StTofSimParam();
	if (Debug()) mSimDb->print();
	if(mBookHisto) bookHistograms();

	return StMaker::Init();
}

//_____________________________________________________________________________
void StBTofSimMaker::Reset()
{
	mGeantData = 0;
	mEvent  = 0;
	mMcEvent = 0;
	//mBTofCollection = 0;
	//if (mWriteStEvent) delete mBTofCollection;
	delete mMcBTofHitCollection;
	mSimDb  = 0;

	if(mDaqMap){delete mDaqMap; mDaqMap = 0;}

	ResetFlags();
}

//_____________________________________________________________________________
Int_t StBTofSimMaker::ResetFlags()
{
	/// TOF hit occupancy flag
	memset(mTofHitFlag, 0, sizeof(mTofHitFlag));
	memset(mVpdHitFlag, 0, sizeof(mVpdHitFlag));
	return kStOk;
}

//_____________________________________________________________________________
Int_t StBTofSimMaker::InitRun(Int_t runnumber)
{
	LOG_INFO << "StBTofSimMaker::InitRun  -- initializing BTOF DAQ map --" << endm;

	/// MRPC-TOF DAQ map
	mDaqMap = new StBTofDaqMap();
	mDaqMap->Init(this);

	return kStOK;
}

//_____________________________________________________________________________
Int_t StBTofSimMaker::FinishRun(Int_t runnumber)
{
	LOG_INFO << "StBTofSimMaker::FinishRun -- cleaning up BTOF DAQ map --" << endm;
	if (mDaqMap){delete mDaqMap; mDaqMap = 0;}
	return kStOk;
}

//_____________________________________________________________________________
Int_t StBTofSimMaker::Finish()
{
	if(mBookHisto){
		LOG_INFO << "StBTofSimMaker::Finish  writing tofsim.root ...";
		TFile theFile(mHistFile.c_str(),"RECREATE","tofsim");
		theFile.cd();
		writeHistograms();
		theFile.Close();
	}


	return kStOK;
}


//_____________________________________________________________________________
Int_t StBTofSimMaker::Make()
{
	LOG_INFO << "StBTofSimMaker  Make() starts" << endm;

	ResetFlags();

	mMcBTofHitCollection = new StMcBTofHitCollection();

	// Check to see that there are GEANT hits
	mGeantData = GetInputDS("geant"); // in bfc chain
	if(!mGeantData) { // when reading the geant.root file
		mGeantData = GetInputDS("geantBranch");
	}
	if(!mGeantData) {
		LOG_WARN << " No GEANT data loaded. Exit! " << endm;
		return kStWarn;
	}
	LOG_INFO << " Found GEANT data -- loading VPD/TOF hits... " << endm;

	// Look for VPD hits
	St_g2t_vpd_hit* g2t_vpd_hits = 0;
	g2t_vpd_hits = dynamic_cast<St_g2t_vpd_hit*>(mGeantData->Find("g2t_vpd_hit"));
	if(!g2t_vpd_hits){
		LOG_WARN << " No VPD hits in GEANT" << endm; }
	else {
		Int_t nvpdhits = g2t_vpd_hits->GetNRows();
		LOG_DEBUG << " Found VPD hits: " << nvpdhits << endm; 
		g2t_vpd_hit_st* vpd_hit = g2t_vpd_hits->begin();
		for(Int_t i=0; i<nvpdhits; i++, vpd_hit++) {
			VpdResponse(vpd_hit);
		}
	}  
	LOG_INFO << " McBTofHit Size (VPD) = " << mMcBTofHitCollection->hits().size() << endm;


	// Look for TOF hits
	St_g2t_ctf_hit* g2t_tfr_hits = 0;
	g2t_tfr_hits = dynamic_cast<St_g2t_ctf_hit*> (mGeantData->Find("g2t_tfr_hit"));
	if(!g2t_tfr_hits) {
		LOG_WARN << " No TOF hits in GEANT" << endm; }
	else {
		Int_t nhits = g2t_tfr_hits->GetNRows();
		LOG_DEBUG << " Found GEANT TOF hits: " << nhits << endm;
		g2t_ctf_hit_st* tofHitsFromGeant = g2t_tfr_hits->begin();

		if(mSlow) {
			//fill this vector with particles that hit the tof and the tof's responses
			TrackVec tofResponseVec;
			tofResponseVec.clear(); 
			for (Int_t i=0; i<nhits; i++, tofHitsFromGeant++) {
				//for every tof hit possible add a response (and neighboring cell response if Xtalk is enabled)
				CellResponse(tofHitsFromGeant,tofResponseVec);
			}
			//compute ToT for all responses saved in tofResponseVec
			CellTimePassTh(tofResponseVec);
		}
		else if(!mSlow) {
			for (Int_t i=0; i<nhits; i++, tofHitsFromGeant++) FastCellResponse(tofHitsFromGeant);
		} 
		else
			LOG_WARN << " No TOF simulator specified " << endm;  
	}
	LOG_INFO << " McBTofHit Size (TOF) = " << mMcBTofHitCollection->hits().size() << endm;


	fillEvent();


	LOG_INFO << " Leaving StBTofSimMaker ... done " << endm;
	return kStOK;
}

//_____________________________________________________________________________
/// Vpd fast simulator
///  Start simulator only uses fast simulation at the moment.
Int_t StBTofSimMaker::VpdResponse(g2t_vpd_hit_st* vpd_hit)
{
	if(!vpd_hit) {
		LOG_WARN << " No VPD hit!" << endm;
		return kStWarn;
	}
	St_g2t_track *g2t_track = (St_g2t_track *) GetDataSet("geant/g2t_track"); //  if (!g2t_track)    return kStWarn;
	if (!g2t_track) {
	  LOG_WARN << " No G2T track table!" << endm;
	  return kStWarn;
	};
	St_g2t_vertex  *g2t_vertex = (St_g2t_vertex *) GetDataSet("geant/g2t_vertex");// if (!g2t_ver)      return kStWarn;
	if (!g2t_vertex) {
	  LOG_WARN << " No G2T vertex table!" << endm;
	  return kStWarn;
	};
	
	Double_t tofV = 0;
	Int_t TrackId         = vpd_hit->track_p;
	if (! TimeAtVertex(TrackId, g2t_track, g2t_vertex, tofV)) return kStWarn;
	//	if (TMath::Abs(tofV) > 45e-9) return kStOK; // 45 ns trigger matching window 

	Int_t vId = vpd_hit->volume_id;
	Int_t itray = vId/1000 + 120;    //! 121: west, 122: east
	Int_t itube = vId%100;           //! 1-19

	Double_t tof = (tofV + vpd_hit->tof + ranGauss.shoot()*mSimDb->timeres_vpd())*1000./nanosecond; //! 140 ps per channel
	Double_t t0 =  (tofV + vpd_hit->tof)*1000./nanosecond;
	Double_t de = vpd_hit->de;
	Double_t pathL = -9999.; //! NA for vpd
	Double_t q = 0.;

	// module '0' below means VPD hit 
	StMcBTofHit *mcHit = new StMcBTofHit(itray,0,itube,de,pathL,t0,tof,q);
	mcHit->setParentTrackId(TrackId);
	storeMcBTofHit(mcHit);
	mVpdHitFlag[(itray-121)*mNVPD+(itube-1)] = 1;

	return kStOk;
}

//_____________________________________________________________________________
/// MRPC-TOF slow simulator
Int_t StBTofSimMaker::CellResponse(g2t_ctf_hit_st* tofHitsFromGeant,
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

	// accepty TOF hit
	if(tofHitsFromGeant->s_track<=0.0 || tofHitsFromGeant->de <=0.0) {
		LOG_DEBUG << " No deposited energy in this TOF hit!" << endm;
		return kStWarn;
	}

	IntVec cellId   = CalcCellId(tofHitsFromGeant->volume_id, tofHitsFromGeant->x[1]);
	Int_t icell, imodule, itray;
	itray   = cellId[0];
	imodule = cellId[1];
	icell   = cellId[2];
	if (itray==-1 || imodule==-1 || icell==-1) {
		LOG_WARN << " Not hit the sensitive MRPC volume!" << endm;
		return kStWarn;
	}
	if (St_tofStatusC::instance()->status(itray,imodule,icell) != 1) return kStOK;
	St_g2t_track *g2t_track = (St_g2t_track *) GetDataSet("geant/g2t_track"); //  if (!g2t_track)    return kStWarn;
	if (!g2t_track) {
	  LOG_WARN << " No G2T track table!" << endm;
	  return kStWarn;
	};
	St_g2t_vertex  *g2t_vertex = (St_g2t_vertex *) GetDataSet("geant/g2t_vertex");// if (!g2t_ver)      return kStWarn;
	if (!g2t_vertex) {
	  LOG_WARN << " No G2T vertex table!" << endm;
	  return kStWarn;
	};
	Double_t tofV = 0;
	Int_t TrackId         = tofHitsFromGeant->track_p;
	if (! TimeAtVertex(TrackId, g2t_track, g2t_vertex, tofV)) return kStWarn;
	//	if (TMath::Abs(tofV) > 45e-9) return kStOK; // 45 ns trigger matching window 
	if(mBookHisto) {
		mDeGeant->Fill(tofHitsFromGeant->de / keV);
		mTofGeant->Fill((tofV + tofHitsFromGeant->tof) / nanosecond);
	}
	g2t_track_st *tof_track = g2t_track->GetTable();

	
	Double_t beta = tof_track[TrackId-1].ptot/tof_track[TrackId-1].e;

	Double_t qtot=-1;
	Double_t tof=-1;
	Double_t t0 = tofV + tofHitsFromGeant->tof;
	Float_t wt=1.0;

	Double_t clusterDensity = mSimDb->nclus(beta); 
	Double_t gapLength  = mSimDb->dg();       
	Double_t alpha   = mSimDb->alpha(); 
	Double_t ka  = mSimDb->ka();        
	Double_t kaa = ka/(alpha*gapLength);

	const Int_t maxClusters=mSimDb->nmaxclus();
	const Int_t nTimeBins = mSimDb->ndt();
	Double_t driftVelocity[maxClusters],nElectrons[maxClusters],startPositionOfCluster[maxClusters],sa[maxClusters];
	Double_t s[maxClusters][nTimeBins];

	Double_t chargeDepositedPerTimeBin[nTimeBins];
	for(Int_t j=0;j<nTimeBins;j++) {chargeDepositedPerTimeBin[j] = 0.0;}

	Int_t nElectronClusters=-1;
	while(nElectronClusters<1) {nElectronClusters=gRandom->Poisson(gapLength*clusterDensity);}
	if(nElectronClusters>maxClusters) nElectronClusters = maxClusters;

	for(Int_t m=0;m<nElectronClusters;m++) {
		driftVelocity[m] = mSimDb->vd_mean()*(0.90+0.20*gRandom->Rndm(1));  //!mm/ps

		Int_t nElectrons_temp=-1;
		while(nElectrons_temp<1){nElectrons_temp = gRandom->Poisson(mSimDb->nmeane());}
		nElectrons[m] = Double_t(nElectrons_temp);

		startPositionOfCluster[m]=gapLength+1;
		while(startPositionOfCluster[m]>gapLength) { startPositionOfCluster[m] = gRandom->Exp(1.0/clusterDensity);  }  //mm
	}  

	Double_t ytmp=0.0; 
	for(Int_t m=0;m<nElectronClusters;m++) {
		sa[m] = (exp(alpha*(gapLength-startPositionOfCluster[m]))-1)*nElectrons[m]*GammaRandom();  
		if (sa[m]>mSimDb->nmaxe()) sa[m] = mSimDb->nmaxe();
		ytmp += kaa*sa[m];
	}
	qtot = ytmp*1.e+12*e_SI;  //pC

	t0 = (tofV + tofHitsFromGeant->tof)*1000/nanosecond;  //ps
	tof= (tofV + tofHitsFromGeant->tof)*1000/nanosecond;  //ps


	for(Int_t j=0;j<nTimeBins;j++) {
		Double_t ts = t0+ mSimDb->dt()*double(j);  //dt=25ps 
		Double_t ytmp1 = 0.;
		for(Int_t m=0;m<nElectronClusters;m++) { 
			Double_t tx = (startPositionOfCluster[m])/(C_C_LIGHT*1.e-3*nanosecond/millimeter);
			Double_t t_drift = (gapLength-startPositionOfCluster[m])/driftVelocity[m];
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


	Int_t icellx = -1;
	wt = 1.0;
	StThreeVectorF local(tofHitsFromGeant->x[0], tofHitsFromGeant->x[1], tofHitsFromGeant->x[2]);
	if(mCellXtalk){ CellXtalk(icell, local.y(), wt, icellx); }
	TrackHit trackhit;
	trackhit.tray    = itray;
	trackhit.module  = imodule;
	trackhit.cell    = icell;
	trackhit.trkId   = TrackId-1;
	trackhit.dE      = tofHitsFromGeant->de * wt;
	trackhit.dQ      = qtot * wt; 
	for(Int_t j=0;j<nTimeBins;j++) {
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
		for(Int_t j=0;j<nTimeBins;j++) {
			trackhitx.dQdt[j] = chargeDepositedPerTimeBin[j] * (1.-wt);
		}
		tofResponseVec.push_back(trackhitx);
		mTofHitFlag[itray-1][(imodule-1)*mNCell+(icellx-1)] = 1;
	}

	return kStOk;
}

//____________________________________________________________________________
Int_t StBTofSimMaker::CellTimePassTh(TrackVec& tofResponseVec)
	//only for SLOW simulation (i.e. slow simulator part2)
	// Corrects response depending on which track is recorded (since electronics can only see first)
	// Stores the output into StBTof container

{
	TrackVec  trackSumVec;
	trackSumVec.clear();

	Int_t eraseId[500000];
	Int_t nhits = tofResponseVec.size();
	Int_t nTimeBins = mSimDb->ndt();
	for(Int_t i=0;i<nhits;i++) eraseId[i] = 0;


	for(Int_t i=0;i<nhits;i++){
		if(eraseId[i]) continue; 
		TrackHit sumhit=tofResponseVec[i]; 

		for(Int_t j=i+1;j<nhits;j++) {   
			if(eraseId[j]) continue;

			if(tofResponseVec[j].tray   != sumhit.tray ||
					tofResponseVec[j].module != sumhit.module ||
					tofResponseVec[j].cell   != sumhit.cell) continue;

			if(tofResponseVec[j].tof < sumhit.tof) {
				sumhit.tof      = tofResponseVec[j].tof;
				sumhit.s_track  = tofResponseVec[j].s_track;
				sumhit.position = tofResponseVec[j].position;
				if(tofResponseVec[j].trkId != sumhit.trkId) {
					LOG_DEBUG << " Two tracks match to one cell." << endm;
					sumhit.trkId = tofResponseVec[j].trkId;
				}
			}
			sumhit.dE   += tofResponseVec[j].dE;
			sumhit.dQ   += tofResponseVec[j].dQ;

			Double_t dQdt[nTimeBins];  for(Int_t aa=0;aa<nTimeBins;aa++){dQdt[aa]=sumhit.dQdt[aa];}

			if(sumhit.t0 == tofResponseVec[j].t0) {
				for(Int_t m=0;m<nTimeBins;m++) { sumhit.dQdt[m] += tofResponseVec[j].dQdt[m];}
			} 
			else if(sumhit.t0 > tofResponseVec[j].t0) {
				Int_t nbinoffset = (int)((sumhit.t0 - tofResponseVec[j].t0)  / 25.);//pico seconds
				for(Int_t m=0;m<nTimeBins;m++) {
					if(m<nbinoffset) {
						sumhit.dQdt[m] = tofResponseVec[j].dQdt[m];
					} else{
						sumhit.dQdt[m] = tofResponseVec[j].dQdt[m] + dQdt[m-nbinoffset];
					}
				}
				sumhit.t0 = tofResponseVec[j].t0;
			} else { //t0 < tofResponseVec[j].t0
				Int_t nbinoffset = (int)((tofResponseVec[j].t0 - sumhit.t0)  / 25.);//pico seconds
				for(Int_t m=0;m<nTimeBins;m++) {
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
		Float_t tof = 0.;
		bool pass = kFALSE;
		for(Int_t m=0;m<nTimeBins;m++) {
			if(trackSumVec[i].dQdt[m]>(mSimDb->adc_thre()*0.001) && !pass) {// pC
				tof = trackSumVec[i].tof;// ps
				pass = kTRUE;
				break;
			}
		}


		Float_t deltaMRPC=ranGauss.shoot()*85.;  //!ps
		tof+=deltaMRPC;


		if(pass) {
			StMcBTofHit *mcHit = new StMcBTofHit();
			StMcTrack *partnerTrk = new StMcTrack(&(tof_track[trackSumVec[i].trkId]));
			Int_t truthId=partnerTrk->key();
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
				Float_t beta=pathLength/tof/3e-2;
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
Int_t StBTofSimMaker::fillEvent()
{
	LOG_DEBUG << "Filling McEvent and Event"<<endm;

	// update histograms
	if(mBookHisto) {
		for(Int_t i=0;i<mNTray;i++) {
			Int_t ncell = 0;
			for(Int_t j=0;j<mNTOF;j++) { 
				if(mTofHitFlag[i][j]) {
					mCellGeant->Fill(j,i);
					ncell++;
				}
			}
			mNCellGeant->Fill(ncell,i);
		}
		Int_t ne = 0;
		Int_t nw = 0;
		for(Int_t i=0;i<2*mNVPD;i++) {
			if(mVpdHitFlag[i]) {
				mVpdGeant->Fill(i);
				if(i<mNVPD) nw++;
				else ne++;
			}
		}
		mNVpdGeant->Fill(nw,ne);
	}

	/// send off to StMcEvent
	mMcEvent = (StMcEvent*)GetInputDS("StMcEvent");
	if (!mMcEvent) {
		LOG_ERROR << "No StMcEvent! Bailing out ..." << endm;
	} else {
		mMcEvent->setBTofHitCollection(mMcBTofHitCollection);
		LOG_INFO << " ... StMcTofCollection stored in StMcEvent" << endm;
	}

	/// send off to StEvent
	if (mWriteStEvent){
	  mEvent = (StEvent*)GetInputDS("StEvent");
	  if (!mEvent) {
	    LOG_ERROR << "No StEvent! Bailing out ..." << endm;
	  } else { // mEvent non-zero

	  //Store Collections
	  mBTofCollection = mEvent->btofCollection();
	  if(!mBTofCollection) {
	    mBTofCollection = new StBTofCollection();
	    mEvent->setBTofCollection(mBTofCollection);
	  }
	  //Store Collections
	  mBTofCollection = mEvent->btofCollection();
	  if (! mBTofCollection ) {
	    mBTofCollection= new StBTofCollection();
	    mEvent->setBTofCollection(mBTofCollection);
	  }
	  /// creat StBTofHit / tofRawData / tofData collection
	  for(Int_t jj = 0; jj < (Int_t)mMcBTofHitCollection->hits().size(); jj++) {
	    StMcBTofHit *aMcBTofHit = mMcBTofHitCollection->hits()[jj];

	    if(!aMcBTofHit) continue;
	    Int_t trayid = aMcBTofHit->tray();
	    Int_t moduleid = aMcBTofHit->module();
	    Int_t cellid = aMcBTofHit->cell();

	    Int_t chn = 255;  // default
	    if(trayid>0&&trayid<=120) {
	      chn = mDaqMap->Cell2TDIGChan(moduleid,cellid);
	    } else if(trayid==121) {
	      chn = mDaqMap->WestPMT2TDIGLeChan(cellid);
	    } else if(trayid==122) {
	      chn = mDaqMap->EastPMT2TDIGLeChan(cellid);
	    }
#if 0
	    Int_t index;
	    if(trayid>0&&trayid<=120) { 
	      index = (trayid-1)*mNTOF + (moduleid-1)*mNCell + (cellid-1);
	    } else if(trayid==121||trayid==122) {
	      index = (trayid-1)*mNTOF + (cellid-1);
	    }
#endif

	    /// Efficiency
	    Float_t eff = 1.;
	    if(trayid>0&&trayid<=120) eff = mSimDb->eff_tof(trayid, moduleid, cellid);
	    else if(trayid==121||trayid==122) eff = mSimDb->eff_vpd(trayid, cellid);
	    if (gRandom->Uniform(1.0) > eff){LOG_DEBUG<<"Hit removed by inefficiency cut (at " << eff*100 << "%)"<<endm; continue; } //! inefficiency


	    Float_t mcTof=aMcBTofHit->tof()/1000.;//from picoseconds to nanoseconds
	    static  Float_t tot  = 15;
	    static  Float_t tStart = 0; // MC start time
	    Float_t tdc = mcTof;
	    Float_t corr;
	    if (aMcBTofHit->module() > 0) { // Tof
	      corr = St_tofTOffsetC::instance()->t0(aMcBTofHit->tray(),aMcBTofHit->module(),aMcBTofHit->cell());
	      if (corr < -9999.) continue;
	      tdc += corr;
	      corr = St_tofTotbCorrC::instance()->Corr(aMcBTofHit->tray(),aMcBTofHit->module(),aMcBTofHit->cell(),tot);
	      if (corr < -9999.) continue;
	      tdc += corr;
	      corr = St_tofZbCorrC::instance()->Corr(aMcBTofHit->tray(),aMcBTofHit->module(),aMcBTofHit->cell(),aMcBTofHit->position().z());
	      if (corr < -9999.) continue;
	      tdc += corr;
	    } else { // (aMcBTofHit->module() = 0 -> VPD
	      corr = St_vpdTotCorrC::instance()->Corr(aMcBTofHit->cell(),tot);
	      if (corr < -9999.) continue;
	      tdc += corr;
	    }
	    tdc += tStart;
	    //Fill the StBTofHit
	    StBTofHit aBTofHit;
	    aBTofHit.Clear();
	    aBTofHit.setHardwarePosition(kBTofId);
	    aBTofHit.setTray((Int_t)aMcBTofHit->tray());
	    aBTofHit.setModule((unsigned char)aMcBTofHit->module());
	    aBTofHit.setCell((Int_t)aMcBTofHit->cell());

	    aBTofHit.setLeadingEdgeTime(tdc);
	    aBTofHit.setTrailingEdgeTime(tdc+tot);
	    aBTofHit.setAssociatedTrack(NULL);//done in StBTofMatchMaker
	    aBTofHit.setIdTruth(aMcBTofHit->parentTrackId(), 100);
	    aBTofHit.setPathLength(aMcBTofHit->pathLength());
	    aBTofHit.setTime(aMcBTofHit->time());
	    mBTofCollection->addHit(new StBTofHit(aBTofHit));

	    //Fill the StBTofRawHit
	    StBTofRawHit aBTofRawHit;
	    aBTofRawHit.Clear();
	    aBTofRawHit.setTray((Int_t)aMcBTofHit->tray());
	    aBTofRawHit.setChannel(6*(aMcBTofHit->module() - 1) + (Int_t)aMcBTofHit->cell());
	    aBTofRawHit.setFlag(1);
	    mBTofCollection->addRawHit(new StBTofRawHit(aBTofRawHit));

	  }

	  //Fill StBTofHeader -- 	
	  StBTofHeader aHead;
	  
	  mBTofCollection->setHeader(new StBTofHeader(aHead));

	  LOG_INFO << "... StBTofCollection Stored in StEvent! " << endm;
          } // mEvent non-zero
	}

	/// check StMcEvent and StEvent
	if(Debug()) {
		LOG_DEBUG << " ==== Test McBTofHitCollection ==== " << endm;
		StSPtrVecMcBTofHit& mcBTofHits = mMcEvent->btofHitCollection()->hits();
		Int_t nCell[mNTray];
		for(Int_t i=0;i<mNTray;i++) nCell[i] = 0;
		Int_t nEast=0;
		Int_t nWest=0;
		for(Int_t i=0;i<(Int_t)mcBTofHits.size();i++) {
			LOG_DEBUG << (*mcBTofHits[i]) << endm;

			if(mBookHisto) {
				Int_t itray = mcBTofHits[i]->tray();
				Int_t imodule = mcBTofHits[i]->module();
				Int_t icell = mcBTofHits[i]->cell();
				Float_t t0 = mcBTofHits[i]->time();
				Float_t tof = mcBTofHits[i]->tof();
				Float_t de = mcBTofHits[i]->dE();


				LOG_DEBUG << "tray# "<<itray << endm;

				// fill BTOF histograms
				if(itray>0&&itray<=120) {
					mCellSeen->Fill((imodule-1)*mNCell+(icell-1),itray-1);
					mDeSeen->Fill( de / keV );
					mT0Seen->Fill( t0 /1000 );//ns
					mTofSeen->Fill( tof / 1000 );//ns
					mTofResSeen->Fill( (tof-t0) );//ps 
					nCell[itray-1]++;
				}
				// fill VPD histograms
				else if(itray==121 || itray==122) {
					mVpdSeen->Fill((itray-121)*mNVPD + (icell-1));
					mVpdResSeen->Fill( (tof-t0) / nanosecond );
					if(itray==121) nWest++;
					else nEast++;
				}
			}
		}
		if(mBookHisto) {
			for(Int_t i=0;i<mNTray;i++) mNCellSeen->Fill(nCell[i],i);
			mNVpdSeen->Fill(nWest,nEast);
		}

		LOG_INFO << " ==== Test TofRawDataCollection ==== " << endm;
		for(Int_t i=0;i<mNTray;i++) nCell[i] = 0;
		nEast=0;
		nWest=0;

		if (mWriteStEvent){
		  StSPtrVecBTofHit& bTofHits=mEvent->btofCollection()->tofHits();
		  StBTofHit* bHit;
		  for(Int_t aa=0;aa<(int)bTofHits.size();aa++){
		    bHit=bTofHits[aa];
		    Int_t itray=bHit->tray();
		    Int_t imodule=bHit->module();
		    Int_t icell=bHit->cell();
		    if(mBookHisto) {mCellReco->Fill((imodule-1)*mNCell+(icell-1),itray-1);}
		  }

		  if(mBookHisto) {
		    for(Int_t i=0;i<mNTray;i++) mNCellReco->Fill(nCell[i],i);
		    mNVpdReco->Fill(nWest,nEast);
		  }

		}
	}

	if(Debug()) cout<<"leaving fill event"<<endl;

	return kStOk;
}



//_____________________________________________________________________________
IntVec StBTofSimMaker::CalcCellId(Int_t volume_id, Float_t ylocal) 
{
	IntVec cellId;
	Int_t ires    = volume_id;

	Int_t rileft  = Int_t(ires/10/100/100);   //! west (1) or east (2)
	ires          = ires-rileft*100*100*10;
	Int_t itray   = Int_t(ires/10/100);       //! tray id in half barrel
	ires          = ires-itray*100*10;
	Int_t imodule = Int_t(ires/10);           //! module id 1-32
	itray = itray + (rileft-1)*mNTray/2;    //! tray id 1-120

	Int_t icell = Int_t((ylocal + mBTofPadWidth * mNCell/2) / mBTofPadWidth) + 1;

	if(itray<=0 || itray>mNTray) itray = -1;
	if(imodule<=0 || imodule>mNModule) imodule = -1;  
	if(icell<=0 || icell>mNCell) icell = -1;

	cellId.push_back(itray);
	cellId.push_back(imodule);
	cellId.push_back(icell);

	return cellId;
}

//_____________________________________________________________________________
Double_t StBTofSimMaker::GammaRandom()
{
	Double_t xmax,ymin,x,y,x1;
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
Int_t StBTofSimMaker::CellXtalk(Int_t icell, Float_t ylocal, Float_t& wt, Int_t& icellx)
{
	Float_t yc = (icell-1-2.5)*mBTofPadWidth;  //! y center in this pad
	Float_t dy = ylocal - yc;

	wt = 1.;
	icellx = -1;
	Float_t dyCut = mSimDb->dy_xtalk();//dyCut is by default set to 1
	if(TMath::Abs(dy)<dyCut) return kStOk;   //! no Xtalk when hit is in the cell center

	wt = 1. - (TMath::Abs(dy) - dyCut)/(mBTofPadWidth - 2.0*dyCut);

	if(dy>0) icellx = icell + 1;
	else     icellx = icell - 1;

	if(icellx>mNCell) icellx = -1;
	if(icellx<=0)    icellx = -1;

	return kStOk;
}


//_____________________________________________________________________________
Int_t StBTofSimMaker::FastCellResponse(g2t_ctf_hit_st* tofHitsFromGeant)
{
	// Simulate the single cell response for a geant hit
	if(tofHitsFromGeant->s_track<=0.0 || tofHitsFromGeant->de <=0.0) {
		LOG_WARN << " No deposit energy in this tof hit! " << endm;
		return kStWarn;
	}
	St_g2t_track *g2t_track = (St_g2t_track *) GetDataSet("geant/g2t_track"); //  if (!g2t_track)    return kStWarn;
	if (!g2t_track) {
	  LOG_WARN << " No G2T track table!" << endm;
	  return kStWarn;
	};
	St_g2t_vertex  *g2t_vertex = (St_g2t_vertex *) GetDataSet("geant/g2t_vertex");// if (!g2t_ver)      return kStWarn;
	if (!g2t_vertex) {
	  LOG_WARN << " No G2T vertex table!" << endm;
	  return kStWarn;
	};

	g2t_track_st *tof_track = g2t_track->GetTable();
	Int_t partnerTrkId         = tofHitsFromGeant->track_p;
	Double_t tofV = 0;
	if (! TimeAtVertex(partnerTrkId, g2t_track, g2t_vertex, tofV)) return kStWarn;
	//	if (TMath::Abs(tofV) > 45e-9) return kStOK; // 45 ns trigger matching window 
	if(mBookHisto) {
		mDeGeant->Fill(tofHitsFromGeant->de / keV);
		mTofGeant->Fill((tofV + tofHitsFromGeant->tof) / nanosecond);
	}

	IntVec cellId   = CalcCellId(tofHitsFromGeant->volume_id, tofHitsFromGeant->x[1]);

	Int_t icell, imodule, itray;
	itray   = cellId[0];
	imodule = cellId[1];
	icell   = cellId[2];
	if (itray==-1 || imodule==-1 || icell==-1) {
		LOG_WARN << " Not hit the sensitive MRPC volume !!! " << endm;
		return kStWarn;
	}
	if (St_tofStatusC::instance()->status(itray,imodule,icell) != 1) return kStOK;
	StThreeVectorF local(tofHitsFromGeant->x[0], tofHitsFromGeant->x[1], tofHitsFromGeant->x[2]);

	StMcTrack *partnerTrk =  new StMcTrack(&(tof_track[partnerTrkId-1]));

	/// X-talk
	Int_t icellx = -1;
	Float_t wt = 1.0;
	if(mCellXtalk)   CellXtalk(icell, local.y(), wt, icellx);

	Double_t tof= (tofV + tofHitsFromGeant->tof)*1000./nanosecond + ranGauss.shoot()*mSimDb->timeres_tof()*1000./nanosecond;    //! 85ps per channel
	Double_t t0 = (tofV + tofHitsFromGeant->tof)*1000./nanosecond;
	Double_t de = tofHitsFromGeant->de * wt;
	Double_t pathL = tofHitsFromGeant->s_track;
	Double_t q = 0.;

	StMcBTofHit *mcBTofHit = new StMcBTofHit(itray,imodule,icell,de,pathL,t0,tof,q);
	mcBTofHit->setPosition(local);
	mcBTofHit->setParentTrack(partnerTrk);
	storeMcBTofHit(mcBTofHit);

	mTofHitFlag[itray-1][(imodule-1)*mNCell+(icell-1)] = 1;

	if(icellx <= 0 || icellx > mNCell) return kStOk;  //! no X-talk
	///
	/// X talk signal
	///
	Double_t tofx = (tofV + tofHitsFromGeant->tof) + ranGauss.shoot()*mSimDb->timeres_tof();    //! 85ps per channel
	Double_t dex = tofHitsFromGeant->de * (1. - wt);
	Double_t qx = 0.*(1.-wt);

	StMcBTofHit *mcBTofHitx = new StMcBTofHit(itray,imodule,icellx,dex,pathL,t0,tofx,qx);
	mcBTofHitx->setPosition(local);
	mcBTofHitx->setParentTrack(partnerTrk);
	mcBTofHitx->setParentTrackId(partnerTrkId);
	storeMcBTofHit(mcBTofHitx);

	mTofHitFlag[itray-1][(imodule-1)*mNCell+(icellx-1)] = 1;

	return kStOk;
}

//_____________________________________________________________________________
Int_t StBTofSimMaker::storeMcBTofHit(StMcBTofHit* mcBTofHit)
{
	//this function adds a hit to a previous hit (if they mactch the same cell location), 
	// or it stores the new hit (the last part below)
	Bool_t hitFound = kFALSE;

	//this is primarily for VPD hits 
	for(size_t j=0;j<mMcBTofHitCollection->hits().size();j++) { 
		StMcBTofHit *tempHit = mMcBTofHitCollection->hits()[j];
		if(!tempHit) continue;
		if(mcBTofHit->sameCell(*tempHit)) {
			hitFound = kTRUE;
			Float_t t1 = mcBTofHit->time(); 
			Float_t t2 = tempHit->time();   
			Float_t tof1 = mcBTofHit->tof();
			Float_t dE1 = mcBTofHit->dE();
			Float_t dE2 = tempHit->dE();
			Float_t s1 = mcBTofHit->pathLength();
			Float_t q1 = mcBTofHit->charge();
			Float_t q2 = tempHit->charge();
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
Int_t StBTofSimMaker::fillRaw(){
	//not currently used
	//fill the adc and tdc entries.
	return kStOk;

}

//_____________________________________________________________________________
/// simulate electronic noise (empty)
Int_t StBTofSimMaker::electronicNoise(){
	return kStOk;
}

//_____________________________________________________________________________
void  StBTofSimMaker::setHistFileName(string s){
	mHistFile=s;
	return;
}
//_____________________________________________________________________________
Int_t StBTofSimMaker::bookHistograms()
{
	//only done if Histogram setting is turned on
	mBetaHist=new TH1F("mBetaHist","mBetaHist", 100, -2, 2);
	mPathLHist=new TH1F("mPathLHist","mPathLHist", 100, -2, 500);//cm's
	mTofHist=new TH1F("mTofHist","mTofHist", 1000, -10, 10000);
	mRecMass=new TH1F("mRecMass","mRecMass", 1000, -2, 4);

	mCellGeant  = new TH2F("CellGeant","CellGeant",192,0.,192.,120,1.,120.);
	mVpdGeant   = new TH1F("VpdGeant","VpdGeant",38,0.,38.);
	mNCellGeant = new TH2F("NCellGeant","NCellGeant",192,0.,192.,120,1.,120.);   
	mNVpdGeant  = new TH2F("NVpdGeant","NVpdGeant",19,0.,19.,19,0.,19.);
	mDeGeant    = new TH1F("DeGeant","DeGeant",1000,0.,10.);      //! 10 keV
	mTofGeant   = new TH1F("TofGeant","TofGeant",1000,0.,20.);    //! 20 ns

	mCellSeen   = new TH2F("CellSeen","CellSeen",192,0.,192.,120,1.,120.);
	mVpdSeen    = new TH1F("VpdSeen","VpdSeen",38,0.,38.);
	mNCellSeen  = new TH2F("NCellSeen","NCellSeen",192,0.,192.,120,1.,120.);
	mNVpdSeen   = new TH2F("NVpdSeen","NVpdSeen",19,0.,19.,19,0.,19.);
	mDeSeen     = new TH1F("DeSeen","DeSeen",1000,0.,10.);        //! 10 kev
	mT0Seen    = new TH1F("T0Seen","T0Seen",1000,0.,20.);      //! ns
	mTofSeen    = new TH1F("TofSeen","TofSeen",1000,0.,20.);      //! 20 ns

	mTofResSeen = new TH1F("TofResSeen","TofResSeen",1001,-500.,500.);//! ps
	mVpdResSeen = new TH1F("VpdResSeen","VpdResSeen",1001,-500.,500.); //! ps

	mCellReco   = new TH2F("CellReco","CellReco",192,0.,192.,120,1.,120.);
	mVpdReco    = new TH1F("VpdReco","VpdReco",38,0.,38.);
	mNCellReco  = new TH2F("NCellReco","NCellReco",192,0.,192.,120,1.,120.);
	mNVpdReco   = new TH2F("NVpdReco","NVpdReco",19,0.,19.,19,0.,19.);
	mADCReco    = new TH1F("ADCReco","ADCReco",4096,0.,4096.);
	mTDCReco    = new TH1F("TDCReco","TDCReco",4096,0.,4096.);
	mT0Reco   = new TH1F("T0Reco","T0Reco",1000,0.,20.);      //! ns
	mTofResReco = new TH1F("TofResReco","TofResReco",1000,-300.,300.);//ps
	mVpdResReco = new TH1F("VpdResReco","VpdResReco",1000,-100.,100.);
	mTACorr     = new TH2F("TACorr","TACorr",512,0.,4096.,512,0.,4096.);

	mModHist     = new TH1F("ModuleHist","ModuleHist",201,-100,100);
	return kStOk;

}

//_____________________________________________________________________________
Int_t StBTofSimMaker::writeHistograms()
{
	//only done if Histogram setting is turned on

	mBetaHist->Write();
	mPathLHist->Write();
	mTofHist->Write();
	mRecMass->Write();

	mCellGeant->Write();
	mVpdGeant->Write();
	mNCellGeant->Write();
	mNVpdGeant->Write();
	mDeGeant->Write();   
	mTofGeant->Write();

	mCellSeen->Write();
	mVpdSeen->Write();
	mNCellSeen->Write();
	mNVpdSeen->Write();
	mDeSeen->Write();
	mT0Seen->Write();
	mTofSeen->Write();
	mTofResSeen->Write();
	mVpdResSeen->Write();

	mCellReco->Write();
	mVpdReco->Write();
	mNCellReco->Write();
	mNVpdReco->Write();
	mADCReco->Write();
	mTDCReco->Write();
	mT0Reco->Write();
	mTofResReco->Write();
	mVpdResReco->Write();
	mTACorr->Write();

	mModHist->Write();
	return kStOk;
}

