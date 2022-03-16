/***************************************************************************
 *
 * $Id: StHltMaker.cxx,v 1.2 2012/12/12 22:10:35 fisyak Exp $
 *
 * Author: L. Xue, H. Qiu, A. Tang, Jan 2011
 ***************************************************************************
 *
 * Description: Hlt Maker to propaganda online Hlt tracking and selection 
 *              information.
 *
 ***************************************************************************
 *
 * $Log: StHltMaker.cxx,v $
 * Revision 1.2  2012/12/12 22:10:35  fisyak
 * add sys/types.h include for APPLE
 *
 * Revision 1.1  2011/02/01 18:24:02  xueliang
 * *** empty log message ***
 *
 **************************************************************************/

#ifdef __APPLE__
#include <sys/types.h>
#endif


#include <iostream>
#include <fstream>
#include <stdlib.h>

#include <Stiostream.h>
#include "StHltMaker.h"
#include "StRtsTable.h"
#include "HLTFormats.h"
#include "RTS/src/DAQ_HLT/hlt_gl3.h"
#include "StThreeVectorF.hh"

#include "StEvent/StEvent.h"
#include "StEvent/StHltEvent.h"
#include "StEvent/StHltTrackNode.h"
#include "StEvent/StHltTrack.h"
#include "StEvent/StHltBTofHit.h"
#include "StEvent/StHltVpdHit.h"
#include "StEvent/StHltBEmcTowerHit.h"
#include "StEvent/StHltTriggerReason.h"
#include "StEvent/StHltHighPt.h"
#include "StEvent/StHltHeavyFragment.h"
#include "StEvent/StHltDiElectron.h"

using namespace std;
using namespace star;
using namespace star::rts;
using namespace star::rts::hlt;

//__________________________________________________________________________________________
namespace {
#define HTLPAIRATTR(bank) pair<string, EHLTBANKS>(_QUOTE_(HLT_##bank),_NAME2_(kHLT_,bank))
#define SELECTBANK(bank)    case _NAME2_(kHLT_,bank): { processBank((_NAME2_(HLT_,bank) *)(&(hlt.data))); break;}
		class HltBanks {
			map<string, EHLTBANKS> fHltBanks;
			public:
			HltBanks() {
				fHltBanks.insert(HTLPAIRATTR(EVE));
				fHltBanks.insert(HTLPAIRATTR(TOF));
				fHltBanks.insert(HTLPAIRATTR(PVPD));
				fHltBanks.insert(HTLPAIRATTR(EMC));
				fHltBanks.insert(HTLPAIRATTR(GT));
				fHltBanks.insert(HTLPAIRATTR(PT));
				fHltBanks.insert(HTLPAIRATTR(NODE));
				fHltBanks.insert(HTLPAIRATTR(HIPT));
				fHltBanks.insert(HTLPAIRATTR(DIEP));
				fHltBanks.insert(HTLPAIRATTR(HF));
				fHltBanks.insert(HTLPAIRATTR(NONE));
			}
			EHLTBANKS BankId(const char *bankName) {
				map<string,EHLTBANKS>::iterator it= 
					fHltBanks.find(string(bankName));
				return (it == fHltBanks.end()) ? kHLT_NONE : it->second;
			}
		};
		HltBanks gHltBanks;
		EHLTBANKS BankId(const char *bankName) {  
			EHLTBANKS j= gHltBanks.BankId(bankName) ; 
			return j;
		}
}

ClassImp(StHltMaker)

//__________________________________________________________________________________________
///< default constructor 
StHltMaker::StHltMaker(const char *name) : StRTSBaseMaker("hlt",name)
{

	mStEvent = 0;
	mStHltEvent = 0;

	mNumHighPt = 0;
	mNumHeavyFragment = 0;
	mNumDielectron = 0;

	SetAttr("hltMode",1);   // 1 is default value for full model production

	for(u_int i=0; i<NMax; i++){
		mHighPtNodeSN[i] = -1;
		mHeavyFragmentNodeSN[i] = -1;
		mDaughter1NodeSN[i] = -1;
		mDaughter2NodeSN[i] = -1;
	}

	LOG_DEBUG << "StHltMaker::ctor"  << endm;

}

//__________________________________________________________________________________________
StHltMaker::~StHltMaker() 
///< default destructor
{
}

//__________________________________________________________________________________________
///< Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StHltMaker::Init()
{
	return StMaker::Init();
}

//__________________________________________________________________________________________
Int_t StHltMaker::InitRun(int runumber)
{
	return StMaker::Init();
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! 
  \return the DAQ data for the bank \c "gl3" if exists
  \sa \htmlonly 
  <a href="http://docs.google.com/Doc?docid=dgv8pf9t_60dwhg3zd4&hl=en">"A DAQ_READER Cookbook"</a> 
  \endhtmlonly
 */
//_____________________________________________________________
StRtsTable *StHltMaker::GetNextGl3()
{
	///< Get "gl3" DAQ data assuming the maker name 
	///< matches the "detector name"
	return GetNext("gl3");
}

//_____________________________________________________________________________
///< Clear - this method is called in loop for prepare the maker for the next event
void  StHltMaker::Clear(Option_t *) 
{

	StMaker::Clear(); ///< perform the basic clear (mandatory)

}

//_____________________________________________________________________________
///< GetHltEvent This method is to obtain the HltEvent from StEvent.
StHltEvent *StHltMaker::GetHltEvent()
{
	// Get StHltEvent
	StHltEvent *HltEvent = 0;
	mStEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));

	if (mStEvent) {
		HltEvent = mStEvent->hltEvent();

		if ( !HltEvent )  {
			LOG_DEBUG << "Create StHltEvent !" << endm;

			HltEvent = new StHltEvent();       ///<  to create the StHltEvent
			mStEvent->setHltEvent(HltEvent);   ///<  Save the StHltEvent to StEvent
		}
	}
	else {LOG_WARN << "No StEvent found" << endm; }

	return HltEvent;
}

//_____________________________________________________________________________
///< Make - this method is called in loop for each event
Int_t StHltMaker::Make()
{

	if ( Token() == 0 )  return kStOK ; ///< if Token == 0, this is not a real event.
	mStHltEvent = GetHltEvent();
	if(!mStHltEvent)  {
		LOG_WARN << "No hlt event found in this data !" << endm;
		return kStOK;
	}

	mNumHighPt = 0;
	mNumHeavyFragment = 0;
	mNumDielectron = 0;
	///< Initialize the series number of triggered particles
	for(u_int i=0; i<NMax; i++){
		mHighPtNodeSN[i] = -1;
		mHeavyFragmentNodeSN[i] = -1;
		mDaughter1NodeSN[i] = -1;
		mDaughter2NodeSN[i] = -1;
	}

	int counter = 0; 
	///< Begin to loop hlt banks in daq
	while ( GetNextGl3() ) {
		counter++;
		TGenericTable::iterator iword = DaqDta()->begin();
		hlt_gl3_t &hlt = *(hlt_gl3_t*)*iword;

		switch (BankId(hlt.name)) {
			SELECTBANK(EVE)
				SELECTBANK(TOF)
				SELECTBANK(PVPD)
				SELECTBANK(EMC)
				SELECTBANK(GT)
				SELECTBANK(PT)
				SELECTBANK(NODE)
				SELECTBANK(HIPT)
				SELECTBANK(DIEP)
				SELECTBANK(HF)
			default: case kHLT_NONE:{
										LOG_ERROR << "Unknown bank name was  delivered: <" << hlt.name << ">" << endm;
										return kStERR; break;
									}
		}

	}

	fillNodePointer(mStHltEvent);     ///< create pointers in track node
	fillHighPt(mStHltEvent);          ///< fill high pt information 
	fillHeavyFragment(mStHltEvent);   ///< fill heavy fragment information
	fillDielectron(mStHltEvent);      ///< fill di-electron information
	fillTriggerReason(mStHltEvent);   ///< fill trigger reason



	///<   clear the tracks, tracknode, bemc, btof, vpd information in "triggered tracks only" model
	Int_t hltMode = IAttr("hltMode");

	if(!hltMode){ 

		StSPtrVecHltTrack& VecgTrack = mStHltEvent->globalTrack();
		VecgTrack.resize(0);
		StSPtrVecHltTrack& VecpTrack = mStHltEvent->primaryTrack();
		VecpTrack.resize(0);
		StSPtrVecHltTrackNode& VecTrackNode = mStHltEvent->trackNode();
		VecTrackNode.resize(0);
		StSPtrVecHltBTofHit& VecbTofHit = mStHltEvent->bTofHit();
		VecbTofHit.resize(0);
		StSPtrVecHltBEmcTowerHit& VecbEmcHit = mStHltEvent->bEmcTowerHits();
		VecbEmcHit.resize(0);
		StSPtrVecHltVpdHit& VecpVpdHit = mStHltEvent->vpdHit();
		VecpVpdHit.resize(0);

		LOG_INFO << "Clear The Vector Of HLT Tracks, TrackNodes, BTofHit, BEmcTowerHit, VpdHit in <<Triggered Tracks Only>> model " << endm;

	}

	if (counter <= 0) {
		LOG_DEBUG << "There was no hlt data for this event. " << endm;
	} else {
		LOG_DEBUG << "End of hlt data for this event : " << GetEventNumber() << ", Total = "  << counter+1 
			<< " records were found" << endm;
	}
	return kStOK;

}

//_____________________________________________________________________________
///< Fill StHltEvent at event level
void StHltMaker::processBank(const HLT_EVE  *hlt_eve)
{

	LOG_DEBUG << "Begin to fill HLT_EVE bank " << endm;

	mStHltEvent->setVersion((unsigned int) hlt_eve->version) ;
	///< version is a date from which on HLTFormat.h changed 
	mStHltEvent->setTriggerReasonBitOred((unsigned int) hlt_eve->hltDecision) ;
	///< StHltTriggerReason::reasonBit with "OR" operated.

	mStHltEvent->setT0(hlt_eve->T0) ;  ///< vpd start time
	mStHltEvent->setVpdVertexZ(hlt_eve->vpdVertexZ);

	if( hlt_eve->version >= 0x20100216){  ///< 20100216 is the start day when begin to save dE/dx gain in daq
		mStHltEvent->setInnerSecGain(hlt_eve->innerSectorGain) ;
		mStHltEvent->setOuterSecGain(hlt_eve->outerSectorGain) ;
	} else {
		mStHltEvent->setInnerSecGain(-999.) ;
		mStHltEvent->setOuterSecGain(-999.) ;
	}

	float vertX = hlt_eve->vertexX ;
	float vertY = hlt_eve->vertexY ;
	float vertZ = hlt_eve->vertexZ ;
	StThreeVectorF vertex(vertX,vertY,vertZ);
	mStHltEvent->setVertex(vertex) ;

	float lmvertX = hlt_eve->lmVertexX ;
	float lmvertY = hlt_eve->lmVertexY ;
	float lmvertZ = hlt_eve->lmVertexZ ;
	StThreeVectorF lmvertex(lmvertX,lmvertY,lmvertZ);
	mStHltEvent->setLowMultVertex(lmvertex) ;

	LOG_DEBUG << "Finish to fill HLT_EVE bank " << endm;

}

//_____________________________________________________________________________
///< Fill StHltBTofHit
void StHltMaker::processBank(const HLT_TOF  *hlt_tof)
{

	LOG_DEBUG << "Begin to fill HLT_TOF bank " << endm;

	for(u_int i=0 ; i < hlt_tof->nTofHits ; i++){

		// create StHltBTofHit 
		StHltBTofHit *aHit = new StHltBTofHit();

		aHit->setTrayId(hlt_tof->tofHit[i].trayId);
		aHit->setChannel(hlt_tof->tofHit[i].channel);
		aHit->setTdc(hlt_tof->tofHit[i].tdc);
		aHit->setTot(hlt_tof->tofHit[i].tot);
		aHit->setTof(hlt_tof->tofHit[i].tof); 
		aHit->setTriggerTime(hlt_tof->tofHit[i].triggertime);

		// save pointer to the vector
		mStHltEvent->addBTofHit(aHit);

	}

	LOG_DEBUG << "Finish to fill HLT_TOF bank " << endm;

}

//_____________________________________________________________________________
///< Fill StHltPvpdHit
void StHltMaker::processBank(const HLT_PVPD *hlt_pvpd)
{

	LOG_DEBUG << "Begin to fill HLT_PVPD bank " << endm;

	for(u_int i=0 ; i < hlt_pvpd->nPvpdHits ; i++){

		// create StHltVpdHit
		StHltVpdHit *aHit = new StHltVpdHit();

		int trayID = hlt_pvpd->pvpdHit[i].trayId;
		if(trayID==120) aHit->setDirection(west);
		else aHit->setDirection(east);
		aHit->setChannel(hlt_pvpd->pvpdHit[i].channel);
		aHit->setTdc(hlt_pvpd->pvpdHit[i].tdc);
		aHit->setTot(hlt_pvpd->pvpdHit[i].tot);
		aHit->setTof(hlt_pvpd->pvpdHit[i].tof);
		aHit->setTriggerTime(hlt_pvpd->pvpdHit[i].triggertime);

		// save pointer to a vector
		mStHltEvent->addVpdHit(aHit);

	}

	LOG_DEBUG << "Finish to fill HLT_PVPD bank " << endm;

}

//_____________________________________________________________________________
///< Fill StHltBEmcTowerHit
void StHltMaker::processBank(const HLT_EMC  *hlt_emc)
{

	LOG_DEBUG << "Begin to fill HLT_EMC bank " << endm;

	for(u_int i=0 ; i< hlt_emc->nEmcTowers ; i++){

		// create StHltBEmcTowerHit
		StHltBEmcTowerHit *aHit = new StHltBEmcTowerHit();

		aHit->setAdc(hlt_emc->emcTower[i].adc);
		aHit->setEnergy(hlt_emc->emcTower[i].energy);
		aHit->setPhi(hlt_emc->emcTower[i].phi);
		aHit->setEta(hlt_emc->emcTower[i].eta);
		aHit->setZ(hlt_emc->emcTower[i].z); 
		aHit->setSoftId(hlt_emc->emcTower[i].softId);
		aHit->setDaqId(hlt_emc->emcTower[i].daqId);

		// save pointer to a vector
		mStHltEvent->addBEmcTowerHit(aHit);

	}

	LOG_DEBUG << "Finish to fill HLT_EMC bank " << endm;

}

//_____________________________________________________________________________
///< Fill StHltTrack  "global"
void StHltMaker::processBank(const HLT_GT   *hlt_gt)
{

	LOG_DEBUG << "Begin to fill HLT_GT bank " << endm;

	for(u_int i=0 ; i < hlt_gt->nGlobalTracks; i++){

		// create StHltTrack
		StHltTrack *aTrack = new StHltTrack();

		aTrack->setType(global);
		aTrack->setId(hlt_gt->globalTrack[i].id);
		aTrack->setFlag(hlt_gt->globalTrack[i].flag);
		aTrack->setInnerMostRow(hlt_gt->globalTrack[i].innerMostRow);
		aTrack->setOuterMostRow(hlt_gt->globalTrack[i].outerMostRow);
		aTrack->setNHits(hlt_gt->globalTrack[i].nHits);
		aTrack->setNDedx(hlt_gt->globalTrack[i].ndedx);
		aTrack->setQ(hlt_gt->globalTrack[i].q);
		aTrack->setChi2(0,hlt_gt->globalTrack[i].chi2[0]);
		aTrack->setChi2(1,hlt_gt->globalTrack[i].chi2[1]);
		aTrack->setDedx(hlt_gt->globalTrack[i].dedx);
		aTrack->setPt(hlt_gt->globalTrack[i].pt);
		aTrack->setPhi0(hlt_gt->globalTrack[i].phi0);
		aTrack->setPsi(hlt_gt->globalTrack[i].psi);
		aTrack->setR0(hlt_gt->globalTrack[i].r0);
		aTrack->setTanl(hlt_gt->globalTrack[i].tanl);
		aTrack->setZ0(hlt_gt->globalTrack[i].z0);
		aTrack->setLength(hlt_gt->globalTrack[i].length);
		aTrack->setDpt(hlt_gt->globalTrack[i].dpt);
		aTrack->setDpsi(hlt_gt->globalTrack[i].dpsi);
		aTrack->setDz0(hlt_gt->globalTrack[i].dz0);
		aTrack->setDtanl(hlt_gt->globalTrack[i].dtanl);

		// save pointer to a vector
		mStHltEvent->addGlobalTrack(aTrack);

	}


	LOG_DEBUG << "Finish to fill HLT_GT bank " << endm;

}

//_____________________________________________________________________________
///< Fill StHltTrack  "primary"
void StHltMaker::processBank(const HLT_PT   *hlt_pt)
{

	LOG_DEBUG << "Begin to fill HLT_PT bank " << endm;

	for(u_int i=0;i<hlt_pt->nPrimaryTracks;i++){

		// create StHltTrack
		StHltTrack *aTrack = new StHltTrack();

		aTrack->setType(primary);
		aTrack->setId(hlt_pt->primaryTrack[i].id);
		aTrack->setFlag(hlt_pt->primaryTrack[i].flag);
		aTrack->setInnerMostRow(hlt_pt->primaryTrack[i].innerMostRow);
		aTrack->setOuterMostRow(hlt_pt->primaryTrack[i].outerMostRow);
		aTrack->setNHits(hlt_pt->primaryTrack[i].nHits);
		aTrack->setNDedx(hlt_pt->primaryTrack[i].ndedx);
		aTrack->setQ(hlt_pt->primaryTrack[i].q) ;
		aTrack->setChi2(0,hlt_pt->primaryTrack[i].chi2[0]);
		aTrack->setChi2(1,hlt_pt->primaryTrack[i].chi2[1]);
		aTrack->setDedx(hlt_pt->primaryTrack[i].dedx);
		aTrack->setPt(hlt_pt->primaryTrack[i].pt);
		aTrack->setPhi0(hlt_pt->primaryTrack[i].phi0);
		aTrack->setPsi(hlt_pt->primaryTrack[i].psi);
		aTrack->setR0(hlt_pt->primaryTrack[i].r0);
		aTrack->setTanl(hlt_pt->primaryTrack[i].tanl);
		aTrack->setZ0(hlt_pt->primaryTrack[i].z0);
		aTrack->setLength(hlt_pt->primaryTrack[i].length);
		aTrack->setDpt(hlt_pt->primaryTrack[i].dpt);
		aTrack->setDpsi(hlt_pt->primaryTrack[i].dpsi);
		aTrack->setDz0(hlt_pt->primaryTrack[i].dz0);
		aTrack->setDtanl(hlt_pt->primaryTrack[i].dtanl);

		// save pointer to a vector
		mStHltEvent->addPrimaryTrack(aTrack);
	}


	LOG_DEBUG << "Finish to fill HLT_PT bank " << endm;

}

//_____________________________________________________________________________
///< Fill StHltTrackNode
void StHltMaker::processBank(const HLT_NODE *hlt_node)
{
	LOG_DEBUG << "Begin to fill HLT_NODE bank " << endm;

	for(u_int i=0 ; i< hlt_node->nNodes ; i++){

		// create StHltTrackNode
		StHltTrackNode *aNode = new StHltTrackNode();

		aNode->setBEmcMatchPhiDiff(hlt_node->node[i].emcMatchPhiDiff);
		aNode->setBEmcMatchZEdge(hlt_node->node[i].emcMatchZEdge);
		aNode->setBTofProjChannel(hlt_node->node[i].projChannel);
		aNode->setBTofCellLocalY(hlt_node->node[i].localY);
		aNode->setBTofCellLocalZ(hlt_node->node[i].localZ);
		aNode->setBTofPathLength(hlt_node->node[i].pathlength);
		aNode->setBeta(hlt_node->node[i].beta);
		aNode->setTof(hlt_node->node[i].tof);

		aNode->setGlobalTrackSN(hlt_node->node[i].globalTrackSN);
		aNode->setPrimaryTrackSN(hlt_node->node[i].primaryTrackSN);
		aNode->setTofHitSN(hlt_node->node[i].tofHitSN);
		aNode->setEmcTowerSN(hlt_node->node[i].emcTowerSN);

		// save pointer to a vector
		mStHltEvent->addTrackNode(aNode);

	}

	LOG_DEBUG << "Finish to fill HLT_NODE bank " << endm;

}

//_____________________________________________________________________________
///< Fill StHltHighPt
void StHltMaker::processBank(const HLT_HIPT *hlt_hipt)
{

	LOG_DEBUG << "Begin to fill HLT_HTPT bank " << endm;

	mNumHighPt = hlt_hipt->nHighPt;
	for(u_int i=0; i<hlt_hipt->nHighPt; i++){
		mHighPtNodeSN[i] = hlt_hipt->highPtNodeSN[i];
	}

	LOG_DEBUG << "Finish to fill HLT_HTPT bank " << endm;

}

//_____________________________________________________________________________
///< Fill StHltDielectron
void StHltMaker::processBank(const HLT_DIEP *hlt_diep)
{

	LOG_DEBUG << "Begin to fill HLT_DIEP bank " << endm;

	mNumDielectron = hlt_diep->nEPairs;
	for(u_int i=0; i<hlt_diep->nEPairs; i++){

		// create StHltDiElectron
		StHltDiElectron *aDielectron = new StHltDiElectron();

		mDaughter1NodeSN[i] = hlt_diep->ePair[i].dau1NodeSN;
		mDaughter2NodeSN[i] = hlt_diep->ePair[i].dau2NodeSN;
		aDielectron->setPt(hlt_diep->ePair[i].pt);
		aDielectron->setPsi(hlt_diep->ePair[i].psi);
		aDielectron->setTanl(hlt_diep->ePair[i].tanl);
		aDielectron->setInvariantMass(hlt_diep->ePair[i].invariantMass);

		aDielectron->setDaughter1SelectionBit(hlt_diep->ePair[i].dau1SelectionBit);
		aDielectron->setDaughter2SelectionBit(hlt_diep->ePair[i].dau2SelectionBit);

		// save pointer to a vector
		mStHltEvent->addDiElectron(aDielectron);

	}

	LOG_DEBUG << "Finish to fill HLT_DIEP bank " << endm;

}

//_____________________________________________________________________________
///< Fill StHltHeavyFragment
void StHltMaker::processBank(const HLT_HF   *hlt_hf)
{

	LOG_DEBUG << "Begin to fill HLT_HF bank " << endm;

	mNumHeavyFragment = hlt_hf->nHeavyFragments;
	for(u_int i=0 ; i < hlt_hf->nHeavyFragments ; i++){
		mHeavyFragmentNodeSN[i] = hlt_hf->heavyFragmentSN[i];
	}

	LOG_DEBUG << "Finish to fill HLT_HF bank " << endm;

}

//_____________________________________________________________________________
///< Fill pointers in node
void StHltMaker::fillNodePointer(StHltEvent* event)
{

	LOG_DEBUG << "Begin to create pointers in tracknode" << endm;

	StSPtrVecHltTrackNode& VecTrackNode = event->trackNode();
	StSPtrVecHltTrack& VecgTrack = event->globalTrack();
	StSPtrVecHltTrack& VecpTrack = event->primaryTrack();
	StSPtrVecHltBTofHit& VecbTofHit = event->bTofHit();
	StSPtrVecHltBEmcTowerHit& VecbEmcHit = event->bEmcTowerHits();

	// create pointer in StHltTrackNode 
	for(u_int i=0 ; i<VecTrackNode.size() ; i++){

		StHltTrackNode *trackNode = (StHltTrackNode*)VecTrackNode.at(i);
		int globalTrackSN = trackNode->globalTrackSN();
		int primaryTrackSN = trackNode->primaryTrackSN();
		int tofHitSN = trackNode->tofHitSN();
		int emcTowerSN = trackNode->emcTowerSN();

		StHltTrack *gTrack = 0;
		StHltTrack *pTrack = 0;
		StHltBTofHit *TofHit = 0;
		StHltBEmcTowerHit *BEmcHit = 0;
		if(globalTrackSN >= 0) gTrack = (StHltTrack*)VecgTrack.at(globalTrackSN);
		if(primaryTrackSN >= 0) pTrack = (StHltTrack*)VecpTrack.at(primaryTrackSN);
		if(tofHitSN >= 0) TofHit = (StHltBTofHit*)VecbTofHit.at(tofHitSN);
		if(emcTowerSN >= 0) BEmcHit = (StHltBEmcTowerHit*)VecbEmcHit.at(emcTowerSN);

		trackNode->setGlobalTrack(gTrack);
		trackNode->setPrimaryTrack(pTrack);
		trackNode->setBTofHit(TofHit);
		trackNode->setBEmcTowerHit(BEmcHit);

		if(globalTrackSN >= 0) gTrack->setTrackNode(trackNode);
		if(primaryTrackSN >= 0) pTrack->setTrackNode(trackNode);
		if(tofHitSN >= 0) TofHit->setTrackNode(trackNode);
		if(emcTowerSN >= 0) BEmcHit->setTrackNode(trackNode);

	}

	LOG_DEBUG << "Finish to create pointers in tracknode" << endm;

}

//_____________________________________________________________________________
///< Fill HighPt
void StHltMaker::fillHighPt(StHltEvent* event)
{

	LOG_DEBUG << "Begin to fill high pt to StHltEvent" << endm;

	StSPtrVecHltTrackNode& VecTrackNode = event->trackNode();

	for(u_int i=0 ; i<mNumHighPt ; i++){

		// create StHltHighPt
		StHltHighPt *highPt = new StHltHighPt();
		StHltTrackNode* node = (StHltTrackNode*)VecTrackNode.at(mHighPtNodeSN[i]);

		StHltTrack globalTrack ;
		if(node->globalTrack()) globalTrack = *node->globalTrack();
		StHltTrack primaryTrack ;
		if(node->primaryTrack()) primaryTrack = *node->primaryTrack();
		StHltBTofHit bTofHit ;
		if(node->bTofHit()) bTofHit = *node->bTofHit();
		StHltBEmcTowerHit bEmcHit ;
		if(node->bEmcTowerHit()) bEmcHit = *node->bEmcTowerHit();

		highPt->setGlobalTrack(globalTrack);
		highPt->setPrimaryTrack(primaryTrack);
		highPt->setBTofHit(bTofHit);
		highPt->setBEmcTowerHit(bEmcHit);
		highPt->setGlobalTrackSN(node->globalTrackSN());
		highPt->setPrimaryTrackSN(node->primaryTrackSN());
		highPt->setTofHitSN(node->tofHitSN());
		highPt->setEmcTowerSN(node->emcTowerSN());
		highPt->setBEmcMatchPhiDiff(node->bEmcMatchPhiDiff());
		highPt->setBEmcMatchZEdge(node->bEmcMatchZEdge());
		highPt->setBTofProjChannel(node->bTofProjChannel());
		highPt->setBTofCellLocalY(node->bTofCellLocalY());
		highPt->setBTofCellLocalZ(node->bTofCellLocalZ());
		highPt->setBTofPathLength(node->bTofPathLength());
		highPt->setBeta(node->beta());
		highPt->setTof(node->tof());

		// save pointer to a vector
		mStHltEvent->addHighPt(highPt);

	}

	LOG_DEBUG << "Finish to fill high pt to StHltEvent" << endm;

}


//_____________________________________________________________________________
///< Fill HeavyFragment
void StHltMaker::fillHeavyFragment(StHltEvent* event)
{

	LOG_DEBUG << "Begin to fill heavy fragment to StHltEvent" << endm;

	StSPtrVecHltTrackNode& VecTrackNode = event->trackNode();

	for(u_int i=0 ; i<mNumHeavyFragment ; i++){

		// create StHltHeavyFragment
		StHltHeavyFragment *heavyFragment = new StHltHeavyFragment();
		StHltTrackNode* node = (StHltTrackNode*)VecTrackNode.at(mHeavyFragmentNodeSN[i]);

		StHltTrack globalTrack ; 
		if(node->globalTrack()) globalTrack = *node->globalTrack();
		StHltTrack primaryTrack ;
		if(node->primaryTrack()) primaryTrack = *node->primaryTrack();
		StHltBTofHit bTofHit ;
		if(node->bTofHit()) bTofHit = *node->bTofHit();
		StHltBEmcTowerHit bEmcHit;
		if(node->bEmcTowerHit()) bEmcHit = *node->bEmcTowerHit();

		heavyFragment->setGlobalTrack(globalTrack);
		heavyFragment->setPrimaryTrack(primaryTrack);
		heavyFragment->setBTofHit(bTofHit);
		heavyFragment->setBEmcTowerHit(bEmcHit);
		heavyFragment->setGlobalTrackSN(node->globalTrackSN());
		heavyFragment->setPrimaryTrackSN(node->primaryTrackSN());
		heavyFragment->setTofHitSN(node->tofHitSN());
		heavyFragment->setEmcTowerSN(node->emcTowerSN());
		heavyFragment->setBEmcMatchPhiDiff(node->bEmcMatchPhiDiff());
		heavyFragment->setBEmcMatchZEdge(node->bEmcMatchZEdge());
		heavyFragment->setBTofProjChannel(node->bTofProjChannel());
		heavyFragment->setBTofCellLocalY(node->bTofCellLocalY());
		heavyFragment->setBTofCellLocalZ(node->bTofCellLocalZ());
		heavyFragment->setBTofPathLength(node->bTofPathLength());
		heavyFragment->setBeta(node->beta());
		heavyFragment->setTof(node->tof());

		// save pointer to a vector
		mStHltEvent->addHeavyFragment(heavyFragment);
	}

	LOG_DEBUG << "Finish to fill heavy fragment to StHltEvent" << endm;

}

//_____________________________________________________________________________
///< Fill Dielectron
void StHltMaker::fillDielectron(StHltEvent* event)
{

	LOG_DEBUG << "Begin to fill di-electron to StHltEvent" << endm;

	StSPtrVecHltTrackNode& VecTrackNode = event->trackNode();
	StSPtrVecHltDiElectron& VecDiElectron = event->diElectron();

	for(u_int i=0 ; i<mNumDielectron ; i++){

		StHltDiElectron *diElectron = (StHltDiElectron*)VecDiElectron.at(i);
		StHltTrackNode* daughter1node = (StHltTrackNode*)VecTrackNode.at(mDaughter1NodeSN[i]);
		StHltTrackNode* daughter2node = (StHltTrackNode*)VecTrackNode.at(mDaughter2NodeSN[i]);

		StHltTrack daughter1globalTrack ;
		if(daughter1node->globalTrack()) daughter1globalTrack = *daughter1node->globalTrack();
		StHltTrack daughter1primaryTrack ;
		if(daughter1node->primaryTrack()) daughter1primaryTrack = *daughter1node->primaryTrack();
		StHltBTofHit daughter1bTofHit ;
		if(daughter1node->bTofHit()) daughter1bTofHit = *daughter1node->bTofHit();
		StHltBEmcTowerHit daughter1bEmcHit ;
		if(daughter1node->bEmcTowerHit()) daughter1bEmcHit = *daughter1node->bEmcTowerHit();

		diElectron->setDaughter1GlobalTrack(daughter1globalTrack);
		diElectron->setDaughter1PrimaryTrack(daughter1primaryTrack);
		diElectron->setDaughter1BTofHit(daughter1bTofHit);
		diElectron->setDaughter1BEmcTowerHit(daughter1bEmcHit);
		diElectron->setDaughter1GlobalTrackSN(daughter1node->globalTrackSN());
		diElectron->setDaughter1PrimaryTrackSN(daughter1node->primaryTrackSN());
		diElectron->setDaughter1TofHitSN(daughter1node->tofHitSN());
		diElectron->setDaughter1EmcTowerSN(daughter1node->emcTowerSN());
		diElectron->setDaughter1BEmcMatchPhiDiff(daughter1node->bEmcMatchPhiDiff());
		diElectron->setDaughter1BEmcMatchZEdge(daughter1node->bEmcMatchZEdge());
		diElectron->setDaughter1BTofProjChannel(daughter1node->bTofProjChannel());
		diElectron->setDaughter1BTofCellLocalY(daughter1node->bTofCellLocalY());
		diElectron->setDaughter1BTofCellLocalZ(daughter1node->bTofCellLocalZ());
		diElectron->setDaughter1BTofPathLength(daughter1node->bTofPathLength());
		diElectron->setDaughter1Beta(daughter1node->beta());
		diElectron->setDaughter1Tof(daughter1node->tof());

		StHltTrack daughter2globalTrack ;
		if(daughter2node->globalTrack()) daughter2globalTrack = *daughter2node->globalTrack();
		StHltTrack daughter2primaryTrack ;
		if(daughter2node->primaryTrack()) daughter2primaryTrack = *daughter2node->primaryTrack();
		StHltBTofHit daughter2bTofHit ;
		if(daughter2node->bTofHit()) daughter2bTofHit = *daughter2node->bTofHit();
		StHltBEmcTowerHit daughter2bEmcHit ;
		if(daughter2node->bEmcTowerHit()) daughter2bEmcHit = *daughter2node->bEmcTowerHit();

		diElectron->setDaughter2GlobalTrack(daughter2globalTrack);
		diElectron->setDaughter2PrimaryTrack(daughter2primaryTrack);
		diElectron->setDaughter2BTofHit(daughter2bTofHit);
		diElectron->setDaughter2BEmcTowerHit(daughter2bEmcHit);
		diElectron->setDaughter2GlobalTrackSN(daughter2node->globalTrackSN());
		diElectron->setDaughter2PrimaryTrackSN(daughter2node->primaryTrackSN());
		diElectron->setDaughter2TofHitSN(daughter2node->tofHitSN());
		diElectron->setDaughter2EmcTowerSN(daughter2node->emcTowerSN());
		diElectron->setDaughter2BEmcMatchPhiDiff(daughter2node->bEmcMatchPhiDiff());
		diElectron->setDaughter2BEmcMatchZEdge(daughter2node->bEmcMatchZEdge());
		diElectron->setDaughter2BTofProjChannel(daughter2node->bTofProjChannel());
		diElectron->setDaughter2BTofCellLocalY(daughter2node->bTofCellLocalY());
		diElectron->setDaughter2BTofCellLocalZ(daughter2node->bTofCellLocalZ());
		diElectron->setDaughter2BTofPathLength(daughter2node->bTofPathLength());
		diElectron->setDaughter2Beta(daughter2node->beta());
		diElectron->setDaughter2Tof(daughter2node->tof());

	}

	LOG_DEBUG << "Finish to fill di-electron to StHltEvent" << endm;

}

//_____________________________________________________________________________
///< Fill TriggerReason
void StHltMaker::fillTriggerReason(StHltEvent* event)
{

	LOG_DEBUG << "Begin to fill trigger reason to StHltEvent" << endm;

	// create StHltTriggerReason
	StSPtrVecHltDiElectron& VecDiElectron = event->diElectron();
	for(u_int i=0 ; i<VecDiElectron.size() ; i++){

		StHltDiElectron *diElectron = (StHltDiElectron*)VecDiElectron.at(i);
		StHltTriggerReason *aTriggerReason = new  StHltTriggerReason();
		aTriggerReason->setReasonBit(kDiElectron);
		aTriggerReason->setReason(diElectron);
		mStHltEvent->addTriggerReason(aTriggerReason);

	}

	StSPtrVecHltHighPt& VecHighPt = event->highPt();
	for(u_int i=0 ; i<VecHighPt.size() ; i++){

		StHltHighPt *highPt = (StHltHighPt*)VecHighPt.at(i);
		StHltTriggerReason *aTriggerReason = new  StHltTriggerReason();
		aTriggerReason->setReasonBit(kHighPt);
		aTriggerReason->setReason(highPt);
		mStHltEvent->addTriggerReason(aTriggerReason);

	}

	StSPtrVecHltHeavyFragment& VecHeavyFragment = event->heavyFragment();
	for(u_int i=0 ; i<VecHeavyFragment.size() ; i++){

		StHltHeavyFragment *heavyFragment = (StHltHeavyFragment*)VecHeavyFragment.at(i);
		StHltTriggerReason *aTriggerReason = new  StHltTriggerReason();
		aTriggerReason->setReasonBit(kHeavyFragment);
		aTriggerReason->setReason(heavyFragment);
		mStHltEvent->addTriggerReason(aTriggerReason);

	}

	LOG_DEBUG << "Finish to fill trigger reason to StHltEvent" << endm;

}

//_____________________________________________________________________________
///< Finish ...
Int_t StHltMaker::Finish()
{

	return StMaker::Finish();

}
