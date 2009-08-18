#include "StPi0DataMaker.h"

#include <numeric>

#include <TH1F.h>
#include <TDataSet.h>
#include <TRandom.h>
#include <TVector3.h>
#include <TDataSet.h>
#include <TObjectSet.h>
#include <TList.h>
#include <TString.h>
#include <TMath.h>

#include <St_base/StMessMgr.h>
#include <StEvent/StEvent.h>
#include <StEvent/StEventTypes.h>
#include <StEvent/StTriggerId.h>
#include <StEmcUtil/geometry/StEmcGeom.h>
#include <StEmcUtil/database/StBemcTables.h>
#include <StEmcUtil/projection/StEmcPosition.h>
#include <StDaqLib/EMC/StEmcDecoder.h>
#include <tables/St_emcStatus_Table.h>
#include <tables/St_smdStatus_Table.h>
#include <St_db_Maker/St_db_Maker.h>
#include <StMcEventTypes.hh>
#include <StMcEvent.hh>
#include <StMcVertex.hh>
#include <StMcTrack.hh>
#include <StMcEventMaker/StMcEventMaker.h>
#include <StEmcADCtoEMaker/StBemcData.h>
#include <StEventUtilities/StuFtpcRefMult.hh>

#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

ClassImp(StPi0DataMaker);

//_____________________________________________________________________________
StPi0DataMaker::StPi0DataMaker(const char *name) 
    : inherited(name) 
    , mEmcGeom(0)
    , mPsdGeom(0)
    , mSmdeGeom(0)
    , mSmdpGeom(0)
    , mEmcPosition(0)
    , mEmcDecoder(0)
    , mBemcTables(0)
    , mEventMixer(0)
    , mMCGammaTreeDataArray(0)
    , mMCPionTreeDataArray(0)
    , mMCEtaTreeDataArray(0)
    , mMCNbarTreeDataArray(0)
    , mCandidateTreeDataArray(0)
    , mCandidateTreeDataMixArray(0)
    , mCandidateTreeDataSubmixArray(0)
    , mEventTreeDataArray(0)
    , mHitTreeDataArray(0)
    , mClusterTreeDataArray(0)
    , mPointTreeDataArray(0)
    , mSMDThresholdTreeDataArray(0)
    , mEventSummary(0)
    , mTriggerSummary(0)
{
}

//_____________________________________________________________________________
StPi0DataMaker::~StPi0DataMaker() {
}

//_____________________________________________________________________________
void createDataset(Bool_t create, TDataSet *&dataset, TString name) {
    if (create) {
	{LOG_DEBUG << "Creating dataset " << name << endm;}
	dataset = new TDataSet(name);
	if (dataset) {
	    //{LOG_DEBUG << "Created dataset " << dataset << ", list " << dataset->GetList() << endm;}
	} else {LOG_ERROR << "Cannot create dataset " << name << endm;}
    } else {
	dataset = 0;
    }
}

//_____________________________________________________________________________
Int_t StPi0DataMaker::Init() {
    {LOG_DEBUG << "Starting Init()" << endm;}
    Int_t result = this->inherited::Init();

    this->mEmcGeom = new StEmcGeom("bemc");
    if (!this->mEmcGeom) {LOG_ERROR << "mEmcGeom not created!" << endm;}
    this->mSmdeGeom = new StEmcGeom("bsmde");
    if (!this->mSmdeGeom) {LOG_ERROR << "mSmdeGeom not created!" << endm;}
    this->mSmdpGeom = new StEmcGeom("bsmdp");
    if (!this->mSmdpGeom) {LOG_ERROR << "mSmdpGeom not created!" << endm;}
    this->mPsdGeom = new StEmcGeom("bprs");
    if (!this->mPsdGeom) {LOG_ERROR << "mPsdGeom not created!" << endm;}
    {LOG_DEBUG << "StEmcGeoms created" << endm;}

    this->mEmcPosition = new StEmcPosition();
    if (!this->mEmcPosition) {LOG_ERROR << "mEmcPosition not created!" << endm;}
    {LOG_DEBUG << "StEmcPosition created" << endm;}

    this->mBemcTables = new StBemcTables();
    if (!this->mBemcTables) {LOG_ERROR << "mBemcTables not created!" << endm;}
    {LOG_DEBUG << "StBemcTables created" << endm;}

    this->mEmcDecoder = new StEmcDecoder();
    if (!this->mEmcDecoder) {LOG_ERROR << "mEmcDecoder not created!" << endm;}
    {LOG_DEBUG << "StEmcDecoder created" << endm;}

    list<TEventMixerParameters::value_type> min; list<TEventMixerParameters::value_type> max; list<TEventMixerParameters::mixer_type::size_type> size;
    if (this->settings.MixingClassZ)        { min.push_back(-70);   max.push_back(70);    size.push_back(TEventMixerParameters::mixer_type::size_type(((70) - (-70)) / this->settings.MixingClassZSize)); } // Mixing class in Z_vertex
    if (this->settings.MixingClassBemcMult) { min.push_back(0);     max.push_back(30);    size.push_back(TEventMixerParameters::mixer_type::size_type(((30) - (0)) / this->settings.MixingClassBemcMultSize)); } // Mixing class in the number of BEMC points
    if (this->settings.MixingClassTrigger)  { min.push_back(0.5);   max.push_back(3.5);   size.push_back(TEventMixerParameters::mixer_type::size_type(3));  } // Mixing class in trigger
    if (this->settings.MixingClassJetEta)   { min.push_back(-1.2);  max.push_back(1.2);   size.push_back(TEventMixerParameters::mixer_type::size_type(((1.2) - (-1.2)) / this->settings.MixingClassJetEtaSize)); } // Mixing class in jet eta
    if (this->settings.MixingClassJetPhi)   { min.push_back(-TMath::Pi());  max.push_back(+TMath::Pi());   size.push_back(TEventMixerParameters::mixer_type::size_type(((+TMath::Pi()) - (-TMath::Pi())) / this->settings.MixingClassJetPhiSize)); } // Mixing class in jet phi
    if (this->settings.MixingClassJetET)    { min.push_back(0);     max.push_back(20);    size.push_back(TEventMixerParameters::mixer_type::size_type(((20) - (0)) / this->settings.MixingClassJetETSize)); } // Mixing class in jet E_T

    this->mEventMixer = new TEventMixerParameters(min, max, size, this->settings.MixedEventsNumber, 2);
    if (!this->mEventMixer) {LOG_ERROR << "mEventMixer not created!" << endm;}
    {LOG_DEBUG << "Event mixer created" << endm;}
    
    {LOG_DEBUG << "Finished Init()" << endm;}
    return result;
}

//_____________________________________________________________________________
void StPi0DataMaker::Clear(Option_t *option) {
    {LOG_DEBUG << "Starting Clear()" << endm;}
    this->inherited::Clear(option);

    this->mMCGammaTreeDataArray = 0;
    this->mMCPionTreeDataArray = 0;
    this->mMCEtaTreeDataArray = 0;
    this->mMCNbarTreeDataArray = 0;
    this->mCandidateTreeDataArray = 0;
    this->mCandidateTreeDataMixArray = 0;
    this->mCandidateTreeDataSubmixArray = 0;
    this->mEventTreeDataArray = 0;
    this->mHitTreeDataArray = 0;
    this->mClusterTreeDataArray = 0;
    this->mPointTreeDataArray = 0;
    this->mSMDThresholdTreeDataArray = 0;

    this->mEventSummary = 0;
    this->mTriggerSummary = 0;

    {LOG_DEBUG << "Finished Clear()" << endm;}
}

//_____________________________________________________________________________
Int_t StPi0DataMaker::Make() {
    {LOG_DEBUG << "Starting Make()" << endm;}
    Int_t result = inherited::Make();

    this->mEventSummary = createH1F(0, eventSummaryName, "0: total number of events, 1: accepted events", eventSummaryNbins, eventSummaryMin, eventSummaryMax);
    if (!this->mEventSummary) {LOG_WARN << "mEventSummary not created!" << endm;}
    this->mTriggerSummary = createH1F(0, triggerSummaryName, "0:MB, 1:HT1, 2:HT2, 3:all", triggerSummaryNbins, triggerSummaryMin, triggerSummaryMax);
    if (!this->mTriggerSummary) {LOG_WARN << "mTriggerSummary not created!" << endm;}
    {LOG_DEBUG << "Summary histograms created" << endm;}
    
    {LOG_DEBUG << "Start creating datasets" << endm;}
    createDataset(this->settings.saveMCGammas || this->settings.saveMCGammasPlain,                     this->mMCGammaTreeDataArray,         mcGammaDatasetName);
    createDataset(this->settings.saveMCPions || this->settings.saveMCPionsPlain,                       this->mMCPionTreeDataArray,          mcPionDatasetName);
    createDataset(this->settings.saveMCEtas || this->settings.saveMCEtasPlain,                         this->mMCEtaTreeDataArray,           mcEtaDatasetName);
    createDataset(this->settings.saveMCNbars || this->settings.saveMCNbarsPlain,                       this->mMCNbarTreeDataArray,          mcNbarDatasetName);
    createDataset(this->settings.saveCandidates || this->settings.saveCandidatesPlain,                 this->mCandidateTreeDataArray,       candidateDatasetName);
    createDataset(this->settings.saveCandidatesMixed || this->settings.saveCandidatesMixedPlain,       this->mCandidateTreeDataMixArray,    candidateMixDatasetName);
    createDataset(this->settings.saveCandidatesSubmixed || this->settings.saveCandidatesSubmixedPlain, this->mCandidateTreeDataSubmixArray, candidateSubmixDatasetName);
    createDataset(this->settings.saveEvents || this->settings.saveEventsPlain,                         this->mEventTreeDataArray,           eventDatasetName);
    createDataset(this->settings.saveHits || this->settings.saveHitsPlain,                             this->mHitTreeDataArray,             hitDatasetName);
    createDataset(this->settings.saveClusters || this->settings.saveClustersPlain,                     this->mClusterTreeDataArray,         clusterDatasetName);
    createDataset(this->settings.savePoints || this->settings.savePointsPlain,                         this->mPointTreeDataArray,           pointDatasetName);
    createDataset(this->settings.saveSMDThreshold || this->settings.saveSMDThresholdPlain,             this->mSMDThresholdTreeDataArray,    smdThresholdDatasetName);
    {LOG_DEBUG << "Data arrays created" << endm;}

    if (this->mEventSummary) this->mEventSummary->Fill(0);

    StEvent *event = (StEvent*)GetInputDS(this->settings.datasetNameStEvent);
    if (!event) {
	if (this->mEventSummary) this->mEventSummary->Fill(3);
        {LOG_ERROR << "Cannot find StEvent at " << this->settings.datasetNameStEvent << endm;}
        result = kStWarn;
    }

    StMcEvent *mc_event = (StMcEvent*)GetInputDS(this->settings.datasetNameStMcEvent);
    if ((this->settings.isSimulation || this->settings.isEmbedding) && !mc_event) {
        if (mEventSummary) mEventSummary->Fill(4);
	{LOG_ERROR << "Cannot find StMcEvent at " << this->settings.datasetNameStMcEvent << endm;}
    }

    StEmcCollection* emcCollection = event ? (StEmcCollection*)event->emcCollection() : 0;
    if (event && !emcCollection) {
	if (mEventSummary) mEventSummary->Fill(5);
	{LOG_ERROR << "No StEmcCollection" << endm;}
    }

    StPrimaryVertex* primaryVertex = event ? event->primaryVertex() : 0;
    if (event && !primaryVertex) {
	if (mEventSummary) mEventSummary->Fill(6);
	{LOG_INFO << "No primary vertex" << endm;}
    }

    StEventSummary* summary = event ? event->summary() : 0;
    if (event && !summary) {LOG_WARN << "No StEventSummary" << endm;}

    Double_t bFld = 0;
    if (summary) {bFld = summary->magneticField() / 10.0;} // bFld in Tesla
    if(summary && (fabs(bFld) < 0.01)) {LOG_WARN << "Wrong field " << bFld << endm;}

    TMyTriggerData triggerData;
    getTriggerData(event, &this->settings.triggers[0], triggerData);
    if (this->settings.isSimulation && event && mc_event) triggerData.triggered |= this->settings.triggersSim; // take pure MC event as certain trigger
    
    Int_t triggerMixingClass = -1;
    if (this->settings.triggers[0] == 0) triggerMixingClass = 0;
    for (UInt_t trgInd = 0;trgInd < (sizeof(triggerData.triggered) * 8);trgInd++) if (triggerData.triggered & (1 << trgInd)) triggerMixingClass = trgInd;

    if (triggerData.triggered || (this->settings.triggers[0] == 0)) {

    {LOG_DEBUG << "Event accepted " << event->runId() << " " << event->id() << endm;}
    {LOG_DEBUG << "Trigger mixing class " << triggerMixingClass << endm;}
    if (mEventSummary) mEventSummary->Fill(1);
    for (UInt_t trgInd = 0;trgInd < (sizeof(triggerData.triggered) * 8);trgInd++) if (mTriggerSummary && (triggerData.triggered & (1 << trgInd))) mTriggerSummary->Fill(trgInd);

    StSPtrVecEmcPoint emcPoints;
    if (emcCollection) {
	const StSPtrVecEmcPoint &points = emcCollection->barrelPoints();
	for(StSPtrVecEmcPointConstIterator pointIter = points.begin();pointIter != points.end();pointIter++) {
	    const StEmcPoint *point = *pointIter;
	    if (point) {
		{LOG_DEBUG << "Copying point..." << endm;}
		//StEmcPoint *pointNew = dynamic_cast<StEmcPoint*>(point->Clone());
		StEmcPoint *pointNew = new StEmcPoint(
		    point->position(), 
		    point->positionError(), 
		    point->size(), 
		    point->hardwarePosition(), 
		    point->charge(), 
		    point->energy(), 
		    point->chiSquare(), 
		    point->trackReferenceCount()
		);
		if (pointNew) {
		    for (UInt_t i = 0;i < point->cluster(kBarrelEmcTowerId).size();i++) pointNew->addCluster(kBarrelEmcTowerId, point->cluster(kBarrelEmcTowerId)[i]);
		    for (UInt_t i = 0;i < point->cluster(kBarrelEmcPreShowerId).size();i++) pointNew->addCluster(kBarrelEmcPreShowerId, point->cluster(kBarrelEmcPreShowerId)[i]);
		    for (UInt_t i = 0;i < point->cluster(kBarrelSmdEtaStripId).size();i++) pointNew->addCluster(kBarrelSmdEtaStripId, point->cluster(kBarrelSmdEtaStripId)[i]);
		    for (UInt_t i = 0;i < point->cluster(kBarrelSmdPhiStripId).size();i++) pointNew->addCluster(kBarrelSmdPhiStripId, point->cluster(kBarrelSmdPhiStripId)[i]);
		}
		{LOG_DEBUG << "Copied point." << endm;}
		if (pointNew) {
		    emcPoints.push_back(pointNew);
		} else {
		    {LOG_ERROR << "Cannot clone StEmcPoint " << point << endm;}
		}
	    }
	}
	{LOG_DEBUG << "Points copied from size " << points.size() << " to size " << emcPoints.size() << endm;}
    }

    if (this->mEmcDecoder) {
	this->mEmcDecoder->SetFixTowerMapBug(this->settings.doTowerSwapFix);
	this->mEmcDecoder->SetDateTime(this->GetDate(), this->GetTime());
    }
    if (this->mBemcTables) this->mBemcTables->loadTables(this);
    StMaker *trigMaker = this->GetMaker(this->settings.triggerFullSimulatorNameFinal);

    TMyEventData eventData;
    list<TEventMixerParameters::value_type> eventMixingClass;

    getEventData(event, mc_event, this->mEmcGeom, this->mSmdeGeom, this->mSmdpGeom, this->mPsdGeom, this->mBemcTables
	, &this->settings.triggers[0], this->GetMaker(this->settings.triggerFullSimulatorName), this->GetMaker(this->settings.triggerFullSimulatorNameEmbed), trigMaker
	, this->GetMaker(this->settings.adcToEMakerName), this->settings.jetConeRadius, this->GetMaker(this->settings.jetMakerName), this->settings.jetFullMakerBranchName, this->GetMaker(this->settings.spinDbMakerName)
	, this->GetDataSet("geant"), this->settings.isPythia, eventData);
    eventData.trigger = triggerData;
    associateTracksWithEmcPoints(event, mc_event, emcPoints, this->mEmcGeom, this->mSmdeGeom, this->mSmdpGeom, this->mPsdGeom, this->mEmcPosition, this->mBemcTables);
    if (this->settings.MixingClassZ)        eventMixingClass.push_back(eventData.zTPC);
    if (this->settings.MixingClassBemcMult) eventMixingClass.push_back(eventData.nPoints);
    if (this->settings.MixingClassTrigger)  eventMixingClass.push_back(triggerMixingClass);
    if (this->settings.MixingClassJetEta)   eventMixingClass.push_back(eventData.jet.eta);
    if (this->settings.MixingClassJetPhi)   eventMixingClass.push_back(eventData.jet.phi);
    if (this->settings.MixingClassJetET)    eventMixingClass.push_back(eventData.jet.eT);
    eventData.mixerStatus = 0;
    if (this->mEventMixer && this->mEventMixer->withinParameters(eventMixingClass)) {
	eventData.mixerStatus |= (1 << 0);
    	if (this->mEventMixer->getMixer(eventMixingClass).getCount() >= this->mEventMixer->getMixer(eventMixingClass).getSize() - 1) eventData.mixerStatus |= (1 << 1);
    	if (this->mEventMixer->getMixer(eventMixingClass).getCount() >= 2 - 1) eventData.mixerStatus |= (1 << 2);
    }

    if (eventData.corruptedCrates) {LOG_WARN << "BEMC data is corrupted: crates bitmask " << (UInt_t)(eventData.corruptedCrates) << endm;}
    {LOG_DEBUG << "Acceptance " << (Int_t)eventData.acceptanceBTOW << " " << (Int_t)eventData.acceptanceBSMDE << " " << (Int_t)eventData.acceptanceBSMDP << ", hits " << (Int_t)eventData.nHitsBTOW << " " << (Int_t)eventData.nHitsBSMDE << " " << (Int_t)eventData.nHitsBSMDP << ", clusters " << (Int_t)eventData.nClustersBTOW << " " << (Int_t)eventData.nClustersBSMDE << " " << (Int_t)eventData.nClustersBSMDP << endm;}

    if (mc_event && (this->settings.saveMCPions || this->settings.saveMCPionsPlain) && this->mMCPionTreeDataArray) {
	    {LOG_DEBUG << "Saving MC pions" << endm;}
    	    StPtrVecMcTrack &tracks = mc_event->tracks();
    	    for (Int_t i = 0;i < (Int_t)tracks.size();i++) {
		const StMcTrack *mcTrack = tracks[i];
		if (((i == 0) || !this->settings.saveFirstMCPionOnly) && mcTrack && (mcTrack->geantId() == 7) && (!tracks[i]->isShower())) {
		    // 1 - gamma
		    // 7 - pi0
		    // 17 - eta
		    {LOG_INFO << "Saving MC pion" << endm;}
		    TMyMCDecayTreeData *mcPionTreeData = new TMyMCDecayTreeData();
		    if (mcPionTreeData) {
			getMCDecayData(mcTrack, event, this->mBemcTables, this->mEmcGeom, this->mSmdeGeom, this->mSmdpGeom, this->mPsdGeom, mcPionTreeData->decay);
			mcPionTreeData->event = eventData;
			TObjectSet *objSet = new TObjectSet(mcPionTreeData);
			if (objSet) {
			    this->mMCPionTreeDataArray->Add(objSet);
			} else {
			    {LOG_ERROR << "Cannot create TObjectSet" << endm;}
			    delete mcPionTreeData;
			}
		    } else {LOG_ERROR << "Cannot create structure" << endm;}
		}
	    }
    }
    if (mc_event && (this->settings.saveMCEtas || this->settings.saveMCEtasPlain) && this->mMCEtaTreeDataArray) {
	    {LOG_DEBUG << "Saving MC etas" << endm;}
    	    StPtrVecMcTrack &tracks = mc_event->tracks();
    	    for (Int_t i = 0;i < (Int_t)tracks.size();i++) {
		const StMcTrack *mcTrack = tracks[i];
		if (((i == 0) || !this->settings.saveFirstMCEtaOnly) && mcTrack && (mcTrack->geantId() == 17) && (!tracks[i]->isShower())) {
		    {LOG_INFO << "Saving MC eta" << endm;}
		    TMyMCDecayTreeData *mcEtaTreeData = new TMyMCDecayTreeData();
		    if (mcEtaTreeData) {
			getMCDecayData(mcTrack, event, this->mBemcTables, this->mEmcGeom, this->mSmdeGeom, this->mSmdpGeom, this->mPsdGeom, mcEtaTreeData->decay);
			mcEtaTreeData->event = eventData;
			TObjectSet *objSet = new TObjectSet(mcEtaTreeData);
			if (objSet) {
			    this->mMCEtaTreeDataArray->Add(objSet);
			} else {
			    {LOG_ERROR << "Cannot create TObjectSet" << endm;}
			    delete mcEtaTreeData;
			}
		    } else {LOG_ERROR << "Cannot create structure" << endm;}
		}
	    }
    }
    if (mc_event && (this->settings.saveMCNbars || this->settings.saveMCNbarsPlain) && this->mMCNbarTreeDataArray) {
	    {LOG_DEBUG << "Saving MC nbars" << endm;}
    	    StPtrVecMcTrack &tracks = mc_event->tracks();
    	    for (Int_t i = 0;i < (Int_t)tracks.size();i++) {
		const StMcTrack *mcTrack = tracks[i];
		if (((i == 0) || !this->settings.saveFirstMCNbarOnly) && mcTrack && (mcTrack->geantId() == 25) && (!tracks[i]->isShower())) {
		    {LOG_INFO << "Saving MC nbar" << endm;}
		    TMyMCParticleTreeData *mcNbarTreeData = new TMyMCParticleTreeData();
		    if (mcNbarTreeData) {
			getMCParticleData(mcTrack, event, this->mBemcTables, this->mEmcGeom, this->mSmdeGeom, this->mSmdpGeom, this->mPsdGeom, mcNbarTreeData->particle);
			mcNbarTreeData->event = eventData;
			TObjectSet *objSet = new TObjectSet(mcNbarTreeData);
			if (objSet) {
			    this->mMCNbarTreeDataArray->Add(objSet);
			} else {
			    {LOG_ERROR << "Cannot create TObjectSet" << endm;}
			    delete mcNbarTreeData;
			}
		    } else {LOG_ERROR << "Cannot create structure" << endm;}
		}
	    }
    }
    if (mc_event && (this->settings.saveMCGammas || this->settings.saveMCGammasPlain) && this->mMCGammaTreeDataArray) {
	    {LOG_DEBUG << "Saving MC gammas" << endm;}
	    StPtrVecMcTrack &tracks = mc_event->tracks();
	    for (Int_t i = 0;i < (Int_t)tracks.size();i++) {
    		const StMcTrack *mcTrack = tracks[i];
    		if (((i == 0) || !this->settings.saveFirstMCGammaOnly) && mcTrack && (mcTrack->geantId() == 1) && (!tracks[i]->isShower())) {
		    {LOG_INFO << "Saving MC gamma" << endm;}
	    	    TMyMCParticleTreeData *mcGammaTreeData = new TMyMCParticleTreeData();
		    if (mcGammaTreeData) {
	    		getMCParticleData(mcTrack, event, this->mBemcTables, this->mEmcGeom, this->mSmdeGeom, this->mSmdpGeom, this->mPsdGeom, mcGammaTreeData->particle);
    			mcGammaTreeData->event = eventData;
			TObjectSet *objSet = new TObjectSet(mcGammaTreeData);
			if (objSet) {
			    this->mMCGammaTreeDataArray->Add(objSet);
			} else {
			    {LOG_ERROR << "Cannot create TObjectSet" << endm;}
			    delete mcGammaTreeData;
			}
		    } else {LOG_ERROR << "Cannot create structure" << endm;}
		}
	    }
    }

    {LOG_DEBUG << "Collecting points" << endm;}
    TEventMixer::list_type pointsData;
    const StSPtrVecEmcPoint& points = emcPoints;
    for(StSPtrVecEmcPointConstIterator pointIter = points.begin();pointIter != points.end();pointIter++) {
	const StEmcPoint *point = *pointIter;
	if (point) {
    	    TMyPointTreeData *pointTreeData = new TMyPointTreeData();
	    if (pointTreeData) {
    	        getPointData(point, trigMaker, this->mEmcGeom, this->mSmdeGeom, this->mSmdpGeom, this->mPsdGeom, this->mBemcTables, &this->settings.triggers[0], pointTreeData->point);
		{LOG_DEBUG << "Point: etaCoord = " << point->position().pseudoRapidity() << ", phiCoord = " << point->position().phi() << ", E = " << point->energy() << ", size = " << (Int_t)pointTreeData->point.clusterBTOW.size << " " << (Int_t)pointTreeData->point.clusterBSMDE.size << " " << (Int_t)pointTreeData->point.clusterBSMDP.size << endm;}
		pointTreeData->event = eventData;
    	        pointsData.push_back(*pointTreeData);
    	        if ((this->settings.savePoints || this->settings.savePointsPlain) && this->mPointTreeDataArray) {
		    {LOG_INFO << "Saving point" << endm;}
		    TObjectSet *objSet = new TObjectSet(pointTreeData);
		    if (objSet) {
			this->mPointTreeDataArray->Add(objSet);
		    } else {
		        {LOG_ERROR << "Cannot create TObjectSet" << endm;}
		        delete pointTreeData;
		    }
	        } else {
	    	    delete pointTreeData;
		}
	    } else {LOG_ERROR << "Cannot create structure" << endm;}
	}
    }

    if ((this->settings.saveCandidates || this->settings.saveCandidatesPlain) && this->mCandidateTreeDataArray) {
	{LOG_DEBUG << "Saving candidates" << endm;}
	this->combineCandidates(pointsData, pointsData, this->mCandidateTreeDataArray);
    }

  if ((this->settings.saveCandidatesSubmixed || this->settings.saveCandidatesSubmixedPlain) && this->mCandidateTreeDataSubmixArray && (pointsData.size() >= 3)) {
    {LOG_DEBUG << "Saving candidates (submixed)" << endm;}
    Int_t numPoints = pointsData.size();
    TEventMixer::list_type pointsDataSubmix = pointsData;
    Int_t *indices = new Int_t[numPoints];
    Bool_t *indicesShuffled = new Bool_t[numPoints];
    for (Int_t i = 0;i < this->settings.SubmixedEventsNumber;i++) {
      Bool_t shuffledWell = false;
      for (Int_t j = 0;j < numPoints;indicesShuffled[j++] = false);
      if (this->settings.ShuffleSubmixEnergy) {
	shuffledWell |= shuffleArray(indices, numPoints, true, 3 * numPoints, true);
	TEventMixer::list_type::iterator iterOrig = pointsData.begin();
	for (Int_t i = numPoints - 1;i >= 0;i--, iterOrig++) {
	  TEventMixer::list_type::iterator iterShuffle = pointsDataSubmix.begin();
	  for (Int_t ii = indices[i];ii >= 0;ii--, ++iterShuffle);
	  TMyPointTreeData &pointOrig = *iterOrig;
	  TMyPointTreeData &pointShuffle = *iterShuffle;
	  pointShuffle.point.energy = pointOrig.point.energy;
	  pointShuffle.point.clusterBTOW.energy = pointOrig.point.clusterBTOW.energy;
#ifdef SAVE_BPRS
	  pointShuffle.point.clusterBPRS.energy = pointOrig.point.clusterBPRS.energy;
#endif
	  pointShuffle.point.clusterBSMDE.energy = pointOrig.point.clusterBSMDE.energy;
	  pointShuffle.point.clusterBSMDP.energy = pointOrig.point.clusterBSMDP.energy;
	}
        for (Int_t j = 0;j < numPoints;j++) indicesShuffled[j] |= (indices[j] != j);
      }
      if (this->settings.ShuffleSubmixEta) {
	shuffledWell |= shuffleArray(indices, numPoints, true, 3 * numPoints, true);
	TEventMixer::list_type::iterator iterOrig = pointsData.begin();
	for (Int_t i = numPoints - 1;i >= 0;i--, ++iterOrig) {
	  TEventMixer::list_type::iterator iterShuffle = pointsDataSubmix.begin();
	  for (Int_t ii = indices[i];ii >= 0;ii--, ++iterShuffle);
	  TMyPointTreeData &pointOrig = *iterOrig;
	  TMyPointTreeData &pointShuffle = *iterShuffle;
	  pointShuffle.point.etaCoord = pointOrig.point.etaCoord;
	}
        for (Int_t j = 0;j < numPoints;j++) indicesShuffled[j] |= (indices[j] != j);
      }
      if (this->settings.ShuffleSubmixPhi) {
	shuffledWell |= shuffleArray(indices, numPoints, true, 3 * numPoints, true);
	TEventMixer::list_type::iterator iterOrig = pointsData.begin();
	for (Int_t i = numPoints - 1;i >= 0;i--, ++iterOrig) {
	  TEventMixer::list_type::iterator iterShuffle = pointsDataSubmix.begin();
	  for (Int_t ii = indices[i];ii >= 0;ii--, ++iterShuffle);
	  TMyPointTreeData &pointOrig = *iterOrig;
	  TMyPointTreeData &pointShuffle = *iterShuffle;
	  pointShuffle.point.phiCoord = pointOrig.point.phiCoord;
	}
        for (Int_t j = 0;j < numPoints;j++) indicesShuffled[j] |= (indices[j] != j);
      }
      Bool_t shuffledAllWell = true;
      for (Int_t j = 0;j < numPoints;shuffledAllWell &= indicesShuffled[j++]);
      if (shuffledWell && shuffledAllWell) {
	this->combineCandidates(pointsDataSubmix, pointsDataSubmix, this->mCandidateTreeDataSubmixArray);
      }
    }
    delete [] indices;
    delete [] indicesShuffled;
  }
    if ((this->settings.saveCandidatesMixed || this->settings.saveCandidatesMixedPlain) && this->mCandidateTreeDataMixArray && this->mEventMixer) {
        {LOG_DEBUG << "Saving candidates (mixed)" << endm;}
	if (!this->mEventMixer->withinParameters(eventMixingClass)) {if (this->mEventSummary) this->mEventSummary->Fill(9);}
        TEventMixer &mixer = this->mEventMixer->getMixer(eventMixingClass);
        mixer.addEvent(pointsData);
        if (mixer.getCount() >= 2) {
	    Int_t mixedPoint1 = mixer.getCount() - 1;
	    TEventMixer::const_list_reference points1 = mixer.getList(mixedPoint1, 0);
	    for (Int_t mixedPoint2 = 0;mixedPoint2 < mixedPoint1;mixedPoint2++) {
		TEventMixer::const_list_reference points2 = mixer.getList(mixedPoint2, 1);
		this->combineCandidates(points1, points2, this->mCandidateTreeDataMixArray);
	    }
        } else {
	    if (this->mEventSummary) this->mEventSummary->Fill(8);
	    {LOG_DEBUG << "Mixer is not ready" << endm;}
        }
    }

    if ((this->settings.saveEvents || this->settings.saveEventsPlain) && this->mEventTreeDataArray) {
	{LOG_INFO << "Saving event" << endm;}
	TMyEventTreeData *eventTreeData = new TMyEventTreeData();
	if (eventTreeData) {
	    eventTreeData->event = eventData;
	    TObjectSet *objSet = new TObjectSet(eventTreeData);
	    if (objSet) {
		this->mEventTreeDataArray->Add(objSet);
	    } else {
	        {LOG_ERROR << "Cannot create TObjectSet" << endm;}
	        delete eventTreeData;
	    }
	} else {LOG_ERROR << "Cannot create structure" << endm;}
    }

    if ((this->settings.saveSMDThreshold || this->settings.saveSMDThresholdPlain) && this->mSMDThresholdTreeDataArray) {
	{LOG_INFO << "Saving SMD threshold" << endm;}
	TMySMDThresholdTreeData *smdThresholdTreeData = new TMySMDThresholdTreeData();
	if (smdThresholdTreeData) {
	    getSMDThresholdData(emcCollection->detector(kBarrelSmdEtaStripId), this->mSmdeGeom, this->mBemcTables, this->settings.smd1Threshold, this->settings.smdThresholdEnergy, this->settings.smdThresholdEt, smdThresholdTreeData->smdEta1);
	    getSMDThresholdData(emcCollection->detector(kBarrelSmdPhiStripId), this->mSmdpGeom, this->mBemcTables, this->settings.smd1Threshold, this->settings.smdThresholdEnergy, this->settings.smdThresholdEt, smdThresholdTreeData->smdPhi1);
	    getSMDThresholdData(emcCollection->detector(kBarrelSmdEtaStripId), this->mSmdeGeom, this->mBemcTables, this->settings.smd2Threshold, this->settings.smdThresholdEnergy, this->settings.smdThresholdEt, smdThresholdTreeData->smdEta2);
	    getSMDThresholdData(emcCollection->detector(kBarrelSmdPhiStripId), this->mSmdpGeom, this->mBemcTables, this->settings.smd2Threshold, this->settings.smdThresholdEnergy, this->settings.smdThresholdEt, smdThresholdTreeData->smdPhi2);
	    getSMDThresholdData(emcCollection->detector(kBarrelSmdEtaStripId), this->mSmdeGeom, this->mBemcTables, this->settings.smd3Threshold, this->settings.smdThresholdEnergy, this->settings.smdThresholdEt, smdThresholdTreeData->smdEta3);
	    getSMDThresholdData(emcCollection->detector(kBarrelSmdPhiStripId), this->mSmdpGeom, this->mBemcTables, this->settings.smd3Threshold, this->settings.smdThresholdEnergy, this->settings.smdThresholdEt, smdThresholdTreeData->smdPhi3);
	    smdThresholdTreeData->event = eventData;
	    TObjectSet *objSet = new TObjectSet(smdThresholdTreeData);
	    if (objSet) {
		this->mSMDThresholdTreeDataArray->Add(objSet);
	    } else {
	        {LOG_ERROR << "Cannot create TObjectSet" << endm;}
	        delete smdThresholdTreeData;
	    }
	} else {LOG_ERROR << "Cannot create structure" << endm;}
    }

  for (Int_t detId = 0;detId <= 3;detId++) {
    StDetectorId detectorId = StDetectorId(kBarrelEmcTowerId + detId);
    const StEmcDetector *detector = emcCollection->detector(detectorId);
    if (detector) {
      const StEmcClusterCollection *clusterCollection = detector->cluster();
      if (clusterCollection && (this->settings.saveClusters || this->settings.saveClustersPlain) && this->mClusterTreeDataArray) {
	{LOG_DEBUG << "Saving clusters" << endm;}
	for (StSPtrVecEmcClusterConstIterator clusterIter = clusterCollection->clusters().begin();clusterIter != clusterCollection->clusters().end();clusterIter++) {
	  const StEmcCluster *cluster = *clusterIter;
	  if (cluster) {
	    {LOG_INFO << "Saving cluster" << endm;}
	    TMyClusterTreeData *clusterTreeData = new TMyClusterTreeData();
	    if (clusterTreeData) {
		getClusterData(cluster, this->mEmcGeom, this->mSmdeGeom, this->mSmdpGeom, this->mPsdGeom, this->mBemcTables, clusterTreeData->cluster);
		{LOG_DEBUG << "Cluster: det = " << (Int_t)clusterTreeData->cluster.detector << ", etaCoord = " << clusterTreeData->cluster.etaCoord << ", phiCoord = " << clusterTreeData->cluster.phiCoord << ", E = " << clusterTreeData->cluster.energy << ", size = " << (Int_t)clusterTreeData->cluster.size << endm;}
		clusterTreeData->event = eventData;
		TObjectSet *objSet = new TObjectSet(clusterTreeData);
		if (objSet) {
		    this->mClusterTreeDataArray->Add(objSet);
		} else {
		    {LOG_ERROR << "Cannot create TObjectSet" << endm;}
		    delete clusterTreeData;
		}
	    } else {LOG_ERROR << "Cannot create structure" << endm;}
	  }
	}
      }
      if ((this->settings.saveHits || this->settings.saveHitsPlain) && this->mHitTreeDataArray) {
	{LOG_DEBUG << "Saving hits" << endm;}
	for (UInt_t moduleIndex = 1;moduleIndex <= detector->numberOfModules();moduleIndex++) {
	  const StEmcModule *module = detector->module(moduleIndex);
	  if (module) {
	    for (StSPtrVecEmcRawHitConstIterator hitIter = module->hits().begin();hitIter != module->hits().end();hitIter++) {
	      const StEmcRawHit *hit = *hitIter;
	      if (hit) {
		{LOG_INFO << "Saving hit" << endm;}
		TMyHitTreeData *hitTreeData = new TMyHitTreeData();
		if (hitTreeData) {
    		    getHitData(hit, this->mEmcGeom, this->mSmdeGeom, this->mSmdpGeom, this->mPsdGeom, this->mBemcTables, hitTreeData->hit);
		    hitTreeData->event = eventData;
		    TObjectSet *objSet = new TObjectSet(hitTreeData);
		    if (objSet) {
			this->mHitTreeDataArray->Add(objSet);
		    } else {
		        {LOG_ERROR << "Cannot create TObjectSet" << endm;}
		        delete hitTreeData;
		    }
		} else {LOG_ERROR << "Cannot create structure" << endm;}
	      }
	    }
	  }
	}
      }
    }
  }

    } else { // if event in not accepted
	if (mEventSummary) mEventSummary->Fill(7);
	if (mTriggerSummary) mTriggerSummary->Fill(mTriggerSummary->GetXaxis()->GetBinCenter(mTriggerSummary->GetXaxis()->GetNbins()));
	{LOG_INFO << "Trigger is not accepted" << endm;}
    }

    this->postData();

    {LOG_DEBUG << "Finished Make()";}
    return result;
}

//______________________________________________________________________________
Float_t diffPointsEnergy(const TMyPointTreeData &pt1, const TMyPointTreeData &pt2) {
    return pt1.point.energy - pt2.point.energy;
}

Bool_t comparePointsEnergy(const TMyPointTreeData &pt1, const TMyPointTreeData &pt2) {
    return diffPointsEnergy(pt1, pt2) > 0;
}

Float_t getPointJetDist(const TMyPointTreeData &pt) {
    TVector3 v;
    v.SetPtEtaPhi(EMCRadius, pt.event.jet.eta, pt.event.jet.phi);
    v.SetZ(v.Z() + pt.event.zTPC);
    Float_t deta = pt.point.etaCoord - v.Eta();
    Float_t dphi = pt.point.phiCoord - pt.event.jet.phi;
    while (dphi < -TMath::Pi()) dphi += TMath::TwoPi();
    while (dphi > +TMath::Pi()) dphi -= TMath::TwoPi();
    return sqrt((deta * deta) + (dphi * dphi));
}

Float_t diffPointsJetDist(const TMyPointTreeData &pt1, const TMyPointTreeData &pt2) {
    return getPointJetDist(pt1) - getPointJetDist(pt2);
}

Bool_t comparePointsJetDist(const TMyPointTreeData &pt1, const TMyPointTreeData &pt2) {
    return diffPointsJetDist(pt1, pt2) > 0;
}

typedef pair<Float_t, Float_t> point_id_type;
typedef pair<point_id_type, point_id_type> point_match_type;
typedef list<point_match_type> list_match_type;
typedef Float_t (*diff_type)(const TMyPointTreeData &pt1, const TMyPointTreeData &pt2);
typedef Bool_t (*compare_type)(const TMyPointTreeData &pt1, const TMyPointTreeData &pt2);

point_id_type getPointId(const TMyPointTreeData &pt) {
    return point_id_type(pt.event.eventId, pt.point.energy);
}

point_match_type getPointsMatch(const TMyPointTreeData &pt1, const TMyPointTreeData &pt2) {
    return point_match_type(getPointId(pt1), getPointId(pt2));
}

Bool_t arePointsMatched(const TMyPointTreeData &pt1, const TMyPointTreeData &pt2, const list_match_type &matches) {
    return find(matches.begin(), matches.end(), getPointsMatch(pt1, pt2)) != matches.end();
}

typedef pair<TMyPointTreeData, Float_t> distance_type;
typedef pair<TMyPointTreeData, distance_type> closest_type;
typedef closest_type (*update_closest_type)(const closest_type &, const TMyPointTreeData &);

closest_type updateClosest(const closest_type &cur_closest, const TMyPointTreeData &pt, diff_type diffPoints) {
    closest_type new_closest(cur_closest);
    Float_t dist = TMath::Abs(diffPoints(new_closest.first, pt));
    if ((dist < new_closest.second.second) || (new_closest.second.second < 0)) {
	new_closest.second.first = pt;
	new_closest.second.second = dist;
    }
    return new_closest;
}

closest_type updateClosestEnergy(const closest_type &cur_closest, const TMyPointTreeData &pt) {
    return updateClosest(cur_closest, pt, diffPointsEnergy);
}

closest_type updateClosestJetDist(const closest_type &cur_closest, const TMyPointTreeData &pt) {
    return updateClosest(cur_closest, pt, diffPointsJetDist);
}

void matchPointListsSimple(StPi0DataMaker::TEventMixer::const_list_reference points1, StPi0DataMaker::TEventMixer::const_list_reference points2, compare_type compare, list_match_type &matches) {
    StPi0DataMaker::TEventMixer::list_type pointsSorted1 = points1;
    StPi0DataMaker::TEventMixer::list_type pointsSorted2 = points2;
    pointsSorted1.sort(compare);
    pointsSorted2.sort(compare);
    StPi0DataMaker::TEventMixer::list_type::const_iterator point1Iter = pointsSorted1.begin();
    StPi0DataMaker::TEventMixer::list_type::const_iterator point2Iter = pointsSorted2.begin();
    for (;(point1Iter != pointsSorted1.end()) && (point2Iter != pointsSorted2.end());++point1Iter, ++point2Iter) {
	matches.push_back(getPointsMatch(*point1Iter, *point2Iter));
    }
}

void matchPointListsClosest(StPi0DataMaker::TEventMixer::const_list_reference points1, StPi0DataMaker::TEventMixer::const_list_reference points2, update_closest_type updateClosest, list_match_type &matches) {
    for (StPi0DataMaker::TEventMixer::list_type::const_iterator point1Iter = points1.begin();point1Iter != points1.end();++point1Iter) {
	closest_type closest(*point1Iter, distance_type(TMyPointTreeData(), -1));
	closest = accumulate(points2.begin(), points2.end(), closest, updateClosest);
	if (closest.second.second >= 0) matches.push_back(getPointsMatch(closest.first, closest.second.first));
    }
    for (StPi0DataMaker::TEventMixer::list_type::const_iterator point2Iter = points2.begin();point2Iter != points2.end();++point2Iter) {
	closest_type closest(*point2Iter, distance_type(TMyPointTreeData(), -1));
	closest = accumulate(points1.begin(), points1.end(), closest, updateClosest);
	if (closest.second.second >= 0) matches.push_back(getPointsMatch(closest.second.first, closest.first));
    }
}

//______________________________________________________________________________
void StPi0DataMaker::combineCandidates(TEventMixer::const_list_reference points1, TEventMixer::const_list_reference points2, TDataSet *array) {

    list_match_type matchesEnergySimple;
    matchPointListsSimple(points1, points2, comparePointsEnergy, matchesEnergySimple);
    list_match_type matchesJetDistSimple;
    matchPointListsSimple(points1, points2, comparePointsJetDist, matchesJetDistSimple);
    list_match_type matchesEnergyClosest;
    matchPointListsClosest(points1, points2, updateClosestEnergy, matchesEnergyClosest);
    list_match_type matchesJetDistClosest;
    matchPointListsClosest(points1, points2, updateClosestJetDist, matchesJetDistClosest);

    Bool_t sameEvent = false;
    Int_t pointNum = 0;
    for (TEventMixer::list_type::const_iterator point1Iter = points1.begin();point1Iter != points1.end();++point1Iter) {
	++pointNum;
        TEventMixer::const_value_reference point1 = *point1Iter;
        TEventMixer::list_type::const_iterator point2Iter = points2.begin();
        if (sameEvent) for (Int_t point2Num = 0;point2Num < pointNum;++point2Num) ++point2Iter;
        for (;point2Iter != points2.end();++point2Iter) {
    	    TEventMixer::const_value_reference point2 = *point2Iter;
    	    if (!sameEvent) sameEvent = ((point1.event.runId == point2.event.runId) && (point1.event.eventId == point2.event.eventId));
    	    Bool_t samePoint = ((point1.point.etaCoord == point2.point.etaCoord) && (point1.point.phiCoord == point2.point.phiCoord));
    	    Bool_t saveThisCandidate = (!samePoint);
    	    if (!this->settings.saveCandidatesWithoutSMD && ((point1.point.clusterBSMDE.energy == 0) && (point1.point.clusterBSMDP.energy == 0) && (point2.point.clusterBSMDE.energy == 0) && (point2.point.clusterBSMDP.energy == 0))) saveThisCandidate = false;
    	    if (!this->settings.saveCandidatesWithoutSMDBoth && ((point1.point.clusterBSMDE.energy == 0) || (point1.point.clusterBSMDP.energy == 0) || (point2.point.clusterBSMDE.energy == 0) || (point2.point.clusterBSMDP.energy == 0))) saveThisCandidate = false;
    	    if (saveThisCandidate && array) {
		if (sameEvent) {LOG_INFO << "Saving candidate" << endm;} else {LOG_INFO << "Saving mixed candidate" << endm;}
		TMyCandidateTreeData *candidateTreeData = new TMyCandidateTreeData();
		if (candidateTreeData) {
		    candidateTreeData->point1 = point1;
		    candidateTreeData->point2 = point2;
		    candidateTreeData->candidate.pointsMatched = 0;
		    candidateTreeData->candidate.pointsMatched |= (arePointsMatched(point1, point2, matchesEnergySimple)   ? 1 : 0);
    		    candidateTreeData->candidate.pointsMatched |= (arePointsMatched(point1, point2, matchesJetDistSimple)  ? 2 : 0);
		    candidateTreeData->candidate.pointsMatched |= (arePointsMatched(point1, point2, matchesEnergyClosest)  ? 4 : 0);
		    candidateTreeData->candidate.pointsMatched |= (arePointsMatched(point1, point2, matchesJetDistClosest) ? 8 : 0);
		    TObjectSet *objSet = new TObjectSet(candidateTreeData);
		    if (objSet) {
			array->Add(objSet);
		    } else {
		        {LOG_ERROR << "Cannot create TObjectSet" << endm;}
		        delete candidateTreeData;
		    }
		} else {LOG_ERROR << "Cannot create structure" << endm;}
    	    }
	}
    }
}

//___________________________________________________________________________
Int_t StPi0DataMaker::Finish() {
    {LOG_DEBUG << "Starting Finish()" << endm;}

    Int_t result = this->inherited::Finish();

    if (this->mEmcGeom) delete this->mEmcGeom; this->mEmcGeom = 0;
    if (this->mSmdeGeom) delete this->mSmdeGeom; this->mSmdeGeom = 0;
    if (this->mSmdpGeom) delete this->mSmdpGeom; this->mSmdpGeom = 0;
    if (this->mPsdGeom) delete this->mPsdGeom; this->mPsdGeom = 0;
    if (this->mEmcPosition) delete this->mEmcPosition; this->mEmcPosition = 0;
    if (this->mBemcTables) delete this->mBemcTables; this->mBemcTables = 0;
    if (this->mEmcDecoder) delete this->mEmcDecoder; this->mEmcDecoder = 0;

    if (this->mEventMixer) delete this->mEventMixer; this->mEventMixer = 0;

    {LOG_DEBUG << "Finished Finish()" << endm;}
    return result;
}

//___________________________________________________________________________
void postDataset(StMaker *maker, TDataSet *dataset) {
    if (maker && dataset) {
	{LOG_DEBUG << "Posting dataset " << dataset->GetName() << endm;}
	maker->AddData(dataset);
    }
}
//___________________________________________________________________________
void postHistogram(StMaker *maker, TH1 *histogram) {
    if (maker && histogram) {
	{LOG_DEBUG << "Posting histogram " << histogram->GetName() << endm;}
	TObjectSet *objSet = new TObjectSet(histogram->GetName(), histogram);
	if (objSet) {
	    maker->AddData(objSet);
	    if (!(TH1*)(objSet->GetObject())) {LOG_ERROR << "Histogram disappeared!" << endm;}
	} else {LOG_ERROR << "Cannot create TObjectSet" << endm;}
    }
}

//___________________________________________________________________________
void StPi0DataMaker::postData() {
    {LOG_DEBUG << "Starting postData()" << endm;}

    {LOG_DEBUG << "Start posting arrays" << endm;}
    postDataset(this, this->mMCGammaTreeDataArray);
    postDataset(this, this->mMCPionTreeDataArray);
    postDataset(this, this->mMCEtaTreeDataArray);
    postDataset(this, this->mMCNbarTreeDataArray);
    postDataset(this, this->mCandidateTreeDataArray);
    postDataset(this, this->mCandidateTreeDataMixArray);
    postDataset(this, this->mCandidateTreeDataSubmixArray);
    postDataset(this, this->mEventTreeDataArray);
    postDataset(this, this->mHitTreeDataArray);
    postDataset(this, this->mClusterTreeDataArray);
    postDataset(this, this->mPointTreeDataArray);
    postDataset(this, this->mSMDThresholdTreeDataArray);

    {LOG_DEBUG << "Start posting histograms" << endm;}
    postHistogram(this, this->mEventSummary);
    postHistogram(this, this->mTriggerSummary);

    {LOG_DEBUG << "Finished postData()" << endm;}
}
