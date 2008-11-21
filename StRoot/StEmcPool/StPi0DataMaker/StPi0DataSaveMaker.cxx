#include "StPi0DataSaveMaker.h"

#include <TFile.h>
#include <TTree.h>
#include <TH1F.h>
#include <TDataSet.h>
#include <TObjectSet.h>
#include <TList.h>
#include <TString.h>

#include <St_base/StMessMgr.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>
#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>

ClassImp(StPi0DataSaveMaker);

//_____________________________________________________________________________
StPi0DataSaveMaker::StPi0DataSaveMaker(const Char_t *name) 
    : inherited(name) 
    , outputFileName("")
    , mFile(0)
    , mMCGammaTreeDataArray(0)
    , mMCGammaIndex(0)
    , mMCGammaTree(0)
    , mMCGammaTreePlain(0)
    , mMCPionTreeDataArray(0)
    , mMCPionIndex(0)
    , mMCPionTree(0)
    , mMCPionTreePlain(0)
    , mMCEtaTreeDataArray(0)
    , mMCEtaIndex(0)
    , mMCEtaTree(0)
    , mMCEtaTreePlain(0)
    , mMCNbarTreeDataArray(0)
    , mMCNbarIndex(0)
    , mMCNbarTree(0)
    , mMCNbarTreePlain(0)
    , mCandidateTreeDataArray(0)
    , mCandidateIndex(0)
    , mCandidateTree(0)
    , mCandidateTreePlain(0)
    , mCandidateTreeDataMixArray(0)
    , mCandidateMixIndex(0)
    , mCandidateMixTree(0)
    , mCandidateMixTreePlain(0)
    , mCandidateTreeDataSubmixArray(0)
    , mCandidateSubmixIndex(0)
    , mCandidateSubmixTree(0)
    , mCandidateSubmixTreePlain(0)
    , mEventTreeDataArray(0)
    , mEventIndex(0)
    , mEventTree(0)
    , mEventTreePlain(0)
    , mHitTreeDataArray(0)
    , mHitIndex(0)
    , mHitTree(0)
    , mHitTreePlain(0)
    , mClusterTreeDataArray(0)
    , mClusterIndex(0)
    , mClusterTree(0)
    , mClusterTreePlain(0)
    , mPointTreeDataArray(0)
    , mPointIndex(0)
    , mPointTree(0)
    , mPointTreePlain(0)
    , mSMDThresholdTreeDataArray(0)
    , mSMDThresholdIndex(0)
    , mSMDThresholdTree(0)
    , mSMDThresholdTreePlain(0)
    , mEventSummary(0)
    , mTriggerSummary(0)
    , mEventCounter(0)
{
}

//_____________________________________________________________________________
StPi0DataSaveMaker::~StPi0DataSaveMaker() {
}

//_____________________________________________________________________________
void initArray(Bool_t create, TClonesArray *&array, Int_t &index, TString branchName, Int_t size) {
    index = 0;
    if (create) {
	{LOG_DEBUG << "Creating array for branch " << branchName << endm;}
	array = new TClonesArray(getBranchType(branchName), 0);
	if (array) {
	    array->ExpandCreate(size);
	    array->BypassStreamer(true);
	} else {LOG_ERROR << "Cannot create array for branch " << branchName << endm;}
    } else {
	array = 0;
    }
}

//_____________________________________________________________________________
Int_t StPi0DataSaveMaker::Init() {
    {LOG_DEBUG << "Starting Init()" << endm;}
    Int_t result = this->inherited::Init();

    this->mFile = new TFile(this->outputFileName, "UPDATE");
    if (this->mFile) {
	this->mFile->cd();
        this->mFile->SetCompressionLevel(this->settings.compressionLevel);
	{LOG_INFO << "TFile " << this->mFile->GetName() << " IsOpen = " << (int) this->mFile->IsOpen() << endm;}
        this->mFile->WriteObject(&this->settings, "settings");
    } else {LOG_ERROR << "Cannot create TFile " << this->outputFileName << endm;}

    this->mEventSummary = createH1F(this->mFile, eventSummaryName, "0: total number of events, 1: accepted events", eventSummaryNbins, eventSummaryMin, eventSummaryMax);
    if (!this->mEventSummary) {LOG_WARN << "mEventSummary not created!" << endm;}
    this->mTriggerSummary = createH1F(this->mFile, triggerSummaryName, "0:MB, 1:HT1, 2:HT2, 3:all", triggerSummaryNbins, triggerSummaryMin, triggerSummaryMax);
    if (!this->mTriggerSummary) {LOG_WARN << "mTriggerSummary not created!" << endm;}
    {LOG_DEBUG << "Summary histograms created" << endm;}

    initArray(this->settings.saveMCGammas,           this->mMCGammaTreeDataArray,         this->mMCGammaIndex,         mcGammaBranchName,      this->settings.clonesArraySize);
    initArray(this->settings.saveMCPions,            this->mMCPionTreeDataArray,          this->mMCPionIndex,          mcPionBranchName,       this->settings.clonesArraySize);
    initArray(this->settings.saveMCEtas,             this->mMCEtaTreeDataArray,           this->mMCEtaIndex,           mcEtaBranchName,        this->settings.clonesArraySize);
    initArray(this->settings.saveMCNbars,            this->mMCNbarTreeDataArray,          this->mMCNbarIndex,          mcNbarBranchName,       this->settings.clonesArraySize);
    initArray(this->settings.saveCandidates,         this->mCandidateTreeDataArray,       this->mCandidateIndex,       candidateBranchName,    this->settings.clonesArraySize);
    initArray(this->settings.saveCandidatesMixed,    this->mCandidateTreeDataMixArray,    this->mCandidateMixIndex,    candidateBranchName,    this->settings.clonesArraySize);
    initArray(this->settings.saveCandidatesSubmixed, this->mCandidateTreeDataSubmixArray, this->mCandidateSubmixIndex, candidateBranchName,    this->settings.clonesArraySize);
    initArray(this->settings.saveEvents,             this->mEventTreeDataArray,           this->mEventIndex,           eventBranchName,        this->settings.clonesArraySize);
    initArray(this->settings.saveHits,               this->mHitTreeDataArray,             this->mHitIndex,             hitBranchName,          this->settings.clonesArraySize);
    initArray(this->settings.saveClusters,           this->mClusterTreeDataArray,         this->mClusterIndex,         clusterBranchName,      this->settings.clonesArraySize);
    initArray(this->settings.savePoints,             this->mPointTreeDataArray,           this->mPointIndex,           pointBranchName,        this->settings.clonesArraySize);
    initArray(this->settings.saveSMDThreshold,       this->mSMDThresholdTreeDataArray,    this->mSMDThresholdIndex,    smdThresholdBranchName, this->settings.clonesArraySize);
    {LOG_DEBUG << "Data arrays created" << endm;}

    const Int_t maxVirtualSize = 25*1024*1024;
    if (this->settings.saveMCGammas)                this->mMCGammaTree              = createTree(this->mFile, mcGammaTreeName, "Photons (simulated)", mcGammaBranchName, this->mMCGammaTreeDataArray->Class_Name(), this->mMCGammaTreeDataArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveMCPions)                 this->mMCPionTree               = createTree(this->mFile, mcPionTreeName, "Pions (simulated)", mcPionBranchName, this->mMCPionTreeDataArray->Class_Name(), this->mMCPionTreeDataArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveMCEtas)                  this->mMCEtaTree                = createTree(this->mFile, mcEtaTreeName, "Eta (simulated)", mcEtaBranchName, this->mMCEtaTreeDataArray->Class_Name(), this->mMCEtaTreeDataArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveMCNbars)                 this->mMCNbarTree               = createTree(this->mFile, mcNbarTreeName, "Nbar (simulated)", mcNbarBranchName, this->mMCNbarTreeDataArray->Class_Name(), this->mMCNbarTreeDataArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveCandidates)              this->mCandidateTree            = createTree(this->mFile, candidateTreeName, "Pion candidates", candidateBranchName, this->mCandidateTreeDataArray->Class_Name(), this->mCandidateTreeDataArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveCandidatesMixed)         this->mCandidateMixTree         = createTree(this->mFile, candidateTreeMixName, "Pion candidates (event mixing)", candidateBranchName, this->mCandidateTreeDataMixArray->Class_Name(), this->mCandidateTreeDataMixArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveCandidatesSubmixed)      this->mCandidateSubmixTree      = createTree(this->mFile, candidateTreeSubmixName, "Pion candidates (subevent mixing)", candidateBranchName, this->mCandidateTreeDataSubmixArray->Class_Name(), this->mCandidateTreeDataSubmixArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveEvents)                  this->mEventTree                = createTree(this->mFile, eventTreeName, "Events", eventBranchName, this->mEventTreeDataArray->Class_Name(), this->mEventTreeDataArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveHits)                    this->mHitTree                  = createTree(this->mFile, hitTreeName, "Hits", hitBranchName, this->mHitTreeDataArray->ClassName(), this->mHitTreeDataArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveClusters)                this->mClusterTree              = createTree(this->mFile, clusterTreeName, "Clusters", clusterBranchName, this->mClusterTreeDataArray->Class_Name(), this->mClusterTreeDataArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.savePoints)                  this->mPointTree                = createTree(this->mFile, pointTreeName, "Points", pointBranchName, this->mPointTreeDataArray->Class_Name(), this->mPointTreeDataArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveSMDThreshold)            this->mSMDThresholdTree         = createTree(this->mFile, smdThresholdTreeName, "SMD Threshold", smdThresholdBranchName, this->mSMDThresholdTreeDataArray->Class_Name(), this->mSMDThresholdTreeDataArray, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    {LOG_DEBUG << "Data trees created" << endm;}

    if (this->settings.saveMCGammasPlain)           this->mMCGammaTreePlain         = createTree(this->mFile, mcGammaTreePlainName, "Photons (simulated)", mcGammaBranchName, getBranchType(mcGammaBranchName), &this->mMCGammaPlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveMCPionsPlain)            this->mMCPionTreePlain          = createTree(this->mFile, mcPionTreePlainName, "Pions (simulated)", mcPionBranchName, getBranchType(mcPionBranchName), &this->mMCPionPlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveMCEtasPlain)             this->mMCEtaTreePlain           = createTree(this->mFile, mcEtaTreePlainName, "Eta (simulated)", mcEtaBranchName, getBranchType(mcEtaBranchName), &this->mMCEtaPlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveMCNbarsPlain)            this->mMCNbarTreePlain          = createTree(this->mFile, mcNbarTreePlainName, "Nbar (simulated)", mcNbarBranchName, getBranchType(mcNbarBranchName), &this->mMCNbarPlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveCandidatesPlain)         this->mCandidateTreePlain       = createTree(this->mFile, candidateTreePlainName, "Pion candidates", candidateBranchName, getBranchType(candidateBranchName), &this->mCandidatePlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveCandidatesMixedPlain)    this->mCandidateMixTreePlain    = createTree(this->mFile, candidateTreeMixPlainName, "Pion candidates (event mixing)", candidateBranchName, getBranchType(candidateBranchName), &this->mCandidateMixPlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveCandidatesSubmixedPlain) this->mCandidateSubmixTreePlain = createTree(this->mFile, candidateTreeSubmixPlainName, "Pion candidates (subevent mixing)", candidateBranchName, getBranchType(candidateBranchName), &this->mCandidateSubmixPlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveEventsPlain)             this->mEventTreePlain           = createTree(this->mFile, eventTreePlainName, "Events", eventBranchName, getBranchType(eventBranchName), &this->mEventPlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveHitsPlain)               this->mHitTreePlain             = createTree(this->mFile, hitTreePlainName, "Hits", hitBranchName, getBranchType(hitBranchName), &this->mHitPlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveClustersPlain)           this->mClusterTreePlain         = createTree(this->mFile, clusterTreePlainName, "Clusters", clusterBranchName, getBranchType(clusterBranchName), &this->mClusterPlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.savePointsPlain)             this->mPointTreePlain           = createTree(this->mFile, pointTreePlainName, "Points", pointBranchName, getBranchType(pointBranchName), &this->mPointPlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    if (this->settings.saveSMDThresholdPlain)       this->mSMDThresholdTreePlain    = createTree(this->mFile, smdThresholdTreePlainName, "SMD Threshold", smdThresholdBranchName, getBranchType(smdThresholdBranchName), &this->mSMDThresholdPlainStructure, maxVirtualSize, this->settings.basketSize, this->settings.splitLevel);
    {LOG_DEBUG << "Data trees created (plain)" << endm;}

    this->mEventCounter = 0;
    this->save();
    
    {LOG_DEBUG << "Finished Init()" << endm;}
    return result;
}

//_____________________________________________________________________________
template<class TTreeData>
void fillDataset(const StMaker *maker, TString datasetName, TClonesArray *array, Int_t &index, TTree *tree, TTreeData *plainDataStructure, TTree *treePlain, TString branchName) {
    if (((array && tree) || (treePlain && plainDataStructure)) && maker) {
	{LOG_INFO << "Reading dataset " << datasetName << endm;}
	TDataSet *dataset = maker->GetData(datasetName);
	if (dataset) {
	    TListIter iter(dataset->GetList());
	    while (const TObjectSet *objSet = dynamic_cast<const TObjectSet *>(iter())) {
		{LOG_DEBUG << "Found TObjectSet" << endm;}
		const TTreeData *treeData = dynamic_cast<const TTreeData*>(objSet->GetObject());
		if (treeData) {
		    {LOG_INFO << "Found item" << endm;}
		    if (tree && array) {
			{LOG_DEBUG << "Filling tree & array" << endm;}
			TTreeData *arrayTreeData = dynamic_cast<TTreeData*>(array->AddrAt(index));
			if (arrayTreeData) {
			    {LOG_DEBUG << "Copying item..." << endm;}
			    *arrayTreeData = *treeData;
			}
			index++;
			if (index >= array->GetEntries()) {
			    {LOG_INFO << "Fill output tree" << endm;}
    		    	    tree->SetBranchAddress(branchName, &array);
			    {LOG_DEBUG << "Branch address set" << endm;}
    			    tree->Fill();
			    {LOG_DEBUG << "Tree filled" << endm;}
    			    index = 0;
			}
		    }
		    if (treePlain && plainDataStructure) {
			{LOG_DEBUG << "Filling plain tree & structure" << endm;}
			if (treeData) {
			    *plainDataStructure = *treeData;
			    {LOG_DEBUG << "Copied item" << endm;}
			}
    		    	treePlain->SetBranchAddress(branchName, &plainDataStructure);
			{LOG_DEBUG << "Branch address set" << endm;}
			treePlain->Fill();
			{LOG_DEBUG << "Plain tree filled" << endm;}
		    }
		} else {LOG_ERROR << "Cannot fing object in TObjectSet in dataset " << datasetName << endm;}
	    }
	} else {LOG_ERROR << "Cannot find dataset " << datasetName << endm;}
    }
}

void fillHistogram(const StMaker *maker, TString histName, TH1 *histogram) {
    if (maker && histogram) {
	{LOG_INFO << "Reading dataset " << histName << endm;}
	const TObjectSet *objSet = dynamic_cast<const TObjectSet *>(maker->GetData(histName));
	if (objSet) {
	    TH1 *hist = (TH1*)(objSet->GetObject());
	    if (hist) {
		//{LOG_DEBUG << "Nbins " << histogram->GetXaxis()->GetNbins() << " " << hist->GetXaxis()->GetNbins() << " | " << eventSummaryNbins << " " << triggerSummaryNbins << endm;}
		histogram->Add(hist);
	    } else {LOG_ERROR << "Cannot find histogram " << histName << endm;}
	} else {LOG_ERROR << "Cannot find dataset " << histName << endm;}
    }
}

//_____________________________________________________________________________
Int_t StPi0DataSaveMaker::Make() {
    {LOG_DEBUG << "Starting Make()" << endm;}
    Int_t result = this->inherited::Make();
    const StMaker *maker = this;
    if (this->settings.dataMakerName != "") {
	maker = this->GetMaker(this->settings.dataMakerName);
	if (!maker) {
	    {LOG_ERROR << "Cannot find maker " << this->settings.dataMakerName << endm;}
    	    result = kStWarn;
	}
    }

    {LOG_DEBUG << "Start reading datasets" << endm;}
    fillDataset<TMyMCDecayTreeData>      (maker, mcPionDatasetName,          this->mMCPionTreeDataArray,          this->mMCPionIndex,          this->mMCPionTree,          &this->mMCPionPlainStructure,          this->mMCPionTreePlain,          mcPionBranchName);
    fillDataset<TMyMCDecayTreeData>      (maker, mcEtaDatasetName,           this->mMCEtaTreeDataArray,           this->mMCEtaIndex,           this->mMCEtaTree,           &this->mMCEtaPlainStructure,           this->mMCEtaTreePlain,           mcEtaBranchName);
    fillDataset<TMyMCParticleTreeData>   (maker, mcNbarDatasetName,          this->mMCNbarTreeDataArray,          this->mMCNbarIndex,          this->mMCNbarTree,          &this->mMCNbarPlainStructure,          this->mMCNbarTreePlain,          mcNbarBranchName);
    fillDataset<TMyMCParticleTreeData>   (maker, mcGammaDatasetName,         this->mMCGammaTreeDataArray,         this->mMCGammaIndex,         this->mMCGammaTree,         &this->mMCGammaPlainStructure,         this->mMCGammaTreePlain,         mcGammaBranchName);
    fillDataset<TMyCandidateTreeData>    (maker, candidateDatasetName,       this->mCandidateTreeDataArray,       this->mCandidateIndex,       this->mCandidateTree,       &this->mCandidatePlainStructure,       this->mCandidateTreePlain,       candidateBranchName);
    fillDataset<TMyCandidateTreeData>    (maker, candidateMixDatasetName,    this->mCandidateTreeDataMixArray,    this->mCandidateMixIndex,    this->mCandidateMixTree,    &this->mCandidateMixPlainStructure,    this->mCandidateMixTreePlain,    candidateBranchName);
    fillDataset<TMyCandidateTreeData>    (maker, candidateSubmixDatasetName, this->mCandidateTreeDataSubmixArray, this->mCandidateSubmixIndex, this->mCandidateSubmixTree, &this->mCandidateSubmixPlainStructure, this->mCandidateSubmixTreePlain, candidateBranchName);
    fillDataset<TMyEventTreeData>        (maker, eventDatasetName,           this->mEventTreeDataArray,           this->mEventIndex,           this->mEventTree,           &this->mEventPlainStructure,           this->mEventTreePlain,           eventBranchName);
    fillDataset<TMyHitTreeData>          (maker, hitDatasetName,             this->mHitTreeDataArray,             this->mHitIndex,             this->mHitTree,             &this->mHitPlainStructure,             this->mHitTreePlain,             hitBranchName);
    fillDataset<TMyClusterTreeData>      (maker, clusterDatasetName,         this->mClusterTreeDataArray,         this->mClusterIndex,         this->mClusterTree,         &this->mClusterPlainStructure,         this->mClusterTreePlain,         clusterBranchName);
    fillDataset<TMyPointTreeData>        (maker, pointDatasetName,           this->mPointTreeDataArray,           this->mPointIndex,           this->mPointTree,           &this->mPointPlainStructure,           this->mPointTreePlain,           pointBranchName);
    fillDataset<TMySMDThresholdTreeData> (maker, smdThresholdDatasetName,    this->mSMDThresholdTreeDataArray,    this->mSMDThresholdIndex,    this->mSMDThresholdTree,    &this->mSMDThresholdPlainStructure,    this->mSMDThresholdTreePlain,    smdThresholdBranchName);

    fillHistogram(maker, eventSummaryName,   this->mEventSummary);
    fillHistogram(maker, triggerSummaryName, this->mTriggerSummary);
    {LOG_DEBUG << "Finished reading datasets" << endm;}

    this->mEventCounter++;

    if ((this->settings.saveInterval > 0) && ((this->mEventCounter % this->settings.saveInterval) == 0)) this->save();

    {LOG_DEBUG << "Finished Make()" << endm;}
    return result;
}

//______________________________________________________________________________
void finishArray(TClonesArray *&array, Int_t &index, TTree *&tree, TString branchName, const TMyDataAnalysisSettings *settings) {
    if (array) {
	{LOG_INFO << "Finishing array " << array->GetName() << endm;}
	for (Int_t i = array->GetSize() - 1;i >= index;i--) array->RemoveAt(i);
	array->Compress();
	if (tree && (index > 0)) {
	    tree->SetBranchAddress(branchName, &array);
	    tree->Fill();
	}
    }
    if (tree && settings) {
	TList *list = tree->GetUserInfo();
	if (list) {
	    TMyDataAnalysisSettings *settingsNew = new TMyDataAnalysisSettings();
	    if (settingsNew) {
		(*settingsNew) = (*settings);
		list->Add(settingsNew);
	    }
	}
    }
    array = 0;
    tree = 0;
    index = 0;
}

//______________________________________________________________________________
Int_t StPi0DataSaveMaker::Finish() {
    {LOG_DEBUG << "Starting Finish()" << endm;}
    Int_t result = this->inherited::Finish();

    finishArray(this->mMCGammaTreeDataArray,         this->mMCGammaIndex,         this->mMCGammaTree,         mcGammaBranchName,      &this->settings);
    finishArray(this->mMCPionTreeDataArray,          this->mMCPionIndex,          this->mMCPionTree,          mcPionBranchName,       &this->settings);
    finishArray(this->mMCEtaTreeDataArray,           this->mMCEtaIndex,           this->mMCEtaTree,           mcEtaBranchName,        &this->settings);
    finishArray(this->mMCNbarTreeDataArray,          this->mMCNbarIndex,          this->mMCNbarTree,          mcNbarBranchName,       &this->settings);
    finishArray(this->mCandidateTreeDataArray,       this->mCandidateIndex,       this->mCandidateTree,       candidateBranchName,    &this->settings);
    finishArray(this->mCandidateTreeDataMixArray,    this->mCandidateMixIndex,    this->mCandidateMixTree,    candidateBranchName,    &this->settings);
    finishArray(this->mCandidateTreeDataSubmixArray, this->mCandidateSubmixIndex, this->mCandidateSubmixTree, candidateBranchName,    &this->settings);
    finishArray(this->mEventTreeDataArray,           this->mEventIndex,           this->mEventTree,           eventBranchName,        &this->settings);
    finishArray(this->mHitTreeDataArray,             this->mHitIndex,             this->mHitTree,             hitBranchName,          &this->settings);
    finishArray(this->mClusterTreeDataArray,         this->mClusterIndex,         this->mClusterTree,         clusterBranchName,      &this->settings);
    finishArray(this->mPointTreeDataArray,           this->mPointIndex,           this->mPointTree,           pointBranchName,        &this->settings);
    finishArray(this->mSMDThresholdTreeDataArray,    this->mSMDThresholdIndex,    this->mSMDThresholdTree,    smdThresholdBranchName, &this->settings);
    {LOG_DEBUG << "Finished arrays and trees" << endm;}

    this->mEventSummary = 0;
    this->mTriggerSummary = 0;

    {LOG_DEBUG << "Saving file" << endm;}
    this->save();

    if (this->mFile) {
	{LOG_DEBUG << "Closing file" << endm;}
	this->mFile->Close();
	delete this->mFile;
	this->mFile = 0;
    }
    
    {LOG_DEBUG << "Finished Finish()" << endm;}
    return result;
}

//___________________________________________________________________________
void StPi0DataSaveMaker::save() {
    if (this->mFile) {
        this->mFile->Write(0, TObject::kOverwrite);
	{LOG_INFO << "TFile " << this->mFile->GetName() << " IsOpen = " << (Int_t)this->mFile->IsOpen() << ": updated on disk." << endm;}
    } else {LOG_WARN << "Cannot save: no mFile" << endm;}
}
