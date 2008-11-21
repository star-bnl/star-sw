#include "StPi0DataStructures.h"

#include <TFile.h>
#include <TError.h>
#include <TH1F.h>
#include <TTree.h>

#define DEFINE_DATA_STRUCTURES
#define DATASTRUCTURE_BEGIN(CLASS) \
ClassImp(CLASS); \
CLASS::CLASS(const this_type &data) \
{ \
    *this = data; \
} \
void CLASS::Copy(void *objTo) const { \
    this_type *tobjTo = reinterpret_cast<this_type*>(objTo); \
    if (tobjTo) (*tobjTo) = (*this); \
}
#define DATASTRUCTURE_TREE_BEGIN(CLASS) \
ClassImp(CLASS); \
CLASS::CLASS(const this_type &data) \
    : inherited(data) \
{ \
    *this = data; \
} \
void CLASS::Copy(TObject &objTo) const { \
    this_type &tobjTo = dynamic_cast<this_type&>(objTo); \
    tobjTo = (*this); \
}
#define DATA_DEF(TYPE, NAME, TITLE)
#define DATASTRUCTURE_END(CLASS, VALID_CONDITION) \
Bool_t CLASS::isValid() const { \
    return (VALID_CONDITION); \
}
#define DATASTRUCTURE_TREE_END(CLASS, VALID_CONDITION) \
Bool_t CLASS::isValid() const { \
    return (VALID_CONDITION); \
}
#include "StPi0DataStructures.h"
#undef DEFINE_DATA_STRUCTURES
#undef DATASTRUCTURE_BEGIN
#undef DATASTRUCTURE_TREE_BEGIN
#undef DATA_DEF
#undef DATASTRUCTURE_END
#undef DATASTRUCTURE_TREE_END

#define DEFINE_DATA_STRUCTURES
#define DATASTRUCTURE_BEGIN(CLASS) \
CLASS::CLASS(Int_t i) \
{
#define DATASTRUCTURE_TREE_BEGIN(CLASS) \
CLASS::CLASS(Int_t i) \
    : inherited() \
{
#define DATA_DEF(TYPE, NAME, TITLE) \
    this->NAME = (TYPE)i;
#define DATASTRUCTURE_END(CLASS, VALID_CONDITION) \
}
#define DATASTRUCTURE_TREE_END(CLASS, VALID_CONDITION) \
}
#include "StPi0DataStructures.h"
#undef DEFINE_DATA_STRUCTURES
#undef DATASTRUCTURE_BEGIN
#undef DATASTRUCTURE_TREE_BEGIN
#undef DATA_DEF
#undef DATASTRUCTURE_END
#undef DATASTRUCTURE_TREE_END

#define DEFINE_DATA_STRUCTURES
#define DATASTRUCTURE_BEGIN(CLASS) \
CLASS::~CLASS() {
#define DATASTRUCTURE_TREE_BEGIN(CLASS) \
CLASS::~CLASS() {
#define DATA_DEF(TYPE, NAME, TITLE) \
    this->NAME = (TYPE)0;
#define DATASTRUCTURE_END(CLASS, VALID_CONDITION) \
}
#define DATASTRUCTURE_TREE_END(CLASS, VALID_CONDITION) \
}
#include "StPi0DataStructures.h"
#undef DEFINE_DATA_STRUCTURES
#undef DATASTRUCTURE_BEGIN
#undef DATASTRUCTURE_TREE_BEGIN
#undef DATA_DEF
#undef DATASTRUCTURE_END
#undef DATASTRUCTURE_TREE_END

#define DEFINE_DATA_STRUCTURES
#define DATASTRUCTURE_BEGIN(CLASS) \
CLASS::this_type &CLASS::operator=(const this_type &data) {
#define DATASTRUCTURE_TREE_BEGIN(CLASS) \
CLASS::this_type &CLASS::operator=(const this_type &data) {
#define DATA_DEF(TYPE, NAME, TITLE) \
    this->NAME = data.NAME;
#define DATASTRUCTURE_END(CLASS, VALID_CONDITION) \
    return *this; \
}
#define DATASTRUCTURE_TREE_END(CLASS, VALID_CONDITION) \
    return *this; \
}
#include "StPi0DataStructures.h"
#undef DEFINE_DATA_STRUCTURES
#undef DATASTRUCTURE_BEGIN
#undef DATASTRUCTURE_TREE_BEGIN
#undef DATA_DEF
#undef DATASTRUCTURE_END
#undef DATASTRUCTURE_TREE_END

//-------------------------------------------

const Char_t *mcGammaTreeName = "mcGammasTree";
//const Char_t *mcGammaTreePlainName = "mcGammasTreePlain";
const Char_t *mcGammaTreePlainName = mcGammaTreeName;
const Char_t *mcGammaBranchName = "mcGammasBranch";
const Char_t *mcGammaDatasetName = "mcGammaDataset";
const Char_t *mcGammaBranchType = "TMyMCParticleTreeData";

const Char_t *mcPionTreeName = "mcPionsTree";
//const Char_t *mcPionTreePlainName = "mcPionsTreePlain";
const Char_t *mcPionTreePlainName = mcPionTreeName;
const Char_t *mcPionBranchName = "mcPionBranch";
const Char_t *mcPionDatasetName = "mcPionDataset";
const Char_t *mcPionBranchType = "TMyMCDecayTreeData";

const Char_t *mcEtaTreeName = "mcEtaTree";
//const Char_t *mcEtaTreePlainName = "mcEtaTreePlain";
const Char_t *mcEtaTreePlainName = mcEtaTreeName;
const Char_t *mcEtaBranchName = "mcEtaBranch";
const Char_t *mcEtaDatasetName = "mcEtaDataset";
const Char_t *mcEtaBranchType = "TMyMCDecayTreeData";

const Char_t *mcNbarTreeName = "mcNbarTree";
//const Char_t *mcNbarTreePlainName = "mcNbarTreePlain";
const Char_t *mcNbarTreePlainName = mcNbarTreeName;
const Char_t *mcNbarBranchName = "mcNbarBranch";
const Char_t *mcNbarDatasetName = "mcNbarDataset";
const Char_t *mcNbarBranchType = "TMyMCParticleTreeData";

const Char_t *candidateTreeName = "candidatesTree";
//const Char_t *candidateTreePlainName = "candidatesTreePlain";
const Char_t *candidateTreePlainName = candidateTreeName;
const Char_t *candidateDatasetName = "candidatesDataset";
const Char_t *candidateBranchName = "candidateBranch";
const Char_t *candidateBranchType = "TMyCandidateTreeData";

const Char_t *candidateTreeMixName = "candidatesTreeMix";
//const Char_t *candidateTreeMixPlainName = "candidatesTreeMixPlain";
const Char_t *candidateTreeMixPlainName = candidateTreeMixName;
const Char_t *candidateMixDatasetName = "candidatesMixDataset";

const Char_t *candidateTreeSubmixName = "candidatesTreeSubmix";
//const Char_t *candidateTreeSubmixPlainName = "candidatesTreeSubmixPlain";
const Char_t *candidateTreeSubmixPlainName = candidateTreeSubmixName;
const Char_t *candidateSubmixDatasetName = "candidatesSubmixDataset";

const Char_t *eventTreeName = "eventsTree";
//const Char_t *eventTreePlainName = "eventsTreePlain";
const Char_t *eventTreePlainName = eventTreeName;
const Char_t *eventBranchName = "eventBranch";
const Char_t *eventDatasetName = "eventDataset";
const Char_t *eventBranchType = "TMyEventTreeData";

const Char_t *pointTreeName = "pointsTree";
//const Char_t *pointTreePlainName = "pointsTreePlain";
const Char_t *pointTreePlainName = pointTreeName;
const Char_t *pointBranchName = "pointBranch";
const Char_t *pointDatasetName = "pointDataset";
const Char_t *pointBranchType = "TMyPointTreeData";

const Char_t *clusterTreeName = "clustersTree";
//const Char_t *clusterTreePlainName = "clustersTreePlain";
const Char_t *clusterTreePlainName = clusterTreeName;
const Char_t *clusterBranchName = "clusterBranch";
const Char_t *clusterDatasetName = "clusterDataset";
const Char_t *clusterBranchType = "TMyClusterTreeData";

const Char_t *hitTreeName = "hitsTree";
//const Char_t *hitTreePlainName = "hitsTreePlain";
const Char_t *hitTreePlainName = hitTreeName;
const Char_t *hitBranchName = "hitBranch";
const Char_t *hitDatasetName = "hitDataset";
const Char_t *hitBranchType = "TMyHitTreeData";

const Char_t *smdThresholdTreeName = "smdThresholdTree";
//const Char_t *smdThresholdTreePlainName = "smdThresholdTreePlain";
const Char_t *smdThresholdTreePlainName = smdThresholdTreeName;
const Char_t *smdThresholdBranchName = "smdThresholdBranch";
const Char_t *smdThresholdDatasetName = "smdThresholdDataset";
const Char_t *smdThresholdBranchType = "TMySMDThresholdTreeData";

const Char_t *triggerSummaryName = "triggerSummary";
const Int_t triggerSummaryNbins = (sizeof(triggered_type)*8) + 1;
const Float_t triggerSummaryMin = -0.5;
const Float_t triggerSummaryMax = (sizeof(triggered_type)*8) + 1 - 0.5;

const Char_t *eventSummaryName = "eventSummary";
const Int_t eventSummaryNbins = 10;
const Float_t eventSummaryMin = -0.5;
const Float_t eventSummaryMax = 9.5;

//-------------------------------------------
const Char_t *getBranchType(const Char_t *name) {
    const Char_t *type = 0;
#define checkBrName(brName, brType) if (name && (strcmp(name, brName) == 0)) type = brType;
    checkBrName(mcGammaBranchName, mcGammaBranchType);
    checkBrName(mcPionBranchName, mcPionBranchType);
    checkBrName(mcEtaBranchName, mcEtaBranchType);
    checkBrName(mcNbarBranchName, mcNbarBranchType);
    checkBrName(candidateBranchName, candidateBranchType);
    checkBrName(eventBranchName, eventBranchType);
    checkBrName(pointBranchName, pointBranchType);
    checkBrName(clusterBranchName, clusterBranchType);
    checkBrName(hitBranchName, hitBranchType);
    checkBrName(smdThresholdBranchName, smdThresholdBranchType);
    return type;
}

//-------------------------------------------
TTree *createTree(TFile *file, const Char_t *name, const Char_t *title, const Char_t *branch, const Char_t *type, void *address, const Int_t maxVirtualSize, const Int_t basketSize, const Int_t splitLevel) {
    if (0) Info(__FILE__, "Start creating tree %s \"%s\" with branch %s", name, title, branch);
    TTree *tree = file ? (TTree*)file->Get(name) : 0;
    if (tree) {
        if (0) Info(__FILE__, "\tFound in file %s", file->GetName());
        tree->SetBranchAddress(branch, &address);
        if (0) Info(__FILE__, "\tSet branch address");
    } else {
        if (type) {
    	    if (0) Info(__FILE__, "\tCreating tree");
    	    tree = new TTree(name, title);
        } else {
    	    Error(__FILE__, "Unknown branch name %s when creating tree %s \"%s\"", branch, name, title);
        }
        if (tree) {
            if (0) Info(__FILE__, "\tSetting directory");
            tree->SetDirectory(file);
            if (0) Info(__FILE__, "\tCreating branch");
            tree->Branch(branch, type, &address, basketSize, splitLevel);
            if (0) Info(__FILE__, "\tDone");
        } else {
            Error(__FILE__, "\tTree %s \"%s\" NOT created!", name, title);
        }
    }
    if (0) Info(__FILE__, "\tSetting max virtual size");
    if (tree && (maxVirtualSize > 0)) tree->SetMaxVirtualSize(maxVirtualSize);
    if (0) Info(__FILE__, "\tFinished tree.");
    return tree;
}

//-------------------------------------------
TH1F *createH1F(TFile *file, const Char_t *name, const Char_t *title, Int_t nbins, Float_t min, Float_t max) {
    if (0) Info(__FILE__, "Start creating histogram %s \"%s\"", name, title);
    TH1F *h = file ? (TH1F*)file->Get(name) : 0;
    if (h) {
	if (0) Info(__FILE__, "\tFound in file %s", file->GetName());
    } else {
	if (0) Info(__FILE__, "\tCreating histogram");
	h = new TH1F(name, title, nbins, min, max);
	if (h) {
            if (0) Info(__FILE__, "\tSetting directory");
    	    h->SetDirectory(file);
    	    if (0) Info(__FILE__, "\tCreated");
	} else {
    	    Error(__FILE__, "\tHistogram %s \"%s\" NOT created!", name, title);
	}
    }
    if (0) Info(__FILE__, "Finished histogram.");
    return h;
}
