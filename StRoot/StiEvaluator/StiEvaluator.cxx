// $Id StiEvaluator.cxx$
// A. Rose (WSU)
// M. Calderon de la Barca (BNL)
// 8/01
// Modifications Sep 2002
// Use EfficiencyAnalysis class based on usage of StMiniMcTree
// do only 3 multiplicity classes, but will need to run for 3 particle id's
// and both charge signs
// 
// $Log: StiEvaluator.cxx,v $
// Revision 1.33  2002/11/27 00:08:55  calderon
// New version of evaluator using the minimctrees
//

#include <iostream>
#include <string>
#include "StiEvaluator.h"
#include "TChain.h"
#include "TBranch.h"
#include "StMiniMcEvent/StMiniMcEvent.h"

ClassImp(StiEvaluator)
    
StiEvaluator::StiEvaluator() :
    // These values should be given in the macro, so set
    // the default values to something unusable, following the golden
    // rule: setting default values to something sensible is akin to sabotage.
    mFitPtsLimit(99),
    mDcaLimit(99),
    mGeantId(9999),
    mFileName(0),
    mChain(0)
{
}
StiEvaluator::StiEvaluator(const StiEvaluator& e) :
    mFitPtsLimit(e.mFitPtsLimit),
    mDcaLimit(e.mDcaLimit),
    mGeantId(e.mGeantId),
    mFileName(e.mFileName),
    mChain(e.mChain)
{
}

StiEvaluator::~StiEvaluator() {
}

int StiEvaluator::initialize() {
    cout << "StiEvaluator::initialize()" << endl;
    EfficiencyAnalysis AnalysisPiPlHi;

    AnalysisPiPlHi.setFitPtsLimit(mFitPtsLimit); //cut is >=
    AnalysisPiPlHi.setDcaLimit(mDcaLimit);
    AnalysisPiPlHi.setGeantId(8);   
    AnalysisPiPlHi.setFileName(mFileName); // should already be figured out in the macro

    // copy construct so that the track cuts, and file name are identical
    cout << "made efficiency analysis, now make its copies and set their geant id's" << endl;
    EfficiencyAnalysis AnalysisPiPlMe(AnalysisPiPlHi);
    EfficiencyAnalysis AnalysisPiPlLo(AnalysisPiPlHi);

    //
    // same for pi-
    //
    EfficiencyAnalysis AnalysisPiMiHi(AnalysisPiPlHi);
    AnalysisPiMiHi.setGeantId(9);
    EfficiencyAnalysis AnalysisPiMiMe(AnalysisPiMiHi);
    EfficiencyAnalysis AnalysisPiMiLo(AnalysisPiMiHi);

    //
    // same for k+, k-
    //
    EfficiencyAnalysis AnalysisKaPlHi(AnalysisPiPlHi);
    AnalysisKaPlHi.setGeantId(11);
    EfficiencyAnalysis AnalysisKaPlMe(AnalysisKaPlHi);
    EfficiencyAnalysis AnalysisKaPlLo(AnalysisKaPlHi);

    EfficiencyAnalysis AnalysisKaMiHi(AnalysisPiPlHi);
    AnalysisKaMiHi.setGeantId(12);
    EfficiencyAnalysis AnalysisKaMiMe(AnalysisKaMiHi);
    EfficiencyAnalysis AnalysisKaMiLo(AnalysisKaMiHi);

    //
    // same for p, pbar
    //
    EfficiencyAnalysis AnalysisPrPlHi(AnalysisPiPlHi);
    AnalysisPrPlHi.setGeantId(14);
    EfficiencyAnalysis AnalysisPrPlMe(AnalysisPrPlHi);
    EfficiencyAnalysis AnalysisPrPlLo(AnalysisPrPlHi);

    EfficiencyAnalysis AnalysisPrMiHi(AnalysisPiPlHi);
    AnalysisPrMiHi.setGeantId(15);
    EfficiencyAnalysis AnalysisPrMiMe(AnalysisPrMiHi);
    EfficiencyAnalysis AnalysisPrMiLo(AnalysisPrMiHi);

    
    
    mEfficiencyAnalysisVector.push_back(AnalysisPiPlHi); // index 0
    mEfficiencyAnalysisVector.push_back(AnalysisPiPlMe); // index 1
    mEfficiencyAnalysisVector.push_back(AnalysisPiPlLo); // index 2
    mEfficiencyAnalysisVector.push_back(AnalysisPiMiHi); // index 3
    mEfficiencyAnalysisVector.push_back(AnalysisPiMiMe); // index 4
    mEfficiencyAnalysisVector.push_back(AnalysisPiMiLo); // index 5
    mEfficiencyAnalysisVector.push_back(AnalysisKaPlHi); // index 6
    mEfficiencyAnalysisVector.push_back(AnalysisKaPlMe); // index 7
    mEfficiencyAnalysisVector.push_back(AnalysisKaPlLo); // index 8
    mEfficiencyAnalysisVector.push_back(AnalysisKaMiHi); // index 9
    mEfficiencyAnalysisVector.push_back(AnalysisKaMiMe); // index 10
    mEfficiencyAnalysisVector.push_back(AnalysisKaMiLo); // index 11
    mEfficiencyAnalysisVector.push_back(AnalysisPrPlHi); // index 12
    mEfficiencyAnalysisVector.push_back(AnalysisPrPlMe); // index 13
    mEfficiencyAnalysisVector.push_back(AnalysisPrPlLo); // index 14
    mEfficiencyAnalysisVector.push_back(AnalysisPrMiHi); // index 15
    mEfficiencyAnalysisVector.push_back(AnalysisPrMiMe); // index 16
    mEfficiencyAnalysisVector.push_back(AnalysisPrMiLo); // index 17

    // the name of output histogram should be different though
    cout << "Figure out output names " << endl;
    string suffixes[3] = {"Hi", "Me", "Lo"};
    int geantIds[6] = {8,9,11,12,14,15};
    char gid[3]= "xx";
    string suffix;
    for (int g=0; g<6; ++g) 
	for (int s=0; s<3; ++s) {
	    sprintf(gid,"%02i",geantIds[g]);
	    suffix = suffixes[s] + gid;
	    mEfficiencyAnalysisVector[g*3+s].setSuffix(suffix);
	}
    cout << "booking histos" << mGeantId << endl;
    for (size_t i=0; i<mEfficiencyAnalysisVector.size(); ++i) {
	cout << mEfficiencyAnalysisVector[i].suffix() << endl;
	mEfficiencyAnalysisVector[i].createHistograms();
    }

    return 0;
}


size_t StiEvaluator::getIndex(size_t mult) {
    //
    // return an index into the vector of multipliticy classes
    // based on some multiplicity.
    // For StiEvaluation, use the MC multiplicity
    // in order to compare old and new trackers using the same 
    // number

    if (mult >= 4000) return 0;
    else if (mult >= 2000) return 1;
    else return 2;
}

void StiEvaluator::resethistograms() {
    cout << "StiEvaluator::resethistograms()" << endl;
    for (size_t i=0; i<mEfficiencyAnalysisVector.size(); ++i) {
	mEfficiencyAnalysisVector[i].resetHistograms();
    }
}

void StiEvaluator::makehistograms() {
    // makehistograms() method
    cout << "StiEvaluator::makehistograms" << endl;
    resethistograms();
    
    mChain->SetBranchAddress("StMiniMcEvent",&minimcevent);
    
    // now make a pointer to the branch, in this case a simple data member,
    // where the raw negative multiplicity is stored
    // and also make a pointer to the full event in our tree structure...
    TBranch* btree        = mChain->GetBranch("StMiniMcEvent");
//     TBranch* bNUncNegPrim = mChain->GetBranch("mNUncorrectedNegativePrimaries");
//     TBranch* bZVertex     = mChain->GetBranch("mVertexZ");

    int nEvents = (int) mChain->GetEntries();
    cout << "# events " << nEvents << endl;
    for (int i=0; i<nEvents; ++i) {
	// event loop
	// read the multiplicity and zvertex branches only first

	//bNUncNegPrim->GetEntry(i);
	//bZVertex->GetEntry(i);
	btree->GetEntry(i);
	size_t mcMultiplicity = (size_t) minimcevent->mNMcTrack;
	
	if (-25. < minimcevent->mVertexZ && minimcevent->mVertexZ < 25.) {

  	    cout << "Getting Event " << i << ", z vertex " << minimcevent->mVertexZ << ", mc mult " << mcMultiplicity << endl;
 	    
	    size_t index = getIndex(mcMultiplicity);
	    mEfficiencyAnalysisVector[index].fillHistograms(minimcevent);
	    mEfficiencyAnalysisVector[index+3].fillHistograms(minimcevent);
	    mEfficiencyAnalysisVector[index+6].fillHistograms(minimcevent);
	    mEfficiencyAnalysisVector[index+9].fillHistograms(minimcevent);
	    mEfficiencyAnalysisVector[index+12].fillHistograms(minimcevent);
	    mEfficiencyAnalysisVector[index+15].fillHistograms(minimcevent);
	}
    }
    return;
}

void StiEvaluator::writehistograms() {
    // write all histograms to appropriate file
    cout << "Writing  " << mFileName << endl;
    for (size_t i=0; i<mEfficiencyAnalysisVector.size();++i)
	mEfficiencyAnalysisVector[i].writeHistograms();
    return;
}

void StiEvaluator::setChain(TChain* val) { mChain = val;}

void StiEvaluator::setFitPtsLimit(double val) { mFitPtsLimit = val;}

void StiEvaluator::setDcaLimit(double val) { mDcaLimit = val;}

void StiEvaluator::setGeantId(int val) { mGeantId = val;}

void StiEvaluator::setFileName(char* val) { mFileName = val;}


