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
// Revision 1.38  2003/11/20 22:58:10  calderon
// Move inline functions to header file.
// Initialize minimcevent pointer to zero in constructor of StiEvaluator.  Otherwise
// we can't set the branch address properly and the code breaks.
//
// Revision 1.37  2003/09/02 17:59:47  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.36  2003/07/09 01:08:07  calderon
// Changes to access StMiniMcEvent through methods, rather than data members
//
// Revision 1.35  2003/06/19 21:17:07  calderon
// Check the file name (for debugging)
// Set the suffix before call to create histograms for the inclusive case.
//
// Revision 1.34  2003/06/19 01:37:03  calderon
// Adding all the histograms from Zbigniew.
//
// Revision 1.33  2002/11/27 00:08:55  calderon
// New version of evaluator using the minimctrees
//

#include "Stiostream.h"
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
    minimcevent(0),
    mFitPtsLimit(99),
    mDcaLimit(99),
    mGeantId(9999),
    mEtaMinimum(-10),
    mEtaMaximum(10),
    mPullType(0),
    mFileName(0),
    mChain(0)
{
}
StiEvaluator::StiEvaluator(const StiEvaluator& e) :
    mFitPtsLimit(e.mFitPtsLimit),
    mDcaLimit(e.mDcaLimit),
    mGeantId(e.mGeantId),
    mEtaMinimum(e.mEtaMinimum),
    mEtaMaximum(e.mEtaMaximum),
    mPullType(e.mPullType),
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
    AnalysisPiPlHi.setEtaRange(mEtaMinimum,mEtaMaximum);
    AnalysisPiPlHi.setPullType(mPullType);
    AnalysisPiPlHi.setGeantId(8);   
    AnalysisPiPlHi.setFileName(mFileName); // should already be figured out in the macro

    cout << "File Name " << mFileName << endl;
    cout << "cuts " << endl;
    cout << "fit points >= " << mFitPtsLimit << endl; 
    cout << "global dca <= " << mDcaLimit    << endl; 
    cout << "eta limits    " << mEtaMinimum << " - " << mEtaMaximum << endl;
    cout << "pull (tpt/it) " << mPullType   << endl;
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

    // inclusive
    EfficiencyAnalysis AnalysisAllHi(AnalysisPiPlHi);
    AnalysisAllHi.setGeantId(9999);   
    EfficiencyAnalysis AnalysisAllMe(AnalysisAllHi);
    EfficiencyAnalysis AnalysisAllLo(AnalysisAllHi);
    
    
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
    mEfficiencyAnalysisVector.push_back(AnalysisAllHi);  // index 18
    mEfficiencyAnalysisVector.push_back(AnalysisAllMe);  // index 19
    mEfficiencyAnalysisVector.push_back(AnalysisAllLo);  // index 20

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
    mEfficiencyAnalysisVector[18].setSuffix("HiAll");
    mEfficiencyAnalysisVector[19].setSuffix("MeAll");
    mEfficiencyAnalysisVector[20].setSuffix("LoAll");

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
	size_t mcMultiplicity = (size_t) minimcevent->nMcTrack();
	
	if (-50. < minimcevent->vertexZ() && minimcevent->vertexZ() < 50.) {

  	    cout << "Getting Event " << i << ", z vertex " << minimcevent->vertexZ() << ", mc mult " << mcMultiplicity << endl;
 	    
	    size_t index = getIndex(mcMultiplicity);
	    mEfficiencyAnalysisVector[index].fillHistograms(minimcevent);
	    mEfficiencyAnalysisVector[index+3].fillHistograms(minimcevent);
	    mEfficiencyAnalysisVector[index+6].fillHistograms(minimcevent);
	    mEfficiencyAnalysisVector[index+9].fillHistograms(minimcevent);
	    mEfficiencyAnalysisVector[index+12].fillHistograms(minimcevent);
	    mEfficiencyAnalysisVector[index+15].fillHistograms(minimcevent);
            mEfficiencyAnalysisVector[index+18].fillHistograms(minimcevent);
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

void StiEvaluator::setEtaRange(double val1, double val2) 
{
  mEtaMinimum = val1;
  mEtaMaximum = val2;
}

void StiEvaluator::setPullType(bool val) { mPullType = val;}

void StiEvaluator::setFileName(char* val) { mFileName = val;}


