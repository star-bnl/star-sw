//StPythiaAssociator.cxx
//M.L. Miller (MIT)
//02/05

//std
#include <map>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
using namespace std;


//StEmc
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"

//root
#include "TTree.h"
#include "TFriendElement.h"
#include "TFile.h"

//StMuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

//StJetMaker
#include "StJetMaker/StJet.h"
#include "StJetMaker/StJets.h"
#include "StJetMaker/StJetReader.h"
#include "StJetMaker/StJetSimuUtil/IoManager.h"
#include "StJetMaker/StJetSimuUtil/StPythiaAssociator.h"

ClassImp(StPythiaAssociator)

    StPythiaAssociator::StPythiaAssociator(const char* name, const char* fname, StJetReader* jreader)
	: StMaker(name), mFilename(fname), mReader(jreader), mIoManager(new IoManager())
{
    cout <<"StPythiaAssociator::StPythiaAssociator()"<<endl;
}

StPythiaAssociator::~StPythiaAssociator()
{
    cout <<"StPythiaAssociator::~StPythiaAssociator()"<<endl;
}

Int_t StPythiaAssociator::Init()
{
    cout <<"StPythiaAssociator::InitFile()"<<endl;
    mIoManager->openWrite(mFilename.c_str());

    return StMaker::Init();
}

Int_t StPythiaAssociator::Make()
{

    //cleanup the TTree's
    TClonesArray& pairArray = *(mIoManager->assocArray()->mArray);
    TClonesArray& recoArray = *(mIoManager->recoArray()->mArray);
    TClonesArray& pythiaArray = *(mIoManager->pythiaArray()->mArray);
    pairArray.Clear();
    recoArray.Clear();
    pythiaArray.Clear();
    
    //get pointer to pythia StJets object:
    JetBranchesMap& mStJetsMap = mReader->jetsMap();
    
    JetBranchesMap::iterator where1 = mStJetsMap.find("PythiaKtJet");
    assert(where1!=mStJetsMap.end());
    StJets* pythiaJets = (*where1).second;
    assert(pythiaJets);
    
    //get pointer to reco StJets object:
    JetBranchesMap::iterator where2 = mStJetsMap.find("KtJet");
    assert(where2!=mStJetsMap.end());
    StJets* recoJets = (*where2).second;
    assert(recoJets);

    int nPyJets = pythiaJets->nJets();
    int nRecoJets = recoJets->nJets();

    TClonesArray& pyjets = *(pythiaJets->jets());
    TClonesArray& rcjets = *(recoJets->jets());

    typedef vector<StJet*> JetVec;
    JetVec pyjetvec;
    for (int i=0; i<nPyJets; ++i) {
	StJet* pyjet = static_cast<StJet*>( pyjets[i] );
	pyjetvec.push_back(pyjet);

	//hang these on a Tree
	int addAt = pythiaArray.GetLast()+1;
	new ( pythiaArray[addAt]) StJet( *pyjet );
    }

    for (int i=0; i<nRecoJets; ++i) {
	StJet* rcjet = static_cast<StJet*>(rcjets[i]);
	int addAt = recoArray.GetLast()+1;
	new ( recoArray[addAt]) StJet( *rcjet );
    }
    
    std::sort(pyjetvec.begin(), pyjetvec.end(), PtSorter());

    //ok, now we have our Pythia jets sorted in decreasing pt

    //make pairs
    vector<JetPair> finalJetPairs;
    vector<JetPair> workPairs;

    for (JetVec::iterator it=pyjetvec.begin(); it!=pyjetvec.end(); ++it) {
	StJet* pyjet = *it;
	
	
	for (int i=0; i<nRecoJets; ++i) {
	    StJet* recojet = static_cast<StJet*>( rcjets[i] );
	    
	    JetPair jp;
	    jp.pythiaRank = it - pyjetvec.begin();
	    jp.pythiaJet = *pyjet;
	    jp.recoJet = *recojet;
	    jp.deltaPhi = gJetDeltaPhi(pyjet, recojet);
	    jp.deltaEta = pyjet->Eta() - recojet->Eta();
	    jp.deltaR = sqrt(jp.deltaPhi*jp.deltaPhi + jp.deltaEta*jp.deltaEta);
	    workPairs.push_back(jp);
	}
	std::sort(workPairs.begin(), workPairs.end(), JetPairSorter());

	//now tag, store for later
	for (vector<JetPair>::iterator it2=workPairs.begin(); it2!=workPairs.end(); ++it2) {
	    JetPair& jp = *it2;
	    jp.pairRank = it2 - workPairs.begin();
	    finalJetPairs.push_back(jp);
	}
	//and cleanup for next round:
	workPairs.clear();

    }

    //now, stream for fun:
    //cout <<"\n Jet Pair Vec -----------------------------\n"<<endl;

    for (vector<JetPair>::iterator it=finalJetPairs.begin(); it!=finalJetPairs.end(); ++it) {
	//cout <<"\nNew Pair\n"<<(*it)<<endl;
	int addAt = pairArray.GetLast()+1;
	new ( pairArray[addAt]) JetPair( (*it) );
    }

    mIoManager->recoTree()->Fill();
    mIoManager->pythiaTree()->Fill();
    mIoManager->assocTree()->Fill();
    
    return StMaker::Make();
}

Int_t StPythiaAssociator::Finish()
{
    mIoManager->write();
    return StMaker::Finish();
}

