//M.L. Miller
//MIT Software
//6/04

//std
#include <map>
#include <string>
#include <algorithm>
#include <iostream>


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
#include "StJetMaker/StJetSkimEvent.h"

ClassImp(StJetReader)

double gDeltaPhi(double p1, double p2)
{
    float dp = p1 - p2;
    while(dp >  M_PI) {dp -= 2.0 * M_PI;}
    while(dp < -1.*M_PI) {dp += 2.0 * M_PI;}
    return dp;
};

double gDeltaR(const TLorentzVector* jet, const StThreeVectorF& track)
{
    double dEta = jet->Eta() - track.pseudoRapidity();
    double dPhi = gDeltaPhi(jet->Phi(), track.phi() );
    return sqrt(dEta*dEta + dPhi*dPhi);
}

StJetReader::StJetReader(const char* name, StMuDstMaker* uDstMaker)
: StMaker(name), mFile(0), mTree(0), mSkimFile(0), mSkimTree(0), 
mDstMaker(uDstMaker), mCounter(0), mValid(false), mSkimEvent(0), mOfstream(0)
{
    cout <<"StJetReader::StJetReader()"<<endl;
}

StJetReader::~StJetReader()
{
    cout <<"StJetReader::~StJetReader()"<<endl;
}

Int_t StJetReader::Init()
{
    return kStOk;
}

void StJetReader::InitFile(const char* file)
{
    cout <<"StJetReader::InitFile()"<<endl;
	
    cout <<"open file:\t"<<file<<"\tfor reading"<<endl;
    mFile = new TFile(file,"READ");
    assert(mFile);
	
    cout <<"recover tree"<<endl;
    TObject* tree = mFile->Get("jet");
    TTree* t = dynamic_cast<TTree*>(tree);
    assert(t);
    mTree = t;
	
    cout <<"\tset tree pointer"<<endl;
    cout <<"Number of entries in tree:\t"<<t->GetEntries();
    
    cout <<"\tGet Branches"<<endl;
    TObjArray* branches = t->GetListOfBranches();
    if (!branches) {cout <<"StJetReader::InitFile().  Null branches"<<endl; abort();}
	
    cout <<"\tLoop on branches"<<endl;
    
    for (int i=0; i<branches->GetLast()+1; ++i) {
		TBranch* branch = dynamic_cast<TBranch*>((*branches)[i]);
		if (!branch) {cout <<"StJetReader::InitFile().  Null branch"<<endl; abort();}
		string bname( branch->GetName() );
		cout <<"\t--- Found branch:\t"<<bname<<endl;
		
		if ( (bname.find("jet")!=bname.npos) || (bname.find("Jet")!=bname.npos) ) {
			cout <<"\t\tcreate StJets object for branch:\t"<<bname<<endl;
			
			//create StJets object here, put in map:
			StJets* jets = new StJets();
			jets->Clear();
			mStJetsMap[bname] = jets;
			cout <<"\t\tset branch address for branch:\t"<<bname.c_str()<<endl;
			t->SetBranchStatus(bname.c_str(), 1);
			t->SetBranchAddress(bname.c_str(), &jets);
		}
    }
	
    if (0) {
		string jetCheck(file);
		jetCheck += ".read.txt";
		mOfstream = new ofstream(jetCheck.c_str());
    }
    
    cout <<"\tfinished!"<<endl;
	
    return ;
}

void StJetReader::InitJetSkimFile(const char* file)
{
	cout <<" StJetReader::InitJetSkimFile(const char* file)"<<endl;
	cout <<"open file:\t"<<file<<"\tfor reading"<<endl;
	
	mSkimFile = new TFile(file,"READ");
	assert(mSkimFile);
	
	TObject* t = mSkimFile->Get("jetSkimTree");
	assert(t);
	
	mSkimTree = dynamic_cast<TTree*>(t);
	assert(mSkimTree);
	
	mSkimEvent = new StJetSkimEvent();
	
	//set branch status
	mSkimTree->SetBranchStatus("skimEventBranch", 1);
	mSkimTree->SetBranchAddress("skimEventBranch", &mSkimEvent);

	cout <<"successfully recovered tree from file."<<endl;
}

void StJetReader::InitTree(TTree* tree)
{
    cout <<"StJetReader::InitTree()"<<endl;

    cout <<"\tset tree pointer"<<endl;
    cout <<"Number of entries in tree:\t"<<tree->GetEntries();

    TList* friendList = tree->GetListOfFriends();
    TTree* t=0;
    for (int j=0; j<friendList->GetSize()+1; ++j) {
	TFriendElement* fr = static_cast<TFriendElement*>( friendList->At(j) );
	string tree_name( fr->GetTreeName() );
	if (tree_name == "jet") {
	    t = fr->GetTree();
	    break;
	}
    }
    assert(t);
    
    cout <<"\tGet Branches"<<endl;
    TObjArray* branches = t->GetListOfBranches();
    if (!branches) {cout <<"StJetReader::InitFile().  Null branches"<<endl; abort();}

    cout <<"\tLoop on branches"<<endl;

    for (int i=0; i<branches->GetLast()+1; ++i) {
	TBranch* branch = dynamic_cast<TBranch*>((*branches)[i]);
	if (!branch) {cout <<"StJetReader::InitFile().  Null branch"<<endl; abort();}
	string bname( branch->GetName() );
	cout <<"\t--- Found branch:\t"<<bname<<endl;
	
	if ( (bname.find("jet")!=bname.npos) || (bname.find("Jet")!=bname.npos) ) {
	    cout <<"\t\tcreate StJets object for branch:\t"<<bname<<endl;

	    //create StJets object here, put in map:
	    StJets* jets = new StJets();
	    jets->Clear();
	    mStJetsMap[bname] = jets;
	    cout <<"\t\tset branch address for branch:\t"<<bname.c_str()<<endl;
	    t->SetBranchStatus(bname.c_str(), 1);
	    t->SetBranchAddress(bname.c_str(), &jets);
	}
    }

    cout <<"\tfinished!"<<endl;

    return ;
}

int StJetReader::preparedForDualRead()
{
	cout <<"StJetReader::preparedForDualRead()"<<endl;
	int jetStatus = (mTree) ? 1 : 0;
	int skimStatus = (mSkimTree) ? 1 : 0;
	cout <<"jet tree status:\t"<<jetStatus<<endl;
	cout <<"skim tree status:\t"<<skimStatus<<endl;
	assert(mTree);
	assert(mSkimTree);
	
	int nJetEvents = mTree->GetEntries();
	int nSkimEvents = mSkimTree->GetEntries();
	cout <<"nJetEvents:\t"<<nJetEvents<<endl;
	cout <<"nSkimEvents:\t"<<nSkimEvents<<endl;
	assert(nJetEvents==nSkimEvents);
	assert(mSkimEvent);
	
	cout <<"Congratulations, you are ready to process events!"<<endl;
	mValid = true;
	return 1;
}

Int_t StJetReader::Make()
{
	int status = mTree->GetEntry(mCounter);
	if (status<0) {
		cout <<"StJetReader::getEvent(). ERROR:\tstatus<0.  return null"<<endl;
	}
	
	if (mSkimTree) {
		status = mSkimTree->GetEntry(mCounter);
	}
	
	mCounter++;
	
//     if (mDstMaker) {//double check consistency:
// 		StMuDst* mudst = mDstMaker->muDst();
// 		for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
// 			StJets* j = (*it).second;
// 			
// 			if ( !j->isSameEvent(mudst) ) {
// 				cout <<"StJetReader::Maker() ERROR:\tisSameEvent()==false.  abort"<<endl;
// 				abort();
// 			}
// 		}
//     }
    return kStOk;
}

Int_t StJetReader::Finish()
{
    if (mOfstream) {
		delete mOfstream;
		mOfstream=0;
    }
	
    return kStOk;
}

void dumpProtojetToStream(int event, ostream& os, StJets* stjets);

string idString(TrackToJetIndex* t)
{
    string idstring;
    StDetectorId mDetId = t->detectorId();
    if (mDetId==kTpcId) {
		idstring = "kTpcId";
    }
    else if (mDetId==kBarrelEmcTowerId) {
		idstring = "kBarrelEmcTowerId";
    }
    else if (mDetId==kEndcapEmcTowerId) {
		idstring = "kEndcapEmcTowerId";
    }
    else {
		idstring = "kUnknown";
    }
    return idstring;
}

//nice check to verify that the jet 4-mom is equal to the _vector_ sum of it's part
bool verifyJet(StJets* stjets, int ijet)
{
    TClonesArray& jets = *(stjets->jets());
    StJet* pj = static_cast<StJet*>(jets[ijet]);
    
    
    StThreeVectorD j3(pj->Px(), pj->Py(), pj->Pz());
    StLorentzVectorD j4(pj->E(),j3 );
	
    //cout <<"\tjet:\t"<<ijet<<"\t4mom:\t"<<j4<<endl;
	
    StLorentzVectorD jetMom(0., 0., 0., 0.);
    
    typedef vector<TrackToJetIndex*> FourpList;
    FourpList particles = stjets->particles(ijet);
    
    for (FourpList::iterator it=particles.begin(); it!=particles.end(); ++it) {
		TLorentzVector* v = *it;
		StThreeVectorD v3(v->Px(), v->Py(), v->Pz() );
		StLorentzVectorD v4(v->E(), v3 );
		jetMom += v4;
		//cout <<"\t\t4p:\t"<<v4<<endl;
    }
    //cout <<"\t\t\tcheck:\t"<<jetMom<<endl;
    StLorentzVectorD diff = j4-jetMom;
    if (abs(diff)>1.e-6) { //they have to be the same to 1 eV
		cout <<"verifyJet.  assert will fail for jet:\t"<<ijet<<"\t4p:\t"<<j4<<"\tcompared to sum_particles:\t"<<jetMom<<endl;
		return false;
    }
    else {
		return true;
    }
}


void StJetReader::exampleFastAna()
{
	cout <<"StJetReader::exampleEventAna()"<<endl;
	
	StJetSkimEvent* skEv = skimEvent();
	    
	//basic information:
	cout <<"fill/run/event:\t"<<skEv->fill()<<"\t"<<skEv->runId()<<"\t"<<skEv->eventId()<<endl;
	cout <<"fileName:\t"<<skEv->mudstFileName().GetString()<<endl;
	
	//some spin info:
	cout <<"bx48:\t"<<skEv->bx48()<<"\tspinBits:\t"<<skEv->spinBits()<<endl;
	
	//some event info:
	cout <<"Ebbc:\t"<<skEv->eBbc()<<"\tWbbc:\t"<<skEv->wBbc()<<"\tbbcTimeBin:\t"<<skEv->bbcTimeBin()<<endl;
	
	//best vertex info:
	const float* bestPos = skEv->bestVert()->position();
	//cout <<"best Vertex (x,y,z):\t"<<bestPos[0]<<"\t"<<bestPos[1]<<"\t"<<bestPos[2]<<endl;
	
	//trigger info:
	const TClonesArray* trigs = skEv->triggers();
	assert(trigs);
	int nTrigs = trigs->GetLast()+1;
	for (int i=0; i<nTrigs; ++i) {
		StJetSkimTrig* trig = static_cast<StJetSkimTrig*>( (*trigs)[i] );
		assert(trig);
		if (trig->didFire() != 0) {
//			cout <<"\tTriggerd:\t"<<trig->trigId()<<"\twith prescale:\t"<<trig->prescale()<<endl;
		}
		if (trig->shouldFireL2() == 1) {
		  cout << "\tshTrigger L2: " << trig->trigId() <<endl;
		  UInt_t *l2temp = trig->L2ResultEmulated();
		  for (int ii=0; ii<9; ii++) {
		    cout << "SimL2--- " << ii << "\t" << l2temp[ii] << endl;
		  }
		}
	}
		
	//L2 Info:
	cout <<"\n--- Non-zero L2 Results:"<<endl;
	/*
	const int* l2Results = skEv->l2Result();
	for (int i=0; i<32; ++i) {
		cout <<i<<"\t"<<l2Results[i]<<endl;
	}
	 
	const TArrayI& l2Results = skEv->l2Result();
	for (int i=0; i<l2Results.GetSize(); ++i) {
		int val = l2Results[i];
		if (val>0) {
			cout <<i<<"\t"<<val<<endl;
		}
	}
	*/
	UInt_t *l2temp = skEv->L2Result();
	for (int ii=0; ii<9; ii++) {
	  if (l2temp[ii]!=0)  cout << "DatL2--- " << ii << "\t" << l2temp[ii] << endl;
	}
	

	//vertex info:
	const TClonesArray* verts = skEv->vertices();
	assert(verts);
	int nVerts = verts->GetLast()+1;
	for (int i=0; i<nVerts; ++i) {
		StJetSkimVert* vert = static_cast<StJetSkimVert*>( (*verts)[i] );
		assert(vert);
		const float* vertPos = vert->position();
		cout <<"vert:\t"<<i<<"\t at z:\t"<<vertPos[2]<<"\tranking:\t"
			<<vert->ranking()<<"\tnTracks:\t"<<vert->nTracksUsed()<<endl;
	}
	
	
	//And then access to jets from different algorithms...
	cout <<"\nLoop on jet branches"<<endl;
    for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
		
		StJets* stjets = (*it).second;
		int nJets = stjets->nJets();
		cout <<"Found\t"<<nJets<<"\tjets from:\t"<<(*it).first<<endl;
		
		//first make sure that we have the same run/event:
		assert(stjets->runId()==skEv->runId());
		assert(stjets->eventId()==skEv->eventId());

		TClonesArray* jets = stjets->jets();
		
		
		for(int ijet=0; ijet<nJets; ++ijet){ 
			
			//loop on jets
			StJet* j = static_cast<StJet*>( (*jets)[ijet] );
			assert(j);
			assert(verifyJet(stjets, ijet));
			
			cout <<"jet:\t"<<ijet<<"\tEjet:\t"<<j->E()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<"\tdetEta:\t"<<j->detEta()<<endl;
			
			//look at 4-momenta in the jet:
			typedef vector<TrackToJetIndex*> TrackToJetVec;
			TrackToJetVec particles = stjets->particles(ijet);
			
			for (TrackToJetVec::iterator partIt=particles.begin(); partIt!=particles.end(); ++partIt) {
				const TrackToJetIndex* theParticle = *partIt;
				cout <<"\tparticle \t pt:\t"<<theParticle->Pt()<<"\tnHitsFit:\t"<<theParticle->nHitsFit()<<endl;
			}
		}
    }	
}

void StJetReader::exampleEventAna()
{
    cout <<"StJetReader::exampleEventAna()"<<endl;
	
    //Get pointers to retreive the emc info
	/*
    StEmcGeom* geom = StEmcGeom::getEmcGeom(detname[0].Data());
    StEmcADCtoEMaker* adc2e =dynamic_cast<StEmcADCtoEMaker*>( GetMaker("Eread") );
    assert(adc2e);
    StBemcData* data = adc2e->getBemcData();
	 */
    //int numHits = data->NTowerHits;
    //cout << "Number Hits: " << numHits;
	
    StMuDst* muDst = 0;
    if (mDstMaker!=0) {
		muDst = mDstMaker->muDst();
		cout <<"nPrimary:\t"<<muDst->primaryTracks()->GetLast()+1;
    }
    
    for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
		
		StJets* stjets = (*it).second;
		int nJets = stjets->nJets();
		cout <<"Found\t"<<nJets<<"\tjets from:\t"<<(*it).first<<endl;
		
		TClonesArray* jets = stjets->jets();
		
		//Dylan, here's a nice check...
		if (0) {
			if (stjets->nJets()>0) {
				dumpProtojetToStream(muDst->event()->eventId(), *mOfstream, stjets);
			}
		}
		
		for(int ijet=0; ijet<nJets; ++ijet){ 
			
			//loop on jets
			StJet* j = static_cast<StJet*>( (*jets)[ijet] );
			assert(j);
			assert(verifyJet(stjets, ijet));
			
			cout <<"jet:\t"<<ijet<<"\tEjet:\t"<<j->E()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
			
			//look at 4-momenta in the jet:
			typedef vector<TrackToJetIndex*> TrackToJetVec;
			TrackToJetVec particles = stjets->particles(ijet);
			
			/*
			for (TrackToJetVec::iterator it=particles.begin(); it!=particles.end(); ++it) {
				TrackToJetIndex* t2j = (*it); //remember, TrackToJetIndex inherits from TLorentzVector, so it _is_ the 4p of a track/tower
				assert(t2j);
				double dphi = gDeltaPhi(j->Phi(), t2j->Phi());
				double deta = j->Eta()-t2j->Eta();
				double dR = sqrt(dphi*dphi + deta*deta);
				
				cout <<"\tPt_part:\t"<<t2j->Pt()<<"\tEta_part:\t"<<t2j->Eta()<<"\tPhi_part:\t"<<t2j->Phi()<<"\tdR:\t"<<dR<<"\t"<<idString(t2j)<<endl;
				
			}
			 */
		}
    }
}



// -------------------- old, MLM
/*
  	    else {
		typedef StJets::TrackVec TrackVec;
		TrackVec tracks = stjets->jetParticles(muDst, i);
		
		int itrack = 0;
		for (TrackVec::iterator tit = tracks.begin(); tit!=tracks.end(); ++tit) {
		    StMuTrack *muTrack = *tit;
		    
		    //cout <<"\t--track "<<itrack<<endl;
		    const StThreeVectorF& mom = muTrack->momentum();
		    double dR = gDeltaR(j, mom);
		    cout <<"\tPt_track:\t"<<mom.perp()
			 <<"\tEta_track:\t"<<mom.pseudoRapidity()
			 <<"\tPhi_track:\t"<<mom.phi()
			 <<"\tdR:\t"<<dR<<endl;
		    
		    //dylan, now get it from the double-check method (stored directly at jet finding, _with_ energy subtraction)
		    const TLorentzVector* check = stjets->trackToJetIndex(muDst, i, muTrack); //see StJets::addProtoJet(), where 4-p is stored
		    assert(check);
		    double dphi_check = gDeltaPhi(j->Phi(), check->Phi());
		    double deta_check = j->Eta()-check->Eta();
		    double dR_check = sqrt(dphi_check*dphi_check + deta_check*deta_check);
		    cout <<"\tPt_check:\t"<<check->Pt()<<"\tEta_check:\t"<<check->Eta()<<"\tPhi_check:\t"<<check->Phi()<<"\tdR:\t"<<dR_check<<endl;
		    
		    ++itrack;
		}
		//now get the bemc info:
		vector<int> towerIndices = stjets->jetBemcTowerIndices(i);
		const int maxHits = 4800;

		for (vector<int>::iterator bit=towerIndices.begin(); bit!=towerIndices.end(); ++bit) {
		    int towerIndex = (*bit);
		    if (towerIndex>maxHits) {
			cout <<"StJetReader::exampleEventAna(). ERROR:\ttowerIndex out of bounds. abort()"<<endl;
			abort();
		    }
		    float eta, phi;
		    geom->getEtaPhi(towerIndex, eta, phi);
		    double e = data->TowerEnergy[towerIndex];
		    double dphi = gDeltaPhi(j->Phi(), phi);
		    double deta = j->Eta()-eta;
		    double dR = sqrt( dphi*dphi  +  deta*deta );
		    cout <<"\tE_tower:\t"<<e<<"\tEta_tower:\t"<<eta<<"\tPhi_tower:\t"<<phi<<"\tdR:\t"<<dR<<endl;

		    //dylan, now get it from the double-check method (stored directly at jet finding, _with_ energy subtraction)
		    const TLorentzVector* check = stjets->trackToJetIndex(i, (*bit));
		    if (!check) {
			cout <<"you're going to segfualt on jet:\t"<<i<<"\ttower:\t"<<(*bit)<<endl;
		    }
		    assert(check);
		    double dphi_check = gDeltaPhi(j->Phi(), check->Phi());
		    double deta_check = j->Eta()-check->Eta();
		    double dR_check = sqrt(dphi_check*dphi_check + deta_check*deta_check);
		    cout <<"\tE_check:\t"<<check->E()<<"\tEta_check:\t"<<check->Eta()<<"\tPhi_check:\t"<<check->Phi()<<"\tdR:\t"<<dR_check<<endl;
		}
}
*/
