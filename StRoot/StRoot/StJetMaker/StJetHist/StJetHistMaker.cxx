//root
#include "TFile.h"
#include "TTree.h"
#include "TH3.h"
#include "TNtuple.h"

//std
#include <vector>
#include <map>
#include <algorithm>
#include <cmath>
using namespace std;

//St_base
#include "StChain.h"
#include "StDetectorDbMaker/StDetectorDbTriggerID.h"

//StEvent
#include "StEvent.h"
#include "StEnumerations.h"
#include "StEventTypes.h"

//SCL
#include "StPhysicalHelixD.hh"

//vertex constraint
#include "tables/St_vertexSeed_Table.h"

//STAR
#include "TFile.h"
#include "StChain.h"
#include "SystemOfUnits.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"

//StEvent
#include "StEventTypes.h"

//StEmc
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcRawMaker/StEmcRawMaker.h"
#include "StEmcRawMaker/defines.h"

//Endcap
#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/database/cstructs/eemcConstDB.hh"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"


//local
#include "StJetHistMaker.h"

ClassImp(StJetHistMaker)

//StJetHistMaker::StJetHistMaker(const Char_t *name, StMuDstMaker* uDstMaker, StEmcADCtoEMaker* adc2e, StEmcSimulatorMaker* sim, const char *outputName) 
//: StMaker(name), muDstMaker(uDstMaker), mEmcSim(sim), mOutName(outputName), mAdc2E(adc2e), mTables(new StBemcTables())
    StJetHistMaker::StJetHistMaker(StMuDstMaker* uDstMaker, const char *outputName) 
	: StMaker("StJetHistMaker"), muDstMaker(uDstMaker), mOutName(outputName), mTables(new StBemcTables())
{
    mudst=0;
}

Int_t StJetHistMaker::InitRun(Int_t runId)
{
    cout <<"Welcome to StJetHistMaker::InitRun()"<<endl;
    mTables->loadTables((StMaker*)this);

    /*
    cout <<"id\tadc\teta\tphi\tpedestal\trms\tgain\tstatus"<<endl;
    cout <<"--\t---\t---\t---\t--------\t---\t----\t------\n"<<endl;

    for (int id=1; id<24; ++id) {
	float pedestal, rms;
	int CAP=0; //this arument matters only for SMD
	mTables->getPedestal(BTOW, id, CAP, pedestal, rms);
	
	//get eta/phi
	float eta, phi;
	//geom->getEtaPhi(id,eta,phi);
	
	//get gain
	float gain = -1;
	mTables->getCalib(BTOW,id,1,gain);

	cout <<id<<"\t"<<"-"<<"\t"<<"-"<<"\t"<<"-"<<"\t"<<pedestal<<"\t"<<rms<<"\t"<<gain<<"\t"<<"-"<<endl;

    }

    abort();
    */
    
    return kStOk;
    
}

Int_t StJetHistMaker::Init() 
{
    cout << "StJetHistMaker: output file: " << mOutName << endl;
    
    //open output file
    mOutfile = new TFile(mOutName,"RECREATE");

    //for mb
    mbVertexZvsNp = new TH2F("mbVertexZvsNp","Z_{vertex} vs N_{good-primary} (mb)", 101, -0.5, 100.5, 400, -200., 200.);
    mbTrackKin = new TH3F("mbTrackKin","Eta vs Phi vs Pt (mb)", 200, 0., 10., 100, -3.14159, 3.14159, 100, -1.5, 1.5);
    mbNfitVsEta = new TH2F("mbNfitVsEta","Eta vs N_{fit} (mb)", 56, -0.5, 55.5, 100, -1.5, 1.5);

    //for ht1
    ht1VertexZvsNp = new TH2F("ht1VertexZvsNp","Z_{vertex} vs N_{good-primary} (ht1)", 101, -0.5, 100.5, 400, -200., 200.);
    ht1TrackKin = new TH3F("ht1TrackKin","Eta vs Phi vs Pt (ht1)", 200, 0., 10., 100, -3.14159, 3.14159, 100, -1.5, 1.5);
    ht1NfitVsEta = new TH2F("ht1NfitVsEta","Eta vs N_{fit} (ht1)", 56, -0.5, 55.5, 100, -1.5, 1.5);

    //for other (e.g., pythia)
    otherVertexZvsNp = new TH2F("otherVertexZvsNp","Z_{vertex} vs N_{good-primary} (other)", 101, -0.5, 100.5, 400, -200., 200.);
    otherTrackKin = new TH3F("otherTrackKin","Eta vs Phi vs Pt (other)", 200, 0., 10., 100, -3.14159, 3.14159, 100, -1.5, 1.5);
    otherNfitVsEta = new TH2F("otherNfitVsEta","Eta vs N_{fit} (other)", 56, -0.5, 55.5, 100, -1.5, 1.5);

    //EMC MIP hist:
    mipHistVsEta = new TH2F("mipHistVsEta","ADC_{MIP} distribution vs Tower Eta",20, 0., 1., 200, 0, 200);
    mipEvsEta = new TH2F("mipEvsEta","E_{MIP} distribution vs Tower Eta",20, 0., 1., 200, 0, 2);
    towerEvsId = new TH2F("towerEvsId","E_{Tower} vs ID",2401, -0.5, 2400.5, 100, 0., 10.);
    towerAdcvsId = new TH2F("towerAdcvsId","ADC_{Tower} vs ID", 2401, -0.5, 2400.5, 100, 0., 500.);


    return StMaker::Init();
}

void StJetHistMaker::fillBarrelHits()
{
    cout <<"void StJetHistMaker::fillBarrelHits()"<<endl;
    
    //assert(mEmcSim);
    
    StEmcGeom* geom = StEmcGeom::instance("bemc"); // for towers
    assert(geom);

    //Get status tables.
    assert(mTables);

    //Now loop on emc data
    StEvent* event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
    assert(event);
    StEmcCollection* emc = event->emcCollection();
    assert(emc);
    
    // now it is like StEvent, getting energies for towers
    StEmcDetector* detector = emc->detector(kBarrelEmcTowerId);
    assert(detector);

    //cout <<"id\tadc\teta\tphi\tpedestal\trms\tgain\tstatus"<<endl;
    //cout <<"--\t---\t---\t---\t--------\t---\t----\t------\n"<<endl;

    //check peds, gain, status, etc...
    for(int m = 1; m<=120;m++) { //loop on modules...
	StEmcModule* module = detector->module(m);
	assert(module);
	
	StSPtrVecEmcRawHit& rawHits = module->hits();
	
	for(UInt_t k=0;k<rawHits.size();k++) { //loop on hits in modules
	    StEmcRawHit* tempRawHit = rawHits[k];
	    
	    //Get eta, phi
	    int m = tempRawHit->module();
            int e = tempRawHit->eta();
            int s = abs(tempRawHit->sub());
            int id, status;
	    //int adc = tempRawHit->adc(); //not pedestal subtracted!
	    geom->getId(m,e,s,id); // to get the software id
	    
	    //now check the status: (//BTOW defined in StEmcRawMaker/defines.h
	    mTables->getStatus(BTOW, id, status);

	    //Get ped, rms
	    float pedestal, rms;
	    int CAP=0; //this arument matters only for SMD
	    mTables->getPedestal(BTOW, id, CAP, pedestal, rms);

	    //get eta/phi
	    float eta, phi;
	    geom->getEtaPhi(id,eta,phi);

	    //get gain
	    float gain = -1;
	    mTables->getCalib(BTOW,id,1,gain);

	    if (gain!=0. && status==1);


	    //cout <<id<<"\t"<<adc<<"\t"<<eta<<"\t"<<phi<<"\t"<<pedestal<<"\t"<<rms<<"\t"<<gain<<"\t"<<status<<endl;
	    
	}
    }
}

Int_t StJetHistMaker::Make()
{
    cout <<" Start StJetHistMaker :: "<< GetName() <<endl;
    gMessMgr->SwitchOff("I");
    gMessMgr->SwitchOff("E");
    gMessMgr->SwitchOff("W");
    gMessMgr->SwitchOff("Q");
    
    //fillBarrelHits();

    mudst = muDstMaker->muDst();
    StMuEvent* muEvent = mudst->event();
    
    //Event quantities:		http://www.star.bnl.gov/cgi-bin/cvsweb.cgi/StRoot/StMuDSTMaker/COMMON/
    StThreeVectorF vertex = muEvent->primaryVertexPosition();
    /*
    int nprimary = mudst->numberOfPrimaryTracks();
    int runId = muEvent->runId();
    int eventId = muEvent->eventNumber();
    int day = runId/1000 - 5000;
    StRunInfo& runInfo = muEvent->runInfo();
	
    assert(runInfo.beamFillNumber(blue)==runInfo.beamFillNumber(yellow));
    float fill = runInfo.beamFillNumber(blue);
    */
	
    StMuTriggerIdCollection tic = muEvent -> triggerIdCollection();
    StTriggerId l1trig = tic.l1();

    int startriggers[3] = {45010, 45201, 45202}; //mb, bht1-slow, bht2-slow
    int trigs[3] = {0,0,0};
    int prescales[3] = {0,0,0};

    //see if we satisfy various triggers
    for (int i=0; i<3; ++i) {
	if (l1trig.isTrigger(startriggers[i])) {
	    trigs[i] = 1;
	}
    }

    //get trigger prescales:
    StDetectorDbTriggerID& v = *(StDetectorDbTriggerID::instance());
    for(unsigned int i = 0; i < v.getL0NumRows(); i++){ //loop on table
	
	for (int j=0; j<3; ++j) { //loop on triggers
	    
	    if (v.getL0OfflineTrgId(i) == startriggers[j]) { //ok, it's one of our triggers
		prescales[j] = v.getPsL0(i);
	    }
	}
    }
    
    if (vertex.z()!=0. && fabs(vertex.z())<50 ) {

	int nprim = 0;
	int nTracks = mudst->primaryTracks()->GetLast()+1;
	
	for(int i = 0; i < nTracks; i++)	{
	    StMuTrack* track = mudst->primaryTracks(i);
	    assert(track);
	    
	    if ( track->flag()>0
		 && track->topologyMap().trackFtpcEast()==false
		 && track->topologyMap().trackFtpcWest()==false
		 && track->pt()>0.2
		 && track->nHitsFit()>30
		 && fabs(track->eta())<0.5
		 ) {
		
		
		StThreeVectorF mom = track->momentum();
		//if (fabs(track->eta())<0.5 && track->nHitsFit>30) 
		++nprim;
		    
		//ok, we have a vertex, and a good primary track.
		if (trigs[0] == 1) { //we have a mb event
		    mbTrackKin->Fill( mom.perp(), mom.phi(), mom.pseudoRapidity() );
		    mbNfitVsEta->Fill(track->nHitsFit(), mom.pseudoRapidity());
		}
		if (trigs[1] == 1) {//we have a ht1-slow event
		    ht1TrackKin->Fill( mom.perp(), mom.phi(), mom.pseudoRapidity() );
		    ht1NfitVsEta->Fill(track->nHitsFit(), mom.pseudoRapidity());
		}
		if (trigs[0]==0 && trigs[1]==0 && trigs[2]==0) {//ok, "other"
		    otherTrackKin->Fill( mom.perp(), mom.phi(), mom.pseudoRapidity() );
		    otherNfitVsEta->Fill(track->nHitsFit(), mom.pseudoRapidity());
		}
		
	    }
	}

	if (trigs[0] == 1) { //we have a mb event
	    mbVertexZvsNp->Fill(nprim, vertex.z());
	}
	if (trigs[1] == 1) {//we have a ht1-slow event
	    ht1VertexZvsNp->Fill(nprim, vertex.z());
	}
	if (trigs[0]==0 && trigs[1]==0 && trigs[2]==0) {//ok, "other"
	    otherVertexZvsNp->Fill(nprim, vertex.z());
	}
    }

    return kStOk;
}

void StJetHistMaker::FinishFile(void)
{
    //close file
    mOutfile->Write();
    mOutfile->Close();
    delete mOutfile;
}

Int_t StJetHistMaker::Finish()
{
    FinishFile();
    StMaker::Finish();
    return kStOK;
}

