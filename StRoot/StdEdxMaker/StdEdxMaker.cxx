//  StdEdxMaker.cxx
// M.L. Miller
// 5/00

#include <iostream.h>
#include <assert.h>
#include <math.h>
#include <string>
#include <cmath>

#include "TreeEntryClasses.h"
#include "StdEdxMaker.h"

#include "TStyle.h"
#include "TNtuple.h"
#include "TH1.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TFile.h"
#include "StBFChain.h"
#include "TreeEntryClasses.h"
#include "StLorentzVectorF.hh"

//#include "EvtAna.h"
#include "AnaTrackId.h"
#include "TrMean.h"
#include "TreeEntryClasses.h"
#include "DeDxPreparation.h"
// StRoot
#include "StBFChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"

// StEvent
#include "StEventTypes.h"

#include "StMemoryInfo.hh"
// SCL
#include "StMemoryInfo.hh"
#include "StGlobals.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StParticleTable.hh"
#include "StParticleDefinition.hh"
#include "StLorentzVectorF.hh"
#include "StarCallf77.h" 
#include "BetheBloch.h"

#define gufld   F77_NAME(gufld,GUFLD)
extern "C" {void gufld(Float_t *, Float_t *);}
const Double_t Massp = 0.93827231;
const Double_t MassK = 0.493677;
const Double_t MassPi= 0.13956995;
const Double_t Masse = 0.51099907e-3;
const Double_t Massd = 1.87561339;
BetheBloch BB;

ClassImp(StdEdxMaker)
    
StdEdxMaker::StdEdxMaker(const Char_t *name) : StMaker(name)
{
}
//_________________________________________________
StdEdxMaker::~StdEdxMaker() {
  delete dedxprep;
  delete trmean;
}
//_________________________________________________
Int_t StdEdxMaker::Init()
{

  TFile *f = (TFile *) ((StBFChain *)GetChain())->GetTFile();
  assert(f);
  f->cd();
  dedxprep = new DeDxPreparation();
  trmean   = new TrMean;
  m_Vertex = new VertexEntry;
  m_Event  = new EventEntry;
  
  //m_Vertex = new VertexEntry();
  m_Track = new TrackEntry();
  m_EventTree = new TTree("DeDxTree","The DeDx Tree");
  Int_t buffsize = 64000;
  Int_t splitlevel = 1;
  cout<<"MakingBranch"<<endl;
  m_EventTree->Branch("TrackBranch","TrackEntry",&m_Track, buffsize, splitlevel);
  return StMaker::Init();
}
//_________________________________________________
Int_t StdEdxMaker::Make()
{
    
    StEvent* rEvent = 0;
    rEvent = (StEvent*) GetInputDS("StEvent");

    clear();
    if (rEvent) run(rEvent);
    
    return kStOK;
}
//________________________________________________________________________________
void StdEdxMaker::kill()
{
#if 0
    if (s_instance) {
	delete s_instance->m_EventTree;
	delete s_instance->m_Track;
	delete s_instance;
	s_instance = 0;
    }
#endif
}
void StdEdxMaker::run(StEvent* rEvent)
{
    m_StEvent = rEvent;
    if (m_StEvent->summary()) {
	cout <<"Valid Summary"<<endl;
    }
	m_StEvent = rEvent;
	clear();	    
	m_Event->fill(m_StEvent);  //Store this info for the tree
	primaryProcess(m_StEvent);
	//globalProcess(rEvent);
//     }
//     else {
// 	cout <<"Invalid Summary"<<endl;
// 	return;
//     }
    return;
}

void StdEdxMaker::clear()
{
    m_PrimTrackNumber = 0;
    m_GlobTrackNumber = 0;
    dedxprep->clear();
    trmean->clearAll();
    return;
}


//________________________________________________________________________________
//Loop Over Primary Vertices
void StdEdxMaker::primaryProcess(StEvent* rEvent)
{
    StPrimaryVertex* primVtx = 0;
    cout <<"Number of Vertices:\t"<<rEvent->numberOfPrimaryVertices()<<endl;
    if (rEvent->numberOfPrimaryVertices() > 0) {
	for(unsigned int i=0; i<rEvent->numberOfPrimaryVertices(); i++) {
	    primVtx = rEvent->primaryVertex(i);
	    cout <<primVtx->position()<<endl;
	    if (!primVtx) {
		cout << "No primary vertex in this event!" << endl;
		return;
	    }
	    if (primVtx->daughters().size()) break;
	}
	//Store Info, Analyze Vertex
	m_Vertex->fill(primVtx);
	vertexAna(primVtx);
    }
    return;
}

//Loop over global tracks
void StdEdxMaker::globalProcess(StEvent* rEvent)
{
    StSPtrVecTrackNode& trackNode = rEvent->trackNodes();
    StSPtrVecTrackNodeIterator iter;
    for (iter = trackNode.begin(); iter != trackNode.end(); iter++) {
	if (*iter) {
	    StTrack* track = (*iter)->track(global);
	    if (track) {
		m_GlobTrackNumber++;
		trackAna(track);
	    }
	}
    }
    return;
}
    
    

//_________________________________________________
void StdEdxMaker::vertexAna(StPrimaryVertex* primVtx)
{
    const StSPtrVecPrimaryTrack& tracks = primVtx->daughters();
    cout<<"Number of Tracks: "<< tracks.size()<<endl;
    
    vector<StPrimaryTrack*>::const_iterator iter;
    StTrack* track   = 0;
    StGlobalTrack*  globTrk = 0;

    // Loop over the primary tracks.
    for (iter = tracks.begin(); iter != tracks.end(); iter++) {
	track = *iter;
	globTrk = dynamic_cast<StGlobalTrack*>(track->node()->track(global));
	
	if (track==0 || globTrk==0 || track->flag() < 0 ) continue;
	m_PrimTrackNumber++;
	trackAna(track);
    }
    
    return;
}
//________________________________________________________________________________
//Fill the TTree here (per track)
void StdEdxMaker::trackAna( StTrack* track )
{
  Float_t x[3] = { 0, 0, 0};
  Float_t b[3];
  gufld(x,b);
    if (fmod(m_PrimTrackNumber, 100.) == 0.) {
	cout<<"Processing Track:\t"<<m_PrimTrackNumber<<endl;}
    
    dedxprep->clearAll();
    dedxprep->setBField(b[2] );
    dedxprep->setFillTuple(true);
    dedxprep->setTrack(track);
    dedxprep->findHits();
    dedxprep->dxNorm();
    dedxprep->DoFitZ();
    //dedxprep->printNormChargeVec();

    double tfact = .55; //Keep 55 percent of hits
    trmean->clearAll();
    trmean->setTrack(track);
    trmean->setTruncationFactor(tfact);
    trmean->findHits();
    trmean->dxNorm();
    trmean->compute();
    
    //Get info from CO's method
    AnaTrackId* trackId = new AnaTrackId(track);
    StDedxPidTraits* dedxPidTr = trackId->getPidTraits();
    if (dedxPidTr) {
	
	TrackEntry entry(track); //Make the track Entry

	//fill the hit array, I'm not sure where this should be done
	const vector<HitEntry>& hitvec = dedxprep->hitEntryVec();
	for (vector<HitEntry>::const_iterator it=hitvec.begin(); it!=hitvec.end(); it++) {
	    entry.addHit(*it);
	}

	entry.setFlag0Points(dedxprep->numberOfGoodHits());
	entry.setMean55(trmean->mean());
	entry.setErrorOnMean55(trmean->errorOnMean());
	entry.setDeDxPoints55(trmean->numberOfPoints());
       	entry.setPidTraits(dedxPidTr);

	entry.setAvgZ(dedxprep->avgZ());
	entry.setAvgZSquared(dedxprep->avgZSquared());
	
        entry.setZFitted(dedxprep->fitZ());
        entry.setdZFitted(dedxprep->fitdZ());
        entry.setSFitted(dedxprep->fitS());
        entry.setdSFitted(dedxprep->fitdS());
        entry.setValFitted(dedxprep->ValFitted());
        entry.setTpcLength(dedxprep->TpcLength());
        entry.setTrackZ(dedxprep->TrackZ());

	Double_t pmag = entry.pmag();
	entry.setze(BB(pmag/Masse));
	entry.setzK(BB(pmag/MassK));
	entry.setzPi(BB(pmag/MassPi));
	entry.setzp(BB(pmag/Massp));
	entry.setzd(BB(pmag/Massd));

	//Store data for inner-padrows only
	dedxprep->clear();
	dedxprep->setBField(b[2] );
	dedxprep->setFillTuple(false);
	dedxprep->setMinPadrow(0);
	dedxprep->setMaxPadrow(13);
	dedxprep->dxNorm();
	entry.setFlag0PointsInner(dedxprep->numberOfGoodHits());
	entry.setAvgZInner(dedxprep->avgZ());
	entry.setAvgZInnerSquared(dedxprep->avgZSquared());

	entry.setAvgZOuterSquared(dedxprep->avgZSquared());

	//Store data for outer-padrows only
	dedxprep->clear();
	dedxprep->setBField(b[2] );
	dedxprep->setFillTuple(false);
	dedxprep->setMinPadrow(14);
	dedxprep->setMaxPadrow(45);
	dedxprep->dxNorm();
	entry.setFlag0PointsOuter(dedxprep->numberOfGoodHits());
	entry.setAvgZOuter(dedxprep->avgZ());
	entry.setAvgZOuterSquared(dedxprep->avgZSquared());
	
	entry.addVertex(*m_Vertex); 	    //Add the vertex info
	entry.addEvent(*m_Event);            //Add event info
	
	m_Track->fill(entry);
	m_EventTree->Fill();
    }
    return;
}

