//////////////////////////////////////////////////////////////////////
//
// $Id: StppEvent.cxx,v 1.9 2002/12/04 20:28:08 thenry Exp $
// $Log: StppEvent.cxx,v $
// Revision 1.9  2002/12/04 20:28:08  thenry
// StppuDstMaker was modified to allow multiple jet analysis modules to be
// run simultaneosly with various parameters while the Maker loads the events
// and analyses them.  Four different jet analyzers exist:
//
// Konstanin's Analyzers:
//     Kt type: StppKonstKtJetAnalyzer
//     Cone type: StppKonstConeJetAnalyzer
//
// Mike's Analyzers:
//     Kt type: StppMikeKtJetAnalyzer
//     Cone type: StppMikeConeJetAnalyzer
//
// These modules all require the StJetFinder modules.
//
// Revision 2002/08/31 Thomas
// Modified to handle multiple third party analyzers
//
// Revision 1.8  2002/06/24 13:22:59  akio
// numerous bug fix & updates
//
// Revision 1.7  2002/05/16 21:55:05  akio
// bug fixed for unpol bunch spin bit
//
// Revision 1.6  2002/02/15 14:50:13  jeromel
// Re-enabled changes. StEvent commited as well.
//
// Revision 1.5  2002/02/13 17:15:10  jeromel
// Commented out recent addition from Akio to prevent compilation collapse.
// Need StEvent addition.
//
// Revision 1.4  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
// Revision 1.3  2002/01/24 17:38:33  akio
// add L3 info, zdc info & fix phi/psi confusion
//
// Revision 1.2  2002/01/17 02:06:13  akio
// fixed bug in L3 weighted phi
//
// Revision 1.1  2002/01/16 20:22:53  akio
// First version
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
//
//////////////////////////////////////////////////////////////////////
#include <iostream.h>

//#include "Rtypes.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuTrackFourVec.h"
#include "StTriggerDetectorCollection.h"
#include "StCtbTriggerDetector.h"

//StJetFinder
#include "StJetFinder/StProtoJet.h"

//StSpinMaker
#include "StppJetAnalyzer.h"
#include "StppEvent.h"
#include "StEtGridFlat.h"
#include "StppTrack.h"
#include "StppTrackFourVec.h"
#include "StJet.h"

ClassImp(StppEvent)

extern "C" void fpdpi0mass_(int*,int*,float*,float*);

StppEvent::StppEvent(){
    tracks = new TClonesArray ("StMuTrack",200);  //warning hardcoded # of track limits!
    trackChoice = 0; //primary track is default
#ifdef _Jet_
    jets = new TClonesArray ("StJet", 20);

    mAnalyzers = new (StppJetAnalyzer*)[MAXANALYZERS];
    numAnalyzers = 0;
    mStJetsMap = 0;
    
    jetR    = 0.7;
    mJetClusterRadius = 1.4;
    jetSeed = 1.5;
    jetCut  = 0.2;
#endif 
    infoLevel = 0;
    BunchIdDifference=-999;
    clear();
}

StppEvent::~StppEvent() {
    clear() ;
    tracks->Delete();
    delete tracks ;
#ifdef _Jet_
    jets->Delete();
    delete jets;
    jets=0;
#endif 
} 

void StppEvent::clear(){
    runN = 1;
    eventN = 0;
    token = 0;
    triggerWord = 0;
    time = 0;
    bunchId = 0;
    bunchId7bit = 0;
    doubleSpinIndex = 0;

    if(tracks) tracks->Clear();
    nPrimTrack = 0;
    nGoodTrack = 0;
    xVertex = -9999.0;
    yVertex = -9999.0;
    zVertex = -9999.0;
    LCP = 0;
    sumPt = 0.0;
    vectorSumPt = 0.0;
    weightedEta = 0.0;
    weightedPhi = 0.0;

    bbcAdcSumEast = 0;
    bbcAdcSumWest = 0;
    bbcNHitEast=0;
    bbcNHitWest=0;
    zVertexBbc=0.0;

    fpdESumNorth  = 0.0;
    fpdAdcSumNorth  = 0;
    fpdAdcSumSouth  = 0;
    fpdAdcSumTop    = 0;
    fpdAdcSumBottom = 0;
    fpdAdcSumPres1  = 0;
    fpdAdcSumPres2  = 0;
    fpdAdcSumSmdX   = 0;
    fpdAdcSumSmdY   = 0;
    fpdSouthVeto    = 0;
    fpdPi0Mass      = 0.0;
    fpdPi0E         = 0.0;
    fpdPi0Eta       = 0.0;
    fpdPi0Phi       = 0.0;
    fpdPi0EShare    = 0.0;  
    fpdPi0SmdDiff   = 0.0;  

    ctbAdcSum = 0.0;
    ctbNHit=0;
    zdcEast = 0;
    zdcWest=0;
    zdcTdcEast = 0;
    zdcTdcWest = 0;
    zdcRatioEast = 0.0;
    zdcRatioWest = 0.0;

    svtNHit=0;
    emcHighTower = 0.0;  
  
#ifdef _Jet_
    //nJets = 0;
    //jets->Clear();
#endif
}

#ifndef __CINT__
Int_t StppEvent::fill(StEvent *event){
    foundJet = false;

    for(Int_t anaNum = 0; anaNum < numAnalyzers; anaNum++) {
	// Clear out the tracklist so that all the tracks are not in
	// any jet.  Using true here makes sure that disk compression
	// works well.  (No reason to save invalid track information.)
	// Using false here should make things faster, but might put
	// extraneous junk in the data file.
	StppJetAnalyzer *ana = mAnalyzers[anaNum];
	StJets *muDstJets = ana->getmuDstJets();
	muDstJets->Clear(true);
    }

    if(!event && !mudst){
	cout << "StppEvent::fill()   Error:Neither StEvent nor Mudst exist!" << endl;
	return 1;
    }
  
    StMuEvent* muevent;
    if(!event && mudst) {muevent = mudst->event(); }
  
    //event info
    if(event){
	eventN = event->id() ;
	runN   = event->runId(); 
	time   = event->time(); 
    }else{
	eventN = muevent->eventInfo().id() ;
	runN   = muevent->eventInfo().runId(); 
	time   = muevent->eventInfo().time(); 
    }
  
    // Get primary tracks
    nPrimTrack = 0; 
    TClonesArray* mutracks;
    if(mudst){
	switch(trackChoice){
	case 0: mutracks = mudst->primaryTracks(); break;
	case 1: mutracks = mudst->globalTracks(); break;
	case 2: mutracks = mudst->l3Tracks(); break;
	}
	nPrimTrack = mutracks->GetEntries();   
	for(int i=0; i<=mutracks->GetLast(); i++){
	    new((*tracks)[i]) StMuTrack((const StMuTrack &) *mutracks->UncheckedAt(i));
	}
    }else if(event){
	//getting tracks from StEvent
	StSPtrVecTrackNode* exnode = 0;
	StTrackType type;
	switch(trackChoice){
	case 0: exnode = &(event->trackNodes()); type=primary; break;
	case 1: exnode = &(event->trackNodes()); type=global;  break;
	case 2:
	    StL3Trigger* l3 = event->l3Trigger();
	    if(l3) exnode = &(l3->trackNodes());
	    type=global;
	    break;
	}    
	Int_t nnode=exnode->size();
	for( Int_t in=0; in<nnode; in++ ) {
	    UInt_t nprim = (*exnode)[in]->entries(type);
	    if(nprim==1){
		new((*tracks)[nPrimTrack++]) StMuTrack(event, (*exnode)[in]->track(type));
	    }
	}
    }

    //fill some track related summary
    nGoodTrack = 0; 
    float maxpt = 0.0;
    float sumPx=0.0;
    float sumPy=0.0;
    sumPt=0.0; weightedEta=0.0; 
    for(int i=0; i<=tracks->GetLast(); i++) {
	StMuTrack *t = (StMuTrack *)(* tracks)[i];
	float pt = t->pt();
	if(t->id() > 0 && t->nHits() > 20 && pt > 0.2 && fabs(t->eta())<1.4){
	    if(pt > maxpt){
		maxpt = pt;
		LCP = i;
	    }
	    sumPt += pt;
	    sumPx += pt * cos(t->phi());
	    sumPy += pt * sin(t->phi());
	    weightedEta += pt * t->eta();
	    nGoodTrack++;
	}
    }
  
    if(sumPt>0.0){
	vectorSumPt = sqrt(sumPx*sumPx+sumPy*sumPy);
	weightedPhi = (float)atan2((double)sumPy,(double)sumPx);
	weightedEta/= sumPt; 
    }else{
	vectorSumPt = -9.0; weightedPhi=-9.0; weightedEta=-9.0;
    }
    //create at least 1 track even if there is none
    if(nPrimTrack==0) new((*tracks)[0]) StMuTrack();
    if (infoLevel > 1){
	cout << "StppEvent : number of primary tracks " << nPrimTrack << endl;
    }
  
    // Get primary vertex
    StThreeVectorF vtx(-999.0, -999.0, -999.0);
    if(event){
	StPrimaryVertex* v = event->primaryVertex();
	if(v) vtx = v->position();
    }else{
	vtx = muevent->primaryVertexPosition();
    }  
    xVertex = vtx.x(); yVertex = vtx.y(); zVertex = vtx.z();
    if (infoLevel > 1){
	cout << "StppEvent : primary vertex " 
	     << xVertex << " "  << yVertex << " " << zVertex << endl;
    }

    // Getting detector infos
    StCtbTriggerDetector* ctb = 0;
    StZdcTriggerDetector* zdc = 0;
    StBbcTriggerDetector* bbc = 0;
    StFpdCollection*      fpd = 0;
    StL0Trigger*          l0  = 0;
    if(event) {
	StTriggerDetectorCollection* trg = event->triggerDetectorCollection();
	if(trg){
	    ctb = &(trg->ctb());
	    zdc = &(trg->zdc());
	    bbc = &(trg->bbc());
	}      
	fpd = event->fpdCollection();
	l0 = event->l0Trigger();
    }else{
	ctb = &(muevent->ctbTriggerDetector());
	zdc = &(muevent->zdcTriggerDetector());
	bbc = &(muevent->bbcTriggerDetector());
	fpd = &(muevent->fpdCollection());
	l0  = &(muevent->l0Trigger());
    }

    //CTB
    if(ctb){
	ctbAdcSum = 0.0;
	ctbNHit = 0;
	for(unsigned int i=0; i<ctb->numberOfTrays(); i++){
	    for(unsigned int j=0; j<ctb->numberOfSlats(); j++){
		ctbAdcSum += ctb->mips(i,j);
		if(ctb->mips(i,j)>0) ctbNHit++;
	    }
	}
    }

    //ZDC
    if(zdc){
	zdcEast = (Int_t)zdc->adc(4);
	zdcWest = (Int_t)zdc->adc(0);
	zdcTdcEast = (Int_t)zdc->adc(8);
	zdcTdcWest = (Int_t)zdc->adc(9);
	if(zdcEast>0) {zdcRatioEast = zdc->adc(7)/zdcEast;} else {zdcRatioEast=0.0;}
	if(zdcWest>0) {zdcRatioWest = zdc->adc(3)/zdcWest;} else {zdcRatioWest=0.0;}
    }

    //BBC
    if(bbc){
	bbcAdcSumEast = bbc->adcSumEast();
	bbcAdcSumWest = bbc->adcSumWest();
	bbcNHitEast = bbc->nHitEast();
	bbcNHitWest = bbc->nHitWest();
	zVertexBbc = bbc->zVertex();
    }

    //FPD
    if(fpd){
	token = fpd->token();
	fpdAdcSumNorth  = fpd->sumAdcNorth();
	fpdAdcSumSouth  = fpd->sumAdcSouth();
	fpdAdcSumTop    = fpd->sumAdcTop();
	fpdAdcSumBottom = fpd->sumAdcBottom();
	fpdAdcSumPres1  = fpd->sumAdcPreShower1();
	fpdAdcSumPres2  = fpd->sumAdcPreShower2();
	fpdAdcSumSmdX   = fpd->sumAdcSmdX();
	fpdAdcSumSmdY   = fpd->sumAdcSmdY();
	fpdSouthVeto    = fpd->southVeto();
	
	static const float NGain[12] = {  1.029, 0.738, 0.673, 0.522, 
					  1.124, 0.906, 0.707, 0.597,
					  1.076, 0.756, 0.786, 0.454}; 
	for(int i=1; i<=12; i++){
	  fpdESumNorth += fpd->north(i) * NGain[i-1] * 0.04;
	}
    }

    //l0 trigger info
    if(l0){
	token = l0->triggerToken();
	triggerWord = l0->triggerWord();
	bunchId = l0->bunchCrossingId();
	bunchId7bit = l0->bunchCrossingId7bit(runN);
	doubleSpinIndex = l0->spinBits();
	// unpol bunch xing now will get spin index==15
	if(runN >= 3020032 && doubleSpinIndex==0 && bunchId7bit%2==1){doubleSpinIndex=15;}
	// checking bunch ids
	if(BunchIdDifference==-999){
	    BunchIdDifference = bunchId - bunchId7bit;
	    if(BunchIdDifference<0)  BunchIdDifference+=120;
	}
	int diff = bunchId - bunchId7bit;
	if(diff<0) diff+=120;
	else if (BunchIdDifference != diff){
	    cout << "2 bunch Id did not agree "<< bunchId<<" - "<<bunchId7bit<<" = "
		 <<diff<<" != "<<BunchIdDifference<<endl;
	}
	cout << "AKIO-Run#/Token/Unix-Time/BunchID/7Bit/SpinBits:" 
	     << runN << " " << token << " " << time << " " 
	     << bunchId << " " << bunchId7bit << " " << doubleSpinIndex  << endl;
    }

#ifdef _Jet_
    //simple jet finder
    nJets = 0;
    //StEtGridFlat *g = new StEtGridFlat;
    //g->createKeys();
    //for(int i=0; i<=tracks->GetLast(); i++){
	//StMuTrack *t = (StMuTrack*)(*tracks)[i];
	//if(t->flag()>0 && t->nHits()>20 && t->pt()>0.2 && fabs(t->eta())<1.4) g->add(t);
    //}
    //StJet* jet;
    //while( (jet = g->findJet(jetR,jetSeed,jetCut,nJets)) != 0){
	//StJet* jj = new((*jets)[nJets]) StJet(jet);
	//delete jet;
	//nJets++;
    //}
    //delete g;

    cout << "Number Analyzers " << numAnalyzers << endl;
    //now use third-party jet analysis
    for(Int_t anaNum = 0; anaNum < numAnalyzers; anaNum++)
    {
      StppJetAnalyzer* thisAna = mAnalyzers[anaNum];
      if (!thisAna) {
          cout <<"StppEvent::fill() ERROR:\tmAnalyzer[" << anaNum << "]==0.  abort()"<<endl;
          abort();
      }
      thisAna->setEvent(this);
      thisAna->findJets();

      typedef StppJetAnalyzer::JetList JetList;
      JetList &cJets = thisAna->getJets();
      
      StJets *muDstJets = thisAna->getmuDstJets();
      
      if (cJets.size()>0) foundJet = true;
      
      for(JetList::iterator it=cJets.begin(); it!=cJets.end(); ++it) {
	  muDstJets->addProtoJet(*it);
      }
    }  
    
#endif

    /*
    //example fpd analysis
    //print out fpd infos
    //fpd->dump();
    //print out bbc infos
    //bbc->dump();

    //calling fortran pi0 finder from FPD
    float result[10], rin[10], bbcdif;
    int   iin[10];
    for(int i=0; i<10; i++){ result[i]=0.0; }
    unsigned short east=1500, west=1500;
    for(int i=0; i<8; i++){
      if(bbc->tdc(i)>0    && bbc->tdc(i)<east   ) east=bbc->tdc(i);
      if(bbc->tdc(i+16)>0 && bbc->tdc(i+16)<west) west=bbc->tdc(i+16);
    }
    if(east<1500 && west<1500) { bbcdif= (float)west-(float)east; }
    StMuTrack *t = (StMuTrack *)(* tracks)[LCP];
    rin[0]=zVertex;
    rin[1]=bbcdif;
    rin[2]=t->pt();
    rin[3]=t->eta();
    rin[4]=t->phi();
    iin[0]=token;
    iin[1]=bunchId7bit;
    iin[2]=triggerWord;
    iin[3]=nPrimTrack;
    iin[4]=runN;
    iin[5]=eventN;
    iin[6]=(int)east;
    iin[7]=(int)west;
    iin[8]=bunchId%120;
    int iadc[256];
    unsigned short * adc = fpd->adc();
    for(int i=0; i<256; i++){iadc[i] = (int) adc[i];}
    fpdpi0mass_(iadc, iin, rin, result);
    fpdPi0E      = result[0];
    fpdPi0Mass   = result[1];
    fpdPi0EShare = result[2];
    fpdPi0Eta    = result[3];
    fpdPi0Phi    = result[4]; 
    fpdPi0SmdDiff= result[5]; 
    cout << "fpdPi0Mass: ";
    if(fpdPi0Mass>0.0){
      for(int i=0; i<5; i++){cout << result[i] << " ";}; cout<< endl;
    }

    */
    return 0;
}
#endif /*__CINT__*/

Int_t StppEvent::correctedBunchId(){
    if (bunchId7bit>113) return (bunchId7bit-113)/2;
    else                 return (bunchId7bit+7)/2;
}
/*
StppEvent::TrackVec StppEvent::anaJetParticles(int ana, int jetIndex)
{
    TrackVec vec;
    int n = tracks->GetLast()+1;
    for (int i=0; i<n; ++i) {
	StppTrack* p = (StppTrack*)(*tracks)[i];
	if (p->anaJetIndex[ana]==jetIndex) {
	    vec.push_back(p);
	}
    }
    return vec;
}

StppEvent::TrackVec StppEvent::jetParticles(int jetIndex)
{
    TrackVec vec;
    int n = tracks->GetLast()+1;
    for (int i=0; i<n; ++i) {
	StppTrack* p = (StppTrack*)(*tracks)[i];
	if (p->jetIndex==jetIndex) {
	    vec.push_back(p);
	}
    }
    return vec;
}

void StppEvent::setClusterIndex(StProtoJet& pj, int ijet)
{
    typedef StProtoJet::FourVecList FourVecList;
    FourVecList& l = pj.list();
    
    for (FourVecList::iterator it=l.begin(); it!=l.end(); ++it) {
	AbstractFourVec* fv = *it;
	StppTrackFourVec* temp = dynamic_cast<StppTrackFourVec*>(fv);
	if (!temp) {
	    cout <<"StppEvent::setClusterIndex() ERROR:\tDowncast failed."<<endl;
	}
	else {
	    StppTrack* part = temp->particle();
	    part->clusterJetIndex = ijet;
	}
    }
}


void StppEvent::setConeIndex(StProtoJet& pj, int ijet)
{
    typedef StProtoJet::FourVecList FourVecList;
    FourVecList& l = pj.list();
    
    for (FourVecList::iterator it=l.begin(); it!=l.end(); ++it) {
	AbstractFourVec* fv = *it;
	StppTrackFourVec* temp = dynamic_cast<StppTrackFourVec*>(fv);
	if (!temp) {
	    cout <<"StppEvent::setConeIndex() ERROR:\tDowncast failed."<<endl;
	}
	else {
	    StppTrack* part = temp->particle();
	    part->coneJetIndex = ijet;
	}
    }
}
*/
