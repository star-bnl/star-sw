//////////////////////////////////////////////////////////////////////
//
// $Id: StppEvent.cxx,v 1.25 2004/01/23 01:30:18 akio Exp $
// $Log: StppEvent.cxx,v $
// Revision 1.25  2004/01/23 01:30:18  akio
// Adding 2004 ntuples
//
// Revision 1.24  2004/01/23 00:54:44  akio
// update for ntp2003
//
// Revision 1.23  2003/10/16 19:48:37  akio
// updates for 2003
//
// Revision 1.22  2003/09/23 20:43:09  akio
// *** empty log message ***
//
// Revision 1.21  2003/09/23 17:47:55  akio
// small fix for warnings
//
// Revision 1.20  2003/09/23 16:42:39  perev
// {} in case added
//
// Revision 1.19  2003/09/23 16:08:41  perev
// some inits added
//
// Revision 1.18  2003/09/22 14:28:25  akio
// Fix for RH8.0 and FPD layer1 info to ntuple
//
// Revision 1.17  2003/09/11 05:49:22  perev
// ansi corrs
//
// Revision 1.16  2003/09/02 17:59:01  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.15  2003/07/16 19:58:32  perev
// Cleanup of StTriggerData2003 at all
//
// Revision 1.14  2003/07/07 17:48:13  akio
// adding trigger id mask (daqbits/summary bits/daqTrgId)
//
// Revision 1.13  2003/05/14 18:00:24  akio
// New addition for 2003 data ntuple prodction
// Also fix a problem with MuTrack creating from StEvent tracks.
//
// Revision 1.12  2003/04/03 19:50:46  thenry
// Whae Major Evil Bug fix:
// muDstJets NOW is CLEARED before adding the jets found in the event, so
// that the first jet is now the first jet of the event, not the first jet of
// the FILE.
//
// Revision 1.11  2003/03/07 23:46:59  thenry
// Added Fill calls with different parameters
//
// Revision 1.10  2003/02/04 21:57:09  akio
// Improvments on pi0 reconstruction code and ntuple
//
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
#include <stdio.h>
#include <Stiostream.h>

#include "StEventTypes.h"
#include "StEvent.h"
#include "StTriggerData.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuTrackFourVec.h"
#include "StTriggerDetectorCollection.h"
#include "StCtbTriggerDetector.h"
#include "StuProbabilityPidAlgorithm.h"
#include "StuFtpcRefMult.hh"

//trigger data2003
#include "StTriggerData2003.h"
#include "StDaqLib/TRG/trgStructures2003.h"
//trigger data2004
#include "StTriggerData2004.h"
#include "StDaqLib/TRG/trgStructures2004.h"

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

StuProbabilityPidAlgorithm* mProbabilityPidAlgorithm;
extern "C" void fpdpi0mass_(int*,int*,float*,float*, int*, int*);
extern "C" void fillntp2003_(int*, float*, int*, float*);
extern "C" void fillntp2004_(int*, float*, int*, float*);
extern "C" void fillh1d_(int*, float*, float*);
extern "C" void fillh2d_(int*, float*, float*, float*);

#include "ntp2003.h"
#include "ntp2004.h"

StppEvent::StppEvent(){
    mudst=0;
    tracks = new TClonesArray ("StMuTrack",200);  //warning hardcoded # of track limits!
    trackChoice = 0; //primary track is default
#ifdef _Jet_
    jets = new TClonesArray ("StJet", 20);
    typedef StppJetAnalyzer* StppJetAnalyzerP;
    mAnalyzers = new StppJetAnalyzerP[MAXANALYZERS];
    numAnalyzers = 0;
    mStJetsMap = 0;
    
    jetR    = 0.7;
    mJetClusterRadius = 1.4;
    jetSeed = 1.5;
    jetCut  = 0.2;
#endif 
    infoLevel = 0;
    BunchIdDifference=-999;

    TrgDataType2003* trgd=0; FPDL012(trgd,0,0);
    clear();
}

StppEvent::~StppEvent() {
    TrgDataType2003* trgd=0; FPDL012(trgd,2,0);    
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
    LCP     =  0;
    LCPpt   =  0.0;
    LCPeta  = -9.0;
    LCPphi  = -9.0;
    sumPt   = 0.0;
    vectorSumPt = 0.0;
    weightedEta =-9.0;
    weightedPhi =-9.0;

    bbcAdcSumEast = 0;
    bbcAdcSumWest = 0;
    bbcNHitEast=0;
    bbcNHitWest=0;
    zVertexBbc=0.0;

    fpdESumNorth  = 0.0;
    fpdESumSouth  = 0.0;
    fpdESumTop    = 0.0;
    fpdESumBottom = 0.0;
    fpdESumSmdX   = 0.0;
    fpdESumSmdY   = 0.0;
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
    return fill(event, NULL);
}

Int_t StppEvent::fill(StMuDst* uDst){
    return fill(NULL, uDst);
}

Int_t StppEvent::fill(StEvent *event, StMuDst* uDst){
    mudst = uDst;
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

    if(!event && !uDst){
	cout << "StppEvent::fill()   Error:Neither StEvent nor Mudst exist!" << endl;
	return 1;
    }
  
    StMuEvent* muevent=0;
    if(!event && mudst) {muevent = mudst->event(); }
  
    //event info
    StTriggerData* trgdata=0;
    if(event){
	eventN = event->id() ;
	runN   = event->runId(); 
	time   = event->time(); 
	trgdata = event->triggerData();
	//if(trgdata){
	//  trgdata->dump();
	//}
    }else{
        eventN = muevent->eventInfo().id() ;
	runN   = muevent->eventInfo().runId(); 
	time   = muevent->eventInfo().time(); 
    }
    
    // Get primary tracks
    nPrimTrack = 0; 
    TClonesArray* mutracks=0;
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
	StTrackType type = primary;
	if (mProbabilityPidAlgorithm) delete mProbabilityPidAlgorithm;
	StuProbabilityPidAlgorithm*  mProbabilityPidAlgorithm = new StuProbabilityPidAlgorithm(*event);
	StMuTrack::setProbabilityPidAlgorithm(mProbabilityPidAlgorithm);
	switch(trackChoice){
	case 0: exnode = &(event->trackNodes()); type=primary; break;
	case 1: exnode = &(event->trackNodes()); type=global;  break;
	case 2: {
	    StL3Trigger* l3 = event->l3Trigger();
	    if(l3) exnode = &(l3->trackNodes());
	    type=global;}
	    break;
        default: assert(0);
	}    
	Int_t nnode=exnode->size();
	for( Int_t in=0; in<nnode; in++ ) {
	    UInt_t nprim = (*exnode)[in]->entries(type);
	    if(nprim==1){
		new((*tracks)[nPrimTrack++]) StMuTrack(event,(*exnode)[in]->track(type));
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
		LCPpt=pt;
		LCPeta=t->eta();
		LCPphi=t->phi();
	    }
	    sumPt += pt;
	    sumPx += pt * cos(t->phi());
	    sumPy += pt * sin(t->phi());
	    weightedEta += pt * t->eta();
	    nGoodTrack++;
	}
    }
  
    if(sumPt>0.0){
	vectorSumPt = ::sqrt(sumPx*sumPx+sumPy*sumPy);
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
      //2002 data
      if(runN<4000000){
        token = fpd->token();	
	if(runN<3007014){
	  static const short SmdXoff[60] = {
	    31 ,      38 ,      64 ,      60 ,     260 ,
	    255 ,      56 ,      32 ,      75 ,      73 ,
	    42 ,      45 ,      58 ,      63 ,     252 ,
	    248 ,      34 ,      38 ,      76 ,      67 ,
	    36 ,      46 ,      62 ,      61 ,     256 ,
	    251 ,      36 ,      36 ,      86 ,      78 ,
	    29 ,      42 ,      56 ,      61 ,     228 ,
	    251 ,      43 ,      30 ,      75 ,      68 ,
	    44 ,      40 ,      58 ,      62 ,     243 ,
	    229 ,      54 ,      34 ,      74 ,      76 ,
	    45 ,      46 ,      60 ,      60 ,     240 ,
	    263 ,      39 ,      34 ,      85 ,      81 };
	  static const short SmdYoff[100] = {
	    50 ,      38 ,      62 ,      60 ,     262 ,
	    237 ,      42 ,      34 ,      79 ,      81 ,
	    77 ,      67 ,      34 ,      35 ,     264 ,
	    253 ,      56 ,      55 ,      36 ,      46 ,
	    39 ,      39 ,     112 ,     107 ,      35 ,
	    45 ,      26 ,      21 ,      45 ,      60 ,
	    42 ,      32 ,     103 ,      98 ,      43 ,
	    46 ,      28 ,      28 ,      56 ,      58 ,
	    54 ,      46 ,     101 ,      98 ,      45 ,
	    50 ,      23 ,      25 ,      54 ,      64 ,
	    34 ,      35 ,     100 ,     100 ,      43 ,
	    43 ,      22 ,      20 ,      46 ,      50 ,
	    37 ,      43 ,     119 ,     123 ,      43 ,
	    49 ,      23 ,      29 ,      71 ,      67 ,
	    36 ,      44 ,     111 ,     107 ,      42 ,
	    40 ,      21 ,      23 ,      70 ,      49 ,
	    37 ,      31 ,      90 ,     104 ,      43 ,
	    47 ,      28 ,      20 ,      65 ,      73 ,
	    31 ,      33 ,     113 ,      97 ,      46 ,
	    49 ,      22 ,      27 ,      48 ,      68 };
	  static const short Northoff [12] = {
	    14 ,      70 ,      23 ,       3 ,       3 ,
	    146 ,     192 ,     176 ,     106 ,      22 ,
	    130 ,      32 };
	  static const short Southoff [16] = {
	    37 ,      33 ,      33 ,      34 ,      34 ,
	    37 ,      40 ,      34 ,      37 ,      31 ,
	    38 ,      39 ,      38 ,      35 ,      35 ,
	    36 };
	  static const short Topoff   [16] = {
	    14 ,      28 ,      28 ,      37 ,      41 ,
	    16 ,      32 ,      40 ,      37 ,      28 ,
	    11 ,      19 ,      16 ,      19 ,      53 ,
	    33 };
	  static const short Bottomoff[16] = {
	    105 ,      43 ,       8 ,      53 ,      29 ,
	    71 ,       78 ,      34 ,      32 ,      13 ,
	    29 ,       34 ,      44 ,      18 ,      42 ,
	    41 };
	  
	  unsigned short* adc = fpd->adc();
	  int j=0;
	  for(int i=0; i< 16; i++) { adc[j] = (adc[j] >    Topoff[i]) ? adc[j]-   Topoff[i] : 0;  j++;}
	  for(int i=0; i< 16; i++) { adc[j] = (adc[j] > Bottomoff[i]) ? adc[j]-Bottomoff[i] : 0;  j++;}
	  for(int i=0; i< 16; i++) { adc[j] = (adc[j] >  Southoff[i]) ? adc[j]- Southoff[i] : 0;  j++;}
	  for(int i=0; i< 12; i++) { adc[j] = (adc[j] >  Northoff[i]) ? adc[j]- Northoff[i] : 0;  j++;}
	  for(int i=0; i< 60; i++) { adc[j] = (adc[j] >   SmdXoff[i]) ? adc[j]-  SmdXoff[i] : 0;  j++;}
	  for(int i=0; i<100; i++) { adc[j] = (adc[j] >   SmdYoff[i]) ? adc[j]-  SmdYoff[i] : 0;  j++;}
	}
	
	fpdAdcSumNorth  = fpd->sumAdcNorth();
	fpdAdcSumSouth  = fpd->sumAdcSouth();
	fpdAdcSumTop    = fpd->sumAdcTop();
	fpdAdcSumBottom = fpd->sumAdcBottom();
	fpdAdcSumPres1  = fpd->sumAdcPreShower1();
	fpdAdcSumPres2  = fpd->sumAdcPreShower2();
	fpdAdcSumSmdX   = fpd->sumAdcSmdX();
	fpdAdcSumSmdY   = fpd->sumAdcSmdY();
	fpdSouthVeto    = fpd->southVeto();
	
	//North tower calibration
	static const float NGain1[12] = {  1.029, 0.738, 0.673, 0.522, 
					   1.124, 0.906, 0.707, 0.597,
					   1.076, 0.756, 0.786, 0.454}; 
	static const float NGain2[12] = {  0.965, 0.814, 0.644, 0.543, 
					   1.124, 0.912, 0.674, 0.579, 
					   1.036, 0.759, 0.824, 0.433};
	const float* gain;
	if(runN <= 3014027){gain=NGain1;}
	else {gain=NGain2;}
	for(int i=0; i<12; i++){
	  fpdENorth[i] = fpd->north(i+1) * gain[i] * 0.04;
	  fpdESumNorth += fpdENorth[i];
	}

	//North SMD calibration
	static const float SmdXGain[60] = {
	  0.81925 , 1.58648 , 1.30039 , 1.93758 , 0.68921 ,
	  0.84525 , 0.80624 , 1.43043 , 0.72822 , 1.02731 ,
	  0.65020 , 0.93628 , 1.14434 , 1.84655 , 0.74122 ,
	  0.81925 , 0.89727 , 1.37841 , 0.67620 , 1.07932 ,
	  0.78023 , 1.13134 , 0.85826 , 1.46944 , 0.94928 ,
	  0.84525 , 0.98830 , 1.19636 , 0.70221 , 1.04031 ,
	  1.05332 , 1.52146 , 1.13134 , 1.96359 , 1.06632 ,
	  1.09233 , 1.07932 , 1.54746 , 0.81925 , 1.40442 ,
	  1.07932 , 1.07932 , 1.13134 , 2.01560 , 0.93628 ,
	  0.81925 , 1.05332 , 0.88427 , 0.94928 , 0.63719 ,
	  0.79324 , 0.92328 , 1.27438 , 3.04291 , 0.74122 ,
	  1.09233 , 1.01430 , 0.96229 , 0.79324 , 0.87126 };
	static const float SmdYGain[100] = {
	  1.07932 , 0.88427 , 1.11834 , 0.83225 , 0.78023 ,
	  0.65020 , 0.80624 , 0.70221 , 0.76723 , 0.62419 ,
	  0.67620 , 0.62419 , 0.76723 , 0.87126 , 0.74122 ,
	  0.78023 , 0.92328 , 1.04031 , 0.78023 , 0.84525 ,
	  0.78023 , 0.85826 , 0.42913 , 0.45514 , 1.59948 ,
	  1.85956 , 1.62549 , 2.71782 , 0.76723 , 0.63719 ,
	  1.26138 , 1.09233 , 0.46814 , 0.48114 , 3.27698 ,
	  2.36671 , 2.82185 , 2.74382 , 1.37841 , 0.68921 ,
	  1.39142 , 1.13134 , 0.55917 , 2.32770 , 2.89987 ,
	  2.43173 , 2.65280 , 3.43303 , 1.54746 , 0.88427 ,
	  1.11834 , 1.61248 , 0.61118 , 0.91027 , 2.28869 ,
	  3.65410 , 2.50975 , 3.87516 , 0.92328 , 1.00130 ,
	  1.15735 , 0.85826 , 0.46814 , 0.48114 , 2.40572 ,
	  1.84655 , 8.98570 , 2.36671 , 0.96229 , 0.84525 ,
	  1.20936 , 1.49545 , 0.65020 , 0.53316 , 2.89987 ,
	  4.10923 , 3.47204 , 2.96489 , 1.01430 , 1.56047 ,
	  1.31339 , 1.84655 , 0.63719 , 0.59818 , 2.95189 ,
	  4.36931 , 3.15995 , 3.28999 , 1.13134 , 1.71651 ,
	  1.37841 , 1.13134 , 0.80624 , 0.59818 , 2.54876 ,
	  2.32770 , 3.12094 , 2.00260 , 1.00130 , 0.98830 };
	
	for(int i=0; i<=60; i++){
	  fpdESmdX[i] = fpd->smdx(i+1) * SmdXGain[i];
	  fpdESumSmdX += fpdESmdX[i];
	}
	for(int i=0; i<=100; i++){
	  fpdESmdY[i] = fpd->smdy(i+1) * SmdYGain[i];
	  fpdESumSmdY += fpdESmdY[i];
	}
      } 
    }
    
    //l0 trigger info
    if(l0){
	token = l0->triggerToken();
	triggerWord = l0->triggerWord();
	bunchId = l0->bunchCrossingId();
	bunchId7bit = l0->bunchCrossingId7bit(runN);
	doubleSpinIndex = l0->spinBits(runN);
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
    for(Int_t anaNum = 0; anaNum < numAnalyzers; anaNum++){
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
      muDstJets->Clear();
      
      if (cJets.size()>0) foundJet = true;
      
      for(JetList::iterator it=cJets.begin(); it!=cJets.end(); ++it) {
	muDstJets->addProtoJet(*it);
      }
    }  
    
#endif
    
    //print out fpd infos
    //fpd->dump();
    //print out bbc infos
    //bbc->dump();
    
    //Bridge to fortran/ntuple filling
    if(runN<4000000){
      //calling fortran pi0 finder from FPD
      float result[10], rin[10], bbcdif=-999.9;
      int iin[10], ibbca[32], ibbct[32];
      for(int i=0; i<10; i++){ result[i]=0.0; }
      unsigned short east=1500, west=1500;
      for(int i=0; i<8; i++){
	if(bbc->tdc(i)>0    && bbc->tdc(i)<east   ) east=bbc->tdc(i);
	if(bbc->tdc(i+16)>0 && bbc->tdc(i+16)<west) west=bbc->tdc(i+16);
      }
      if(east<1500 && west<1500) { bbcdif= (float)west-(float)east; }
      rin[0]=zVertex;
      rin[1]=bbcdif;
      rin[2]=LCPpt;
      rin[3]=LCPeta;
      rin[4]=LCPphi;
      rin[5]=(float)zdcTdcEast;
      rin[6]=(float)zdcTdcWest;
      rin[7]=(float)zdcEast;
      rin[8]=(float)zdcWest;
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
      for(int i=0; i<32; i++) {ibbca[i] = (int) bbc->adc(i); ibbct[i] = (int) bbc->tdc(i);}
      fpdpi0mass_(iadc, iin, rin, result, ibbca, ibbct);
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
    }else if(runN<4300000){
      if(trgdata){
	int year = trgdata->year();
	if(year==2003){
	  StTriggerData2003* t2003=(StTriggerData2003*)trgdata;
	  TrgDataType2003* trgd=t2003->getTriggerStructure2003();
	  int iin[100], iout[100];
	  float rin[100], rout[100];
	  iin[0]=runN;
	  
	  int nbuf=1;
	  int npre=t2003->numberOfPreXing();
	  int npost=t2003->numberOfPostXing();
	  if (npre < npost) {
	    nbuf = (2*npost) + 1;
	  } else {
	    nbuf = (2*npre) + 1;
	  }
	  int GB = FPDL012(trgd,1,nbuf);
	  if(GB!=0) {printf("Taking %d bunch for layer0 FPD East DSM\n",GB);}
	    
	  ntp2003_.event       = eventN;
	  ntp2003_.BChi        = trgdata->bunchCounterHigh();
	  ntp2003_.BClo        = trgdata->bunchCounterLow();
	  ntp2003_.Token       = trgdata->token();
	  ntp2003_.TrgWd       = trgdata->triggerWord();
	  ntp2003_.TrgId       = event->triggerIdCollection()->nominal()->mask();
	  printf("Trigger wd = %d  id=%d\n",ntp2003_.TrgWd,ntp2003_.TrgId);
	  ntp2003_.prepost     = 0;
	  ntp2003_.bunchId     = trgdata->bunchId48Bit();
	  ntp2003_.bunchid7bit = trgdata->bunchId7Bit();
	  ntp2003_.spinBit     = trgdata->spinBit();
	  ntp2003_.NPrimTrk    = nPrimTrack;
	  ntp2003_.NTPCTrk     = nGoodTrack;
	  ntp2003_.NEastFTPCTrk= uncorrectedNumberOfFtpcEastPrimaries(*event);
	  ntp2003_.xVertex     = xVertex;
	  ntp2003_.yVertex     = yVertex;
	  ntp2003_.zVertex     = zVertex;	  
	  ntp2003_.ntrk        = 0;
	  int n=0;
	  if(nPrimTrack>0) {
	    for(int i=0; i<=tracks->GetLast(); i++) {
	      StMuTrack *t = (StMuTrack *)(* tracks)[i];
	      if(t->id() > 0){
		ntp2003_.trknhit[n]=t->nHits();
		ntp2003_.trkpt[n]  =t->pt() * t->charge();
		ntp2003_.trketa[n] =t->eta();
		ntp2003_.trkphi[n] =t->phi();
		ntp2003_.trkdcax[n] =t->dcaGlobal().x();
		ntp2003_.trkdcay[n] =t->dcaGlobal().y();
		ntp2003_.trkdcaz[n] =t->dcaGlobal().z();
		n++;
		//		printf("DCA n,dcax/y/z,eta = %d %f %f %f %f\n",n,t->dcaGlobal().x(),t->dcaGlobal().y(),t->dcaGlobal().z(),t->eta());
		ntp2003_.ntrk=n;
	      }	    
	    }
	  }
	  for(int i=0; i<8;   i++) {ntp2003_.VTXDSM[i]=trgd->TrgSum.DSMdata.VTX[i];
	                            ntp2003_.FPDDSM[i]=trgd->TrgSum.DSMdata.FPD[i];
				    ntp2003_.FPDENSL1[i]=trgd->rawTriggerDet[0].FPDEastNSLayer1[i];
				    ntp2003_.FPDETBL1[i]=trgd->rawTriggerDet[0].FPDEastTBLayer1[i];
				    ntp2003_.FPDWNSL1[i]=trgd->rawTriggerDet[0].FPDWestNSLayer1[i];
				    ntp2003_.FPDWTBL1[i]=trgd->rawTriggerDet[0].FPDWestTBLayer1[i];
	                            ntp2003_.FPDTDC[i]=(fpd->tdc())[i];}
	  for(int i=0; i<16;  i++) {ntp2003_.ZDC[i]=trgd->rawTriggerDet[0].ZDC[i];}
	  for(int i=0; i<256; i++) {ntp2003_.CTB[i]=trgd->rawTriggerDet[0].CTB[i];
	                            ntp2003_.FPDADC[i]=(fpd->adc())[i];}
	  for(int i=0; i<80;  i++) {ntp2003_.BBC[i]=trgd->rawTriggerDet[0].BBC[i];}
	  for(int i=0; i<112; i++) {ntp2003_.FPDENS[i]=trgd->rawTriggerDet[GB].FPDEastNSLayer0[i];
	                            ntp2003_.FPDWNS[i]=trgd->rawTriggerDet[0].FPDWestNSLayer0[i];}
	  for(int i=0; i<64; i++)  {ntp2003_.FPDETB[i]=trgd->rawTriggerDet[GB].FPDEastTBLayer0[i];
	                            ntp2003_.FPDWTB[i]=trgd->rawTriggerDet[0].FPDWestTBLayer0[i];}
	  for(int i=0; i<32;  i++) {ntp2003_.FPDWEST[i]   =bbc->adc(i); 
	                            ntp2003_.FPDWEST[i+32]=bbc->tdc(i);}
	  fillntp2003_(iin,rin,iout,rout);
	}else{
	  printf("StppEvent: StTriggerData have wrong year %d\n",year);
	}
      }else{
	printf("StppEvent: No StTriggerData found\n");
      }
    }else if(runN<5200000){
      if(trgdata){
	int year = trgdata->year();
	if(year==2004){
	  StTriggerData2004* t2004=(StTriggerData2004*)trgdata;
	  TrgDataType2004* trgd=t2004->getTriggerStructure2004();
	  int iin[100], iout[100];
	  float rin[100], rout[100];
	  iin[0]=runN;
	  
	  int nbuf=1;
	  int npre=t2004->numberOfPreXing();
	  int npost=t2004->numberOfPostXing();
	  if (npre < npost) {
	    nbuf = (2*npost) + 1;
	  } else {
	    nbuf = (2*npre) + 1;
	  }
	  int GB=0;
	  //int GB = FPDL012(trgd,1,nbuf);
	  //if(GB!=0) {printf("Taking %d bunch for layer0 FPD East DSM\n",GB);}
	    
	  ntp2004_.event       = eventN;
	  ntp2004_.BChi        = trgdata->bunchCounterHigh();
	  ntp2004_.BClo        = trgdata->bunchCounterLow();
	  ntp2004_.Token       = trgdata->token();
	  ntp2004_.TrgWd       = trgdata->triggerWord();
	  ntp2004_.TrgId       = event->triggerIdCollection()->nominal()->mask();
	  printf("Trigger wd = %d  id=%d\n",ntp2004_.TrgWd,ntp2004_.TrgId);
	  ntp2004_.prepost     = 0;
	  ntp2004_.bunchId     = trgdata->bunchId48Bit();
	  ntp2004_.bunchid7bit = trgdata->bunchId7Bit();
	  ntp2004_.spinBit     = trgdata->spinBit();
	  ntp2004_.NPrimTrk    = nPrimTrack;
	  ntp2004_.NTPCTrk     = nGoodTrack;
	  ntp2004_.NEastFTPCTrk= uncorrectedNumberOfFtpcEastPrimaries(*event);
	  ntp2004_.xVertex     = xVertex;
	  ntp2004_.yVertex     = yVertex;
	  ntp2004_.zVertex     = zVertex;	  
	  ntp2004_.ntrk        = 0;
	  int n=0;
	  if(nPrimTrack>0) {
	    for(int i=0; i<=tracks->GetLast(); i++) {
	      StMuTrack *t = (StMuTrack *)(* tracks)[i];
	      if(t->id() > 0){
		ntp2004_.trknhit[n]=t->nHits();
		ntp2004_.trkpt[n]  =t->pt() * t->charge();
		ntp2004_.trketa[n] =t->eta();
		ntp2004_.trkphi[n] =t->phi();
		ntp2004_.trkdcax[n] =t->dcaGlobal().x();
		ntp2004_.trkdcay[n] =t->dcaGlobal().y();
		ntp2004_.trkdcaz[n] =t->dcaGlobal().z();
		n++;
		//		printf("DCA n,dcax/y/z,eta = %d %f %f %f %f\n",n,t->dcaGlobal().x(),t->dcaGlobal().y(),t->dcaGlobal().z(),t->eta());
		ntp2004_.ntrk=n;
	      }	    
	    }
	  }
	  for(int i=0; i<8;   i++) {ntp2004_.VTXDSM[i]=trgd->TrgSum.DSMdata.VTX[i];
	                            ntp2004_.FPDDSM[i]=trgd->TrgSum.DSMdata.FPD[i];
				    ntp2004_.FPDENSL1[i]=trgd->rawTriggerDet[0].FPDEastNSLayer1[i];
				    ntp2004_.FPDETBL1[i]=trgd->rawTriggerDet[0].FPDEastTBLayer1[i];
				    ntp2004_.FPDWNSL1[i]=trgd->rawTriggerDet[0].FPDWestNSLayer1[i];
				    ntp2004_.FPDWTBL1[i]=trgd->rawTriggerDet[0].FPDWestTBLayer1[i];
	                            ntp2004_.FPDTDC[i]=(fpd->tdc())[i];}
	  for(int i=0; i<16;  i++) {ntp2004_.ZDC[i]=trgd->rawTriggerDet[0].ZDC[i];}
	  for(int i=0; i<32;  i++) {ntp2004_.ZDCSMD[i]=trgd->rawTriggerDet[0].ZDCSMD[i];}
	  for(int i=0; i<256; i++) {ntp2004_.CTB[i]=trgd->rawTriggerDet[0].CTB[i];
	                            ntp2004_.FPDADC[i]=(fpd->adc())[i];}
	  for(int i=0; i<80;  i++) {ntp2004_.BBC[i]=trgd->rawTriggerDet[0].BBC[i];}
	  for(int i=0; i<112; i++) {ntp2004_.FPDENS[i]=trgd->rawTriggerDet[GB].FPDEastNSLayer0[i];
	                            ntp2004_.FPDWNS[i]=trgd->rawTriggerDet[0].FPDWestNSLayer0[i];}
	  for(int i=0; i<64; i++)  {ntp2004_.FPDETB[i]=trgd->rawTriggerDet[GB].FPDEastTBLayer0[i];
	                            ntp2004_.FPDWTB[i]=trgd->rawTriggerDet[0].FPDWestTBLayer0[i];}
	  for(int i=0; i<32;  i++) {ntp2004_.FPDWEST[i]   =bbc->adc(i); 
	                            ntp2004_.FPDWEST[i+32]=bbc->tdc(i);}
	  fillntp2004_(iin,rin,iout,rout);
	}else{
	  printf("StppEvent: StTriggerData have wrong year %d\n",year);
	}
      }else{
	printf("StppEvent: No StTriggerData found\n");
      }
    }      
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

int StppEvent::FPDL012(TrgDataType2003* trgd, int mode, int nbuf){
  int i,j,k,l,p,goodbunch;
  int l2[2][4], l1sum[2][4], l02sum[2][4], d02[2][4], d12[2][4];
  int l1[2][4][4],l01sum[2][4][4], d01[2][4][4];
  const int SUMTHR=40;
  const int DIFFTHR=3;
  const int maxch[4]={4,4,2,2};

  static int ntot;
  static int n[2][4][10];
  static int nn[2][4][4][10];
  static int match[11];
  
  goodbunch=0;
  if(mode==0) {
    ntot=0;
    for(i=0; i<11; i++){match[i]=0;}
    for(i=0; i<2; i++){
      for(j=0; j<4; j++){
	for(k=0; k<10; k++){
	  n[i][j][k]=0;
	  for(l=0; l<4; l++){
	    nn[i][j][l][k]=0;
	  }
	}
      }
    }
  }else if(mode==1){
    ntot++;    
    printf("# of buffer recorded=%d\n",nbuf);
    checkFPDL012(trgd,0,0,l2,l1sum,l02sum,d02,d12,l1,l01sum,d01);
    for(i=0; i<2; i++){
      for(j=0; j<4; j++){
	if(i==1 && (j==0 || j==2)) continue;
	n[i][j][0]++; // Totla # of events
	if(l2[i][j]>SUMTHR){ 
	  n[i][j][1]++; //Total # of events with SUM>SUMTHR
	  if(abs(d12[i][j])>DIFFTHR){ 
	    printf("!!! L1-L2 inconsistent!!!! %d %d\n",i,j);
	  }else{
	    n[i][j][2]++; //L1-L2 consistent
	    if(abs(d02[i][j])<DIFFTHR){ 
	      n[i][j][3]++; //L0-L2 consistent = good events
	    }else{  
	      n[i][j][4]++;  //L0-L2 inconsistent at pre/post=0, bad event	      
	      printf("L0-L2 inconsistency found in %d %d\n",i,j);
	      for(p=1; p<nbuf; p++){
		checkFPDL012(trgd,p,0,l2,l1sum,l02sum,d02,d12,l1,l01sum,d01);
		if(abs(d02[i][j])<DIFFTHR){ 
		  n[i][j][5]++; //L0-L2 consistent at pre/post		  		  
		  printf("Found L0-L2 matching in prepost=%d\n",p);
		  int isBad=0;
		  for(int k=0; k<maxch[j]; k++){
		    if(abs(d01[i][j][k])>DIFFTHR){
		      isBad=1; break;
		    }
		  }
		  if(isBad==0){
		    n[i][j][6]++; //L0-L1 consistent at pre/post		  		  
		    printf("Found L0-L1 matching in prepost=%d\n",p);
		    if(goodbunch!=0 && goodbunch!=p){
		      n[i][j][7]++; //Multiple match and different!!!
		      printf("Found different bunch in a event!!!!\n");		      
		    }else{
		      match[p]++;
		      goodbunch=p;
		    }
		    break;
		  }else{
		    printf("Found L0-L1 does not match in prepost=%d\n",p);
		  }
		}
	      }
	      if(goodbunch==0){
		n[i][j][8]++; //found no match
		printf("Found no matching\n");
	      }
	    }
	  }
	}
      }
    }
  }else{
    const char* tit[10]={"Total       ",
			 "L2>40ch     ",
			 "L1-L2 OK    ",
			 "L0-L2 OK    ",
			 "L0-L2 BAD   ",
			 "Found Match ",
			 "L1 match too",
			 "Mult Match  ",
			 "NoMatch/BAD ",
			 "------------"};
    printf("FPDL012  Total Analyzed Event = %d\n",ntot);
    printf("FPDL012             :       EN        ES        ET        EB |       WS        WB \n");
    for(i=0; i<9; i++){
      printf("FPDL012 %s: %8d  %8d  %8d  %8d | %8d  %8d\n",tit[i]
	     ,n[0][0][i],n[0][1][i],n[0][2][i],n[0][3][i]
	     ,n[1][1][i],n[1][3][i]);
    }
    printf("FPDL012-SUMMARY %d ",ntot);
    for(i=1; i<9; i++){
      printf("%d %d %d %d %d %d : "
	     ,n[0][0][i],n[0][1][i],n[0][2][i],n[0][3][i]
	     ,n[1][1][i],n[1][3][i]);
    }
    printf(" / ");
    for(i=0; i<11; i++){printf("%d ",match[i]);}
    printf("\n");
  }
  return goodbunch;
}

void StppEvent::checkFPDL012(TrgDataType2003* trgd, int pp, int print,
			     int l2[2][4], int l1sum[2][4], int l02sum[2][4], 
			     int d02[2][4], int d12[2][4],
			     int l1[2][4][4], int l01sum[2][4][4], int d01[2][4][4]){
  int i,j,k;
  static const int adr[8] ={3,2,1,0,7,6,5,4};

  for(i=0; i<2; i++){
    for(j=0; j<4; j++) {
      l2[i][j]=0; l1sum[i][j]=0; l02sum[i][j]=0; d02[i][j]=0; d12[i][j]=0;
      for(k=0; k<4; k++) {
	l1[i][j][k]=0; l01sum[i][j][k]=0; d01[i][j][k]=0;
      }
    }
  }    

  //Layer0 info
  static const int sl[11]={6,7,8,9,10,11,12,14,15,16,17};
  static const int ch[16]={7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};
  for(i=0; i<11; i++){
    int slot=sl[i];
    for(j=0; j<16; j++){
      int chan=ch[j];
      int add = i*16+j;
      if(slot>13) { add = (slot-14)*16+j; }
      if(chan != 15) {
	//printf("slot=%d chan=%d add=%d\n",slot,chan,add);
	//east
	if(slot==6)  {l01sum[0][0][0]+=trgd->rawTriggerDet[pp].FPDEastNSLayer0[add];}
	if(slot==7)  {l01sum[0][0][1]+=trgd->rawTriggerDet[pp].FPDEastNSLayer0[add];}
	if(slot==8)  {l01sum[0][0][2]+=trgd->rawTriggerDet[pp].FPDEastNSLayer0[add];}
	if(slot==9)  {
	  if(chan<7) {l01sum[0][0][3]+=trgd->rawTriggerDet[pp].FPDEastNSLayer0[add];}
	  else if(chan<14)       
	             {l01sum[0][1][0]+=trgd->rawTriggerDet[pp].FPDEastNSLayer0[add];}
	}
	if(slot==10) {l01sum[0][1][1]+=trgd->rawTriggerDet[pp].FPDEastNSLayer0[add];}
	if(slot==11) {l01sum[0][1][2]+=trgd->rawTriggerDet[pp].FPDEastNSLayer0[add];}
	if(slot==12) {l01sum[0][1][3]+=trgd->rawTriggerDet[pp].FPDEastNSLayer0[add];}
	if(slot==14) {l01sum[0][2][0]+=trgd->rawTriggerDet[pp].FPDEastTBLayer0[add];}
	if(slot==15) {
	  if(chan<10){l01sum[0][2][1]+=trgd->rawTriggerDet[pp].FPDEastTBLayer0[add];}
	}
	if(slot==16) {l01sum[0][3][0]+=trgd->rawTriggerDet[pp].FPDEastTBLayer0[add];}
	if(slot==17) {
	  if(chan<10){l01sum[0][3][1]+=trgd->rawTriggerDet[pp].FPDEastTBLayer0[add];}	
	}
	//west
	if(slot==6)  {l01sum[1][1][0]+=trgd->rawTriggerDet[pp].FPDWestNSLayer0[add];}
	if(slot==7)  {l01sum[1][1][1]+=trgd->rawTriggerDet[pp].FPDWestNSLayer0[add];}
	if(slot==8)  {l01sum[1][1][2]+=trgd->rawTriggerDet[pp].FPDWestNSLayer0[add];}
	if(slot==9)  {
          if(chan<10){l01sum[1][1][3]+=trgd->rawTriggerDet[pp].FPDWestNSLayer0[add];}
	}
	if(slot==10) {l01sum[1][3][0]+=trgd->rawTriggerDet[pp].FPDWestNSLayer0[add];}
	if(slot==11) {l01sum[1][3][1]+=trgd->rawTriggerDet[pp].FPDWestNSLayer0[add];}
      }
    }
  }

  //Layer1 info
  static const int ns[8] ={0,0,0,0,1,1,1,1};
  static const int ans[8]={0,1,2,3,0,1,2,3};
  static const int tb[4] ={2,2,3,3};
  static const int atb[4]={0,1,0,1};
  for(i=0; i<8; i++){
    //printf("nstb=%d ch=%d add=%d\n",ns[i],ans[i],adr[i]);
    l1[0][ns[i]][ans[i]] = trgd->rawTriggerDet[0].FPDEastNSLayer1[adr[i]];
    d01[0][ns[i]][ans[i]] = l01sum[0][ns[i]][ans[i]]-l1[0][ns[i]][ans[i]];
    l1sum[0][ns[i]]  += l1[0][ns[i]][ans[i]];
    l02sum[0][ns[i]] += l01sum[0][ns[i]][ans[i]];
  }
  for(i=0; i<4; i++){
    //printf("nstb=%d ch=%d add=%d\n",tb[i],atb[i],adr[i]);
    l1[0][tb[i]][atb[i]] = trgd->rawTriggerDet[0].FPDEastTBLayer1[adr[i]];
    d01[0][tb[i]][atb[i]] = l01sum[0][tb[i]][atb[i]]-l1[0][tb[i]][atb[i]];
    l1sum[0][tb[i]] += l1[0][tb[i]][atb[i]];
    l02sum[0][tb[i]] += l01sum[0][tb[i]][atb[i]];
  }
  static const int adr2[6] ={3,2,1,0,6,5};
  static const int sb[6] ={1,1,1,1,3,3};
  static const int asb[6]={0,1,2,3,0,1};
  //  for(i=0; i<8;  i++){
  //  printf("%d\n",trgd->rawTriggerDet[0].FPDWestNSLayer1[i]);
  // }
  for(i=0; i<6; i++){
    l1[1][sb[i]][asb[i]] = trgd->rawTriggerDet[0].FPDWestNSLayer1[adr2[i]];
    d01[1][sb[i]][asb[i]] = l01sum[1][sb[i]][asb[i]]-l1[1][sb[i]][asb[i]];
    l1sum[1][sb[i]]  += l1[1][sb[i]][asb[i]];
    l02sum[1][sb[i]] += l01sum[1][sb[i]][asb[i]];
  }
  
  //Layer2 info
  static const int ew[8]  ={0,0,0,0,1,1,1,1};
  static const int nstb[8]={0,1,2,3,1,3,0,2};
  static const int nbit[8]={16384,16384,8192,8192,16384,16384,8192,8192};
  for(i=0; i<8; i++){
    l2[ew[i]][nstb[i]]=trgd->TrgSum.DSMdata.FPD[adr[i]] % nbit[i];    
    d02[ew[i]][nstb[i]]=l02sum[ew[i]][nstb[i]]-l2[ew[i]][nstb[i]];
    d12[ew[i]][nstb[i]]= l1sum[ew[i]][nstb[i]]-l2[ew[i]][nstb[i]];
  }
  
  if(print==1){
    printf("*** CHECK FPD FOR PRE/POST=%d***\n",pp);
    printf("EW  Mod Ch  L0   L1   0-1\n");
    int ih;
    float d;
    float p=(float)pp;
    static float one=1.0;
    for(i=0; i<2; i++){
      for(j=0; j<4; j++){
	ih = 1000*(i+1) + 100*(j+1); d=(float)d02[i][j];
	fillh2d_(&ih, &d, &p, &one);
	ih = 1000*(i+1) + 100*(j+1) + 1; d=(float)d12[i][j];
	if(pp==0) { fillh1d_(&ih, &d, &one); }
	for(k=0; k<4; k++){
	  if(i==1 && (j==0 || j==2)) break;
	  if(j>1 && k>1) break;
	  printf("%3d %2d %2d %4d %4d %4d\n",i,j,k,l01sum[i][j][k],l1[i][j][k],d01[i][j][k]);
	  ih=1000*(i+1) + 100*(j+1) + 10*(1+k); d=(float)d01[i][j][k];
	  fillh2d_(&ih,&d,&p,&one);
	}
      }
    }
    printf("  : EN     ES     ET    EB |  WS    WB \n");
    printf("L0: %4d  %4d  %4d  %4d | %4d  %4d\n",l02sum[0][0],l02sum[0][1],l02sum[0][2],l02sum[0][3]
	                                        ,l02sum[1][1],l02sum[1][3]);
    printf("L1: %4d  %4d  %4d  %4d | %4d  %4d\n",l1sum[0][0],l1sum[0][1],l1sum[0][2],l1sum[0][3]
	                                        ,l1sum[1][1],l1sum[1][3]);
    printf("L2: %4d  %4d  %4d  %4d | %4d  %4d\n",l2[0][0],l2[0][1],l2[0][2],l2[0][3]
	                                        ,l2[1][1],l2[1][3]);
    printf("02: %4d  %4d  %4d  %4d | %4d  %4d\n",d02[0][0],d02[0][1],d02[0][2],d02[0][3]
	                                        ,d02[1][1],d02[1][3]);
    printf("12: %4d  %4d  %4d  %4d | %4d  %4d\n",d12[0][0],d12[0][1],d12[0][2],d12[0][3]
	                                        ,d12[1][1],d12[1][3]);  
  }
}
