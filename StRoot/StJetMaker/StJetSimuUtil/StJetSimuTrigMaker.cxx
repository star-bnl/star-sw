//Author :: Renee Fatemi
//
// Sept 21 2004 -- Maker produces Trigger from BBC, EEMC, EMC
//


#include "StJetMaker/StJetSimuUtil/StJetSimuTrigMaker.h"
#include "StChain.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEmcPoint.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "TH1F.h"
#include "TH2F.h"
#include "StEvent.h"
#include "StEvent/StBbcTriggerDetector.h"
#include "TClonesArray.h"

ClassImp(StJetSimuTrigMaker)

    StJetSimuTrigMaker::StJetSimuTrigMaker(const char *name):StMaker(name){
    }

StJetSimuTrigMaker::~StJetSimuTrigMaker(){
}

/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StJetSimuTrigMaker::Init(){
    muDstMaker = (StMuDstMaker*)GetMaker("MuDst");
    assert(muDstMaker);
    muEvent= new StMuEvent();
    muEmcCol = new StMuEmcCollection();
    points = new StMuEmcPoint();
    primTrack = new StMuTrack();

    evtID=0;

    bbcTrig=0;  
    for (int i=0;i<BBCadcNum;i++) {
	BBCadc[i]=0;
    }

    BHTmaxt=0;   //Hold HT for whole Barrel
    BJPmaxt=0;   //Holds max JP sum for whole Barrel
    BJPsumt=0;  //Holds sum of all JP in Barrel
    EHTmaxt=0;   //Hold HT for whole EEMC
    EJPmaxt=0;   //Holds max JP sum for whole EEMC
    EJPsumt=0;  //Holds sum of all JP in EEMC

    pointArray=1000;
    numPoints=0;
    for (int count=0; count<pointArray;count++){
	pointE[count]=0;
	pointPhi[count]=0;
	pointEta[count]=0;
	sinTheta[count]=0;
    }

    nTracks=0;
    for (int j=0;j<100;j++){
	qTrack[j]=0;
	ptTrack[j]=0;
	phiTrack[j]=0;
	etaTrack[j]=0;
    }


    return StMaker::Init();
}

Int_t StJetSimuTrigMaker::Make()
{
    //is muDstMaker...
    StMuDst* dst = muDstMaker->muDst();
    assert(dst);
    muEvent = dst->event();
    assert(muEvent);
    
    StBbcTriggerDetector *bbc=&(muEvent->bbcTriggerDetector());
    
    int Npmt=bbc->numberOfPMTs();
    if (print)  bbc->dump();
    
    int Wbbc=0;
    int Ebbc=0;
    for (int pmt=0;pmt<Npmt;pmt++){    
	BBCadc[pmt]=bbc->adc(pmt);
	int bbcadc=bbc->adc(pmt);
	if (bbcadc>5) {
	    if (pmt<16) {
		if (print) cout << "BBC EAST = true" << endl;
		Ebbc=1;
	    }
	    if (23<pmt && pmt<40) {
		if (print) cout << "BBC WEST = true" << endl;
		Wbbc=1;
	    }
	}
    }

    bbcTrig=0;
    if ((Ebbc==1)&&(Wbbc==1)){
	bbcTrig=1;
    }

    /*Maybe it was mentioned already, but the best way to get rid of Ftpc
      tracks is to look in the tracktopologyMap:
      track->topologyMap()->hasHitInDetector(kTpcId)
      will give you only tracks with a Tpc part. Alternatively, do:
      !track->topologyMap()->hasHitInDetector(kFtpcEastId) &&
      !track->topologyMap()->hasHitInDetector(kFtpcWestId)
      This works for old and new data and for StEvent and MuDst.

      You need to check id()>0 for all tracks for sure. You can ask
      nHits()>10 for TPC tracks and this will cut away FTPC tracks
      (since they have max 10 hits).
      If you really want to see "good" tracks and talk about pt
      distributions and such, you need more quality cuts on tracks,
      like Jan did for his LCP analysis. At minimum you may want
      to have nHitsFit/nHitsPoss>50%(or nHits>20~25) and DCA<3cm.
    */ 

    nTracks=muDstMaker->muDst()->numberOfPrimaryTracks();
    if (print) printf(" nTracks =%d\n",nTracks);
    for (int i=0;i<nTracks;i++){
	primTrack=muDstMaker->muDst()->primaryTracks(i);
	ptTrack[i]=primTrack->pt();
	etaTrack[i]=primTrack->eta();
	phiTrack[i]=primTrack->phi();
	qTrack[i]=primTrack->charge();
	if (print) printf("Tracks #%d has pT=%f, eta=%f, phi=%f and q=%d\n",i,primTrack->pt(),primTrack->eta(),primTrack->phi(),primTrack->charge());
	if (print) printf("Tracks #%d has pT=%f, eta=%f, phi=%f and q=%d\n",i,ptTrack[i],etaTrack[i],phiTrack[i],qTrack[i]);
    }
  
    StMuEvent* muEve = muDstMaker->muDst()->event();
    StEventInfo &info=muEve->eventInfo();
    evtID=info.id();
    if (print) cout << "Event # = "<< info.id() << "evtID=  " << evtID <<endl;

  
    // set array values to zero before each event
    memset(jpBsum,0,sizeof(jpBsum));
    memset(jpBmax,0,sizeof(jpBmax));
    memset(jpB_hit_num,0,sizeof(jpB_hit_num));
    memset(tpBsum,0,sizeof(tpBsum));
    memset(tpBmax,0,sizeof(tpBmax));

  
    // set array values to zero before each event
    memset(jpEsum,0,sizeof(jpEsum));
    memset(jpEmax,0,sizeof(jpEmax));
    memset(jpE_hit_num,0,sizeof(jpE_hit_num));
    memset(tpEsum,0,sizeof(tpEsum));
    memset(tpEmax,0,sizeof(tpEmax));

    BHTmaxt=0;   //Hold HT for whole Barrel
    BJPmaxt=0;   //Holds max JP sum for whole Barrel
    BJPsumt=0;  //Holds sum of all JP in Barrel
    EHTmaxt=0;   //Hold HT for whole EEMC
    EJPmaxt=0;   //Holds max JP sum for whole EEMC
    EJPsumt=0;  //Holds sum of all JP in EEMC

    muEmcCol=muDstMaker->muDst()->muEmcCollection();
    if(muEmcCol) {
    
	//GET BARREL points 
	numPoints=muEmcCol->getNPoints();
	if (print) cout <<"Number of points this event = " << numPoints << endl;
	for (int count=0; count<numPoints; count++) {
	    points=muEmcCol->getPoint(count);
	    pointE[count]=points->getEnergy();
	    pointPhi[count]=points->getPhi();
	    pointEta[count]=points->getEta();
	    sinTheta[count]=sqrt(1-tanh(pointEta[count])*tanh(pointEta[count]));
	    if (print) printf("For point #%d E=%f phi=%f eta=%f\n",count,pointE[count],pointPhi[count],pointEta[count]);
	}
  
	// According to STAR NOTE#229A all detectors should be numbered first from the +z side (West End) looking toward the interactions region 
	// If a detector needs additional numbering on the -z side then the numbers should be consecutive with the +z elements.
	// Following this, standing on the west side looking at the interaction region, module 58 is at 12 o'clock and in JP0 with JP1 and so on
	// proceeding in a clockwise manner. Standing on the east side looking at the interaction region module 118 is at 12 o'clock 
	// and in JP6 with JP7 and so on proceeding in a clockwise manner.

	///****************************////////////////////////////////////**************************//////////////////////////////////
	///****************************BARREL********************************************************//////////////////////////////////
	///****************************////////////////////////////////////**************************//////////////////////////////////
	//JP0 goes from module=53/2 to module=3/1 (TP=0+26-29,30+56-59,60+86-89,90+116-119,120+146-149)
	//JP1 goes from module=3/2 to module=13/1
	//JP2 goes from module=13/2 to module=23/1
	//JP3 goes from module=23/2 to module=33/1
	//JP4 goes from module=33/2 to module=43/1
	//JP5 goes from module=43/2 to module=53/1 (TP=21-25,51-55,81-85,111-115,141-145)
	//JP6 goes from module=113/2 to module=63/1
	//JP7 goes from module=63/2 to module=73/1
	//JP8 goes from module=73/2 to module=83/1
	//JP9 goes from module=83/2 to module=93/1
	//JP10 goes from module=93/2 to module=103/1
	//JP11 goes from module=103/2 to module=113/1

	//TP(0-29) for eta bin (1-4) all modules 1-60
	//TP(30-59) for eta bin(5-8) all modules  1-60
	//TP(60-89) for eta bin(9-12) all modules 1-60 
	//TP(90-119) for eta bin(13-16) all modules 1-60 
	//TP(120-149) for eta bin(17-20) all modules 1-60
	//TP(150-179) for eta bin (1-4) all modules 61-120
	//TP(180-209) for eta bin(5-8) all modules 61-120 
	//TP(210-239) for eta bin(9-12) all modules  61-120
	//TP(240-269) for eta bin(13-16) all modules  61-120
	//TP(270-299) for eta bin(17-20) all modules 61-120

	det=1;
	emcGeom = StEmcGeom::getEmcGeom("bemc");
	for (int n=1; n<=BemcTow; n++){
	  emcGeom->getBin(n,Bmod,Beta,Bsub);
	    BTowADC =  muEmcCol->getTowerADC(n,det);
	    if (BTowADC>0) {
		if (print) printf("n=%d, mod=%d, sub=%d, Beta=%d adc=%d\n",n,Bmod,Bsub,Beta,BTowADC);
		
		int jpBindex=(Bmod+Bsub+5)/10;
		if (((Bmod+Bsub+5)>=60)&&((Bmod+Bsub+5)<=67)) {
		  jpBindex=0;
		}
		if (((Bmod+Bsub+5)>=120)&&((Bmod+Bsub+5)<=127)) {
		  jpBindex=6;
		}
	       

		int tpBindex=((Bmod+Bsub-3)/2) + 30*((Beta-1)/4);
		if ((Bmod==1)&&(Bsub==1)) {
		    tpBindex=(29 + 30*((Beta-1)/4));
		}  
		if (Bmod>60){//need to add 150 to tp# for east side 
		  tpBindex=120 + ((Bmod+Bsub-3)/2) + 30*((Beta-1)/4);
		    if ((Bmod==61)&&(Bsub==1)) {
			tpBindex=150 + (29+ 30*((Beta-1)/4));
		    }  
		}
		Sum(&jpBsum[jpBindex],&BTowADC);
		Max(&jpBmax[jpBindex],&BTowADC);
		Sum(&tpBsum[tpBindex],&BTowADC);
		Max(&tpBmax[tpBindex],&BTowADC);
		jpB_hit_num[jpBindex]++;
		if (print) printf("jpBindex=%d, jpBsum=%d, jpBmax=%d\n",jpBindex,jpBsum[jpBindex],jpBmax[jpBindex]);
		if (print) printf("tpBindex=%d, tpBsum=%d, tpBmax=%d\n",tpBindex,tpBsum[tpBindex],tpBmax[tpBindex]);
	    }
	}
    
	for (int q=0; q < 6; q++){
	  //for (int q=0; q < 12; q++){//use this for whole barrel
	    Sum(&BJPsumt,&jpBsum[q]);
	    Max(&BHTmaxt,&jpBmax[q]);
	    Max(&BJPmaxt,&jpBsum[q]);
	}

    
    
    
    

	///****************************////////////////////////////////////**************************//////////////////////////////////
	///****************************ENDCAP2004****************************************************//////////////////////////////////
	///****************************////////////////////////////////////**************************//////////////////////////////////
	//Due to changes in EEMC muDST for 2004 sec,sub and eta all start at 1 and go to 12,5,12
	//This code will not work on MuDst before 2004 --actually it will but it will give you GARBAGE!!!
	//Tower 01TA01 has sec=1,sub=1,eta=1
	//Tower 01TA02 has sec=1,sub=1,eta=2
	//Define EMC towers in terms of jet patches (start 0 instead of 1)
	//By definition jp0=towers 11D-1C with Eid #0-119 (m=636-35)
	//              jp1=tower 1D-3C with Eid #120-239 (m=36-155)
	//              jp2=tower 3D-5C with Eid #240-359 (m=156-275)
	//              jp3=tower 3D-5C with Eid #359-479 
	//              jp4=tower 3D-5C with Eid #479-599
	//              jp5=tower 3D-5C with Eid #599-719
	//tp=0 is defined as Eids 0-2 + 12-14
	//tp=1 is defined as Eids 24-26 + 36-38
	//tp=30 is defined as Eid 3-6 + 15-18
	//tp=60 is defined as Eid 7-11 + 19-23	

	NumETow=muEmcCol->getNEndcapTowerADC();
	for (int m=0; m<NumETow; m++){
	    muEmcCol->getEndcapTowerADC(m,ETowADC,Esec,Esub,Eeta);
	    int Eid = m+84;
	    if (ETowADC){
		if (Eid > 719) Eid=Eid-720;
		int jpEindex=Eid/120;
		int tpEindex=Eid/24;
		if (Eeta>7) {
		    tpEindex+=60;
		}
		if ((Eeta>3)&&(Eeta<8)) {
		    tpEindex+=30;
		}

		Sum(&jpEsum[jpEindex],&ETowADC);
		Max(&jpEmax[jpEindex],&ETowADC);
		Sum(&tpEsum[tpEindex],&ETowADC);
		Max(&tpEmax[tpEindex],&ETowADC);
		jpE_hit_num[jpEindex]++;
    
		if (print){
		    printf("Etow=%d, Esec=%d, Esub=%d, Eeta=%d ETowADC=%d\n",m,Esec,Esub,Eeta,ETowADC);
		    printf("Eid=%d, jpEindex=%d, jpEsum=%d, jpEmax=%d\n",Eid,jpEindex,jpEsum[jpEindex],jpEmax[jpEindex]);
		    printf("Eid=%d, tpEindex=%d, tpEsum=%d, tpEmax=%d\n",Eid,tpEindex,tpEsum[tpEindex],tpEmax[tpEindex]);
		}
	    }
	}
    
	
	for (int q=0; q < 6; q++){
      
	    Sum(&EJPsumt,&jpEsum[q]);
	    Max(&EHTmaxt,&jpEmax[q]);
	    Max(&EJPmaxt,&jpEsum[q]);
	    if (print) printf("q=%d; JPmax=%d ,JPsum=%d\n",q,jpEmax[q],jpEsum[q]);
	}

	if (print){
	    printf("EJPsum=%d ,EHTmax=%d,EJPmax=%d\n",EJPsumt,EHTmaxt,EJPmaxt);
	    printf("BJPsum=%d ,BHTmax=%d,BJPmax=%d\n",BJPsumt,BHTmaxt,BJPmaxt);
	}
    }

    if(!muEmcCol) cout << "No EMC muDst info!" << endl;   
  
    return kStOK;
}


void StJetSimuTrigMaker::Sum(int *sum,int *sumadd){

    (*sum)=(*sumadd)+(*sum);
}   

void StJetSimuTrigMaker::Max(int *max,int *maxcomp){

    if ((*max) < (*maxcomp))
	(*max)=(*maxcomp);
}  








