//Author :: Renee Fatemi
//
// Sept 21 2004 -- Maker produces Trigger from BBC, EEMC, EMC
//

#include "StJetMaker/StJetSimuUtil/StJetSimuTrigMaker.h"
#include "StChain.h"
#include "StJetMaker/StJetSimuUtil/StJetEmcTrigSim.h"

//std
#include <map>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
using namespace std;


//StEmc
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcRawMaker/StEmcRawMaker.h"
//#include "StEmcTriggerMaker/StBemcTrigger.h"
//#include "StEmcTriggerMaker/StEmcTriggerMaker.h"


//StMuDSTMaker
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEmcPoint.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//StEvent
#include "StEvent/StEmcDetector.h"
#include "StEvent/StEmcCollection.h"
#include "StEvent/StEmcRawHit.h"
#include "StEvent/StEmcModule.h"
#include "StEvent/StTpcPadrowHitCollection.h"
#include "StEvent/StTpcHitCollection.h"
#include "StEvent/StTrack.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StBbcTriggerDetector.h"
#include "StEvent/StTriggerData.h"
#include "StEventTypes.h"                                                                                                                             

//root
#include "TH1F.h"
#include "TH2F.h"
#include "TClonesArray.h"



ClassImp(StJetSimuTrigMaker)

StJetSimuTrigMaker::StJetSimuTrigMaker(const char *name):StMaker(name){
  
}

StJetSimuTrigMaker::~StJetSimuTrigMaker(){
  delete myTable;
}

/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StJetSimuTrigMaker::Init(){

    muDstMaker = (StMuDstMaker*)GetMaker("MuDst");
    assert(muDstMaker);
    muEvent= new StMuEvent();
    muEmcCol = new StMuEmcCollection();
    myTable = new StBemcTables();
    htTrig =(StJetEmcTrigSim*)GetMaker("StJetEmcTrigSim");
    //trgMaker=(StEmcTriggerMaker*)GetMaker("bemctrigger");
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

    for (int j=0;j<20;j++){
      TowEt[j]=0;
    }

    return StMaker::Init();
}

Int_t StJetSimuTrigMaker::Make()
{
   
    StMuDst* dst = muDstMaker->muDst();
    assert(dst);
    muEvent = dst->event();
    assert(muEvent);

    StEventInfo &info=muEvent->eventInfo();
    evtID=info.id();
    if (print) cout << "Event # = "<< info.id() << "evtID=  " << evtID <<endl; 
   
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

    //access Alex/Mike StJetEmcTrigSim -- 1st StEmcTriggerMaker -- historical only
    Alex_ht_Et = htTrig->hiTowerEt();
    Alex_ht_DSM = htTrig->hiTowerAdc6Bit();
    Alex_ht_id = htTrig->hiTowerId();    
    if (1) printf("AlexDSM=%d, AlexEt=%f,AlexID=%d\n",Alex_ht_DSM,Alex_ht_Et,Alex_ht_id);

    /* Uncomment this section to get Trigger information from StEmcTriggerMaker
       do cvs co StEmcTriggerMaker and cons
    JP1_2004_evt=-1;
    JP1_2004_id=-1;
    JP1_2004_dsm=-1;
    JP1_2004_evt=trgMaker->is2004JP1();
    JP1_2004_id=trgMaker->get2004JP1_ID();
    JP1_2004_dsm=trgMaker->get2004JP1_ADC();
    cout<<"JP1_2004_evt= "<<JP1_2004_evt<<" JP1_2004_id="<<JP1_2004_id<<" JP1_2004_dsm="<<JP1_2004_dsm<<endl;
    

    HT1_2004_evt=-1;
    HT1_2004_id=-1;
    HT1_2004_dsm=-1;
    HT1_2004_evt=trgMaker->is2004HT1();
    HT1_2004_id=trgMaker->get2004HT1_ID();
    HT1_2004_dsm=trgMaker->get2004HT1_ADC();
    cout<<"HT1_2004_evt= "<<HT1_2004_evt<<" HT1_2004_id="<<HT1_2004_id<<" DSM ADC = "<< HT1_2004_dsm<<endl;
    */

    ReneeBEMC();
    //ReneeEEMC();

    return kStOK;
}


void StJetSimuTrigMaker::ReneeBEMC(){
  //This method may be used with ideal or real database entries for simulation
  //The trigger patch numbering is my own and does not reflect "standard" BEMC
  //numbering scheme

    // set array values to zero before each event
    memset(jpBsum,0,sizeof(jpBsum));
    memset(jpBmax, 0,sizeof(jpBmax));
    memset(jpB_hit_num,0,sizeof(jpB_hit_num));
    memset(tpBsum,0,sizeof(tpBsum));
    memset(tpBmax,0,sizeof(tpBmax));
    memset(TowEt,0,sizeof(TowEt));
  
    BHTmaxt=0;   //Hold HT for whole Barrel
    BJPmaxt=0;   //Holds max JP sum for whole Barrel
    BJPsumt=0;  //Holds sum of all JP in Barrel

    StMuDst* dst = muDstMaker->muDst();
    assert(dst);
    muEvent = dst->event();
    assert(muEvent);

    myTable->loadTables((StMaker*)this);
    assert(myTable); 
    
    StEmcCollection* emc = 0;
    StEvent* event = dynamic_cast<StEvent*>( GetInputDS("StEvent") ); 
    if (event){
      emc = event->emcCollection();
    }
    else   {
      emc = dst->emcCollection();
    }
    if (emc==0) cout<<"Why emc == 0 in MuDst?"<<endl;
    if (emc){
      StEmcDetector* detector = emc->detector(kBarrelEmcTowerId);
      emcGeom = StEmcGeom::getEmcGeom("bemc");
      if (detector!=0){   
	
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

	for (int m = 1; m<=60;m++) { //loop on modules...
	  StEmcModule* module = detector->module(m);
	  assert(module);
	  StSPtrVecEmcRawHit& rawHit=module->hits();
	  for(UInt_t k=0;k<rawHit.size();k++) {
	    Bmod=rawHit[k]->module();
	    Beta=rawHit[k]->eta();
	    Bsub=abs(rawHit[k]->sub());
	    //BTowADC=rawHit[k]->adc();	  
	    Benergy=rawHit[k]->energy();
	    int id;
	    emcGeom->getId(Bmod,Beta,Bsub,id);
	    emcGeom->getEtaPhi(id,Teta,Tphi);
	    myTable->getCalib(BTOW, id, 1, Bgain);
	    myTable->getStatus(BTOW, id, Bstat);
	    myTable->getPedestal(BTOW,id,0,Bped,Brms);
	    BTowADC= static_cast<int>(Benergy/Bgain);
	    if ((BTowADC>0)&&(Bstat==1)) {
	      if (print) printf("n=%d, mod=%d, sub=%d, Beta=%d, Teta=%f, Tphi=%f\n",id,Bmod,Bsub,Beta,Teta,Tphi);
	      if (print) printf("n=%d, adc=%d, Bgain=%f, Benergy = %f, Bped=%f, Bstat=%d\n",id,BTowADC,Bgain,Benergy,Bped,Bstat);     

	      
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
	}
	
	for (int q=0; q < 6; q++){
	  //for (int q=0; q < 12; q++){//use this for whole barrel
	  Sum(&BJPsumt,&jpBsum[q]);
	  Max(&BHTmaxt,&jpBmax[q]);
	  Max(&BJPmaxt,&jpBsum[q]);
	  if (1) printf("jpBindex=%d, BJPmax=%d, BHTmax=%d\n",q,jpBsum[q],jpBmax[q]); 
	}
      } 
    }

    //for (int i=0;i<4800;i++){
    //  myTable->getCalib(BTOW, i, 1, Bgain);
    //  myTable->getStatus(BTOW, i, Bstat);
    //  cout<<" *Tow ID="<<i<<" gain="<<Bgain<<" status="<<Bstat<<endl;
    // }

}

void StJetSimuTrigMaker::ReneeEEMC(){
  //This method uses no pedestal subtraction and therefore should only be used with "Ideal" detector gains and peds in simulation//

    // set array values to zero before each event
    memset(jpEsum,0,sizeof(jpEsum));
    memset(jpEmax,0,sizeof(jpEmax));
    memset(jpE_hit_num,0,sizeof(jpE_hit_num));
    memset(tpEsum,0,sizeof(tpEsum));
    memset(tpEmax,0,sizeof(tpEmax));


    EHTmaxt=0;   //Hold HT for whole EEMC
    EJPmaxt=0;   //Holds max JP sum for whole EEMC
    EJPsumt=0;  //Holds sum of all JP in EEMC

    muEmcCol=muDstMaker->muDst()->muEmcCollection();
    if(muEmcCol) {
     
      
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
    
    for (int j=0;j<20;j++){
      if (print) cout<<"Final TowEt="<<TowEt[j]<<endl;
    }
    
    
}


void StJetSimuTrigMaker::Sum(int *sum,int *sumadd){

    (*sum)=(*sumadd)+(*sum);
}   

void StJetSimuTrigMaker::Max(int *max,int *maxcomp){

    if ((*max) < (*maxcomp))
	(*max)=(*maxcomp);
}  








