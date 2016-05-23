//*-- Author : Renee Fatemi 

#include "TFile.h"
#include "StSpinPool/StMCAsymMaker/StMCAsymMaker.h"
#include "StChain.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEventTypes.hh"
#include "StMcEvent.hh"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

//#include "StMCAsymEvent.h"
#include "StPythiaEvent.h"

#include "tables/St_particle_Table.h"

ClassImp(StMCAsymMaker)

StMCAsymMaker::StMCAsymMaker(const char *name):StMaker(name) {
    mEvent = new StPythiaEvent();
}

StMCAsymMaker::~StMCAsymMaker() {
    delete mEvent;
}

Int_t StMCAsymMaker::Init() {
  int iset = 0;
  dssvini2009a_(&iset);
  return StMaker::Init();
}

void StMCAsymMaker::Zero() {
    pid=-10;
    hard_p= -10;
    cos_th= -10;
    x1= -10;
    x2= -10;
    s= 0;
    t= 0;
    u= 0;

    partonic_all=0;
    Q2=0;
    pid=0;

    df1_NLO_GSA=0;
    df2_NLO_GSA=0;
    weight_NLO_GSA=0;

    df1_NLO_GSB=0;
    df2_NLO_GSB=0;
    weight_NLO_GSB=0;

    df1_NLO_GSC=0;
    df2_NLO_GSC=0;
    weight_NLO_GSC=0;

    df1_LO=0;
    df2_LO=0;
    f1_LO=0;
    f2_LO=0;
    weight_LO=0;

    df1_NLO=0;
    df2_NLO=0;
    f1_NLO=0;
    f2_NLO=0;
    weight_NLO=0;

    df1_NLO_g0=0;
    df2_NLO_g0=0;
    weight_NLO_g0=0;

    df1_NLO_gmax=0;
    df2_NLO_gmax=0;
    weight_NLO_gmax=0;

    df1_NLO_gmin=0;
    df2_NLO_gmin=0;
    weight_NLO_gmin=0;

    df1_NLO_m015=0;
    df2_NLO_m015=0;
    weight_NLO_m015=0;

    df1_NLO_m030=0;
    df2_NLO_m030=0;
    weight_NLO_m030=0;

    df1_NLO_m045=0;
    df2_NLO_m045=0;
    weight_NLO_m045=0;

    df1_NLO_m060=0;
    df2_NLO_m060=0;
    weight_NLO_m060=0;

    df1_NLO_m075=0;
    df2_NLO_m075=0;
    weight_NLO_m075=0;

    df1_NLO_m090=0;
    df2_NLO_m090=0;
    weight_NLO_m090=0;

    df1_NLO_m105=0;
    df2_NLO_m105=0;
    weight_NLO_m105=0;

    df1_NLO_p030=0;
    df2_NLO_p030=0;
    weight_NLO_p030=0;

    df1_NLO_p045=0;
    df2_NLO_p045=0;
    weight_NLO_p045=0;

    df1_NLO_p060=0;
    df2_NLO_p060=0;
    weight_NLO_p060=0;

    df1_NLO_p070=0;
    df2_NLO_p070=0;
    weight_NLO_p070=0;

    df1_NLO_DSSV=0;
    df2_NLO_DSSV=0;
    weight_NLO_DSSV=0;

    df1_NLO_DSSV2009a=0;
    df2_NLO_DSSV2009a=0;
    weight_NLO_DSSV2009a=0;

    df1_NLO_LSS1=0;
    df2_NLO_LSS1=0;
    weight_NLO_LSS1=0;

    df1_NLO_LSS2=0;
    df2_NLO_LSS2=0;
    weight_NLO_LSS2=0;

    df1_NLO_LSS3=0;
    df2_NLO_LSS3=0;
    weight_NLO_LSS3=0;

    df1_NLO_LSS2010_delGpos = 0;
    df2_NLO_LSS2010_delGpos = 0;
    weight_NLO_LSS2010_delGpos = 0;

    df1_NLO_LSS2010_chsign_delG = 0;
    df2_NLO_LSS2010_chsign_delG = 0;
    weight_NLO_LSS2010_chsign_delG = 0;

    df1_NLO_AAC1=0;
    df2_NLO_AAC1=0;
    weight_NLO_AAC1=0;

    df1_NLO_AAC2=0;
    df2_NLO_AAC2=0;
    weight_NLO_AAC2=0;

    df1_NLO_AAC3=0;
    df2_NLO_AAC3=0;
    weight_NLO_AAC3=0;

    df1_NLO_BB1=0;
    df2_NLO_BB1=0;
    weight_NLO_BB1=0;

    df1_NLO_BB2=0;
    df2_NLO_BB2=0;
    weight_NLO_BB2=0;

    df1_NLO_BB2010 = 0;
    df2_NLO_BB2010 = 0;
    weight_NLO_BB2010 = 0;

    df1_NLO_DNS1=0;
    df2_NLO_DNS1=0;
    weight_NLO_DNS1=0;

    df1_NLO_DNS2=0;
    df2_NLO_DNS2=0;
    weight_NLO_DNS2=0;

}

void StMCAsymMaker::Clear(const Option_t* c) {
    Zero();
    mEvent->Clear(c);
    StMaker::Clear(c);
}
//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StMCAsymMaker::Make() {
    //Get StMcEvent to look at GEANT record
    mcEvent = (StMcEvent*)GetDataSet("StMcEvent");
    if (!mcEvent) {
        LOG_WARN << "No StMcEvent" << endm;
        return kStWarn;
    }

    //GET EVTID FROM MuDst 
    muDstMaker = (StMuDstMaker*)GetMaker("MuDst"); assert(muDstMaker);
    StMuDst* dst = muDstMaker->muDst(); assert(dst);
    muEvent = dst->event();  assert(muEvent);
    StEventInfo &info=muEvent->eventInfo();
    evtid=info.id();

    //GET GEANT EVENT
    TDataSet *Event = GetDataSet("geant"); //Event->ls(3);

    //GET PYTHIA RECORD from particleTable
    TDataSetIter geantDstI(Event);
    particleTabPtr = (St_particle  *) geantDstI("particle");

    // if no particle table in the file just skip it
    if (particleTabPtr!=0){
      
      particle_st* particleTable = particleTabPtr->GetTable();//particleTabPtr->Print();
      
      //GET EVTID and SUBPROCESS ID from struct g2t_event
      Pg2t_event=(St_g2t_event *) geantDstI("g2t_event"); //Pg2t_event->Print();
      g2t_event_st *g2t_event1=Pg2t_event->GetTable();
      geantID= g2t_event1->n_event; 
      geantPID= g2t_event1->subprocess_id;
      pid=geantPID;
      //TEST that geantID==eventID to ensure .geant and .MuDst file are synchronized
      assert(evtid==geantID);
      
      //GET PARTONIC KINEMATICS from struct Pg2t_pythia
      Pg2t_pythia=(St_g2t_pythia *) geantDstI("g2t_pythia");// Pg2t_pythia->Print();
      g2t_pythia_st *g2t_pythia1=Pg2t_pythia->GetTable();
      s= g2t_pythia1-> mand_s;
      t= g2t_pythia1-> mand_t;
      u= g2t_pythia1-> mand_u;
      hard_p= g2t_pythia1->hard_p;
      cos_th= g2t_pythia1->cos_th;
      x1= g2t_pythia1->bjor_1;
      x2= g2t_pythia1->bjor_2;
      
      //GET FLAVOR AFTER INTIAL RADIATION BEFORE and AFTER SCATTERING
      flavor1=particleTable[4].idhep;
      flavor2=particleTable[5].idhep;
      flavor3=particleTable[6].idhep;
      flavor4=particleTable[7].idhep;
      
      //GET SCATTERED PARTON RECORD
      parton1[0]=particleTable[6].idhep;// particle id
      parton1[1]=particleTable[6].phep[0];//px
      parton1[2]=particleTable[6].phep[1];//py
      parton1[3]=particleTable[6].phep[2];//pz
      parton1[4]=particleTable[6].phep[3];//E
      parton1[5]=particleTable[6].phep[4];//m
      parton1[6]=particleTable[6].isthep;//status
      parton1[7]=particleTable[6].jmohep[0];//moth1
      parton1[8]=particleTable[6].jmohep[1];//moth2
      parton1[9]=particleTable[6].jdahep[0];//daughter1
      parton1[10]=particleTable[6].jdahep[1];//daughter2
      parton2[0]=particleTable[7].idhep;// particle id
      parton2[1]=particleTable[7].phep[0];//px
      parton2[2]=particleTable[7].phep[1];//py
      parton2[3]=particleTable[7].phep[2];//pz
      parton2[4]=particleTable[7].phep[3];//E
      parton2[5]=particleTable[7].phep[4];//m
      parton2[6]=particleTable[7].isthep;//status
      parton2[7]=particleTable[7].jmohep[0];//moth1
      parton2[8]=particleTable[7].jmohep[1];//moth2
      parton2[9]=particleTable[7].jdahep[0];//daughter1
      parton2[10]=particleTable[7].jdahep[1];//daughter2
      
      if (0){//PRINT OUT PYTHIA RECORD
        printf("PID/evtid from McEvent = %d,%d; PID/evtid from Table = %d,%d:\n",pid,evtid,geantPID,geantID);
        printf("row |   id   |   px   |   py   |   pz   |   E   |   m   | status | moth1 | moth2 | daught1 | daught2 |\n");
        for (int i=0; i<particleTabPtr->GetNRows();++i) {
	  printf("  %d,  %d,  %f,   %f,   %f,   %f,   %f,   %d,   %d,   %d,   %d,   %d\n",i,particleTable[i].idhep, particleTable[i].phep[0],
		 particleTable[i].phep[1], particleTable[i].phep[2] , particleTable[i].phep[3], particleTable[i].phep[4], particleTable[i].isthep,
		 particleTable[i].jmohep[0], particleTable[i].jmohep[1], particleTable[i].jdahep[0], particleTable[i].jdahep[1]);}
	cout << endl;
	cout << "flavor1: " << flavor1 << "  flavor2: " << flavor2 << "  flavor3: " << flavor3 << "  flavor4: " << flavor4 << endl << endl;
      }
      
      //Get partonic a_LL, polarized/unpolarized pdfs using Q2 = partonic_pT^2
      partonic_all=getPartonicALL(s,t,u,pid,flavor1,flavor2,flavor3,flavor4);
      Q2=hard_p*hard_p;
      
      //NLO GS SCENARIO A
      df1_NLO_GSA=get_polPDF_NLO_GSA(flavor1,x1,Q2);
      df2_NLO_GSA=get_polPDF_NLO_GSA(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_GSA=(df1_NLO_GSA*df2_NLO_GSA*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO GS SCENARIO B
      df1_NLO_GSB=get_polPDF_NLO_GSB(flavor1,x1,Q2);
      df2_NLO_GSB=get_polPDF_NLO_GSB(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_GSB=(df1_NLO_GSB*df2_NLO_GSB*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO GS SCENARIO C
      df1_NLO_GSC=get_polPDF_NLO_GSC(flavor1,x1,Q2);
      df2_NLO_GSC=get_polPDF_NLO_GSC(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_GSC=(df1_NLO_GSC*df2_NLO_GSC*partonic_all)/(f1_NLO*f2_NLO);
      
      //LO GRSV
      df1_LO=get_polPDF_LO(flavor1,x1,Q2);
      df2_LO=get_polPDF_LO(flavor2,x2,Q2);
      f1_LO=get_unpolPDF_LO(flavor1,x1,Q2);
      f2_LO=get_unpolPDF_LO(flavor2,x2,Q2);
      weight_LO=(df1_LO*df2_LO*partonic_all)/(f1_LO*f2_LO);
      
      //NLO GRSV
      df1_NLO=get_polPDF_NLO(flavor1,x1,Q2);
      df2_NLO=get_polPDF_NLO(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO=(df1_NLO*df2_NLO*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_g0 GRSV
      df1_NLO_g0=get_polPDF_NLO_g0(flavor1,x1,Q2);
      df2_NLO_g0=get_polPDF_NLO_g0(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_g0=(df1_NLO_g0*df2_NLO_g0*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_gmax  GRSV
      df1_NLO_gmax=get_polPDF_NLO_gmax(flavor1,x1,Q2);
      df2_NLO_gmax=get_polPDF_NLO_gmax(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_gmax=(df1_NLO_gmax*df2_NLO_gmax*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_gmin  GRSV
      df1_NLO_gmin=get_polPDF_NLO_gmin(flavor1,x1,Q2);
      df2_NLO_gmin=get_polPDF_NLO_gmin(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_gmin=(df1_NLO_gmin*df2_NLO_gmin*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_m015 GRSV
      df1_NLO_m015=get_polPDF_NLO_m015(flavor1,x1,Q2);
      df2_NLO_m015=get_polPDF_NLO_m015(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_m015=(df1_NLO_m015*df2_NLO_m015*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_m030 GRSV
      df1_NLO_m030=get_polPDF_NLO_m030(flavor1,x1,Q2);
      df2_NLO_m030=get_polPDF_NLO_m030(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_m030=(df1_NLO_m030*df2_NLO_m030*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_m045 GRSV
      df1_NLO_m045=get_polPDF_NLO_m045(flavor1,x1,Q2);
      df2_NLO_m045=get_polPDF_NLO_m045(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_m045=(df1_NLO_m045*df2_NLO_m045*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_m060 GRSV
      df1_NLO_m060=get_polPDF_NLO_m060(flavor1,x1,Q2);
      df2_NLO_m060=get_polPDF_NLO_m060(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_m060=(df1_NLO_m060*df2_NLO_m060*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_m075 GRSV
      df1_NLO_m075=get_polPDF_NLO_m075(flavor1,x1,Q2);
      df2_NLO_m075=get_polPDF_NLO_m075(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_m075=(df1_NLO_m075*df2_NLO_m075*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_m090 GRSV
      df1_NLO_m090=get_polPDF_NLO_m090(flavor1,x1,Q2);
      df2_NLO_m090=get_polPDF_NLO_m090(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_m090=(df1_NLO_m090*df2_NLO_m090*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_m105 GRSV
      df1_NLO_m105=get_polPDF_NLO_m105(flavor1,x1,Q2);
      df2_NLO_m105=get_polPDF_NLO_m105(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_m105=(df1_NLO_m105*df2_NLO_m105*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_p030 GRSV
      df1_NLO_p030=get_polPDF_NLO_p030(flavor1,x1,Q2);
      df2_NLO_p030=get_polPDF_NLO_p030(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_p030=(df1_NLO_p030*df2_NLO_p030*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_p045 GRSV
      df1_NLO_p045=get_polPDF_NLO_p045(flavor1,x1,Q2);
      df2_NLO_p045=get_polPDF_NLO_p045(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_p045=(df1_NLO_p045*df2_NLO_p045*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_p060 GRSV
      df1_NLO_p060=get_polPDF_NLO_p060(flavor1,x1,Q2);
      df2_NLO_p060=get_polPDF_NLO_p060(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_p060=(df1_NLO_p060*df2_NLO_p060*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO_p070 GRSV
      df1_NLO_p070=get_polPDF_NLO_p070(flavor1,x1,Q2);
      df2_NLO_p070=get_polPDF_NLO_p070(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_p070=(df1_NLO_p070*df2_NLO_p070*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO DSSV
      df1_NLO_DSSV=get_polPDF_NLO_DSSV(flavor1,x1,Q2);
      df2_NLO_DSSV=get_polPDF_NLO_DSSV(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_DSSV=(df1_NLO_DSSV*df2_NLO_DSSV*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO DSSV 2009a
      df1_NLO_DSSV2009a=get_polPDF_NLO_DSSV2009a(flavor1,x1,Q2);
      df2_NLO_DSSV2009a=get_polPDF_NLO_DSSV2009a(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_DSSV2009a=(df1_NLO_DSSV2009a/f1_NLO)*(df2_NLO_DSSV2009a/f2_NLO)*partonic_all;
      
      //NLO LSS SCENARIO 1
      df1_NLO_LSS1=get_polPDF_NLO_LSS1(flavor1,x1,Q2);
      df2_NLO_LSS1=get_polPDF_NLO_LSS1(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_LSS1=(df1_NLO_LSS1*df2_NLO_LSS1*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO LSS SCENARIO 2
      df1_NLO_LSS2=get_polPDF_NLO_LSS2(flavor1,x1,Q2);
      df2_NLO_LSS2=get_polPDF_NLO_LSS2(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_LSS2=(df1_NLO_LSS2*df2_NLO_LSS2*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO LSS SCENARIO 3
      df1_NLO_LSS3=get_polPDF_NLO_LSS3(flavor1,x1,Q2);
      df2_NLO_LSS3=get_polPDF_NLO_LSS3(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_LSS3=(df1_NLO_LSS3*df2_NLO_LSS3*partonic_all)/(f1_NLO*f2_NLO);

      //NLO LSS2010 pos x*DeltaG
      df1_NLO_LSS2010_delGpos = get_polPDF_NLO_LSS2010_delGpos(flavor1,x1,Q2);
      df2_NLO_LSS2010_delGpos = get_polPDF_NLO_LSS2010_delGpos(flavor2,x2,Q2);
      f1_NLO = get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO = get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_LSS2010_delGpos = (df1_NLO_LSS2010_delGpos*df2_NLO_LSS2010_delGpos*partonic_all)/(f1_NLO*f2_NLO);

      //NLO LSS2010 node x*DeltaG
      df1_NLO_LSS2010_chsign_delG = get_polPDF_NLO_LSS2010_chsign_delG(flavor1,x1,Q2);
      df2_NLO_LSS2010_chsign_delG = get_polPDF_NLO_LSS2010_chsign_delG(flavor2,x2,Q2);
      f1_NLO = get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO = get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_LSS2010_chsign_delG = (df1_NLO_LSS2010_chsign_delG*df2_NLO_LSS2010_chsign_delG*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO AAC SCENARIO 1
      df1_NLO_AAC1=get_polPDF_NLO_AAC1(flavor1,x1,Q2);
      df2_NLO_AAC1=get_polPDF_NLO_AAC1(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_AAC1=(df1_NLO_AAC1*df2_NLO_AAC1*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO AAC SCENARIO 2
      df1_NLO_AAC2=get_polPDF_NLO_AAC2(flavor1,x1,Q2);
      df2_NLO_AAC2=get_polPDF_NLO_AAC2(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_AAC2=(df1_NLO_AAC2*df2_NLO_AAC2*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO AAC SCENARIO 3
      df1_NLO_AAC3=get_polPDF_NLO_AAC3(flavor1,x1,Q2);
      df2_NLO_AAC3=get_polPDF_NLO_AAC3(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_AAC3=(df1_NLO_AAC3*df2_NLO_AAC3*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO BB SCENARIO 1
      df1_NLO_BB1=get_polPDF_NLO_BB1(flavor1,x1,Q2);
      df2_NLO_BB1=get_polPDF_NLO_BB1(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_BB1=(df1_NLO_BB1*df2_NLO_BB1*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO BB SCENARIO 2
      df1_NLO_BB2=get_polPDF_NLO_BB2(flavor1,x1,Q2);
      df2_NLO_BB2=get_polPDF_NLO_BB2(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_BB2=(df1_NLO_BB2*df2_NLO_BB2*partonic_all)/(f1_NLO*f2_NLO);

      //NLO BB2010
      df1_NLO_BB2010 = get_polPDF_NLO_BB2010(flavor1,x1,Q2);
      df2_NLO_BB2010 = get_polPDF_NLO_BB2010(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_BB2010 = (df1_NLO_BB2010*df2_NLO_BB2010*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO DNS SCENARIO 1
      df1_NLO_DNS1=get_polPDF_NLO_DNS1(flavor1,x1,Q2);
      df2_NLO_DNS1=get_polPDF_NLO_DNS1(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_DNS1=(df1_NLO_DNS1*df2_NLO_DNS1*partonic_all)/(f1_NLO*f2_NLO);
      
      //NLO DNS SCENARIO 2
      df1_NLO_DNS2=get_polPDF_NLO_DNS2(flavor1,x1,Q2);
      df2_NLO_DNS2=get_polPDF_NLO_DNS2(flavor2,x2,Q2);
      f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
      f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
      weight_NLO_DNS2=(df1_NLO_DNS2*df2_NLO_DNS2*partonic_all)/(f1_NLO*f2_NLO);
      
      if (0) {
	printf("LO:  df1_LO=%f, df2_LO=%f, f1_LO=%f, f2_LO=%f, weight_LO=%f\n",df1_LO,df2_LO,f1_LO,f2_LO,weight_LO);
	printf("NLO:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO,df2_NLO,f1_NLO,f2_NLO,weight_NLO);
	printf("NLO_gmin:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_gmin,df2_NLO_gmin,f1_NLO,f2_NLO,weight_NLO_gmin);
	printf("NLO_g0:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_g0,df2_NLO_g0,f1_NLO,f2_NLO,weight_NLO_g0);
	printf("NLO_gmax:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_gmax,df2_NLO_gmax,f1_NLO,f2_NLO,weight_NLO_gmax);
	
	printf("NLO_m015:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_m015,df2_NLO_m015,f1_NLO,f2_NLO,weight_NLO_m015);
	printf("NLO_m030:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_m030,df2_NLO_m030,f1_NLO,f2_NLO,weight_NLO_m030);
	printf("NLO_m045:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_m045,df2_NLO_m045,f1_NLO,f2_NLO,weight_NLO_m045);
	printf("NLO_m060:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_m060,df2_NLO_m060,f1_NLO,f2_NLO,weight_NLO_m060);
	printf("NLO_m075:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_m075,df2_NLO_m075,f1_NLO,f2_NLO,weight_NLO_m075);
	printf("NLO_m090:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_m090,df2_NLO_m090,f1_NLO,f2_NLO,weight_NLO_m090);
	printf("NLO_m105:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_m105,df2_NLO_m105,f1_NLO,f2_NLO,weight_NLO_m105);
	
	printf("NLO_p030:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_p030,df2_NLO_p030,f1_NLO,f2_NLO,weight_NLO_p030);
	printf("NLO_p045:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_p045,df2_NLO_p045,f1_NLO,f2_NLO,weight_NLO_p045);
	printf("NLO_p060:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_p060,df2_NLO_p060,f1_NLO,f2_NLO,weight_NLO_p060);
	printf("NLO_p070:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_p070,df2_NLO_p070,f1_NLO,f2_NLO,weight_NLO_p070);
	
	
	printf("DSSV:  df1_DSSV=%f, df2_DSSV=%f, f1_DSSV=%f, f2_DSSV=%f, weight_DSSV=%f\n",df1_NLO_DSSV,df2_NLO_DSSV,f1_NLO,f2_NLO,weight_NLO_DSSV);
	printf("LSS1:  df1_LSS1=%f, df2_LSS1=%f, f1_LSS1=%f, f2_LSS1=%f, weight_LSS1=%f\n",df1_NLO_LSS1,df2_NLO_LSS1,f1_NLO,f2_NLO,weight_NLO_LSS1);
	printf("LSS2:  df1_LSS2=%f, df2_LSS2=%f, f1_LSS2=%f, f2_LSS2=%f, weight_LSS2=%f\n",df1_NLO_LSS2,df2_NLO_LSS2,f1_NLO,f2_NLO,weight_NLO_LSS2);
	printf("LSS3:  df1_LSS3=%f, df2_LSS3=%f, f1_LSS3=%f, f2_LSS3=%f, weight_LSS3=%f\n",df1_NLO_LSS3,df2_NLO_LSS3,f1_NLO,f2_NLO,weight_NLO_LSS3);
	printf("AAC1:  df1_AAC1=%f, df2_AAC1=%f, f1_AAC1=%f, f2_AAC1=%f, weight_AAC1=%f\n",df1_NLO_AAC1,df2_NLO_AAC1,f1_NLO,f2_NLO,weight_NLO_AAC1);
	printf("AAC2:  df1_AAC2=%f, df2_AAC2=%f, f1_AAC2=%f, f2_AAC2=%f, weight_AAC2=%f\n",df1_NLO_AAC2,df2_NLO_AAC2,f1_NLO,f2_NLO,weight_NLO_AAC2);
	printf("AAC3:  df1_AAC3=%f, df2_AAC3=%f, f1_AAC3=%f, f2_AAC3=%f, weight_AAC3=%f\n",df1_NLO_AAC3,df2_NLO_AAC3,f1_NLO,f2_NLO,weight_NLO_AAC3);
	
	printf("BB1:  df1_BB1=%f, df2_BB1=%f, f1_BB1=%f, f2_BB1=%f, weight_BB1=%f\n",df1_NLO_BB1,df2_NLO_BB1,f1_NLO,f2_NLO,weight_NLO_BB1);
	printf("BB2:  df1_BB2=%f, df2_BB2=%f, f1_BB2=%f, f2_BB2=%f, weight_BB2=%f\n",df1_NLO_BB2,df2_NLO_BB2,f1_NLO,f2_NLO,weight_NLO_BB2);
	printf("DNS1:  df1_DNS1=%f, df2_DNS1=%f, f1_DNS1=%f, f2_DNS1=%f, weight_DNS1=%f\n",df1_NLO_DNS1,df2_NLO_DNS1,f1_NLO,f2_NLO,weight_NLO_DNS1);
	printf("DNS2:  df1_DNS2=%f, df2_DNS2=%f, f1_DNS2=%f, f2_DNS2=%f, weight_DNS2=%f\n",df1_NLO_DNS2,df2_NLO_DNS2,f1_NLO,f2_NLO,weight_NLO_DNS2);
      }
    
 
    fillPythiaEvent(mEvent);

    }

    return kStOK;
}

void StMCAsymMaker::fillPythiaEvent(StPythiaEvent* pythia)
{
    g2t_event_st* eventTable = Pg2t_event->GetTable();

    pythia->setRunId(eventTable->n_run);
    pythia->setEventId(eventTable->n_event);

    if (mcEvent && mcEvent->primaryVertex())
        pythia->setVertex(mcEvent->primaryVertex()->position().xyz());

    g2t_pythia_st* pythiaTable = Pg2t_pythia->GetTable();

    pythia->setProcessId(pythiaTable->subprocess_id);
    pythia->setS(pythiaTable->mand_s);
    pythia->setT(pythiaTable->mand_t);
    pythia->setU(pythiaTable->mand_u);
    pythia->setPt(pythiaTable->hard_p);
    pythia->setCosTheta(pythiaTable->cos_th);
    pythia->setX1(pythiaTable->bjor_1);
    pythia->setX2(pythiaTable->bjor_2);
    pythia->setMstu72(pythiaTable->mstu72);
    pythia->setMstu73(pythiaTable->mstu73);
    pythia->setMstp111(pythiaTable->mstp111);
    pythia->setPartonALL(partonic_all);
    
    pythia->setDF1(StPythiaEvent::LO, df1_LO);
    pythia->setDF1(StPythiaEvent::NLO, df1_NLO);
    pythia->setDF1(StPythiaEvent::ZERO, df1_NLO_g0);
    pythia->setDF1(StPythiaEvent::MAX, df1_NLO_gmax);
    pythia->setDF1(StPythiaEvent::MIN, df1_NLO_gmin);
    pythia->setDF1(StPythiaEvent::M015, df1_NLO_m015);
    pythia->setDF1(StPythiaEvent::M030, df1_NLO_m030);
    pythia->setDF1(StPythiaEvent::M045, df1_NLO_m045);
    pythia->setDF1(StPythiaEvent::M060, df1_NLO_m060);
    pythia->setDF1(StPythiaEvent::M075, df1_NLO_m075);
    pythia->setDF1(StPythiaEvent::M090, df1_NLO_m090);
    pythia->setDF1(StPythiaEvent::M105, df1_NLO_m105);
    pythia->setDF1(StPythiaEvent::P030, df1_NLO_p030);
    pythia->setDF1(StPythiaEvent::P045, df1_NLO_p045);
    pythia->setDF1(StPythiaEvent::P060, df1_NLO_p060);
    pythia->setDF1(StPythiaEvent::P070, df1_NLO_p070);
    pythia->setDF1(StPythiaEvent::GS_NLOA, df1_NLO_GSA);
    pythia->setDF1(StPythiaEvent::GS_NLOB, df1_NLO_GSB);
    pythia->setDF1(StPythiaEvent::GS_NLOC, df1_NLO_GSC);
    pythia->setDF1(StPythiaEvent::DSSV, df1_NLO_DSSV);
    pythia->setDF1(StPythiaEvent::DSSV2009a, df1_NLO_DSSV2009a);
    pythia->setDF1(StPythiaEvent::LSS1, df1_NLO_LSS1);
    pythia->setDF1(StPythiaEvent::LSS2, df1_NLO_LSS2);
    pythia->setDF1(StPythiaEvent::LSS3, df1_NLO_LSS3);
    pythia->setDF1(StPythiaEvent::LSS2010_delGpos, df1_NLO_LSS2010_delGpos);
    pythia->setDF1(StPythiaEvent::LSS2010_chsign_delG, df1_NLO_LSS2010_chsign_delG);
    pythia->setDF1(StPythiaEvent::AAC1, df1_NLO_AAC1);
    pythia->setDF1(StPythiaEvent::AAC2, df1_NLO_AAC2);
    pythia->setDF1(StPythiaEvent::AAC3, df1_NLO_AAC3);
    pythia->setDF1(StPythiaEvent::BB1, df1_NLO_BB1);
    pythia->setDF1(StPythiaEvent::BB2, df1_NLO_BB2);
    pythia->setDF1(StPythiaEvent::BB2010, df1_NLO_BB2010);
    pythia->setDF1(StPythiaEvent::DNS1, df1_NLO_DNS1);
    pythia->setDF1(StPythiaEvent::DNS2, df1_NLO_DNS2);

    pythia->setDF2(StPythiaEvent::LO, df2_LO);
    pythia->setDF2(StPythiaEvent::NLO, df2_NLO);
    pythia->setDF2(StPythiaEvent::ZERO, df2_NLO_g0);
    pythia->setDF2(StPythiaEvent::MAX, df2_NLO_gmax);
    pythia->setDF2(StPythiaEvent::MIN, df2_NLO_gmin);
    pythia->setDF2(StPythiaEvent::M015, df2_NLO_m015);
    pythia->setDF2(StPythiaEvent::M030, df2_NLO_m030);
    pythia->setDF2(StPythiaEvent::M045, df2_NLO_m045);
    pythia->setDF2(StPythiaEvent::M060, df2_NLO_m060);
    pythia->setDF2(StPythiaEvent::M075, df2_NLO_m075);
    pythia->setDF2(StPythiaEvent::M090, df2_NLO_m090);
    pythia->setDF2(StPythiaEvent::M105, df2_NLO_m105);
    pythia->setDF2(StPythiaEvent::P030, df2_NLO_p030);
    pythia->setDF2(StPythiaEvent::P045, df2_NLO_p045);
    pythia->setDF2(StPythiaEvent::P060, df2_NLO_p060);
    pythia->setDF2(StPythiaEvent::P070, df2_NLO_p070);
    pythia->setDF2(StPythiaEvent::GS_NLOA, df2_NLO_GSA);
    pythia->setDF2(StPythiaEvent::GS_NLOB, df2_NLO_GSB);
    pythia->setDF2(StPythiaEvent::GS_NLOC, df2_NLO_GSC);
    pythia->setDF2(StPythiaEvent::DSSV, df2_NLO_DSSV);
    pythia->setDF2(StPythiaEvent::DSSV2009a, df2_NLO_DSSV2009a);
    pythia->setDF2(StPythiaEvent::LSS1, df2_NLO_LSS1);
    pythia->setDF2(StPythiaEvent::LSS2, df2_NLO_LSS2);
    pythia->setDF2(StPythiaEvent::LSS3, df2_NLO_LSS3);
    pythia->setDF2(StPythiaEvent::LSS2010_delGpos, df2_NLO_LSS2010_delGpos);
    pythia->setDF2(StPythiaEvent::LSS2010_chsign_delG, df2_NLO_LSS2010_chsign_delG);
    pythia->setDF2(StPythiaEvent::AAC1, df2_NLO_AAC1);
    pythia->setDF2(StPythiaEvent::AAC2, df2_NLO_AAC2);
    pythia->setDF2(StPythiaEvent::AAC3, df2_NLO_AAC3);
    pythia->setDF2(StPythiaEvent::BB1, df2_NLO_BB1);
    pythia->setDF2(StPythiaEvent::BB2, df2_NLO_BB2);
    pythia->setDF2(StPythiaEvent::BB2010, df2_NLO_BB2010);
    pythia->setDF2(StPythiaEvent::DNS1, df2_NLO_DNS1);
    pythia->setDF2(StPythiaEvent::DNS2, df2_NLO_DNS2);
    
    pythia->setF1(StPythiaEvent::LO, f1_LO);
    pythia->setF1(StPythiaEvent::NLO, f1_NLO);
    
    pythia->setF2(StPythiaEvent::LO, f2_LO);
    pythia->setF2(StPythiaEvent::NLO, f2_NLO);

    particle_st* particleTable = particleTabPtr->GetTable();

    for (int i = 0; i < particleTabPtr->GetNRows(); ++i)
      pythia->addParticle(TParticle(particleTable[i].idhep,
				    particleTable[i].isthep,
				    particleTable[i].jmohep[0],
				    particleTable[i].jmohep[1],
				    particleTable[i].jdahep[0],
				    particleTable[i].jdahep[1],
				    TLorentzVector(particleTable[i].phep),
				    TLorentzVector(particleTable[i].vhep)));

}

//Gehrmann-Stirling NL0 Set A
Double_t StMCAsymMaker::get_polPDF_NLO_GSA(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_GSA=201;
    int polid=0;

    //    cout<<"get_polPDF_NLO_GSA: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;   
    if ((Q2>=1.0)&&(Q2<=1E+06))  polar_(&polset_NLO_GSA, &x, &Q2, parpol, &polid);
    //    cout<<"get_polPDF_NLO_GSA: UV="<<parpol[0]<<" DV="<<parpol[1]<<" UB="<<parpol[3]<<" DB="<<parpol[4]<<" S+Sbar="<<parpol[5]<<" GL="<<parpol[2]<<endl;

  
    //parpol[0] = uv - ubar so it is only the valence contribution. Need to add back in ubar for pythia since we don't know the difference between the sea and valence
    if (flavor==2) pdf=parpol[0]+parpol[2]; //uv + usea
    //parpol[1] = dv - dbar
    if (flavor==1) pdf=parpol[1]+parpol[3]; //dv + dsea quark
    //parpol[2] = 1/2 usea= 1/2(usea +ubar) = ubar
    if (flavor==-2) pdf=parpol[2];//ubar==1/2usea quark 
    //parpol[3] = 1/2 dsea= 1/2(dsea +dbar) = dbar
    if (flavor==-1) pdf=parpol[3];//dbar==1/2dsea quark
    //parpol[4] = 1/2 strangesea = 1/2*(ssea+sbar) = sbar
    if (abs(flavor)==3) pdf=parpol[4]; //s==1/2sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];//c+b+t == 1/2 strangesea

  return pdf;
}

//Gehrmann-Stirling NL0 Set B
Double_t StMCAsymMaker::get_polPDF_NLO_GSB(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_GSB=202;
    int polid=0;

    //cout<<"get_polPDF_NLO_GSB: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;
    if ((Q2>=1.0)&&(Q2<=1E+06))  polar_(&polset_NLO_GSB, &x, &Q2, parpol, &polid);
    //cout<<"get_polPDF_NLO_GSB: UV="<<parpol[0]<<" DV="<<parpol[1]<<" UB="<<parpol[3]<<" DB="<<parpol[4]<<" S+Sbar="<<parpol[5]<<" GL="<<parpol[2]<<endl;
    
  
    //parpol[0] = uv - ubar so it is only the valence contribution. Need to add back in ubar for pythia since we don't know the difference between the sea and valence
    if (flavor==2) pdf=parpol[0]+parpol[2]; //uv + usea
    //parpol[1] = dv - dbar
    if (flavor==1) pdf=parpol[1]+parpol[3]; //dv + dsea quark
    //parpol[2] = 1/2 usea= 1/2(usea +ubar) = ubar
    if (flavor==-2) pdf=parpol[2];//ubar==1/2usea quark 
    //parpol[3] = 1/2 dsea= 1/2(dsea +dbar) = dbar
    if (flavor==-1) pdf=parpol[3];//dbar==1/2dsea quark
    //parpol[4] = 1/2 strangesea = 1/2*(ssea+sbar) = sbar
    if (abs(flavor)==3) pdf=parpol[4]; //s==1/2sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];//c+b+t == 1/2 strangesea

  return pdf;
}

//Gehrmann-Stirling NL0 Set C
Double_t StMCAsymMaker::get_polPDF_NLO_GSC(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_GSC=203;
    int polid=0;

    //cout<<"get_polPDF_NLO_GSC: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;
    if ((Q2>=1.0)&&(Q2<=1E+06))  polar_(&polset_NLO_GSC, &x, &Q2, parpol, &polid);
    //cout<<"get_polPDF_NLO_GSC: UV="<<parpol[0]<<" DV="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" S+Sbar="<<parpol[4]<<" GL="<<parpol[5]<<endl;
   
    //parpol[0] = uv - ubar so it is only the valence contribution. Need to add back in ubar for pythia since we don't know the difference between the sea and valence
    if (flavor==2) pdf=parpol[0]+parpol[2]; //uv + usea
    //parpol[1] = dv - dbar
    if (flavor==1) pdf=parpol[1]+parpol[3]; //dv + dsea quark
    //parpol[2] = 1/2 usea= 1/2(usea +ubar) = ubar
    if (flavor==-2) pdf=parpol[2];//ubar==1/2usea quark 
    //parpol[3] = 1/2 dsea= 1/2(dsea +dbar) = dbar
    if (flavor==-1) pdf=parpol[3];//dbar==1/2dsea quark
    //parpol[4] = 1/2 strangesea = 1/2*(ssea+sbar) = sbar
    if (abs(flavor)==3) pdf=parpol[4]; //s==1/2sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];//c+b+t == 1/2 strangesea


  return pdf;
}





//GRSV LO standard
Double_t StMCAsymMaker::get_polPDF_LO(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_LO=101;
    int polid=0;
    //cout<<"get_polPDF_LO: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6)  polar_(&polset_LO, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_LO:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}


//GRSV NLO standard
Double_t StMCAsymMaker::get_polPDF_NLO(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO=102;
    int polid=0;
    //cout<<"get_polPDF_NLO: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO, &x, &Q2, parpol, &polid);
    //cout <<"get_polPDF_NLO:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO g0
Double_t StMCAsymMaker::get_polPDF_NLO_g0(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_g0=103;
    int polid=0;
    //cout<<"get_polPDF_NLO_g0: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_g0, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_g0:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}


//GRSV NLO gmax
Double_t StMCAsymMaker::get_polPDF_NLO_gmax(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_gmax=104;
    int polid=0;
    //cout<<"get_polPDF_NLO_gmax: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_gmax, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_gmax:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}


//GRSV NLO gmin
Double_t StMCAsymMaker::get_polPDF_NLO_gmin(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_gmin=105;
    int polid=0;
    //cout<<"get_polPDF_NLO_gmin: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_gmin, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_gmin:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO m015
Double_t StMCAsymMaker::get_polPDF_NLO_m015(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_m015=106;
    int polid=0;
    //cout<<"get_polPDF_NLO_m015: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_m015, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_m015:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO m030
Double_t StMCAsymMaker::get_polPDF_NLO_m030(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_m030=107;
    int polid=0;
    //cout<<"get_polPDF_NLO_m030: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_m030, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_m030:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO m045
Double_t StMCAsymMaker::get_polPDF_NLO_m045(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_m045=108;
    int polid=0;
    //cout<<"get_polPDF_NLO_m045: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_m045, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_m045:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO m060
Double_t StMCAsymMaker::get_polPDF_NLO_m060(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_m060=109;
    int polid=0;
    //cout<<"get_polPDF_NLO_m060: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_m060, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_m060:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO m075
Double_t StMCAsymMaker::get_polPDF_NLO_m075(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_m075=110;
    int polid=0;
    //cout<<"get_polPDF_NLO_m075: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_m075, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_m075:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO m090
Double_t StMCAsymMaker::get_polPDF_NLO_m090(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_m090=111;
    int polid=0;
    //cout<<"get_polPDF_NLO_m090: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_m090, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_m090:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO m105
Double_t StMCAsymMaker::get_polPDF_NLO_m105(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_m105=112;
    int polid=0;
    //cout<<"get_polPDF_NLO_m105: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_m105, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_m105:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO p030
Double_t StMCAsymMaker::get_polPDF_NLO_p030(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_p030=113;
    int polid=0;
    //cout<<"get_polPDF_NLO_p030: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_p030, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_p030:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO p045
Double_t StMCAsymMaker::get_polPDF_NLO_p045(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_p045=114;
    int polid=0;
    //cout<<"get_polPDF_NLO_p045: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_p045, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_p045:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO p060
Double_t StMCAsymMaker::get_polPDF_NLO_p060(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_p060=115;
    int polid=0;
    //cout<<"get_polPDF_NLO_p060: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_p060, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_p060:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//GRSV NLO p070
Double_t StMCAsymMaker::get_polPDF_NLO_p070(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_p070=116;
    int polid=0;
    //cout<<"get_polPDF_NLO_p070: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8&&Q2<=1.0e6) polar_(&polset_NLO_p070, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_p070:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//DSSV NLO
Double_t StMCAsymMaker::get_polPDF_NLO_DSSV(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_DSSV=301;
    int polid=0;
    //cout<<"get_polPDF_NLO_DSSV: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=1.0&&Q2<=1.0e5) polar_(&polset_NLO_DSSV, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_DSSV:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1]+parpol[2];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0]+parpol[3];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//DSSV 2009a NLO
Double_t StMCAsymMaker::get_polPDF_NLO_DSSV2009a(int flavor, double x, double Q2){

    double duv=0,ddv=0,dubar=0,ddbar=0,dstr=0,dglu=0;

    if (x>=1.0e-5&&x<=1.0&&Q2>=1.0&&Q2<=1.0e5) {
      dssvfit2009a_(&x,&Q2,&duv,&ddv,&dubar,&ddbar,&dstr,&dglu);
      switch (flavor) {
      case  1: return (ddv+ddbar)/x; //dv + dsea quark
      case  2: return (duv+dubar)/x; //uv + usea quark
      case -1: return ddbar/x;	//dbar==dsea quark
      case -2: return dubar/x;	//ubar==usea quark
      case  3:
      case -3: return dstr/x;	//s==sbar quark
      case 21: return dglu/x;	//gluon
      case  4:
      case -4:
      case  5:
      case -5:
      case  6:
      case -6: return dstr/x;
      }
    }
    return 1000;
}

//LSS NLO Scenario 1
Double_t StMCAsymMaker::get_polPDF_NLO_LSS1(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_LSS1=401;
    int polid=0;
    //    cout<<"get_polPDF_NLO_LSS1: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=1.0&&Q2<=5.8e5) polar_(&polset_NLO_LSS1, &x, &Q2, parpol, &polid);
    //    cout <<"getpolPDF_NLO_LSS1:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//LSS NLO Scenario 2
Double_t StMCAsymMaker::get_polPDF_NLO_LSS2(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_LSS2=402;
    int polid=0;
    //    cout<<"get_polPDF_NLO_LSS2: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=1.0&&Q2<=5.8e5) polar_(&polset_NLO_LSS2, &x, &Q2, parpol, &polid);
    //    cout <<"getpolPDF_NLO_LSS2:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//LSS NLO Scenario 3
Double_t StMCAsymMaker::get_polPDF_NLO_LSS3(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_LSS3=403;
    int polid=0;
    //    cout<<"get_polPDF_NLO_LSS3: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=1.0&&Q2<=5.8e5) polar_(&polset_NLO_LSS3, &x, &Q2, parpol, &polid);
    //    cout <<"getpolPDF_NLO_LSS3:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//LSS2010 pos x*DeltaG
Double_t StMCAsymMaker::get_polPDF_NLO_LSS2010_delGpos(int flavor, double x, double Q2)
{
  static int iini = 0;
  int iset = 1;  
  double uub, ddb, u, d, ub, db, st, gl;

  if (iini == 0) intini_.iini = 0;

  lss2010_(&iset,&x,&Q2,&uub,&ddb,&u,&d,&ub,&db,&st,&gl);

  if (iini == 0) iini = 1;

  switch (flavor) {
  case 1: return ddb/x;
  case 2: return uub/x;
  case -1: return db/x;
  case -2: return ub/x;
  case 3:
  case 4:
  case 5:
  case 6:
  case -3:
  case -4:
  case -5:
  case -6: return st/x;
  case 21: return gl/x;
  }

  return 1000;
}

//LSS2010 node x*DeltaG
Double_t StMCAsymMaker::get_polPDF_NLO_LSS2010_chsign_delG(int flavor, double x, double Q2)
{
  static int iini = 0;
  int iset = 2;
  double uub, ddb, u, d, ub, db, st, gl;

  if (iini == 0) intini_.iini = 0;

  lss2010_(&iset,&x,&Q2,&uub,&ddb,&u,&d,&ub,&db,&st,&gl);

  if (iini == 0) iini = 1;

  switch (flavor) {
  case 1: return ddb/x;
  case 2: return uub/x;
  case -1: return db/x;
  case -2: return ub/x;
  case 3:
  case 4:
  case 5:
  case 6:
  case -3:
  case -4:
  case -5:
  case -6: return st/x;
  case 21: return gl/x;
  }

  return 1000;
}

//AAC NLO Scenario 1
Double_t StMCAsymMaker::get_polPDF_NLO_AAC1(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_AAC1=501;
    int polid=0;
    //cout<<"get_polPDF_NLO_AAC1: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=1.0&&Q2<=1.0e8) polar_(&polset_NLO_AAC1, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_AAC1:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//AAC NLO Scenario 2
Double_t StMCAsymMaker::get_polPDF_NLO_AAC2(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_AAC2=502;
    int polid=0;
    //cout<<"get_polPDF_NLO_AAC2: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=1.0&&Q2<=1.0e8) polar_(&polset_NLO_AAC2, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_AAC2:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//AAC NLO Scenario 3
Double_t StMCAsymMaker::get_polPDF_NLO_AAC3(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_AAC3=503;
    int polid=0;
    //cout<<"get_polPDF_NLO_AAC3: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=1.0&&Q2<=1.0e8) polar_(&polset_NLO_AAC3, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_AAC3:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//BB NLO Scenario 1
Double_t StMCAsymMaker::get_polPDF_NLO_BB1(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_BB1=601;
    int polid=0;
    //cout<<"get_polPDF_NLO_BB1: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=1.0&&Q2<=1.0e6) polar_(&polset_NLO_BB1, &x, &Q2, parpol, &polid);
    //cout <<"get_polPDF_NLO_BB1:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//BB NLO Scenario 2
Double_t StMCAsymMaker::get_polPDF_NLO_BB2(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_BB2=602;
    int polid=0;
    //cout<<"get_polPDF_NLO_BB2: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=1.0&&Q2<=1.0e6) polar_(&polset_NLO_BB2, &x, &Q2, parpol, &polid);
    //cout <<"get_polPDF_NLO_BB2:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//BB NL0 2010
Double_t StMCAsymMaker::get_polPDF_NLO_BB2010(int flavor, double x, double Q2)
{
  static int iini = 0;
  int iset = 1;
  double uv, duv, dv, ddv, gl, dgl, qb, dqb, g1p, dg1p, g1n, dg1n;

  if (iini == 0) intini_.iini = 0;

  polpdf_(&iset,&x,&Q2,&uv,&duv,&dv,&ddv,&gl,&dgl,&qb,&dqb,&g1p,&dg1p,&g1n,&dg1n);

  if (iini == 0) iini = 1;

  switch (flavor) {
  case 1: return (dv+qb)/x;
  case 2: return (uv+qb)/x;
  case 3:
  case 4:
  case 5:
  case 6:
  case -1:
  case -2:
  case -3:
  case -4:
  case -5:
  case -6: return qb/x;
  case 21: return gl/x;
  }

  return 1000;
}

//DNS NLO Scenario 1
Double_t StMCAsymMaker::get_polPDF_NLO_DNS1(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_DNS1=701;
    int polid=0;
    //cout<<"get_polPDF_NLO_p070: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=1.0&&Q2<=5.0e4) polar_(&polset_NLO_DNS1, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_p070:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//DNS NLO Scenario 2
Double_t StMCAsymMaker::get_polPDF_NLO_DNS2(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_DNS2=702;
    int polid=0;
    //cout<<"get_polPDF_NLO_p070: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=1.0&&Q2<=5.0e4) polar_(&polset_NLO_DNS2, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO_p070:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

    if (flavor==1) pdf=parpol[1];      //dv + dsea quark
    if (flavor==2) pdf=parpol[0];      //uv + usea quark
    if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
    if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
    if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
    if (flavor==21) pdf=parpol[5];     //gluon
    if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];

    return pdf;
}

//Returns unpolarized CTEQ
Double_t StMCAsymMaker::get_unpolPDF_LO(int flavor, double x, double Q2){

    Double_t pdf=0.0;
    Int_t iset=3;//LO
    Int_t er=0;
    Int_t fl=10;

    if (flavor==1) fl=2;
    if (flavor==2) fl=1;
    if (flavor==-1) fl=-2;
    if (flavor==-2) fl=-1;
    if (flavor==21) fl=0;
    if (flavor==3) fl=3;
    if (flavor==-3) fl=-3;
    if (flavor==4) fl=4;
    if (flavor==-4) fl=-4;
    if (flavor==5) fl=5;
    if (flavor==-5) fl=-5;

    double Q=pow(Q2,0.5);
    pdf=ctq5pd_(&iset,&fl,&x,&Q,&er);
    if (er!=0) pdf=0.0;

    return pdf;
}

Double_t StMCAsymMaker::get_unpolPDF_NLO(int flavor, double x, double Q2){

    Double_t pdf=0.0; 
    Int_t iset=1;//NLO MSbar scheme
    Int_t er=0;
    Int_t fl=10;

    if (flavor==1) fl=2;
    if (flavor==2) fl=1;
    if (flavor==-1) fl=-2;
    if (flavor==-2) fl=-1;
    if (flavor==21) fl=0;
    if (flavor==3) fl=3;
    if (flavor==-3) fl=-3;
    if (flavor==4) fl=4;
    if (flavor==-4) fl=-4;
    if (flavor==5) fl=5;
    if (flavor==-5) fl=-5;

    double Q=pow(Q2,0.5);
    pdf=ctq5pd_(&iset,&fl,&x,&Q,&er);
    if (er!=0) pdf=0.0;

    return pdf;
}


Double_t StMCAsymMaker::getPartonicALL(double s, double t, double u, int sub, int inA, int inB, int outA, int outB){

    //Werner definitions:
    //1: qq'->qq' (qqbar'->qqbar') 2: qq->qq   3: qqbar->q'qbar'  4: qqbar->qqbar 5: qqbar->gg   6: gg->qqbar  7: qg->qg   8: gg->gg
    //PYTHIA definitions:
    //1: 11a                       2:11b       3: 12a             4:11 and 12b    5: 13          6: 53         7: 28       8: 68
    //NOTES:
    // 3==5==6==-1  1==7  1!=2 and 1!=4

    double N1,N2,N3,N4,N5,N6,N7,N8;
    double D1,D2,D3,D4,D5,D6,D7,D8;
    double all=-10;

    num_(&s,&t,&u,&N1,&N2,&N3,&N4,&N5,&N6,&N7,&N8);
    denom_(&s,&t,&u,&D1,&D2,&D3,&D4,&D5,&D6,&D7,&D8);
    if (0){
        cout<<"s="<<s<<" t="<<t<<" u="<<u<<" sub="<<sub<<" inA="<<inA<<" inB="<<inB<<" outA="<<outA<<" outB="<<outB<<endl;
        cout<<" 1="<<N1<<" "<<D1<<endl;
        cout<<" 2="<<N2<<" "<<D2<<endl;
        cout<<" 3="<<N3<<" "<<D3<<endl;
        cout<<" 4="<<N4<<" "<<D4<<endl;
        cout<<" 5="<<N5<<" "<<D5<<endl;
        cout<<" 6="<<N6<<" "<<D6<<endl;
        cout<<" 7="<<N7<<" "<<D7<<endl;
        cout<<" 8="<<N8<<" "<<D8<<endl;
    }


    if ((sub==11)&&(abs(inA)!=abs(inB))) all=N1/D1;
    if ((sub==11)&&(inA==inB)&&(outA==outB)) all=N2/D2;
    //This line added as bug fix 07/17/08 RHF
    //Before then these events given all=N2/D2
    if ((sub==11)&&(inA==(-1*inB))&&(outA==(-1*outB))&&(inA==outA)&&(inB==outB)) all=N4/D4;
    if ((sub==12)&&(abs(inA)!=abs(outA))) all=N3/D3;
    if ((sub==12)&&(abs(inA)==abs(outA))) all=N4/D4;
    if (sub==13) all=N5/D5;
    if (sub==53) all=N6/D6;
    if (sub==28) all=N7/D7;
    if (sub==68) all=N8/D8;

    //prompt photon subprocesses
    if (sub==29) all=N7/D7;//q_i + g -> q_i + gamma
    if (sub==14) all=N6/D6;//q_i+qbar_i -> g + gamma
    if (sub==18) all=N6/D6;//q_i+qbar_i -> gamma + gamma

    return all;
}

Double_t StMCAsymMaker::getProtonA1(Double_t x,Double_t Q2){

  // This piece coded by Pibero
  // Virtual photon-nucleon asymmetry measured from deep inelatic scattering
  // of polarized charged lepton beams from polarized targets. It is defined
  // as the sum of polarized PDF weighted by charge square over the sum of
  // unpolarized PDF weighted by charge square for all quark/antiquark flavors.

  // Flavors are PDG codes for the quarks and gluons:
  // down=1, up=2, strange=3, charm=4, bottom=5, top=6 
  // The weights are proportional to the square of the  
  // quark charge (in units of e) and indexed by flavor.
  // d=-1/3, u=2/3, s=-1/3, c=2/3, b=-1/3, t=2/3.
  // The PDG code for an antiquark is just the negative
  // of its partner quark. The charge is also the negative
  // of its partner quark charge, so the weights are the
  // same for quarks and antiquarks. The PDF are GRSV-standard
  
  const Double_t weights[] = { -1, 1, 4, 1, 4, 1, 4 };
  Double_t polSum = 0;
  Double_t unpolSum = 0;

  for (int flavor = 1; flavor <= 6; ++flavor) 
    {
      Double_t polPdf = get_polPDF_NLO(flavor, x, Q2);
      polPdf += get_polPDF_NLO(-flavor, x, Q2);
      polPdf *= weights[flavor];
      polSum += polPdf;
      Double_t unpolPdf = get_unpolPDF_NLO(flavor, x, Q2);
      unpolPdf += get_unpolPDF_NLO(-flavor, x, Q2);
      unpolPdf *= weights[flavor];
      unpolSum += unpolPdf;     
    }
  
  Double_t A1 = polSum / unpolSum;
  
  return A1;

}

struct PDFWrapper {
  typedef double (*PDF)(int,double,double);

  static const PDF pdfs[StPythiaEvent::NPDF];
  static PDF pdf;
  static int flavor;
  static double Q2;

  static void setPdfFlavorQ2(int pdf, int flavor, double Q2)
  {
    PDFWrapper::pdf = pdfs[pdf];
    PDFWrapper::flavor = flavor;
    PDFWrapper::Q2 = Q2;
  }

  static double eval(double& x) {  return pdf(flavor,x,Q2); }
};

const PDFWrapper::PDF PDFWrapper::pdfs[StPythiaEvent::NPDF] = {
  StMCAsymMaker::get_polPDF_LO,
  StMCAsymMaker::get_polPDF_NLO, // STD
  StMCAsymMaker::get_polPDF_NLO_g0,
  StMCAsymMaker::get_polPDF_NLO_gmax,
  StMCAsymMaker::get_polPDF_NLO_gmin,
  StMCAsymMaker::get_polPDF_NLO_m015,
  StMCAsymMaker::get_polPDF_NLO_m030,
  StMCAsymMaker::get_polPDF_NLO_m045,
  StMCAsymMaker::get_polPDF_NLO_m060,
  StMCAsymMaker::get_polPDF_NLO_m075,
  StMCAsymMaker::get_polPDF_NLO_m090,
  StMCAsymMaker::get_polPDF_NLO_m105,
  StMCAsymMaker::get_polPDF_NLO_p030,
  StMCAsymMaker::get_polPDF_NLO_p045,
  StMCAsymMaker::get_polPDF_NLO_p060,
  StMCAsymMaker::get_polPDF_NLO_p070,
  StMCAsymMaker::get_polPDF_NLO_GSA,
  StMCAsymMaker::get_polPDF_NLO_GSB,
  StMCAsymMaker::get_polPDF_NLO_GSC,
  StMCAsymMaker::get_polPDF_NLO_DSSV,
  StMCAsymMaker::get_polPDF_NLO_LSS1,
  StMCAsymMaker::get_polPDF_NLO_LSS2,
  StMCAsymMaker::get_polPDF_NLO_LSS3,
  StMCAsymMaker::get_polPDF_NLO_AAC1,
  StMCAsymMaker::get_polPDF_NLO_AAC2,
  StMCAsymMaker::get_polPDF_NLO_AAC3,
  StMCAsymMaker::get_polPDF_NLO_BB1,
  StMCAsymMaker::get_polPDF_NLO_BB2,
  StMCAsymMaker::get_polPDF_NLO_DNS1,
  StMCAsymMaker::get_polPDF_NLO_DNS2,
  StMCAsymMaker::get_polPDF_NLO_DSSV2009a,
  StMCAsymMaker::get_polPDF_NLO_LSS2010_delGpos,
  StMCAsymMaker::get_polPDF_NLO_LSS2010_chsign_delG,
  StMCAsymMaker::get_polPDF_NLO_BB2010
};

PDFWrapper::PDF PDFWrapper::pdf = 0;
int PDFWrapper::flavor = 0;
double PDFWrapper::Q2 = 0;

double StMCAsymMaker::get_polPDF_firstMoment(int pdf, int flavor, double Q2, double xmin, double xmax, double epsilon)
{
  PDFWrapper::setPdfFlavorQ2(pdf,flavor,Q2);
  return dinteg_(PDFWrapper::eval,xmin,xmax,epsilon);
}
