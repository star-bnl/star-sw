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

ClassImp(StMCAsymMaker)

StMCAsymMaker::StMCAsymMaker(const char *name):StMaker(name) {
    mEvent = new StPythiaEvent();
}

StMCAsymMaker::~StMCAsymMaker() {
    delete mEvent;
}

Int_t StMCAsymMaker::Init() {
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

    //LO
    df1_LO=get_polPDF_LO(flavor1,x1,Q2);
    df2_LO=get_polPDF_LO(flavor2,x2,Q2);
    f1_LO=get_unpolPDF_LO(flavor1,x1,Q2);
    f2_LO=get_unpolPDF_LO(flavor2,x2,Q2);
    weight_LO=(df1_LO*df2_LO*partonic_all)/(f1_LO*f2_LO);

    //NLO
    df1_NLO=get_polPDF_NLO(flavor1,x1,Q2);
    df2_NLO=get_polPDF_NLO(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO=(df1_NLO*df2_NLO*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_g0
    df1_NLO_g0=get_polPDF_NLO_g0(flavor1,x1,Q2);
    df2_NLO_g0=get_polPDF_NLO_g0(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_g0=(df1_NLO_g0*df2_NLO_g0*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_gmax
    df1_NLO_gmax=get_polPDF_NLO_gmax(flavor1,x1,Q2);
    df2_NLO_gmax=get_polPDF_NLO_gmax(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_gmax=(df1_NLO_gmax*df2_NLO_gmax*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_gmin
    df1_NLO_gmin=get_polPDF_NLO_gmin(flavor1,x1,Q2);
    df2_NLO_gmin=get_polPDF_NLO_gmin(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_gmin=(df1_NLO_gmin*df2_NLO_gmin*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_m015
    df1_NLO_m015=get_polPDF_NLO_m015(flavor1,x1,Q2);
    df2_NLO_m015=get_polPDF_NLO_m015(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_m015=(df1_NLO_m015*df2_NLO_m015*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_m030
    df1_NLO_m030=get_polPDF_NLO_m030(flavor1,x1,Q2);
    df2_NLO_m030=get_polPDF_NLO_m030(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_m030=(df1_NLO_m030*df2_NLO_m030*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_m045
    df1_NLO_m045=get_polPDF_NLO_m045(flavor1,x1,Q2);
    df2_NLO_m045=get_polPDF_NLO_m045(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_m045=(df1_NLO_m045*df2_NLO_m045*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_m060
    df1_NLO_m060=get_polPDF_NLO_m060(flavor1,x1,Q2);
    df2_NLO_m060=get_polPDF_NLO_m060(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_m060=(df1_NLO_m060*df2_NLO_m060*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_m075
    df1_NLO_m075=get_polPDF_NLO_m075(flavor1,x1,Q2);
    df2_NLO_m075=get_polPDF_NLO_m075(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_m075=(df1_NLO_m075*df2_NLO_m075*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_m090
    df1_NLO_m090=get_polPDF_NLO_m090(flavor1,x1,Q2);
    df2_NLO_m090=get_polPDF_NLO_m090(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_m090=(df1_NLO_m090*df2_NLO_m090*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_m105
    df1_NLO_m105=get_polPDF_NLO_m105(flavor1,x1,Q2);
    df2_NLO_m105=get_polPDF_NLO_m105(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_m105=(df1_NLO_m105*df2_NLO_m105*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_p030
    df1_NLO_p030=get_polPDF_NLO_p030(flavor1,x1,Q2);
    df2_NLO_p030=get_polPDF_NLO_p030(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_p030=(df1_NLO_p030*df2_NLO_p030*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_p045
    df1_NLO_p045=get_polPDF_NLO_p045(flavor1,x1,Q2);
    df2_NLO_p045=get_polPDF_NLO_p045(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_p045=(df1_NLO_p045*df2_NLO_p045*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_p060
    df1_NLO_p060=get_polPDF_NLO_p060(flavor1,x1,Q2);
    df2_NLO_p060=get_polPDF_NLO_p060(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_p060=(df1_NLO_p060*df2_NLO_p060*partonic_all)/(f1_NLO*f2_NLO);

    //NLO_p070
    df1_NLO_p070=get_polPDF_NLO_p070(flavor1,x1,Q2);
    df2_NLO_p070=get_polPDF_NLO_p070(flavor2,x2,Q2);
    f1_NLO=get_unpolPDF_NLO(flavor1,x1,Q2);
    f2_NLO=get_unpolPDF_NLO(flavor2,x2,Q2);
    weight_NLO_p070=(df1_NLO_p070*df2_NLO_p070*partonic_all)/(f1_NLO*f2_NLO);

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
    }

    fillPythiaEvent(mEvent);

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

    pythia->setF1(StPythiaEvent::LO, f1_LO);
    pythia->setF1(StPythiaEvent::NLO, f1_NLO);
    
    pythia->setF2(StPythiaEvent::LO, f2_LO);
    pythia->setF2(StPythiaEvent::NLO, f2_NLO);

    particle_st* particleTable = particleTabPtr->GetTable();

    for (int i = 4; i < 8; ++i) pythia->addParticle(particleTable[i]);
}

//GRSV LO standard 
Double_t StMCAsymMaker::get_polPDF_LO(int flavor, double x, double Q2){

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_LO=101;
    int polid=0;
    //cout<<"get_polPDF_LO: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

    if (Q2>=0.8)  polar_(&polset_LO, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO, &x, &Q2, parpol, &polid);
    //cout <<"getpolPDF_NLO:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

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

    if (Q2>=0.8) polar_(&polset_NLO_g0, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_gmax, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_gmin, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_m015, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_m030, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_m045, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_m060, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_m075, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_m090, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_m105, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_p030, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_p045, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_p060, &x, &Q2, parpol, &polid);
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

    if (Q2>=0.8) polar_(&polset_NLO_p070, &x, &Q2, parpol, &polid);
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
    //1: 11a                       2:11b       3: 12a             4:12b           5: 13          6: 53         7: 28       8: 68
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
    if ((sub==11)&&(abs(inA)==abs(inB))) all=N2/D2;
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

