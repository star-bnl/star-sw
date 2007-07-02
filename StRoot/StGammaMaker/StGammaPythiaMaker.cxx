// StGammaPythiaMaker
// Class to access PYTHIA and GEANT records from simulated data
// Renee Fatemi 

#include "TFile.h"
#include "StChain.h"
#include "TLorentzVector.h"

#include "StLorentzVectorF.hh"

#include "StMaker.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"

#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEventTypes.hh"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcTrack.hh"
#include "StMcEvent/StMcVertex.hh"

#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StGammaPythiaMaker.h"


ClassImp(StGammaPythiaMaker)

StGammaPythiaMaker::StGammaPythiaMaker(const char *name):StMaker(name) {}

StGammaPythiaMaker::~StGammaPythiaMaker() {}

Int_t StGammaPythiaMaker::Init()
{
  return StMaker::Init();
}

void StGammaPythiaMaker::Clear(Option_t* option)
{

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
    
    Prompt4Mom.clear();
    Decay4Mom.clear();
    Pion04Mom.clear();
    Frag4Mom.clear();
    Initial4Mom.clear();
    Final4Mom.clear();
    
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
    
    return StMaker::Clear(option);
    
}

/// Make - Called each event
Int_t StGammaPythiaMaker::Make()
{

    cout << "StGammaPythiaMaker;:Make()" << endl;

    //Get StMcEvent to look at GEANT record
    mcEvent=(StMcEvent*)GetDataSet("StMcEvent"); assert(mcEvent);
    
    //GET EVTID FROM MuDST
    muDstMaker = (StMuDstMaker*)GetMaker("MuDst"); assert(muDstMaker);
    StMuDst* dst = muDstMaker->muDst(); assert(dst);
    muEvent = dst->event();  assert(muEvent);
    StEventInfo &info=muEvent->eventInfo();
    evtid=info.id();
    
    //GET GEANT EVENT
    TDataSet *Event = GetDataSet("geant"); //  Event->ls(3);
    
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

    // Collect photons from primary vertex
    if(mcEvent) 
    {
    
        StMcVertex *primary=mcEvent->primaryVertex();
        
        // Loop over daughters of primary vertex
        for(UInt_t id = 0; id < primary->numberOfDaughters(); ++id)
        {
        
            StMcTrack *track = primary->daughter(id);
            if ( !track ) continue;
            const StLorentzVectorF& trackV=track->fourMomentum();
            
            // Store all pion Four Momentum
            if (track->geantId() == 7)
            {
                Pion04Mom.push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
            }
            
            //////////////////////////////
            // Check for prompt photons //
            //////////////////////////////
            
            // Check if subprocess can produce a prompt photon
            if( (pid==14)||(pid==18)||(pid==29)||(pid==114)||(pid==115) )
            {
                
                // Require that track is a photon
                bool prompt_flag = (track->geantId() == 1);
                // Require that track is not a daughter of the initial partons
                prompt_flag &= particleTable[track->eventGenLabel() - 1].jmohep[0] - 1 > 5;
                // Require that track is a daughter of a photon (how PYTHIA handles the evoluton of the
                // final state prompt photon to a stable photon)
                prompt_flag &= particleTable[particleTable[track->eventGenLabel() - 1].jmohep[0] - 1].idhep == 22;
                
                // Store any prompt photons
                if(prompt_flag)
                {
                    Prompt4Mom.push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
                }
                // All other photons are treated as decay photons
                else if(track->geantId() == 1)
                {

                    bool radiation_flag = particleTable[track->eventGenLabel() - 1].jmohep[0] - 1 < 6;
                    if(radiation_flag)
                    {
                        Initial4Mom.push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
                    }
                    else
                    {
            
                        bool decay_flag = track->parent()->geantId() == 7;
                        decay_flag |= track->parent()->geantId() == 17;
                        if(decay_flag)
                        {
                            Decay4Mom.push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
                        }
                        else
                        {
                            Frag4Mom.push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
                        }
                        
                    }
                
                }
                
            }
        
            // Store 2-2 QCD decay photons
            if( (pid==11)||(pid==12)||(pid==13)||(pid==28)||(pid==53)||(pid==68)||(pid==96) )
            {
            
                if(track->geantId() == 1)
                {

                    bool radiation_flag = particleTable[track->eventGenLabel() - 1].jmohep[0] - 1 < 6;
                    if(radiation_flag)
                    {
                        Initial4Mom.push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
                    }
                    else
                    {          
            
                        bool decay_flag = track->parent()->geantId() == 7;
                        decay_flag |= track->parent()->geantId() == 17;
                        
                        if(decay_flag)
                        {
                            Decay4Mom.push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
                        }
                        else
                        {
                            Frag4Mom.push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
                        }
                        
                    }
                
                }

            }
        
        }
        
    }

    // Collect photons from secondary vertices
    collectDecayPhotons();  
    
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
  
    // Print OUT PYTHIA RECORD 
    LOG_DEBUG << "PID/evtid from McEvent ="<<pid<<"/"<<evtid<<"  PID/evtid from Table =" << geantPID << "/" << geantID << endm;
    LOG_DEBUG << "row |   id   |   px   |   py   |   pz   |   E   |   m   | status | moth1 | moth2 | daught1 | daught2" << endm;
    for(int i = 0; i < particleTabPtr->GetNRows(); ++i) 
    {	
  
        LOG_DEBUG << i << "  " << particleTable[i].idhep << "  " << particleTable[i].phep[0] << "  " 
                  << particleTable[i].phep[1] << "  " << particleTable[i].phep[2] << "  " << particleTable[i].phep[3]
                  << "  " << particleTable[i].phep[4] << "  " << particleTable[i].isthep << "  "
                  << particleTable[i].jmohep[0] << "  " << particleTable[i].jmohep[1] << "  " 
                  << particleTable[i].jdahep[0] << "  " << particleTable[i].jdahep[1] << endm;
    }
    
    if ((pid==14)||(pid==18)||(pid==29)||(pid==114)||(pid==115))  //1-2 prompt gammas and the rest are decay
    {
    
        for(int i = 0; i < 9; ++i) 
        {
    
            //need to check if 8 is the correct row in MuDst and also for 114 and 115 events
            if ((particleTable[i].isthep==1)&&(particleTable[i].idhep==22)&&(particleTable[i].jmohep[0]<=8)&&(particleTable[i].jmohep[1]<=8))
            {
                LOG_DEBUG << " PYTHIA prompt px=" << particleTable[i].phep[0]
                          << " py=" << particleTable[i].phep[1] << " pz=" <<particleTable[i].phep[2] << endm;
            }
            	     	  
        }
        
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
    
    if(0) 
    {
        printf("LO:  df1_LO=%f, df2_LO=%f, f1_LO=%f, f2_LO=%f, weight_LO=%f\n",df1_LO,df2_LO,f1_LO,f2_LO,weight_LO);
        printf("NLO:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO,df2_NLO,f1_NLO,f2_NLO,weight_NLO);
        printf("NLO_gmin:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_gmin,df2_NLO_gmin,f1_NLO,f2_NLO,weight_NLO_gmin);
        printf("NLO_g0:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_g0,df2_NLO_g0,f1_NLO,f2_NLO,weight_NLO_g0);
        printf("NLO_gmax:  df1_NLO=%f, df2_NLO=%f, f1_NLO=%f, f2_NLO=%f, weight_NLO=%f\n",df1_NLO_gmax,df2_NLO_gmax,f1_NLO,f2_NLO,weight_NLO_gmax);
    } 
    
    return kStOK;   
    
}


//GRSV LO standard 
Double_t StGammaPythiaMaker::get_polPDF_LO(int flavor, double x, double Q2)
{

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_LO=101;
    int polid=0;
    LOG_DEBUG<<"get_polPDF_LO: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endm;
    
    if (Q2>=0.8)  polar_(&polset_LO, &x, &Q2, parpol, &polid);
    LOG_DEBUG<<"getpolPDF_LO:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endm;
    
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
Double_t StGammaPythiaMaker::get_polPDF_NLO(int flavor, double x, double Q2)
{

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO=102;
    int polid=0;
    LOG_DEBUG<<"get_polPDF_NLO: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endm;
    
    if (Q2>=0.8) polar_(&polset_NLO, &x, &Q2, parpol, &polid);
    LOG_DEBUG<<"getpolPDF_NLO:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endm;
    
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
Double_t StGammaPythiaMaker::get_polPDF_NLO_g0(int flavor, double x, double Q2)
{

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_g0=103;
    int polid=0;
    LOG_DEBUG<<"get_polPDF_NLO_g0: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endm;
    
    if (Q2>=0.8) polar_(&polset_NLO_g0, &x, &Q2, parpol, &polid);
    LOG_DEBUG<<"getpolPDF_NLO_g0:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endm;
    
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
Double_t StGammaPythiaMaker::get_polPDF_NLO_gmax(int flavor, double x, double Q2)
{

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_gmax=104;
    int polid=0;
    LOG_DEBUG<<"get_polPDF_NLO_gmax: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endm;
    
    if (Q2>=0.8) polar_(&polset_NLO_gmax, &x, &Q2, parpol, &polid);
    LOG_DEBUG<<"getpolPDF_NLO_gmax:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endm;
    
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
Double_t StGammaPythiaMaker::get_polPDF_NLO_gmin(int flavor, double x, double Q2)
{

    double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
    double pdf=1000;
    int polset_NLO_gmin=105;
    int polid=0;
    LOG_DEBUG<<"get_polPDF_NLO_gmin: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endm;
    
    if (Q2>=0.8) polar_(&polset_NLO_gmin, &x, &Q2, parpol, &polid);
    LOG_DEBUG<<"getpolPDF_NLO_gmin:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endm;
    
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
Double_t StGammaPythiaMaker::get_unpolPDF_LO(int flavor, double x, double Q2)
{

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

Double_t StGammaPythiaMaker::get_unpolPDF_NLO(int flavor, double x, double Q2)
{

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


Double_t StGammaPythiaMaker::getPartonicALL(double s, double t, double u, int sub, int inA, int inB, int outA, int outB)
{

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
    
    
    LOG_DEBUG<<"s="<<s<<" t="<<t<<" u="<<u<<" sub="<<sub<<" inA="<<inA<<" inB="<<inB<<" outA="<<outA<<" outB="<<outB<<endm;
    LOG_DEBUG<<" 1="<<N1<<" "<<D1<<endm;
    LOG_DEBUG<<" 2="<<N2<<" "<<D2<<endm;
    LOG_DEBUG<<" 3="<<N3<<" "<<D3<<endm;
    LOG_DEBUG<<" 4="<<N4<<" "<<D4<<endm;
    LOG_DEBUG<<" 5="<<N5<<" "<<D5<<endm;
    LOG_DEBUG<<" 6="<<N6<<" "<<D6<<endm;
    LOG_DEBUG<<" 7="<<N7<<" "<<D7<<endm;
    LOG_DEBUG<<" 8="<<N8<<" "<<D8<<endm;
    
    //QCD 2->2 subprocesses
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
       
    return all;

}

vector<StMcTrack*>  StGammaPythiaMaker::filterMcTracks( StMcVertex *vertex, Int_t geantId )
{

    vector<StMcTrack*> listOfTracks;
    if (!vertex) 
    return listOfTracks;
    
    UInt_t ndaughters = vertex->numberOfDaughters();
    for(UInt_t id = 0; id < ndaughters; ++id)
    {
    
        StMcTrack *track = vertex->daughter(id);
        if ( !track ) continue;
        
        StMcTrack *parent=track->parent();
        const StLorentzVectorF& trackV=track->fourMomentum();
        const StLorentzVectorF& parentV=parent->fourMomentum();
        
        LOG_DEBUG << " track genLabel=" << track->eventGenLabel() << " geantID=" << track->geantId() 
                  << " Px="<<trackV.px() << "  Py="<<trackV.py() << "  Pz=" << trackV.pz() << " key=" << track->key() << endl;
        LOG_DEBUG << " parent genLabel=" << parent->eventGenLabel() << " geantID=" << parent->geantId()
                  << " Px=" << parentV.px() << "  Py=" << parentV.py() << "  Pz=" << parentV.pz() << " key=" << parent->key() << endl;
        
        if ( track->geantId() == geantId ) listOfTracks.push_back(track);
        
    }
    
    return listOfTracks;

}

void StGammaPythiaMaker::collectDecayPhotons()
{
    if (StMcEvent* event = (StMcEvent*)GetDataSet("StMcEvent")) 
    {
        if (StMcVertex* vertex = event->primaryVertex()) 
        {
            for (unsigned i = 0; i < vertex->numberOfDaughters(); ++i) 
            {
                StMcTrack* track = vertex->daughter(i);
                if (StMcVertex* stopVertex = track->stopVertex()) 
                {
                    collectDecayPhotons(stopVertex);
                }
            }
        }
    }
}

void StGammaPythiaMaker::collectDecayPhotons(StMcVertex* vertex)
{

    for (unsigned i = 0; i < vertex->numberOfDaughters(); ++i) 
    {
    
        StMcTrack* track = vertex->daughter(i);
        const StLorentzVectorF& trackV = track->fourMomentum();
        
        // Store all pion Four Momentum
        if (track->geantId() == 7)
        {
            Pion04Mom.push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
        }
        
        if (track->geantId() == 1) 
        {
            
            bool decay_flag = track->parent()->geantId() == 7;
            decay_flag |= track->parent()->geantId() == 17;
            if(decay_flag)
            {
                Decay4Mom.push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
            }
            else
            {
                Frag4Mom.push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
            }

        }
        if (StMcVertex* stopVertex = track->stopVertex()) 
        {
            collectDecayPhotons(stopVertex);
        }
    }
}
