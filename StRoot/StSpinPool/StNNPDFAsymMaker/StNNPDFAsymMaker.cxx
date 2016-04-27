#include "StNNPDF.h"
#include "StNNPDFAsymMaker.h"

#include <pwd.h>

#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#include "tables/St_g2t_vertex_Table.h"

#include "StPDFs.h"

#include "StRoot/StSpinPool/StJetSkimEvent/StPythiaEvent.h"

ClassImp(StNNPDFAsymMaker)

StNNPDFAsymMaker::StNNPDFAsymMaker(const char *name):StMaker(name) {
  mUnpPdf = 0;
  mPolPdf = 0;
  mEvent = new StPythiaEvent;
}

StNNPDFAsymMaker::~StNNPDFAsymMaker() {
  if(!mEvent) delete mEvent;
  if(!mUnpPdf) delete mUnpPdf;
  if(!mPolPdf) delete mPolPdf;
}

Int_t StNNPDFAsymMaker::Init() {
  struct passwd* pw = getpwnam("zchang");
  mUnpPdf = new StNNPDF(Form("%s/public/pdfs/NNPDF30_nlo_as_0119", pw->pw_dir), 0);
  mPolPdf = new StNNPDF(Form("%s/public/pdfs/NNPDFpol11_100.LHgrid", pw->pw_dir));

  //mstw
  mPdfs = new StPDFs;
  mPdfs->init_unpolPDF_NLO(Form("%s/public/pdfs/mstw2008nlo", pw->pw_dir), 0);
  //dssv
  StPDFs::init_polPDF_DSSV2009a(Form("%s/public/pdfs/DSSV_GLUON_UPDATE.NLO", pw->pw_dir));
  return StMaker::Init();
}

void StNNPDFAsymMaker::Clear(const Option_t* c) {
  //    mEvent->Clear(c);
    StMaker::Clear(c);
}
//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StNNPDFAsymMaker::Make() {
  getEvent();
  getVertex();
  getPythia();
  getParticles();
  getAsymmetries();  
  return kStOK;
}
void StNNPDFAsymMaker::getEvent()
{
  TDataSet* geant = GetDataSet("geant");
  if (geant) {
    TDataSetIter iter(geant);
    St_g2t_event* eventDescriptor = (St_g2t_event*)iter("g2t_event");
    if (eventDescriptor) {
      g2t_event_st* eventTable = (g2t_event_st*)eventDescriptor->GetTable();
      if (eventTable) {
 	mEvent->setRunId(eventTable->n_run);
 	mEvent->setEventId(eventTable->n_event);
      }
    }
  }
}

void StNNPDFAsymMaker::getPythia()
{
  TDataSet* geant = GetDataSet("geant");
  if (geant) {
    TDataSetIter iter(geant);
    St_g2t_pythia* pythiaDescriptor = (St_g2t_pythia*)iter("g2t_pythia");
    if (pythiaDescriptor) {
      g2t_pythia_st* pythiaTable = (g2t_pythia_st*)pythiaDescriptor->GetTable();
      if (pythiaTable) {
	mEvent->setProcessId(pythiaTable->subprocess_id);
	mEvent->setS(pythiaTable->mand_s);
	mEvent->setT(pythiaTable->mand_t);
	mEvent->setU(pythiaTable->mand_u);
	mEvent->setPt(pythiaTable->hard_p);
	mEvent->setCosTheta(pythiaTable->cos_th);
	mEvent->setX1(pythiaTable->bjor_1);
	mEvent->setX2(pythiaTable->bjor_2);
	mEvent->setMstu72(pythiaTable->mstu72);
	mEvent->setMstu73(pythiaTable->mstu73);
	mEvent->setMstp111(pythiaTable->mstp111);
      }
    }
  }
}

void StNNPDFAsymMaker::getVertex()
{
  TDataSet* geant = GetDataSet("geant");
  if (geant) {
    TDataSetIter iter(geant);
    St_g2t_vertex* vertexDescriptor = (St_g2t_vertex*)iter("g2t_vertex");
    if (vertexDescriptor) {
      g2t_vertex_st* vertexTable = (g2t_vertex_st*)vertexDescriptor->GetTable();
      if (vertexTable) {
	mEvent->setVertex(TVector3(vertexTable[0].ge_x));
      }
    }
  }
}

void StNNPDFAsymMaker::getParticles()
{
  TDataSet* geant = GetDataSet("geant");
  if (geant) {
    TDataSetIter iter(geant);
    St_particle* particleDescriptor = (St_particle*)iter("particle");
    if (particleDescriptor) {
      particle_st* particleTable = (particle_st*)particleDescriptor->GetTable();
      if (particleTable) {
	for (int i = 0; i < particleDescriptor->GetNRows(); ++i) {
	  mEvent->addParticle(TParticle(particleTable[i].idhep, // pdg
					      particleTable[i].isthep, // status
					      particleTable[i].jmohep[0], // mother1
					      particleTable[i].jmohep[1], // mother2
					      particleTable[i].jdahep[0], // daughter1
					      particleTable[i].jdahep[1], // daughter2
					      TLorentzVector(particleTable[i].phep), // momentum and energy
					      TLorentzVector(particleTable[i].vhep))); // production vertex and time
	}
      }
    }
  }
}
void StNNPDFAsymMaker::getAsymmetries()
{
  float s = mEvent->s();
  float t = mEvent->t();
  float u = mEvent->u();
  int pid = mEvent->processId();
  int flavor1 = mEvent->particle(4)->GetPdgCode();
  int flavor2 = mEvent->particle(5)->GetPdgCode();
  int flavor3 = mEvent->particle(6)->GetPdgCode();
  int flavor4 = mEvent->particle(7)->GetPdgCode();
  float x1 = mEvent->x1();
  float x2 = mEvent->x2();
  float Q2 = mEvent->Q2();
  double partonic_all=getPartonicALL(s,t,u,pid,flavor1,flavor2,flavor3,flavor4);
  printf("x1 = %f, flavor1 = %d, x2 = %f, flavor2 = %d Q2=%f flavor3 = %d flavor4 = %d aLL = %f\n", x1, flavor1, x2, flavor2, Q2, flavor3, flavor4, partonic_all);
  mEvent->setPartonALL(partonic_all);
  if( (x1 > 1.0e-5 && x1 < 1.) && (x2 > 1.0e-5 && x2 < 1.) && (Q2 < 1.0e+5 && Q2 > 1.0) ){
    //mUnpPdf->InitPDF(0);
    double f1_NLO_NNPDF = mUnpPdf->XPDF(flavor1, x1, Q2);
    double f2_NLO_NNPDF = mUnpPdf->XPDF(flavor2, x2, Q2);
    mEvent->setF1NNPDF(f1_NLO_NNPDF);
    mEvent->setF2NNPDF(f2_NLO_NNPDF);
    printf("NLO UNP NNPDF: iset=0 f1_NLO=%f, f2_NLO=%f\n", f1_NLO_NNPDF, f2_NLO_NNPDF);
    for(int iset = 0; iset <=100; iset++){
      mPolPdf->InitPDF(iset);
      double df1_NLO_NNPDF = mPolPdf->XPDF(flavor1, x1, Q2);
      double df2_NLO_NNPDF = mPolPdf->XPDF(flavor2, x2, Q2);
      mEvent->setDF1NNPDF(iset, df1_NLO_NNPDF);
      mEvent->setDF2NNPDF(iset, df2_NLO_NNPDF);
      if(iset == 0) printf("NLO POL NNPDF: iset=%d df1_NLO=%f, df2_NLO=%f\n",iset, df1_NLO_NNPDF,df2_NLO_NNPDF);
    }
    //dssv
    double xdf1 = x1*StPDFs::get_polPDF_NLO_DSSV2009a(flavor1, x1, Q2);
    mEvent->setDF1(StPythiaEvent::DSSV2014, xdf1);
    double xdf2 = x2*StPDFs::get_polPDF_NLO_DSSV2009a(flavor2, x2, Q2);
    mEvent->setDF2(StPythiaEvent::DSSV2014, xdf2);
    //unp
    double xf1 = mPdfs->get_unpolPDF_NLO(flavor1, x1, Q2);
    mEvent->setF1(StPythiaEvent::NLO, xf1);
    double xf2 = mPdfs->get_unpolPDF_NLO(flavor2, x2, Q2);
    mEvent->setF2(StPythiaEvent::NLO, xf2);
    printf("dssv: df1 = %f df2 = %f cteq5d f1 = %f f2 = %f\n", xdf1, xdf2, xf1, xf2);
  }else{
    printf("x1 x2 and Q2 out of bound: 1.0E-05 < x1,2 < 1 and 1 < Q2 < 1.0E+5\n");
  }
}
Double_t StNNPDFAsymMaker::getPartonicALL(double s, double t, double u, int sub, int inA, int inB, int outA, int outB){

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
