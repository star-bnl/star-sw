//*-- Author : David Hardtke, based on Jan Balewski
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//   Maker to run minuit based vertex finder                            //
//   
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <strings.h>
#include <math.h>

#include "StMinuitVertexMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "TH2.h"
#include "TNtuple.h"
#include "StMessMgr.h"

#include "StMinuitVertexFinder.h"  

#include "StTreeMaker/StTreeMaker.h"

#include "tables/St_g2t_vertex_Table.h" // tmp for Dz(vertex)
#include "tables/St_vertexSeed_Table.h" //

// for Helix model
#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif


ClassImp(StMinuitVertexMaker)
//void StMinuitVertexMaker::Streamer(TBuffer &b){};  // do NOT change it J.B.

//_____________________________________________________________________________
StMinuitVertexMaker::StMinuitVertexMaker(const char *name):StMaker(name)
{
  use_beamline = false;
  use_CTB = false;
  eval = true;
}
//_____________________________________________________________________________
StMinuitVertexMaker::~StMinuitVertexMaker()
{
}

//_____________________________________________________________________________
Int_t StMinuitVertexMaker::Init()
{
  nEVtot=nEVfound=0;
  // setup params
  EtaCut=1.4; // tracks with larger eta are not considered

    myfinder= new StMinuitVertexFinder();
    //    myfinder->CTBforSeed();
    //    myfinder->UseVertexConstraint(-0.265,0.4088,-0.00135,0.0004333,0.0001);
    //myfinder->UseVertexConstraint(0.0,0.0,0.0,0.0,0.0001);
    if (eval) ntuple = new TNtuple("results","results","thX:thY:thZ:thStat:goodGlob:evX:evY:evZ:evStat:nPrim:nCTB:geantX:geantY:geantZ");
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StMinuitVertexMaker::InitRun(int runnumber){
  if (use_CTB) myfinder->CTBforSeed();
  if (use_beamline) {
     double x0 = 0.;
     double y0 = 0.;
     double dxdz = 0.;
     double dydz = 0.;


    // Get Current Beam Line Constraint from database
    
    TDataSet* dbDataSet = this->GetDataBase("Calibrations/rhic");
    
    if (dbDataSet) {
     vertexSeed_st* vSeed = ((St_vertexSeed*) (dbDataSet->FindObject("vertexSeed")))->GetTable();
     
     x0 = vSeed->x0;
     y0 = vSeed->y0;
     dxdz = vSeed->dxdz;
     dydz = vSeed->dydz;
    }
    else {
      cout << "StMinuitVertexMaker -- No Database for beamline" << endl;
    }   
     cout << "BeamLine Constraint: " << endl;
     cout << "x(z) = " << x0 << " + " << dxdz << " * z" << endl;
     cout << "y(z) = " << y0 << " + " << dydz << " * z" << endl << endl;
     myfinder->UseVertexConstraint(x0,y0,dxdz,dydz,0.0001);
  }
  return 0;
}


//_____________________________________________________________________________
Int_t StMinuitVertexMaker::Finish()
{
  cout <<" Finish fffffffffffffffff ::"<<GetName() <<endl;
  printf(" nEve tot=%d \n", nEVtot);
  //printStat();
  if (eval) {
   TFile out("MinuitVertexEval.root","RECREATE");
   ntuple->Write();
   out.Close();
  }
  return  kStOK;
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
void StMinuitVertexMaker::DoFit(){
  int newV=0;
  StThreeVectorD myvertex;

  StEvent *event = (StEvent *) GetInputDS("StEvent"); 
  assert(event);

  if (myfinder->fit(event)) {
    myvertex = myfinder->result();
    myfinder->printInfo();
    newV=1;
  }  else {
    cout << "Error: vertex fit failed, no vertex." << endl;
  }

  
}
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________


//_____________________________________________________________________________
Int_t StMinuitVertexMaker::Make()
{

  int i;
  nEVtot++;
  primV=NULL;
  stEvent=NULL;
  DoFit();
  if (!FillStEvent()) gMessMgr->Info() << "Error Filling Primary Vertex" << endm;
  if (eval)MakeEvalNtuple();
  return kStOK;
}

//-----------------------------------------------------------------------------

void StMinuitVertexMaker::MakeEvalNtuple(){
  stEvent= (StEvent *) GetInputDS("StEvent");  assert(stEvent);
  int EventId=stEvent->id();

  // get geant vertex
  St_DataSet *gds=GetDataSet("geant");
  St_g2t_vertex  *g2t_ver=0;
  g2t_vertex_st *gver=0;  
  if(gds)  g2t_ver=( St_g2t_vertex *)gds->Find("g2t_vertex");
  if(g2t_ver)gver=g2t_ver->GetTable();
  
  double gx = -999.;
  double gy = -999.;
  double gz = -999.;
    
  if(gver) {
    gx=gver->ge_x[0];
    gy=gver->ge_x[1];
    gz=gver->ge_x[2];
  }


  //  G E T     P R I M     V E R T E X 
  primV=stEvent->primaryVertex();
  if(!primV) {
    printf("primaryVertex()=NULL\n");
    ntuple->Fill(myfinder->result().x(),myfinder->result().y(),myfinder->result().z(),myfinder->status(),stEvent->summary()->numberOfGoodTracks(),-999.,-999.,-999.,-999.,-999.,myfinder->NCtbMatches(),gx,gy,gz);
    }
  else {
    printf("primaryVertex()= %f, %f %f, nTracks=%d\n",primV->position().x(),primV->position().y(),primV->position().z(),primV->numberOfDaughters());  
  ntuple->Fill(myfinder->result().x(),myfinder->result().y(),myfinder->result().z(),myfinder->status(),stEvent->summary()->numberOfGoodTracks(),primV->position().x(),primV->position().y(),primV->position().z(),primV->flag(),primV->numberOfDaughters(),myfinder->NCtbMatches(),gx,gy,gz);
  }
}

//____________________________________________________________________________
bool StMinuitVertexMaker::FillStEvent(){
  //Adds the vertex to StEvent (currently as a primary)
  // Here we invent our own flag and other data to put in
  // In real life we have to get it from somewhere (as done for position)
  UInt_t minuitFlag=1000;
  Float_t cov[6] = {0.1,0.2,0.3,0.4,0.5,0.6};
  Float_t xSq = 5.43;
  Float_t probXSq = 0.2468;

  StPrimaryVertex* primV = new StPrimaryVertex();
  primV->setPosition(myfinder->result());    //requires StThreeVectorF
  primV->setFlag(minuitFlag+myfinder->status());       //requires unsigned int
  primV->setCovariantMatrix(cov);      //requires float[6]
  primV->setChiSquared(xSq);           //requires float
  primV->setProbChiSquared(probXSq);       //requires float
  //primV->setParent();  //requires StTrack* but we won't use this, also
  //addDaughter(StTrack*) and removeDaughter(StTrack*) not used here
  //addDaughter would be used when filling primary tracks in later maker

  StEvent *mEvent = (StEvent *) GetInputDS("StEvent"); 
  mEvent->addPrimaryVertex(primV);
  gMessMgr->Info() << "Added new primary vertex" << endm;
  //Should take out this message in real code

  
  //Looks like would never be false since can't check if adding fails?
  //May as well just return void?
  return kTRUE;
}



void StMinuitVertexMaker::UseBeamLine() {use_beamline = true;}
void StMinuitVertexMaker::DoNotUseBeamLine() {use_beamline = false;}
void StMinuitVertexMaker::UseCTB() {use_CTB = true;}
void StMinuitVertexMaker::DoNotUseCTB() {use_CTB = false;}

//------------  N O T   U S E D   -------------------------------



