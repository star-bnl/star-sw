// *-- Author : David Hardtke, based on Jan Balewski
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

#include "StGenericVertexMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "TH2.h"
#include "TNtuple.h"
#include "StMessMgr.h"

#include "StGenericVertexFinder.h"  
#include "StMinuitVertexFinder.h"  
#include "StppLMVVertexFinder.h"  

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


ClassImp(StGenericVertexMaker)
//___________________________________________________________
StGenericVertexMaker::StGenericVertexMaker(const char *name):StMaker(name)
{
  usebeamline = kFALSE;
  useCTB = kFALSE;
  eval = kFALSE;
  nEvTotal=nEvGood=0;
  externalFindUse=kTRUE; ///Default means that no finding actually done
  use_ITTF=kTRUE;
  m_Mode2=0;
}
//_____________________________________________________________________________
StGenericVertexMaker::~StGenericVertexMaker()
{
}

/*!
  The Init() method instantiates the VertexFinder() method. 
  Since this is  a Maker, the switch between the diverse methods
  will be made as part of the m_Mode mechanism. m_Mode will be
  a bit set to later allow multiple vertex finder running in the
  same pass. The structure does not allow this to happen for now
  (will need to have stack-like of VertexFinders and loop over
  them adding vertices in the collection)

  m_Mode = 0x1     Minuit
  m_Mode = 0x2     ppLMV4  This will not be able to run in parrallele of ppLMV5
  m_Mode = 0x3     ppLMV5  This will not be able to run in parrallele of ppLMV4

  Default          Minuit  (to preserver backward compatibility)

  All VertexFinder-s need to have the same methods (like DoUseITTF()
  NCtbMatches() etc ...) described in the GenericVertexFinder() class).
  Currentely, methods are not part of the base class and need
  cleanup.
  
*/
Int_t StGenericVertexMaker::Init()
{
  // setup params
  EtaCut=1.4; // Sensible default cut

  gMessMgr->Info() << "StGenericVertexMaker::Init: m_Mode=" <<  m_Mode <<"m_Mode2=" <<  m_Mode2 <<  endm;
  if ( m_Mode & 0x1){
    theFinder= new StMinuitVertexFinder();
  } else if ( m_Mode & 0x2){
    theFinder= new StppLMVVertexFinder();
    theFinder->SetMode(0);                 // this mode is an internal to ppLMV option switch
  } else if ( m_Mode & 0x3){
    theFinder= new StppLMVVertexFinder();
    theFinder->SetMode(1);                 // this mode is an internal to ppLMV option switch
  } else {
    // Later, this would NEVER make multiple possible vertex
    // finder unlike for option 0x1 .
    theFinder= new StMinuitVertexFinder();
  }

  if (use_ITTF) theFinder->DoUseITTF();
  //    theFinder->CTBforSeed();
  //    theFinder->UseVertexConstraint(-0.265,0.4088,-0.00135,0.0004333,0.0001);
  //theFinder->UseVertexConstraint(0.0,0.0,0.0,0.0,0.0001);
  if (eval) mEvalNtuple = new TNtuple("results","results","thX:thY:thZ:thStat:goodGlob:evX:evY:evZ:evStat:nPrim:nCTB:geantX:geantY:geantZ");

  theFinder->Init();
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StGenericVertexMaker::InitRun(int runnumber){
  if (useCTB) theFinder->CTBforSeed();
  if (usebeamline) {
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
       gMessMgr->Info() << "StGenericVertexMaker -- No Database for beamline" << endm;
     }   
     gMessMgr->Info() << "BeamLine Constraint: " << endm;
     gMessMgr->Info() << "x(z) = " << x0 << " + " << dxdz << " * z" << endm;
     gMessMgr->Info() << "y(z) = " << y0 << " + " << dydz << " * z" << endm;
     theFinder->UseVertexConstraint(x0,y0,dxdz,dydz,0.0001);
  }
  return StMaker::InitRun(runnumber);
}


//_____________________________________________________________________________
Int_t StGenericVertexMaker::Finish()
{
  //LSB TODO change over to using message manager
  gMessMgr->Info() << "StGenericVertexMaker::Finish " <<GetName() <<endm;
  gMessMgr->Info() << " Total events: " << nEvTotal << endm;
  gMessMgr->Info() << " Good events:  " << nEvGood  << endm; 


  //LSB TODO Leave this for now. Should really be using STAR/ROOT I/O scheme?
  if (eval) {
   TFile out("MinuitVertexEval.root","RECREATE");
   mEvalNtuple->Write();
   out.Close();
  }

  //LSB TODO check whether this is correct usage
  if(theFinder) delete theFinder;

  return  kStOK;
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
Bool_t StGenericVertexMaker::DoFit(){
  StThreeVectorD myvertex;

  StEvent *event = (StEvent *) GetInputDS("StEvent"); 
  assert(event);

  if (theFinder->fit(event)) {
    myvertex = theFinder->result();
    theFinder->printInfo();
  }  else {
    gMessMgr->Error() << "StGenericVertexMaker::DoFit: vertex fit failed, no vertex." << endm;
    return kFALSE;
  }

  return kTRUE;
  
}
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________


//_____________________________________________________________________________
Int_t StGenericVertexMaker::Make()
{
  nEvTotal++;
  primV  = NULL;
  mEvent = NULL;
  mEvent = (StEvent *)GetInputDS("StEvent"); 
  gMessMgr->Debug() << "StGenericVertexMaker::Make: StEvent pointer " << mEvent << endm;
  gMessMgr->Debug() << "StGenericVertexMaker::Make: external find use " << externalFindUse << endm;

  if(!externalFindUse){
    DoFit();
  } 

//   //For testing purposes
//   theFinder->DoNotUseITTF();
//   if (theFinder->fit(mEvent)) theFinder->printInfo();
//   if (theFinder->status()!=-1) theFinder->FillStEvent(mEvent);

//   theFinder->DoUseITTF();
//   if (theFinder->fit(mEvent)) theFinder->printInfo();
//   if (theFinder->status()!=-1) {
//     theFinder->FillStEvent(mEvent);
//     nEvGood++;
//   }


  if (eval)MakeEvalNtuple();

  if(!externalFindUse){
    ///Only fill StEvent when successful
    if (theFinder->status()!=-1){
      theFinder->FillStEvent(mEvent); 
      nEvGood++;
    }
  }
  return kStOK;
}

//-----------------------------------------------------------------------------

void StGenericVertexMaker::MakeEvalNtuple(){

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
  primV=mEvent->primaryVertex();
  if(!primV) {
    printf("primaryVertex()=NULL\n");
    mEvalNtuple->Fill(theFinder->result().x(),theFinder->result().y(),theFinder->result().z(),theFinder->status(),mEvent->summary()->numberOfGoodTracks(),-999.,-999.,-999.,-999.,-999.,theFinder->NCtbMatches(),gx,gy,gz);
    }
  else {
    printf("primaryVertex()= %f, %f %f, nTracks=%d\n",primV->position().x(),primV->position().y(),primV->position().z(),primV->numberOfDaughters());  
  mEvalNtuple->Fill(theFinder->result().x(),theFinder->result().y(),theFinder->result().z(),theFinder->status(),mEvent->summary()->numberOfGoodTracks(),primV->position().x(),primV->position().y(),primV->position().z(),primV->flag(),primV->numberOfDaughters(),theFinder->NCtbMatches(),gx,gy,gz);
  }
}

//____________________________________________________________________________
// LSB Commented out since moved to finder
// void const StGenericVertexMaker::FillStEvent(){
//   //Adds the vertex to StEvent (currently as a primary)
//   // Here we invent our own flag and other data to put in
//   // In real life we have to get it from somewhere (as done for position)
//   UInt_t minuitFlag=1000;
//   Float_t cov[6] = {0.1,0.2,0.3,0.4,0.5,0.6};
//   Float_t xSq = 5.43;
//   Float_t probXSq = 0.2468;

//   StPrimaryVertex* primV = new StPrimaryVertex();
//   primV->setPosition(theFinder->result());    //requires StThreeVectorF
//   primV->setFlag(minuitFlag+theFinder->status());       //requires unsigned int
//   primV->setCovariantMatrix(cov);      //requires float[6]
//   primV->setChiSquared(xSq);           //requires float
//   primV->setProbChiSquared(probXSq);       //requires float
//   //primV->setParent();  //requires StTrack* but we won't use this, also
//   //addDaughter(StTrack*) and removeDaughter(StTrack*) not used here
//   //addDaughter would be used when filling primary tracks in later maker

//   mEvent->addPrimaryVertex(primV);
//   gMessMgr->Debug()
//     << "StGenericVertexMaker::FillStEvent: Added new primary vertex" << endm;

// }

//------------  N O T   U S E D   -------------------------------



