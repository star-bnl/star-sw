/// *-- Author : David Hardtke, based on Jan Balewski
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

#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "Star2Root/St_DataSetIter.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StPrimaryVertex.h"
#include "TH2.h"
#include "TNtuple.h"
#include "St_base/StMessMgr.h"
#include "St_db_Maker/St_db_Maker.h"

#include "StGenericVertexMaker/StGenericVertexFinder.h"
#include "StGenericVertexMaker/StppLMVVertexFinder.h"
#include "StGenericVertexMaker/StFixedVertexFinder.h"

#include "StTreeMaker/StTreeMaker.h"

// Vertex finder implemtations
#include "Minuit/StMinuitVertexFinder.h"
#include "StiPPVertex/StPPVertexFinder.h"
#include "StvPPVertex/StPPVertexFinder.h"


#include "tables/St_g2t_vertex_Table.h" // tmp for Dz(vertex)
#include "tables/St_vertexSeed_Table.h" //

class StMuDst;
class TClonesArray;

// for Helix model
#include "St_base/StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)
#include "StarClassLibrary/SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif


//___________________________________________________________
StGenericVertexMaker::StGenericVertexMaker(const char *name):StMaker(name),
  useITTF(true),
  useBeamline(false),
  useCTB(false),
  usePCT(false),
  useBTOF(false),
  eval(false),
  externalFindUse(true),
  minTracks(0),
  mEvalNtuple(nullptr),
  mEvent(nullptr),
  primV(nullptr),
  theFinder(nullptr),
  nEvTotal(0),
  nEvGood(0)
{
}

//_____________________________________________________________________________
StGenericVertexMaker::~StGenericVertexMaker()
{
  SafeDelete(theFinder);
}


/*!
  The Init() method instantiates the VertexFinder() method.
  Since this is  a Maker, the switch between the diverse methods
  will be made as part of the m_Mode mechanism. m_Mode will be
  a bit set to later allow multiple vertex finder running in the
  same pass. The structure does not allow this to happen for now
  (will need to have stack-like of VertexFinders and loop over
  them adding vertices in the collection)

  All VertexFinder-s need to have the same methods (like DoUseITTF()
  NCtbMatches() etc ...) described in the GenericVertexFinder() class).
  Currentely, methods are not part of the base class and need
  cleanup.

*/
Int_t StGenericVertexMaker::Init()
{
  // setup params
  useITTF       = IAttr("ITTF");
  useBeamline   = IAttr("beamLine");
  useCTB        = IAttr("CTB");
  usePCT        = IAttr("PCT");
  useBTOF       = IAttr("BTOF");
  eval          = IAttr("eval");
  minTracks     = IAttr("minTracks");

  bool isMinuit = ( IAttr("VFMinuit") || IAttr("VFMinuit2") || IAttr("VFMinuit3") );
  bool isPPV    = ( IAttr("VFPPV") || IAttr("VFPPVnoCTB") || IAttr("VFPPVEv") ||  IAttr("VFPPVEvNoBTof") );

  // Recognize different beamline options to be used with some vertex finders
  StGenericVertexFinder::VertexFit_t vertexFitMode;

  if ( IAttr("beamline") && isPPV)
     vertexFitMode = StGenericVertexFinder::VertexFit_t::BeamlineNoFit;
  else if ( IAttr("beamline") && isMinuit )
     vertexFitMode = StGenericVertexFinder::VertexFit_t::Beamline1D;
  else if ( IAttr("beamline1D") )
     vertexFitMode = StGenericVertexFinder::VertexFit_t::Beamline1D;
  else if ( IAttr("beamline3D") )
     vertexFitMode = StGenericVertexFinder::VertexFit_t::Beamline3D;
  else
     vertexFitMode = StGenericVertexFinder::VertexFit_t::NoBeamline;


  if ( isMinuit ) // 3 versions of Minuit for ranking modes
  {
    LOG_INFO << "StMinuitVertexFinder::StMinuitVertexFinder is in use." << endm;

    theFinder= new StMinuitVertexFinder(vertexFitMode);

    if (IAttr("VFMinuit") ) ((StMinuitVertexFinder*) theFinder)->useOldBEMCRank();
    if (IAttr("VFMinuit3") ) ((StMinuitVertexFinder*) theFinder)->lowerSplitVtxRank();
    if (minTracks > 0) ((StMinuitVertexFinder*) theFinder)->SetMinimumTracks(minTracks);

  } else if ( IAttr("VFppLMV")){
    theFinder= new StppLMVVertexFinder();
    theFinder->SetMode(0);                 // this mode is an internal to ppLMV option switch

  } else if ( IAttr("VFppLMV5")){
    theFinder= new StppLMVVertexFinder();
    theFinder->SetMode(1);                 // this mode is an internal to ppLMV option switch

  } else if ( (IAttr("VFPPV") ||  IAttr("VFPPVnoCTB")) && !IAttr("VFPPVev")) {
    // 2 version of PPV w/ & w/o CTB
    LOG_INFO << "StGenericVertexMaker::Init: uses PPVertex finder"<<  endm;
    LOG_INFO << "StPPVertexFinder::StPPVertexFinder is in use" << endm;

    theFinder= new StPPVertexFinder(vertexFitMode);

    if ( IAttr("VFPPVnoCTB")) theFinder->UseCTB(kFALSE);	

    // Because VFstore is a local optio used by users, make sure it is checked first
    int vfstore = IAttr("VFstore");                  
    // ... while the global option would not be used if the local one is used
    //if (vfstore == 0) vfstore = IAttr(".gopt.VFstore");  <--- this will work whenevr gopt will be amde flexible
    
    if (vfstore > 0)  {
      LOG_INFO << "StGenericVertexMaker::Init: Using VFStore depth = " << vfstore << endm;
      ((StPPVertexFinder*)theFinder)->SetStoreUnqualifiedVertex(vfstore);
    }
    if ( IAttr("useBTOFmatchOnly") ){
      LOG_INFO << "StGenericVertexMaker::Init: UseBTOFmatchOnly option enabled" << endm; 
      ((StPPVertexFinder*)theFinder)->UseBTOFmatchOnly();
      useBTOF = 1;
    }

  } else if ( IAttr("VFPPVEv") ||  IAttr("VFPPVEvNoBTof")
           ||(IAttr("VFPPV")   &&  IAttr("Stv"))        )  { // 2 version of PPV w/ & w/o Btof
      LOG_INFO << "StGenericVertexMaker::Init: uses StvPPVertex finder(StEvent based)"<<  endm;
      LOG_INFO << "StPPVertexFinder::StPPVertexFinder is in use" << endm;

      theFinder= new StEvPPV::StPPVertexFinder();
      useBTOF = (IAttr("VFPPVEvNoBTof"))? 0:1;

  } else if ( IAttr("VFFV") || IAttr("VFMCE")) {
      theFinder = new StFixedVertexFinder();
      if (IAttr("VFMCE")){
	LOG_INFO << "StGenericVertexMaker::Init: fixed vertex using MC vertex" << endm;
	theFinder->SetMode(1);
      } else {
        theFinder->SetVertexPosition(0.,0.,0);
	LOG_INFO << "StGenericVertexMaker::Init: fixed vertex 'finder' selected" << endm;
      }

  } else {
    LOG_INFO << "StMinuitVertexFinder::StMinuitVertexFinder is in use." << endm;

    // Later, this would NEVER make multiple possible vertex
    // finder unlike for option 0x1 .
    theFinder= new StMinuitVertexFinder();
    isMinuit=kTRUE;

  }

  theFinder->UsePCT(usePCT);
  theFinder->UseBTOF(useBTOF);

  if(isMinuit) { // this is ugly, one should abort at 'else' above, Jan
    if (useITTF)  ((StMinuitVertexFinder*)theFinder)->DoUseITTF();
    if (useCTB) ((StMinuitVertexFinder*)theFinder)->CTBforSeed();
  } else {
    assert(!eval); // current implementation support only Minuit Vertex finder, JB 
  }

  if (eval)
    mEvalNtuple = new TNtuple("results", "results", "thX:thY:thZ:thStat:goodGlob:evX:evY:evZ:evStat:nPrim:nCTB:geantX:geantY:geantZ");

  theFinder->Init();
  return StMaker::Init();
}

//_____________________________________________________________________________
void StGenericVertexMaker::Clear(const char* opt){
  LOG_INFO <<" StGenericVertexMaker::Clear()"<<endm; 
  theFinder->Clear();
}


//_____________________________________________________________________________

/*!

  InitRun() will initialize the beam line constraint if this option
  is selected. From BFChain, beamLine constraint is set via the "beamline"
  option; this will intrinsically call the UseBeamLine() method BEFORE
  InitRun() is called.

 */
Int_t StGenericVertexMaker::InitRun(int runnumber){

  St_db_Maker *st_db_maker = static_cast<St_db_Maker*>(GetMaker("db"));
  theFinder->InitRun(runnumber, st_db_maker);

  return StMaker::InitRun(runnumber);
}


//_____________________________________________________________________________
Int_t StGenericVertexMaker::Finish()
{

  LOG_INFO << "StGenericVertexMaker::Finish " <<GetName() <<endm;
  LOG_INFO << " Total events: " << nEvTotal << endm;
  LOG_INFO << " Good events:  " << nEvGood  << endm;


  //LSB TODO Leave this for now. Should really be using STAR/ROOT I/O scheme?
  if (eval) {
   TFile out("MinuitVertexEval.root","RECREATE");
   mEvalNtuple->Write();
   out.Close();
  }
  
  if(theFinder) theFinder->Finish();  
  return  kStOK;
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
Bool_t StGenericVertexMaker::DoFit(){

  if (theFinder->fit(mEvent)) {
    theFinder->printInfo();
  }  else {
    LOG_ERROR << "StGenericVertexMaker::DoFit: vertex fit failed, no vertex." << endm;
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

  if (IAttr("useMuDst")) {
    StMuDst* muDst = 0;
    TClonesArray* vtxArray = 0;
    WhiteBoard("muDst",&muDst);
    WhiteBoard("vtxArray",&vtxArray);
    if (!(vtxArray && muDst)) {
      LOG_ERROR << "problems for refitting MuDst" << endm;
      return kStErr;
    }
    theFinder->fit(*muDst);
    theFinder->result(*vtxArray);
    return kStOk;
  }

  mEvent = (StEvent*) GetInputDS("StEvent");
  LOG_DEBUG << "StGenericVertexMaker::Make: StEvent pointer " << mEvent << endm;
  LOG_DEBUG << "StGenericVertexMaker::Make: external find use " << externalFindUse << endm;

  if(!externalFindUse){
    DoFit();
  }

  if (eval)MakeEvalNtuple();

  if(!externalFindUse){
    ///Only fill StEvent when successful
    if (theFinder->size()>0){
      theFinder->FillStEvent(mEvent);
      nEvGood++;
    }
  }
  return kStOK;
}

//-----------------------------------------------------------------------------

void StGenericVertexMaker::MakeEvalNtuple(){ // only for Minuit vertex finder

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
  int nCtb= ((StMinuitVertexFinder*)theFinder)->NCtbMatches(); 
  int stat= ((StMinuitVertexFinder*)theFinder)->statusMin();
  
  if (!primV) {
    LOG_INFO <<"primaryVertex()=NULL"<<endm;
    // why would one access x,y,z of the vertex if it is not found, Jan ???
    float x=999,y=999,z=999;
    mEvalNtuple->Fill(x,y,z,stat,mEvent->summary()->numberOfGoodTracks(),-999.,-999.,-999.,-999.,-999.,nCtb,gx,gy,gz);
  } else
  {
     LOG_INFO << Form("primaryVertex()= %f, %f %f, nTracks=%d\n",
       primV->position().x(), primV->position().y(), primV->position().z(),
       primV->numberOfDaughters()) << endm;

     mEvalNtuple->Fill(primV->position().x(), primV->position().y(), primV->position().z(),
       stat, mEvent->summary()->numberOfGoodTracks(),
       primV->position().x(), primV->position().y(), primV->position().z(), primV->flag(),
       primV->numberOfDaughters(), nCtb, gx, gy, gz);
  }
}
