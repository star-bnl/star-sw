/*
 *
 * Author: J. Bouchet, Subatech; Y. Fisyak, BNL; J. Vanfossen, Kent State
 *
 * 
 **********************************************************
 *
 */

#include "Stiostream.h"
#include "StSstFastSimMaker.h"
#include "StHit.h"
#include "StEventTypes.h"
#include "StChain.h"

#include "Sti/StiVMCToolKit.h"
#include "StarClassLibrary/StRandom.hh"
#include "tables/St_HitError_Table.h"

#include <math.h>

#include "tables/St_g2t_ssd_hit_Table.h"
#include "StSsdDbMaker/StSstDbMaker.h"
#include "StSstUtil/StSstBarrel.hh"
#include "StSstHitCollection.h"
#include "StEvent.h"
#include "StMcEvent.hh"

ClassImp(StSstFastSimMaker)
  
StSstFastSimMaker::StSstFastSimMaker(const char *name):StMaker(name),mDimensions(0){};

StSstFastSimMaker::~StSstFastSimMaker(){}

Int_t StSstFastSimMaker::Init()
{
  int seed=time(NULL);
  mRandom=new StRandom();
  mRandom->setSeed(seed);

  // Define various SSD specific geom. variables.
  mResXSst = 0.0000;				
  mResZSst = 0.0000;
  return kStOk;
}

Int_t StSstFastSimMaker::InitRun(int RunNo)
{
  // Define various SSD hit errors from database
  TDataSet *set = GetDataBase("Calibrations/tracker");
  St_HitError *tableSet = (St_HitError *)set->Find("sstHitError");
  HitError_st* hitError = tableSet->GetTable();
  mResXSst = sqrt(hitError->coeff[0]);
  mResZSst = sqrt(hitError->coeff[3]);
  LOG_DEBUG << "Smearing SST hits by " << mResXSst << " " << mResZSst << " cm in the SST " << endm;

 //load geometry
  if (!gGeoManager) GetDataBase("VmcGeometry");
  LOG_DEBUG << "Geometry Loaded" << endm;

  if (!gGeoManager)
    {
      LOG_ERROR << " StSstFastSimMaker - E - gGeoManager is not available." << endm;
      return kStErr;
    }
  
  //geometry parameters from StSstDbMaker
  mDimensions = StSstDbMaker::instance()->getSstDimensions();
  if(!mDimensions)
    { 
      LOG_ERROR << "No SST dimensions table" << endm;
      return kStFatal;
    } 
  
  setSstParameters(mDimensions);

  return kStOk;
}

Int_t StSstFastSimMaker::Finish()
{
  return kStOk;
}

Int_t StSstFastSimMaker::Make()
{
  // Get the input data structures from StEvent
 LOG_DEBUG << "StSstFastSimMaker::Make()" << endm;
  
  // Get the input data structures from StEvent and StMcEvent
  StEvent* rcEvent = (StEvent*) GetInputDS("StEvent");
  if (! rcEvent)
    {
      LOG_DEBUG << "No StEvent on input" << endm;
      return kStWarn;
    }

 StSstHitCollection *sstHitCol = rcEvent->sstHitCollection();

      bool newCollection = false;
      if (!sstHitCol)
      {
	 LOG_DEBUG << "No existing StSstHitCollection. Creating a new one..." <<endm;
	 sstHitCol = new StSstHitCollection();
	 newCollection = true;
      }

      if(newCollection) rcEvent->setSstHitCollection(sstHitCol);
      LOG_DEBUG << " size of hit collection : " << sstHitCol->numberOfHits() << endm;


  TDataSetIter geant(GetInputDS("geant"));

  St_g2t_ssd_hit *g2t_ssd_hit = (St_g2t_ssd_hit *) geant("g2t_ssd_hit");
  if (! g2t_ssd_hit) {
    LOG_WARN << "No g2t_ssd_hit on input, bye bye" << endm; 
    return kStWarn;
  }
  g2t_ssd_hit_st *g2t         = g2t_ssd_hit->GetTable();

  StSstBarrel *mySst = StSstDbMaker::instance()->getSst();
  int inContainer = 0;
  
  LOG_INFO<<"####      START OF SST FAST SIM MAKER        ####"<<endm;

  if(g2t_ssd_hit){
    LOG_INFO<< "Size of g2t_ssd table= " << (int)g2t_ssd_hit->GetTableSize()<<endm;
    int minWaf      = mSstLayer*1000;
    int currWafId   = 0;
    int currWafNumb = 0;
    int currLadder  = 0;
    int i           = 0;
    unsigned char c   = 0;
    int iok         = 0;
    int in          = 0;
    for (i = 0; i < g2t_ssd_hit->GetNRows() ; i++)    {
      currWafId=g2t[i].volume_id%10000;
      if (currWafId > minWaf)   {
	currLadder  = mySst->idWaferToLadderNumb(currWafId);
	currWafNumb = mySst->idWaferToWafer(currWafId);

	StThreeVectorF error(0.,0.,0.);

	Double_t xl[3] = {g2t[i].x[0],g2t[i].x[1],g2t[i].x[2]};
	LOG_DEBUG <<Form("local position x=%f y=%f z=%f",xl[0],xl[1],xl[2])<<endm;

	Double_t xlSmear[3]={0.,0.,0.};
        xlSmear[0] = (distortHit(xl[0], mResXSst));
	xlSmear[2] = (distortHit(xl[2], mResZSst));
	xlSmear[1] = xl[1];
	LOG_DEBUG <<Form("smeared local position x=%f y=%f z=%f",xlSmear[0],xlSmear[1],xlSmear[2])<<endm;

	Double_t xgSmear[3]     = {0.0};

	mySst->mLadders[currLadder]->mWafers[currWafNumb]->LocalToMaster(xlSmear,xgSmear);
	LOG_DEBUG <<Form("smeared global position x=%f y=%f z=%f",xgSmear[0],xgSmear[1],xgSmear[2])<<endm;

	in  = IsOnWafer(xlSmear[0],xlSmear[2]);
	if(in==0)continue;

	iok = RemoveTriangle(xlSmear[0],xlSmear[2]);
	if(iok==0)continue;

	StSstHit  *mSmearedhit = new StSstHit(xgSmear,error,currWafId,g2t[i].de,c);

	// encode the hardware position
	// 2^3  detector ID number (8) 
	// 2^4  4-12 num_wafer (0-319)
	int hw  =         
	8                                                                             
	+         16 * idWaferToWaferNumb(currWafId);                                        
	mSmearedhit->setHardwarePosition(hw);
	mSmearedhit->setLocalPosition(xlSmear[0],xlSmear[1],xlSmear[2]);
	mSmearedhit->setIdTruth(g2t[i].track_p,100);
	inContainer+=sstHitCol->addHit(mSmearedhit);
      }
    }
  }
  if(inContainer){
    LOG_INFO<<"####   -> "<<inContainer<<" HITS WRITTEN INTO CONTAINER   ####"<<endm;
  }
  else {
    LOG_INFO<<"######### NO SST HITS WRITTEN INTO CONTAINER  ####"<<endm;
  }

  mySst->Reset();
  return kStOK;
}

Bool_t StSstFastSimMaker::accept(StEvent* event){
  return event ? true : false;
}

Float_t StSstFastSimMaker::distortHit(float x, double res){
  double test;
  test = x + mRandom->gauss(0,res);
  return test;
}

Int_t StSstFastSimMaker::idWaferToWaferNumb(int idWafer)
{
  int iW = (int)((idWafer - mSstLayer*1000)/100);
  int iL = idWafer - mSstLayer*1000 - iW*100;
  return ((iL-1)*mNWaferPerLadder + iW -1);
}

Int_t StSstFastSimMaker::idWaferToLadderNumb(int idWafer)
{
  int iW = (int)((idWafer - mSstLayer*1000)/100);
  int iL = idWafer - mSstLayer*1000 - iW*100;
  return iL-1;
}

Int_t StSstFastSimMaker::waferNumbToIdWafer(int waferNumb)
{
  int iL = 1+(int)((waferNumb)/mNLadder);
  int iW = waferNumb-((iL-1)*mNLadder)+1;
  return mSstLayer*1000 + iW*100 + iL;
}

void StSstFastSimMaker::setSstParameters(sstDimensions_st *geom_par){
  mDimensions          = geom_par;
  mSstLayer            = 7; // all layers : 1->7
  mDetectorLargeEdge   = 2.*geom_par[0].waferHalfActLength;
  mDetectorSmallEdge   = 2.*geom_par[0].waferHalfActWidth;
  mNLadder             = 20;
  mNWaferPerLadder     = geom_par[0].wafersPerLadder;
  mNStripPerSide       = geom_par[0].stripPerSide;
  mStripPitch          = geom_par[0].stripPitch;
  mTheta               = geom_par[0].stereoAngle;
}

void StSstFastSimMaker::Clear(Option_t *option)
{
  /*noop*/
}

Int_t StSstFastSimMaker::RemoveTriangle(float x, float y)
{
  int iok    = 0; 
  float beta = (TMath::Pi()/2.) - (mTheta/2.);
  float X    = (mDetectorSmallEdge/2.)*TMath::Tan(mTheta/2.);
  float l    = (mDetectorLargeEdge/2.) -X ;
  if(x>(mDetectorLargeEdge/2.) -X){
    if(y < TMath::ATan(beta)*(x-l)){
      LOG_DEBUG<<"upper right corner , hit rejected at x=" << x <<" y=" <<y << endm;
      iok =0;
    }
    else 
      if(y>-1*TMath::ATan(beta)*(x-l)){
	LOG_DEBUG<<"bottom right corner , hit rejected at x=" << x <<" y=" <<y << endm;
	  iok = 0; //bottom right corner
      }
  }
  if(x<((-1*mDetectorLargeEdge/2.)+X)){
    if(y<-1*TMath::ATan(beta)*(x+l)){
      LOG_DEBUG<<"upper  left corner , hit rejected  at x=" << x <<" y=" <<y << endm;
      iok =0; //upper left corner
    }
    else {
      if(y>1*TMath::ATan(beta)*(x+l)){
	LOG_DEBUG<<"bottom left corner , hit rejected  at x=" << x <<" y=" <<y << endm;
	iok = 0; //bottom left corner
      }
    }
  }  
  else{ iok= 1;}
return iok; // good hit
}

Int_t StSstFastSimMaker::IsOnWafer(float x , float y){
  //Find out for a given z coord and Hardware pos is it on the wafer
  if((x <(mDetectorLargeEdge/2.)) && (x > (-mDetectorLargeEdge/2.)) &&  
     (y <(mDetectorSmallEdge/2.)) && (y > (-mDetectorSmallEdge/2.)))
    return 1;   
  return 0;
}
