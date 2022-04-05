/*
 *
 * Author: J. Bouchet, Subatech; Y. Fisyak, BNL; J. Vanfossen, Kent State
 *
 * 
 **********************************************************
 *
 */

#include "Stiostream.h"
#include "StSsdFastSimMaker.h"
#include "StHit.h"
#include "StEventTypes.h"
#include "StChain.h"

#include "Sti/StiVMCToolKit.h"
#include "StarClassLibrary/StRandom.hh"
#include "tables/St_HitError_Table.h"

#include "TH1.h"
#include "TH2.h"
#include <math.h>

#include "tables/St_g2t_ssd_hit_Table.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StSsdDbMaker/StSstDbMaker.h"
#include "StSsdUtil/StSsdBarrel.hh"
#include "StSsdHitCollection.h"
#include "StEvent.h"
#include "StMcEvent.hh"
ClassImp(StSsdFastSimMaker)
  
  StSsdFastSimMaker::StSsdFastSimMaker(const char *name):StMaker(name){
    mHit        = new StSsdHit();
    WaferNumb   = 0;
    HitsMap     = 0;
    dX          = 0;
    dY          = 0;
    dZ          = 0;
    Local       = 0;
    Ratio       = 0;
  };

  StSsdFastSimMaker::~StSsdFastSimMaker(){ 
    delete mHit;
    delete WaferNumb   ; 
    delete HitsMap     ; 
    delete dX          ; 
    delete dY          ; 
    delete dZ          ; 
    delete Local       ; 
    delete Ratio       ;
}

int StSsdFastSimMaker::Init()
{
  int seed=time(NULL);
  myRandom=new StRandom();
  myRandom->setSeed(seed);

  // Define various SSD specific geom. variables.
  mResXSsd = 0.0000;				
  mResZSsd = 0.0000;
  mSmear=1;	//used to turn on or off smearing
  
  LOG_DEBUG << "Init() : Defining the histograms" << endm;
  if(IAttr(".histos")){
    WaferNumb = new TH1S("WaferNumb","Hits from geant per Wafer Id",1620,7000,8620);
    WaferNumb->SetXTitle("from 7000 to 7000*(iW*100)+16");
    WaferNumb->SetLabelSize(0.03,"X");
    WaferNumb->SetLabelSize(0.03,"Y");
    HitsMap = new TH2S("HitsMap","Map of hits in ssd wafers",20,1,21,16,1,17);
    HitsMap->SetXTitle("Ladder");
    HitsMap->SetYTitle("Wafer");
    HitsMap->SetLabelSize(0.03,"X");
    HitsMap->SetLabelSize(0.03,"Y");
    dX = new TH1F("dX","difference in X after smearing the hit",200,-0.02,0.02);
    dX->SetXTitle("x_{G} - x_{G}^{smear} [cm]");
    dX->SetLabelSize(0.03,"X");
    dX->SetLabelSize(0.03,"Y");
    dY = new TH1F("dY","difference in Y after smearing the hit",200,-0.02,0.02);
    dY->SetXTitle("y_{G} - y_{G}^{smear} [cm]");
    dY->SetLabelSize(0.03,"X");
    dY->SetLabelSize(0.03,"Y");
    dZ = new TH1F("dZ","difference in Z after smearing the hit",800,-2,2);
    dZ->SetXTitle("z_{G} - z_{G}^{smear} [cm]");
    dZ->SetLabelSize(0.03,"X");
    dZ->SetLabelSize(0.03,"Y");
    Local = new TH2F("Local","Local x vs local y : hits ",800,-4,4,250,-2.25,2.25);
    Local->SetXTitle("x_{l}[cm]");
    Local->SetYTitle("y_{l}[cm]");
    Local->SetLabelSize(0.03,"X");
    Local->SetLabelSize(0.03,"Y");
    Ratio  = new TH1F("Ratio","Ratio between initial geant hits and after removal of the dead areas",120,0,1.2);
    Ratio->SetLabelSize(0.03,"X");
    Ratio->SetLabelSize(0.03,"Y");
  }
  return kStOk;
}
//____________________________________________________________
int StSsdFastSimMaker::InitRun(int RunNo)
{
  // Define various SSD hit errors from database
  TDataSet *set = GetDataBase("Calibrations/tracker");
  St_HitError *tableSet = (St_HitError *)set->Find("ssdHitError");
  HitError_st* hitError = tableSet->GetTable();
  mResXSsd = sqrt(hitError->coeff[0]);
  mResZSsd = sqrt(hitError->coeff[3]);
  LOG_DEBUG << "Smearing SSD hits by " << mResXSsd << " " << mResZSsd << " cm in the SSD " << endm;
  
  // 		geometry parameters
  TDataSet *ssdparams = GetInputDB("Geometry/ssd");
  if (! ssdparams) {
    LOG_ERROR << "No  access to Geometry/ssd parameters" << endm;
    return kStFatal;
  }
  TDataSetIter    local(ssdparams);
  m_ctrl        = (St_slsCtrl           *)local("slsCtrl");
  m_dimensions  = (St_ssdDimensions     *)local("ssdDimensions"); 
  m_positions   = (St_ssdWafersPosition *)local("ssdWafersPosition");
  
  if (!m_ctrl) {
    LOG_ERROR << "No  access to control parameters" << endm;
    return kStFatal;
  }   
  if ((!m_dimensions)||(!m_positions)) {
    LOG_ERROR << "No  access to geometry parameters" << endm;
    return kStFatal;
  }  
  St_ssdConfiguration* configTable = (St_ssdConfiguration*) local("ssdConfiguration");
  if (!configTable) {
    LOG_ERROR << "InitRun : No access to ssdConfiguration database" << endm;
    return kStFatal;
  }
  m_config = (ssdConfiguration_st*) configTable->GetTable() ;
  
  return kStOk;
}
//____________________________________________________________
Int_t StSsdFastSimMaker::Finish(){return kStOk;}
//____________________________________________________________
Int_t StSsdFastSimMaker::Make()
{
  // Get the input data structures from StEvent
  mEvent =  (StEvent*) GetInputDS("StEvent");
  if(mEvent)
    {
      mCol = mEvent->ssdHitCollection();
      if (!mCol)  
	{ 
	  LOG_WARN <<"StSsdFastSimulatorMaker -E- no SsdHitCollection!" << endm; 
	  mCol = new StSsdHitCollection; 
	  mEvent->setSsdHitCollection(mCol); 
	  LOG_WARN <<"Make() has added a non existing StSsdHitCollection" <<endm; 
	}   
    }
  if (! mEvent) {
    LOG_WARN << "No StEvent on input, bye bye" << endm; return kStWarn;
    mCol =0;
  }
  TDataSetIter geant(GetInputDS("geant"));
  if (! gGeoManager) GetDataBase("VmcGeometry");
  LOG_DEBUG << "Geometry Loaded" << endm;
  St_g2t_ssd_hit *g2t_ssd_hit = (St_g2t_ssd_hit *) geant("g2t_ssd_hit");
  if (! g2t_ssd_hit) {
    LOG_WARN << "No g2t_ssd_hit on input, bye bye" << endm; return kStWarn;
    return kStWarn;
  }
  g2t_ssd_hit_st *g2t         = g2t_ssd_hit->GetTable();

  LOG_INFO<<"####      START OF SSD FAST SIM MAKER        ####"<<endm;
  mySsd = StSsdBarrel::Instance();
  ssdDimensions_st *dimensions = m_dimensions->GetTable();
  setSsdParameters(dimensions);
  if(Debug()>1) printSsdParameters();
  Int_t inContainer = 0;
  Int_t goodHits    = 0;
  if(g2t_ssd_hit){
    LOG_INFO<<Form("NumberOfRows = %d Size of g2t_ssd table=%d",(int)g2t_ssd_hit->GetNRows(),(int)g2t_ssd_hit->GetTableSize())<<endm;
    Int_t minWaf      = mSsdLayer*1000;
    Int_t currWafId   = 0;
    Int_t currWafNumb = 0;
    Int_t currLadder  = 0;
    Int_t i           = 0;
    unsigned char c   = 0;
    Int_t iok         = 0;
    Int_t in          = 0;
    for (i = 0; i < g2t_ssd_hit->GetNRows() ; i++)    {
      currWafId=g2t[i].volume_id%10000;
      if (currWafId > minWaf)   {
	currLadder  = StSsdBarrel::Instance()->idWaferToLadderNumb(currWafId);
	currWafNumb = StSsdBarrel::Instance()->idWaferToWafer(currWafId);
	StThreeVectorF error(0.,0.,0.);
	mHit = new StSsdHit(g2t[i].x,error,currWafId,g2t[i].de,c);
	LOG_DEBUG<<Form("currWafId=%d currWafNum=%d currLadder=%d",currWafId,currWafNumb,currLadder)<<endm;
	// now we get the position of this wafer  
	// GEANT hits are saved in local coordinate  
	Double_t xl[3] = {mHit->position().x(),mHit->position().y(),mHit->position().z()};
	LOG_DEBUG <<Form("local position x=%f y=%f z=%f",xl[0],xl[1],xl[2])<<endm;
	LOG_DEBUG <<"will smear X"<<endm;
	Double_t xlSmear[3]={0.,0.,0.};
        xlSmear[0] = (distortHit(xl[0], mResXSsd));
	LOG_DEBUG <<"will smear Z"<<endm;
	xlSmear[2] = (distortHit(xl[2], mResZSsd));
	xlSmear[1] = xl[1];
	LOG_DEBUG <<Form("smeared local position x=%f y=%f z=%f",xlSmear[0],xlSmear[1],xlSmear[2])<<endm;
	Double_t xgSmear[3]={0.,0.,0.};
	mySsd->mLadders[currLadder]->mWafers[currWafNumb]->LocalToMaster(xlSmear,xgSmear);
	LOG_DEBUG<< "After : global position are :"<<endm;
	LOG_DEBUG <<Form("x=%f y=%f z=%f",xgSmear[0],xgSmear[1],xgSmear[2])<<endm;
	LOG_DEBUG <<Form("Smearing done...")<< endm;
	in  = IsOnWafer(xlSmear[0],xlSmear[2]);
	if(in==0)continue;
	LOG_DEBUG << "good hit in wafer active area " << endm;
	iok = RemoveTriangle(xlSmear[0],xlSmear[2]);
	if(iok==0)continue;
	goodHits++;
	LOG_DEBUG << "good hit after triangle rejection" << endm;
	Local->Fill(xlSmear[0],xlSmear[2]);
	StSsdHit  *mSmearedhit = new StSsdHit(xgSmear,error,currWafId,g2t[i].de,c);
	//fill histograms
	if(IAttr(".histos")){ 
	  HitsMap->Fill(currLadder+1,currWafNumb+1,1);
	  WaferNumb->Fill(currWafId);
	  dX->Fill(mHit->position().x()-xgSmear[0]);
	  dY->Fill(mHit->position().y()-xgSmear[1]);
	  dZ->Fill(mHit->position().z()-xgSmear[2]);
	}
	  // encode the hardware position
	// 2^3  detector ID number (8) 
	// 2^4  4-12 num_wafer (0-319)
	Int_t hw  =         
	8                                                                             
	+         16 * idWaferToWaferNumb(currWafId);                                        
	mSmearedhit->setHardwarePosition(hw);
	mSmearedhit->setLocalPosition(xlSmear[0],xlSmear[2]);
	mSmearedhit->setIdTruth(g2t[i].track_p,100);
	inContainer+=mCol->addHit(mSmearedhit);
      }
    }
  }
  if(inContainer){
    LOG_INFO<<"####   -> "<<inContainer<<" HITS WRITTEN INTO CONTAINER   ####"<<endm;
    Ratio->Fill(inContainer/(float)g2t_ssd_hit->GetNRows());
  }
  else {
    LOG_INFO<<"######### NO SSD HITS WRITTEN INTO CONTAINER  ####"<<endm;
  }
  LOG_DEBUG << "ssd col= "<<mCol->numberOfHits()<< " inContainer=" << inContainer <<" good hits = " << goodHits <<" initial # hits="<<g2t_ssd_hit->GetNRows()<<endm;
  mySsd->Reset();
  return kStOK;
}
//________________________________________________________________________________
Bool_t StSsdFastSimMaker::accept(StEvent* event){
  return event ? true : false;
}
//_______________________________________________________________________________
Float_t StSsdFastSimMaker::distortHit(Float_t x, double res){
  double test;
  LOG_DEBUG << "In distortHit " << endm;
  LOG_DEBUG << Form("smear me by %f",res)<<endm;
  if(mSmear){
    test = x + myRandom->gauss(0,res);
    
    LOG_DEBUG<< " x was " <<x<< " and is now " << test << endm;
    return test;
  }
  else return x;
}
//_______________________________________________________________
Int_t StSsdFastSimMaker::idWaferToWaferNumb(Int_t idWafer)
{
  // idwafer = layer*1000+waf*100+ladder
  Int_t iW = (int)((idWafer - mSsdLayer*1000)/100);
  Int_t iL = idWafer - mSsdLayer*1000 - iW*100;
  return ((iL-1)*mNWaferPerLadder + iW -1);
}
//_______________________________________________________________
Int_t StSsdFastSimMaker::idWaferToLadderNumb(Int_t idWafer)
{
  // idwafer = layer*1000+waf*100+ladder
  Int_t iW = (int)((idWafer - mSsdLayer*1000)/100);
  Int_t iL = idWafer - mSsdLayer*1000 - iW*100;
  return iL-1;
}
//_______________________________________________________________
Int_t StSsdFastSimMaker::waferNumbToIdWafer(Int_t waferNumb)
{
  Int_t iL = 1+(int)((waferNumb)/mNLadder);
  Int_t iW = waferNumb-((iL-1)*mNLadder)+1;
  return mSsdLayer*1000 + iW*100 + iL;
}
//________________________________________________________________________________
void StSsdFastSimMaker::setSsdParameters(ssdDimensions_st *geom_par){
  mDimensions          = geom_par;
  mSsdLayer            = 7; // all layers : 1->7
  mDetectorLargeEdge   = 2.*geom_par[0].waferHalfActLength;
  mDetectorSmallEdge   = 2.*geom_par[0].waferHalfActWidth;
  mNLadder             = 20;
  mNWaferPerLadder     = geom_par[0].wafersPerLadder;
  mNStripPerSide       = geom_par[0].stripPerSide;
  mStripPitch          = geom_par[0].stripPitch;
  mTheta               = geom_par[0].stereoAngle;
}
//________________________________________________________________________________
void StSsdFastSimMaker::printSsdParameters(){
  LOG_INFO << "###Ladders = " <<mNLadder<<"###"<< endm;
  LOG_INFO << "###Wafers per Ladder = " <<mNWaferPerLadder<<"###"<< endm;
}
void StSsdFastSimMaker::Clear(Option_t *option)
{
  /*noop*/
}
//________________________________________________________________________________
Int_t StSsdFastSimMaker::RemoveTriangle(Float_t x, Float_t y)
{
  Int_t iok = 0; 
  Float_t beta = (TMath::Pi()/2.) - (mTheta/2.);
  Float_t X = (mDetectorSmallEdge/2.)*TMath::Tan(mTheta/2.);
  Float_t l = (mDetectorLargeEdge/2.) -X ;
  LOG_DEBUG <<Form("beta=%f arctan(beta)=%f X=%f l=%f mdetectorsmallEdge=%f mDetectorLargeEdge=%f\n",beta,TMath::ATan(beta),X,l,mDetectorSmallEdge,mDetectorLargeEdge)<<endm;
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
//_______________________________________________________________________________
int StSsdFastSimMaker::IsOnWafer(Float_t x , Float_t y){
  //Find out for a given z coord and Hardware pos is it on the wafer
  if((x <(mDetectorLargeEdge/2.)) && (x > (-mDetectorLargeEdge/2.)) &&  
     (y <(mDetectorSmallEdge/2.)) && (y > (-mDetectorSmallEdge/2.)))
    return 1;   
  return 0;
}
