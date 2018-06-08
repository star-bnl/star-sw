/***************************************************************************
 *
 * $Id: StSvtHitMaker.cxx,v 1.44 2018/06/08 16:38:23 genevb Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Cluster Hit Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtHitMaker.cxx,v $
 * Revision 1.44  2018/06/08 16:38:23  genevb
 * Needs explicit include of TROOT.h
 *
 * Revision 1.43  2009/11/23 16:44:55  fisyak
 * Remove references to tables
 *
 * Revision 1.42  2007/12/17 14:15:15  fisyak
 * Add reject for hits for invalid drift regions
 *
 * Revision 1.41  2007/09/21 00:08:17  caines
 * Save out number of pixels and hits into StEvent hits
 *
 * Revision 1.40  2007/07/12 20:06:50  fisyak
 * Move initialization to IntRun from Init, empty GetSvtDriftCurve, clean up
 *
 * Revision 1.39  2007/04/28 17:57:06  perev
 * Redundant StChain.h removed
 *
 * Revision 1.38  2007/03/21 17:22:58  fisyak
 * Ivan Kotov's drift velocities, use TGeoHMatrix for coordinate transformation
 *
 * Revision 1.37  2006/05/08 13:52:11  fisyak
 * Fill StSvtHits directly into StEvent (if it exists), add local coordinate, add handle for drift velocity hack correction
 *
 * Revision 1.36  2005/07/23 03:37:33  perev
 * IdTruth + Cleanup
 *
 * Revision 1.35  2004/11/03 21:36:04  caines
 * Remove calls that need database and event time information to correct place in InitRun not Init
 *
 * Revision 1.34  2004/07/29 01:36:59  caines
 * Changes for using the drift curves
 *
 * Revision 1.33  2004/07/07 18:09:24  caines
 * Save out fraction drfi5 velocity scaler to StEvent
 *
 * Revision 1.32  2004/06/21 20:18:34  caines
 * Add number of anodes count to res[0] variable for use in dedx calc
 *
 * Revision 1.31  2004/06/14 21:27:46  caines
 * Fine tuning of drift velocity using laser spots from Jana Bielcikova
 *
 * Revision 1.30  2004/05/05 21:27:54  caines
 * Fix bug that causes default drift vel. to be used instead of value from database...
 *
 * Revision 1.29  2004/03/18 04:02:56  caines
 * Remove from global scope variables used in debug mode as they shouldnt be there and caused erratic behaviour
 *
 * Revision 1.28  2004/01/27 02:34:11  perev
 * LeakOff
 *
 * Revision 1.27  2003/09/18 18:16:38  caines
 * Initialise stuff for new compiler
 *
 * Revision 1.26  2003/09/02 17:59:07  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.25  2003/04/14 18:33:45  munhoz
 * reading t0 from DB
 *
 * Revision 1.24  2003/01/28 20:28:51  munhoz
 * new filters for clusters
 *
 * Revision 1.22  2002/02/22 18:43:39  caines
 * Add SetFileNames function
 *
 * Revision 1.21  2002/02/16 22:05:06  jeromel
 * Marcelo's recen changes to StSvtClusterMaker (needed to be in sync with
 * StDbUtilities changes)
 *
 * Revision 1.20  2002/02/03 00:34:50  caines
 * Still fill scs_spt table even if database missing so global doesnt crash
 *
 * Revision 1.19  2002/01/31 23:53:25  caines
 * Return kStOK even if init fails so chain doesnt crash
 *
 * Revision 1.18  2002/01/31 22:56:39  caines
 * Make code cope with database failing, now just sends error messages and quits but doesnt make chain end
 *
 * Revision 1.17  2002/01/28 23:42:10  caines
 * Move to SVT database with StSvtDbMaker
 *
 * Revision 1.16  2001/09/22 01:07:09  caines
 * Fixes now that AddData() is cleared everyevent
 *
 * Revision 1.15  2001/08/07 20:52:15  caines
 * Implement better packing of svt hardware and charge values
 *
 * Revision 1.14  2001/07/24 23:43:08  caines
 * Better flagging of hits out of drift range
 *
 * Revision 1.13  2001/05/09 02:23:06  caines
 *  Fix axis limits
 *
 * Revision 1.12  2001/05/04 14:20:05  caines
 * Improved historgramming
 *
 * Revision 1.11  2001/03/22 20:46:54  caines
 * Comment out some of the QA histograms
 *
 * Revision 1.10  2001/02/19 23:55:34  caines
 * Even get id correct (start from 1 not 0)
 *
 * Revision 1.9  2001/02/19 23:37:59  caines
 * Ensure hits have a unique id for ALL svt, remover print statement
 *
 * Revision 1.8  2001/02/18 00:10:48  caines
 * Improve and use StSvtConfig
 *
 * Revision 1.7  2001/02/07 19:15:54  caines
 * Change evaluation flag when running with GEANT
 *
 * Revision 1.6  2000/11/30 20:42:08  caines
 * Some more evaluation and use dataase
 *
 * Revision 1.5  2000/10/02 13:47:03  caines
 * Fixed some array bound problems. Better flagging of hits
 *
 * Revision 1.4  2000/08/29 22:46:26  caines
 * Fixed some memory leaks
 *
 * Revision 1.3  2000/08/26 20:36:28  caines
 * Adjustments for StSvtCoordinateTransform calls
 *
 * Revision 1.2  2000/08/24 04:26:56  caines
 * Printout for debugging
 *
 * Revision 1.1  2000/08/21 13:03:40  caines
 * First version of cluster->STAR hit maker
 *
 **************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "Stiostream.h"

#include "StSvtHitMaker.h"

#include "TDataSetIter.h"
#include "TObjectSet.h"
#include "StMessMgr.h"
#include "StMCTruth.h"

#include "TROOT.h"
#include "TFile.h"
#include "TNtuple.h"

#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StDbUtilities/StCoordinates.hh"
//#include "StTpcDb/StTpcDb.h"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtWaferGeometry.hh"
#include "StSvtClassLibrary/StSvtT0.hh"
#include "StSvtAnalysedHybridClusters.hh"
#include "StSvtSimulationMaker/StSvtGeantHits.hh"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StDbUtilities/St_svtCorrectionC.h"
#include "StDbUtilities/St_svtHybridDriftVelocityC.h"

ClassImp(StSvtHitMaker)
//___________________________________________________________________________
StSvtHitMaker::StSvtHitMaker(const char *name) : StMaker(name)
{
  int n = (char*)&filenameC - (char*)&iWrite + sizeof(filenameC);
  memset(&iWrite,0,n);
}

//_____________________________________________________________________________
StSvtHitMaker::~StSvtHitMaker()
{
  delete m_x_vs_y;  //! x vs y of Si points
  for (int i=0;i<mNwaf_no;i++) {delete m_waf_no[i];}
  delete [] m_waf_no; m_waf_no=0;

  delete m_ClusTuple;                       //!
  delete m_hfile;                           //!

  // delete Evaluation histos
  delete mTimeHitResolution;  //!
  delete mAnodeHitResolution;  //!
  delete mXHitResolution;  //!
  delete mYHitResolution;  //!
  delete mZHitResolution;  //!
  delete mHitResolution;   //!

}

//_____________________________________________________________________________
Int_t StSvtHitMaker::Init()
{

  if (Debug()) gMessMgr->Debug() << "In StSvtHitMaker::Init() ..."  << endm;

  return  StMaker::Init();
    
}

//_____________________________________________________________________________
Int_t StSvtHitMaker::InitRun(int runumber)
{

  if (Debug()) gMessMgr->Debug() << "In StSvtHitMaker::InitRun() ..."  << endm;
  // Get pointer to StSvtAnalysis

  if( GetSvtRawData()){
    gMessMgr->Warning() << " No SVT raw data "<< endm;
    return kStWarn;
  }

  // get geant hits if running MC data
  mSvtGeantHitColl = 0;
   St_DataSet *dataSet2 = GetDataSet("StSvtGeantHits");
  if (dataSet2)
    mSvtGeantHitColl = (StSvtHybridCollection*)(dataSet2->GetObject());

  if( GetSvtGeometry() != kStOK) return kStWarn;

  // drift velocity
  if( GetSvtDriftVelocity() != kStOK) return kStWarn;

  // drift curves from Robert
  if( GetSvtDriftCurve() != kStOK) return kStWarn;

  // t0
  if( GetSvtT0() != kStOK) return kStWarn;

  //  iWrite=1;
    

  // 		Create tables
  //  St_DataSetIter       local(GetInputDB("svt"));


 // 		geometry parameters

  //if( GetSvtGeometry() != kStOK) return kStWarn;

  // drift velocity
  //  if( GetSvtDriftVelocity() != kStOK) return kStWarn;

  // drift curves from Robert
  //if( GetSvtDriftCurve() != kStOK) return kStWarn;

  m_x_vs_y = new TH2F("si_x_vs_y","X vs Y of Si space points",
		      300,-30,30,300,-30,30);

  m_x_vs_y->SetYTitle("y cm");
  m_x_vs_y->SetXTitle("x cm");

  mNwaf_no = 2*mSvtData->getNumberOfBarrels();
  m_waf_no = new TH2F*[mNwaf_no];
  
  char title1[20];
  char* title3;
  char  title2[4];

  int barrel=0;

  for( int idbarrel=1; idbarrel<=mSvtData->getNumberOfBarrels(); 
       idbarrel++, barrel++){


    float n_ladders = float(mSvtData->getNumberOfLadders(idbarrel))+0.5;

    sprintf(title1,"SvtLayer");
    sprintf(title2,"%d", barrel+1);
    title3 = strcat(title1,title2);

  
    m_waf_no[barrel] = new TH2F(title3," Si z vs ladder no.",
			 100,-25.,25.,100,0.5,n_ladders);
    m_waf_no[barrel]->SetYTitle("ladder no");
    m_waf_no[barrel]->SetXTitle("Z cm");
    barrel++;

    sprintf(title1,"SvtLayer");
    sprintf(title2,"%d", barrel+1);
    title3 = strcat(title1,title2);
    m_waf_no[barrel] = new TH2F(title3," Si z vs ladder no.",
				100,-25.,25.,100,0.5,n_ladders);
    m_waf_no[barrel]->SetYTitle("ladder no");
    m_waf_no[barrel]->SetXTitle("Z cm");
  }

  if( iWrite >0){
    m_hfile = new TFile(filenameN,"RECREATE","Clusters");
    
    m_ClusTuple = new TNtuple("Clusters","Clusters","flag:xl:yl:x:y:z:charge:mom2x:mom2y:numAnodes:numPixels:peak:hybrid:evt");
    
    // if( iWrite > 1){
//       //cluInfo.open(filenameC,ios::out);    
//     }
  }

  if (mSvtGeantHitColl){
    
    mTimeHitResolution = new TH1F("hitTRes","Delta X for Hits",1000,-2.0,2.0);
    mAnodeHitResolution = new TH1F("hitARes","Delta Z for Hits",1000,-2.0,2.0);
    mXHitResolution = new TH1F("hitXRes","Delta X for Hits",1000,-0.1,0.1);
    mYHitResolution = new TH1F("hitYRes","Delta Y for Hits",1000,-0.1,0.1);
    mZHitResolution = new TH1F("hitZRes","Delta Z for Hits",1000,-0.1,0.1);		      
    mHitResolution = new TH2F("hitRes","Delta Z Vs Delta X for Hits",1000,-2.0,2.0,1000,-2.0,2.0);
    
    mTimeHitResolution->SetXTitle("delta X (timebucket)");
    mAnodeHitResolution->SetXTitle("delta Z (anode)");
    mXHitResolution->SetXTitle("delta X (cm)");
    mYHitResolution->SetXTitle("delta Y (cm)");
    mZHitResolution->SetXTitle("delta Z (cm)");
    mHitResolution->SetXTitle("delta X (timebucket)");
    mHitResolution->SetYTitle("delta Z (anode)");
  }

  // 		Add the Maker histograms in the Maker histograms list
  // 		and remove it from the ROOT system directory
  TObject  *objLast,*objHist;
  gROOT->cd();
  objLast = gDirectory->GetList()->Last();
  TIter nextHist(gDirectory->GetList());
  int ready = !objLast;
  while((objHist=nextHist())) {// loop over gDirectory
    if (!ready && objHist!=objLast)		continue;
    ready = 1999;
    if (objHist==objLast)			continue;
    if (!objHist->InheritsFrom("TH1")) 	continue;
    
    // 		Move the histogram from the ROOT list into the "maker's" list
    ((TH1*)objHist)->SetDirectory(0);
    AddHist((TH1*)objHist);
  }
  return kStOK;
 
}


//___________________________________________________________________________

Int_t StSvtHitMaker::GetSvtRawData()
{

  mSvtData = 0;
  St_DataSet *dataSet2 = GetDataSet("StSvtData");
  if( dataSet2) mSvtData = (StSvtData*)(dataSet2->GetObject());

  if( !mSvtData) return kStWarn;
  return kStOk;
}
//___________________________________________________________________________

Int_t StSvtHitMaker::GetSvtClusterData()
{

  mSvtCluColl = 0;
  St_DataSet *dataSet2  = GetDataSet("StSvtAnalResults");
  if( !dataSet2) return kStWarn;
   mSvtCluColl = (StSvtHybridCollection*)(dataSet2->GetObject());
 
  if( !mSvtCluColl) return kStWarn;
  return kStOK;
 
}
//___________________________________________________________________________
Int_t StSvtHitMaker::GetSvtGeometry()
{
  m_geom = 0;
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtGeometry");
  if(!dataSet) {
    gMessMgr->Error("Failure to get SVT geometry - THINGS HAVE GONE SERIOUSLY WRONG!!!!! \n");
    
    return kStOK;
  }

  m_geom = (StSvtGeometry*)dataSet->GetObject();
  
  return kStOK;

}
//___________________________________________________________________________
Int_t StSvtHitMaker::GetSvtDriftVelocity()
{
  m_driftVeloc = 0;
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtDriftVelocity");
  if(!dataSet) {
    gMessMgr->Error("Failure to get SVT drift velocity  - THINGS HAVE GONE SERIOUSLY WRONG!!!!! \n");
    
    return kStOK;
  }

  m_driftVeloc = (StSvtHybridCollection*)dataSet->GetObject();
  
  return kStOK;

}
//___________________________________________________________________________
Int_t StSvtHitMaker::GetSvtDriftCurve()
{
  m_driftCurve = 0;
  return kStOK;

}
//___________________________________________________________________________
Int_t StSvtHitMaker::GetSvtT0()
{
 
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtT0");
  if(!dataSet) {
    gMessMgr->Error("Failure to get SVT t0  - THINGS HAVE GONE SERIOUSLY WRONG!!!!! \n");
    
    return kStOK;
  }

  m_t0 = (StSvtT0*)dataSet->GetObject();
  
  return kStOK;

}
//___________________________________________________________________________
Int_t StSvtHitMaker::Make()
{
  if (Debug()) gMessMgr->Debug() << "In StSvtHitMaker::Make() ..."  << endm;
// Get pointer to StSvtData

  if( GetSvtRawData()){
    gMessMgr->Warning() <<" No SVT raw data" << endm;
    return kStWarn;
  }

  if( GetSvtClusterData()){
    gMessMgr->Warning() <<" No SVT Cluster data" << endm;
    return kStWarn;
  }

  if( !m_geom){
     gMessMgr->Warning() <<" Things are wrong with the SVT database!!!!!!!!!" << endm;
  }

  TransformIntoSpacePoint();
  FillHistograms();

//   if (mSvtGeantHitColl)
//     Eval();

  return kStOK;
}

//____________________________________________________________________________

void StSvtHitMaker::TransformIntoSpacePoint(){

  int index, TotHits=0, GoodHit=0;
  
  
  StSvtCoordinateTransform SvtGeomTrans;
  //SvtGeomTrans.setParamPointers(&srs_par[0], &geom[0], &shape[0], mSvtData->getSvtConfig());
  //   if(m_geom)  SvtGeomTrans.setParamPointers(m_geom, mSvtData->getSvtConfig(), m_driftVeloc, m_t0);
  St_svtCorrectionC *driftVelocityCor = 0;
  if(m_geom)  SvtGeomTrans.setParamPointers(m_geom, mSvtData->getSvtConfig(), m_driftVeloc, m_driftCurve, m_t0,driftVelocityCor );
  StSvtLocalCoordinate localCoord(0,0,0);
  StSvtWaferCoordinate waferCoord(0,0,0,0,0,0);
  StGlobalCoordinate globalCoord(0,0,0); 
  StThreeVectorF mPos(0,0,0);
  StEvent *pEvent = (StEvent*) GetInputDS("StEvent");

  //here is applied laser correction for temperature variations;
  St_svtHybridDriftVelocityC *driftVel = St_svtHybridDriftVelocityC::instance();
  assert(driftVel);
  Int_t index2 = -1;
  for(int barrel = 1;barrel <= mSvtData->getNumberOfBarrels();barrel++) {

    for (int ladder = 1;ladder <= mSvtData->getNumberOfLadders(barrel);ladder++) {

      for (int wafer = 1;wafer <= mSvtData->getNumberOfWafers(barrel);wafer++) {

	for (int hybrid = 1;hybrid <=mSvtData->getNumberOfHybrids();hybrid++){
	  index2++;
	  
	  index = mSvtData->getHybridIndex(barrel,ladder,wafer,hybrid);
	  if(index < 0) continue;
	  if (! driftVel->p(barrel,ladder,wafer,hybrid)) continue;
	  mSvtBigHit = (StSvtAnalysedHybridClusters*)mSvtCluColl->at(index);
	  if( !mSvtBigHit) continue;
	  
	  for( int clu=0; clu<mSvtBigHit->numOfHits(); clu++){
	    if (! driftVel->IsValidDriftRegion(barrel,ladder,wafer,hybrid, mSvtBigHit->WaferPosition()[clu].x())) continue;
	    TotHits++;
	    mSvtBigHit->svtHitData()[clu].id = TotHits;
	    waferCoord.setTimeBucket(mSvtBigHit->WaferPosition()[clu].x());
	    waferCoord.setAnode(mSvtBigHit->WaferPosition()[clu].y());
	    waferCoord.setLayer(mSvtBigHit->getLayerID());
	    waferCoord.setLadder(ladder);
	    waferCoord.setWafer(wafer);
	    waferCoord.setHybrid(hybrid);

	    if( m_geom) {
	      SvtGeomTrans(waferCoord,localCoord);
	      SvtGeomTrans(localCoord,globalCoord);
	    }
	    // 	    cout << " Timebucket=" << waferCoord.timebucket() << 
	    // 	      " x=" << localCoord.position().x() <<
	    // 	      " hybrid=" << localCoord.hybrid() <<
	    // 	      " flag =" <<mSvtBigHit->svtHit()[clu].flag() << endl; 

	    mPos.setX(globalCoord.position().x());
	    mPos.setY(globalCoord.position().y());
	    mPos.setZ(globalCoord.position().z());
	    mSvtBigHit->svtHit()[clu].setPosition(mPos);
	    mSvtBigHit->svtHitData()[clu].uv[0] = localCoord.position().x();
	    mSvtBigHit->svtHitData()[clu].uv[1] = localCoord.position().y();
	    mSvtBigHit->svtHitData()[clu].anode = waferCoord.anode();
	    mSvtBigHit->svtHitData()[clu].timeb = waferCoord.timebucket();
	    //	    cout << "local x = " << localCoord.position().x() <<  ", local y = " << localCoord.position().y() <<  ", local z = " << localCoord.position().z() << endl; 
	    //cout << "global x = " << globalCoord.position().x() <<  ", global y = " << globalCoord.position().y() <<  ", global z = " << globalCoord.position().z() << endl; 

	    if (pEvent) {
	     
	      Int_t hw_position, svtx, svty;
	      StSvtHit *svtHit = 0;
	      StSvtHitCollection *svtHitColl = pEvent->svtHitCollection();
	      if (! svtHitColl) {
		svtHitColl = new StSvtHitCollection;
		pEvent->setSvtHitCollection(svtHitColl);
	      }
	      svtHit = new StSvtHit();
	      StSvtHitData          *dat = mSvtBigHit->svtHitData()   +clu;
	      StThreeVector<double> *waf = mSvtBigHit->WaferPosition()+clu;
	      StSvtHit              *hit= mSvtBigHit->svtHit()        +clu;
	      
	      hw_position = 2;
	      hw_position += (1L<<4)*(index2);
	      svtx = int(waf->x()*4);
	      hw_position += (1L<<13)*(svtx);
	      svty = int(waf->y()*4);
	      hw_position += (1L<<22)*svty;
	      svtHit->setHardwarePosition(hw_position);
	      svtHit->setIdTruth(hit->idTruth(),hit->qaTruth());
	      
	      svtHit->setPeak(dat->peakAdc);
	      svtHit->setCharge(hit->charge());
	      svtHit->setFlag(hit->flag()); 
	      if (driftVelocityCor) {
		if (! driftVelocityCor->pCorrection(svtHit->layer(), svtHit->ladder(), svtHit->wafer(), svtHit->hybrid()))
		  svtHit->setFlag(hit->flag()+100); 
	      }
	      svtHit->setFitFlag(0);
	      svtHit->setPosition(mSvtBigHit->svtHit()[clu].position());
	      svtHit->setId(dat->id);
	      svtHit->setLocalPosition(dat->uv[0],dat->uv[1]);
	      svtHit->setAnode(dat->anode);
	      svtHit->setTimebucket(dat->timeb);
	      svtHit->setNumberOfAnodes(dat->numOfAnodesInClu);
	      svtHit->setNumberOfPixels(dat->numOfPixelsInClu);
	      
	      svtHitColl->addHit(svtHit);
	    }
	    
	    if(mSvtBigHit->svtHit()[clu].flag() < 4) GoodHit++; 
	    
	    if( iWrite > 0){
	      SaveIntoNtuple(mSvtBigHit->numOfHits(),index);
	    }
	  }
	}
      }
    }
    
  }

  SafeDelete(driftVelocityCor);
  gMessMgr->Info() << "Found " << GoodHit << " good hits " << endm;
}

//_____________________________________________________________________________
Int_t StSvtHitMaker::FillHistograms(){
  
  
  int index;

  for(int barrel = 1;barrel <= mSvtData->getNumberOfBarrels();barrel++) {
    
    for (int ladder = 1;ladder <= mSvtData->getNumberOfLadders(barrel);ladder++) {

      for (int wafer = 1;wafer <= mSvtData->getNumberOfWafers(barrel);wafer++) {
	
	for (int hybrid = 1;hybrid <=mSvtData->getNumberOfHybrids();hybrid++){
	  
	  
	  index = mSvtData->getHybridIndex(barrel,ladder,wafer,hybrid);
	  if(index < 0) continue;
	  
	  mSvtBigHit = (StSvtAnalysedHybridClusters*)mSvtCluColl->at(index);
	  if( !mSvtBigHit) continue;
	  
	  for( int clu=0; clu<mSvtBigHit->numOfHits(); clu++){
	    
	    if( mSvtBigHit->svtHit()[clu].flag() < 4){
	      int layer = mSvtBigHit->getLayerID()-1;
	      int ladder = (int)(mSvtBigHit->svtHit()[clu].ladder());
	   
	      m_x_vs_y->Fill(mSvtBigHit->svtHit()[clu].position().x(),mSvtBigHit->svtHit()[clu].position().y());
	      m_waf_no[layer]->Fill(mSvtBigHit->svtHit()[clu].position().z(),ladder,1);
	    }
	  }
	}
      }
    }
  }
  return kStOK;
}
//_____________________________________________________________________________

 
 void StSvtHitMaker::SaveIntoNtuple(int numOfClusters, int index)
{

  for( int i=0; i<numOfClusters; i++){
    if( mSvtBigHit->svtHit()[i].flag() < 255)
      m_ClusTuple->Fill(mSvtBigHit->svtHit()[i].flag(),
			mSvtBigHit->WaferPosition()[i].x(),
			mSvtBigHit->WaferPosition()[i].y(),
			mSvtBigHit->svtHit()[i].position().x(),
			mSvtBigHit->svtHit()[i].position().y(),
			mSvtBigHit->svtHit()[i].position().z(),
			mSvtBigHit->svtHit()[i].charge(),
			mSvtBigHit->svtHitData()[i].mom2[0],
			mSvtBigHit->svtHitData()[i].mom2[1],
			mSvtBigHit->svtHitData()[i].numOfAnodesInClu,
			mSvtBigHit->svtHitData()[i].numOfPixelsInClu,
			mSvtBigHit->svtHitData()[i].peakAdc,
			index,
			mSvtData->getEventNumber());
 //    if( iWrite > 1){
//       cluInfo<<mSvtData->getEventNumber()
// 	     <<setw(13)<<  index
// 	     <<setw(13)<<  mSvtBigHit->svtHit()[i].flag()
// 	     <<setw(13)<<  mSvtBigHit->WaferPosition()[i].x()
// 	     <<setw(13)<<  mSvtBigHit->WaferPosition()[i].y()
// 	     <<setw(13)<<  mSvtBigHit->svtHit()[i].position().x()
// 	     <<setw(13)<<  mSvtBigHit->svtHit()[i].position().y()
// 	     <<setw(13)<<  mSvtBigHit->svtHit()[i].position().z()
// 	     <<setw(13)<<  mSvtBigHit->svtHit()[i].charge()
// 	     <<setw(13)<<  mSvtBigHit->svtHitData()[i].mom2[0]
// 	     <<setw(13)<<  mSvtBigHit->svtHitData()[i].mom2[1]
// 	     <<setw(13)<<  mSvtBigHit->svtHitData()[i].numOfAnodesInClu
// 	     <<setw(13)<<  mSvtBigHit->svtHitData()[i].numOfPixelsInClu
// 	     <<setw(13)<<  mSvtBigHit->svtHitData()[i].peakAdc<<endl;
//     }
  }
}

//_____________________________________________________________________________

Int_t StSvtHitMaker::Eval()
{
  for(int barrel = 1;barrel <= mSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtData->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <=mSvtData->getNumberOfHybrids();hybrid++){
	  
	  int index = mSvtData->getHybridIndex(barrel,ladder,wafer,hybrid);
	  if(index < 0) continue;          
	  
	  mSvtBigHit = (StSvtAnalysedHybridClusters*)mSvtCluColl->at(index);
	  if( !mSvtBigHit) continue;

          mSvtGeantHit = (StSvtGeantHits*)mSvtGeantHitColl->at(index);
          if( !mSvtGeantHit) continue;
	  
	  for( int clu=0; clu<mSvtBigHit->numOfHits(); clu++){

            float tim =  mSvtBigHit->WaferPosition()[clu].x();
            float anod =  mSvtBigHit->WaferPosition()[clu].y();

            float diffTime = 0,diffAnode = 0, diffX = 0, diffY = 0, diffZ = 0;

             for( int gHit = 0; gHit < mSvtGeantHit->numberOfHits(); gHit++){
               if(gHit == 0)
                {
                 diffTime = tim -  mSvtGeantHit->waferCoordinate()[gHit].timebucket();
                 diffAnode = anod - mSvtGeantHit->waferCoordinate()[gHit].anode();
		 diffX =  mSvtBigHit->svtHit()[clu].position().x() - mSvtGeantHit->globalCoordinate()[gHit].position().x();
		 diffY =  mSvtBigHit->svtHit()[clu].position().y() - mSvtGeantHit->globalCoordinate()[gHit].position().y();
		 diffZ =  mSvtBigHit->svtHit()[clu].position().z() - mSvtGeantHit->globalCoordinate()[gHit].position().z();
		}
               if ((fabs(tim -  mSvtGeantHit->waferCoordinate()[gHit].timebucket()) < fabs(diffTime)) &&
		   (fabs(anod - mSvtGeantHit->waferCoordinate()[gHit].anode()) < fabs(diffAnode))) {
		 //Pick closest match
		 diffTime = tim - mSvtGeantHit->waferCoordinate()[gHit].timebucket();
		 diffAnode = anod - mSvtGeantHit->waferCoordinate()[gHit].anode();
		 diffX =  mSvtBigHit->svtHit()[clu].position().x() - mSvtGeantHit->globalCoordinate()[gHit].position().x();
		 diffY =  mSvtBigHit->svtHit()[clu].position().y() - mSvtGeantHit->globalCoordinate()[gHit].position().y();
		 diffZ =  mSvtBigHit->svtHit()[clu].position().z() - mSvtGeantHit->globalCoordinate()[gHit].position().z();
	       }
	     }

	     //cout << " diffZ= " << diffZ << endl;
             mTimeHitResolution->Fill(diffTime);
             mAnodeHitResolution->Fill(diffAnode);
             mXHitResolution->Fill(diffX);
             mYHitResolution->Fill(diffY);
             mZHitResolution->Fill(diffZ);
	     mHitResolution->Fill(diffTime,diffAnode);
	  }
	}
      }
    }
  }
 
  return kStOK;
} 

//_____________________________________________________________________________
Int_t StSvtHitMaker::Finish(){

 if (Debug()) gMessMgr->Debug() << "In StSvtHitMaker::Finish() ..." 
				<<  GetName() << endm;

 if( iWrite == 1){
   iWrite = 0;
   m_hfile->Write();
   m_hfile->Close();
   delete m_hfile; m_hfile=0;
   //cluInfo.close();
 }
 return kStOK;
}

//________________________________________________________________________
void StSvtHitMaker::SetFileNames(char* filen, char* filec){
  filenameN = filen;
  filenameC = filec;
}

//________________________________________________________________________
double StSvtHitMaker::LaserTemperatureCorrection() {
  // This function returns a percentage change of the drift velocity
  // obtained from the laser spot positions. It is actually a ratio of the 
  // average distance between the laser spots over the current distance (both in timebuckets). 

  
  double driftVelocityChange;
  int barrel,ladder,wafer,hybrid;
  double anode,tbin; //anode, timebucket position of a given hit

  int index;         //index of laser spot 1/2 located at B3L15W7 (it is the same one for both of the spots)
  double tbin1,tbin2; //timebucket positions of laser spot 1/2 located at B3L15W7

  driftVelocityChange=1.0;
  barrel=3;
  ladder=15;
  wafer=7;
  hybrid=1;
  
  index= mSvtData->getHybridIndex(barrel,ladder,wafer,hybrid);
  mSvtBigHit = (StSvtAnalysedHybridClusters*)mSvtCluColl->at(index);
  if(!mSvtBigHit) {
    // cout << "No data on B3L15W7" << endl;
    return driftVelocityChange;
  }

  tbin1=-1;
  tbin2=-1;
  for( int clu=0; clu<mSvtBigHit->numOfHits(); clu++){
    // we have to find laser spots:
    // spot1: 195<anode<197, 123<tbin<125
    // spot2: 198<anode<201, 95<tbin<98
    // x: timebucket, y: anode
    tbin=mSvtBigHit->WaferPosition()[clu].x();
    anode=mSvtBigHit->WaferPosition()[clu].y();
    if((tbin>123)&&(tbin<125)&&(anode>195)&&( anode<197)) tbin1=tbin;
    if((tbin>95)&&(tbin<98)&&(anode>198)&&(anode<201)) tbin2=tbin;
  }
  
  if((tbin1>0)&&(tbin2>0)&&(tbin1!=tbin2)) driftVelocityChange=27.03/(tbin1-tbin2);
  else {
    // Print out in the case laser spot(s) was(were) not found.
    // Spot which was not found has tbin=-1.
    cout << "Missing laser spot(s):" << endl;
    cout << "spot 1 status " << tbin1 << endl;
    cout << "spot 2 status " << tbin2 << endl;
    return driftVelocityChange;
  }

  return driftVelocityChange;
}
