/***************************************************************************
 *
 * $Id: StSvtHitMaker.cxx,v 1.5 2000/10/02 13:47:03 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Cluster Hit Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtHitMaker.cxx,v $
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

#include <fstream.h>
#include <iomanip.h>

#include "StSvtHitMaker.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StMessMgr.h"

#include "TFile.h"
#include "TNtuple.h"

#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StDbUtilities/StCoordinates.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtAnalysedHybridClusters.hh"


fstream cluInfo;

ClassImp(StSvtHitMaker)
//___________________________________________________________________________
StSvtHitMaker::StSvtHitMaker(const char *name) : StMaker(name)
{

}

//_____________________________________________________________________________
StSvtHitMaker::~StSvtHitMaker(){

}

//_____________________________________________________________________________
Int_t StSvtHitMaker::Init()
{

  if (Debug()) gMessMgr->Debug() << "In StSvtHitMaker::Init() ..."  << endm;


 // Get pointer to StSvtData

  St_DataSet *dataSet2 = GetDataSet("StSvtData");
  assert(dataSet2);
  mSvtData = (StSvtData*)(dataSet2->GetObject());
  assert(mSvtData);

  // Get pointer to StSvtAnalysis

  dataSet2 = GetDataSet("StSvtAnalResults");
  assert(dataSet2);
  mSvtCluColl = (StSvtHybridCollection*)(dataSet2->GetObject());
  assert(mSvtCluColl);


 // 		Create tables
  St_DataSetIter       local(GetInputDB("svt"));


 // 		geometry parameters
   m_shape       = (St_svg_shape   *) local("svgpars/shape");

   if(  !strncmp(mSvtCluColl->getConfiguration(), "Y1L", strlen("Y1L"))){
   m_geom        = (St_svg_geom    *) local("svgpars/geomy1l");
   }
   else{
   m_geom        = (St_svg_geom    *) local("svgpars/geom");
   }

   if (!m_geom) {
     if (!(m_shape)){
 
       gMessMgr->Error() << "SVT- StSvtHitMaker:svt shapes not exist" << endm;
       return kStWarn;
     }
   }
   
   m_srs_srspar  = (St_srs_srspar  *) local("srspars/srs_srspar");
   
   //srs_srspar_st *srs_par = m_srs_srspar->GetTable();
  

  m_x_vs_y = new TH2F("si_x_vs_y","X vs Y of Si space points",
		      300,-30,30,300,-30,30);

  m_x_vs_y->SetYTitle("y cm");
  m_x_vs_y->SetXTitle("x cm");


  m_waf_no = new TH2F*[2*mSvtCluColl->getNumberOfBarrels()];
  
  char title1[16];
  char* title3;
  char  title2[3];

  int barrel=0;

  for( int idbarrel=1; idbarrel<=mSvtCluColl->getNumberOfBarrels(); 
       idbarrel++, barrel++){


    float n_ladders = float(mSvtCluColl->getNumberOfLadders(idbarrel))+0.5;

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

  if( iWrite == 1){
    m_hfile = new TFile("clusters.root","RECREATE","Clusters");
    
    m_ClusTuple = new TNtuple("Clusters","Clusters","flag:xl:yl:x:y:z:charge:mom2x:mom2y:numAnodes:numPixels:peak:hybrid:evt");

     cluInfo.open("ClusterInfo.dat",ios::out);
    
  }

  return  StMaker::Init();

}

//___________________________________________________________________________

Int_t StSvtHitMaker::Make()
{

  if (Debug()) gMessMgr->Debug() << "In StSvtHitMaker::Make() ..."  << endm;
 
 //              Create output tables
  St_scs_spt    *scs_spt    = new St_scs_spt("scs_spt",100);
  m_DataSet->Add(scs_spt);

    TransformIntoSpacePoint();
    FillHistograms();
  return kStOK;
}

//____________________________________________________________________________

void StSvtHitMaker::TransformIntoSpacePoint(){

  int index;
  
  srs_srspar_st *srs_par = m_srs_srspar->GetTable();
  svg_geom_st* geom = m_geom->GetTable();
  svg_shape_st* shape = m_shape->GetTable();
  
  StSvtCoordinateTransform* SvtGeomTrans = new StSvtCoordinateTransform();
  SvtGeomTrans->setParamPointers(&srs_par[0], &geom[0], &shape[0], mSvtData);
  StSvtLocalCoordinate localCoord(0,0,0);
  StSvtWaferCoordinate waferCoord(0,0,0,0,0,0);
  StGlobalCoordinate globalCoord(0,0,0); 
  StThreeVectorF mPos(0,0,0);
  
  for(int barrel = 1;barrel <= mSvtCluColl->getNumberOfBarrels();barrel++) {

    for (int ladder = 1;ladder <= mSvtCluColl->getNumberOfLadders(barrel);ladder++) {

      for (int wafer = 1;wafer <= mSvtCluColl->getNumberOfWafers(barrel);wafer++) {

	for (int hybrid = 1;hybrid <=mSvtCluColl->getNumberOfHybrids();hybrid++){

	  
	  index = mSvtCluColl->getHybridIndex(barrel,ladder,wafer,hybrid);
	  if(index < 0) continue;
	  
	  mSvtBigHit = (StSvtAnalysedHybridClusters*)mSvtCluColl->at(index);
	  if( !mSvtBigHit) continue;
	  
	  for( int clu=0; clu<mSvtBigHit->numOfHits(); clu++){
	    
	    waferCoord.setTimeBucket(mSvtBigHit->WaferPosition()[clu].x());
	    waferCoord.setAnode(mSvtBigHit->WaferPosition()[clu].y());
	    waferCoord.setLayer(mSvtBigHit->getLayerID());
	    waferCoord.setLadder(ladder);
	    waferCoord.setWafer(wafer);
	    waferCoord.setHybrid(hybrid);

	    SvtGeomTrans->operator()(waferCoord,localCoord);


	    // Flag aas bad those hits not in the drift region
	    if( (localCoord.position().x() < -0.1 && localCoord.hybrid()==2)
		|| (localCoord.position().x() > 0.1 && localCoord.hybrid()==1)
		|| fabs(localCoord.position().x())> 3.1){
	      mSvtBigHit->svtHit()[clu].setFlag( 
				      	mSvtBigHit->svtHit()[clu].flag()+3);
	    }
	    
	    SvtGeomTrans->operator()(localCoord,globalCoord);
	    
// 	    cout << " Timebucket=" << waferCoord.timebucket() << 
// 	      " x=" << localCoord.position().x() <<
// 	      " hybrid=" << localCoord.hybrid() <<
// 	      " flag =" <<mSvtBigHit->svtHit()[clu].flag() << endl; 

	    mPos.setX(globalCoord.position().x());
	    mPos.setY(globalCoord.position().y());
	    mPos.setZ(globalCoord.position().z());
	    mSvtBigHit->svtHit()[clu].setPosition(mPos);

	  }
	  
	  if( mSvtBigHit->numOfHits() > 0){
	    SaveIntoTable(mSvtBigHit->numOfHits(), index);

	    if( iWrite == 1){
	      SaveIntoNtuple(mSvtBigHit->numOfHits(),index);
	    }
	  }
	}
      }
    }
    
  }
}

//____________________________________________________________________________
void StSvtHitMaker::SaveIntoTable(int numOfClusters,  int index)
{
  int nRows=0;
  
  //            Create an iterator
  St_DataSetIter svt_spt(m_DataSet);
  St_scs_spt  *scs_spt=0  ;
  scs_spt  = (St_scs_spt *) svt_spt.Find("scs_spt");
  
  
  if( scs_spt){
    
    nRows=scs_spt->GetNRows();
    if( nRows+numOfClusters > scs_spt->GetTableSize()){
      scs_spt->ReAllocate(nRows+numOfClusters);
    }
    
    scs_spt_st *spt= scs_spt->GetTable();
    spt +=nRows;
    for( int i=0; i<numOfClusters; i++,spt++){
      spt->id = mSvtBigHit->svtHitData()[i].id;
      spt->id_wafer = 1000*mSvtBigHit->svtHit()[i].layer()+100*mSvtBigHit->svtHit()[i].wafer()+mSvtBigHit->svtHit()[i].ladder();
      spt->x[0] = mSvtBigHit->svtHit()[i].position().x();
      spt->x[1] = mSvtBigHit->svtHit()[i].position().y();
      spt->x[2] = mSvtBigHit->svtHit()[i].position().z();
      spt->xl[0] = mSvtBigHit->WaferPosition()[i].x();
      spt->xl[1] = mSvtBigHit->WaferPosition()[i].y();
      spt->xl[2] = mSvtBigHit->WaferPosition()[i].z();
      spt->flag = mSvtBigHit->svtHit()[i].flag();
      spt->id_cluster= mSvtBigHit->svtHitData()[i].id_cluster;
      spt->id_globtrk= mSvtBigHit->svtHitData()[i].id_globtrk ;
      spt->id_match= mSvtBigHit->svtHitData()[i].id_match;
      spt->id_mctrack= 0 ;
      spt->id_track= mSvtBigHit->svtHitData()[i].id_track;
      spt->cov[0]= mSvtBigHit->svtHit()[i].positionError().x()
	*mSvtBigHit->svtHit()[i].positionError().x();
      spt->cov[1]= mSvtBigHit->svtHit()[i].positionError().y()
	*mSvtBigHit->svtHit()[i].positionError().y();
      spt->cov[2]= mSvtBigHit->svtHit()[i].positionError().z()
	*mSvtBigHit->svtHit()[i].positionError().z();

      for( int j=0; j<3; j++){
	spt->res[j]= 0;
      }
      for( int k=0; k<2; k++){
	spt->mom2[k]= mSvtBigHit->svtHitData()[i].mom2[k];
      }  
      spt->de[0]= mSvtBigHit->svtHit()[i].charge();
      spt->de[1]= mSvtBigHit->svtHitData()[i].peakAdc;
      
    }
    scs_spt->SetNRows(nRows+numOfClusters);
  }
  
}

//_____________________________________________________________________________
Int_t StSvtHitMaker::FillHistograms(){
  
  
  int index;

  for(int barrel = 1;barrel <= mSvtCluColl->getNumberOfBarrels();barrel++) {
    
    for (int ladder = 1;ladder <= mSvtCluColl->getNumberOfLadders(barrel);ladder++) {

      for (int wafer = 1;wafer <= mSvtCluColl->getNumberOfWafers(barrel);wafer++) {
	
	for (int hybrid = 1;hybrid <=mSvtCluColl->getNumberOfHybrids();hybrid++){
	  
	  
	  index = mSvtCluColl->getHybridIndex(barrel,ladder,wafer,hybrid);
	  if(index < 0) continue;
	  
	  mSvtBigHit = (StSvtAnalysedHybridClusters*)mSvtCluColl->at(index);
	  if( !mSvtBigHit) continue;
	  
	  for( int clu=0; clu<mSvtBigHit->numOfHits(); clu++){
	    
	    if( mSvtBigHit->svtHit()[clu].flag() < 255){
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
    
    cluInfo<<mSvtData->getEventNumber()
	   <<setw(13)<<  index
	   <<setw(13)<<  mSvtBigHit->svtHit()[i].flag()
	   <<setw(13)<<  mSvtBigHit->WaferPosition()[i].x()
	   <<setw(13)<<  mSvtBigHit->WaferPosition()[i].y()
	   <<setw(13)<<  mSvtBigHit->svtHit()[i].position().x()
	   <<setw(13)<<  mSvtBigHit->svtHit()[i].position().y()
	   <<setw(13)<<  mSvtBigHit->svtHit()[i].position().z()
	   <<setw(13)<<  mSvtBigHit->svtHit()[i].charge()
	   <<setw(13)<<  mSvtBigHit->svtHitData()[i].mom2[0]
	   <<setw(13)<<  mSvtBigHit->svtHitData()[i].mom2[1]
	   <<setw(13)<<  mSvtBigHit->svtHitData()[i].numOfAnodesInClu
	   <<setw(13)<<  mSvtBigHit->svtHitData()[i].numOfPixelsInClu
	   <<setw(13)<<  mSvtBigHit->svtHitData()[i].peakAdc<<endl;
  }
}

//_____________________________________________________________________________
Int_t StSvtHitMaker::Finish(){

 if (Debug()) gMessMgr->Debug() << "In StSvtHitMaker::Finish() ..." 
				<<  GetName() << endm;

 if( iWrite == 1){
   m_hfile->Write();
   m_hfile->Close();
   cluInfo.close();
 }
 return kStOK;
}

