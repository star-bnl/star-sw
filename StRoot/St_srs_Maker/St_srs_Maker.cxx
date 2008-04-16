//$Id: St_srs_Maker.cxx,v 1.39 2007/12/27 23:52:00 fisyak Exp $
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_srs_Maker class for Makers                                        //
// Author : Anon                                                       //
//////////////////////////////////////////////////////////////////////////
//$Log: St_srs_Maker.cxx,v $
//Revision 1.39  2007/12/27 23:52:00  fisyak
//Fix bug with hit error setting
//
//Revision 1.38  2007/12/12 22:49:22  fisyak
//Syncronize srs parameters with Calibrations/tracker/svtHitError
//
//Revision 1.37  2007/04/28 17:56:56  perev
//Redundant StChain.h removed
//
//Revision 1.36  2007/03/21 17:28:25  fisyak
//New coordinate transformation
//
//Revision 1.35  2006/03/16 18:27:40  caines
//Add t0 shift when checking timebucket of hit against largest possible timebucket for flagging hits as bad
//
//Revision 1.34  2004/05/03 23:34:16  perev
//Possible non init WarnOff
//
//Revision 1.33  2003/04/30 20:39:23  perev
//Warnings cleanup. Modified lines marked VP
//
//Revision 1.32  2003/04/16 19:02:54  caines
//Make srs pick up Vd and T0 from database not table
//
//Revision 1.31  2003/04/05 22:36:26  caines
//Fix filling on local coords so its time and anode not cm
//
//Revision 1.30  2002/12/05 23:37:42  caines
//Remove hits if they are on a  bad anode
//
//Revision 1.29  2002/02/15 20:21:54  caines
//Work with database
//
//Revision 1.28  2001/11/12 22:56:38  caines
// Simulation hits now go into our SvtHit collection like real data
//
//Revision 1.27  2001/04/26 23:56:19  caines
//Check that Geant svt hits,tracks and vertex exist before do get table
//
//Revision 1.26  2001/04/26 23:51:39  caines
//Check that Geant svt hits,tracks and vertex exist before do get table
//
//Revision 1.25  2001/04/23 23:22:43  caines
//Fix setPosition error on Solaris
//
//Revision 1.24  2001/04/20 14:36:22  caines
// Added code to do pp pile up
//
//Revision 1.23  2000/06/23 16:52:41  fisyak
//remove params
//
//Revision 1.22  2000/04/07 14:36:09  caines
//Correct x y histogram filling
//
//Revision 1.21  2000/02/24 16:58:39  caines
//Change srs histogram names to srs*
//
//Revision 1.20  2000/02/17 23:23:03  caines
// Adjust for using new geom files using hardware ids
//
//Revision 1.19  1999/12/23 16:38:48  caines
//Added cvs ID strings
//

#include "StMessMgr.h"
#include "St_srs_Maker.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "TString.h"

#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StDbUtilities/StSvtWaferCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StSvtClassLibrary/StSvtHybridBadAnodes.hh"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClusterMaker/StSvtAnalysedHybridClusters.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtT0.hh"
#include "StSvtClassLibrary/StSvtHybridDriftVelocity.hh"
#include "svt/St_svg_am_Module.h"
#include "svt/St_srs_am_Module.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "StDbUtilities/St_svtHybridDriftVelocityC.h"
#include "tables/St_HitError_Table.h"
ClassImp(St_srs_Maker)

//_____________________________________________________________________________
  St_srs_Maker::St_srs_Maker(const char *name):
StMaker(name),
m_config(0),
m_shape(0),
m_srs_activea(0),
m_srs_srspar(0),
m_srs_direct(0)

{
  mSvtBadAnodes = NULL;
}
//_____________________________________________________________________________
St_srs_Maker::~St_srs_Maker(){
}
//_____________________________________________________________________________
Int_t St_srs_Maker::Init(){

 // 		Create Histograms    
   m_x_vs_y = new TH2F("srs_x_vs_y","X vs Y of Si space points",
                           300,-30,30,300,-30,30);
   m_x_vs_y->SetYTitle("y cm");
   m_x_vs_y->SetXTitle("x cm");

   m_waf_no1 = new TH2F("srs_layer1"," Si z vs ladder no.",
                           100,-25.,25.,20,0.5,8.5);
   m_waf_no1->SetYTitle("ladder no");
   m_waf_no1->SetXTitle("Z cm");

   m_waf_no2 = new TH2F("srs_layer2"," Si z vs ladder no.",
			100,-25.,25.,20,0.5,8.5);
   m_waf_no2->SetYTitle("ladder no");
   m_waf_no2->SetXTitle("Z cm");

   m_waf_no3 = new TH2F("srs_layer3"," Si z vs ladder no.",
			100,-25.,25.,20,0.5,12.5);
   m_waf_no3->SetYTitle("ladder no");
   m_waf_no3->SetXTitle("Z cm");

   m_waf_no4 = new TH2F("srs_layer4"," Si z vs ladder no.",
			100,-25.,25.,20,0.5,12.5);
   m_waf_no4->SetYTitle("ladder no");
   m_waf_no4->SetXTitle("Z cm");

   m_waf_no5 = new TH2F("srs_layer5"," Si z vs ladder no.",
			100,-25.,25.,20,0.5,16.5);
   m_waf_no5->SetYTitle("ladder no");
   m_waf_no5->SetXTitle("Z cm");

   m_waf_no6 = new TH2F("srs_layer6"," Si z vs ladder no.",
			100,-25.,25.,20,0.5,16.5);
   m_waf_no6->SetYTitle("ladder no");
   m_waf_no6->SetXTitle("Z cm");

   m_waf_no7 = new TH2F("srs_layer7"," Si z vs ladder no.",
			100,-40.,40.,100,0.5,20.5);
   m_waf_no7->SetYTitle("ladder no");
   m_waf_no7->SetXTitle("Z cm");

   

   return StMaker::Init();
}
//________________________________________________________________________________
Int_t St_srs_Maker::InitRun(Int_t runnuber) {
 //Get configuration setup

   St_DataSet *dataSet = GetDataSet("StSvtConfig");
   
   if (dataSet){
    setConfig((StSvtConfig*)(dataSet->GetObject()));
   }
   else{
     dataSet = new St_ObjectSet("StSvtConfig");
     AddConst(dataSet);  
     mConfig = new StSvtConfig();
     setConfig("FULL");
     mConfig->setConfiguration(mConfigString.Data());
     dataSet->SetObject((TObject*)mConfig);
     
   }
   mCoordTransform =  new StSvtCoordinateTransform();

// 		Create tables
   St_DataSetIter       local(GetInputDB("svt"));


// 		geometry parameters
   m_shape       = (St_svg_shape   *) GetInputDB("svt/svgpars/shape");
   m_config      = (St_svg_config  *) GetInputDB("svt/svgpars/config");
   m_geom        = (St_svg_geom    *) GetInputDB("svt/svgpars/geom");

   if (!m_geom) {
     if (!(m_shape && m_config)){
       cout << " St_srs_Maker::InitRun svt/svgpars/geom do not exist" << endl;
     }
     else {
       m_geom = new St_svg_geom("geom",216);
       Int_t res = svg_am(m_config,m_shape,m_geom);
       if (res != kSTAFCV_OK) return kStWarn;
     }
   }
   m_srs_activea = (St_srs_activea *) GetInputDB("svt/srspars/srs_activea");
   m_srs_srspar  = (St_srs_srspar  *) GetInputDB("svt/srspars/srs_srspar");
   St_HitError  *stiSvtHitErrors = (St_HitError  *) GetInputDB("Calibrations/tracker/svtHitError");
   assert(stiSvtHitErrors);
   St_HitError  *stiSsdHitErrors = (St_HitError  *) GetInputDB("Calibrations/tracker/ssdHitError");
   assert(stiSsdHitErrors);
   m_srs_direct  = new St_srs_direct("srs_direct",2);
   AddConst(m_srs_direct);
   srs_direct_st direct;
   HitError_st  *SvtHitErrors = stiSvtHitErrors->GetTable();
   HitError_st  *SsdHitErrors = stiSsdHitErrors->GetTable();
   direct.sd = TMath::Sqrt(SvtHitErrors->coeff[0]);
   direct.st = TMath::Sqrt(SvtHitErrors->coeff[3]);
   m_srs_direct->AddAt(&direct.sd,0);
   direct.sd = TMath::Sqrt(SsdHitErrors->coeff[0]);
   direct.st = TMath::Sqrt(SsdHitErrors->coeff[3]);
   m_srs_direct->AddAt(&direct.sd,1);
   cout << "Replace hit errors from Calibrations/tracker/(ssd|svt)HitError" << endl;
   m_srs_direct->Print(0,2);
   //   Get Bad Anodes

   GetBadAnodes();
   return kStOK;
}
//___________________________________________________________________________

Int_t St_srs_Maker::GetBadAnodes()
{

  St_DataSet *dataSet;
  
  dataSet = GetDataSet("StSvtBadAnodes");
  if( !dataSet) {
    gMessMgr->Warning() << " No Svt Bad Anodes data set" << endm;
    return kStWarn;
  }

  mSvtBadAnodes = (StSvtHybridCollection*)(dataSet->GetObject());
  if( !mSvtBadAnodes) {
    gMessMgr->Warning() << " No Svt Bad Anodes data " << endm;
    return kStWarn;
  }
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_srs_Maker::Make()
{
  Int_t res; 

  StSvtHybridBadAnodes* BadAnode = NULL;
// 		Create output tables
  St_scs_spt    *scs_spt    = new St_scs_spt("scs_spt",25000);       m_DataSet->Add(scs_spt);
  St_srs_result *srs_result = new St_srs_result("srs_result",25000); m_DataSet->Add(srs_result);
    
  g2t_svt_hit_st *GeantHit=0;
  g2t_vertex_st *GeantVertex=0;
  g2t_track_st *GeantTrack=0;
  
  St_DataSetIter geant(GetInputDS("geant"));
  St_g2t_svt_hit *g2t_svt_hit = (St_g2t_svt_hit *) geant("g2t_svt_hit");
  if( g2t_svt_hit)  GeantHit = g2t_svt_hit->GetTable();
  St_g2t_vertex *g2t_vertex = (St_g2t_vertex *) geant("g2t_vertex");
  if( g2t_vertex) GeantVertex = g2t_vertex->GetTable(); 
  St_g2t_track *g2t_track = (St_g2t_track *) geant("g2t_track");
  if(g2t_track)GeantTrack = g2t_track->GetTable(); 
  if (g2t_svt_hit ) {
    
 
   res =  srs_am (srs_result,  g2t_svt_hit, scs_spt,
                   m_geom,      m_config,    m_shape,
                   m_srs_srspar,m_srs_direct,m_srs_activea);
    if(res!=kSTAFCV_OK) return kStWarn;
    if (Debug()) m_DataSet->ls("*");
  }


  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtGeometry");
  if(!dataSet) {
    gMessMgr->Error("Failure to get SVT geometry - THINGS HAVE GONE SERIOUSLY WRONG!!!!! \n");
    
    return kStOK;
  }
  
  StSvtGeometry *GeomDataBase = (StSvtGeometry*)dataSet->GetObject();
  dataSet = GetDataSet("StSvtT0");
  StSvtT0 *T0 = (StSvtT0*)dataSet->GetObject();
  dataSet = GetDataSet("StSvtDriftVelocity");
#if 0
  StSvtHybridCollection *DriftVel = (StSvtHybridCollection*)
    dataSet->GetObject();
#else
  St_svtHybridDriftVelocityC *DriftVel = St_svtHybridDriftVelocityC::instance();
  assert(DriftVel);
#endif
  if(GeomDataBase)   mCoordTransform->setParamPointers(GeomDataBase, mConfig, 0, T0);
						       //YF	       DriftVel, T0);
  
  // cope with pile up events and fill hit collection
//VPunused   srs_srspar_st* mSvtSrsPar = m_srs_srspar->GetTable();

   int MaxTimeBucket;
   StSvtWaferCoordinate WaferCoord;
   StGlobalCoordinate GlobalCoord;
   StSvtAnalysedHybridClusters* mSvtAnalClusters;
   int VertexId, index, NumOfHits;
   double TimeBucketShift;


   SetSvtAnalysis();

  if( scs_spt->GetNRows()){

    scs_spt_st  *spc   = scs_spt->GetTable();
    for (Int_t i = 0; i < scs_spt->GetNRows(); i++,spc++){
      
      if( spc->id_wafer < 7000){
	
	VertexId=GeantTrack[spc->id_mctrack-1].start_vertex_p-1;
	TimeBucketShift =  GeantVertex[VertexId].ge_tof*T0->getFsca();
	// Shuffle space points if a pile up event
	GlobalCoord.setPosition(StThreeVector<double>(spc->x[0],spc->x[1],
						      spc->x[2]));	 

	//cout << GlobalCoord ;
	mCoordTransform->operator()(GlobalCoord,WaferCoord,spc->id_wafer);

	//Fill hit collection
	index = mSvtAnalColl->getHybridIndex(WaferCoord.barrel(),
					     WaferCoord.ladder(),
					     WaferCoord.wafer(),
					     WaferCoord.hybrid());
	
	if( index <0) continue;
#if 0
	MaxTimeBucket = (int)(3.*T0->getFsca()/
			      ((StSvtHybridDriftVelocity*)
			       DriftVel->at(index))->getV3(1)+T0->getT0());
#else
	MaxTimeBucket = (int)(3.*T0->getFsca()/DriftVel->DriftVelocity(WaferCoord.barrel(),WaferCoord.ladder(),WaferCoord.wafer(),WaferCoord.hybrid())
			       +T0->getT0());
#endif	
	
	WaferCoord.setTimeBucket(WaferCoord.timebucket()+TimeBucketShift);
	
	if( WaferCoord.timebucket() < 0. || WaferCoord.timebucket() > MaxTimeBucket){
	  spc->flag = 7;
	  gMessMgr->Warning() << " Moved hit off of wafer" << endl;
	}
	else{
	  mCoordTransform->operator()(WaferCoord,GlobalCoord);
	  // cout << GlobalCoord << endl;
	  
	  if( GlobalCoord.position().x() < -99){
	    spc->flag = 8;
	    cout << " Moved hit off of wafer" << endl;
	  }
	}
	
	// retrieve bad anodes
	if (mSvtBadAnodes)
	  BadAnode = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(index);
	if(BadAnode)
	  {
	    if( BadAnode->isBadAnode((int)WaferCoord.anode())){
	      //cout << "Found a bad anode" << endl;
	      spc->flag = 9;
	    }
	  }
	
	
	mSvtAnalClusters = (StSvtAnalysedHybridClusters*)
	  mSvtAnalColl->at(index);
	
	NumOfHits=0;
	if( mSvtAnalClusters){
	  NumOfHits = mSvtAnalClusters->numOfHits();
	  // Resize every 10 hits
          if( NumOfHits%10 == 0){
	    mSvtAnalClusters->ReSize();
	  }
	}
	else{
	  mSvtAnalClusters = new StSvtAnalysedHybridClusters(
							WaferCoord.barrel(),
							WaferCoord.ladder(),
							WaferCoord.wafer(),
							WaferCoord.hybrid());
	  mSvtAnalClusters->setMembers(NumOfHits,index);
	}
	if(mSvtAnalClusters) {
	  mSvtAnalClusters->setSvtHit(spc, &WaferCoord);
	  mSvtAnalColl->at(index) = mSvtAnalClusters;
	}			
      }
    }
    
    FillHist(scs_spt);
  }
  return kStOK;
}
//___________________________________________________________________________
Int_t St_srs_Maker::SetSvtAnalysis()
{
  
  mSvtAnalSet = new St_ObjectSet("StSvtAnalResults");
  AddData(mSvtAnalSet);  
  
  mSvtAnalColl = new StSvtHybridCollection(mConfig);    
  mSvtAnalSet->SetObject((TObject*)mSvtAnalColl); 
  
  for(int i=0; i<mConfig->getTotalNumberOfHybrids(); i++){
    mSvtAnalColl->at(i) = NULL;  
  }
  return kStOK;
}
//___________________________________________________________________________
Int_t St_srs_Maker::FillHist(St_scs_spt* scs_spt){
  
  //		Fill histograms 
  
  scs_spt_st  *spc   = scs_spt->GetTable();
  for (Int_t i = 0; i < scs_spt->GetNRows(); i++,spc++){
    
    
    
    int lader = spc->id_wafer -((int)spc->id_wafer/1000)*1000;
    lader = lader -(int)(lader/100)*100;
    float ladder = (float) lader;
    if( 999 < spc->id_wafer && spc->id_wafer <1999){
      m_waf_no1->Fill(spc->x[2],ladder);
    }
    else if( 1999 < spc->id_wafer && spc->id_wafer <2999){
      m_waf_no2->Fill(spc->x[2],ladder);
    }
    else if( 2999 < spc->id_wafer && spc->id_wafer <3999){
      m_waf_no3->Fill(spc->x[2],ladder);
    }
    
    else if( 3999 < spc->id_wafer && spc->id_wafer <4999){
      m_waf_no4->Fill(spc->x[2],ladder);
    }
    else if( 4999 < spc->id_wafer && spc->id_wafer <5999){
      m_waf_no5->Fill(spc->x[2],ladder);
    }
    else if( 5999 < spc->id_wafer && spc->id_wafer <6999){
      m_waf_no6->Fill(spc->x[2],ladder);
    }
    else if( 6999 < spc->id_wafer && spc->id_wafer <8999){
      m_waf_no7->Fill(spc->x[2],ladder);
    }
    
    m_x_vs_y->Fill(spc->x[0],spc->x[1]);
        
  }
  return kStOk;
}

//_____________________________________________________________________________

Int_t St_srs_Maker::setConfig(StSvtConfig* config)
{
  mConfigString = TString(config->getConfiguration());
  mConfig = config;
  return kStOK;
}
//_____________________________________________________________________________

Int_t St_srs_Maker::setConfig(const char* config)
{

  gMessMgr->Message() <<"St_srs_Maker:Setting configuration to "<< config << endm;
  mConfigString = config;
  return kStOK;
}
