//$Id: St_srs_Maker.cxx,v 1.27 2001/04/26 23:56:19 caines Exp $
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_srs_Maker class for Makers                                        //
// Author : Anon                                                       //
//////////////////////////////////////////////////////////////////////////
//$Log: St_srs_Maker.cxx,v $
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

#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StDbUtilities/StSvtWaferCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StMessMgr.h"
#include "St_srs_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "TString.h"

#include "StSvtClassLibrary/StSvtConfig.hh"
#include "svt/St_svg_am_Module.h"
#include "svt/St_srs_am_Module.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"
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
}
//_____________________________________________________________________________
St_srs_Maker::~St_srs_Maker(){
}
//_____________________________________________________________________________
Int_t St_srs_Maker::Init(){

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
   m_shape       = (St_svg_shape   *) local("svgpars/shape");
   m_config      = (St_svg_config  *) local("svgpars/config");
   m_geom        = (St_svg_geom    *) local("svgpars/geom");

   if (!m_geom) {
     if (!(m_shape && m_config)){
       cout << " St_params_Maker:tpg_pad_plane or tpg_detector do not exist" << endl;
     }
     else {
       m_geom = new St_svg_geom("geom",216);
       Int_t res = svg_am(m_config,m_shape,m_geom);
       if (res != kSTAFCV_OK) return kStWarn;
     }
   }
   m_srs_activea = (St_srs_activea *) local("srspars/srs_activea");
   m_srs_srspar  = (St_srs_srspar  *) local("srspars/srs_srspar");
   m_srs_direct  = (St_srs_direct  *) local("srspars/srs_direct");

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
//_____________________________________________________________________________
Int_t St_srs_Maker::Make()
{
  Int_t res; 
// 		Create output tables
  St_scs_spt    *scs_spt    = new St_scs_spt("scs_spt",25000);       m_DataSet->Add(scs_spt);
  St_srs_result *srs_result = new St_srs_result("srs_result",25000); m_DataSet->Add(srs_result);
    
  g2t_svt_hit_st *GeantHit;
  g2t_vertex_st *GeantVertex;
  g2t_track_st *GeantTrack;
  
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


  srs_srspar_st* mSvtSrsPar = m_srs_srspar->GetTable();
  svg_shape_st* mSvtShape  = m_shape->GetTable();
  svg_geom_st* mSvtGeom   = m_geom->GetTable();

  mCoordTransform->setParamPointers(mSvtSrsPar, mSvtGeom, mSvtShape, mConfig);

  int MaxTimeBucket = (int)(3.*mSvtSrsPar[0].fsca/mSvtSrsPar[0].vd);
  StSvtWaferCoordinate WaferCoord;
  StGlobalCoordinate GlobalCoord;
  int VertexId;
  double TimeBucketShift;
  
//		Fill histograms and cope with pile up events

  if( scs_spt->GetNRows()){

    scs_spt_st  *spc   = scs_spt->GetTable();
    for (Int_t i = 0; i < scs_spt->GetNRows(); i++,spc++){
      
      if( spc->id_wafer < 7000){
	
	VertexId=GeantTrack[spc->id_mctrack-1].start_vertex_p-1;
	TimeBucketShift =  GeantVertex[VertexId].ge_tof*mSvtSrsPar[0].fsca;
	// Shuffle space points if a pile up event
	GlobalCoord.setPosition(StThreeVector<double>(spc->x[0],spc->x[1],
						      spc->x[2]));	 

	//cout << GlobalCoord ;
	mCoordTransform->operator()(GlobalCoord,WaferCoord);
	WaferCoord.setTimeBucket(WaferCoord.timebucket()+TimeBucketShift);
	if( WaferCoord.timebucket() < 0. || WaferCoord.timebucket() > MaxTimeBucket){
	  spc->flag = -77;
	  gMessMgr->Warning() << " Moved hit off of wafer" << endl;
	  continue;
	}
	else{
	  mCoordTransform->operator()(WaferCoord,GlobalCoord);
	  // cout << GlobalCoord << endl;
	  
	  if( GlobalCoord.position().x() < -99){
	    spc->flag = -77;
	    cout << " Moved hit off of wafer" << endl;
	    continue;
	  }
	}
      }
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
  }
    
  return kStOK;
}
//_____________________________________________________________________________

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
