//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_srs_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_srs_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "svt/St_svg_am_Module.h"
#include "svt/St_srs_am_Module.h"
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

// 		Create tables
   St_DataSetIter       local(GetInputDB("params/svt"));

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
   m_x_vs_y = new TH2F("si_x_vs_y","X vs Y of Si space points",
                           300,-30,30,300,-30,30);
   m_x_vs_y->SetYTitle("y cm");
   m_x_vs_y->SetXTitle("x cm");

   m_waf_no1 = new TH2F("si_layer1"," Si z vs ladder no.",
                           100,-25.,25.,100,0.5,4.5);
   m_waf_no1->SetYTitle("ladder no");
   m_waf_no1->SetXTitle("Z cm");

   m_waf_no2 = new TH2F("si_layer2"," Si z vs ladder no.",
			100,-25.,25.,100,0.5,4.5);
   m_waf_no2->SetYTitle("ladder no");
   m_waf_no2->SetXTitle("Z cm");

   m_waf_no3 = new TH2F("si_layer3"," Si z vs ladder no.",
			100,-25.,25.,100,0.5,6.5);
   m_waf_no3->SetYTitle("ladder no");
   m_waf_no3->SetXTitle("Z cm");

   m_waf_no4 = new TH2F("si_layer4"," Si z vs ladder no.",
			100,-25.,25.,100,0.5,6.5);
   m_waf_no4->SetYTitle("ladder no");
   m_waf_no4->SetXTitle("Z cm");

   m_waf_no5 = new TH2F("si_layer5"," Si z vs ladder no.",
			100,-25.,25.,100,0.5,7.5);
   m_waf_no5->SetYTitle("ladder no");
   m_waf_no5->SetXTitle("Z cm");

   m_waf_no6 = new TH2F("si_layer6"," Si z vs ladder no.",
			100,-25.,25.,100,0.5,7.5);
   m_waf_no6->SetYTitle("ladder no");
   m_waf_no6->SetXTitle("Z cm");

   m_waf_no7 = new TH2F("si_layer7"," Si z vs ladder no.",
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
  St_scs_spt    *scs_spt    = new St_scs_spt("scs_spt",20000);       m_DataSet->Add(scs_spt);
  St_srs_result *srs_result = new St_srs_result("srs_result",20000); m_DataSet->Add(srs_result);
    
  St_DataSetIter geant(GetInputDS("geant"));
  St_g2t_svt_hit *g2t_svt_hit = (St_g2t_svt_hit *) geant("g2t_svt_hit");
  if (g2t_svt_hit ) {
    
    res =  srs_am (srs_result,  g2t_svt_hit, scs_spt,
                   m_geom,      m_config,    m_shape,
                   m_srs_srspar,m_srs_direct,m_srs_activea);
    if(res!=kSTAFCV_OK) return kStWarn;
    if (Debug()) m_DataSet->ls("*");
  }
  
//		Fill histograms

  if( scs_spt->GetNRows()){

    scs_spt_st  *spc   = scs_spt->GetTable();
    for (Int_t i = 0; i < scs_spt->GetNRows(); i++,spc++){
      m_x_vs_y->Fill(spc->x[0],spc->x[1]);

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


    }
  }
    
  return kStOK;
}
//_____________________________________________________________________________

