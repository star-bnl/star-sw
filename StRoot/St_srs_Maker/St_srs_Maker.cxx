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
  St_srs_Maker::St_srs_Maker(const char *name, const char *title):
StMaker(name,title),
m_config(0),
m_shape(0),
m_srs_activea(0),
m_srs_srspar(0),
m_srs_direct(0)
{
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_srs_Maker::~St_srs_Maker(){
}
//_____________________________________________________________________________
Int_t St_srs_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->DataSet("params"));
// geometry parameters
   m_shape       = (St_svg_shape   *) local("svt/svgpars/shape");
   m_config      = (St_svg_config  *) local("svt/svgpars/config");
   m_geom        = (St_svg_geom    *) local("svt/svgpars/geom");
   m_srs_activea = (St_srs_activea *) local("svt/srspars/srs_activea");
   m_srs_srspar  = (St_srs_srspar  *) local("svt/srspars/srs_srspar");
   m_srs_direct  = (St_srs_direct  *) local("svt/srspars/srs_direct");
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_srs_Maker::Make(){
// Create output tables
   if (!m_DataSet->GetList())  {
     St_scs_spt    *scs_spt    = new St_scs_spt("scs_spt",20000);       m_DataSet->Add(scs_spt);
     St_srs_result *srs_result = new St_srs_result("srs_result",20000); m_DataSet->Add(srs_result);
     St_DataSetIter geant(gStChain->DataSet("geant"));
     St_g2t_svt_hit *g2t_svt_hit = (St_g2t_svt_hit *) geant("g2t_svt_hit");
     if (g2t_svt_hit ) {

       Int_t res =  srs_am (srs_result,g2t_svt_hit,scs_spt,
                          m_geom,m_config,m_shape,m_srs_srspar,m_srs_direct,m_srs_activea);
     }
   }
   if (gStChain->Debug()) m_DataSet->ls("*");
  //  PrintInfo();
 return kStOK;
}
//_____________________________________________________________________________
void St_srs_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_srs_Maker.cxx,v 1.12 1998/10/31 00:26:20 fisyak Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

