//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_sls_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include "St_sls_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "svt/St_sls_am_Module.h"
#include "TFile.h"
#include "StMessMgr.h"

ClassImp(St_sls_Maker)

//_____________________________________________________________________________
  St_sls_Maker::St_sls_Maker(const char *name):
StMaker(name),
m_geom_par(0),
m_geom(0),
m_ctrl(0)

{
}
//_____________________________________________________________________________
St_sls_Maker::~St_sls_Maker(){
}
//_____________________________________________________________________________
Int_t St_sls_Maker::Init(){
  if (Debug())  gMessMgr->Debug() << "In St_sls_Maker::Make() ... "
                               << GetName() << endm;
// 		Create tables
  St_DataSet *svtparams = GetInputDB("svt/ssd");
  St_DataSetIter       local(svtparams);

// 		geometry parameters
  m_geom_par    = (St_sdm_geom_par*)local("sdm_geom_par");
  m_ctrl        = (St_sls_ctrl    *)local("sls_ctrl");
  svtparams = GetInputDB("svt/svgpars");
  local.Reset(svtparams);
  m_geom        = (St_svg_geom    *)local("geom");
  if ((!m_geom_par)||(!m_geom)) {
    gMessMgr->Error() << "No  access to geometry parameters" << endm;
  }   
  if (!m_ctrl) {
    gMessMgr->Error() << "No  access to control parameters" << endm;
  }   
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_sls_Maker::Make()
{
  // 		Create output tables
   Int_t res = 0;
   St_sls_spt    *sls_spt   = new St_sls_spt("sls_spt",5000);
   m_DataSet->Add(sls_spt);
   St_sls_strip  *sls_strip = new St_sls_strip("sls_strip",40000);
   m_DataSet->Add(sls_strip);

   St_DataSetIter geant(GetInputDS("geant"));
   St_g2t_svt_hit *g2t_svt_hit = (St_g2t_svt_hit *) geant("g2t_svt_hit");
   res = sls_am (g2t_svt_hit, m_geom, m_geom_par, m_ctrl, sls_spt, sls_strip);

   if(res!=kSTAFCV_OK){
     gMessMgr->Warning("St_sls_Maker: no output");
     return kStWarn;
   }
   if(Debug())  gMessMgr->Debug() << "In St_sls_Maker::Make() ... "
                               << GetName() << endm;
  return kStOK;
}
//_____________________________________________________________________________
void St_sls_Maker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: St_sls_Maker.cxx,v 1.2 2000/08/15 19:31:19 hippolyt Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
Int_t St_sls_Maker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In St_sls_Maker::Finish() ... "
                               << GetName() << endm; 
  return kStOK;
}



