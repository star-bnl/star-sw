 /**************************************************************************
 * Class      : St_sls_maker.cxx
 **************************************************************************
 * $Id: St_sls_Maker.cxx,v 1.10 2006/09/15 21:09:52 bouchet Exp $
 *
 * $Log: St_sls_Maker.cxx,v $
 * Revision 1.10  2006/09/15 21:09:52  bouchet
 * read the noise and pedestal from ssdStripCalib
 *
 * Revision 1.9  2005/05/13 15:08:58  bouchet
 * reading svt/ssd tables
 *
 * Revision 1.8  2005/05/13 09:28:24  lmartin
 * geant information read from g2t_ssd_hit table
 *
 * Revision 1.7  2005/05/13 08:39:33  lmartin
 * CVS tags added
 *
 * Revision 1.6  2003/10/08 03:46:34  suire
 * *** empty log message ***
 *
 * Revision 1.4  2002/03/25 20:06:43  suire
 * Doxygen documentation, cleaning
 *
 *
 **************************************************************************/
#include <Stiostream.h>
#include <stdlib.h>
#include "St_sls_Maker.h"
#include "StChain.h"
#include "TDataSetIter.h"
#include "TFile.h"
#include "StMessMgr.h"

#include "StSlsBarrel.hh"
#include "tables/St_sls_strip_Table.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_g2t_ssd_hit_Table.h"
#include "tables/St_ssdDimensions_Table.h"
//#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_slsCtrl_Table.h"

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
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t  St_sls_Maker::InitRun(Int_t runNumber) {
// 		geometry parameters
  TDataSet *ssdparams = GetInputDB("Geometry/ssd");
  if (! ssdparams) {
    gMessMgr->Error() << "No  access to Geometry/ssd parameters" << endm;
    return kStErr;
  }
  TDataSetIter    local(ssdparams);
  m_ctrl        = (St_slsCtrl           *)local("slsCtrl");
  m_geom_par    = (St_ssdDimensions     *)local("ssdDimensions");
  m_geom        = (St_ssdWafersPosition *)local("ssdWafersPosition");
  if (!m_ctrl) {
    gMessMgr->Error() << "No  access to control parameters" << endm;
    return kStErr;
  }   
  if ((!m_geom_par)||(!m_geom)) {
    gMessMgr->Error() << "No  access to geometry parameters" << endm;
    return kStErr;
  }   
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_sls_Maker::Make()
{
  // 		Create output tables
   Int_t res = 0;
   St_sls_strip  *sls_strip = new St_sls_strip("sls_strip",40000);
   m_DataSet->Add(sls_strip);

   TDataSetIter geant(GetInputDS("geant"));
   St_g2t_svt_hit *g2t_svt_hit = (St_g2t_svt_hit *) geant("g2t_svt_hit");
   St_g2t_ssd_hit *g2t_ssd_hit = (St_g2t_ssd_hit *) geant("g2t_ssd_hit");

   ssdDimensions_st *geom_par = m_geom_par->GetTable();
   slsCtrl_st *ctrl = m_ctrl->GetTable();

   cout<<"#################################################"<<endl;
   cout<<"####       START OF SSD LAZY SIMULATOR       ####"<<endl;
   cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;
   StSlsBarrel *mySsd = new StSlsBarrel(geom_par);
   cout<<"####        SSD WAFERS INITIALIZATION        ####"<<endl;
   mySsd->initWafers(m_geom);
   int nSsdHits;
   if (g2t_ssd_hit)
     {
       nSsdHits = mySsd->readPointFromTable(g2t_ssd_hit);
     }
   if (nSsdHits == 0)
     {
       if (g2t_svt_hit)
         {
           nSsdHits = mySsd->readPointFromTable(g2t_svt_hit);
         }
     }    
   cout<<"####    ->  "<<nSsdHits<<" HITS READ FROM TABLE        ####"<<endl;
   mySsd->convertGlobalFrameToOther();
   int inactiveHit;
   if (g2t_ssd_hit)
     {
       inactiveHit = mySsd->removeInactiveHitInTable(g2t_ssd_hit);
     }
   else
     {
       if (g2t_svt_hit)
         {
           inactiveHit = mySsd->removeInactiveHitInTable(g2t_svt_hit);
         }
     }    
   cout<<"####    ->   "<<inactiveHit<<" DEAD ZONE HITS REMOVED      ####"<<endl;
   mySsd->chargeSharingOverStrip(ctrl);
   int nSsdStrips = mySsd->writeStripToTable(sls_strip);
   sls_strip->Purge();
   cout<<"####    -> "<<nSsdStrips<<" FIRED STRIPS INTO TABLE     ####"<<endl;
   cout<<"####        END OF SSD LAZY SIMULATOR        ####"<<endl;
   cout<<"#################################################"<<endl;
   delete mySsd;
   if (nSsdStrips) res = kStOK;

   if(res!=kStOK){
     gMessMgr->Warning("St_sls_Maker: no output");
     return kStWarn;
   }
   if(Debug())  gMessMgr->Debug() << "In St_sls_Maker::Make() ... "<< GetName() << endm;
  return kStOK;
}
//_____________________________________________________________________________
void St_sls_Maker::PrintInfo() {
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
Int_t St_sls_Maker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In St_sls_Maker::Finish() ... "
                               << GetName() << endm; 
  return kStOK;
}



