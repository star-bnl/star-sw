// $Id: St_ems_Maker.cxx,v 1.1 1998/11/30 21:18:30 fisyak Exp $
// $Log: St_ems_Maker.cxx,v $
// Revision 1.1  1998/11/30 21:18:30  fisyak
// ems raw data Maker
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:29  perev
// cleanup
//
// Revision 1.5  1998/10/02 13:46:08  fine
// DataSet->DataSetIter
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_ems_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "St_ems_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "emc/St_ems_interface2_Module.h"
#include "emc/St_dep_e_toadc_Module.h"
ClassImp(St_ems_Maker)

//_____________________________________________________________________________
St_ems_Maker::St_ems_Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_ems_Maker::~St_ems_Maker(){
}
//_____________________________________________________________________________
Int_t St_ems_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->DataSet("params"));
   m_ems_control = (St_ems_control *) local("emc/ems/ems_control");
   St_DataSetIter       geom(gStChain->DataSet("geom"));
   m_calb_calg   = (St_calb_calg   *) geom("calb_calg");
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_ems_Maker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
    St_ems_hits *ems_hits_bemc = new St_ems_hits("ems_hits_bemc", 9600);m_DataSet->Add(ems_hits_bemc);
    St_ems_hits *ems_hits_bsmd = new St_ems_hits("ems_hits_bsmd",18000);m_DataSet->Add(ems_hits_bsmd);
    St_ems_hits *ems_hits_eemc = new St_ems_hits("ems_hits_eemc",10000);m_DataSet->Add(ems_hits_eemc);
    St_ems_hits *ems_hits_esmd = new St_ems_hits("ems_hits_esmd",20000);m_DataSet->Add(ems_hits_esmd);
    
    St_emc_hits *emc_hits_bemc  = new St_emc_hits("emc_hits_bemc",4800); m_DataSet->Add(emc_hits_bemc);
    St_emc_hits *emc_hits_bprs  = new St_emc_hits("emc_hits_bprs",4800); m_DataSet->Add(emc_hits_bprs);
    St_emc_hits *emc_hits_bsmde = new St_emc_hits("emc_hits_bsmde",5000);m_DataSet->Add(emc_hits_bsmde);
    St_emc_hits *emc_hits_bsmdp = new St_emc_hits("emc_hits_bsmdp",5000);m_DataSet->Add(emc_hits_bsmdp);
    St_emc_hits *emc_hits_eemc  = new St_emc_hits("emc_hits_eemc",1440);m_DataSet->Add(emc_hits_eemc);
    St_emc_hits *emc_hits_eprs  = new St_emc_hits("emc_hits_eprs",1440);m_DataSet->Add(emc_hits_eprs);
    St_emc_hits *emc_hits_esmde = new St_emc_hits("emc_hits_esmde",5000);m_DataSet->Add(emc_hits_esmde);
    St_emc_hits *emc_hits_esmdp = new St_emc_hits("emc_hits_esmdp",5000);m_DataSet->Add(emc_hits_esmdp);

    St_DataSetIter geant(gStChain->DataSet("geant"));
    St_g2t_event  *g2t_event  = (St_g2t_event  *) geant("g2t_event");
    St_g2t_vertex *g2t_vertex = (St_g2t_vertex *) geant("g2t_vertex");
    St_g2t_track  *g2t_track  = (St_g2t_track  *) geant("g2t_track");
    St_g2t_emc_hit *g2t_emc_hit = (St_g2t_emc_hit *) geant("g2t_emc_hit");
    if (!g2t_emc_hit) {g2t_emc_hit = new St_g2t_emc_hit("g2t_emc_hit",1);}
    St_g2t_emc_hit *g2t_smd_hit = (St_g2t_emc_hit *) geant("g2t_smd_hit");
    if (!g2t_smd_hit) {g2t_smd_hit = new St_g2t_emc_hit("g2t_smd_hit",1);}
    St_g2t_emc_hit *g2t_eem_hit = (St_g2t_emc_hit *) geant("g2t_eem_hit");
    if (!g2t_eem_hit) {g2t_eem_hit = new St_g2t_emc_hit("g2t_eem_hit",1);}
    St_g2t_emc_hit *g2t_esm_hit = (St_g2t_emc_hit *) geant("g2t_esm_hit");
    if (!g2t_esm_hit) {g2t_esm_hit = new St_g2t_emc_hit("g2t_esm_hit",1);}

    Int_t Res_ems =  ems_interface2 (
			g2t_event,
                        g2t_vertex,
                        g2t_track,
                        g2t_emc_hit,
                       	g2t_smd_hit,
                       	g2t_eem_hit,
                        g2t_esm_hit,
                        m_calb_calg,
                        m_ems_control,
                        ems_hits_bemc,
                        ems_hits_bsmd,
                        ems_hits_eemc,
                        ems_hits_esmd);
    if (Res_ems != kSTAFCV_OK) {
      cout << "***** Problem with ems_interface *****" << endl;
	  }

}
 return kStOK;
}
//_____________________________________________________________________________
void St_ems_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_ems_Maker.cxx,v 1.1 1998/11/30 21:18:30 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

