// $Id: St_mwc_Maker.cxx,v 1.1 1999/01/14 19:11:01 druss Exp $
// $Log: St_mwc_Maker.cxx,v $
// Revision 1.1  1999/01/14 19:11:01  druss
// root Maker definitions/header for mwc
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
// St_mwc_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_mwc_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "mwc/St_mwg_Module.h"
#include "mwc/St_mws_Module.h"
#include "mwc/St_mwu_Module.h"
ClassImp(St_mwc_Maker)

//_____________________________________________________________________________
St_mwc_Maker::St_mwc_Maker(const char *name, const char *title):StMaker(name,title){
   printf(" ----- Welcome to St_mwc_Maker -----\n");
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_mwc_Maker::~St_mwc_Maker(){
}
//_____________________________________________________________________________
Int_t St_mwc_Maker::Init(){
// Create tables
//     first Create parameter tables:

   printf(" ----- Welcome to St_mwc_Maker::Init -----\n");
   St_DataSetIter       params(gStChain->DataSet("params"));
   m_geom = ( St_mwc_geo *)  params("mwc/mwcpars/geom");
   m_cal  = ( St_mwc_cal *)  params("mwc/mwcpars/cal");
   m_mpar = ( St_mwc_mpar *) params("mwc/mwcpars/mpar");
   printf(" --- tables defined\n");

   if (!m_geom){
     printf("-- resetting geom\n");
     m_geom = new St_mwc_geo("geom",1);
     printf("-- adding geom\n");
     params("mwc")->Add(m_geom);
     printf("-- done with adding geom\n ");
   }
   if (!m_cal){
      printf("-- resetting cal\n");
      m_cal = new St_mwc_cal("cal",1);
      params("mwc")->Add(m_cal);
   }
   if (!m_mpar){
     printf("-- resetting mpar\n");
     m_mpar = new St_mwc_mpar("mpar",1);
     params("mwc")->Add(m_mpar);
   }
   printf("getting geom table\n");
   mwc_geo_st  *geom = m_geom->GetTable();
//   mwc_cal_st  *cal  - m_cal->GetTable();
   printf("getting param table\n");
   mwc_mpar_st *mpar = m_mpar->GetTable();

   printf("setting geom table\n");
   geom->init  = 0;
   geom->neta  = 16;
   geom->nphi  = 12;
   geom->r1max = 118.669;
   geom->r1min = 54.669;
   geom->r2max = 189.488;
   geom->r2min = 125.488;

//   cal->cc     = 1.00;
   printf("setting param table\n");  
   mpar->gain            = 1.0;     
   mpar->de_thresh       = 0.0;     
   mpar->tof_thresh      = 0.0;      
   mpar->num_counts_out  = 384.0;
   mpar->num_wires_count = 20.0;
   mpar->wires           = 1;

// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_mwc_Maker::Make(){
   printf(" ----- Welcome to St_mwc_Maker::Make -----\n");
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it

// Create Empty tables for us
     St_mwc_mevent *mevent = new St_mwc_mevent("mevent",400);
     St_mwc_sector *sector = new St_mwc_sector("sector",384);
     St_mwc_raw    *raw    = new St_mwc_raw("raw",384);
     St_mwc_cor    *cor    = new St_mwc_cor("cor",384);

     m_DataSet->Add(mevent);
     m_DataSet->Add(sector);
     m_DataSet->Add(raw);
     m_DataSet->Add(cor);

// Read in Geant Tables
     St_DataSetIter geant(gStChain->DataSet("geant"));
     St_g2t_mwc_hit *g2t_mwc_hit = (St_g2t_mwc_hit *) geant("g2t_mwc_hit");
     printf ("checking some values \n");
     g2t_mwc_hit_st *g2t_mwc = g2t_mwc_hit->GetTable();
     printf("id: %i, p[0]: %f, p[1]: %f, p[2]: %f \n",g2t_mwc->id,
            g2t_mwc->p[0],g2t_mwc->p[1],g2t_mwc->p[2]);
     printf ("------------ Calling mws -------\n");
     Int_t mwc_result = mws(
                            g2t_mwc_hit,
                            m_geom,
                            m_mpar,
                            mevent,
                            sector,
                            raw);
     if (mwc_result != kSTAFCV_OK)
     {
        printf("**** Problems with mwc ****\n");
        return kStErr;
     }

}
 return kStOK;
}
//_____________________________________________________________________________
void St_mwc_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_mwc_Maker.cxx,v 1.1 1999/01/14 19:11:01 druss Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

