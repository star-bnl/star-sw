// $Id: St_mwc_Maker.cxx,v 1.2 1999/01/29 22:55:19 druss Exp $
// $Log: St_mwc_Maker.cxx,v $
// Revision 1.2  1999/01/29 22:55:19  druss
// corrected bugs, included new parameters from mwc parameter table
//
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
   printf(" d.e. russ:1/29/99 \n");
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

   // set pointers to table wrappers

   m_geom = new St_mwc_geo("geom",1);
   m_cal  = new St_mwc_cal("cal",1);
   m_mpar = new St_mwc_mpar("mpar",1);

   // create tables

   mwc_geo_st   geom;
   mwc_cal_st   cal;
   mwc_mpar_st  mpar;

   // setting table values

   printf("setting geom table\n");
   geom.init  = 0;
   geom.neta  = 16;
   geom.nphi  = 12;
   geom.r1max = 118.669;
   geom.r1min = 54.669;
   geom.r2max = 189.488;
   geom.r2min = 125.488;
   m_geom->AddAt(&geom,0);

   printf("setting cal table\n");
   cal.cc     = 1.00;
   m_cal->AddAt(&cal,0);

   printf("setting param table\n");  
   mpar.gain            = 11.0;     
   mpar.de_thresh_in    = 2.0e-8;     
   mpar.de_thresh_out   = 3.0e-8;
   mpar.tof_thresh      = 0.0;      
   mpar.num_counts_out  = 96.0;
   mpar.num_wires_count = 80.0;
   mpar.el_noise_width  = 0.00;
   mpar.min_ion         = 0.00;
   mpar.wires           = 1;
   m_mpar->AddAt(&mpar,0);

   // check tables
   
   m_geom->ls("*");
   m_cal ->ls("*");
   m_mpar->ls("*");
   

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

     printf ("checking if the tables exist \n");
     if (!g2t_mwc_hit) {printf("g2t_mwc_hit does not exist\n");return kStErr;}
     if (!m_geom)      {printf("m_geom does not exist\n")     ;return kStErr;}
     if (!m_mpar)      {printf("m_mpar does not exist\n")     ;return kStErr;} 
     if (!mevent)      {printf("mevent does not exist\n")     ;return kStErr;}
     if (!sector)      {printf("sector does not exist\n")     ;return kStErr;} 
     if (!raw)         {printf("raw does not exist\n")        ;return kStErr;} 

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
  printf("* $Id: St_mwc_Maker.cxx,v 1.2 1999/01/29 22:55:19 druss Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}
     
