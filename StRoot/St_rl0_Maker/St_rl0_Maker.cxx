//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_rl0_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_rl0_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "trg/St_rl0_Module.h"


ClassImp(St_rl0_Maker)

//_____________________________________________________________________________
St_rl0_Maker::St_rl0_Maker(const char *name):StMaker(name){
}
//_____________________________________________________________________________
St_rl0_Maker::~St_rl0_Maker(){
}
//_____________________________________________________________________________
Int_t St_rl0_Maker::Init(){

   // read in CTB parameters from parameter table.

   St_DataSetIter       params(GetDataBase("params"));
   m_ctb        = (St_ctg_geo      *) params("ctf/ctg/ctb");

   // set pointers to table wrappers

   m_rl0_ctrl   = new St_rl0_ctrl("ctr",1);      // rl0 control table
   m_rl0_ctbcal = new St_rl0_ctbcal("ctbcal",1);  // rl0 ctb calibration
   m_rl0_mwccal = new St_rl0_mwccal("mwccal",1);  // rl0 mwc calibration

   m_geom = new St_mwc_geo("geom",1);
   
   // create tables

   rl0_ctrl_st   ctr;
   rl0_mwccal_st mwccal;
   rl0_ctbcal_st ctbcal;
   mwc_geo_st   geom;

   // setting ctr table

   ctr.ctb_eta_pixels = 4;
   ctr.ctb_phi_pixels = 4;
   ctr.mwc_eta_pixels = 4;
   ctr.mwc_phi_pixels = 4;
   ctr.nthres         = 10;
   ctr.fpga_gain[0]   = 1.0;
   ctr.zdc_cut        = 1;
   m_rl0_ctrl->AddAt(&ctr,0);

   // setting mwccal table

   mwccal.fit[0]     = 0.4;
   mwccal.fit[1]     = 0.4;
   m_rl0_mwccal->AddAt(&mwccal,0);

 // setting ctbcal table

   ctbcal.fit[0]     = 20;
   ctbcal.fit[1]     = 15;
   m_rl0_ctbcal->AddAt(&ctbcal,0);

 // Setting MWC PARAMETER TABLE -- This is BAD  Should be in DBASE!!
   
   geom.init  = 0;
   geom.neta  = 4;
   geom.nphi  = 12;
   geom.r1max = 118.669;
   geom.r1min = 54.669;
   geom.r2max = 189.488;
   geom.r2min = 125.488;
   m_geom->AddAt(&geom,0); 

   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_rl0_Maker::Make(){

// Create Empty tables for us

   St_rl0_data   *rl0_data = new St_rl0_data("rl0_data",1);
   m_DataSet->Add(rl0_data);

// Read in MWC Tables

   St_DataSetIter mwc(GetDataSet("mwc"));
   St_mwc_raw     *rawmwc = (St_mwc_raw *) mwc("raw");
   St_mwc_sector  *sector = (St_mwc_sector *) mwc("sector");

// Read in CTB Tables

   St_DataSetIter ctf(GetDataSet("ctf"));
   St_ctu_raw     *rawctb = (St_ctu_raw *) ctf("ctb_raw");

// Checking if the tables exist

   if (!m_ctb)        {printf("m_ctb does not exist\n")     ;return kStWarn;}
   if (!m_geom)       {printf("m_geom does not exist\n")    ;return kStWarn;}
   if (!rawctb)       {printf("rawctb does not exist\n")    ;return kStWarn;} 
   if (!sector)       {printf("sector does not exist\n")    ;return kStWarn;} 
   if (!rawmwc)       {printf("rawmwc does not exist\n")    ;return kStWarn;} 
   if (!m_rl0_ctrl)   {printf("rl0_ctrl does not exist\n")  ;return kStWarn;} 
   if (!rl0_data)     {printf("rl0_data does not exist\n")  ;return kStWarn;} 
   if (!m_rl0_ctbcal) {printf("rl0_ctcal does not exist\n") ;return kStWarn;} 
   if (!m_rl0_mwccal) {printf("rl0_mwcal does not exist\n") ;return kStWarn;} 

   Int_t rl0_result = rl0(
                          m_ctb,
                          m_geom,
                          rawctb,
                          sector,
                          rawmwc,
			  m_rl0_ctrl,
			  rl0_data,
			  m_rl0_ctbcal,
			  m_rl0_mwccal);
   if (rl0_result != kSTAFCV_OK)
   {
      printf("**** Problems with rl0 ****\n");
      return kStWarn;
   }

 return kStOK;
}
//_____________________________________________________________________________
     





