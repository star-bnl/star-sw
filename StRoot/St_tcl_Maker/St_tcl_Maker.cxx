// $Id: St_tcl_Maker.cxx,v 1.4 1998/08/10 02:34:34 fisyak Exp $
// $Log: St_tcl_Maker.cxx,v $
// Revision 1.4  1998/08/10 02:34:34  fisyak
// Add St_laser_Maker
//
// Revision 1.3  1998/08/07 19:34:55  fisyak
// Add St_run_Maker
//
// Revision 1.2  1998/07/21 01:04:39  fisyak
// Clean up
//
// Revision 1.1  1998/07/21 00:36:46  fisyak
// tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tcl_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include "St_tcl_Maker.h"
#include "StChain.h"
#include "St_DataSet.h"
#include "St_XDFFile.h"
#include "tpc/St_tpg_main_Module.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_xyz_newtab_Module.h"
ClassImp(St_tcl_Maker)

//_____________________________________________________________________________
St_tcl_Maker::St_tcl_Maker(){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_tcl_Maker::St_tcl_Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_tcl_Maker::~St_tcl_Maker(){
 if (m_DataSet) delete m_DataSet;
 m_DataSet = 0;
}
//_____________________________________________________________________________
void St_tcl_Maker::Clear(Option_t *option){
  if (m_DataSet) {delete m_DataSet; m_DataSet = 0;}
}

//_____________________________________________________________________________
void St_tcl_Maker::Finish(){ 
 Clear();
}
//_____________________________________________________________________________
void St_tcl_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->GetParams());
// geometry parameters
   St_DataSet *tpc = local("tpc");
   if (! tpc)  tpc = local.Mkdir("tpc");
   St_DataSet *tpgpar = local("tpc/tpgpar");
   if (tpgpar){
       St_DataSetIter partable(tpgpar);
       m_tpg_pad_plane = (St_tpg_pad_plane *) partable("tpg_pad_plane");
       m_tpg_detector  = (St_tpg_detector  *) partable("tpg_detector");
       m_tpg_pad       = (St_tpg_pad       *) partable("tpg_pad");
       if (!(m_tpg_pad_plane && m_tpg_detector && m_tpg_pad)) 
       printf("tpc/tpgpar is not initialized. Please add run_Maker to your chain\n");
   }
// tss parameters ?
   St_DataSet *tsspars = local("tpc/tsspars");
   if (tsspars){
       St_DataSetIter partable(tsspars);
       m_tsspar = (St_tss_tsspar *) partable("tsspar");
       if (!m_tsspar) 
       printf("tpc/tsspars is not initialized. Please add run_Maker to your chain\n");
   }
// clustering parameters
   St_DataSet *tclpars = local("tpc/tclpars");
   if (tclpars){
     St_DataSetIter partable(tclpars);
     m_tcl_sector_index = (St_tcl_sector_index *) partable("tcl_sector_index");
     m_tclpar           = (St_tcl_tclpar *) partable("tclpar");
     m_type             = (St_tcl_tpc_index_type *) partable("type");
     if (!(m_tcl_sector_index && m_tclpar && m_type)) {
       cout << " St_tcl_Maker:  clustering parameters have not been initialized" << endl;
       SafeDelete(tclpars);
     }
   }
// Create Histograms    
   StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_tcl_Maker::Make(){
  //  PrintInfo();
   St_DataSetIter tpc_data(m_DataSet);
   if (!m_DataSet->GetList()) {// If DataSet list empty then create it
     St_tcl_tphit  *tphit = (St_tcl_tphit *) tpc_data("tphit");
     if (!tphit) {tphit = new St_tcl_tphit("tphit",200000); tpc_data.Add(tphit);}
     St_tcl_tphit_aux  *tphitau = (St_tcl_tphit_aux *) tpc_data("tphitau");
     if (!tphitau) {tphitau = new St_tcl_tphit_aux("tphitau",200000); tpc_data.Add(tphitau);}
     //     St_tcl_tpc_index  *index = (St_tcl_tpc_index *) tpc_data("index");
     //     if (!index) {index = new St_tcl_tpc_index("index",200000); tpc_data.Add(index);}
     St_tcl_tpcluster  *tpcluster = (St_tcl_tpcluster *) tpc_data("tpcluster");
     if (!tpcluster) {tpcluster = new St_tcl_tpcluster("tpcluster",200000); tpc_data.Add(tpcluster);}
     St_tcl_tp_seq  *tpseq = (St_tcl_tp_seq *) tpc_data("tpseq");
     if (!tpseq) {tpseq = new St_tcl_tp_seq("tpseq",2000000); tpc_data.Add(tpseq);}
     St_DataSet   *sector;
//   St_DataSet *raw_data_tpc =  gStChain->Maker("tss_Maker")->DataSet();
     St_DataSetIter raw_data(gStChain->GetRawData());
     St_DataSet *raw_data_tpc = raw_data("tpc"); 
     if (raw_data_tpc){// Row data exits -> make clustering
       St_DataSetIter next(raw_data_tpc);
       St_raw_sec_m  *raw_sec_m = (St_raw_sec_m *) next("raw_sec_m");
       Int_t sector_tot = 0;
       while (sector=next()){// loop over sectors
         Char_t *name= 0;
         if (name = strstr(sector->GetName(),"Sector")) {
       // look for the sector number
           name  = strchr(name,'_')+1;
           Int_t indx = atoi(name);
           if (gStChain->Debug()) printf(" Sector = %d \n", indx);
           tcl_sector_index_st *tcl_sector_index = m_tcl_sector_index->GetTable();
           tcl_sector_index->CurrentSector = indx;
           St_DataSetIter sect(sector);
           St_raw_row         *raw_row_in     = (St_raw_row *) sect("raw_row_in");
           St_raw_row         *raw_row_out    = (St_raw_row *) sect("raw_row_out");
           St_raw_pad         *raw_pad_in     = (St_raw_pad *) sect("raw_pad_in");
           St_raw_pad         *raw_pad_out    = (St_raw_pad *) sect("raw_pad_out");
           St_raw_seq         *raw_seq_in     = (St_raw_seq *) sect("raw_seq_in");
           St_raw_seq         *raw_seq_out    = (St_raw_seq *) sect("raw_seq_out");
           St_type_shortdata  *pixel_data_in  = (St_type_shortdata *) sect("pixel_data_in");
           St_type_shortdata  *pixel_data_out = (St_type_shortdata *) sect("pixel_data_out");
	// tcl
           Int_t tcl_res = tcl(m_tpg_pad_plane, m_tcl_sector_index, raw_sec_m,
                              raw_row_in, raw_pad_in, raw_seq_in, pixel_data_in,
                              raw_row_out,raw_pad_out,raw_seq_out,pixel_data_out,
                              tpcluster,tpseq);
           sector_tot++;
	// tph
           Int_t k = indx;
           if (sector_tot == 1) k = -k;
           tcl_sector_index->CurrentSector = k;
           Int_t tph_res =  tph(m_tcl_sector_index,m_tclpar,m_tsspar,
                                m_tpg_detector,m_tpg_pad_plane,
                                pixel_data_in,pixel_data_out,
                                tpseq,tpcluster,tphit,tphitau);
	 }
       }
     }
   }
//Histograms     
 return kSTAFCV_OK;
}
//_____________________________________________________________________________
void St_tcl_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_tcl_Maker.cxx,v 1.4 1998/08/10 02:34:34 fisyak Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

