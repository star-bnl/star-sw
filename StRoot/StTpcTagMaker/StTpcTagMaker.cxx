//*-- Author : Iwona Sakrejda
// 
// $Id: StTpcTagMaker.cxx,v 1.4 2000/05/25 00:35:36 sakrejda Exp $
// $Log: StTpcTagMaker.cxx,v $
// Revision 1.4  2000/05/25 00:35:36  sakrejda
// Copying dst_mon_soft_tpc to TpcTag table finished
//
// Revision 1.3  2000/05/24 04:41:00  sakrejda
// Body of the Make() method written
//
// Revision 1.2  2000/05/24 00:25:50  sakrejda
// Body of the Maker and the header cleaned up, comments added.
//
// Revision 1.1  2000/05/24 00:07:02  sakrejda
// Maker to fill TPC reconstruction quality flags created
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcTagMaker class for TPC Reconstruction Tags                      //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StTpcTagMaker.h"
#include "StChain.h"
#include "St_TpcTag_Table.h"
#include "tables/St_dst_mon_soft_tpc_Table.h"


ClassImp(StTpcTagMaker)

//_____________________________________________________________________________
StTpcTagMaker::StTpcTagMaker(const char *name):StMaker(name){
 //  StTpcTagMaker constructor
 //
 //  const char *name -  the name of this constructor
 //
}
//_____________________________________________________________________________
StTpcTagMaker::~StTpcTagMaker(){
  // StTpcTagMaker destructor
 //
}
//_____________________________________________________________________________
Int_t StTpcTagMaker::Init(){
 //  Init - is a first method the top level StChain calls to initialize all its makers
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StTpcTagMaker::Make(){
 //
 //  Make - this methoid is called in loop for each event
 //
 // Find dst_mon_soft_tpc

  St_DataSet *global =  GetInputDS("dst");
  if (!global) return 0;
  St_DataSetIter gime(global);
  St_dst_mon_soft_tpc *tpc_mon_data =(St_dst_mon_soft_tpc *) gime("mon_soft_tpc");
  dst_mon_soft_tpc_st *tpc_mon_data_st = (dst_mon_soft_tpc_st *)tpc_mon_data->GetTable();

  // Create a data set and add the table to it.
  St_TpcTag *tagtab= new St_TpcTag("TpcTag",1); m_DataSet->Add(tagtab);
  //
  TpcTag_st *tagtab_st = (TpcTag_st *) tagtab->GetTable();  
 
  // Fill the Tpc Tags

  tagtab_st->n_clus_tpc_tot = tpc_mon_data_st->n_clus_tpc_tot;
  for(Int_t ij=0;ij<24;ij++){
    tagtab_st->n_clus_tpc_in[ij] = tpc_mon_data_st->n_clus_tpc_in[ij];}
  for(Int_t ij=0;ij<24;ij++){
    tagtab_st->n_clus_tpc_out[ij] = tpc_mon_data_st->n_clus_tpc_out[ij];}
  tagtab_st->n_pts_tpc_tot = tpc_mon_data_st->n_pts_tpc_tot;
  for(Int_t ij=0;ij<24;ij++){
    tagtab_st->n_pts_tpc_in[ij] = tpc_mon_data_st->n_pts_tpc_in[ij];}
  for(Int_t ij=0;ij<24;ij++){
    tagtab_st->n_pts_tpc_out[ij] = tpc_mon_data_st->n_pts_tpc_out[ij];}
  tagtab_st->n_trk_tpc[0] = tpc_mon_data_st->n_trk_tpc[0];
  tagtab_st->n_trk_tpc[1] = tpc_mon_data_st->n_trk_tpc[1];
  for(Int_t ij=0;ij<10;ij++){
    tagtab_st->chrg_tpc_drift[ij] = tpc_mon_data_st->chrg_tpc_drift[ij];}
  tagtab_st->chrg_tpc_tot = tpc_mon_data_st->chrg_tpc_tot;
  for(Int_t ij=0;ij<24;ij++){
    tagtab_st->chrg_tpc_in[ij] = tpc_mon_data_st->chrg_tpc_in[ij];}
  for(Int_t ij=0;ij<24;ij++){
    tagtab_st->chrg_tpc_out[ij] = tpc_mon_data_st->chrg_tpc_out[ij];}
  tagtab_st->hit_frac_tpc[0] = tpc_mon_data_st->hit_frac_tpc[0];
  tagtab_st->hit_frac_tpc[1] = tpc_mon_data_st->hit_frac_tpc[1];
  tagtab_st->avg_trkL_tpc[0] = tpc_mon_data_st->avg_trkL_tpc[0];
  tagtab_st->avg_trkL_tpc[1] = tpc_mon_data_st->avg_trkL_tpc[1];
  tagtab_st->res_pad_tpc[0] = tpc_mon_data_st->res_pad_tpc[0];
  tagtab_st->res_pad_tpc[1] = tpc_mon_data_st->res_pad_tpc[1];
  tagtab_st->res_drf_tpc[0] = tpc_mon_data_st->res_drf_tpc[0];
  tagtab_st->res_drf_tpc[1] = tpc_mon_data_st->res_drf_tpc[1];
 return kStOK;
}











