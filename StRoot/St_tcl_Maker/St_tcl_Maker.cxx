// $Id: St_tcl_Maker.cxx,v 1.26 1999/03/03 00:29:04 sakrejda Exp $
// $Log: St_tcl_Maker.cxx,v $
// Revision 1.26  1999/03/03 00:29:04  sakrejda
// size of the tables reduced following wasted space diagnostics
//
// Revision 1.25  1999/03/02 19:50:42  sakrejda
// Histograms cleaned up
//
// Revision 1.24  1999/03/01 18:53:32  sakrejda
// hit eveluation switchable
//
// Revision 1.23  1999/02/27 23:10:48  sakrejda
// auxiliary hit table eliminated
//
// Revision 1.22  1999/02/26 17:25:30  kathy
// fix histograms
//
// Revision 1.21  1999/02/25 03:36:05  sakrejda
// Threshold lowered, was set for the test data
//
// Revision 1.20  1999/02/19 16:30:25  fisyak
// sanitary check
//
// Revision 1.19  1999/02/16 01:53:57  fisyak
// Make sure that tfs does not run if there tss
//
// Revision 1.18  1999/02/10 20:57:39  kathy
// added histograms to Maker
//
// Revision 1.17  1999/01/20 23:59:56  fisyak
// Just clean up
//
// Revision 1.16  1999/01/08 23:18:30  sakrejda
// index  table created only once and only for the mc run
//
// Revision 1.15  1999/01/02 19:08:22  fisyak
// Add ctf
//
// Revision 1.14  1998/12/18 18:37:16  fisyak
// account module changes
//
// Revision 1.13  1998/12/16 22:19:19  fisyak
// New tfs
//
// Revision 1.12  1998/12/04 15:31:50  fisyak
// Add g2t_vertex for tcl
//
// Revision 1.11  1998/10/31 00:26:22  fisyak
// Makers take care about branches
//
// Revision 1.10  1998/10/06 18:00:48  perev
// cleanup
//
// Revision 1.9  1998/09/18 14:35:31  fisyak
// Fix makers
//
// Revision 1.8  1998/09/15 20:55:27  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.7  1998/08/26 12:15:10  fisyak
// Remove asu & dsl libraries
//
// Revision 1.6  1998/08/18 14:05:04  fisyak
// Add to bfc dst
//
// Revision 1.5  1998/08/14 15:25:41  fisyak
// Move out tpg from run
//
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
#include "St_DataSetIter.h"
#include "St_XDFFile.h"
#include "tpc/St_tpg_main_Module.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_xyz_newtab_Module.h"
#include "tpc/St_tte_hit_match_Module.h"
#include "tpc/St_tfs_g2t_Module.h"
#include "tpc/St_tfs_filt_Module.h"
#include "TH1.h"

ClassImp(St_tcl_Maker)
  
  //_____________________________________________________________________________
  St_tcl_Maker::St_tcl_Maker(const char *name, const char *title):
    StMaker(name,title){
  drawinit=kFALSE;
}
//_____________________________________________________________________________
St_tcl_Maker::~St_tcl_Maker(){
}
//_____________________________________________________________________________
Int_t St_tcl_Maker::Init(){
  // Create tables
  St_DataSetIter       local(gStChain->DataSet("params"));
  // geometry parameters
  St_DataSet *tpc = local("tpc");
  if (! tpc)  tpc = local.Mkdir("tpc");
  St_DataSet *tpgpar = local("tpc/tpgpar");
  if (tpgpar){
    St_DataSetIter partable(tpgpar);
    m_tpg_pad_plane = (St_tpg_pad_plane *) partable("tpg_pad_plane");
    m_tpg_detector  = (St_tpg_detector  *) partable("tpg_detector");
    if (!(m_tpg_pad_plane && m_tpg_detector)) {
      cout << "TPC geometry parameter tables are incomplete."<< endl;
      SafeDelete(tpgpar);
    }
    m_tpg_pad       = (St_tpg_pad       *) partable("tpg_pad");
    if (!m_tpg_pad) {
      m_tpg_pad       = new St_tpg_pad("tpg_pad",1); partable.Add(m_tpg_pad);
    }
    Int_t res = tpg_main(m_tpg_pad_plane,m_tpg_detector,m_tpg_pad); 
  }
  // tcl parameters
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
   St_DataSet *tsspars = local("tpc/tsspars");
   if (tsspars){
       St_DataSetIter partable(tsspars);
       m_tsspar = (St_tss_tsspar *) partable("tsspar");
       if (!m_tsspar) {
         cout << "TPC tss parameter tables are incomplete."<< endl;  
         SafeDelete(tsspars);
       }
       else {
	 tss_tsspar_st *tsspar = m_tsspar->GetTable();
         tsspar->threshold=1;
       }
   }
  // tfs parameters
  St_DataSet *tfspars = local("tpc/tfspars");
  if (tfspars){
    m_tfs_fspar = (St_tfs_fspar *) local("tpc/tfspars/tfs_fspar");
    m_tfs_fsctrl= (St_tfs_fsctrl*) local("tpc/tfspars/tfs_fsctrl");
  }
  // Create Histograms

  // for tph pam
  m_nseq_hit   = new TH1F("TclTphitNseq","num seq in hit",200,0.5,200.5);
  m_tpc_row    = new TH1F("TclTphitRow","tpc row num",2345,100.5,2445.5);
  m_x_of_hit   = new TH1F("TclTphitHitX","x dist of hits",50,-200.,200.);
  m_y_of_hit   = new TH1F("TclTphitHitY","y dist of hits",50,-200.,200.);
  m_z_of_hit   = new TH1F("TclTphitHitZ","z dist of hits",50,-250.,250.);
  m_charge_hit = new TH1F("TclTphitTotChargeHit","total charge in hit",100,0.,0.00004);

  // The following quantities are known only AFTER tracking
  //  m_alpha      = new TH1F("TclTphitAlpha","crossing angle in xy",50,-200.,200.);
  // m_phi        = new TH1F("TclTphitPhi","orientation of hit wrt padplane",64,0.,64.);
  // m_lambda     = new TH1F("TclTphitLambda","dip angle(radians)",64,0.,64.);

  // for tcl pam
  m_nseq_cluster = new TH1F("TclTpclusterNseqCluster"," num seq in cluster",100,0.5,200.5);
  m_nhits = new TH1F("TclTpclusterNhits"," estimated num overlapping hits in cluster",20,-0.5,19.5);
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_tcl_Maker::Make(){
  //  PrintInfo();
  const Int_t max_hit = 400000;
  St_DataSetIter local(m_DataSet);
  if (!m_DataSet->GetList()) {// If DataSet list empty then create it
    St_tcl_tphit     *tphit     = new St_tcl_tphit("tphit",max_hit);         local.Add(tphit);
    St_tcl_tphit_aux *tphitau   = new St_tcl_tphit_aux("tphitau",1);   local.Add(tphitau);
    //    St_tcl_tpc_index *index     = new St_tcl_tpc_index("index",max_hit);     local.Add(index);
    St_tcl_tpcluster *tpcluster = new St_tcl_tpcluster("tpcluster",max_hit); local.Add(tpcluster);
    St_tcl_tp_seq    *tpseq     = new St_tcl_tp_seq("tpseq",4*max_hit);      local.Add(tpseq);
    St_DataSet       *sector;
    St_DataSet       *raw_data_tpc = gStChain->DataSet("tpc_raw");
    Int_t sector_tot = 0;
    if (raw_data_tpc){// Row data exits -> make clustering
      St_DataSetIter next(raw_data_tpc);
      St_raw_sec_m  *raw_sec_m = (St_raw_sec_m *) next("raw_sec_m");
      while (sector=next()){// loop over sectors
	Char_t *name= 0;
	if (name = strstr(sector->GetName(),"Sector")) {
	  // look for the sector number
	  name  = strchr(name,'_')+1;
	  Int_t indx = atoi(name);
	  if (gStChain->Debug()) printf(" Sector = %d \n", indx);
	  tcl_sector_index_st *tcl_sector_index = m_tcl_sector_index->GetTable();
	  m_tcl_sector_index->SetNRows(1);
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
			       m_tpg_pad_plane,
			       pixel_data_in,pixel_data_out,
			       tpseq,tpcluster,tphit,tphitau);
	}
      }
      if (sector_tot && m_tclEvalOn) { //slow simulation exist
	cout << "start run_tte_hit_match" << endl;
	St_DataSetIter geant(gStChain->DataSet("geant"));
	St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geant("g2t_tpc_hit");
        if (g2t_tpc_hit){//geant data exists too
	// create the index table, if any
	St_tcl_tpc_index  *index = (St_tcl_tpc_index *) local("index");
	if (!index) {index = new St_tcl_tpc_index("index",2*max_hit); local.Add(index);}
	
	Int_t Res_tte =  tte_hit_match(g2t_tpc_hit,index,m_type,tphit); 
        if (Res_tte !=  kSTAFCV_OK)  cout << "Problem with tte_hit_match.." << endl;
	cout << "finish run_tte_hit_match" << endl;
	}
      }
    }
    else {
    // Row data does not exit, check GEANT. if it does then use fast cluster simulation
      St_DataSetIter geant(gStChain->DataSet("geant"));
      St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geant("g2t_tpc_hit");
      St_g2t_track   *g2t_track   = (St_g2t_track   *) geant("g2t_track");
      St_g2t_vertex  *g2t_vertex  = (St_g2t_vertex  *) geant("g2t_vertex");
      if (g2t_tpc_hit && g2t_track){
	// create the index table, if any
	St_tcl_tpc_index  *index = (St_tcl_tpc_index *) local("index");
	if (!index) {index = new St_tcl_tpc_index("index",2*max_hit); local.Add(index);}
	cout << "start tfs_run" << endl;
	Int_t Res_tfs_g2t =   tfs_g2t(g2t_tpc_hit, g2t_track, g2t_vertex,
				      m_tfs_fspar,m_tfs_fsctrl,
				      index, m_type, tphit);
	if (Res_tfs_g2t !=  kSTAFCV_OK){cout << "Problem running tfs_g2t..." << endl;}
	else {
	  Int_t Res_tfs_filt = tfs_filt(tphit);
	  if ( Res_tfs_filt !=  kSTAFCV_OK){cout << " Problem running tfs_filt..." << endl;} 
	}
	cout << "finish tfs_run" << endl;
      }
    }
  }
  //Histograms     
   MakeHistograms(); // clustering histograms
  return kStOK;
}
//_____________________________________________________________________________
void St_tcl_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_tcl_Maker.cxx,v 1.26 1999/03/03 00:29:04 sakrejda Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

//----------------------------------------------------------------------

void St_tcl_Maker::MakeHistograms() {
  // Create an iterator
  St_DataSetIter tpc_hits(m_DataSet);
  //Get the table:
  St_tcl_tphit *ptphh = 0;
  St_tcl_tpcluster *ptpcl =0;
  ptphh  = (St_tcl_tphit *) tpc_hits["tphit"];
  ptpcl  = (St_tcl_tpcluster *) tpc_hits["tpcluster"];

  //  cout << " **** NOW MAKING HISTOGRAMS FOR TCL !!!!! " << endl;
  if (ptphh) {
    tcl_tphit_st *r = ptphh->GetTable();
    for(Int_t i=0; i<ptphh->GetNRows();i++,r++){
      Float_t tphit_z      = r->z;
      Float_t tphit_x      = r->x;
      Float_t tphit_y      = r->y;
      Float_t tphit_lambda = r->lambda;
      Float_t tphit_alpha  = r->alpha;
      Float_t tphit_phi    = r->phi;
      Int_t tphit_nseq   = r->nseq;
      Int_t tphit_row    = r->row;
      Float_t tphit_q      = r->q;
        m_nseq_hit->Fill(tphit_nseq);
        m_tpc_row->Fill(tphit_row);
        m_x_of_hit->Fill(tphit_x);
        m_y_of_hit->Fill(tphit_y);
        m_z_of_hit->Fill(tphit_z);
        m_charge_hit->Fill(tphit_q);
	//        m_alpha->Fill(tphit_alpha);
	//        m_phi->Fill(tphit_phi);
	//        m_lambda->Fill(tphit_lambda);
    }
  }
  if (ptpcl) {
    tcl_tpcluster_st *r2 = ptpcl->GetTable();
    for(Int_t i=0; i<ptpcl->GetNRows();i++,r2++){
     Int_t tpcl_nseq = r2->nseq;
     Int_t tpcl_nhits = r2->nhits;
     m_nseq_cluster->Fill(tpcl_nseq);
     m_nhits->Fill(tpcl_nhits);
    }
  }

}

