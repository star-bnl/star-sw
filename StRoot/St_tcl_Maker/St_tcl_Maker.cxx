// $Id: St_tcl_Maker.cxx,v 1.79 2007/12/28 13:47:39 fisyak Exp $
// $Log: St_tcl_Maker.cxx,v $
// Revision 1.79  2007/12/28 13:47:39  fisyak
// Split tcl and tfs Makers
//
// Revision 1.78  2007/05/17 14:13:02  fisyak
// replace printf and  cout by logger printouts
//
// Revision 1.77  2007/04/28 17:57:12  perev
// Redundant StChain.h removed
//
// Revision 1.76  2006/10/17 20:17:18  fisyak
// remove direct filling of StEvent, StEvent is not ready yet
//
// Revision 1.74  2006/08/11 19:42:32  fisyak
// Comment #include St_XDFFile.h
//
// Revision 1.73  2004/06/05 23:39:44  fisyak
// Add (sector,row) for TpcCoordinate transformations
//
// Revision 1.72  2004/05/03 23:34:32  perev
// Possible non init WarnOff
//
// Revision 1.71  2004/01/14 22:29:35  fisyak
// Add IdTruth
//
// Revision 1.70  2003/09/02 17:59:31  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.69  2003/04/29 16:23:28  perev
// non TPCoriented cleanup
//
// Revision 1.68  2002/02/07 22:02:52  hardtke
// Give Init a return type -- make redhat 7.2 happy
//
// Revision 1.67  2002/02/05 22:21:56  hardtke
// Move Init code to InitRun
//
// Revision 1.66  2001/05/22 22:32:49  hardtke
// Add option for returning hits in global coordinates
//
// Revision 1.65  2001/02/13 21:38:16  genevb
// Separated TCL and TPH sector loops to reduce memory usage
//
// Revision 1.64  2000/08/22 00:17:54  hardtke
// Add ability to turn off either half of TPC:  new functions EastOff(), WestOff(), AllOn()
//
// Revision 1.63  2000/08/18 02:22:52  snelling
// changed default behaiviour of eval switch
//
// Revision 1.62  2000/08/10 03:49:40  snelling
// Added drift velocity output Info
//
// Revision 1.61  2000/06/26 22:34:38  snelling
// Removed generating adcxyz table with eval switch (used to much memory)
//
// Revision 1.60  2000/06/23 19:40:01  fisyak
// remove access to params
//
// Revision 1.59  2000/02/26 01:51:11  snelling
// clean up
//
// Revision 1.58  2000/02/25 17:57:05  fisyak
// Set proper include path
//
// Revision 1.57  2000/02/23 23:04:29  hardtke
// get tpg tables from tpcDB
//
// Revision 1.56  2000/02/08 15:12:48  love
// Make tcl_Maker abort empty events
//
// Revision 1.55  2000/02/01 18:49:54  love
// Protect against empty TPC data
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tcl_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "St_tcl_Maker.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
//#include "St_XDFFile.h"
#include "StMessMgr.h"
#include "tables/St_type_shortdata_Table.h"
#include "tables/St_tcl_tpcluster_Table.h"
#include "tables/St_tcl_tp_seq_Table.h"
#include "tables/St_tcc_morphology_Table.h"
#include "tables/St_tpg_pad_plane_Table.h"
#include "tables/St_tpg_detector_Table.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_xyz_newtab_Module.h"
#include "tpc/St_tte_hit_match_Module.h"
#include "StTpcDb/StTpcDb.h"
#include "StDbUtilities/StCoordinates.hh"
#include "TH1.h"

ClassImp(St_tcl_Maker)
  
//_____________________________________________________________________________
St_tcl_Maker::St_tcl_Maker(const char *name):
  StMaker(name),
  m_tclEvalOn(kFALSE),
  m_tclPixTransOn(kFALSE),
  m_tclMorphOn(kFALSE),
  bWriteTNtupleOn(kFALSE),
  m_EastOff(kFALSE),
  m_WestOff(kFALSE),
  m_GlobalHits(kFALSE) {
}

//_____________________________________________________________________________
St_tcl_Maker::~St_tcl_Maker() {
}
//____________________________________________________________________________
Int_t St_tcl_Maker::Init() {
  m_tcl_sector_index = new St_tcl_sector_index("tcl_sector_index",1);
  m_tcl_sector_index->SetNRows(1); 
  AddConst(m_tcl_sector_index);

  return StMaker::Init();
}
//_____________________________________________________________________________

Int_t St_tcl_Maker::InitRun(int runnumber) {

  // Limit Error Messages
  gMessMgr->SetLimit("TPSEQ",10);
  gMessMgr->SetLimit("TPHAM",10);
  gMessMgr->SetLimit("TCL_Get_Row_Seq-E2",5);

  // set bools
  if (m_tclEvalOn) {
    m_tclPixTransOn = kFALSE;
    m_tclMorphOn = kTRUE;
    bWriteTNtupleOn = kFALSE; 
  }

  // 		Create tables
  St_DataSet *tpc = GetDataBase("tpc");
  assert(tpc);

  // 		geometry parameters

  m_tpg_pad_plane = NULL;
  m_tpg_pad_plane = (St_tpg_pad_plane *) GetDataSet("tpcDB/.const/tpg_pad_plane");

  m_tpg_detector  = NULL;
  m_tpg_detector  = (St_tpg_detector  *) GetDataSet("tpcDB/.const/tpg_detector");
  assert ((m_tpg_pad_plane && m_tpg_detector)) ;

  // 		TCL parameters
  St_DataSet *tclpars = tpc->Find("tclpars");
  assert(tclpars);

  
  m_tclpar           = NULL;
  m_tclpar           = (St_tcl_tclpar *) tclpars->Find("tclpar");
  m_type             = NULL;
  m_type             = (St_tcl_tpc_index_type *) tclpars->Find("type");
  assert(m_tclpar && m_type);

  // 		TSS parameters
  St_DataSet *tsspars = tpc->Find("tsspars");
  assert(tsspars);

  m_tsspar = NULL;
  m_tsspar = (St_tss_tsspar *) tsspars->Find("tsspar");
  assert(m_tsspar); 
  tss_tsspar_st *tsspar = m_tsspar->GetTable();
  tsspar->threshold = 1;

  //		Histograms     
  InitHistograms(); // book histograms
  return kStOK;

}

//_____________________________________________________________________________

Int_t St_tcl_Maker::Make() {

  //  m_DataSet is global pointer from StMaker to data set

  if (Debug()) {gMessMgr->QAInfo() << Form("Start of TCL Maker") << endm;}

  // initialize pointers to tables
  tpseq = NULL;
  tphit = NULL;
  tpcluster = NULL;
  morph = NULL;
  index = NULL;
  St_DataSet* sector;
  St_DataSet* raw_data_tpc = GetInputDS("tpc_raw");
  m_raw_data_tpc = kFALSE;
  Int_t sector_tot = 0;

  // write out the drift velocity
  gMessMgr->Info() << "Drift velocity used: " << gStTpcDb->DriftVelocity() 
		   << endm;
 
  if (! raw_data_tpc) {Warning("Make","Raw data does not exist)"); return kStWarn;}
  // Raw data exists -> make clustering
  m_raw_data_tpc = kTRUE;
  St_DataSetIter next(raw_data_tpc);
  St_raw_sec_m  *raw_sec_m = (St_raw_sec_m *) next("raw_sec_m");
  //Get the adcxyz table
  St_tfc_adcxyz *adcxyz = (St_tfc_adcxyz *) next("adcxyz");
  
  //counters for calculating size tables
  Int_t isumpix = 0;
  Int_t isumseq = 0;
  
  while ((sector=next())) {// loop over sectors
    const Char_t *name= 0;
    if ((name = strstr(sector->GetName(),"Sector"))) {
      // look for the sector number
      name  = strchr(name,'_')+1; Int_t indx = atoi(name);
      if (Debug()) {gMessMgr->QAInfo() << Form(" Sector = %d \n", indx) << endm;}
      St_DataSetIter sect(sector);
      St_type_shortdata  *pixel_data_in  = (St_type_shortdata *) sect("pixel_data_in");
      St_type_shortdata  *pixel_data_out = (St_type_shortdata *) sect("pixel_data_out");
      Int_t ipin = pixel_data_in->GetNRows();
      Int_t ipout = pixel_data_out->GetNRows();
      isumpix += ipin + ipout;
      if (Debug()) {gMessMgr->QAInfo()  << "Total number of pixels, " << isumpix << endm;}
      St_raw_seq  *raw_seq_in  = (St_raw_seq *) sect("raw_seq_in");
      St_raw_seq  *raw_seq_out = (St_raw_seq *) sect("raw_seq_out");
      Int_t nseqin = raw_seq_in->GetNRows();
      Int_t nseqout = raw_seq_out->GetNRows();
      isumseq += nseqin + nseqout;
      if (Debug()) {gMessMgr->QAInfo()  << "Total number of sequences, " << isumseq << endm;}
    }
  }
  
  //calculate or estimate the size before creating the tables
  if (Debug()) {gMessMgr->QAInfo()  << "making tcl_tp table with " << isumseq << " entries" << endm;}
  tpseq = new St_tcl_tp_seq("tpseq",isumseq);      
  m_DataSet->Add(tpseq);

  // WARNING no knowledge of actual number of hits here but we create a
  // table smart enough to contain biggest events. Try max number of hits
  // equal to number of pixels divided by 10 (no 1 pad hits and estimated
  // sequence length to be 5)
  int max_hit = (int) ceil((float)(isumpix/10));
  // We have to protect against DAQ event records that say there is TPC data
  // when its empty.
  if (isumpix < 1) {
    Warning ("Make"," TPC data is empty, isumpix=%d dump event.",isumpix);
    return kStWarn;
  }
  gMessMgr->Info() << "number of estimated hits used: " << max_hit 
		   << endm;
  // create tables used with a reasonable size
  tpcluster = new St_tcl_tpcluster("tpcluster",max_hit); 
  m_DataSet->Add(tpcluster);
  
  if(!morph && m_tclMorphOn) {
    // UW morphology study 
    morph = new St_tcc_morphology("morph",max_hit);    
    m_DataSet->Add(morph);
  }
  
  if (!adcxyz && m_tclPixTransOn) {  
    // create flat adcxyz Table for pixel viewing
    gMessMgr->Info() << "making adcxyz table with " << isumpix 
		     << " entries" << endm;
    adcxyz = new St_tfc_adcxyz("adcxyz",isumpix);  
    m_DataSet->Add(adcxyz);
    adcxyz->SetNRows(0);
  }
  // end creation tables
  
  next.Reset();
  
  while ((sector=next())) {  // loop over sectors for TCL
    const  Char_t *name = 0;
    if ((name = strstr(sector->GetName(),"Sector"))) {
      // look for the sector number
      name  = strchr(name,'_') + 1; 
      Int_t indx = atoi(name);
      if (Debug()) {gMessMgr->QAInfo() << Form(" Sector = %d \n", indx) << endm;}
      tcl_sector_index_st *tcl_sector_index = m_tcl_sector_index->GetTable();
      m_tcl_sector_index->SetNRows(1);
      tcl_sector_index->CurrentSector = indx;
      if (m_EastOff&&indx>12) continue;
      if (m_WestOff&&indx<=12) continue;
      St_DataSetIter sect(sector);
      St_raw_row         *raw_row_in     = (St_raw_row *) sect("raw_row_in");
      St_raw_row         *raw_row_out    = (St_raw_row *) sect("raw_row_out");
      St_raw_pad         *raw_pad_in     = (St_raw_pad *) sect("raw_pad_in");
      St_raw_pad         *raw_pad_out    = (St_raw_pad *) sect("raw_pad_out");
      St_raw_seq         *raw_seq_in     = (St_raw_seq *) sect("raw_seq_in");
      St_raw_seq         *raw_seq_out    = (St_raw_seq *) sect("raw_seq_out");
      St_type_shortdata  *pixel_data_in  = (St_type_shortdata *) sect("pixel_data_in");
      St_type_shortdata  *pixel_data_out = (St_type_shortdata *) sect("pixel_data_out");
      
      if (m_tclPixTransOn) {	  // call the pixel translation
	if(Debug()) {gMessMgr->QAInfo() << Form("Starting %20s for sector %2d.\n","xyz_newtab",indx) << endm;}
	// Need to guard against zero size output tables
	if(adcxyz->GetTableSize()){	  
	  Int_t res = xyz_newtab(m_tpg_detector,
				 m_tcl_sector_index,raw_sec_m,
				 raw_row_in,raw_pad_in,raw_seq_in,pixel_data_in,
				 raw_row_out,raw_pad_out,raw_seq_out,pixel_data_out,
				 adcxyz,m_tsspar);
	  
	  if (res != kSTAFCV_OK) Warning("Make","xyz_newtab == %d",res);
	}
      }
      
      //     	TCL
      if(Debug()) {gMessMgr->QAInfo() << Form("Starting %20s for sector %2d.\n","tcl",indx) << endm;}
      
      // Need to guard against zero size output tables
      if(tpcluster->GetTableSize()){	  
	Int_t tcl_res = tcl(m_tpg_pad_plane, m_tcl_sector_index, raw_sec_m,
			    raw_row_in, raw_pad_in, raw_seq_in, pixel_data_in,
			    raw_row_out,raw_pad_out,raw_seq_out,pixel_data_out,
			    tpcluster,tpseq);
	
	if (tcl_res!=kSTAFCV_OK) Warning("Make","tcl == %d",tcl_res);
      }
      // Create morphology table only if needed
      if (m_tclMorphOn) {
	if(Debug()) {gMessMgr->QAInfo() << Form("Starting %20s for sector %2d.\n","cluster_morphology",indx) << endm;}
	
	Int_t tcc_res = cluster_morphology( indx, pixel_data_in, pixel_data_out);
	if(tcc_res) {gMessMgr->Error() << Form("ERROR %d, tcl maker\n",tcc_res) << endm; return kStErr; }
      }
    }
  }
  tpseq->Purge();
  tpcluster->Purge();
  tphit = new St_tcl_tphit("tphit",max_hit);
  m_DataSet->Add(tphit);
  next.Reset();
  
  while ((sector=next())) {  // loop over sectors for TPH
    const  Char_t *name = 0;
    if ((name = strstr(sector->GetName(),"Sector"))) {
      // look for the sector number
      name  = strchr(name,'_') + 1; 
      Int_t indx = atoi(name);
      if (Debug()) {gMessMgr->QAInfo() << Form(" Sector = %d \n", indx) << endm;}
      tcl_sector_index_st *tcl_sector_index = m_tcl_sector_index->GetTable();
      m_tcl_sector_index->SetNRows(1);
      tcl_sector_index->CurrentSector = indx;
      if (m_EastOff&&indx>12) continue;
      if (m_WestOff&&indx<=12) continue;
      St_DataSetIter sect(sector);
      St_type_shortdata  *pixel_data_in  = (St_type_shortdata *) sect("pixel_data_in");
      St_type_shortdata  *pixel_data_out = (St_type_shortdata *) sect("pixel_data_out");
      St_type_shortdata  *pixel_indx_in  = (St_type_shortdata *) sect("pixel_indx_in");
      St_type_shortdata  *pixel_indx_out = (St_type_shortdata *) sect("pixel_indx_out");
      if (! pixel_indx_in  ) {pixel_indx_in  = new St_type_shortdata("pixel_indx_in",1);  AddGarb(pixel_indx_in);}
      if (! pixel_indx_out ) {pixel_indx_out = new St_type_shortdata("pixel_indx_out",1); AddGarb(pixel_indx_out);}
      sector_tot++;
      
      //      TPH
      Int_t k = indx;
      if (sector_tot == 1) {k = -k;}
      tcl_sector_index->CurrentSector = k;
      if(Debug()) {gMessMgr->QAInfo() << Form("Starting %20s for sector %2d.\n","tph",indx) << endm;}
      // Need to guard against zero size output tables
      if(tpcluster->GetTableSize()){	  
	Int_t tph_res = tph(m_tcl_sector_index, m_tclpar,m_tsspar,
			    m_tpg_pad_plane,
			    pixel_data_in, pixel_data_out,
			    pixel_indx_in, pixel_indx_out,
			    tpseq, tpcluster, tphit);
	if (tph_res!=kSTAFCV_OK) Warning("Make","tph == %d",tph_res);
      }
    }
  }
  
  if (sector_tot && m_tclEvalOn) { //slow simulation exist and evaluation switch set
    if (Debug()) {gMessMgr->QAInfo()  << "start run_tte_hit_match" << endm;}
    St_DataSet *geant = GetInputDS("geant");
    if (geant) {
      St_DataSetIter geantI(geant);
      St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geantI("g2t_tpc_hit");
      if (g2t_tpc_hit){//geant data exists too
	
	// create the index table, if any
	index = (St_tcl_tpc_index *) m_DataSet->Find("index");
	if (!index) {
	  index = new St_tcl_tpc_index("index",2*max_hit); 
	  m_DataSet->Add(index);
	}
	
	Int_t Res_tte =  tte_hit_match(g2t_tpc_hit,index,m_type,tphit); 
	//		       ==============================================
	if (Res_tte !=  kSTAFCV_OK) {
	  gMessMgr->Info() << "Problem with tte_hit_match.." << endm;
	}
	if (Debug()) {gMessMgr->QAInfo()  << "finish run_tte_hit_match" << endm;}
      }
    }
  }
  
  // Now move hits to global coordinates, if wanted
  if (m_GlobalHits) {
    gMessMgr->Info() << "Translating hits to Global Coordinates" << endm;
    StTpcCoordinateTransform transform(gStTpcDb);
    StGlobalCoordinate   global;
    tcl_tphit_st *spc = tphit -> GetTable() ;
    for ( Int_t i = 0 ; i < tphit->GetNRows() ; i++ , spc++ )
      {
	StTpcLocalCoordinate local(spc->x,spc->y,spc ->z,(int)spc->row/100,(int)spc->row%100);
	transform(local,global);
	spc -> x = global.position().x();
	spc -> y = global.position().y();
	spc -> z = global.position().z();
	//            {gMessMgr->QAInfo()  << "Local Coordinates: " << local << endm;}
	//            {gMessMgr->QAInfo()  << "Global Coordinates: " << global << endm;}
      }
  }
  
  //		Histograms     
  MakeHistograms(); // clustering histograms
  
  gMessMgr->Info() << "Got through St_tcl_Maker OK." << endm;
  
  return kStOK;
}

//-----------------------------------------------------------------------

void St_tcl_Maker::PrintInfo() {
  {gMessMgr->QAInfo() << Form("**************************************************************\n") << endm;}
  {gMessMgr->QAInfo() << Form("* $Id: St_tcl_Maker.cxx,v 1.79 2007/12/28 13:47:39 fisyak Exp $\n") << endm;}
  {gMessMgr->QAInfo() << Form("**************************************************************\n") << endm;}
  
  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

Int_t St_tcl_Maker::Finish() {
  
  return StMaker::Finish();
}

//----------------------------------------------------------------------

void St_tcl_Maker::InitHistograms() {
  
  // 		Create Histograms
  
  // 		for TPH pam
  
  m_tpc_row    = new TH1F("TclTphitRow" ,"tpc row num"   ,2345,100.5,2445.5);
  m_x_of_hit   = new TH1F("TclTphitHitX","x dist of hits",50,-200.,200.);
  m_y_of_hit   = new TH1F("TclTphitHitY","y dist of hits",50,-200.,200.);
  m_z_of_hit   = new TH1F("TclTphitHitZ","z dist of hits",50,-250.,250.);
  m_charge_hit = new TH1F("TclTphitTotChargeHit","total charge in hit",100,0.,0.00004);
  
  m_nseq_hit     = new TH1F("TclTphitNseq","num seq in hit (empty for TFS)",
			    200,0.5,200.5);
  // 		for TCL pam
  m_nseq_cluster = new TH1F("TclTpclusterNseqCluster",
			    "num seq in cluster (empty for TFS)",
			    100,0.5,200.5);
  m_nhits        = new TH1F("TclTpclusterNhits",
			    "estimated num overlapping hits in cluster (empty for TFS)",
			    20,-0.5,19.5);
}

//----------------------------------------------------------------------

void St_tcl_Maker::MakeHistograms() {
  
  
  // hit table
  tcl_tphit_st* pttphit = NULL; 
  // tphit is global pointer to tpc hit table
  if (tphit) {
    pttphit = tphit->GetTable();
  }
  else { 
    gMessMgr->Info() << "Warning: tphit table header does not exist "  
		     << endm; 
  }
  if (!pttphit) { 
    gMessMgr->Info() << "Warning: tphit table does not exist " << endm; 
  }
  else {
    for(int i=0; i < tphit->GetNRows(); i++) {
      m_tpc_row->Fill((Float_t) pttphit[i].row);
      m_x_of_hit->Fill((Float_t) pttphit[i].x);
      m_y_of_hit->Fill((Float_t) pttphit[i].y);
      m_z_of_hit->Fill((Float_t) pttphit[i].z);
      m_charge_hit->Fill((Float_t) pttphit[i].q);
      if (m_raw_data_tpc) {
	m_nseq_hit->Fill((Float_t) pttphit[i].nseq);
      }
    }
  }
  
  if (m_raw_data_tpc) {
    // tpcluster is global pointer to cluster table
    tcl_tpcluster_st* pttpcl = NULL; 
    // get pointers to tpc hit table
    if (tpcluster) {
      pttpcl = tpcluster->GetTable();
    }
    else { 
      gMessMgr->Info() 
	<< "Warning: tphit cluster table header does not exist "   << endm; 
    }
    if (!pttpcl) { 
      gMessMgr->Info() << "Warning: tphit cluster table does not exist " 
		       << endm; 
    }
    else {
      for(int j=0; j < tpcluster->GetNRows(); j++) {
	m_nseq_cluster->Fill((Float_t) pttpcl[j].nseq);
	m_nhits->Fill((Float_t) pttpcl[j].nhits);
      }
    }
  }
  
}

//_____________________________________________________________________________

Int_t St_tcl_Maker::cluster_morphology( 
				       Int_t 		   sectorNumber,
				       St_type_shortdata *pixel_data_in, 
				       St_type_shortdata *pixel_data_out)
{
  type_shortdata_st *pixTbl;
  unsigned short charge[TCC_PAD][TCC_BIN];
  float meanPadEq3,meanTimeEq4,padSigma1Eq5,timeSigma1Eq6,padTimeSigma1Eq7,
    padSigma2Eq12,timeSigma2Eq13;
  float padTimeSigma2Eq14,ecc1Eq15,ecc2Eq16,linEcc1Eq8,linEcc2Eq9;
  tcl_tp_seq_st *seqTbl = (tcl_tp_seq_st*) tpseq->GetTable();
  int pixBeg,pixEnd,nseq,padrow,sector,seqCnt;
  int iPixTbl,nCluster,iClusterTbl,iSeqTbl;
  int maxCharge,whichPad,whichTimeBin,npad,pads[TCC_PAD];
  int ipad,numberOfPixels;
  unsigned int totChargeEq1;
  static int lastRowPrevTime=-1;
  
  nCluster=tpcluster->GetNRows();
  tcl_tpcluster_st *clusterTbl = (tcl_tpcluster_st*) tpcluster->GetTable();
  for(iClusterTbl=lastRowPrevTime+1;iClusterTbl<nCluster;iClusterTbl++) {
    if(iClusterTbl%473==0) {
      {gMessMgr->QAInfo() << Form("St_tcl_Maker::cluster_morphology Sector %2d, %3d percent done\n",
				  sectorNumber,(100*(iClusterTbl-lastRowPrevTime))/(nCluster-lastRowPrevTime)) << endm;}
    }
    sector=clusterTbl[iClusterTbl].tpc_row/100;
    if(sector!=sectorNumber) { {gMessMgr->QAInfo() << Form("cluster table may be corrupted.\n") << endm;} return 1; }
    padrow=clusterTbl[iClusterTbl].tpc_row%100;
    if(padrow<1||padrow>45) { {gMessMgr->QAInfo() << Form("padrow (%d) out of range.\n",padrow) << endm;} return 2; }
    iSeqTbl=clusterTbl[iClusterTbl].jseq-1;
    nseq=clusterTbl[iClusterTbl].nseq;
    if(padrow<=13) pixTbl= pixel_data_in->GetTable();
    else           pixTbl=pixel_data_out->GetTable();
    npad=0; 		// count number of pads without assuming one sequence per pad
    
    memset(charge,0,sizeof(charge));
    maxCharge=0; numberOfPixels=0;
    
    for(seqCnt=0;seqCnt<nseq;seqCnt++) { // seqCnt is not table index, iSeqTbl is.
      
      // Calculate whichPad, index into pads[]
      
      for(ipad=npad-1;ipad>=0;ipad--) { if(pads[ipad]==seqTbl[iSeqTbl].secpad) { break; } }
      if(ipad>=0) whichPad=ipad;
      else { // create a new entry in pads[]
        if(npad>=TCC_PAD) 						return 55;
        whichPad=npad; pads[npad++]=seqTbl[iSeqTbl].secpad;
      }
      
      pixBeg=seqTbl[iSeqTbl].jpix-1;
      pixEnd=pixBeg+seqTbl[iSeqTbl].tdc_hi-seqTbl[iSeqTbl].tdc_low;
      
      if(seqTbl[iSeqTbl].tpc_row!=clusterTbl[iClusterTbl].tpc_row) 	return 3;
      
      for(iPixTbl=pixBeg;iPixTbl<=pixEnd;iPixTbl++) {
        whichTimeBin=seqTbl[iSeqTbl].tdc_low+iPixTbl-pixBeg;
        if(whichTimeBin<0||whichTimeBin>=TCC_BIN) 			return 81;
        charge[whichPad][whichTimeBin]=pixTbl[iPixTbl].data;
        if(maxCharge<pixTbl[iPixTbl].data) maxCharge=pixTbl[iPixTbl].data;
        numberOfPixels++;
	
      } // end of iPixTbl loop
      
      iSeqTbl=seqTbl[iSeqTbl].next-1;
      
    }// end of seqCnt loop
    
    if(iSeqTbl!=-1) 							return 69;
    if(CalculateQuadrupoleMoms(
			       padrow,npad,pads,charge,
			       totChargeEq1,meanPadEq3,meanTimeEq4,
			       padSigma1Eq5,timeSigma1Eq6,padTimeSigma1Eq7, padSigma2Eq12,
			       timeSigma2Eq13,padTimeSigma2Eq14, ecc1Eq15,ecc2Eq16,
			       linEcc1Eq8,linEcc2Eq9)) 					return 111;

    if(FillOneRowOfMorphTable(
       iClusterTbl,	padrow,	sector,	nseq,	numberOfPixels,
       npad,		totChargeEq1,	maxCharge,
       (float)((1.0*totChargeEq1)/numberOfPixels),
       meanPadEq3,	meanTimeEq4,
       padSigma1Eq5,	timeSigma1Eq6,	  padTimeSigma1Eq7,padSigma2Eq12,
       timeSigma2Eq13,	padTimeSigma2Eq14,ecc1Eq15,ecc2Eq16,
       linEcc1Eq8,	linEcc2Eq9
    )) 									return 102;
  }
  lastRowPrevTime=nCluster-1;
  return kStOK;
}
/*=====================================================================================
                   NOTES FOR FUNCTION cluster_morphology()
---------------+----------------+--------------+---------------------+-----------------
type           | wrapper name   | tableName    | range               | indexStep,
               |                |              |                     | index name
---------------+----------------+--------------+---------------------+-----------------
tcl_tpcluster  | tpcluster      | clusterTbl   | 1 to nok            | ++, iClusterTbl
---------------+----------------+--------------+---------------------+-----------------
tcl_tp_seq     | tpseq          | seqTable     | clusterTbl->jseq,   | tpseq[].next,
               |                |              | clusterTbl->nseq    | iSeqTbl
---------------+----------------+--------------+---------------------+-----------------
type_shortdata | pixel_data_out | pixTbl       | tpseq->jpix,        | ++,
               | or             |              | tpseq->tdc_hi-      | iPixTbl
               | pixel_data_in  |              | tpseq->tdc_low      |
---------------+----------------+--------------+---------------------+-----------------
=====================================================================================*/

//_____________________________________________________________________________

Int_t St_tcl_Maker::FillOneRowOfMorphTable(
  int               	iClusterTbl,
  int 			padrow,
  int 			sector,
  int 			nseq,
  int 			npix,
  int 			npad,
  unsigned int 		totChargeEq1,
  int 			maxCharge,
  float 		averageCharge,
  float 		meanPadPos,
  float 		meanTimePos,
  float 		padSigma1Eq5,
  float 		timeSigma1Eq6,
  float 		padTimeSigma1Eq7,
  float 		padSigma2Eq12,
  float 		timeSigma2Eq13,
  float 		padTimeSigma2Eq14,
  float 		ecc1Eq15,
  float 		ecc2Eq16,
  float 		linEcc1Eq8,
  float 		linEcc2Eq9)
  
{
  tcc_morphology_st singlerow;

  singlerow.clusterId		= iClusterTbl+1;
  singlerow.rowNumber		= padrow;
  singlerow.sectorNumber	= sector;
  singlerow.numberOfSequences	= nseq;
  singlerow.numberOfPixels	= npix;
  singlerow.numberOfPads	= npad;
  singlerow.numberOfHits	= 0;  // will be filled later
  singlerow.totalCharge		= totChargeEq1;
  singlerow.maxCharge		= maxCharge;
  singlerow.averageCharge	= averageCharge;
  singlerow.meanPadPosition	= meanPadPos;
  singlerow.meanTimePosition	= meanTimePos;
  singlerow.padSigma1		= padSigma1Eq5;
  singlerow.timeSigma1		= timeSigma1Eq6;
  singlerow.padTimeSigma1Sq	= padTimeSigma1Eq7;
  singlerow.padSigma2		= padSigma2Eq12;
  singlerow.timeSigma2		= timeSigma2Eq13;
  singlerow.padTimeSigma2Sq	= padTimeSigma2Eq14;
  singlerow.ecc1		= ecc1Eq15;
  singlerow.ecc2		= ecc2Eq16;
  singlerow.linEcc1		= linEcc1Eq8;
  singlerow.linEcc2		= linEcc2Eq9;
  singlerow.meanX		= 0;
  singlerow.meanY		= 0;
  singlerow.meanZ		= 0;
  singlerow.clusterFlag		= 0;

  morph->AddAt(&singlerow,iClusterTbl);

  return kStOK; 
}

//_____________________________________________________________________________

Int_t St_tcl_Maker::CalculateQuadrupoleMoms(
        int padrow,
        int npad,
        int pads[TCC_PAD],
        unsigned short charge[TCC_PAD][TCC_BIN],
        unsigned int  &totChargeEq1,
        float &meanPadEq3,
        float &meanTimeEq4,
        float &padSigma1Eq5,
        float &timeSigma1Eq6,
        float &padTimeSigma1Eq7,
        float &padSigma2Eq12,
        float &timeSigma2Eq13,
        float &padTimeSigma2Eq14,
        float &ecc1Eq15,
        float &ecc2Eq16,
        float &linEcc1Eq8,
        float &linEcc2Eq9)	// Eq #s refer to SN0357
{

  int ii,jj;
  float alpha,beta,qq,centimetersPerPad,tanLinearizationFactor;

  tanLinearizationFactor=tan(LINEARIZATION);
  if(padrow<=13) centimetersPerPad=0.335; else centimetersPerPad=0.67;

  totChargeEq1=0; meanPadEq3=0; meanTimeEq4=0;
  for(ii=npad-1;ii>=0;ii--) {
    for(jj=TCC_BIN-1;jj>=0;jj--) {
      totChargeEq1+=charge[ii][jj];
      meanPadEq3  +=centimetersPerPad*pads[ii]*charge[ii][jj];
      meanTimeEq4 +=(jj+1)*CENTIMETERS_PER_TIME_BIN*charge[ii][jj];
    }
  }
  if((totChargeEq1)>0) meanPadEq3 /=(totChargeEq1); else meanPadEq3 =0;
  if((totChargeEq1)>0) meanTimeEq4/=(totChargeEq1); else meanTimeEq4=0;

  padSigma1Eq5 =0; timeSigma1Eq6 =0; padTimeSigma1Eq7 =0;
  padSigma2Eq12=0; timeSigma2Eq13=0; padTimeSigma2Eq14=0;

  for(ii=npad-1;ii>=0;ii--) {
    for(jj=TCC_BIN-1;jj>=0;jj--) {
      alpha=centimetersPerPad*pads[ii]-(meanPadEq3);
      beta=(jj+1)*CENTIMETERS_PER_TIME_BIN-(meanTimeEq4);
      qq=charge[ii][jj];
      padSigma1Eq5      += alpha        * alpha        * qq ;
      timeSigma1Eq6     += beta         * beta         * qq ;
      padTimeSigma1Eq7  += alpha        * beta         * qq ;
      padSigma2Eq12     += (alpha+beta) * (alpha+beta) * qq ;
      timeSigma2Eq13    += (alpha-beta) * (alpha-beta) * qq ;
      padTimeSigma2Eq14 += (alpha+beta) * (alpha-beta) * qq ;
    }
  }
  if((totChargeEq1)>0) padSigma1Eq5     /=(totChargeEq1); else padSigma1Eq5=0;
  if((totChargeEq1)>0) timeSigma1Eq6    /=(totChargeEq1); else timeSigma1Eq6=0;
  if((totChargeEq1)>0) padTimeSigma1Eq7 /=(totChargeEq1); else padTimeSigma1Eq7=0;
  if((totChargeEq1)>0) padSigma2Eq12    /=(totChargeEq1); else padSigma2Eq12=0;
  if((totChargeEq1)>0) timeSigma2Eq13   /=(totChargeEq1); else timeSigma2Eq13=0;
  if((totChargeEq1)>0) padTimeSigma2Eq14/=(totChargeEq1); else padTimeSigma2Eq14=0;

  padSigma2Eq12    /=2;      // This represents the ::sqrt(2) in Eqs 10 and 11.
  timeSigma2Eq13   /=2;      // This represents the ::sqrt(2) in Eqs 10 and 11.
  padTimeSigma2Eq14/=2;      // This represents the ::sqrt(2) in Eqs 10 and 11.

  padSigma1Eq5 =::sqrt(padSigma1Eq5);
  timeSigma1Eq6=::sqrt(timeSigma1Eq6);
  // sometimes this is ::sqrt(-): padTimeSigma1Eq7=::sqrt(padTimeSigma1Eq7);
  padSigma2Eq12 =::sqrt(padSigma2Eq12);
  timeSigma2Eq13=::sqrt(timeSigma2Eq13);
  // sometimes this is ::sqrt(-): padTimeSigma2Eq14=::sqrt(padTimeSigma2Eq14);

  if(padSigma1Eq5!=0 && timeSigma1Eq6 !=0) {
    ecc1Eq15=(padTimeSigma1Eq7)/((padSigma1Eq5)*(timeSigma1Eq6));
  } else ecc1Eq15=0;

  if(padSigma2Eq12!=0 && timeSigma2Eq13!=0) {
    ecc2Eq16=(padTimeSigma2Eq14)/((padSigma2Eq12)*(timeSigma2Eq13));
  } else ecc2Eq16=0;

  linEcc1Eq8=tan(LINEARIZATION*(ecc1Eq15))/tanLinearizationFactor;
  linEcc2Eq9=tan(LINEARIZATION*(ecc2Eq16))/tanLinearizationFactor;

  return kStOK; 
}

void St_tcl_Maker::EastOff(){m_EastOff = kTRUE;  
  gMessMgr->Info() << "St_tcl_Maker:Turning off East End of TPC " << endm;
}
void St_tcl_Maker::WestOff(){m_WestOff = kTRUE;
  gMessMgr->Info() << "St_tcl_Maker:Turning off West End of TPC " << endm;
}
void St_tcl_Maker::AllOn(){m_WestOff = kFALSE;m_EastOff= kFALSE;
  gMessMgr->Info() << "St_tcl_Maker:Both East and West Ends of TPC Enabled " << endm;
}

