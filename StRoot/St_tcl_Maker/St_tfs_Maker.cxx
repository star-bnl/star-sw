// $Id: St_tfs_Maker.cxx,v 1.1 2007/12/28 13:47:40 fisyak Exp $
// $Log: St_tfs_Maker.cxx,v $
// Revision 1.1  2007/12/28 13:47:40  fisyak
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
// St_tfs_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "St_tfs_Maker.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
//#include "St_XDFFile.h"
#include "StMessMgr.h"
#include "tpc/St_tfs_g2t_Module.h"
#include "tpc/St_tfs_filt_Module.h"
#include "tpc/St_tfs_fill_tphit_pad_tmbk_Module.h"
#include "StTpcDb/StTpcDb.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StEvent.h"
#include "StTpcHitCollection.h"
#include "StTpcHit.h"
#include "StThreeVectorF.hh"
#include "tables/St_tpg_pad_plane_Table.h"
#include "tables/St_tpg_detector_Table.h"
ClassImp(St_tfs_Maker)
  
//_____________________________________________________________________________
Int_t St_tfs_Maker::InitRun(int runnumber) {
  m_tpg_pad_plane = (St_tpg_pad_plane *) GetDataSet("tpcDB/.const/tpg_pad_plane");
  m_tpg_detector  = (St_tpg_detector  *) GetDataSet("tpcDB/.const/tpg_detector");
  assert ((m_tpg_pad_plane && m_tpg_detector)) ;

  // 		Create tables
  St_DataSet *tpc = GetDataBase("tpc");
  assert(tpc);

  // 		geometry parameters

  // 		TFS parameters
  St_DataSet *tfspars = tpc->Find("tfspars");
  St_DataSet *tclpars = tpc->Find("tclpars");
  assert(tfspars && tclpars);
  m_tfs_fspar = NULL;
  m_tfs_fspar = (St_tfs_fspar *) tfspars->Find("tfs_fspar");
  m_tfs_fsctrl= NULL;
  m_tfs_fsctrl= (St_tfs_fsctrl*) tfspars->Find("tfs_fsctrl");
  m_type      = (St_tcl_tpc_index_type *) tclpars->Find("type");
  return kStOK;

}

//_____________________________________________________________________________

Int_t St_tfs_Maker::Make() {

  //  m_DataSet is global pointer from StMaker to data set

  if (Debug()) {gMessMgr->QAInfo() << Form("Start of TFS Maker") << endm;}

  // write out the drift velocity
  gMessMgr->Info() << "Drift velocity used: " << gStTpcDb->DriftVelocity() 
		   << endm;
 
  // 		Raw data does not exist, check GEANT. if it does then use fast cluster simulation
  St_DataSet *geant = GetInputDS("geant");
  if (geant) {
    St_DataSetIter geantI(geant);
    St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geantI("g2t_tpc_hit");
    int max_hit=0;
    if (g2t_tpc_hit) { 
      max_hit = g2t_tpc_hit->GetNRows();
    }
    else {
      gMessMgr->Info() << "No g2t_tpc_hit table!!!!!!" << endm;
    }
    St_g2t_track   *g2t_track   = (St_g2t_track   *) geantI("g2t_track");
    St_g2t_vertex  *g2t_vertex  = (St_g2t_vertex  *) geantI("g2t_vertex");
    if (g2t_tpc_hit && g2t_track){
      // create the index table, if any
      index = (St_tcl_tpc_index *) m_DataSet->Find("index");
      if (!index) {
	index = new St_tcl_tpc_index("index",2*max_hit); 
	m_DataSet->Add(index);
      }
      if (Debug()) {gMessMgr->QAInfo()  << "start tfs_run" << endm;}
      
      // make a tphit table with the size of the number of geant hits
      tphit = new St_tcl_tphit("tphit",max_hit); 
      m_DataSet->Add(tphit);
      
      Int_t Res_tfs_g2t = tfs_g2t(g2t_tpc_hit, g2t_track, g2t_vertex,
				  m_tfs_fspar,m_tfs_fsctrl,
				  index, m_type, tphit);
      if (Res_tfs_g2t != kSTAFCV_OK) {
	gMessMgr->Info() << "Problem running tfs_g2t..." << endm;
      }
      else {
	Int_t Res_tfs_filt = tfs_filt(tphit);
	if ( Res_tfs_filt !=  kSTAFCV_OK){ 
	  gMessMgr->Info() << " Problem running tfs_filt..." << endm;
	}
	
	Int_t Res_tfs_fill_tphit_pad_tmbk=
	  tfs_fill_tphit_pad_tmbk(m_tpg_pad_plane,tphit);
	if ( Res_tfs_fill_tphit_pad_tmbk !=  kSTAFCV_OK){ 
	  gMessMgr->Info() 
	    << " Problem running tfs_fill_tphit_pad_tmbk..." << endm;
	} else {
	  FillStEvent(tphit);
	  SafeDelete(tphit);
	}
      }
      if (Debug()) {gMessMgr->QAInfo()  << "finish tfs_run" << endm;}
    }
  }

  return kStOK;
}

//-----------------------------------------------------------------------

void St_tfs_Maker::PrintInfo() {
  {gMessMgr->QAInfo() << Form("**************************************************************\n") << endm;}
  {gMessMgr->QAInfo() << Form("* $Id: St_tfs_Maker.cxx,v 1.1 2007/12/28 13:47:40 fisyak Exp $\n") << endm;}
  {gMessMgr->QAInfo() << Form("**************************************************************\n") << endm;}

  if (Debug()) StMaker::PrintInfo();
}
//________________________________________________________________________________
void St_tfs_Maker::FillStEvent(St_tcl_tphit *tphit) {
  if (! tphit) return;
  Int_t NH = tphit->GetNRows();
  if (NH <= 0) return;
  StEvent *pEvent = (StEvent *) GetInputDS("StEvent"); assert(pEvent);
  StTpcHitCollection *mTpcHitColl = new StTpcHitCollection();
  pEvent->setTpcHitCollection(mTpcHitColl);
  tcl_tphit_st *hit = tphit->GetTable();
  for (Int_t i = 0; i < NH; i++, hit++) {
    StThreeVectorF p(hit->x,hit->y,hit->z);
    StThreeVectorF e(hit->dx,hit->dy,hit->dz);
    
    unsigned int hw = 1;         // detid_tpc
    hw += (hit->row/100 << 4);   // sector
    hw += (hit->row%100 << 9);   // row
    hw += (hit->npads   << 15);  // npads
    hw += (hit->ntmbk   << 22);  // ntmbks...
    
    StTpcHit *tpcHit = new StTpcHit(p,e,hw,hit->q, 0,
				    hit->id_simtrk,hit->id_quality, hit->id, 
				    hit->minpad, hit->maxpad, hit->mintmbk, hit->maxtmbk,hit->cl_x,hit->cl_t);
    tpcHit->setIdTruth(hit->id_simtrk, hit->id_quality);
    mTpcHitColl->addHit(tpcHit);
  }
}
