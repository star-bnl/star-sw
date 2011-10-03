// $Id: St_tptsts_Maker.cxx,v 1.1.1.1 1999/05/10 13:24:15 love Exp $
// $Log: St_tptsts_Maker.cxx,v $
// Revision 1.1.1.1  1999/05/10 13:24:15  love
// Straight track Maker
//
//copied from St_tpt_Maker
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tptsts_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "St_tptsts_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_tpt_sts_Module.h"
#include "St_tpt_strack_Table.h"
#include "St_tpt_res_Table.h"
#include "TNtuple.h"
#include "St_type_index_Table.h"

ClassImp(St_tptsts_Maker)
  
  //_____________________________________________________________________________
  St_tptsts_Maker::St_tptsts_Maker(const char *name):
    StMaker(name),
    m_tpg_pad_plane(0),
    m_type(0),
    m_tpt_pars(0),
    m_tpt_spars(0)
{
  m_mkstks=kTRUE;
}
//_____________________________________________________________________________
St_tptsts_Maker::~St_tptsts_Maker(){}
//_____________________________________________________________________________
Int_t St_tptsts_Maker::Init(){
  // Create tables
  
  St_DataSet *tpcpars = GetInputDB("params/tpc");
  assert(tpcpars);
  
  St_DataSetIter       gime(tpcpars);
  
// 		TPG parameters
  m_tpg_pad_plane = (St_tpg_pad_plane *) gime("tpgpar/tpg_pad_plane");
  if (!(m_tpg_pad_plane)) Error("Init","tpc/tpgpar is not initialized. \n");
  assert(m_tpg_pad_plane);
  
// 		TCL parameters
  m_type = (St_tcl_tpc_index_type *) gime("tclpars/type");
  if (!m_type) Error("Init"," Clustering parameters have not been initialized");
  assert(m_type);
  
// 		TPT parameters
  m_tpt_pars  = (St_tpt_pars* ) gime("tptpars/tpt_pars" );
  m_tpt_spars = (St_tpt_spars*) gime("tptpars/tpt_spars");
  if (!(m_tpt_pars && m_tpt_spars)) 
    Error("Init", "tpt parameters have not been initialized" );
  assert(m_tpt_pars && m_tpt_spars);
  

// 		Create ntuple
  m_stks = new TNtuple("stks","Tpc straight tracks",
     "evno:alpha:lambda:row:x:y:z:track:cluster:q:zrf:prf:nfit:ax:az:x0:z0:chisqxy:chisqz");

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_tptsts_Maker::Make(){
  
  St_DataSet *tpc_data =  GetInputDS("tpc_hits"); 
  if (!tpc_data) return 0;
  
// 		Clusters exist -> do tracking
  St_DataSetIter gime(tpc_data);
  St_tcl_tphit     *tphit = (St_tcl_tphit     *) gime("tphit");
  //  St_tcl_tpc_index *index = (St_tcl_tpc_index *) gime("index");
  //  if (!index) {index = new St_tcl_tpc_index("index",10*maxNofTracks);  m_DataSet->Add(index);}
      
  St_tpt_strack  *tptrack = new St_tpt_strack("tptrack",maxNofTracks);
  m_DataSet->Add(tptrack);
  St_tpt_res *res1 = new St_tpt_res("res1",50*maxNofTracks);
  St_tpt_res *res2 = new St_tpt_res("res2",50*maxNofTracks);
  m_DataSet->Add(res1);
  m_DataSet->Add(res2);

//			TPT_STS straight line tracker
    if (Debug()) cout << " start tpt_sts run " << endl;
    Int_t Res_tpt = tpt_sts(m_tpt_spars,tphit,res1,res2,tptrack);
//                      ==============================
    
    if (Res_tpt != kSTAFCV_OK) {cout << "Problem with tpt_sts.." << endl;}
    if (Debug()) cout << " finish tpt_sts run " << endl;

  MakeHistograms(); // tracking histograms
  return kStOK;
}
//_____________________________________________________________________________
  void St_tptsts_Maker::MakeHistograms() {
   // go get event number from the event data
   Int_t evno = 0;
   if (m_mkstks) {
     St_DataSet *raw = GetInputDS("TPC_DATA"); 
     if (raw) { 
        St_DataSetIter nex(raw);
        St_type_index *I1 = (St_type_index *) nex("IT1");
        type_index_st *ii = I1->GetTable();
        evno = ii->data_row;
     }
     // Create an iterator
     St_DataSetIter tpc_tracks(m_DataSet);
     //  Make the "Stks" hit and track ntuple  Should be controllable.
     St_tcl_tphit  *n_hit = 0;
     St_tcl_tpcluster *n_clus  = 0;
     St_DataSet *tpc_hits = GetInputDS("tpc_hits");
     if (tpc_hits) {
        St_DataSetIter tpc_data(tpc_hits);
        n_hit      = (St_tcl_tphit *) tpc_data["tphit"];
        n_clus     = (St_tcl_tpcluster *)  tpc_data["tpcluster"];
     }
     if(n_hit){
       St_tpt_strack * n_track = (St_tpt_strack *) tpc_tracks["tptrack"];
       tcl_tphit_st *h = n_hit->GetTable(); 
       for (int i = 0;i<n_hit->GetNRows();i++,h++){
	  // cluster variable is one more than row num in cluster table
	  tcl_tpcluster_st *clu = n_clus->GetTable();
	  clu += h->cluster -1;
	  if(h->track != 0){
	    //find the track, if any
	    tpt_strack_st *t = n_track->GetTable(); 
	    for(int itk=0;itk<n_track->GetNRows();itk++,t++){
	      if(t->trk != h->track) continue;

	      /*    "evno:alpha:lambda:row:x:y:z:track:cluster:q:zrf:prf:nfit:ax:az:x0:z0:chisqxy:chisqz"  */
             Float_t y0 = -196.6;
	     Float_t x0 = (1.0-t->ay*y0)/t->ax;
             Float_t z0 = t->bzy+y0*t->azy;
             Float_t ax = -t->ay/t->ax;
	      Float_t row[] = {evno,h->alpha,h->lambda,
			       h->row,h->x,h->y,h->z,h->track,
                               h->cluster,h->q,h->zrf,h->prf,
			       t->npnt,ax,t->azy,x0,z0,
			       t->chisqxy,t->chisqz};
	      m_stks->Fill(row);
	    } //end of itk for loop
	  } //end of if h->track
	  else{
	    Float_t row[] = {evno,h->alpha,h->lambda,
                             h->row,h->x,h->y,h->z,h->track,
                             h->cluster,h->q,h->zrf,h->prf,
                             0,0,0,0,0,0,0};
	    m_stks->Fill(row);
	  } // end of no track else
       }  // end of hit loop
     }  //end of if(n_hit)
   }  //end of if(m_mksts)
}  // end of MakeHistograms member.
//_____________________________________________________________________________
void St_tptsts_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_tptsts_Maker.cxx,v 1.1.1.1 1999/05/10 13:24:15 love Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

