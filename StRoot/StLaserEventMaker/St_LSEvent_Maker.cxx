// $Id: St_LSEvent_Maker.cxx,v 1.1.1.1 1999/04/27 14:29:31 love Exp $
// $Log: St_LSEvent_Maker.cxx,v $
// Revision 1.1.1.1  1999/04/27 14:29:31  love
// First release of Laser Event
//
//copied from St_tpt_Maker
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_LSEvent_Maker class                                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "St_LSEvent_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_tpt_sts_Module.h"
#include "St_tfc_adcxyz_Table.h"
#include "St_tpt_strack_Table.h"
#include "St_tpt_res_Table.h"
#include "TTree.h"
#include "LSEvent/LSEvent.h"
#include "St_type_index_Table.h"

ClassImp(St_LSEvent_Maker)
  
  //_____________________________________________________________________________
  St_LSEvent_Maker::St_LSEvent_Maker(const char *name):
    StMaker(name),
    m_tpg_pad_plane(0),
    m_type(0),
    m_tpt_pars(0),
    m_tpt_spars(0)
{
  m_mkstks=kTRUE;
  m_rowmin=14;
  m_rowmax=40;
}
//_____________________________________________________________________________
St_LSEvent_Maker::~St_LSEvent_Maker(){}
//_____________________________________________________________________________
Int_t St_LSEvent_Maker::Init(){
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

//  Create a root tree. (let controlling Macro make the file?)
  event = new LSEvent();
  m_stks = new TTree("stks","Tpc straight track tree");
  Int_t bufsize= 64000;
  TBranch *br = m_stks->Branch("event", "LSEvent",&event, bufsize, 1);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_LSEvent_Maker::Make(){
  
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
  void St_LSEvent_Maker::MakeHistograms() {
    // reset static clones arrays
   // go get event number from the event data
   Int_t evno = 0;
   if (m_mkstks) {
     St_DataSet *raw = GetDataSet("TPC_DATA"); 
     if (raw) { 
        St_DataSetIter nex(raw);
        St_type_index *I1 = (St_type_index *) nex("IT1");
        type_index_st *ii = I1->GetTable();
        evno = ii->data_row;
     }
     // Fill the event header.  Run number and date passed in from macro.
     event->SetHeader(evno, m_runno, m_date);
     cout << "Event "<< evno << " Run " << m_runno << endl;

     //  Make the "stks"  TTree  Should be controllable.
     St_tfc_adcxyz  *n_adc = 0;
     St_tcl_tphit  *n_hit = 0;
     St_tcl_tpcluster *n_clus  = 0;
     St_DataSet *tpc_hits = GetDataSet("tpc_hits");
     if (tpc_hits) {
        St_DataSetIter tpc_data(tpc_hits);
        n_hit      = (St_tcl_tphit *) tpc_data["tphit"];
        n_clus     = (St_tcl_tpcluster *)  tpc_data["tpcluster"];
     }
     if(n_hit){
       tcl_tphit_st *h = n_hit->GetTable();
       for (int i = 0;i<n_hit->GetNRows();i++,h++){
          event->AddHit(h->q,h->x,h->y,h->z,h->row,h->track, h->flag);
       }
       event->SetLastHit(n_hit->GetNRows()-1);          
       cout << n_hit->GetNRows() << " hits, " ; 
     }
     // Create an iterator for the track dataset
     St_DataSetIter tpc_tracks(m_DataSet);
     St_tpt_strack * n_track = (St_tpt_strack *) tpc_tracks["tptrack"];
     tpt_strack_st *t = n_track->GetTable();
     Int_t ntks=n_track->GetNRows();
     for(int itk=0;itk<ntks;itk++,t++){
          Float_t y0 = -196.6;
	  Float_t x0 = (1.0-t->ay*y0)/t->ax;
          Float_t z0 = t->bzy+y0*t->azy;
          Float_t ax = -t->ay/t->ax;
          event->AddTrack(t->trk,t->npnt,ax, t->az, x0, y0, z0, 
			       t->chisqxy,t->chisqz);
     } //end of itk for loop - fix the maximum counter
         event->SetLastTrack(ntks-1);          
     cout <<  ntks << " tracks, ";
     // Find the adc table.
     St_DataSet *tpc_raw = GetDataSet("tpc_raw");
     if(tpc_raw){
        St_DataSetIter tpcadc(tpc_raw);
        n_adc = (St_tfc_adcxyz *) tpcadc["adcxyz"];
     }
     if(n_adc){
         Int_t npixwrit=0;
         tfc_adcxyz_st *p = n_adc->GetTable();
         cout << n_adc->GetNRows() << " pixels in adcxyz table, " ;
         for(int iadc=0;iadc<n_adc->GetNRows();iadc++,p++){
	   if(p->row >=m_rowmin && p->row<=m_rowmax){
	     event->AddPixel(100*p->sector+p->row,p->pad,p->bucket,
                   p->adc,p->x,p->y,p->z);
	     npixwrit++;
	   }
	 }
         event->SetLastPixel(npixwrit-1);          
	 cout << npixwrit <<" pixels written to event " << evno << endl;
     }
     m_stks->Fill(); //Fill the Tree
   }  //end of if(m_mksts)
}  // end of MakeHistograms member.
//_____________________________________________________________________________
void St_LSEvent_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_LSEvent_Maker.cxx,v 1.1.1.1 1999/04/27 14:29:31 love Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

