// $Id: StLaserEventMaker.cxx,v 1.1 1999/09/28 15:34:34 love Exp $
// $Log: StLaserEventMaker.cxx,v $
// Revision 1.1  1999/09/28 15:34:34  love
// change LSEvent to LaserEvent
//

//
// Revision 1.1.1.1  1999/09/28 14:29:31  love
// First release of StLaserEventMaker
//
// copied from St_LSEvent_Maker
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StLaserEventMaker class                                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StLaserEventMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_tpt_Module.h"
#include "tables/St_tfc_adcxyz_Table.h"
#include "tables/St_tpt_track_Table.h"
#include "TTree.h"
#include "StLaserEvent/StLaserEvent.h"
#include "tables/St_type_index_Table.h"

ClassImp(StLaserEventMaker)

//_____________________________________________________________________________
  StLaserEventMaker::StLaserEventMaker(const char *name):
    StMaker(name),
    m_tpg_pad_plane(0),
    m_type(0),
    m_tpt_pars(0)
{
  m_mklaser=kTRUE;
  m_rowmin=14;
  m_rowmax=45;
}
//_____________________________________________________________________________
StLaserEventMaker::~StLaserEventMaker(){}
//_____________________________________________________________________________
void StLaserEventMaker::Clear(Option_t *option){
  event->Clear(option);
  StMaker::Clear(option);
}
//_____________________________________________________________________________
Int_t StLaserEventMaker::Init(){
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
  if (!(m_tpt_pars)) 
    Error("Init", "tpt parameters have not been initialized" );
  assert(m_tpt_pars);

//  Create a root tree. (let controlling Macro make the file?)
  event = new StLaserEvent();
  m_laser = new TTree("laser","Tpc laser track tree");
  Int_t bufsize= 64000;
  TBranch *br = m_laser->Branch("event", "StLaserEvent",&event, bufsize, 1);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StLaserEventMaker::Make(){
  
  St_DataSet *tpc_data =  GetInputDS("tpc_hits"); 
  if (!tpc_data) return 0;
  
// 		Clusters exist -> do tracking
  St_DataSetIter gime(tpc_data);
  St_tcl_tphit     *tphit = (St_tcl_tphit     *) gime("tphit");
  //  St_tcl_tpc_index *index = (St_tcl_tpc_index *) gime("index");
  //  if (!index) {index = new St_tcl_tpc_index("index",10*maxNofTracks);  m_DataSet->Add(index);}
      
  St_tpt_track  *tptrack = new St_tpt_track("tptrack",maxNofTracks);
  m_DataSet->Add(tptrack);


//			call TPT tracker 
    if (Debug()) cout << " start tpt run " << endl;
    Int_t Res_tpt = tpt(m_tpt_pars,tphit,tptrack);
//                      ==============================
    
    if (Res_tpt != kSTAFCV_OK) {
     cout << "Problem with tpt.." << endl;
      return kStErr;}
    
    if (Debug()) cout << " finish tpt run " << endl;

  MakeHistograms(); // tracking histograms
  return kStOK;
}
//_____________________________________________________________________________
  void StLaserEventMaker::MakeHistograms() {
    // reset static clones arrays
   // go get event number from the event data
   Int_t evno = 0;
   if (m_mklaser) {
     St_DataSet *raw = GetDataSet("TPC_DATA"); 
     if (raw) { 
        St_DataSetIter nex(raw);
        St_type_index *I1 = (St_type_index *) nex("IT1");
        if(!I1) I1 = (St_type_index *) nex("IT2");
        if (I1) {
          type_index_st *ii = I1->GetTable();
          evno = ii->data_row;
        }
     }
     // Fill the event header.  Run number and date passed in from macro.
     event->SetHeader(evno, m_runno, m_date);
     cout << "Event "<< evno << " Run " << m_runno << endl;

     //  Make the "laser"  TTree  Should be controllable.
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
       cout << n_hit->GetNRows() << " hits, " ; 
     }
     // Create an iterator for the track dataset
     St_DataSetIter tpc_tracks(m_DataSet);
     St_tpt_track * n_track = (St_tpt_track *) tpc_tracks["tptrack"];
     tpt_track_st *t = n_track->GetTable();
     Int_t ntks=n_track->GetNRows();
     for(int itk=0;itk<ntks;itk++,t++){
          event->AddTrack(t->flag,t->hitid,t->id,t->id_globtrk,
         t->ndedx, t->nfit, t->nrec, t->npos, t->q,
         t->chisq[0], t->chisq[1], t->dedx[0], t->invp, t->curvature,
         t->psi, t->tanl, t->phi0, t->r0, t->z0);
     } //end of itk for loop - fix the maximum counter
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
	 cout << npixwrit <<" pixels written to event " << evno << endl;
     }
     m_laser->Fill(); //Fill the Tree
   }  //end of if(m_mksts)
}  // end of MakeHistograms member.
//_____________________________________________________________________________

