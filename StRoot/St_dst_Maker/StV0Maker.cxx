//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StV0Maker class                                                    //
//                                                                      //
// $Id: StV0Maker.cxx,v 1.29 2000/10/12 18:26:38 genevb Exp $
// $Log: StV0Maker.cxx,v $
// Revision 1.29  2000/10/12 18:26:38  genevb
// Edit some warning messages
//
// Revision 1.28  2000/10/12 14:52:14  genevb
// Remove vertex table entries when trimming V0s
//
// Revision 1.27  2000/09/01 15:00:23  genevb
// Reset dcaV0 to 0.8cm
//
// Revision 1.26  2000/08/31 21:47:08  genevb
// Allow V0s to be trimmed after finding Xis
//
// Revision 1.25  2000/07/07 23:06:48  caines
// Increase memory allocation for v0s
//
// Revision 1.24  2000/06/15 21:32:10  genevb
// Bug fix for zeroing variables
//
// Revision 1.23  2000/06/13 13:04:21  genevb
// Fixed bug with not finding primary vertex
//
// Revision 1.22  2000/05/30 20:31:39  genevb
// Adding ev0_am3 option
//
// Revision 1.21  2000/03/30 22:32:06  genevb
// Fixed a typo
//
// Revision 1.20  2000/03/30 16:33:39  genevb
// Change messages to say where they are called from
//
// Revision 1.19  2000/03/02 20:41:35  caines
// New dcav0 cut went from 0.7cm ->2.5cm from Curtis
//
// Revision 1.18  2000/02/08 21:14:18  genevb
// Handle cases with no tracks.
//
// Revision 1.17  2000/01/27 01:33:53  caines
// Now only runs ev0eval if geant there
//
// Revision 1.16  1999/12/10 17:20:26  genevb
// No need to set 4 primary vertices, changed formula for table allocation
//
// Revision 1.15  1999/11/10 02:19:10  lbarnby
// change in StV0Maker related to multiple primary vertices in vertex table
//
// Revision 1.14  1999/11/02 11:27:56  macl
// added n_point quality cut to V0 daughter tracks
//
// Revision 1.13  1999/09/29 20:29:09  wdeng
// Accommodate dst_track and dst_vertex change
//
// Revision 1.12  1999/09/16 13:53:08  fisyak
// Fix typo in ev0par2 (thanks Matt)
//
// Revision 1.11  1999/09/12 23:03:04  fisyak
// Move parameters into makers
//
// Revision 1.10  1999/07/17 00:31:25  genevb
// Use StMessMgr
//
// Revision 1.9  1999/07/15 13:57:54  perev
// cleanup
//
// Revision 1.8  1999/07/14 15:48:19  caines
// Correct check on stk_track when eval turned on
//
// Revision 1.7  1999/07/12 23:04:16  fisyak
// Remove glob2
//
// Revision 1.6  1999/07/12 01:49:39  fine
// Clean up
//
// Revision 1.5  1999/07/11 01:55:45  fisyak
// Fix glob->impact
//
// Revision 1.4  1999/07/08 19:09:52  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>

#include "TMath.h"
#include "StV0Maker.h"

#include "StVertexId.h"

#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "global/St_ev0_am2_Module.h"
#include "global/St_ev0_am3_Module.h"
#include "global/St_ev0_eval2_Module.h"

#include "tables/St_dst_xi_vertex_Table.h"
#include "tables/St_dst_tkf_vertex_Table.h"

St_dst_xi_vertex* dst_xi_vertex = 0;
St_dst_tkf_vertex* dst_tkf_vertex = 0;
Int_t rsize,rvsize,lastV0;

ClassImp(StV0Maker)
  
  //_____________________________________________________________________________
  StV0Maker::StV0Maker(const char *name):StMaker(name),
  m_ev0par(0)
{
  m_ev0EvalOn=kFALSE;
}
//_____________________________________________________________________________
StV0Maker::~StV0Maker(){
}
//_____________________________________________________________________________
Int_t StV0Maker::Init(){
  m_ev0par2 = new St_ev0_ev0par2("ev0par2",3); // For finding V0s, even for Xis
  m_ev0parT = new St_ev0_ev0par2("ev0parT",3); // For trimming to pure V0s
  {
    ev0_ev0par2_st row;
    memset(&row,0,m_ev0par2->GetRowSize());
  // TPC only cuts
    row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
    row.dca	 =        0.8; // cut on dca between the two tracks ;
    row.dcav0	 =        0.8; // cut on dca(impact parameter) of V0 from event vertex ;
    row.dlen	 =        2.0; // cut on dist. of decay from prim. vertex ;
    row.alpha_max=        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
    row.ptarm_max=        0.3; // Max. value of arm. pt allowed, only first entry used;
    row.dcapnmin=         0.7; // Min. value of tracks at interaction ;
    if (m_Mode == 1) {
      row.dcapnmin=       0.0; // Min. value of tracks at interaction for ev03;
    }
    row.n_point  =         11; // Min. number of TPC hits on a track ;
    m_ev0parT->AddAt(&row,0);
    row.dcav0	 =        2.5; // cut on dca(impact parameter) of V0 from event vertex ;
    row.dcapnmin=         0.4; // Min. value of tracks at interaction ;
    m_ev0par2->AddAt(&row,0);
    memset(&row,0,m_ev0par2->GetRowSize());
  //SVT only cuts
    row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
    row.dca	 =        0.8; // cut on dca between the two tracks ;
    row.dcav0	 =        1.5; // cut on dca(impact parameter) of V0 from event vertex ;
    row.dlen	 =      10000; // cut on dist. of decay from prim. vertex ;
    row.alpha_max=        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
    row.ptarm_max=        0.3; // Max. value of arm. pt allowed, only first entry used;
    row.dcapnmin=         100; // Min. value of tracks at interaction ;
    row.n_point  =          1; // Min. number of SVT hits on a track ;
    m_ev0parT->AddAt(&row,1);
    row.dcav0	 =        2.5; // cut on dca(impact parameter) of V0 from event vertex ;
    row.dcapnmin=         100; // Min. value of tracks at interaction ;
    m_ev0par2->AddAt(&row,1);
    memset(&row,0,m_ev0par2->GetRowSize());
  // SVT+TPC cuts
    row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
    row.dca	 =        0.8; // cut on dca between the two tracks ;
    row.dcav0	 =        0.7; // cut on dca(impact parameter) of V0 from event vertex ;
    row.dlen	 =        0.6; // cut on dist. of decay from prim. vertex ;
    row.alpha_max=        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
    row.ptarm_max=        0.3; // Max. value of arm. pt allowed, only first entry used;
    row.dcapnmin =        0.7; // Min. value of tracks at interaction ;
    row.n_point  =         11; // Min. number of SVT+TPC hits on a track ;
    m_ev0parT->AddAt(&row,2);
    row.dcav0	 =        2.5; // cut on dca(impact parameter) of V0 from event vertex ;
    row.dcapnmin=         0.4; // Min. value of tracks at interaction ;
    m_ev0par2->AddAt(&row,2);
  }
  AddRunCont(m_ev0par2);
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StV0Maker::Make(){
  //if(Debug()) gMessMgr->Debug("StV0Maker::Make(): Calling ev0..."); 
  PrintInfo();   
  
  int iMake = kStOK;
  int iRes = 0;
  vertex = 0;
  dst_v0_vertex = 0;
  
  St_DataSet     *match = GetDataSet("match"); 
  if (!match) {
    gMessMgr->Warning() << "StV0Maker::Make(): match is missing" << endm;
    return kStWarn;
  }
  St_DataSetIter matchI(match);         
  St_dst_track   *globtrk  = (St_dst_track *) matchI("globtrk");
  if (!globtrk) {
    gMessMgr->Warning() << "StV0Maker::Make(): globtrk is missing" << endm;
    return kStWarn;
  }
  
  St_DataSet     *primary = GetDataSet("primary"); 
  
  if (!primary) {
    gMessMgr->Warning() << "StV0Maker::Make(): primary is missing" << endm;
    return kStWarn;
  }
  St_DataSetIter primaryI(primary);
  vertex   = (St_dst_vertex *) primaryI("vertex");
  if (!vertex) {
    gMessMgr->Warning() << "StV0Maker::Make(): vertex is missing" << endm;
    return kStWarn;
  }
  
  St_ev0_eval   *ev0_eval  = 0;
  St_DataSet    *tpctracks = GetInputDS("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  St_tte_eval   *evaltrk   = 0;
  
  if (tpctracks) {
    St_DataSetIter tpc_tracks(tpctracks); 
    tptrack   = (St_tpt_track  *) tpc_tracks("tptrack");
    evaltrk   = (St_tte_eval   *) tpc_tracks("evaltrk");
  }
  if (! evaltrk)    {evaltrk = new St_tte_eval("evaltrk",1); AddGarb(evaltrk);}
  
  St_DataSet     *svtracks = GetInputDS("svt_tracks");
  St_stk_track   *stk_track   = 0;
  
  if (svtracks) {
    stk_track = (St_stk_track  *) svtracks->Find("stk_track");
  }
  
  
  dst_vertex_st *vrtx = vertex->GetTable();
  for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,vrtx++) {
  // Above loop runs until primary vertex is found. When found, the code
  // below is executed, and a "break;" gets out of the for-loop. If no
  // primary vertex is found, the loop just ends normally, doing nothing.
  if (vrtx->vtx_id == kEventVtxId && vrtx->iflag == 1) {
    // ev0
    if(Debug()) gMessMgr->Info() << "StV0Maker::Make(): Calling ev0..." << endm;
    Int_t v0_limit = globtrk->GetNRows()/80;
    v0_limit = v0_limit*v0_limit;
    if (v0_limit < 15000) v0_limit=15000;
    if (! dst_v0_vertex) {
      dst_v0_vertex = new St_dst_v0_vertex("dst_v0_vertex",v0_limit); 
      AddData(dst_v0_vertex);
    }
    vertex->ReAllocate(v0_limit);
    Long_t NGlbTrk = globtrk->GetNRows();
    Long_t space2 = 0;
    Long_t space3 = 0;
    if (m_Mode == 1) {
      space3 = NGlbTrk;
    } else {
      space2 = NGlbTrk;
    }
    St_ev0_track2 *ev0track2 = new St_ev0_track2("ev0_track2",space2);
    St_ev0_track3 *ev0track3 = new St_ev0_track3("ev0_track3",space3);
    AddGarb(ev0track2);
    AddGarb(ev0track3);
    if (NGlbTrk < 1) {
      gMessMgr->Warning("StV0Maker::Make(): Cannot call ev0 with <1 tracks");
      iRes = kSTAFCV_ERR;
    } else {
      if (m_Mode == 0) {
        iRes = ev0_am2(m_ev0par2,globtrk,vertex,dst_v0_vertex,ev0track2);
      } else if (m_Mode == 1) {
        gMessMgr->Info("StV0Maker::Make(): Calling ev0_am3");
        iRes = ev0_am3(m_ev0par2,globtrk,vertex,dst_v0_vertex,ev0track3);
      } else {
        gMessMgr->Error() << "StV0Maker::Make(): Unknown mode = "
                          << m_Mode << endm;
        iRes = kSTAFCV_ERR;
      }
    }
    //       =========================================================
    
    if (iRes !=kSTAFCV_OK) {
      gMessMgr->Warning("StV0Maker::Make(): Problem on return from EV0");
      iMake = kStWarn;
    }
    if(m_ev0EvalOn){   
      //ev0_eval2
	St_DataSetIter geant(GetInputDS("geant"));
	St_g2t_track   *g2t_track    = (St_g2t_track  *) geant("g2t_track");
	St_g2t_vertex  *g2t_vertex   = (St_g2t_vertex *) geant("g2t_vertex");

      if (tptrack && evaltrk && g2t_track && g2t_vertex) {
	if (! stk_track)    {stk_track = new St_stk_track("stk_track",1); AddGarb(stk_track);}
	ev0_eval = new St_ev0_eval("ev0_eval",20000);
	AddData(ev0_eval);
	
	if(Debug()) gMessMgr->Info("StV0Maker::Make(): Calling ev0_eval2..");
	Int_t Res_ev0_eval = kSTAFCV_BAD;
	Res_ev0_eval = ev0_eval2(stk_track,tptrack,evaltrk,
				 vertex,dst_v0_vertex,ev0_eval,
				 g2t_track,g2t_vertex);
	
	if (Res_ev0_eval != kSTAFCV_OK) {
	  gMessMgr->Warning("StV0Maker::Make(): Problem on return from ev0eval2");
        }
      }
    }     // If ev0 evaluation switched on 
    break;
  } // If-then block if primary vertex found
  } // For-loop to find primary vertex
  return iMake;
}
//_____________________________________________________________________________
void StV0Maker::Trim(){
  if (!((dst_v0_vertex) && (vertex))) return;

  // Get Xi vertices
  St_DataSet *xi = GetDataSet("xi"); 
  if (!xi) return;
  St_DataSetIter xiI(xi);         
  dst_xi_vertex = (St_dst_xi_vertex *) xiI("dst_xi_vertex");
  if (!dst_xi_vertex) return;

  // Get Kink vertices (if they are done)
  St_DataSet *kink = GetDataSet("kink"); 
  if (kink) {
    St_DataSetIter kinkI(kink);         
    dst_tkf_vertex = (St_dst_tkf_vertex *) kinkI("dst_tkf_vertex");
  } else
    dst_tkf_vertex = 0;

  Int_t ixi = dst_xi_vertex->GetNRows() - 1;
  ev0_ev0par2_st* pars = m_ev0parT->GetTable(0);
  rsize = dst_v0_vertex->GetRowSize();
  rvsize = vertex->GetRowSize();

  for (Int_t iv0 = 0; iv0 < dst_v0_vertex->GetNRows(); iv0++) {
    dst_v0_vertex_st* v0row = dst_v0_vertex->GetTable(iv0);
    Bool_t isXiV0 = (v0row->dcav0 < 0);
    Bool_t passV0 = (TMath::Abs(v0row->dcav0) < pars->dcav0) &&
        (v0row->dcap > pars->dcapnmin) && (v0row->dcan > pars->dcapnmin);

    if (isXiV0 && passV0) {
      v0row->dcav0 = - (v0row->dcav0);
    } else if (!(isXiV0 || passV0)) {

      // Want to delete this V0 and vertex
      // Take last good V0 and move it here
      Bool_t notGood = kTRUE;
      lastV0 = dst_v0_vertex->GetNRows();
      Long_t idVert = v0row->id_vertex;
      while (notGood) {
        if ((--lastV0)==iv0) {
          break;
	}
        dst_v0_vertex_st* v0rowL = dst_v0_vertex->GetTable(lastV0);
        isXiV0 = (v0rowL->dcav0 < 0);
        passV0 = (TMath::Abs(v0rowL->dcav0) < pars->dcav0) &&
            (v0rowL->dcap > pars->dcapnmin) && (v0rowL->dcan > pars->dcapnmin);
	if (isXiV0 || passV0) {
	  notGood = kFALSE;

	  // Move row to empty slot
	  Long_t newId = v0row->id;
	  Long_t oldId = v0rowL->id;
	  v0rowL->id = newId;
	  memcpy(v0row,v0rowL,rsize);
	  
	  if (isXiV0) { // Fix index in any Xis using this V0
	    // Will assume that Xis are ordered by increasing V0 index
	    while (ixi>=0) {
              dst_xi_vertex_st* xirow = dst_xi_vertex->GetTable(ixi);
	      if (xirow->id_v0 < oldId) break;
	      if (xirow->id_v0 == oldId) xirow->id_v0 = newId;
              ixi--;
	    }
	  }
	} else ChopVertex(v0rowL->id_vertex);
      }
      dst_v0_vertex->SetNRows(lastV0);
      ChopVertex(idVert);
      iv0--; // Repeat analysis of this V0
    }
  }

  dst_v0_vertex->Purge();
  vertex->Purge();
  gMessMgr->Info() << "StV0Maker::Trim(): saving " << dst_v0_vertex->GetNRows()
                   << " V0 candidates" << endm;
}
//_____________________________________________________________________________
void StV0Maker::ChopVertex(Long_t idVert){

  // Want to delete this vertex
  // Take last vertex and move it here
  Int_t ivert = (Int_t) idVert - 1;
  Int_t lastVert = vertex->GetNRows();
  if ((--lastVert)!=ivert) {
    dst_vertex_st* vertrow = vertex->GetTable(ivert);
    dst_vertex_st* vertrowL = vertex->GetTable(lastVert);
    Long_t idVertL = vertrowL->id;
    memcpy(vertrow,vertrowL,rvsize);
    vertrow->id = idVert;
    Int_t i;
    switch (vertrowL->vtx_id) {
      case kV0DecayIdentifier : {
        for (i = lastV0; i>0 ; i--) {
          dst_v0_vertex_st* v0row = dst_v0_vertex->GetTable(i-1);
          if (v0row->id_vertex == idVertL) {
            v0row->id_vertex = idVert;
            break;
          }
        }
        if (!i)
          gMessMgr->Warning("StV0Maker::ChopVertex(): v0 vertex not found");
        break; }
      case kXiDecayIdentifier : {
        for (i = dst_xi_vertex->GetNRows(); i>0 ; i--) {
          dst_xi_vertex_st* xirow = dst_xi_vertex->GetTable(i-1);
          if (xirow->id_xi == idVertL) {
            xirow->id_xi = idVert;
            break;
          }
        }
        if (!i)
          gMessMgr->Warning("StV0Maker::ChopVertex(): xi vertex not found");
        break; }
      case kKinkDecayIdentifier : {
	if (!dst_tkf_vertex) {
	  gMessMgr->Warning() << "StV0Maker::ChopVertex(): "
	    << " Deleting kink vertex, but kinks not done" << endm;
          break;
	}
        for (i = dst_tkf_vertex->GetNRows(); i>0 ; i--) {
          dst_tkf_vertex_st* kinkrow = dst_tkf_vertex->GetTable(i-1);
          if (kinkrow->id_vertex == idVertL) {
            kinkrow->id_vertex = idVert;
            break;
          }
        }
        if (!i)
          gMessMgr->Warning("StV0Maker::ChopVertex(): kink vertex not found");
        break; }
      default : gMessMgr->Warning() << "StV0Maker::ChopVertex(): "
	    << " Moving vertex of unkown type=" << vertrowL->vtx_id << endm;
    }
  }
  vertex->SetNRows(lastVert);
}
