//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StXiMaker class                                                    //
//                                                                      //
// $Id: StXiMaker.cxx,v 1.21 2003/09/23 01:18:18 jeromel Exp $
// $Log: StXiMaker.cxx,v $
// Revision 1.21  2003/09/23 01:18:18  jeromel
// READ_UNINIT_MEM(read) problem fixed (of course used in a if() statement)
//
// Revision 1.20  2003/09/02 17:59:26  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.19  2002/01/31 18:07:49  genevb
// Switch to using database for cut parameters
//
// Revision 1.18  2001/03/05 17:16:17  genevb
// Reduce vertex table buffer size slightly
//
// Revision 1.17  2001/03/02 04:37:16  genevb
// Tightened some DCA cuts
//
// Revision 1.16  2000/08/31 21:47:08  genevb
// Allow V0s to be trimmed after finding Xis
//
// Revision 1.15  2000/06/13 13:04:21  genevb
// Fixed bug with not finding primary vertex
//
// Revision 1.14  2000/03/30 22:32:07  genevb
// Fixed a typo
//
// Revision 1.13  2000/03/30 16:33:39  genevb
// Change messages to say where they are called from
//
// Revision 1.12  1999/11/15 22:02:49  lbarnby
// Correct check in mistake. Ignore previous comment
//
// Revision 1.11  1999/11/15 21:54:24  lbarnby
// changes in idl file to fix St_dst_Maker
//
// Revision 1.10  1999/09/29 20:29:10  wdeng
// Accommodate dst_track and dst_vertex change
//
// Revision 1.9  1999/09/12 23:03:04  fisyak
// Move parameters into makers
//
// Revision 1.8  1999/07/17 19:00:21  fisyak
// Add sanctimonious checks
//
// Revision 1.7  1999/07/17 00:31:25  genevb
// Use StMessMgr
//
// Revision 1.6  1999/07/15 13:57:54  perev
// cleanup
//
// Revision 1.5  1999/07/12 23:04:17  fisyak
// Remove glob2
//
// Revision 1.4  1999/07/12 01:49:39  fine
// Clean up
//
// Revision 1.3  1999/07/08 19:09:53  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include <string.h>

#include "TMath.h"
#include "StXiMaker.h"

#include "StVertexId.h"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "global/St_exiam_Module.h"

#include "StV0Maker.h"

ClassImp(StXiMaker)
  
  //_____________________________________________________________________________
  StXiMaker::StXiMaker(const char *name):StMaker(name)
{
  m_exiaux = 0;
  m_exipar = 0;
  drawinit=kFALSE;
}
//_____________________________________________________________________________
StXiMaker::~StXiMaker(){
}
//_____________________________________________________________________________
Int_t StXiMaker::Init(){
  TDataSet* dbDataSet = GetDataBase("global/vertices");
  if (!dbDataSet) {
    gMessMgr->Error("StXiMaker::Init(): could not find appropriate database");
    return kStErr;
  }
  m_exipar = (St_exi_exipar*) (dbDataSet->FindObject("exipar"));
  if (!m_exipar) {
    gMessMgr->Error("StXiMaker::Init(): could not find exipar in database");
    return kStErr;
  }
  if (!m_exiaux) m_exiaux = new St_exi_aux("exi_aux",1);

  AddRunCont(m_exipar);
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StXiMaker::Make(){
  //  if(Debug()) gMessMgr->Debug("StXiMaker::Make(): Calling exi...");
  PrintInfo();
  
  int iMake = kStOK;
  int iRes = 0;

  St_DataSet *match = GetDataSet("match"); 
  if (!match) return kStWarn;
  St_DataSetIter matchI(match);         
  St_dst_track     *globtrk  = (St_dst_track *) matchI("globtrk");

  St_DataSet     *primary = GetDataSet("primary"); 
  if (!primary) return kStWarn;
  St_DataSetIter primaryI(primary);         
  St_dst_vertex  *vertex   = (St_dst_vertex *) primaryI("vertex");

  St_DataSet *v0 = GetDataSet("v0"); 
  if (!v0) return kStWarn;
  St_DataSetIter v0I(v0);         
  St_dst_v0_vertex *dst_v0_vertex  = (St_dst_v0_vertex *) v0I("dst_v0_vertex");
  St_dst_xi_vertex  *dst_xi_vertex = 0;
  
  if (!globtrk || !vertex || !dst_v0_vertex) return kStWarn;
  dst_vertex_st *vrtx = vertex->GetTable();
  for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,vrtx++) {
  // Above loop runs until primary vertex is found. When found, the code
  // below is executed, and a "break;" gets out of the for-loop. If no
  // primary vertex is found, the loop just ends normally, doing nothing.
  if (vrtx->vtx_id == kEventVtxId && vrtx->iflag == 1) {
    Int_t xi_limit = dst_v0_vertex->GetNRows();
    if (xi_limit < 250) xi_limit=250;
    dst_xi_vertex = new St_dst_xi_vertex("dst_xi_vertex",xi_limit);
    AddData(dst_xi_vertex);
  
    iRes = exiam(m_exipar,globtrk,vertex,dst_v0_vertex,dst_xi_vertex,m_exiaux);
  //	 ===================================================================
  
    if (iRes != kSTAFCV_OK) iMake = kStWarn;
    if (iRes != kSTAFCV_OK) {
      gMessMgr->Warning("StXiMaker::Make(): Problem on return from EXI");
    }
    for (Int_t ixi = 0; ixi < dst_xi_vertex->GetNRows(); ixi++) {
      dst_xi_vertex_st* xirow = dst_xi_vertex->GetTable(ixi);
      dst_v0_vertex_st* v0row = dst_v0_vertex->GetTable(xirow->id_v0-1);
      v0row->dcav0 = - TMath::Abs(v0row->dcav0);
    }
    StV0Maker* v0mak = (StV0Maker*) GetMaker("v0");
    if (v0mak) v0mak->Trim(); // Trims extra V0s kept for looking for Xis
    break;
  } // If-then block if primary vertex found
  } // For-loop to find primary vertex
  return iMake;
}

//_____________________________________________________________________________

