//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StXiMaker class                                                    //
//                                                                      //
// $Id: StXiMaker.cxx,v 1.14 2000/03/30 22:32:07 genevb Exp $
// $Log: StXiMaker.cxx,v $
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

#include <iostream.h>
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

ClassImp(StXiMaker)
  
  //_____________________________________________________________________________
  StXiMaker::StXiMaker(const char *name):StMaker(name)
{
  drawinit=kFALSE;
}
//_____________________________________________________________________________
StXiMaker::~StXiMaker(){
}
//_____________________________________________________________________________
Int_t StXiMaker::Init(){

  if (!m_exiaux) m_exiaux = new St_exi_aux("exi_aux",1);
  //  m_exipar = (St_exi_exipar *)  params("exipars/exipar");
  m_exipar = new St_exi_exipar("exipar",3);
  {
    exi_exipar_st row;
    //
    memset(&row,0,m_exipar->GetRowSize());
    // TPC only cuts
    row.use_pid	 =          0; // logical flag to control usage of global pid ;
    row.dca_max	 =          1; // cut on dca between the two tracks ;
    row.bxi_max	 =          1; // cut on impact param. of xi from prim. vertex ;
    row.rv_xi	 =          2; // cut on min. dist. of decay from prim. vertex ;
    row.rv_v0	 =          5; // cut on min. dist. of decay from prim. vertex ;
    row.dmass	 =       0.01; // v0 mass cut +/- [dmass] ;
    row.bpn_v0	 =          2; // cut on v0 pion daught. impact param. ;
    row.pchisq	 =          0; // cut on chi^2 probability of vertex fit;
    m_exipar->AddAt(&row,0);
    memset(&row,0,m_exipar->GetRowSize());
    //SVT only cuts
    row.use_pid	 =          0; // logical flag to control usage of global pid ;
    row.dca_max	 =          0; // cut on dca between the two tracks ;
    row.bxi_max	 =          0; // cut on impact param. of xi from prim. vertex ;
    row.rv_xi	 =        999; // cut on min. dist. of decay from prim. vertex ;
    row.rv_v0	 =        999; // cut on min. dist. of decay from prim. vertex ;
    row.dmass	 =          0; // v0 mass cut +/- [dmass] ;
    row.bpn_v0	 =        999; // cut on v0 pion daught. impact param. ;
    row.pchisq	 =          0; // cut on chi^2 probability of vertex fit;
    m_exipar->AddAt(&row,1);
    memset(&row,0,m_exipar->GetRowSize());
    // SVT+TPC cuts
    row.use_pid	 =          0; // logical flag to control usage of global pid ;
    row.dca_max	 =          1; // cut on dca between the two tracks ;
    row.bxi_max	 =          1; // cut on impact param. of xi from prim. vertex ;
    row.rv_xi	 =          2; // cut on min. dist. of decay from prim. vertex ;
    row.rv_v0	 =          5; // cut on min. dist. of decay from prim. vertex ;
    row.dmass	 =       0.01; // v0 mass cut +/- [dmass] ;
    row.bpn_v0	 =          2; // cut on v0 pion daught. impact param. ;
    row.pchisq	 =          0; // cut on chi^2 probability of vertex fit;
    m_exipar->AddAt(&row,2);
  }
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
  if( vrtx->vtx_id != kEventVtxId || vrtx->iflag != 1){
    for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,vrtx++){
      if( vrtx->vtx_id == kEventVtxId && vrtx->iflag == 1 ) break;
    }
  }
  if (vrtx->vtx_id == kEventVtxId && vrtx->iflag == 1) {
    Int_t xi_limit = 2*dst_v0_vertex->GetNRows();
    if (xi_limit < 250) xi_limit=250;
    dst_xi_vertex = new St_dst_xi_vertex("dst_xi_vertex",xi_limit);
    AddData(dst_xi_vertex);
  
    iRes = exiam(m_exipar,globtrk,vertex,dst_v0_vertex,dst_xi_vertex,m_exiaux);
  //	 ===================================================================
  
    if (iRes != kSTAFCV_OK) iMake = kStWarn;
    if (iRes != kSTAFCV_OK) {
      gMessMgr->Warning("StXiMaker::Make(): Problem on return from EXI");
    }
  }
  return iMake;
}

//_____________________________________________________________________________

