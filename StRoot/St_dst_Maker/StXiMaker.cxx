//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StXiMaker class                                                    //
//                                                                      //
// $Id: StXiMaker.cxx,v 1.5 1999/07/12 23:04:17 fisyak Exp $
// $Log: StXiMaker.cxx,v $
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

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

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
  St_DataSet *globalParams = GetInputDB("params/global");
  assert(globalParams);
  St_DataSetIter params(globalParams);

  if (!m_exiaux) m_exiaux = new St_exi_aux("exi_aux",1);
  m_exipar = (St_exi_exipar *)  params("exipars/exipar");
  if (!m_exipar) {
    m_exipar = new St_exi_exipar("exipar",3);
    //    AddConst(m_exipar);
    // m_exipar->SetNRows(3);
  }
  AddConst(m_exipar);
  m_exipar->SetNRows(3);
  exi_exipar_st *exipar = m_exipar->GetTable();
  
  // TPC only cuts
  
  exipar->use_pid = 0;
  exipar->dca_max = 1.;
  exipar->bxi_max = 1.;
  exipar->rv_xi   = 2.;
  exipar->rv_v0   = 5.;
  exipar->dmass   = 0.01;
  exipar->bpn_v0  = 2.;
  exipar->pchisq  = 0.;
  exipar++;
  
  //SVT only cuts
  
  exipar->use_pid = 0;
  exipar->dca_max = 0.;
  exipar->bxi_max = 0.;
  exipar->rv_xi   = 999.;
  exipar->rv_v0   = 999.;
  exipar->dmass   = 0.;
  exipar->bpn_v0  = 999.;
  exipar->pchisq  = 0.;
  exipar++;
  
  // SVT+TPC cuts
  
  exipar->use_pid = 0;
  exipar->dca_max = 1.;
  exipar->bxi_max = 1.;
  exipar->rv_xi   = 2.;
  exipar->rv_v0   = 5.;
  exipar->dmass   = 0.01;
  exipar->bpn_v0  = 2.;
  exipar->pchisq  = 0.;
  exipar++;

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StXiMaker::Make(){
  //  if(Debug()) cout << "Calling exi..."<< endl;  
  PrintInfo();
  
  int iMake = kStOK;
  int iRes = 0;

  St_DataSet *match = GetDataSet("match"); 
  St_DataSetIter matchI(match);         
  St_dst_track     *globtrk  = (St_dst_track *) matchI("globtrk");

  St_DataSet     *primary = GetDataSet("primary"); 
  St_DataSetIter primaryI(primary);         
  St_dst_vertex  *vertex   = (St_dst_vertex *) primaryI("vertex");

  St_DataSet *v0 = GetDataSet("v0"); 
  St_DataSetIter v0I(v0);         
  St_dst_v0_vertex *dst_v0_vertex  = (St_dst_v0_vertex *) v0I("dst_v0_vertex");
  St_dst_xi_vertex  *dst_xi_vertex = 0;
  
  dst_track_st *glob  = globtrk->GetTable();
  dst_vertex_st *vrtx = vertex->GetTable();
  if( vrtx->vtx_id != 1 || vrtx->iflag != 1){
    for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,vrtx++){
      if( vrtx->vtx_id == 1 && vrtx->iflag == 1 ) break;
    }
  }
  if (vrtx->vtx_id == 1 && vrtx->iflag == 1) {
    Int_t xi_limit = 2*dst_v0_vertex->GetNRows();
    if (xi_limit < 250) xi_limit=250;
    dst_xi_vertex = new St_dst_xi_vertex("dst_xi_vertex",xi_limit);
    AddData(dst_xi_vertex);
  
    iRes = exiam(m_exipar,globtrk,vertex,dst_v0_vertex,dst_xi_vertex,m_exiaux);
  //	 ===================================================================
  
    if (iRes != kSTAFCV_OK) iMake = kStWarn;
    if (iRes != kSTAFCV_OK) {cout << " Problem on return from EXI " << endl;}
  }
  return iMake;
}

//_____________________________________________________________________________
void StXiMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StXiMaker.cxx,v 1.5 1999/07/12 23:04:17 fisyak Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();

}

