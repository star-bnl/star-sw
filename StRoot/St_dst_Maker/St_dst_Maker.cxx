// $Id: St_dst_Maker.cxx,v 1.6 1999/02/20 18:49:16 fisyak Exp $
// $Log: St_dst_Maker.cxx,v $
// Revision 1.6  1999/02/20 18:49:16  fisyak
// Add event/run information
//
// Revision 1.5  1999/02/19 17:37:42  fisyak
// Add RICH hits to dst
//
// Revision 1.4  1999/02/19 17:35:47  fisyak
// Add RICH hits to dst
//
// Revision 1.2  1999/01/20 23:58:03  fisyak
// Tree 2 GetTree
//
// Revision 1.1  1999/01/02 19:09:22  fisyak
// Add Clones
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:29  perev
// cleanup
//
// Revision 1.5  1998/10/02 13:46:08  fine
// DataSet->DataSetIter
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_dst_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TClass.h"
#include "St_dst_Maker.h"
#include "StChain.h"
#include "St_ObjectSet.h"
#include "St_DataSetIter.h"
#include "TRandom.h"
#include "TSystem.h"
#include "St_particle_Table.h"
#include "St_dst_event_header_Table.h"
#include "St_dst_track_Table.h"
#include "St_dst_track_aux_Table.h"
#include "St_dst_vertex_Table.h"
#include "St_dst_v0_vertex_Table.h"
#include "St_dst_xi_vertex_Table.h"
#include "St_dst_dedx_Table.h"
#include "St_dst_point_Table.h"
#include "St_dst_event_summary_Table.h"
#include "St_dst_monitor_soft_Table.h"
#include "St_dst_TriggerDetectors_Table.h"
#include "St_tpt_track_Table.h"
#include "St_g2t_rch_hit_Table.h"
#include "St_dst_rch_Table.h"


ClassImp(St_dst_Maker)

//_____________________________________________________________________________
St_dst_Maker::St_dst_Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_dst_Maker::~St_dst_Maker(){
}
//_____________________________________________________________________________
Int_t St_dst_Maker::Init(){
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_dst_Maker::Make(){
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
    St_DataSet *geant = gStChain->DataSet("geant");
    if (geant) {
      St_DataSetIter geantI(geant);
      St_particle *particle = (St_particle *) geantI["particle"];
      if (particle) m_DataSet->Shunt(particle);
      St_g2t_rch_hit *g2t_rch_hit = (St_g2t_rch_hit *) geantI("g2t_rch_hit");
      if (g2t_rch_hit) m_DataSet->Shunt(g2t_rch_hit);
    }
    St_DataSet *global = gStChain->DataSet("global");
    if (global) {
      St_DataSetIter dstI(global);
      St_DataSet *dst = dstI("dst");
      m_DataSet->Update(dst);
#if 0
      dstI.Cd("dst");
      St_dst_event_header  *event_header  = (St_dst_event_header  *) dstI("event_header");
      St_dst_track      *globtrk     = (St_dst_track     *) dstI("globtrk");
      St_dst_track_aux  *globtrk_aux = (St_dst_track_aux *) dstI("globtrk_aux");
      St_dst_track      *globtrk2     = (St_dst_track     *) dstI("globtrk2");
      St_dst_track      *primtrk     = (St_dst_track     *) dstI("primtrk");
      St_dst_track_aux  *primtrk_aux = (St_dst_track_aux *) dstI("primtrk_aux");
      St_dst_vertex     *vertex      = (St_dst_vertex    *) dstI("vertex");
      St_dst_v0_vertex  *dst_v0_vertex = (St_dst_v0_vertex    *) dstI("dst_v0_vertex"); 
      St_dst_xi_vertex  *dst_xi_vertex = (St_dst_xi_vertex    *) dstI("dst_xi_vertex");
      St_dst_dedx       *dst_dedx    = (St_dst_dedx      *) dstI("dst_dedx");
      St_dst_point      *point       = (St_dst_point     *) dstI("point");
      St_dst_event_summary *event_summary = (St_dst_event_summary *) dstI("event_summary");
      St_dst_monitor_soft  *monitor_soft  = (St_dst_monitor_soft  *) dstI("monitor_soft");
      

      if (event_header) m_DataSet->Shunt(event_header);
      if (globtrk)      m_DataSet->Shunt(globtrk);
      if (globtrk2)     m_DataSet->Shunt(globtrk2);
      if (globtrk_aux)  m_DataSet->Shunt(globtrk_aux);
      if (primtrk)      m_DataSet->Shunt(primtrk);
      if (primtrk_aux)  m_DataSet->Shunt(primtrk_aux);
      if (vertex)       m_DataSet->Shunt(vertex);
      if (dst_v0_vertex)m_DataSet->Shunt(dst_v0_vertex);
      if (dst_xi_vertex)m_DataSet->Shunt(dst_xi_vertex);
      if (dst_dedx)     m_DataSet->Shunt(dst_dedx);
      if (point)        m_DataSet->Shunt(point);
      if (event_summary)m_DataSet->Shunt(event_summary);
      if (monitor_soft) m_DataSet->Shunt(monitor_soft);
#endif
    }
    St_DataSet *trg = gStChain->DataSet("trg");
    if (trg) {
      St_DataSetIter trgI(trg);
      St_dst_TriggerDetectors *dst = (St_dst_TriggerDetectors *) trgI("dst_TriggerDetectors");
      if (dst)          m_DataSet->Shunt(dst);
    }
    St_DataSet *l3t = gStChain->DataSet("l3Tracks");
    if (l3t) {
      St_DataSetIter l3tI(l3t);
      St_tpt_track   *track = (St_tpt_track *) l3tI("tptrack");
      if (track)         m_DataSet->Shunt(track);
    }
  }
  return kStOK;
}
//_____________________________________________________________________________
void St_dst_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_dst_Maker.cxx,v 1.6 1999/02/20 18:49:16 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

