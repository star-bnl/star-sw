// $Id: StEventManager.cxx,v 1.2 1999/05/03 01:39:21 fisyak Exp $
// $Log: StEventManager.cxx,v $
// Revision 1.2  1999/05/03 01:39:21  fisyak
// Remove tables from DST, add access to different makers
//
// Revision 1.1  1999/05/01 01:49:14  fisyak
// Add StRootEvent fill
//
// Revision 1.10  1999/04/16 02:43:42  fine
// Problem with the scope of the long i was solved. Some improvement as well
//
// Revision 1.9  1999/04/15 18:24:55  wenaus
// Follow a g++ compiler suggestion
//
// Revision 1.8  1999/03/30 15:47:31  wenaus
// update to new maker scheme
//
// Revision 1.7  1999/03/19 17:23:13  wenaus
// Load trigger detectors
//
// Revision 1.6  1999/02/24 23:14:16  wenaus
// EOF recognition for ROOT files
//
// Revision 1.5  1999/02/24 18:30:32  wenaus
// dst_tof elimination; changes for ROOT
//
// Revision 1.4  1999/02/24 01:56:14  genevb
// Add Xi vertices
//
// Revision 1.3  1999/02/22 02:36:28  wenaus
// handling of new tof tables
//
// Revision 1.2  1999/02/10 23:15:20  wenaus
// multi-file processing changes; leak fixes
//
// Revision 1.1  1999/01/30 23:06:38  wenaus
// Maker to read from tables or Objy into StEvent
//
//
///////////////////////////////////////////////////////////////////////////////
//
// StEventManager
//
// Description: 
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus (BNL)
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StEventManager.h"
#include "StChain/StChain.h"
#include "TString.h"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

StEventManager::StEventManager():event(0)
{
//   dst = 0;
}

StEventManager::~StEventManager()
{
}


ooStatus StEventManager::readRunHeader(dst_run_header_st& table)
{
  TString nm("dst/run_header");
//VF  St_dst_run_header *tableWrap = (St_dst_run_header *) dst->Next(nm.Data()];
  St_dst_run_header *tableWrap = (St_dst_run_header *) dst[nm.Data()];
  if (tableWrap && tableWrap->GetTable()) {
    table = *(tableWrap->GetTable());
  } else {
    // cout << "StEventManager: " << nm << " not found" << endl;
    return oocError;
  }
  return oocSuccess;
}

ooStatus StEventManager::readRunSummary(dst_run_summary_st& table)
{
  TString nm = "dst/run_summary";  
//VF  St_dst_run_summary *tableWrap = (St_dst_run_summary *) dst->Next(nm.Data()];
  St_dst_run_summary *tableWrap = (St_dst_run_summary *) dst[nm.Data()];
  if (tableWrap && tableWrap->GetTable()) {
    table = *(tableWrap->GetTable());
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    return oocError;
  }
  return oocSuccess;
}

void StEventManager::readHeader(dst_event_header_st& table)
{
  TString nm = "dst/event_header";  
//VF  St_dst_event_header *tableWrap = (St_dst_event_header *) dst->Next(nm.Data()];
  St_dst_event_header *tableWrap = (St_dst_event_header *) dst[nm.Data()];
  if (tableWrap) {
    table = *(tableWrap->GetTable());
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
  }
}

ooStatus StEventManager::readTable(dst_TriggerDetectors_st& table)
{
  TString nm = "dst_TriggerDetectors";  
//VF  St_dst_TriggerDetectors *tableWrap = (St_dst_TriggerDetectors *) dst->Next(nm.Data()];
  St_dst_TriggerDetectors *tableWrap = (St_dst_TriggerDetectors *) dst[nm.Data()];
  if (tableWrap) {
    table = *(tableWrap->GetTable());
    return oocSuccess;
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    return oocError;
  }
}

ooStatus StEventManager::readTable(dst_event_summary_st& table)
{
  TString nm = "dst/event_summary";  
//VF   St_dst_event_summary *tableWrap = (St_dst_event_summary *) dst->Next(nm.Data()];
  St_dst_event_summary *tableWrap = (St_dst_event_summary *) dst[nm.Data()];
  if (tableWrap) {
    table = *(tableWrap->GetTable());
    return oocSuccess;
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    return oocError;
  }
}

ooStatus StEventManager::readTable(dst_monitor_hard_st& table)
{
  TString nm = "dst/monitor_hard";  
//VF   St_dst_monitor_hard *tableWrap = (St_dst_monitor_hard *) dst->Next(nm.Data()];
  St_dst_monitor_hard *tableWrap = (St_dst_monitor_hard *) dst[nm.Data()];
  if (tableWrap) {
    table = *(tableWrap->GetTable());
    return oocSuccess;
  } else {
    // Don't complain; it's never there
    // cout << "StEventManager: " << nm << " not found" << endl;
    return oocError;
  }
}

ooStatus StEventManager::readTable(dst_monitor_soft_st& table)
{
  TString nm = "dst/monitor_soft";  
//VF  St_dst_monitor_soft *tableWrap = (St_dst_monitor_soft *) dst->Next(nm.Data()];
  St_dst_monitor_soft *tableWrap = (St_dst_monitor_soft *) dst[nm.Data()];
  if (tableWrap) {
    table = *(tableWrap->GetTable());
    return oocSuccess;
  } else {
    // Don't complain; it's hardly ever there
    // cout << "StEventManager: " << nm << " not found" << endl;
    return oocError;
  }
}

ooStatus StEventManager::readTable(particle_st& table)
{
  TString nm = "particle";  
//VF  St_particle *tableWrap = (St_particle *) dst->Next(nm.Data()];
  St_particle *tableWrap = (St_particle *) dst[nm.Data()];
  if (tableWrap) {
    table = *(tableWrap->GetTable());
    return oocSuccess;
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    return oocError;
  }
}

dst_dedx_st* StEventManager::returnTable_dst_dedx(long& nentries)
{
  dst_dedx_st* table = NULL;
  TString nm = "dst/dst_dedx";  
//VF  St_dst_dedx *tableWrap = (St_dst_dedx *) dst->Next(nm.Data()];
  St_dst_dedx *tableWrap = (St_dst_dedx *) dst[nm.Data()];
  if (tableWrap) {
    table = tableWrap->GetTable();
    nentries = tableWrap->GetNRows();
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    nentries = 0;
  }
  return table;
}

dst_point_st* StEventManager::returnTable_dst_point(long& nentries)
{
  dst_point_st* table = NULL;
  TString nm = "dst/point";  
//VF  St_dst_point *tableWrap = (St_dst_point *) dst->Next(nm.Data()];
  St_dst_point *tableWrap = (St_dst_point *) dst[nm.Data()];
  if (tableWrap) {
    table = tableWrap->GetTable();
    nentries = tableWrap->GetNRows();
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    nentries = 0;
  }
  return table;
}

dst_tof_evt_st* StEventManager::returnTable_dst_tof_evt(long& nentries)
{
  dst_tof_evt_st* table = NULL;
  TString nm = "dst/dst_tof_evt";  
//VF  St_dst_tof_evt *tableWrap = (St_dst_tof_evt *) dst->Next(nm.Data()];
  St_dst_tof_evt *tableWrap = (St_dst_tof_evt *) dst[nm.Data()];
  if (tableWrap) {
    table = tableWrap->GetTable();
    nentries = tableWrap->GetNRows();
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    nentries = 0;
  }
  return table;
}

dst_tof_trk_st* StEventManager::returnTable_dst_tof_trk(long& nentries)
{
  dst_tof_trk_st* table = NULL;
  TString nm = "dst/dst_tof_trk";  
//  St_dst_tof_trk *tableWrap = (St_dst_tof_trk *) dst->Next(nm.Data()];
  St_dst_tof_trk *tableWrap = (St_dst_tof_trk *) dst[nm.Data()];
  if (tableWrap) {
    table = tableWrap->GetTable();
    nentries = tableWrap->GetNRows();
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    nentries = 0;
  }
  return table;
}

dst_track_st* StEventManager::returnTable_dst_track(long& nentries)
{
  dst_track_st* table = NULL;
  St_dst_track *tableWrap;
  TString nm = "dst/globtrk";  
//  tableWrap = (St_dst_track *) dst->Next(nm.Data()];
  tableWrap = (St_dst_track *) dst[nm.Data()];
  if (tableWrap) {
    table = tableWrap->GetTable();
    nentries = tableWrap->GetNRows();
    cout << nm << " found" << endl;
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    nentries = 0;
  }
  return table;
}

dst_track_aux_st* StEventManager::returnTable_dst_track_aux(long& nentries)
{
  dst_track_aux_st* table = NULL;
  TString nm = "dst/globtrk_aux";  
//VF  St_dst_track_aux *tableWrap = (St_dst_track_aux *) dst->Next(nm.Data()];
  St_dst_track_aux *tableWrap = (St_dst_track_aux *) dst[nm.Data()];
  if (tableWrap) {
    table = tableWrap->GetTable();
    nentries = tableWrap->GetNRows();
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    nentries = 0;
  }
  return table;
}

dst_vertex_st* StEventManager::returnTable_dst_vertex(long& nentries)
{
  dst_vertex_st* table = NULL;
  TString nm = "dst/vertex";  
//VF  St_dst_vertex *tableWrap = (St_dst_vertex *) dst->Next(nm.Data()];
  St_dst_vertex *tableWrap = (St_dst_vertex *) dst[nm.Data()];
  if (tableWrap) {
    table = tableWrap->GetTable();
    nentries = tableWrap->GetNRows();
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    nentries = 0;
  }
  return table;
}

dst_v0_vertex_st* StEventManager::returnTable_dst_v0_vertex(long& nentries)
{
  dst_v0_vertex_st* table = NULL;
  TString nm = "dst/dst_v0_vertex";  
//VF  St_dst_v0_vertex *tableWrap = (St_dst_v0_vertex *) dst->Next(nm.Data()];
  St_dst_v0_vertex *tableWrap = (St_dst_v0_vertex *) dst[nm.Data()];
  if (tableWrap) {
    table = tableWrap->GetTable();
    nentries = tableWrap->GetNRows();
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    nentries = 0;
  }
  return table;
}

dst_xi_vertex_st* StEventManager::returnTable_dst_xi_vertex(long& nentries)
{
  dst_xi_vertex_st* table = NULL;
  TString nm = "dst/dst_xi_vertex";  
//VF  St_dst_xi_vertex *tableWrap = (St_dst_xi_vertex *) dst->Next(nm.Data()];
  St_dst_xi_vertex *tableWrap = (St_dst_xi_vertex *) dst[nm.Data()];
  if (tableWrap) {
    table = tableWrap->GetTable();
    nentries = tableWrap->GetNRows();
  } else {
    cout << "StEventManager: " << nm << " not found" << endl;
    nentries = 0;
  }
  return table;
}
