// $Id: StEventManager.h,v 1.1 1999/05/01 01:49:14 fisyak Exp $
//
// $Log: StEventManager.h,v $
// Revision 1.1  1999/05/01 01:49:14  fisyak
// Add StRootEvent fill
//
// Revision 1.9  1999/04/16 02:43:43  fine
// Problem with the scope of the long i was solved. Some improvement as well
//
// Revision 1.8  1999/03/30 15:47:32  wenaus
// update to new maker scheme
//
// Revision 1.7  1999/03/19 17:23:13  wenaus
// Load trigger detectors
//
// Revision 1.6  1999/02/24 18:30:33  wenaus
// dst_tof elimination; changes for ROOT
//
// Revision 1.5  1999/02/24 01:56:14  genevb
// Add Xi vertices
//
// Revision 1.4  1999/02/22 15:48:35  wenaus
// remove the oo.h include that snuck in somehow!
//
// Revision 1.3  1999/02/22 02:36:29  wenaus
// handling of new tof tables
//
// Revision 1.2  1999/02/09 13:46:00  wenaus
// Remove ref to StUtil
//
// Revision 1.1  1999/01/30 23:06:39  wenaus
// Maker to read from tables or Objy into StEvent
//

#ifndef StEventManager_HH
#define StEventManager_HH

///////////////////////////////////////////////////////////////////////////////
//
// StEventManager
//
// Description: 
//  Class to emulate the Objectivity event store manager class and serve
//  up events from XDF
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus, BNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
// suppress 'bool' definition in ObjectSpace stuff
//#define OS_OMIT_BOOL        Needed only if Star.hh is included
//#include "StUtil/Star.hh"   Not needed at present
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/dst_run_header.h"
#include "tables/dst_run_summary.h"
#include "tables/dst_TriggerDetectors.h"
#include "tables/dst_dedx.h"
#include "tables/dst_event_header.h"
#include "tables/dst_event_summary.h"
#include "tables/dst_monitor_hard.h"
#include "tables/dst_monitor_soft.h"
#include "tables/dst_point.h"
#include "tables/dst_tof_evt.h"
#include "tables/dst_tof_trk.h"
#include "tables/dst_track.h"
#include "tables/dst_track_aux.h"
#include "tables/dst_vertex.h"
#include "tables/dst_v0_vertex.h"
#include "tables/dst_xi_vertex.h"
#include "tables/particle.h"
#include "tables/gen_header.h"

#include "tables/St_dst_run_header_Table.h"
#include "tables/St_dst_run_summary_Table.h"
#include "tables/St_dst_TriggerDetectors_Table.h"
#include "tables/St_dst_dedx_Table.h"
#include "tables/St_dst_event_header_Table.h"
#include "tables/St_dst_event_summary_Table.h"
#include "tables/St_dst_monitor_hard_Table.h"
#include "tables/St_dst_monitor_soft_Table.h"
#include "tables/St_dst_point_Table.h"
#include "tables/St_dst_tof_evt_Table.h"
#include "tables/St_dst_tof_trk_Table.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_track_aux_Table.h"
#include "tables/St_dst_vertex_Table.h"
#include "tables/St_dst_v0_vertex_Table.h"
#include "tables/St_dst_xi_vertex_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_gen_header_Table.h"

typedef int ooStatus;
#define oocSuccess		((ooStatus) 1)
#define oocError		((ooStatus) 0)

class StMaker;

class StEventManager{

public:
  StEventManager();
  ~StEventManager();
  void ResetDstIter(St_DataSet *set){if (set) dst.Reset(set);};

  ooStatus readRunHeader(dst_run_header_st& runInfo);
  ooStatus readRunSummary(dst_run_summary_st& runInfo);
  void readHeader(dst_event_header_st& eventInfo);


  ooStatus readTable(dst_TriggerDetectors_st& table);
  ooStatus readTable(dst_event_summary_st& table);
  ooStatus readTable(dst_monitor_hard_st& table);
  ooStatus readTable(dst_monitor_soft_st& table);
  ooStatus readTable(particle_st& table);

  dst_dedx_st* returnTable_dst_dedx(long& nentries);
  dst_point_st* returnTable_dst_point(long& nentries);
  dst_tof_evt_st* returnTable_dst_tof_evt(long& nentries);
  dst_tof_trk_st* returnTable_dst_tof_trk(long& nentries);
  dst_track_st* returnTable_dst_track(long& nentries);
  dst_track_aux_st* returnTable_dst_track_aux(long& nentries);
  dst_vertex_st* returnTable_dst_vertex(long& nentries);
  dst_v0_vertex_st* returnTable_dst_v0_vertex(long& nentries);
  dst_xi_vertex_st* returnTable_dst_xi_vertex(long& nentries);


private:

  St_DataSet* event;
  St_DataSetIter dst;

  };

#endif
