// $Id: StEventManager.hh,v 1.3 1999/09/01 20:26:40 fisyak Exp $
//
// $Log: StEventManager.hh,v $
// Revision 1.3  1999/09/01 20:26:40  fisyak
// remove double includes
//
// Revision 1.2  1999/07/11 23:27:50  fisyak
// dst_TriggerDetectors => dst_TrgDet
//
// Revision 1.1  1999/05/22 18:23:42  perev
// Add classes from StEventReaderMaker
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

 * Removed remaining pieces of the RICH pixel table.
 *
 * Revision 2.0  1999/11/04 19:03:00  ullrich
///////////////////////////////////////////////////////////////////////////////
//
// StEventManager
//
// Description: 
//  Class to emulate the Objectivity event store manager class and serve
//  up events from 
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
 * Revised to build new StEvent version
 *
#include "tables/St_dst_run_header_Table.h"
#include "tables/St_dst_run_summary_Table.h"
#include "tables/St_dst_TrgDet_Table.h"
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
 **************************************************************************/
#ifndef StEventManager_HH
#include "tables/St_dst_track_Table.h"               
//VP#include "StMaker.h"
#include "tables/St_dst_v0_vertex_Table.h"           
#include "tables/St_dst_vertex_Table.h"              
#include "tables/St_dst_xi_vertex_Table.h"           
#include "tables/St_event_header_Table.h"            
class StEventManager{


  StEventManager(){currentChain=0;event=0;};
  virtual ~StEventManager(){};

  virtual ooStatus openEvent(const char* colName)=0;
  virtual ooStatus readEvent()=0;
  virtual void closeEvent(){};
  virtual void setup(){};
  virtual void shutdown(){};

  virtual ooStatus readRunHeader(dst_run_header_st&   runInfo) const =0;
  virtual ooStatus readRunSummary(dst_run_summary_st& runInfo) const =0;
  virtual ooStatus readHeader(dst_event_header_st&    eventInfo) const =0;

  virtual ooStatus readTable(dst_TrgDet_st& table) const =0;
  virtual ooStatus readTable(dst_event_summary_st&    table) const =0;
  virtual ooStatus readTable(dst_monitor_hard_st&     table) const =0;
  virtual ooStatus readTable(dst_monitor_soft_st&     table) const =0;
  virtual ooStatus readTable(particle_st&             table) const =0;

  virtual particle_st*  	   returnTable_particle            (long& nentries) const =0;
  virtual dst_monitor_soft_st* 	   returnTable_dst_monitor_soft    (long& nentries) const =0;
  virtual dst_monitor_hard_st* 	   returnTable_dst_monitor_hard    (long& nentries) const =0;
  virtual dst_event_summary_st*    returnTable_dst_event_summary   (long& nentries) const =0;
  virtual dst_TrgDet_st*           returnTable_dst_TrgDet          (long& nentries) const =0;
  virtual dst_event_header_st* 	   returnTable_dst_event_header    (long& nentries) const =0;
  virtual dst_run_summary_st* 	   returnTable_dst_run_summary     (long& nentries) const =0;
  virtual dst_run_header_st* 	   returnTable_dst_run_header      (long& nentries) const =0;

  virtual dst_dedx_st* 	           returnTable_dst_dedx       	   (long& nentries) const =0;
  virtual dst_point_st*            returnTable_dst_point    	   (long& nentries) const =0;
  virtual dst_tof_evt_st*          returnTable_dst_tof_evt  	   (long& nentries) const =0;
  virtual dst_tof_trk_st*          returnTable_dst_tof_trk  	   (long& nentries) const =0;
  virtual dst_track_st*            returnTable_dst_track    	   (long& nentries) const =0;
  virtual dst_track_aux_st*        returnTable_dst_track_aux	   (long& nentries) const =0;
  virtual dst_vertex_st*           returnTable_dst_vertex   	   (long& nentries) const =0;
  virtual dst_v0_vertex_st*   	   returnTable_dst_v0_vertex	   (long& nentries) const =0;
  virtual dst_xi_vertex_st* 	   returnTable_dst_xi_vertex	   (long& nentries) const =0;

  virtual void setMaker(StMaker * mk) {currentChain=mk;};

    virtual dst_v0_vertex_st*      returnTable_dst_v0_vertex(long&)     const =0;               
  StMaker* 	 currentChain;
  St_DataSet* 	 event;

    

    virtual void setMaker(StMaker* mk) {mCurrentChain=mk;};
    
protected:
    StMaker* 	 mCurrentChain;
    St_DataSet*  mEvent;
};
#endif
