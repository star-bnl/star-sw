// $Id: StRootEventManager.cc,v 1.6 1999/09/24 01:23:05 fisyak Exp $
// $Log: StRootEventManager.cc,v $
// Revision 1.6  1999/09/24 01:23:05  fisyak
// Reduced Include Path
//
// Revision 1.5  1999/07/24 00:25:50  fisyak
// Gene corrections
//
// Revision 1.4  1999/07/23 22:49:47  perev
// Remove redundant printout
//
// Revision 1.3  1999/07/11 23:27:50  fisyak
// dst_TriggerDetectors => dst_TrgDet
//
// Revision 1.2  1999/06/24 17:30:27  fisyak
// Add more print out
//
// Revision 1.1  1999/05/22 18:23:42  perev
// Add classes from StEventReaderMaker
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
// StRootEventManager
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
 * Removed remaining pieces of the RICH pixel table.
 *
 * Revision 2.3  2000/01/05 16:08:25  ullrich
 * Removed some unnecessary checks.
 *
 * Revision 2.2  1999/12/21 15:13:16  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
StRootEventManager::StRootEventManager():StEventManager()
{
}
 * Revision 2.1  1999/11/04 17:41:32  ullrich
StRootEventManager::~StRootEventManager()
{
}
using namespace std;
ooStatus StRootEventManager::openEvent(const char* name)
 *
//  dst.Reset(0);
#include "StRootEventManager.hh"
  if (!currentChain) 	return oocError;
  St_DataSet *dsDst = currentChain->GetDataSet(name);
  if (!dsDst) 		return oocError;
  if (!dsDst->GetList())return oocError;
  dst.Reset(dsDst);
  return oocSuccess;
StRootEventManager::StRootEventManager():StEventManager() { /* noop */ }
//  mDst.Reset(0);
ooStatus StRootEventManager::readEvent()
ooStatus
StRootEventManager::openEvent(const char* name)
    if (!dsDst->GetList())      return oocError;
    //  mDst.Reset(0);
    //  grab the event from the maker
void StRootEventManager::closeEvent()
{
}
    St_DataSet *dsDst = mCurrentChain->GetDataSet(name);
void StRootEventManager::setup()
{
}
StRootEventManager::readEvent()
void StRootEventManager::shutdown()
{
}

#define READ_TABLE(FUNCTION,TABLE_TYPE) \
ooStatus StRootEventManager::FUNCTION(TABLE_TYPE ## _st& table) const\
{\
  const char *nm =  #TABLE_TYPE;\
  St_DataSetIter *Dst = (St_DataSetIter*)&dst;\
  St_ ## TABLE_TYPE *tableWrap = (St_ ## TABLE_TYPE *) (*Dst)(nm);\
  if (tableWrap) {\
    table = *(tableWrap->GetTable());\
    return oocSuccess;\
  } else {\
    cout << "StRootEventManager: " << nm << " not found" << endl;\
    return oocError;\
  }\
}
READ_TABLE(readRunHeader,dst_run_header)
READ_TABLE(readRunSummary,dst_run_summary)
READ_TABLE(readHeader,dst_event_header)
READ_TABLE(readTable,dst_TrgDet)
READ_TABLE(readTable,dst_event_summary)
READ_TABLE(readTable,dst_monitor_hard)
READ_TABLE(readTable,dst_monitor_soft)
READ_TABLE(readTable,particle)

#undef READ_TABLE
    //  grab the event from the maker
    return oocSuccess;
}
    mDst.Reset(dsDst);
    return oocSuccess;
}

ooStatus
  St_DataSetIter *Dst = (St_DataSetIter*)&dst;\

void StRootEventManager::closeEvent() { /* noop */ }

void StRootEventManager::setup() { /* noop */ }

void StRootEventManager::shutdown() { /* noop */ }

    cout << "StRootEventManager: Table type  " << nt << \
TABLE_TYPE ##_st* StRootEventManager::returnTable_ ## TABLE_TYPE(long& nentries) const\
{\
  TABLE_TYPE ## _st* table = NULL;\
  St_ ## TABLE_TYPE  *tableWrap;\
  const char *nm =  #TABLE_NAME;\
  const char *nt =  #TABLE_TYPE;\
  St_DataSetIter *Dst = (St_DataSetIter*)&mDst;\
  tableWrap = (St_ ## TABLE_TYPE *) (*Dst)[nt];\
RETURN_TABLE(dst_monitor_soft,monitor_soft)
RETURN_TABLE(dst_monitor_hard,monitor_hard)
    nentries = tableWrap->GetNRows();\
  else {\
RETURN_TABLE(dst_event_header,event_header)
RETURN_TABLE(dst_run_summary,run_summary)
RETURN_TABLE(dst_run_header,run_header)
RETURN_TABLE(dst_dedx,dedx)
RETURN_TABLE(dst_point,point)
RETURN_TABLE(dst_tof_evt,tof_evt)
RETURN_TABLE(dst_tof_trk,tof_trk)
RETURN_TABLE(dst_track,globtrk)
RETURN_TABLE(dst_track_aux,globtrk_aux)
RETURN_TABLE(dst_vertex,vertex)
RETURN_TABLE(dst_v0_vertex,v0_vertex)
RETURN_TABLE(dst_xi_vertex,xi_vertex)
RETURN_TABLE(dst_mon_soft_emc,mon_soft_emc)    
#undef RETURN_TABLE
    const char *nt =  "St_dst_track";
    St_DataSetIter *Dst = (St_DataSetIter*)&mDst;
    tableWrap = (St_dst_track*) (*Dst)[nt];
    if (!tableWrap && nm[0]!='-') tableWrap = (St_dst_track*) (*Dst)[nm];
    if (tableWrap) {
	table = tableWrap->GetTable();
	nentries = tableWrap->GetNRows();
    }
    else {
	cerr << "StRootEventManager: Table type  " << nt << 
	    " - name " << nm << " not found in DataSet " << Dst->Pwd()->GetName() << endl;
	nentries = 0;
    }
    return table;
}
