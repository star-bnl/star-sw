/***************************************************************************
 *
 * $Id: StRootEventManager.cc,v 2.0 1999/11/04 19:03:00 ullrich Exp $
 *
 * Author: Original version by T. Wenaus, BNL
 *         Revised version for new StEvent by T. Ullrich, Yale
 ***************************************************************************
 *
 * Description:
 * Concrete implementation of DST table server (here from ROOT).
 *
 ***************************************************************************
 *
 * $Log: StRootEventManager.cc,v $
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 * Revision 2.4  2000/05/25 14:44:47  ullrich
 * Removed remaining pieces of the RICH pixel table.
 *
 * Revision 2.3  2000/01/05 16:08:25  ullrich
 * Removed some unnecessary checks.
 *
 * Revision 2.2  1999/12/21 15:13:16  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.1  1999/11/04 17:41:32  ullrich
 * Fixed typo
using namespace std;
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 **************************************************************************/
#include "StRootEventManager.hh"
#include "StChain.h"
#include <string>
#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif

StRootEventManager::StRootEventManager():StEventManager() { /* noop */ }
//  mDst.Reset(0);
//  grab the event from the maker

ooStatus
StRootEventManager::openEvent(const char* name)
    if (!dsDst->GetList())      return oocError;
    //  mDst.Reset(0);
    //  grab the event from the maker
    if (!mCurrentChain) 	return oocError;
    St_DataSet *dsDst = mCurrentChain->GetDataSet(name);
    if (!dsDst) 		return oocError;
StRootEventManager::readEvent()
{
    //  grab the event from the maker
    return oocSuccess;
}
    mDst.Reset(dsDst);
    return oocSuccess;
}

ooStatus
StRootEventManager::readEvent() {return oocSuccess;}

void StRootEventManager::closeEvent() { /* noop */ }

void StRootEventManager::setup() { /* noop */ }

void StRootEventManager::shutdown() { /* noop */ }

#define RETURN_TABLE(TABLE_TYPE,TABLE_NAME) \
TABLE_TYPE ##_st* StRootEventManager::returnTable_ ## TABLE_TYPE(long& nentries) const\
{\
  TABLE_TYPE ## _st* table = NULL;\
  St_ ## TABLE_TYPE  *tableWrap;\
  const char *nm =  #TABLE_NAME;\
  const char *nt =  #TABLE_TYPE;\
  St_DataSetIter *Dst = (St_DataSetIter*)&mDst;\
  tableWrap = (St_ ## TABLE_TYPE *) (*Dst)[nt];\
  if (!tableWrap && nm[0]!='-') tableWrap = (St_ ## TABLE_TYPE *) (*Dst)[nm];\
  if (tableWrap) {\
    table = tableWrap->GetTable();\
    nentries = tableWrap->GetNRows();\
  } \
  else {\
    cerr << "StRootEventManager: Table type  " << nt << \
	  " - name " << nm << " not found in DataSet " << Dst->Pwd()->GetName() << endl;\
    nentries = 0;\
  }\
  return table;\
}


RETURN_TABLE(particle,-)
RETURN_TABLE(run_header,run_header)
RETURN_TABLE(event_header,event_header)
RETURN_TABLE(dst_run_summary,run_summary)
RETURN_TABLE(dst_event_summary,event_summary)
RETURN_TABLE(dst_summary_param,summary_param)
RETURN_TABLE(dst_TrgDet,TrgDet)
RETURN_TABLE(dst_L0_Trigger,L0_Trigger)         
RETURN_TABLE(dst_dedx,dedx)            
RETURN_TABLE(dst_mon_soft_ctb,mon_soft_ctb)    
RETURN_TABLE(dst_mon_soft_emc,mon_soft_emc)    
RETURN_TABLE(dst_mon_soft_ftpc,mon_soft_ftpc)  
RETURN_TABLE(dst_mon_soft_glob,mon_soft_glob)  
RETURN_TABLE(dst_mon_soft_l3,mon_soft_l3)    
RETURN_TABLE(dst_mon_soft_rich,mon_soft_rich) 
RETURN_TABLE(dst_rch_pixel,rch_pixel)     
RETURN_TABLE(dst_mon_soft_svt,mon_soft_svt)   
RETURN_TABLE(dst_mon_soft_tpc,mon_soft_tpc)   
RETURN_TABLE(dst_point,point)          
RETURN_TABLE(dst_tkf_vertex,kinkVertex)    
RETURN_TABLE(dst_v0_vertex,v0_vertex)      
RETURN_TABLE(dst_vertex,vertex)        
RETURN_TABLE(dst_xi_vertex,xi_vertex)         
	     
#undef RETURN_TABLE

dst_track_st*
StRootEventManager::returnTable_dst_globtrk(long& nentries) const
{
    dst_track_st* table = NULL;
    St_dst_track  *tableWrap;
    const char *nm =  "globtrk";
    const char *nt =  "St_dst_track";
    St_DataSetIter *Dst = (St_DataSetIter*)&mDst;
    tableWrap = (St_dst_track*) (*Dst)[nt];
    if (!tableWrap && nm[0]!='-') tableWrap = (St_dst_track*) (*Dst)[nm];
    if (tableWrap) {
	table = tableWrap->GetTable();
	nentries = tableWrap->GetNRows();
    }
    else {
    const char *nm =  "glbtrk";
	    " - name " << nm << " not found in DataSet " << Dst->Pwd()->GetName() << endl;
	nentries = 0;
    }
    return table;
}

dst_track_st*
StRootEventManager::returnTable_dst_primtrk(long& nentries) const
{
    dst_track_st* table = NULL;
    St_dst_track  *tableWrap;
    const char *nm =  "primtrk";
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
