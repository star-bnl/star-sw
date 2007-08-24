/***************************************************************************
 *
 * $Id: StRootEventManager.cc,v 2.15 2007/08/24 15:37:43 perev Exp $
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
 * Revision 2.15  2007/08/24 15:37:43  perev
 * Decrease prints
 *
 * Revision 2.14  2007/06/04 21:53:46  fine
 * replace cerr with LOG_INFO
 *
 * Revision 2.13  2004/08/28 18:52:03  fisyak
 * Replace StEvent Hit containers if there are entries in the corrensponding tables
 *
 * Revision 2.12  2003/09/02 17:58:09  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.11  2003/01/22 21:42:00  genevb
 * Fix solaris compilation
 *
 * Revision 2.10  2002/04/18 23:29:35  jeromel
 * Implementation of the SVT 2 tables scheme ...
 *
 * Revision 2.9  2001/11/07 21:20:46  ullrich
 * Added L1 trigger.
 *
 * Revision 2.8  2001/09/12 23:48:33  ullrich
 * Removed code to load run_header and run_summary tables.
 *
 * Revision 2.7  2001/04/23 16:02:02  perev
 * small fix
 *
 * Revision 2.6  2001/04/20 16:23:22  perev
 * Remove annoing warning
 *
 * Revision 2.5  2000/08/17 00:38:05  ullrich
 * Added CpyTrk table.
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
 *
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 **************************************************************************/
#include <string>
#include <Stsstream.h>
#include <Stiostream.h>
#include "Rtypes.h"
#include "StRootEventManager.hh"
#include "StChain.h"
using std::string;

StRootEventManager::StRootEventManager():StEventManager() { /* noop */ }

StRootEventManager::~StRootEventManager() { /* noop */ }

ooStatus
StRootEventManager::openEvent(const char* name)
{
    //  mDst.Reset(0);
    //  grab the event from the maker
    if (!mCurrentChain) 	return oocError;
    St_DataSet *dsDst = mCurrentChain->GetDataSet(name);
    if (!dsDst) 		return oocError;
    // if (!dsDst->GetList())      return oocError;
    mDst.Reset(dsDst);
    return oocSuccess;
}

ooStatus
StRootEventManager::readEvent() {return oocSuccess;}

void StRootEventManager::closeEvent() { /* noop */ }

void StRootEventManager::setup() { /* noop */ }

void StRootEventManager::shutdown() { /* noop */ }

#define RETURN_TABLE(TABLE_TYPE,TABLE_NAME) \
_NAME2_(TABLE_TYPE,_st)* StRootEventManager::_NAME2_(returnTable_,TABLE_TYPE)(long& nentries) const\
{\
  static int n13=13;\
  _NAME2_(TABLE_TYPE,_st)* table = NULL;\
  _NAME2_(St_,TABLE_TYPE)  *tableWrap;\
  const char *nm =  _QUOTE_(TABLE_NAME);\
  const char *nt =  _QUOTE_(TABLE_TYPE);\
  St_DataSetIter *Dst = (St_DataSetIter*)&mDst;\
  if (! Dst->Pwd()) {nentries = 0; return table;}\
  tableWrap = (_NAME2_(St_,TABLE_TYPE) *) (*Dst)[nt];\
  if (!tableWrap && nm[0]!='-') tableWrap = (_NAME2_(St_,TABLE_TYPE) *) (*Dst)[nm];\
  if (tableWrap) {\
    table = tableWrap->GetTable();\
    nentries = tableWrap->GetNRows();\
  } \
  else {\
  if ((n13--)>0) {\
   LOG_INFO << "StRootEventManager: Table type  " << nt << \
  " - name " << nm << " not found in DataSet " << Dst->Pwd()->GetName() << endm;}\
    nentries = 0;\
  }\
  return table;\
}


RETURN_TABLE(particle,-)
RETURN_TABLE(event_header,event_header)
RETURN_TABLE(dst_event_summary,event_summary)
RETURN_TABLE(dst_summary_param,summary_param)
RETURN_TABLE(dst_TrgDet,TrgDet)
RETURN_TABLE(dst_L0_Trigger,L0_Trigger)         
RETURN_TABLE(dst_L1_Trigger,L1_Trigger)         
RETURN_TABLE(dst_dedx,dedx)            
RETURN_TABLE(dst_mon_soft_ctb,mon_soft_ctb)    
RETURN_TABLE(dst_mon_soft_emc,mon_soft_emc)    
RETURN_TABLE(dst_mon_soft_ftpc,mon_soft_ftpc)  
RETURN_TABLE(dst_mon_soft_glob,mon_soft_glob)  
RETURN_TABLE(dst_mon_soft_l3,mon_soft_l3)    
RETURN_TABLE(dst_mon_soft_rich,mon_soft_rich) 
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
static int n13=13;
    dst_track_st* table = NULL;
    St_dst_track  *tableWrap;
    const char *nm =  "globtrk";
    const char *nt =  "St_dst_track";
    St_DataSetIter *Dst = (St_DataSetIter*)&mDst;
    if (! Dst->Pwd()) {nentries = 0; return table;}
    tableWrap = (St_dst_track*) (*Dst)[nt];
    if (!tableWrap && nm[0]!='-') tableWrap = (St_dst_track*) (*Dst)[nm];
    if (tableWrap) {
	table = tableWrap->GetTable();
	nentries = tableWrap->GetNRows();
    }
    else if (--n13 >0) {
	   LOG_INFO <<  "StRootEventManager: Table type  " << nt << 
	    " - name " << nm << " not found in DataSet " << Dst->Pwd()->GetName() << endm;
	nentries = 0;
    }
    return table;
}

dst_track_st*
StRootEventManager::returnTable_dst_primtrk(long& nentries) const
{
static int n13=13;
    dst_track_st* table = NULL;
    St_dst_track  *tableWrap;
    const char *nm =  "primtrk";
    const char *nt =  "St_dst_track";
    St_DataSetIter *Dst = (St_DataSetIter*)&mDst;
    if (! Dst->Pwd()) {nentries = 0; return table;}
    tableWrap = (St_dst_track*) (*Dst)[nt];
    if (!tableWrap && nm[0]!='-') tableWrap = (St_dst_track*) (*Dst)[nm];
    if (tableWrap) {
	table = tableWrap->GetTable();
	nentries = tableWrap->GetNRows();
    }
    else if(--n13 >0) {
	   LOG_INFO << "StRootEventManager: Table type  " << nt << 
	    " - name " << nm << " not found in DataSet " << Dst->Pwd()->GetName() << endm;
	nentries = 0;
    }
    return table;
}

dst_track_st*
StRootEventManager::returnTable_CpyTrk(long& nentries) const
{
static int n13=13;
    dst_track_st* table = NULL;
    St_dst_track  *tableWrap;
    const char *nm =  "CpyTrk";
    const char *nt =  "St_dst_track";
    St_DataSetIter *Dst = (St_DataSetIter*)&mDst;
    tableWrap = (St_dst_track*) (*Dst)[nt];
    if (!tableWrap && nm[0]!='-') tableWrap = (St_dst_track*) (*Dst)[nm];
    if (tableWrap) {
	table = tableWrap->GetTable();
	nentries = tableWrap->GetNRows();
    }
    else if(--n13 >0) {
	   LOG_INFO  << "StRootEventManager: Table type  " << nt << 
	    " - name " << nm << " not found in DataSet " << Dst->Pwd()->GetName() << endm;
	nentries = 0;
    }
    return table;
}

dst_track_st*
StRootEventManager::returnTable_EstGlobal(long& nentries) const
{
static int n13=13;
    dst_track_st* table = NULL;
    St_dst_track  *tableWrap;
    const char *nm =  "EstGlobal";
    const char *nt =  "St_dst_track";
    St_DataSetIter *Dst = (St_DataSetIter*)&mDst;
    if (! Dst->Pwd()) {nentries = 0; return table;}
    tableWrap = (St_dst_track*) (*Dst)[nt];
    if (!tableWrap && nm[0]!='-') tableWrap = (St_dst_track*) (*Dst)[nm];
    if (tableWrap) {
	table = tableWrap->GetTable();
	nentries = tableWrap->GetNRows();
    }
    else if(--n13 >0) {
	   LOG_INFO << "StRootEventManager: Table type  " << nt << 
	    " - name " << nm << " not found in DataSet " << Dst->Pwd()->GetName() << endm;
	nentries = 0;
    }
    return table;
}

dst_track_st*
StRootEventManager::returnTable_EstPrimary(long& nentries) const
{
static int n13=13;
    dst_track_st* table = NULL;
    St_dst_track  *tableWrap;
    const char *nm =  "EstPrimary";
    const char *nt =  "St_dst_track";
    St_DataSetIter *Dst = (St_DataSetIter*)&mDst;
    if (! Dst->Pwd()) {nentries = 0; return table;}
    tableWrap = (St_dst_track*) (*Dst)[nt];
    if (!tableWrap && nm[0]!='-') tableWrap = (St_dst_track*) (*Dst)[nm];
    if (tableWrap) {
	table = tableWrap->GetTable();
	nentries = tableWrap->GetNRows();
    }
    else if(--n13 >0){
	   LOG_INFO << "StRootEventManager: Table type  " << nt << 
	    " - name " << nm << " not found in DataSet " << Dst->Pwd()->GetName() << endm;
	nentries = 0;
    }
    return table;
}
