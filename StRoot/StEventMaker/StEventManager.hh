/***************************************************************************
 *
 * $Id: StEventManager.hh,v 2.1 2000/05/25 14:45:36 ullrich Exp $
 *
 * Author: Original version by T. Wenaus, BNL
 *         Revised version for new StEvent by T. Ullrich, Yale
 ***************************************************************************
 *
 * Description:
 * Abstract interface to a DST table server
 *
 ***************************************************************************
 *
 * $Log: StEventManager.hh,v $
 * Revision 2.1  2000/05/25 14:45:36  ullrich
 * Removed remaining pieces of the RICH pixel table.
 *
 * Revision 2.1  2000/05/25 14:45:36  ullrich
 * Removed remaining pieces of the RICH pixel table.
 *
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 **************************************************************************/
#ifndef StEventManager_HH
#define StEventManager_HH

#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/St_particle_Table.h"
#include "tables/St_gen_header_Table.h"
#include "tables/St_dst_L0_Trigger_Table.h"          
#include "tables/St_dst_TrgDet_Table.h"              
#include "tables/St_dst_dedx_Table.h"                
#include "tables/St_dst_event_summary_Table.h"       
#include "tables/St_dst_mon_soft_ctb_Table.h"        
#include "tables/St_dst_mon_soft_emc_Table.h"        
#include "tables/St_dst_mon_soft_ftpc_Table.h"       
#include "tables/St_dst_mon_soft_glob_Table.h"       
#include "tables/St_dst_mon_soft_l3_Table.h"         
#include "tables/St_dst_mon_soft_rich_Table.h"       
#include "tables/St_dst_mon_soft_svt_Table.h"        
#include "tables/St_dst_mon_soft_tpc_Table.h"        
#include "tables/St_dst_point_Table.h"               
#include "tables/St_dst_run_summary_Table.h"         
#include "tables/St_dst_summary_param_Table.h"                    
#include "tables/St_dst_tkf_vertex_Table.h"          
#include "tables/St_dst_track_Table.h"               
#include "tables/St_dst_v0_vertex_Table.h"           
#include "tables/St_dst_vertex_Table.h"              
#include "tables/St_dst_xi_vertex_Table.h"           
#include "tables/St_event_header_Table.h"            
#include "tables/St_run_header_Table.h"              

enum ooStatus {oocError, oocSuccess };

class StMaker;

class StEventManager {
public:
    StEventManager(){mCurrentChain=0;mEvent=0;};
    virtual ~StEventManager(){};
    
    virtual ooStatus openEvent(const char* colName)=0;
    virtual ooStatus readEvent()=0;
    virtual void closeEvent(){};
    virtual void setup(){};
    virtual void shutdown(){};
    
    virtual particle_st*  	   returnTable_particle(long&)          const =0;
    virtual run_header_st*         returnTable_run_header(long&)     const =0;             
    virtual event_header_st*       returnTable_event_header(long&)      const =0;             
    virtual dst_run_summary_st*    returnTable_dst_run_summary(long&)   const =0;             
    virtual dst_event_summary_st*  returnTable_dst_event_summary(long&) const =0;             
    virtual dst_L0_Trigger_st*     returnTable_dst_L0_Trigger(long&)    const =0;             
    virtual dst_TrgDet_st*         returnTable_dst_TrgDet(long&)        const =0;             
    virtual dst_dedx_st*           returnTable_dst_dedx(long&)          const =0;                    
    virtual dst_mon_soft_ctb_st*   returnTable_dst_mon_soft_ctb(long&)  const =0;            
    virtual dst_mon_soft_emc_st*   returnTable_dst_mon_soft_emc(long&)  const =0;            
    virtual dst_mon_soft_ftpc_st*  returnTable_dst_mon_soft_ftpc(long&) const =0;           
    virtual dst_mon_soft_glob_st*  returnTable_dst_mon_soft_glob(long&) const =0;           
    virtual dst_mon_soft_l3_st*    returnTable_dst_mon_soft_l3(long&)   const =0;             
    virtual dst_mon_soft_rich_st*  returnTable_dst_mon_soft_rich(long&) const =0;           
    virtual dst_mon_soft_svt_st*   returnTable_dst_mon_soft_svt(long&)  const =0;            
    virtual dst_mon_soft_tpc_st*   returnTable_dst_mon_soft_tpc(long&)  const =0;            
    virtual dst_point_st*          returnTable_dst_point(long&)         const =0;                   
    virtual dst_summary_param_st*  returnTable_dst_summary_param(long&) const =0;           
    virtual dst_tkf_vertex_st*     returnTable_dst_tkf_vertex(long&)    const =0;              
    virtual dst_track_st*          returnTable_dst_globtrk(long&)       const =0;                   
    virtual dst_track_st*          returnTable_dst_primtrk(long&)       const =0;                   
    virtual dst_v0_vertex_st*      returnTable_dst_v0_vertex(long&)     const =0;               
    virtual dst_vertex_st*         returnTable_dst_vertex(long&)        const =0;                  
    virtual dst_xi_vertex_st*      returnTable_dst_xi_vertex(long&)     const =0;               
    
    virtual void setMaker(StMaker* mk) {mCurrentChain=mk;};
    
protected:
    StMaker* 	 mCurrentChain;
    St_DataSet*  mEvent;
};
#endif
