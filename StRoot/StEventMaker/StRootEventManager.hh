/***************************************************************************
 *
 * $Id: StRootEventManager.hh,v 2.0 1999/11/04 19:03:00 ullrich Exp $
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
 * $Log: StRootEventManager.hh,v $
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 * Revision 2.1  2000/05/25 14:44:51  ullrich
 * Removed remaining pieces of the RICH pixel table.
 *
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 **************************************************************************/
#ifndef StRootEventManager_HH
#define StRootEventManager_HH

#include "StEventManager.hh"
class StMaker;

class StRootEventManager : public StEventManager {
public:
    StRootEventManager();
    virtual ~StRootEventManager();
    
    virtual ooStatus openEvent(const char* colName);
    virtual ooStatus readEvent();
    virtual void closeEvent();
    virtual void setup();
    virtual void shutdown();
    
    virtual particle_st*  	   returnTable_particle(long&)          const;
    virtual run_header_st*         returnTable_run_header(long&)     const;             
    virtual event_header_st*       returnTable_event_header(long&)      const;             
    virtual dst_run_summary_st*    returnTable_dst_run_summary(long&)   const;             
    virtual dst_event_summary_st*  returnTable_dst_event_summary(long&) const;             
    virtual dst_L0_Trigger_st*     returnTable_dst_L0_Trigger(long&)    const;             
    virtual dst_TrgDet_st*         returnTable_dst_TrgDet(long&)        const;             
    virtual dst_dedx_st*           returnTable_dst_dedx(long&)          const;                    
    virtual dst_mon_soft_ctb_st*   returnTable_dst_mon_soft_ctb(long&)  const;            
    virtual dst_mon_soft_emc_st*   returnTable_dst_mon_soft_emc(long&)  const;            
    virtual dst_mon_soft_ftpc_st*  returnTable_dst_mon_soft_ftpc(long&) const;           
    virtual dst_mon_soft_glob_st*  returnTable_dst_mon_soft_glob(long&) const;           
    virtual dst_mon_soft_l3_st*    returnTable_dst_mon_soft_l3(long&)   const;             
    virtual dst_mon_soft_rich_st*  returnTable_dst_mon_soft_rich(long&) const;           
    virtual dst_rch_pixel_st*      returnTable_dst_rch_pixel(long&)     const;               
    virtual dst_mon_soft_svt_st*   returnTable_dst_mon_soft_svt(long&)  const;            
    virtual dst_mon_soft_tpc_st*   returnTable_dst_mon_soft_tpc(long&)  const;            
    virtual dst_point_st*          returnTable_dst_point(long&)         const;                   
    virtual dst_summary_param_st*  returnTable_dst_summary_param(long&) const;           
    virtual dst_tkf_vertex_st*     returnTable_dst_tkf_vertex(long&)    const;              
    virtual dst_track_st*          returnTable_dst_globtrk(long&)       const;                   
    virtual dst_track_st*          returnTable_dst_primtrk(long&)       const;                   
    virtual dst_v0_vertex_st*      returnTable_dst_v0_vertex(long&)     const;               
    virtual dst_vertex_st*         returnTable_dst_vertex(long&)        const;                  
    virtual dst_xi_vertex_st*      returnTable_dst_xi_vertex(long&)     const;               

protected:
    St_DataSetIter mDst;
};
#endif
