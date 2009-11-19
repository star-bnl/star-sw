/***************************************************************************
 *
 * $Id: StRootEventManager.hh,v 2.7 2009/11/19 19:44:05 fisyak Exp $
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
 * Revision 2.7  2009/11/19 19:44:05  fisyak
 * Step back with tables
 *
 * Revision 2.5  2002/04/18 23:29:35  jeromel
 * Implementation of the SVT 2 tables scheme ...
 *
 * Revision 2.4  2001/11/07 21:20:46  ullrich
 * Added L1 trigger.
 *
 * Revision 2.3  2001/09/12 23:48:34  ullrich
 * Removed code to load run_header and run_summary tables.
 *
 * Revision 2.2  2000/08/17 00:38:09  ullrich
 * Added CpyTrk table.
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
    ~StRootEventManager();
    
    ooStatus openEvent(const char* colName);
    ooStatus readEvent();
    void closeEvent();
    void setup();
    void shutdown();
    
    particle_st*  	   returnTable_particle(long&)          const;
    event_header_st*       returnTable_event_header(long&)      const;
    dst_event_summary_st*  returnTable_dst_event_summary(long&) const;
    dst_L0_Trigger_st*     returnTable_dst_L0_Trigger(long&)    const;
    dst_L1_Trigger_st*     returnTable_dst_L1_Trigger(long&)    const;
    dst_TrgDet_st*         returnTable_dst_TrgDet(long&)        const;
    dst_dedx_st*           returnTable_dst_dedx(long&)          const;     
    dst_mon_soft_ctb_st*   returnTable_dst_mon_soft_ctb(long&)  const;
    dst_mon_soft_emc_st*   returnTable_dst_mon_soft_emc(long&)  const;
    dst_mon_soft_ftpc_st*  returnTable_dst_mon_soft_ftpc(long&) const;
    dst_mon_soft_glob_st*  returnTable_dst_mon_soft_glob(long&) const;
    dst_mon_soft_l3_st*    returnTable_dst_mon_soft_l3(long&)   const;
    dst_mon_soft_rich_st*  returnTable_dst_mon_soft_rich(long&) const;
    dst_mon_soft_svt_st*   returnTable_dst_mon_soft_svt(long&)  const;
    dst_mon_soft_tpc_st*   returnTable_dst_mon_soft_tpc(long&)  const;
    dst_point_st*          returnTable_dst_point(long&)         const;    
    dst_summary_param_st*  returnTable_dst_summary_param(long&) const;
    dst_tkf_vertex_st*     returnTable_dst_tkf_vertex(long&)    const;
    dst_track_st*          returnTable_dst_globtrk(long&)       const;    
    dst_track_st*          returnTable_dst_primtrk(long&)       const;    
    dst_v0_vertex_st*      returnTable_dst_v0_vertex(long&)     const;
    dst_vertex_st*         returnTable_dst_vertex(long&)        const;   
    dst_xi_vertex_st*      returnTable_dst_xi_vertex(long&)     const;
    dst_track_st*          returnTable_CpyTrk(long&)            const;
    dst_track_st*          returnTable_EstGlobal(long&)         const;
    dst_track_st*          returnTable_EstPrimary(long&)        const;

protected:
    St_DataSetIter mDst;
};
#endif
