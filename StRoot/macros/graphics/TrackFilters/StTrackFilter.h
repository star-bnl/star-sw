//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
// $Id: StTrackFilter.h,v 1.2 1999/12/14 17:56:40 fine Exp $ 
// $Log: StTrackFilter.h,v $
// Revision 1.2  1999/12/14 17:56:40  fine
// Advanced Event Filter
// 
//
#ifndef STAR_StTrackFilter
#define STAR_StTrackFilter

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTrackFilter base class                              //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////

#include "StVirtualEventFilter.h"

class St_TableIter;
class St_tcl_tphit;
class St_g2t_tpc_hit;
class St_g2t_svt_hit;

class StTrackFilter : public StVirtualEventFilter  {
 private:
    Int_t     m_Track_P[100]; // track id to draw g2t_tpc_hit
    Int_t     m_LTrackP;      // the number of hits in m_Track_P

    Int_t     m_id_globtrk[100];
    Int_t     m_Lid_globtrk;

    Int_t     m_tpt_track_id[100];
    Int_t     m_Ltpt_track;

    const St_TableSorter *m_G2t_track; // pointer to g2t_track sorted table
    St_TableIter *m_NextG2tTrack;

    Int_t     m_Ge_pid;          // The "ge_pid" value we are looking for
    const St_TableSorter *m_G2t_vertex;

 protected:
    Int_t SubChannel(St_tcl_tphit   &hit, Int_t rowNumber,Width_t &size,Style_t &style); 
    Int_t SubChannel(St_g2t_tpc_hit &hit, Int_t rowNumber,Width_t &size,Style_t &style); 
    Int_t SubChannel(St_g2t_svt_hit &hit, Int_t rowNumber,Width_t &size,Style_t &style); 
 public:
    StTrackFilter() : m_LTrackP(0),m_Lid_globtrk(0),m_Ltpt_track(0), m_G2t_track(0), m_NextG2tTrack(0) {}
    virtual ~StTrackFilter() {Reset();}
    virtual Int_t Channel(const St_TableSorter *tableObject,Int_t index,Width_t &size,Style_t &style);
    virtual Int_t Channel(const St_Table *tableObject,Int_t rowNumber,Width_t &size,Style_t &style);
    virtual Int_t Reset(Int_t reset=0);
    Int_t SetGe_pid(Int_t pid);
    Int_t SetTrack_P(Int_t *track_p,Int_t n=1); 
    Int_t SetId_globtrk(Int_t *id_globtrk,Int_t n=1); 
    Int_t SetTptID(Int_t *track_id,Int_t n=1); 
    ClassDef(StTrackFilter,0)
};

#endif
