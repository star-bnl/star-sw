//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
// $Id: StSectorHitFilter.h,v 1.1 2000/09/26 17:45:48 fine Exp $ 
//
#ifndef STAR_StSectorHitFilter
#define STAR_StSectorHitFilter

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSectorHitFilter class                                              //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////

#include "StVirtualEventFilter.h"

#include <set>

#ifndef ST_NO_NAMESPACES
using std::set;
#endif

class St_tcl_tphit;
class St_dst_track;
class St_tpt_track;

class StSectorHitFilter : public StVirtualEventFilter  {
 private:
    Int_t     m_badCounter;   // # hits with no track id assigned
    Int_t     m_row[100];     // array of the sector/padrow's to look up
    Int_t     m_nRow;         // the total number of the padrows to look up
    set<long> m_trackId;      //! the list of the global track's ids
    set<long> m_TptTrackID;   //! the list of the track's ids
    const TTableSorter *m_Primtrk;

 protected:
    Int_t DstHitSubChannel(const TTableSorter *tableObject, Int_t index,Size_t &size, Style_t &style);
    Int_t HitSubChannel(const TTableSorter *tableObject,Int_t index,Size_t &size,Style_t &style); 
    Int_t SubChannel(St_dst_track   &track, Int_t rowNumber,Size_t &size,Style_t &style); 
    Int_t SubChannel(St_tpt_track   &track, Int_t rowNumber,Size_t &size,Style_t &style);

 public:
    StSectorHitFilter() :m_nRow(0), m_Primtrk(0) {}
    virtual ~StSectorHitFilter() {Reset();}
    virtual Int_t Channel(const TTableSorter *tableObject,Int_t index,Size_t &size,Style_t &style);
    virtual Int_t Channel(const TTable *tableObject,Int_t rowNumber,Size_t &size,Style_t &style);
    virtual Int_t Reset(Int_t reset=0); 
    Int_t SetSecRow(Int_t *sectorRows,Int_t n=1); 

    ClassDef(StSectorHitFilter,0)
};

//__________________________________________________
// $Log: StSectorHitFilter.h,v $
// Revision 1.1  2000/09/26 17:45:48  fine
// Dst and Tcl chair classes have been moved from example to here
//
// Revision 1.2  2000/07/23 02:10:07  fine
// New featues: The filter can handle dst_point tables from STAR dst
//
// Revision 1.1  2000/07/19 21:53:23  fine
// Graphical filter to draw the hits from the selected PadRows and the associated tracks too
//
// Revision 1.1  2000/07/19 21:13:46  fine
// Graphical filter to draw the hits from the selected PadRows and the associated tracks too
//
//__________________________________________________
#endif
