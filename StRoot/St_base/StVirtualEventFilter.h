//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
// $Id: StVirtualEventFilter.h,v 1.4 1999/11/24 18:51:22 fine Exp $
//
#ifndef STAR_StVirtualEventFilter
#define STAR_StVirtualEventFilter

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StVirtualEventFilter virtual base class                              //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////

#include <TObject.h>
#include <Gtypes.h>

class StObjArray;
class StGlobalTrack;
class St_TableSorter;
class StVertex;
class St_Table;

class StVirtualEventFilter : public TObject {
 protected:
    Int_t m_ActiveFlag;  // Flag whether this filter is on/off 
 public:
    StVirtualEventFilter(Int_t flag=0):m_ActiveFlag(flag){;}
    virtual ~StVirtualEventFilter(){;}
    Int_t IsOn() { return GetFlag();}
    Int_t IsOff(){ return !GetFlag();}
    virtual Int_t GetFlag();
    Int_t Turn(Int_t flag=1);
    Int_t TurnOn() { return Turn();}
    Int_t TurnOff(){ return Turn(0);}
    Int_t Toggle() { return GetFlag()? TurnOff():TurnOn();}
    virtual Int_t Channel(StGlobalTrack *globTrack,Width_t &size,Style_t &style);
    virtual Int_t Channel(const StObjArray *hitCollection,Width_t &size,Style_t &style);
    virtual Int_t Channel(const St_TableSorter *tableObject,Int_t index,Width_t &size,Style_t &style);
    virtual Int_t Channel(const StVertex *vertexObject,Width_t &size,Style_t &style);
    virtual Int_t Channel(const St_Table *tableObject,Int_t rowNumber,Width_t &size,Style_t &style);
    virtual Int_t Reset(Int_t reset=0){return reset;}
    ClassDef(StVirtualEventFilter,0)
};

inline Int_t StVirtualEventFilter::Turn(Int_t flag){ Int_t s = GetFlag(); m_ActiveFlag = flag; return s;}

// $Log: StVirtualEventFilter.h,v $
// Revision 1.4  1999/11/24 18:51:22  fine
// all StVirtual::Filter have been renamed to Channel
//
// Revision 1.3  1999/11/10 02:24:37  fine
// StVirtualFilter::Reset method has been introduced
//

#endif
