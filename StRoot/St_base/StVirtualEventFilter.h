//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
// $Id: StVirtualEventFilter.h,v 1.10 2000/09/15 15:17:19 fine Exp $
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

// #include "StTypeDefs.h"
#include <TObject.h>
#include <Gtypes.h>

class StObjArray;
class StGlobalTrack;
class StVertex;

class TTableSorter;
class TTable;

class StVirtualEventFilter : public TObject {
 protected:
    Int_t m_ActiveFlag;  // Flag whether this filter is on/off 
    ULong_t mOptions;    // Bit masks fro the user defined options
 public:
    StVirtualEventFilter(Int_t flag=0):m_ActiveFlag(flag){;}
    virtual ~StVirtualEventFilter(){;}
    Int_t IsOn() { return GetFlag();}
    Int_t IsOff(){ return !GetFlag();}
    virtual Int_t GetFlag();
    ULong_t GetOptions();
    Int_t Turn(Int_t flag=1);
    Int_t TurnOn() { return Turn();}
    Int_t TurnOff(){ return Turn(0);}
    Int_t Toggle() { return GetFlag()? TurnOff():TurnOn();}
    virtual Int_t Channel(StGlobalTrack *globTrack,Size_t &size,Style_t &style);
    virtual Int_t Channel(const StObjArray *hitCollection,Size_t &size,Style_t &style);
    virtual Int_t Channel(const TTableSorter *tableObject,Int_t index,Size_t &size,Style_t &style);
    virtual Int_t Channel(const StVertex *vertexObject,Size_t &size,Style_t &style);
    virtual Int_t Channel(const TTable *tableObject,Int_t rowNumber,Size_t &size,Style_t &style);
    virtual Int_t Reset(Int_t reset=0){return reset;}
    ULong_t SetOptions(ULong_t opt);

    ClassDef(StVirtualEventFilter,0) // virtual base class for the custom "event" filters (useful for 3D visualization)
};

inline ULong_t StVirtualEventFilter::GetOptions(){return mOptions;}
inline ULong_t StVirtualEventFilter::SetOptions(ULong_t opt){ ULong_t o = GetOptions(); mOptions = opt; return o;}
inline Int_t StVirtualEventFilter::Turn(Int_t flag){ Int_t s = GetFlag(); m_ActiveFlag = flag; return s;}

// $Log: StVirtualEventFilter.h,v $
// Revision 1.10  2000/09/15 15:17:19  fine
// new method and data-member to add options introduced
//
// Revision 1.9  2000/03/24 20:35:21  fine
// adjusted to ROOT 2.24. Doesn't work yet. Under development
//
// Revision 1.8  1999/12/21 18:57:13  fine
// compilation warning plus new type for SizeAttribute
//
// Revision 1.7  1999/12/05 06:34:16  fine
// Several const methods for St_TableSorter introduced
//
// Revision 1.6  1999/12/04 21:59:57  fine
// new class comment
//
// Revision 1.5  1999/12/04 21:56:27  fine
// new non-const signature for Channel(St_TableSorter) method
//
// Revision 1.4  1999/11/24 18:51:22  fine
// all StVirtual::Filter have been renamed to Channel
//
// Revision 1.3  1999/11/10 02:24:37  fine
// StVirtualFilter::Reset method has been introduced
//

#endif
