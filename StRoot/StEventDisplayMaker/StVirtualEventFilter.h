//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
//   
//  
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

class StVirtualEventFilter : public TObject {
 protected:
    Int_t m_ActiveFlag;  // Flag whether this filter is on/off 
 public:
    StVirtualEventFilter(Int_t flag=1):m_ActiveFlag(flag){;}
    virtual ~StVirtualEventFilter(){;}
    Int_t IsOn() { return GetFlag();}
    Int_t IsOff(){ return !GetFlag();}
    virtual Int_t GetFlag();
    Int_t Turn(Int_t flag=1);
    Int_t TurnOn() { return Turn();}
    Int_t TurnOff(){ return Turn(0);}
    Int_t Toggle() { return GetFlag()? TurnOff():TurnOn();}
    virtual Int_t Filter(StGlobalTrack *globTrack,Width_t &size,Style_t &style);
    virtual Int_t Filter(const StObjArray *hitCollection,Width_t &size,Style_t &style);
    ClassDef(StVirtualEventFilter,0)
};

inline Int_t StVirtualEventFilter::Turn(Int_t flag){ Int_t s = GetFlag(); m_ActiveFlag = flag; return s;}

#endif
