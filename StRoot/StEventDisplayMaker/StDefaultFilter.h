//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
// $Id: StDefaultFilter.h,v 1.2 2000/08/27 16:55:09 fine Exp $ 
#ifndef STAR_StDefaultFilter
#define STAR_StDefaultFilter

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StDefaultFilter base class                              //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////

#include "StVirtualEventFilter.h"

class TTableIter;
class St_dst_track;
class St_dst_point;

class StDefaultFilter : public StVirtualEventFilter  {
 private:

    Int_t STFLowColor; 
    Int_t STFHighColor;
    float STFLowEnergy;
    float STFHighEnergy;
    Int_t STFSize;
    Int_t STFStyle; 
    Int_t STFColorEnergy;
    Int_t STFColor;
    Int_t STFCutTracksOutOfRange ;
    Int_t STFKindOfColoring ;

    Int_t  mNbins;
    Double_t mDeLookUp[100];
    Double_t mlookFactor;

    const TTableSorter *mDedx;

 protected:
    Int_t SubChannel(St_dst_track &track, Int_t index,Size_t &size, Style_t &style);
    Int_t SubChannel(const TTableSorter *tableObject, Int_t index,Size_t &size, Style_t &style);
    Int_t MakeColor(Double_t energy);
 public:
    StDefaultFilter();
    virtual ~StDefaultFilter() {Reset();}
    virtual Int_t Channel(const TTableSorter *tableObject,Int_t index,Size_t &size,Style_t &style);
    virtual Int_t Channel(const TTable *tableObject,Int_t rowNumber,Size_t &size,Style_t &style);
    virtual Int_t Reset(Int_t reset=0);
    Int_t SetTrack_Stylez(Int_t lowcolor,float lowenergy,Int_t highcolor,float highenergy,Size_t size,Style_t style,
                          Int_t CutTracksOutOfRange, Int_t KindOfColoring);
    ClassDef(StDefaultFilter,0)
};
// $Log: StDefaultFilter.h,v $
// Revision 1.2  2000/08/27 16:55:09  fine
// Title with Run event number etc
//
// Revision 1.1  2000/08/26 03:14:44  fine
// New default filter from M.Panebratcev has been introduced
//
//

#endif
