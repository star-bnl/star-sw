//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
// $Id: StDefaultFilter.h,v 1.5 2000/09/26 17:04:48 fine Exp $ 
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
class TColoredAxis;
class TH1F;
class St_dst_dedx;

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
    TColoredAxis *mColorAxis; // the axise to present the track coloring schema
    const TTableSorter *mDedx;

 protected:
    virtual void  Distribution(St_dst_dedx  *obj,TH1F &de);
    virtual void  Distribution(St_dst_track *obj,TH1F &de);
    virtual Int_t SubChannel(St_dst_track &track, Int_t index,Size_t &size, Style_t &style);
    virtual Int_t SubChannel(const TTableSorter *tableObject, Int_t index,Size_t &size, Style_t &style);
    Int_t MakeColor(Double_t energy);
    Int_t CreatePalette(TTable *obj);

 public:
    StDefaultFilter();
    virtual ~StDefaultFilter() {Reset();}
    virtual Int_t Channel(const TTableSorter *tableObject,Int_t index,Size_t &size,Style_t &style);
    virtual Int_t Channel(const TTable *tableObject,Int_t rowNumber,Size_t &size,Style_t &style);
    TColoredAxis *GetColorAxis();

    virtual Int_t Reset(Int_t reset=0);
    Int_t SetTrack_Stylez(Int_t lowcolor,float lowenergy,Int_t highcolor,float highenergy,Size_t size,Style_t style,
                          Int_t CutTracksOutOfRange, Int_t KindOfColoring);
    ClassDef(StDefaultFilter,0)
};

//_____________________________________________________________________________
inline  TColoredAxis *StDefaultFilter::GetColorAxis(){ return mColorAxis;}

//_____________________________________________________________________________
// $Log: StDefaultFilter.h,v $
// Revision 1.5  2000/09/26 17:04:48  fine
// Two separate Distribution methods introduced
//
// Revision 1.4  2000/09/25 01:29:50  fine
// new StFtpcTrackFilter for Janet has been introdcued
//
// Revision 1.3  2000/09/01 22:39:03  fine
// minor bug fixex (Sun complained)
//
// Revision 1.2  2000/08/27 16:55:09  fine
// Title with Run event number etc
//
// Revision 1.1  2000/08/26 03:14:44  fine
// New default filter from M.Panebratcev has been introduced
//
//

#endif
