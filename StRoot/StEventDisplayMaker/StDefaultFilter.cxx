// $Id: StDefaultFilter.cxx,v 1.1 2000/08/26 03:14:42 fine Exp $
#include "iostream.h"
#include "TH1.h"
#include "StDefaultFilter.h"
#include "TTableSorter.h"
#include "TTableIter.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_dedx_Table.h"
#include "StCL.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StDefaultFilter virtual base class                                     //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(StDefaultFilter)
//_____________________________________________________________________________
StDefaultFilter::StDefaultFilter()
{
   Reset();
}
//_____________________________________________________________________________
Int_t StDefaultFilter::SetTrack_Stylez(Int_t lowcolor,float lowenergy,Int_t highcolor,float highenergy,Size_t size,Style_t style,
Int_t CutTracksOutOfRange, Int_t KindOfColoring)
{
 STFLowColor   = lowcolor; 
 STFHighColor  = highcolor;
 STFLowEnergy  = lowenergy;
 STFHighEnergy = highenergy;
// STFSize       = size;
 STFStyle      = style;
 STFCutTracksOutOfRange = CutTracksOutOfRange ;
 STFKindOfColoring      = KindOfColoring ;
 return 0 ;
}

//_____________________________________________________________________________
Int_t StDefaultFilter::Reset(Int_t reset)
{
  mDedx = 0;

  STFLowColor  = 51; 
  STFHighColor = 100;
  STFLowEnergy = 0;
  STFHighEnergy = 0.00001;
  STFSize  = 0;
  STFStyle = 0; 
  STFColorEnergy = 0;
  STFColor = 0;
  STFCutTracksOutOfRange = 0;
  STFKindOfColoring = 0;
  mlookFactor = 1;
  return reset; 
}
//________________________________________________________________________________
Int_t StDefaultFilter::MakeColor(Double_t energy)
{
 STFColor = 0;
 if ( energy>STFHighEnergy )  
   STFColor = STFHighColor;
 else if ( energy<STFLowEnergy ) 
   STFColor = STFLowColor;
 else  {
     STFColor = STFLowColor
                + int((STFHighColor-STFLowColor)
                 *mDeLookUp[int((energy - STFLowEnergy)*mlookFactor)]);
 }
 return STFColor;
}

//_____________________________________________________________________________
Int_t StDefaultFilter::Channel(const TTableSorter* tableObject,Int_t index,Size_t &size,Style_t &style)
{
  //
  // This is an example how one can separate tableObject's by the table "name"
  // rather by the table "type" as the next "Channel" does
  //
  // Int_t rowNumber = tableObject->GetIndex(UInt_t(index));
 
  Color_t color = -1;
  TString mStr = tableObject->GetTable()->GetName();
  if(mStr == "dst_dedx" && !mDedx)
  {
    mDedx = tableObject;
    // Create lookup table
    Int_t lookUpSize = sizeof(mDeLookUp);
    mNbins = lookUpSize/sizeof(mDeLookUp[0]);
    TH1F de("__de__","dedx",mNbins,STFLowEnergy,STFHighEnergy);

    St_dst_dedx *t    = (St_dst_dedx *)tableObject->GetTable();
    dst_dedx_st *dedx = 0;
    dst_dedx_st *dEnd = t->end();
    for (dedx = t->begin();dedx !=dEnd; dedx++)
                     de.Fill(dedx->dedx[0]);
    Double_t  s = de.ComputeIntegral();
    Double_t *f = de.GetIntegral();

    for (int i = 0; i < mNbins; i++) 
                   mDeLookUp[i] = f[i]/s;
    
    mlookFactor = 1./de.GetBinWidth(1);
  } else {
     color = StVirtualEventFilter::Channel(tableObject,index,size,style);   
  }
  return color; 
}

//_____________________________________________________________________________
Int_t StDefaultFilter::Channel(const TTable *tableObject,Int_t rowNumber,Size_t &size,Style_t &style)
{
  // Introduce in here your own rule to select a particular row of the given tableObjectvertex
  //
  // Input:
  //   TTable *tableObject - pointer
  //   Int_t rowNumber       - the current row number
  // 
  // Return value: > 0 the color index this vertex will be drawn
  //               = 0 this vertex will be not drawn
  //               < 0 no track will be drawn fro  now untill StVirtualFilter::Reset called
  // Output:
  //         size (option) - the marker size to be used to draw this vertex
  //         style(option) - the marker style to be used to draw this vertex
  // Note: One may not assign any value for the output parameters size and style
  //       The "default" values will be used instead.
  //
  // This is an example how one can separate tableObject's by the table "type"
  // rather by the table "name" as the previous "Channel" does
  //

  TString mStr = tableObject->GetType();
  if( mStr == "dst_track_st" ) {
     St_dst_track &track = *((St_dst_track *)tableObject); 
     return SubChannel(track,rowNumber,size,style);
  }
  return StVirtualEventFilter::Channel(tableObject,rowNumber,size,style);
}
//_____________________________________________________________________________
Int_t StDefaultFilter::SubChannel(St_dst_track   &track, Int_t rowNumber,Size_t &size,Style_t &style)
{
  // SubChannel to provide a selections for St_dst_track tracks.
  if (mDedx) {
    const St_dst_dedx &t = *(St_dst_dedx *)mDedx->GetTable();
    Double_t energy = t.GetTable((*mDedx)[rowNumber-1])->dedx[0];   
    return MakeColor(energy);
  } else {
    printf(" no dedx !!!!\n");
    return StVirtualEventFilter::Channel(&track,rowNumber,size,style);
  }
}


// $Log: StDefaultFilter.cxx,v $
// Revision 1.1  2000/08/26 03:14:42  fine
// New default filter from M.Panebratcev has been introduced
//
