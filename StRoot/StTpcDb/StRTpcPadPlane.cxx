/***************************************************************************
 *
 * $Id: StRTpcPadPlane.cxx,v 1.10 2000/11/14 22:00:06 genevb Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC pad plane geometry interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcPadPlane.cxx,v $
 * Revision 1.10  2000/11/14 22:00:06  genevb
 * Switched several functions from float to double
 *
 * Revision 1.9  2000/03/30 17:02:36  hardtke
 * limit warning message in StRTpcPadPlane
 *
 * Revision 1.8  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#include "StRTpcPadPlane.h"

ClassImp(StRTpcPadPlane)
 
//_____________________________________________________________________________
int   StRTpcPadPlane::numberOfPadsAtRow(int row) const {
 int npad = 0;
 if (row >= 1 && row<=numberOfRows()) {
    if ( row<=numberOfInnerRows() ) 
      npad = (*mPadPlane)[0].innerPadsPerRow[row-1];
    else 
      npad = (*mPadPlane)[0].outerPadsPerRow[row-1-numberOfInnerRows()];
 }
 return npad;
}

//_____________________________________________________________________________
double   StRTpcPadPlane::radialDistanceAtRow(int row) const {
 double radius = 0;
 if (row >= 1 && row<=numberOfRows()) {
   if ( row<=numberOfInnerRows() ) 
     radius = (*mPadPlane)[0].innerRowRadii[row-1];
   else 
     radius = (*mPadPlane)[0].outerRowRadii[row-1-numberOfInnerRows()];
 }
 return radius;
}

//_____________________________________________________________________________
double StRTpcPadPlane::PadWidthAtRow(int row) const {
 double width = 0;
 if (row >= 1 && row<=numberOfRows()) {
   if ( row<=numberOfInnerRows()) 
     width = innerSectorPadWidth();
  else
     width = outerSectorPadWidth();
 }
 return width;
}

//_____________________________________________________________________________
double StRTpcPadPlane::PadLengthAtRow(int row) const {
 double Length=0;
 if (row >= 1 && row<=numberOfRows()) {
   if ( row<=numberOfInnerRows()) 
      Length = innerSectorPadLength();
   else 
      Length = outerSectorPadLength();
 }
 return Length;
}

//_____________________________________________________________________________
double StRTpcPadPlane::PadPitchAtRow(int row) const {
 double Pitch = 0;
 if (row >= 1 && row<=numberOfRows()) {
   if ( row<=numberOfInnerRows()) 
     Pitch = innerSectorPadPitch();
  else 
     Pitch = outerSectorPadPitch();
 }
 return Pitch;
}

//_____________________________________________________________________________
double StRTpcPadPlane::RowPitchAtRow(int row) const {
 double Pitch = 0;
 if (row >= 1 && row<=numberOfRows()) {
   if ( row<=numberOfInnerRows48() ) 
      Pitch = innerSectorRowPitch1();
   else if (row>numberOfInnerRows48()&&row<=numberOfInnerRows()) 
      Pitch = innerSectorRowPitch2();
   else 
      Pitch = outerSectorRowPitch();
 }
 return Pitch;
}

//_____________________________________________________________________________
int StRTpcPadPlane::indexForRowPad(int row, int pad) const {
  if (pad >numberOfPadsAtRow(row)) {
    gMessMgr->Message("","W") << "StRTpcPadPlane::Invalid Pad number, row, pad = " << row << "," << pad << endm;;
   return -1;
  }

  int index = 0;
  if (row>0 && row<=numberOfInnerRows() )
     for (int i=1;i<row;i++) index += numberOfPadsAtRow(i);    
  else if (row>numberOfInnerRows()&&row<=numberOfRows())
     for (int i=numberOfInnerRows()+1;i<row;i++)  index += numberOfPadsAtRow(i);
  index+=pad-1;
  return index;
}








