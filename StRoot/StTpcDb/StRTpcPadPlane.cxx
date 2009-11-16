/***************************************************************************
 *
 * $Id: StRTpcPadPlane.cxx,v 1.13.4.1 2007/08/12 23:27:42 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC pad plane geometry interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcPadPlane.cxx,v $
 * Revision 1.13.4.1  2007/08/12 23:27:42  jeromel
 * Further fixes for SL06g built for SL44
 *
 * Revision 1.14  2007/08/04 00:38:04  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
 * Revision 1.13  2004/08/26 20:34:16  genevb
 * Improve speed by keeping own copy of table row
 *
 * Revision 1.12  2002/02/22 01:03:21  jeromel
 * Undo recent changes (don't understand it yet). Will be recoverable ...
 *
 * Revision 1.11  2002/02/21 18:35:58  hardtke
 * Speed up by hardwiring number of inner rows and making array for numberOfRowsAt function
 *
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
      npad = mPadPlane.innerPadsPerRow[row-1];
    else 
      npad = mPadPlane.outerPadsPerRow[row-1-numberOfInnerRows()];
 }
 return npad;
}

//_____________________________________________________________________________
double   StRTpcPadPlane::radialDistanceAtRow(int row) const {
 double radius = 0;
 if (row >= 1 && row<=numberOfRows()) {
   if ( row<=numberOfInnerRows() ) 
     radius = mPadPlane.innerRowRadii[row-1];
   else 
     radius = mPadPlane.outerRowRadii[row-1-numberOfInnerRows()];
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

int   StRTpcPadPlane::numberOfRows() const { return mPadPlane.padRows;}

int   StRTpcPadPlane::numberOfInnerRows() const {
return mPadPlane.innerPadRows;
}

int   StRTpcPadPlane::numberOfInnerRows48() const {
return mPadPlane.innerPadRows48;
}

int   StRTpcPadPlane::numberOfInnerRows52() const {
return mPadPlane.innerPadRows52;
}
 
int   StRTpcPadPlane::numberOfOuterRows() const {
return mPadPlane.outerPadRows;
}

double StRTpcPadPlane::innerSectorPadWidth() const {
return mPadPlane.innerSectorPadWidth;
}

double StRTpcPadPlane::innerSectorPadLength() const {
return mPadPlane.innerSectorPadLength;
}

double StRTpcPadPlane::innerSectorPadPitch() const {
return mPadPlane.innerSectorPadPitch;
}

double StRTpcPadPlane::innerSectorRowPitch1() const {
return mPadPlane.innerSectorRowPitch1;
}
double StRTpcPadPlane::innerSectorRowPitch2() const {
return mPadPlane.innerSectorRowPitch2;
}

double StRTpcPadPlane::firstPadRow() const {
return mPadPlane.firstPadRow;
}

double StRTpcPadPlane::firstOuterSectorPadRow() const {
return mPadPlane.firstOuterSectorPadRow;
}

double StRTpcPadPlane::lastOuterSectorPadRow() const {
return mPadPlane.lastOuterSectorPadRow;
}

double StRTpcPadPlane::firstRowWidth() const {
return mPadPlane.firstRowWidth;
}
 
double StRTpcPadPlane::lastRowWidth() const {
return mPadPlane.lastRowWidth;
}

double StRTpcPadPlane::outerSectorPadWidth() const {
return mPadPlane.outerSectorPadWidth;
}

double StRTpcPadPlane::outerSectorPadLength() const {
return mPadPlane.outerSectorPadLength;
}

double StRTpcPadPlane::outerSectorPadPitch() const {
return mPadPlane.outerSectorPadPitch;
}

double StRTpcPadPlane::outerSectorRowPitch() const {
return mPadPlane.outerSectorRowPitch;
}

double StRTpcPadPlane::outerSectorLength() const {
return mPadPlane.outerSectorLength;
}

double StRTpcPadPlane::ioSectorSeparation() const {
return mPadPlane.ioSectorSeparation;
}

double StRTpcPadPlane::innerSectorEdge() const {
return mPadPlane.innerSectorEdge;
}

double StRTpcPadPlane::outerSectorEdge() const {
return mPadPlane.outerSectorEdge;
}

double StRTpcPadPlane::innerSectorPadPlaneZ() const {
return mPadPlane.innerSectorPadPlaneZ;
}

double StRTpcPadPlane::outerSectorPadPlaneZ() const {
return mPadPlane.outerSectorPadPlaneZ;
}







