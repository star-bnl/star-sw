#include <StRTpcPadPlane.h>

ClassImp(StRTpcPadPlane)

  //void StRTpcPadPlane::AddData(tpc_padplanes PadIn) {
  // mPadPlane = PadIn;
  // }

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

float StRTpcPadPlane::innerSectorPadWidth() const {
return mPadPlane.innerSectorPadWidth;
}

float StRTpcPadPlane::innerSectorPadLength() const {
return mPadPlane.innerSectorPadLength;
}

float StRTpcPadPlane::innerSectorPadPitch() const {
return mPadPlane.innerSectorPadPitch;
}

float StRTpcPadPlane::innerSectorRowPitch1() const {
return mPadPlane.innerSectorRowPitch1;
}
float StRTpcPadPlane::innerSectorRowPitch2() const {
return mPadPlane.innerSectorRowPitch2;
}

float StRTpcPadPlane::firstPadRow() const {
return mPadPlane.firstPadRow;
}

float StRTpcPadPlane::firstOuterSectorPadRow() const {
return mPadPlane.firstOuterSectorPadRow;
}

float StRTpcPadPlane::lastOuterSectorPadRow() const {
return mPadPlane.lastOuterSectorPadRow;
}

float StRTpcPadPlane::firstRowWidth() const {
return mPadPlane.firstRowWidth;
}
 
float StRTpcPadPlane::lastRowWidth() const {
return mPadPlane.lastRowWidth;
}

float StRTpcPadPlane::outerSectorPadWidth() const {
return mPadPlane.outerSectorPadWidth;
}

float StRTpcPadPlane::outerSectorPadLength() const {
return mPadPlane.outerSectorPadLength;
}

float StRTpcPadPlane::outerSectorPadPitch() const {
return mPadPlane.outerSectorPadPitch;
}

float StRTpcPadPlane::outerSectorRowPitch() const {
return mPadPlane.outerSectorRowPitch;
}

float StRTpcPadPlane::outerSectorLength() const {
return mPadPlane.outerSectorLength;
}

float StRTpcPadPlane::ioSectorSeparation() const {
return mPadPlane.ioSectorSeparation;
}

float StRTpcPadPlane::innerSectorEdge() const {
return mPadPlane.innerSectorEdge;
}

float StRTpcPadPlane::outerSectorEdge() const {
return mPadPlane.outerSectorEdge;
}
 
int   StRTpcPadPlane::numberOfPadsAtRow(int row) const {
 int npad;
 if (row<1||row>numberOfRows()) {npad = 0;}
  else if (row>0&&row<=numberOfInnerRows()) 
    {npad = mPadPlane.innerPadsPerRow[row-1];}
  else {npad = mPadPlane.outerPadsPerRow[row-1-numberOfInnerRows()];}
 return npad;
}

float   StRTpcPadPlane::radialDistanceAtRow(int row) const {
 int radius;
 if (row<1||row>numberOfRows()) {radius = 0;}
  else if (row>0&&row<=numberOfInnerRows()) {
        radius = mPadPlane.innerRowRadii[row-1];}
  else {radius = mPadPlane.outerRowRadii[row-1-numberOfInnerRows()];}
 return radius;
}

float StRTpcPadPlane::PadWidthAtRow(int row) const {
 float width;
 if (row<1||row>numberOfRows()) {width = 0;}
  else if (row>0&&row<=numberOfInnerRows()) {width = innerSectorPadWidth();}
  else {width = outerSectorPadWidth();}
 return width;
}

float StRTpcPadPlane::PadLengthAtRow(int row) const {
 float Length;
 if (row<1||row>numberOfRows()) {Length = 0;}
  else if (row>0&&row<=numberOfInnerRows()) {Length = innerSectorPadLength();}
  else {Length = outerSectorPadLength();}
 return Length;
}

float StRTpcPadPlane::PadPitchAtRow(int row) const {
 float Pitch;
 if (row<1||row>numberOfRows()) {Pitch = 0;}
  else if (row>0&&row<=numberOfInnerRows()) {Pitch = innerSectorPadPitch();}
  else {Pitch = outerSectorPadPitch();}
 return Pitch;
}

float StRTpcPadPlane::RowPitchAtRow(int row) const {
 float Pitch;
 if (row<1||row>numberOfRows()) {Pitch = 0;}
  else if (row>0&&row<=numberOfInnerRows48()) {Pitch = innerSectorRowPitch1();}
  else if (row>numberOfInnerRows48()&&row<=numberOfInnerRows()) 
           {Pitch = innerSectorRowPitch2();}
  else {Pitch = outerSectorRowPitch();}
 return Pitch;
}



//float StRTpcPadPlane::radialDistanceAtRow(int row) const = 0;  
//float StRTpcPadPlane::PadWidthAtRow(int row)       const = 0;
//float StRTpcPadPlane::PadLengthAtRow(int row)      const = 0;
//float StRTpcPadPlane::PadPitchAtRow(int row)       const = 0;
//float StRTpcPadPlane::RowPitchAtRow(int row)       const = 0;







