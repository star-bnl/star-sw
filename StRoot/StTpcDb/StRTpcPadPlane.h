#ifndef __STRTPCPADPLANE__
#define __STRTPCPADPLANE__
//#include <TObject.h>
#include "StTpcPadPlaneI.h"
#include "StDbLib/Geometry/tpcPadPlanes.h"

class StRTpcPadPlane : public StTpcPadPlaneI {

private:

  tpcPadPlanes* mPadPlane;

public:

  StRTpcPadPlane(){}
  ~StRTpcPadPlane(){}
  void AddData(tpcPadPlanes* PadIn) {
  mPadPlane = PadIn;
 }
// void AddData(tpc_padplanes PadIn);

  //Implements Abstract Interface 
 
 int   numberOfRows()           const; 
 int   numberOfInnerRows()      const;
 int   numberOfInnerRows48()    const;
 int   numberOfInnerRows52()    const;
 int   numberOfOuterRows()      const;
 float innerSectorPadWidth()    const;
 float innerSectorPadLength()   const;
 float innerSectorPadPitch()    const;
 float innerSectorRowPitch1()   const;
 float innerSectorRowPitch2()   const;
 float firstPadRow()            const;
 float firstOuterSectorPadRow() const;
 float lastOuterSectorPadRow()  const;
 float firstRowWidth()          const; 
 float lastRowWidth()           const;
 float outerSectorPadWidth()    const;
 float outerSectorPadLength()   const;
 float outerSectorPadPitch()    const;
 float outerSectorRowPitch()    const;
 float outerSectorLength()      const;
 float ioSectorSeparation()     const;
 float innerSectorEdge()        const;
 float outerSectorEdge()        const;
 
 int   numberOfPadsAtRow(int row)    const;
 float radialDistanceAtRow(int row) const;  
 float PadWidthAtRow(int row)       const;
 float PadLengthAtRow(int row)      const;
 float PadPitchAtRow(int row)       const;
 float RowPitchAtRow(int row)       const;

 int indexForRowPad(int row, int pad)   const;

 ClassDef(StRTpcPadPlane,0)

};
#endif









