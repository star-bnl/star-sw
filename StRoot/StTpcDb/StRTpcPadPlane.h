/***************************************************************************
 *
 * $Id: StRTpcPadPlane.h,v 1.10 2000/01/12 15:14:40 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Pad Plane Geometry Interface 
 *
 ***************************************************************************
 *
 * $Log: StRTpcPadPlane.h,v $
 * Revision 1.10  2000/01/12 15:14:40  hardtke
 * Update StTpcWirePlanes to use new variable names in tpcWirePlanes.idl/ Add Z position functions to StTpcPadPlane
 *
 * Revision 1.9  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STRTPCPADPLANE__
#define __STRTPCPADPLANE__
//#include <TObject.h>
#include "StMessMgr.h"
#include "StTpcPadPlaneI.h"
#include "tables/St_tpcPadPlanes_Table.h"

class StRTpcPadPlane : public StTpcPadPlaneI {

private:

  St_tpcPadPlanes* mPadPlane;

public:

  StRTpcPadPlane(St_tpcPadPlanes* PadIn=0){AddData(PadIn);}
  ~StRTpcPadPlane(){}
  void AddData(St_tpcPadPlanes* PadIn) {
   mPadPlane = PadIn;
 } 

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
 float innerSectorPadPlaneZ()   const;
 float outerSectorPadPlaneZ()   const;
 
 int   numberOfPadsAtRow(int row)    const;
 float radialDistanceAtRow(int row) const;  
 float PadWidthAtRow(int row)       const;
 float PadLengthAtRow(int row)      const;
 float PadPitchAtRow(int row)       const;
 float RowPitchAtRow(int row)       const;

 int indexForRowPad(int row, int pad)   const;

 ClassDef(StRTpcPadPlane,0)

};
inline int   StRTpcPadPlane::numberOfRows() const { return (*mPadPlane)[0].padRows;}

inline int   StRTpcPadPlane::numberOfInnerRows() const {
return (*mPadPlane)[0].innerPadRows;
}

inline int   StRTpcPadPlane::numberOfInnerRows48() const {
return (*mPadPlane)[0].innerPadRows48;
}

inline int   StRTpcPadPlane::numberOfInnerRows52() const {
return (*mPadPlane)[0].innerPadRows52;
}
 
inline int   StRTpcPadPlane::numberOfOuterRows() const {
return (*mPadPlane)[0].outerPadRows;
}

inline float StRTpcPadPlane::innerSectorPadWidth() const {
return (*mPadPlane)[0].innerSectorPadWidth;
}

inline float StRTpcPadPlane::innerSectorPadLength() const {
return (*mPadPlane)[0].innerSectorPadLength;
}

inline float StRTpcPadPlane::innerSectorPadPitch() const {
return (*mPadPlane)[0].innerSectorPadPitch;
}

inline float StRTpcPadPlane::innerSectorRowPitch1() const {
return (*mPadPlane)[0].innerSectorRowPitch1;
}
inline float StRTpcPadPlane::innerSectorRowPitch2() const {
return (*mPadPlane)[0].innerSectorRowPitch2;
}

inline float StRTpcPadPlane::firstPadRow() const {
return (*mPadPlane)[0].firstPadRow;
}

inline float StRTpcPadPlane::firstOuterSectorPadRow() const {
return (*mPadPlane)[0].firstOuterSectorPadRow;
}

inline float StRTpcPadPlane::lastOuterSectorPadRow() const {
return (*mPadPlane)[0].lastOuterSectorPadRow;
}

inline float StRTpcPadPlane::firstRowWidth() const {
return (*mPadPlane)[0].firstRowWidth;
}
 
inline float StRTpcPadPlane::lastRowWidth() const {
return (*mPadPlane)[0].lastRowWidth;
}

inline float StRTpcPadPlane::outerSectorPadWidth() const {
return (*mPadPlane)[0].outerSectorPadWidth;
}

inline float StRTpcPadPlane::outerSectorPadLength() const {
return (*mPadPlane)[0].outerSectorPadLength;
}

inline float StRTpcPadPlane::outerSectorPadPitch() const {
return (*mPadPlane)[0].outerSectorPadPitch;
}

inline float StRTpcPadPlane::outerSectorRowPitch() const {
return (*mPadPlane)[0].outerSectorRowPitch;
}

inline float StRTpcPadPlane::outerSectorLength() const {
return (*mPadPlane)[0].outerSectorLength;
}

inline float StRTpcPadPlane::ioSectorSeparation() const {
return (*mPadPlane)[0].ioSectorSeparation;
}

inline float StRTpcPadPlane::innerSectorEdge() const {
return (*mPadPlane)[0].innerSectorEdge;
}

inline float StRTpcPadPlane::outerSectorEdge() const {
return (*mPadPlane)[0].outerSectorEdge;
}

inline float StRTpcPadPlane::innerSectorPadPlaneZ() const {
return (*mPadPlane)[0].innerSectorPadPlaneZ;
}

inline float StRTpcPadPlane::outerSectorPadPlaneZ() const {
return (*mPadPlane)[0].outerSectorPadPlaneZ;
}

#endif









