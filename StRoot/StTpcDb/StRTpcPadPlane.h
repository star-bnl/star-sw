/***************************************************************************
 *
 * $Id: StRTpcPadPlane.h,v 1.11 2000/11/14 22:00:06 genevb Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Pad Plane Geometry Interface 
 *
 ***************************************************************************
 *
 * $Log: StRTpcPadPlane.h,v $
 * Revision 1.11  2000/11/14 22:00:06  genevb
 * Switched several functions from float to double
 *
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
 
 int   numberOfRows()            const; 
 int   numberOfInnerRows()       const;
 int   numberOfInnerRows48()     const;
 int   numberOfInnerRows52()     const;
 int   numberOfOuterRows()       const;
 double innerSectorPadWidth()    const;
 double innerSectorPadLength()   const;
 double innerSectorPadPitch()    const;
 double innerSectorRowPitch1()   const;
 double innerSectorRowPitch2()   const;
 double firstPadRow()            const;
 double firstOuterSectorPadRow() const;
 double lastOuterSectorPadRow()  const;
 double firstRowWidth()          const; 
 double lastRowWidth()           const;
 double outerSectorPadWidth()    const;
 double outerSectorPadLength()   const;
 double outerSectorPadPitch()    const;
 double outerSectorRowPitch()    const;
 double outerSectorLength()      const;
 double ioSectorSeparation()     const;
 double innerSectorEdge()        const;
 double outerSectorEdge()        const;
 double innerSectorPadPlaneZ()   const;
 double outerSectorPadPlaneZ()   const;
 
 int   numberOfPadsAtRow(int row)    const;
 double radialDistanceAtRow(int row) const;  
 double PadWidthAtRow(int row)       const;
 double PadLengthAtRow(int row)      const;
 double PadPitchAtRow(int row)       const;
 double RowPitchAtRow(int row)       const;

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

inline double StRTpcPadPlane::innerSectorPadWidth() const {
return (*mPadPlane)[0].innerSectorPadWidth;
}

inline double StRTpcPadPlane::innerSectorPadLength() const {
return (*mPadPlane)[0].innerSectorPadLength;
}

inline double StRTpcPadPlane::innerSectorPadPitch() const {
return (*mPadPlane)[0].innerSectorPadPitch;
}

inline double StRTpcPadPlane::innerSectorRowPitch1() const {
return (*mPadPlane)[0].innerSectorRowPitch1;
}
inline double StRTpcPadPlane::innerSectorRowPitch2() const {
return (*mPadPlane)[0].innerSectorRowPitch2;
}

inline double StRTpcPadPlane::firstPadRow() const {
return (*mPadPlane)[0].firstPadRow;
}

inline double StRTpcPadPlane::firstOuterSectorPadRow() const {
return (*mPadPlane)[0].firstOuterSectorPadRow;
}

inline double StRTpcPadPlane::lastOuterSectorPadRow() const {
return (*mPadPlane)[0].lastOuterSectorPadRow;
}

inline double StRTpcPadPlane::firstRowWidth() const {
return (*mPadPlane)[0].firstRowWidth;
}
 
inline double StRTpcPadPlane::lastRowWidth() const {
return (*mPadPlane)[0].lastRowWidth;
}

inline double StRTpcPadPlane::outerSectorPadWidth() const {
return (*mPadPlane)[0].outerSectorPadWidth;
}

inline double StRTpcPadPlane::outerSectorPadLength() const {
return (*mPadPlane)[0].outerSectorPadLength;
}

inline double StRTpcPadPlane::outerSectorPadPitch() const {
return (*mPadPlane)[0].outerSectorPadPitch;
}

inline double StRTpcPadPlane::outerSectorRowPitch() const {
return (*mPadPlane)[0].outerSectorRowPitch;
}

inline double StRTpcPadPlane::outerSectorLength() const {
return (*mPadPlane)[0].outerSectorLength;
}

inline double StRTpcPadPlane::ioSectorSeparation() const {
return (*mPadPlane)[0].ioSectorSeparation;
}

inline double StRTpcPadPlane::innerSectorEdge() const {
return (*mPadPlane)[0].innerSectorEdge;
}

inline double StRTpcPadPlane::outerSectorEdge() const {
return (*mPadPlane)[0].outerSectorEdge;
}

inline double StRTpcPadPlane::innerSectorPadPlaneZ() const {
return (*mPadPlane)[0].innerSectorPadPlaneZ;
}

inline double StRTpcPadPlane::outerSectorPadPlaneZ() const {
return (*mPadPlane)[0].outerSectorPadPlaneZ;
}

#endif









