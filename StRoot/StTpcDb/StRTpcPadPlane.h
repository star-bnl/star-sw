/*!
 * \class StRTpcPadPlane 
 * \author David Hardtke
 *
 * TPC Pad Plane Geometry Interface
 */

#ifndef __STRTPCPADPLANE__
#define __STRTPCPADPLANE__
//#include <TObject.h>
#include "StMessMgr.h"
#include "StTpcPadPlaneI.h"
#include "tables/St_tpcPadPlanes_Table.h"

class StRTpcPadPlane : public StTpcPadPlaneI {

private:

  tpcPadPlanes_st mPadPlane;

public:

  StRTpcPadPlane(St_tpcPadPlanes* PadIn=0){AddData(PadIn);}
  ~StRTpcPadPlane(){}
  void AddData(St_tpcPadPlanes* PadIn) {
   if (PadIn) mPadPlane = (*PadIn)[0];
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

#endif

/***************************************************************************
 *
 * $Id: StRTpcPadPlane.h,v 1.15 2007/08/04 00:38:04 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Pad Plane Geometry Interface 
 *
 ***************************************************************************
 *
 * $Log: StRTpcPadPlane.h,v $
 * Revision 1.15  2007/08/04 00:38:04  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
 * Revision 1.14  2004/08/26 20:34:17  genevb
 * Improve speed by keeping own copy of table row
 *
 * Revision 1.13  2002/02/22 01:03:22  jeromel
 * Undo recent changes (don't understand it yet). Will be recoverable ...
 *
 * Revision 1.12  2002/02/21 18:35:58  hardtke
 * Speed up by hardwiring number of inner rows and making array for numberOfRowsAt function
 *
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
