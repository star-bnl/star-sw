/***************************************************************************
 *
 * $Id: StTpcPadPlaneI.h,v 1.5 2000/11/14 22:00:06 genevb Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Abstract Interface for Tpc Pad Plane Geometry  
 *
 ***************************************************************************
 *
 * $Log: StTpcPadPlaneI.h,v $
 * Revision 1.5  2000/11/14 22:00:06  genevb
 * Switched several functions from float to double
 *
 * Revision 1.4  2000/01/12 15:14:41  hardtke
 * Update StTpcWirePlanes to use new variable names in tpcWirePlanes.idl/ Add Z position functions to StTpcPadPlane
 *
 * Revision 1.3  1999/12/16 22:00:54  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STTPCPADPLANEI__
#define __STTPCPADPLANEI__
#include <TObject.h>

class StTpcPadPlaneI : public TObject {

  //Abstract base class defining accessors
public:

  virtual int   numberOfRows()            const = 0;
  virtual int   numberOfInnerRows()       const = 0;
  virtual int   numberOfInnerRows48()     const = 0;
  virtual int   numberOfInnerRows52()     const = 0;
  virtual int   numberOfOuterRows()       const = 0;
  virtual double innerSectorPadWidth()    const = 0;
  virtual double innerSectorPadLength()   const = 0;
  virtual double innerSectorPadPitch()    const = 0;
  virtual double innerSectorRowPitch1()   const = 0;
  virtual double innerSectorRowPitch2()   const = 0;
  virtual double firstPadRow()            const = 0;
  virtual double firstOuterSectorPadRow() const = 0;
  virtual double lastOuterSectorPadRow()  const = 0;
  virtual double firstRowWidth()          const = 0; 
  virtual double lastRowWidth()           const = 0;
  virtual double outerSectorPadWidth()    const = 0;
  virtual double outerSectorPadLength()   const = 0;
  virtual double outerSectorPadPitch()    const = 0;
  virtual double outerSectorRowPitch()    const = 0;
  virtual double outerSectorLength()      const = 0;
  virtual double ioSectorSeparation()     const = 0;
  virtual double innerSectorEdge()        const = 0;
  virtual double outerSectorEdge()        const = 0;
  virtual double innerSectorPadPlaneZ()   const = 0;
  virtual double outerSectorPadPlaneZ()   const = 0;
 
  virtual int   numberOfPadsAtRow(int row)    const = 0;
  virtual double radialDistanceAtRow(int row) const = 0;  
  virtual double PadWidthAtRow(int row)       const = 0;
  virtual double PadLengthAtRow(int row)      const = 0;
  virtual double PadPitchAtRow(int row)       const = 0;
  virtual double RowPitchAtRow(int row)       const = 0;

  virtual int indexForRowPad(int row, int pad)  const = 0;


ClassDef(StTpcPadPlaneI,0)

};
#endif















