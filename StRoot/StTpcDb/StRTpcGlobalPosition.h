/***************************************************************************
 *
 * $Id: StRTpcGlobalPosition.h,v 1.3 2007/08/04 00:38:03 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Pad Plane Geometry Interface 
 *
 ***************************************************************************
 *
 * $Log: StRTpcGlobalPosition.h,v $
 * Revision 1.3  2007/08/04 00:38:03  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
 * Revision 1.2  2002/02/12 22:50:35  hardtke
 * separate geometrical tpc rotation from field twist
 *
 * Revision 1.1  2001/05/21 23:26:08  hardtke
 * Add tpcGlobalPosition to StTpcDb.  This includes the global position offset and the rotation w.r.t. the magnet
 *
 * Revision 1.11  2000/11/14 22:00:06  genevb
 * Switched several functions from float to double
 *
 * Revision 1.10  2000/01/12 15:14:40  hardtke
 * Update StTpcWirePlanes to use new variable names in tpcWirePlanes.idl/ Add Z position functions to StTpcGlobalPosition
 *
 * Revision 1.9  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STRTPCGlobalPosition__
#define __STRTPCGlobalPosition__
//#include <TObject.h>
#include "StMessMgr.h"
#include "StTpcGlobalPositionI.h"
#include "tables/St_tpcGlobalPosition_Table.h"

class StRTpcGlobalPosition : public StTpcGlobalPositionI {

private:

  St_tpcGlobalPosition* mGlobalPosition;

public:

  StRTpcGlobalPosition(St_tpcGlobalPosition* GlobPosIn=0){AddData(GlobPosIn);}
  ~StRTpcGlobalPosition(){}
  void AddData(St_tpcGlobalPosition* GlobPosIn) {
   mGlobalPosition = GlobPosIn;
 } 

  //Implements Abstract Interface 
 
  double TpcCenterPositionX()    const; //center of TPC in global
  double TpcCenterPositionY()    const; //coordinates (cm)
  double TpcCenterPositionZ()    const; // 

  // now we define the rotation angles.  The angles are in radians.
  // Positive rotation defined by right hand rule
  double TpcRotationAroundGlobalAxisX() const;
  double TpcRotationAroundGlobalAxisY() const;
  double TpcRotationAroundGlobalAxisZ() const;
  double TpcEFieldRotationX()   const;  // These are used
  double TpcEFieldRotationY()   const;  // for ExB twist correction
  double TpcEFieldRotationZ()   const;


 ClassDef(StRTpcGlobalPosition,0)

};

#endif









