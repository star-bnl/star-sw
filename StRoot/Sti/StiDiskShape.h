#ifndef STI_DISK_SHAPE_H
#define STI_DISK_SHAPE_H

#include "StiCylindricalShape.h"

/*!
  Class to represent a think cylinder shape within the STAR geometry
  \author Yuri Fisyak, BNL, 01/18/05 
*/
class StiDiskShape: public StiCylindricalShape {
public:

  // constructor
  StiDiskShape(): StiCylindricalShape() {}
  StiDiskShape(const string &name,
	       float halfDepth_, 
	       float thickness_,
	       float outerRadius_, 
	       float openingAngle_) : 
		      StiCylindricalShape(name, halfDepth_, thickness_,outerRadius_, openingAngle_) {}
		      StiShapeCode getShapeCode() const { return kDisk; }
};
#endif
