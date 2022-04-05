// Represents the position and orientation of a detector in STAR.
// 
// Ben Norman, Kent State
// 26 July 01
//
// The "center" of a planar detector is its center of gravity (the mindpoint
// in local x, y, and z).  For curved
// (cylindrical or conical sections) detectors, the "center" is the midpoint
// in z, opening angle, and radial thickness.
//
// The "normal" coordinates give the magnitude and azimuthal angle of a
// normal vector from the global origin to the plane of the detector.  The
// plane of a planar detector is simply the one in which it lies.  For a
// curved detector, the plane is the one which contains the tangent to the 
// curved section at its center and is normal to the transverse projection of
// the radial vector from the
// origin to its center.  Note that these definitions all assume that the
// plane of the detector is parallel to global z.  The third "normal" 
// cooridnate gives the location of the detector center along the detector
// plane in the aximuthal direction (i.e., local y).  This representation is
// best for the Kalman local track model.
//
// The "center" coordinates are a little more natural and are best used for
// rendering and radial ordering.  Here, the magnitude and azimuthal angle
// of a vector from the global origin to the center of the detector are
 // given, as well as an orientation angle.  The orientation angle is the
// angle from the vector above to the detector plane's outward normal.
// It is 0 for detectors which have xOffset==0.
//
// when setting the values, one must set all 3 for a representation at once.
// the other representation is then recalculated and both are available
// for quick access
//
// The layerRadius is independent and is used for ordering detectors in R.

#ifndef STI_PLACEMENT_H
#define STI_PLACEMENT_H

#include <ostream>

#include "TGeoMatrix.h"
#include "TVector3.h"


class StiPlacement{

public:

    enum StiRegion {kBackwardRapidity, kMidRapidity, kForwardRapidity, kUndefined};
    
    // constructors
    StiPlacement();

/// this constructor defines ALL the variables in StiPlacement class. 
/// Some of them directly, others indirectly, some by default.
/// This setting could be changed by additional setters if needed
    StiPlacement(float  normRefAngle,float  normRadius,float normYOffset,float centralZ); 
    StiPlacement(const TGeoMatrix& transMatrix, const TVector3& localCenter=TVector3(), const TVector3& normal=TVector3(0, 1, 0));
    // accessors
    float getNormalRefAngle()	const { return normalRefAngle;   }
    float getNormalRadius() 	const { return normalRadius;     }
    float getNormalYoffset() 	const { return normalYoffset;    }
    float getCenterRefAngle() 	const { return centerRefAngle;   }
    float getCenterRadius() 	const { return centerRadius;     }
    float getCenterOrientation()const { return centerOrientation;}
    float getLayerRadius() 	const { return layerRadius;      }
    float getLayerAngle() 	const { return layerAngle;       }
    float getZcenter() 		const { return zCenter;          }
    StiRegion getRegion() 	const { return mRegion;          }

    // mutators
    void setNormalRep(float refAngle_, float radius_, float xOffset_);
    //    void setCenterRep(float refAngle_, float radius_, float orientation_);
    void setLayerRadius(float radius_);
    void setLayerAngle(float angle);
    void setZcenter(float val)        { zCenter = val; }
    void setRegion(StiRegion r)       { mRegion = r;}

    friend std::ostream& operator<<(std::ostream& os, const StiPlacement& p);

protected:

    // store both representations
    float normalRefAngle; // in [-pi, pi)
    float normalRadius;   // >= 0
    float normalYoffset;
    float centerRefAngle; // in [-pi, pi)
    float centerRadius;   // >= 0
    float centerOrientation;  // in [-pi/2, pi/2)

    // independent radius for ordering
    float layerRadius;
    float layerAngle;

    float zCenter;
    StiRegion mRegion; // backward, midrapidity, forwrad, default to kUndefined

};


#endif
