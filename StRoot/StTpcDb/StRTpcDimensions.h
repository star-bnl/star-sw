/***************************************************************************
 *
 * $Id: StRTpcDimensions.h,v 1.8 2000/02/15 22:21:47 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Root Implementation of Tpc Dimensions interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcDimensions.h,v $
 * Revision 1.8  2000/02/15 22:21:47  hardtke
 * Add effective drift distances
 *
 * Revision 1.7  1999/12/16 22:00:52  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STRTPCDIMENSIONS__
#define __STRTPCDIMENSIONS__
#include "StTpcDimensionsI.h"
#include "StTpcPadPlaneI.h"
#include "StTpcWirePlaneI.h"
#include "tables/St_tpcDimensions_Table.h"
#include "tables/St_tpcEffectiveGeom_Table.h"

class StRTpcDimensions : public StTpcDimensionsI {

 private:

  St_tpcDimensions* mTpc; //!
  St_tpcEffectiveGeom* mEffGeo; //!
  StTpcPadPlaneI* pp; //!
  StTpcWirePlaneI* wp; //! 

public:

  StRTpcDimensions() : mTpc(0), mEffGeo(0) {;}
  void AddData( St_tpcDimensions* TpcIn);
  void AddData( St_tpcEffectiveGeom* GeoIn);
  StRTpcDimensions(St_tpcDimensions* TpcIn){ AddData(TpcIn); }
  StRTpcDimensions(St_tpcDimensions* TpcIn,St_tpcEffectiveGeom* GeoIn){ 
  AddData(TpcIn); AddData(GeoIn);}
  virtual ~StRTpcDimensions(){;}
  void SetPadPlanePointer(StTpcPadPlaneI* in){pp = in;}
  void SetWirePlanePointer(StTpcWirePlaneI* in){wp = in;}
 

  //accessors

    int   numberOfSectors()     const;

  //TPC field cage parameters:
    float ifcRadius()           const;
    float ofcRadius()           const;
    float tpcTotalLength()      const;

  //TPC wheel parameters:
    float wheelInnerRadius()    const;
    float wheelOuterRadius()    const;
    float wheelThickness()      const;

    float senseGasOuterRadius() const;
    float tpeaThickness()       const;

  //TPC cathode parameters:
    float cathodeInnerRadius()  const;
    float cathodeOuterRadius()  const;
    float cathodeThickness()    const; 

  //TPC distances
    float innerEffectiveDriftDistance()  const;
    float outerEffectiveDriftDistance()  const;
    float gatingGridZ()                  const;
    float zInnerOffset()                 const;
    float zOuterOffset()                 const;


    ClassDef(StRTpcDimensions,0)

};

#ifndef __CINT__
inline  void StRTpcDimensions::AddData( St_tpcDimensions* TpcIn){ mTpc = TpcIn;}

inline  void StRTpcDimensions::AddData( St_tpcEffectiveGeom* GeoIn){ mEffGeo = GeoIn;}

inline int StRTpcDimensions::numberOfSectors() const {
return (*mTpc)[0].numberOfSectors;
}

inline float StRTpcDimensions::ifcRadius() const {
return (*mTpc)[0].tpcInnerRadius;
}
    
inline float StRTpcDimensions::ofcRadius() const {
return (*mTpc)[0].tpcOuterRadius;
}
    
inline float StRTpcDimensions::tpcTotalLength() const {
return (*mTpc)[0].tpcTotalLength;
}

inline float StRTpcDimensions::wheelInnerRadius() const {
return (*mTpc)[0].wheelInnerRadius;
}

inline float StRTpcDimensions::wheelOuterRadius() const {
return (*mTpc)[0].wheelOuterRadius;
}

inline float StRTpcDimensions::wheelThickness() const {
return (*mTpc)[0].wheelThickness;
}

inline float StRTpcDimensions::senseGasOuterRadius() const {
return (*mTpc)[0].senseGasOuterRadius;
}
    
inline float StRTpcDimensions::tpeaThickness() const {
return (*mTpc)[0].tpeaThickness; 
}

inline float StRTpcDimensions::cathodeInnerRadius() const {
return (*mTpc)[0].cathodeInnerRadius;
}
    
inline float StRTpcDimensions::cathodeOuterRadius() const {
return (*mTpc)[0].cathodeOuterRadius;
}
    
inline float StRTpcDimensions::cathodeThickness() const {
return (*mTpc)[0].cathodeThickness;
} 

inline float StRTpcDimensions::zInnerOffset() const {
  return (*mEffGeo)[0].z_inner_offset;
}

inline float StRTpcDimensions::zOuterOffset() const {
  return (*mEffGeo)[0].z_outer_offset;
}

#endif
#endif















