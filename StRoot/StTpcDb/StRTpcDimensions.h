/***************************************************************************
 *
 * $Id: StRTpcDimensions.h,v 1.9 2000/11/14 22:00:06 genevb Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Root Implementation of Tpc Dimensions interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcDimensions.h,v $
 * Revision 1.9  2000/11/14 22:00:06  genevb
 * Switched several functions from float to double
 *
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
    double ifcRadius()           const;
    double ofcRadius()           const;
    double tpcTotalLength()      const;

  //TPC wheel parameters:
    double wheelInnerRadius()    const;
    double wheelOuterRadius()    const;
    double wheelThickness()      const;

    double senseGasOuterRadius() const;
    double tpeaThickness()       const;

  //TPC cathode parameters:
    double cathodeInnerRadius()  const;
    double cathodeOuterRadius()  const;
    double cathodeThickness()    const; 

  //TPC distances
    double innerEffectiveDriftDistance()  const;
    double outerEffectiveDriftDistance()  const;
    double gatingGridZ()                  const;
    double zInnerOffset()                 const;
    double zOuterOffset()                 const;


    ClassDef(StRTpcDimensions,0)

};

#ifndef __CINT__
inline  void StRTpcDimensions::AddData( St_tpcDimensions* TpcIn){ mTpc = TpcIn;}

inline  void StRTpcDimensions::AddData( St_tpcEffectiveGeom* GeoIn){ mEffGeo = GeoIn;}

inline int StRTpcDimensions::numberOfSectors() const {
return (*mTpc)[0].numberOfSectors;
}

inline double StRTpcDimensions::ifcRadius() const {
return (*mTpc)[0].tpcInnerRadius;
}
    
inline double StRTpcDimensions::ofcRadius() const {
return (*mTpc)[0].tpcOuterRadius;
}
    
inline double StRTpcDimensions::tpcTotalLength() const {
return (*mTpc)[0].tpcTotalLength;
}

inline double StRTpcDimensions::wheelInnerRadius() const {
return (*mTpc)[0].wheelInnerRadius;
}

inline double StRTpcDimensions::wheelOuterRadius() const {
return (*mTpc)[0].wheelOuterRadius;
}

inline double StRTpcDimensions::wheelThickness() const {
return (*mTpc)[0].wheelThickness;
}

inline double StRTpcDimensions::senseGasOuterRadius() const {
return (*mTpc)[0].senseGasOuterRadius;
}
    
inline double StRTpcDimensions::tpeaThickness() const {
return (*mTpc)[0].tpeaThickness; 
}

inline double StRTpcDimensions::cathodeInnerRadius() const {
return (*mTpc)[0].cathodeInnerRadius;
}
    
inline double StRTpcDimensions::cathodeOuterRadius() const {
return (*mTpc)[0].cathodeOuterRadius;
}
    
inline double StRTpcDimensions::cathodeThickness() const {
return (*mTpc)[0].cathodeThickness;
} 

inline double StRTpcDimensions::zInnerOffset() const {
  return (*mEffGeo)[0].z_inner_offset;
}

inline double StRTpcDimensions::zOuterOffset() const {
  return (*mEffGeo)[0].z_outer_offset;
}

#endif
#endif















