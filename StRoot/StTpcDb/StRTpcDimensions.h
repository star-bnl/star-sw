/***************************************************************************
 *
 * $Id: StRTpcDimensions.h,v 1.9.4.1 2007/08/12 23:27:40 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Root Implementation of Tpc Dimensions interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcDimensions.h,v $
 * Revision 1.9.4.1  2007/08/12 23:27:40  jeromel
 * Further fixes for SL06g built for SL44
 *
 * Revision 1.10  2007/08/04 00:38:03  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
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


#endif















