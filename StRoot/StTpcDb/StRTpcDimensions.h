#ifndef __STRTPCDIMENSIONS__
#define __STRTPCDIMENTIONS__
#include <StTpcDimensionsI.h>
#include <TPCDimensions.time.h>

class StRTpcDimensions : public StTpcDimensionsI {

 private:

  tpc_dimensions mTpc;


public:

  StRTpcDimensions(){}
 ~StRTpcDimensions(){}
  void AddData(tpc_dimensions TpcIn){
  mTpc = TpcIn;
  }

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

ClassDef(StRTpcDimensions,0)

};
#endif















