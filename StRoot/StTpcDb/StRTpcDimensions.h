#ifndef __STRTPCDIMENSIONS__
#define __STRTPCDIMENSIONS__
#include "StTpcDimensionsI.h"
#include "tables/St_tpcDimensions_Table.h"

class StRTpcDimensions : public StTpcDimensionsI {

 private:

  St_tpcDimensions* mTpc; //!


public:

  StRTpcDimensions() : mTpc(0){;}
  virtual ~StRTpcDimensions(){;}
  void AddData( St_tpcDimensions* TpcIn);
  StRTpcDimensions(St_tpcDimensions* TpcIn){ AddData(TpcIn); }

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

#ifndef __CINT__
inline  void StRTpcDimensions::AddData( St_tpcDimensions* TpcIn){ mTpc = TpcIn;}

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
#endif
#endif















