/***************************************************************************
 *
 * $Id: StRichPIDTraits.h,v 1.1 2000/04/03 19:40:51 horsley Exp $
 *
 * Author: Matt Horsley, March 29, 2000
 ***************************************************************************
 *
 * Description: patterened after StDedxPidTraits.h
 *
 ***************************************************************************
 *
 **************************************************************************/
#ifndef StRichPIDTraits_hh
#define StRichPIDTraits_hh

#include "StTrackPidTraits.h"

#include <string>
#include <iostream.h>
#if !defined(ST_NO_NAMESPACES)
 using std::string;
#endif


class StParticleDefinition;

class StRichPIDTraits : public StTrackPidTraits {

public:
  StRichPIDTraits();
  StRichPIDTraits(StDetectorId, StParticleDefinition*, UShort_t, Float_t, Float_t);
  
  // StRichPIDTraits& operator=(const StRichPIDTraits&); use default
  virtual ~StRichPIDTraits();
  string       particleName()    const;
  UShort_t     numberOfPoints()  const;
  Float_t      areaOnPadPlane()  const;
  Float_t      totalArea()       const;
  Float_t      photonDensity()   const;
  
protected:
  string     mName;  
  UShort_t   mNumberOfPoints;
  Float_t    mAreaOnPadPlane;
  Float_t    mTotalArea;
    
  StObject* clone();
  ClassDef(StRichPIDTraits,1)
};
#endif
