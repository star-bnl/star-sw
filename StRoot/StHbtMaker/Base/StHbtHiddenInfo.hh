/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Base class for the hidden info
 *   Anything can be carried through the class that derived from it
 *   getParticleHiddenInfo() has to be written : it return a copy of this
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef StHbtHiddenInfo_hh
#define StHbtHiddenInfo_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtHiddenInfo{

public:
  StHbtHiddenInfo(){/* no-op */};
  virtual ~StHbtHiddenInfo(){/* no-op */};

// !!! MANDATORY !!!
// --- Copy the hidden info from StHbtTrack to StHbtParticle
  virtual StHbtHiddenInfo* getParticleHiddenInfo() const =0;
  virtual StHbtHiddenInfo* clone() const;

};

inline StHbtHiddenInfo* StHbtHiddenInfo::clone() const{
  return getParticleHiddenInfo();
}

#endif
