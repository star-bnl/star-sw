/**
 * @file StiTpcIsActiveFunctor.h
 * @class StiTpcIsActiveFunctor
 * @brief function object for determine a TPC padrow's active regions
 *
 * @author Ben Norman, Kent State University
 * @author Claude Pruneau, Wayne State University
 * @date March 2002
 */

#ifndef STI_TPC_IS_ACTIVE_FUNCTOR
#define STI_TPC_IS_ACTIVE_FUNCTOR
#include "Sti/StiIsActiveFunctor.h"

///Class defines a isActiveFunctor specific to the STAR tpc.
///The isActive status depends on the livelihood of the east and 
///west side of the TPC. 
class StiTpcIsActiveFunctor : public StiIsActiveFunctor
{
 public:
  StiTpcIsActiveFunctor(bool active=true, bool west=true, bool east=true);
  virtual ~StiTpcIsActiveFunctor();
  virtual bool operator()(double dYlocal, double dZlocal) const;
  virtual bool isActive() const;
  virtual bool isEastActive() const;
  virtual bool isWestActive() const;
  void setEastActive(bool value);
  void setWestActive(bool value);

 protected:
  /// is the east half of the padrow on?
  bool _eastActive;
  /// is the west half of the padrow on?
  bool _westActive;
};

inline bool StiTpcIsActiveFunctor::isActive() const
{
  return _active && (_eastActive || _westActive);
}

inline bool StiTpcIsActiveFunctor::isEastActive() const
{
  return _eastActive;
}

inline bool StiTpcIsActiveFunctor::isWestActive() const
{
  return _westActive;
}

inline void StiTpcIsActiveFunctor::setEastActive(bool value)
{
  _eastActive = value;
}

inline void StiTpcIsActiveFunctor::setWestActive(bool value)
{
  _westActive = value;
}


#endif // ifndef STI_TPC_IS_ACTIVE_FUNCTOR
