/***************************************************************************
 *
 * $Id: StSvtGeantHits.hh,v 1.1 2000/11/30 20:47:48 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: keep geant hits for evaluation in hit maker
 *
 ***************************************************************************
 *
 * $Log: StSvtGeantHits.hh,v $
 * Revision 1.1  2000/11/30 20:47:48  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/

#ifndef STSVTGEANTHITS_HH
#define STSVTGEANTHITS_HH

#include "StSvtClassLibrary/StSvtHybridObject.hh"

#define MAX_HITS 50

class StSvtWaferCoordinate;

class StSvtGeantHits : public StSvtHybridObject
{
public:
  StSvtGeantHits(int barrel, int ladder, int wafer, int hybrid);
  virtual ~StSvtGeantHits();

  void setNumOfHits(int nhits);
  void setGeantHit(int index , StSvtWaferCoordinate& waferCoord);

  int numberOfHits();
  StSvtWaferCoordinate* waferCoordinate();

private:

  int mNumOfHits;
  StSvtWaferCoordinate* mWaferCoord;  //!
 
  ClassDef(StSvtGeantHits,1)
};

inline int StSvtGeantHits::numberOfHits(){return mNumOfHits;}
inline StSvtWaferCoordinate* StSvtGeantHits::waferCoordinate(){return mWaferCoord;}

#endif
