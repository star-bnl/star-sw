/***************************************************************************
 *
 * $Id: StSvtGeantHits.hh,v 1.4 2001/04/03 15:24:24 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: keep geant hits for evaluation in hit maker
 *
 ***************************************************************************
 *
 * $Log: StSvtGeantHits.hh,v $
 * Revision 1.4  2001/04/03 15:24:24  caines
 * Increase hit space size again
 *
 * Revision 1.3  2001/03/22 20:46:21  caines
 * Expand MAX_HIT so high multiplicty events work
 *
 * Revision 1.2  2001/03/19 22:25:53  caines
 * Catch wrong wafer ids more elegantly
 *
 * Revision 1.1  2000/11/30 20:47:48  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/

#ifndef STSVTGEANTHITS_HH
#define STSVTGEANTHITS_HH

#define MAX_HITS 200

#include "StSvtClassLibrary/StSvtHybridObject.hh"
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#include "StThreeVector.hh"

class StSvtWaferCoordinate;
class StGlobalCoordinate;

class StSvtGeantHits : public StSvtHybridObject
{
public:
  StSvtGeantHits(int barrel, int ladder, int wafer, int hybrid);
  virtual ~StSvtGeantHits();

  void setNumOfHits(int nhits);
  void setGeantHit(int index , StSvtWaferCoordinate& waferCoord);
  void setGlobalCoord(int index, StThreeVector<double>& x);

  int numberOfHits();
  StSvtWaferCoordinate* waferCoordinate();
  StGlobalCoordinate*   globalCoordinate();

private:

  int mNumOfHits;
  StSvtWaferCoordinate* mWaferCoord;  //!
  StGlobalCoordinate*   mGlobalCoord; //!
 
  ClassDef(StSvtGeantHits,1)
};

inline int StSvtGeantHits::numberOfHits(){return mNumOfHits;}
inline StSvtWaferCoordinate* StSvtGeantHits::waferCoordinate(){return mWaferCoord;}
inline StGlobalCoordinate* StSvtGeantHits::globalCoordinate(){return mGlobalCoord;}

#endif
