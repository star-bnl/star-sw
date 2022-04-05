/***************************************************************************
 *
 * $Id: StSvtGeantHits.hh,v 1.9 2005/07/23 03:37:34 perev Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: keep geant hits for evaluation in hit maker
 *
 ***************************************************************************
 *
 * $Log: StSvtGeantHits.hh,v $
 * Revision 1.9  2005/07/23 03:37:34  perev
 * IdTruth + Cleanup
 *
 * Revision 1.8  2004/03/30 21:27:12  caines
 * Remove asserts from code so doesnt crash if doesnt get parameters it just quits with kStErr
 *
 * Revision 1.6  2003/07/31 19:18:09  caines
 * Petrs improved simulation code
 *
 * Revision 1.5  2001/08/13 15:34:18  bekele
 * Debugging tools added
 *
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
class StSvtLocalCoordinate;
class StGlobalCoordinate;

class StSvtGeantHits : public StSvtHybridObject
{
public:
  StSvtGeantHits(int barrel, int ladder, int wafer, int hybrid);
  virtual ~StSvtGeantHits();

  void setNumOfHits(int nhits);
  void setPeak(int index, float peak);
  void setGeantHit(int index ,int* svtAtt, float* AnTime);
  void setGeantHit(int index , StSvtWaferCoordinate* waferCoord);
  void setLocalCoord( int index, StThreeVector<double>* x );
  void setGlobalCoord(int index, StThreeVector<double>* x);
  void setTrackId(int index ,int idtrk);
  int  getTrackId(int index);

  int numberOfHits();
  float peak(int index);
  StSvtWaferCoordinate* waferCoordinate();
  StSvtLocalCoordinate* localCoordinate();
  StGlobalCoordinate*   globalCoordinate();

private:

  int mNumOfHits;
  StSvtWaferCoordinate *mWaferCoord;  //!
  StSvtLocalCoordinate *mLocalCoord;  //!
  StGlobalCoordinate   *mGlobalCoord; //!
  int                  *mIdTrack;
  float *mPeak; // mV

  ClassDef(StSvtGeantHits,1)
};

inline int StSvtGeantHits::numberOfHits()                     {return mNumOfHits  ;}
inline float StSvtGeantHits::peak(int index)                  {return mPeak[index];}
inline StSvtWaferCoordinate* StSvtGeantHits::waferCoordinate(){return mWaferCoord ;}
inline StGlobalCoordinate* StSvtGeantHits::globalCoordinate() {return mGlobalCoord;}
inline StSvtLocalCoordinate* StSvtGeantHits::localCoordinate(){return mLocalCoord ;}

#endif
