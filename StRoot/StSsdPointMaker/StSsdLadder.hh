/*!
 * \class StSsdLadder
 * \author to be filled - doc by L.Martin
 * \date 02/27/04 for the documentation

This class is the description of the SSD ladder objects.
A ladder is made of :

- An unique Id
- A layer number (must be 7)
- the number of wafers on that ladder and the number of strips per wafer side
- An array of pointers to the wafers

 */
#ifndef STSSDLADDER_HH
#define STSSDLADDER_HH

class StSsdWafer;
class St_ssdWafersPosition;

class StSsdLadder
{
 public:
  StSsdLadder(int rLadderNumb,int rSsdLayer, int rNWaferPerLadder, int rNStripPerSide);
 ~StSsdLadder();

  void  initWafers(St_ssdWafersPosition *wafpos);
  int  getLadderNumb();
  int  getWaferPerLadder();
  void debugUnPeu (int monwafer);
  StSsdWafer** mWafers;
  
 private:

  int    mLadderNumb;
  int    mSsdLayer;
  int    mNWaferPerLadder;
  int    mNStripPerSide;

  int idWaferToWaferNumb(int idWafer);
  int waferNumbToIdWafer(int waferNumb);
};

inline int StSsdLadder::getLadderNumb() { return mLadderNumb; }
inline int StSsdLadder::getWaferPerLadder() { return mNWaferPerLadder; }

#endif
