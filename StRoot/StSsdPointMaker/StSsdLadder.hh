#ifndef STSSDLADDER_HH
#define STSSDLADDER_HH

class StSsdWafer;
class St_svg_geom;

class StSsdLadder
{
 public:
  StSsdLadder(int rLadderNumb,int rSsdLayer, int rNWaferPerLadder, int rNStripPerSide);
 ~StSsdLadder();

  void  initWafers(St_svg_geom *geom_class);

  StSsdWafer** mWafers;
  
 private:

  int    mLadderNumb;
  int    mSsdLayer;
  int    mNWaferPerLadder;
  int    mNStripPerSide;

  int idWaferToWaferNumb(int idWafer);
  int waferNumbToIdWafer(int waferNumb);
};
#endif
