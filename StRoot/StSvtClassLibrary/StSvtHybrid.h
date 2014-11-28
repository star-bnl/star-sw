#ifndef StSvtHybrid_h
#define StSvtHybrid_h
/*************************************************************
 *  $Id:
 * $Log:
 *************************************************************/
class StSvtHybrid {
public:
  StSvtHybrid(int barrel = 0, int ladder = 0, int wafer = 0, int hybrid = 0) {  
    mBarrel = barrel;  mLadder = ladder;  mWafer  = wafer;  mHybrid = hybrid;
  }
  virtual ~StSvtHybrid() {}
  int getBarrelID() {return mBarrel;} // return Barrel number
  int getLadderID() {return mLadder;} // return Ladder number
  int getWaferID()  {return mWafer;}  // return Wafer number
  int getHybridID() {return mHybrid;} // return Hybrid number
  int getLayerID()  {return 2*mBarrel +  mLadder%2 - 1;}    // return Layer number

  void setBarrelID(int barrel) {mBarrel = barrel;} // set Barrel number
  void setLadderID(int ladder) {mLadder = ladder;} // set Ladder number
  void setWaferID(int wafer)   {mWafer = wafer;}    // set Wafer number
  void setHybridID(int hybrid) {mHybrid = hybrid;} // set Hybrid number
  void setHybrid(int barrel, int ladder, int wafer, int hybrid) {mBarrel = barrel;
                                                                 mLadder = ladder;
                                                                 mWafer = wafer;
                                                                 mHybrid = hybrid;}

protected:

  int mBarrel; //  Barrel number
  int mLayer;  //  Layer number
  int mLadder; //  Ladder number
  int mWafer;  //  Wafer number
  int mHybrid; //  Hybrid number
};
#endif
