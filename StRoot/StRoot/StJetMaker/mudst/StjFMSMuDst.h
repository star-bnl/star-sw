// -*- mode: c++;-*-
// $Id: StjFMSMuDst.h,v 1.1 2017/05/22 19:35:00 zchang Exp $
#ifndef STJFMSMUDST_H
#define STJFMSMUDST_H

#include "StjFMS.h"

//#include "TMatrix.h"
//#include "StFmsDbMaker.h";

//class TTree;
class StFmsCollection;

class StMuDstMaker;
//class StEEmcDb;
class StFmsDbMaker;
//class StMuEvent;
//class StMuTrack;

class StjFMSMuDst : public StjFMS {

public:
  StjFMSMuDst();
  virtual ~StjFMSMuDst() { }


  void Init();
  void setVertex(float vx, float vy, float vz)
  {
    _setVertex = true;
    _vx = vx;
    _vy = vy;
    _vz = vz;
  }
  StjTowerEnergyList getEnergyList();
  StFmsCollection*   findFmsCollection();


private:

  StFmsDbMaker* mFmsDbMaker;
  StFmsCollection* mFmsColl;
  //  StEEmcDb* mEeDb;

  bool _setVertex;

  double _vx;
  double _vy;
  double _vz;
  //  StFmsDbMaker* fmsdb;

};

#endif // STJFMSMUDST_H
