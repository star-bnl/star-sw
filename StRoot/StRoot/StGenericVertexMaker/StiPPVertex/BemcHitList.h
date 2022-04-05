#ifndef BemcHitList_h
#define BemcHitList_h

#include "StGenericVertexMaker/StiPPVertex/ScintHitList.h"
class StEmcDetector;
class StBemcTables;
class StEmcGeom ;
class St_db_Maker;


class BemcHitList : public ScintHitList {
 private:
  enum {mxm=120,mxe=20,mxs=2};
  int mes2bin[mxm][mxe][mxs]; // map module, eta, sub {m,e,s} --> my bin iBin=iPhi+nPhi*iEta;
  StBemcTables *myTable;
  StEmcGeom *geomB;
  // params:
  float kSigPed;

 public:
 BemcHitList();
  virtual  ~BemcHitList();
  void clear();
  void initRun(St_db_Maker* db_maker);
  void build( StEmcDetector*det, float adcMin);
  virtual   int etaBin(float eta);
  virtual float bin2EtaLeft(int iEta);
};

#endif
