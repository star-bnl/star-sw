#ifndef BtofHitList_h
#define BtofHitList_h

#include <vector>

#include "StGenericVertexMaker/StiPPVertex/ScintHitList.h"

class StBTofTables;
class StBTofCollection;
class StBTofGeometry;
class St_db_Maker;


class BtofHitList : public ScintHitList {
 private:
  enum {mxTray=120,mxModule=32,mxCell=6};
  int tmc2bin[mxTray][mxModule][mxCell]; // map {t,m,c}--> my bin
  StBTofTables *myTable;

  StBTofGeometry* geometry;

 public:
  BtofHitList();
  virtual  ~BtofHitList();
  void clear();
  void initRun(St_db_Maker* db_maker);
  void build(StBTofCollection *btofColl);
  int  cell2bin(int tray, int module, int cell);
  int  addBtofTrack(int tray, int module, int cell);
  int  addBtofMatch(std::vector<int> ibinVec);
  bool isMatched(std::vector<int> ibinVec);
  bool isVetoed(std::vector<int> ibinVec);
  float getWeight(std::vector<int> ibinVec);
  virtual   int etaBin(float eta);
  virtual float bin2EtaLeft(int iEta);

  StBTofGeometry* Geometry() { return geometry; }
};

#endif
