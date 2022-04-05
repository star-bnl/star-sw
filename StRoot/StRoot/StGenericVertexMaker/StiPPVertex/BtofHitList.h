#ifndef BtofHitList_h
#define BtofHitList_h

#include "StGenericVertexMaker/StiPPVertex/ScintHitList.h"
#include <vector>

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#ifndef __CINT__
#if !defined(ST_NO_TEMPLATE_DEF_ARGS)
typedef vector<Int_t>  IntVec;
#else
typedef vector<Int_t, allocator<Int_t> >  IntVec;
#endif
#endif

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
  int  addBtofMatch(IntVec ibinVec);
  bool isMatched(IntVec ibinVec);
  bool isVetoed(IntVec ibinVec);
  float getWeight(IntVec ibinVec);
  virtual   int etaBin(float eta);
  virtual float bin2EtaLeft(int iEta);

  StBTofGeometry* Geometry() { return geometry; }
};

#endif
