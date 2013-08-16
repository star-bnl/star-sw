#ifndef BtofHitList_h
#define BtofHitList_h

#include "ScintHitList.h"
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
typedef std::vector<Int_t>  IntVec;

class StBTofTables;
class StBTofCollection;

namespace StEvPPV {
class BtofHitList : public ScintHitList {
 private:
  enum {mxTray=120,mxModule=32,mxCell=6};
  int tmc2bin[mxTray][mxModule][mxCell]; // map {t,m,c}--> my bin
  StBTofTables *myTable;

 public:
  BtofHitList();
  virtual  ~BtofHitList();
  void clear();
  void initRun();
  void build(StBTofCollection *btofColl);
  int  cell2bin(int tray, int module, int cell);
  int  addBtofTrack(int tray, int module, int cell);
  int  addBtofMatch(IntVec ibinVec);
  bool isMatched(IntVec ibinVec);
  bool isVetoed(IntVec ibinVec);
  float getWeight(IntVec ibinVec);
  virtual   int etaBin(float eta);
  virtual float bin2EtaLeft(int iEta);
};
}// end namespace StEvPPV


#endif
