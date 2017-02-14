#ifndef EemcHitList_h
#define EemcHitList_h
#include <sys/types.h>
#include "ScintHitList.h"
#include "StEEmcUtil/EEfeeRaw/EEdims.h"
class StEmcDetector;
class StEEmcDb;
class EEmcGeomSimple;

namespace StEvPPV {
class EemcHitList : public ScintHitList {
 private:

  StEEmcDb* eeDb; 
  EEmcGeomSimple *geomE;
  int name2bin[MaxSectors][MaxSubSec][MaxEtaBins]; // map --> my bin
  const Float_t *etaHL; // limits of eta bins

  //params
  unsigned int killStatEEmc;
 
 public:
 EemcHitList(StEEmcDb* x, unsigned int y, EEmcGeomSimple *z);
  virtual  ~EemcHitList();
  void clear();
  void initRun();
  void build( StEmcDetector*det, float adcMin);
  virtual  int etaBin(float eta);
  virtual float bin2EtaLeft(int iEta);

};
}// end namespace StEvPPV

#endif
