/*
  Generic temporary class for STAR subdetectors (e.g. TPC, SVT, FTPC)
  Each Subdetector has some devices (TPC has 24 devices, which means sectors)
  Each device has some rows (TPC sector has 45 rows, which means padrows)
  Each row has some hits on it.  Here, we talk of *local* hit positions

  MALisa 9jun99
*/


#ifndef StSubDetector_HH
#define  StSubDetector_HH

#include <vector>

class StDevice;
class StTpcHit;
class StMcTpcHit;
// #include "StDevice.hh"
// #include "StEvent/StTpcHit.hh"
// #include "StMcEvent/StMcTpcHit.hh"


class StSubDetector{

public:
  StSubDetector(const unsigned int nDevices=24, const unsigned int nRows=45);
  ~StSubDetector();

  void               addHit(const StTpcHit*);
  void               addHit(const StMcTpcHit*);
  unsigned int       numOfDevices() { return mNDevices; };
  StDevice*          device(const int iDev){return mDevices[iDev];};

private:
  vector<StDevice*>  mDevices;
  unsigned int mNDevices;

};

#endif
