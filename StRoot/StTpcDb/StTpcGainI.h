#include <TObject.h>

class StTpcGainI : public TObject {

  //Abstract base class defining accessors
public:

  virtual float getGain(int sector, int row, int pad)   const = 0;
  virtual float getOnlineGain(int sector, int row, int pad) const = 0;
  virtual float getNominalGain(int sector, int row, int pad) const = 0;
  virtual float getRelativeGain(int sector, int row, int pad) const = 0;
  virtual float getAverageGainInner(int sector) const = 0;
  virtual float getAverageGainOuter(int sector) const = 0;

};
















