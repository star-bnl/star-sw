#include "StTriggerStudyEvent.h"
#include <map>
using namespace std;

ClassImp(StTriggerStudyEvent)

StTriggerStudyEvent::StTriggerStudyEvent()
{
  Clear();
}

void StTriggerStudyEvent::Clear(const Option_t* opt)
{
  mJPEt.clear();
  mTriggers.clear();
  mPrescales.clear();
  mSimuTriggers.clear();
  mTPatchDSM.clear();
  mJPatchDSM.clear();
  mTowerDSM.clear();
  mTowerADC.clear();
  mTowers.clear();
  mTPatches.clear();
  mJPatches.clear();
  mBbcTimeBin = -1;
  mVertexPosition.setX(-999);
  mVertexPosition.setY(-999);
  mVertexPosition.setZ(-999);
}
