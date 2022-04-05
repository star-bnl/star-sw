#include "StDetectorDbMaker/St_tpcChargeEventC.h"
#include "St_starClockOnlC.h"


double St_tpcChargeEventC::timeDifference(unsigned long long bunchCrossingNumber, int idx) {
    // time difference between the event # idx and bunchCrossingNumber
    return ((double) (bunchCrossingNumber - eventBunchCrossing(idx))) /
      (St_starClockOnlC::instance()->Frequency());
  }

int St_tpcChargeEventC::indexBeforeBunchCrossing(unsigned long long bunchCrossingNumber) {
  // optimized search for typically looking for an index which
  // is the same or very close to previously found index

  int lastIndex = nChargeEvents() - 1;
  if (lastIndex < 0) return -999;

  if (localSearchUpperIndex < 0) { // no search yet
    // start from the middle and move outwards
    localSearchLowerIndex = nChargeEvents() / 2;
    localSearchUpperIndex = localSearchLowerIndex;
  }

  // try the same one as before first
  int direction = 0;
  if (bunchCrossingNumber >= eventBunchCrossing(localSearchUpperIndex) &&
      localSearchLowerIndex != lastIndex) direction = 1;
  else if (bunchCrossingNumber < eventBunchCrossing(localSearchLowerIndex)) direction = -1;
  else return localSearchLowerIndex;

  int delta = 1; // look up or down just one first

  while (direction > 0) { // look for higher indices
    localSearchLowerIndex = localSearchUpperIndex;
    localSearchUpperIndex = std::min(lastIndex,localSearchUpperIndex + delta);
    delta = localSearchUpperIndex - localSearchLowerIndex;
    if (bunchCrossingNumber < eventBunchCrossing(localSearchUpperIndex)) direction = 0;
    else if (localSearchUpperIndex == lastIndex) {
      localSearchLowerIndex = lastIndex;
      return lastIndex; // local indices will be lastIndex,lastIndex
    } else delta *= 2; // expand range and keep looking for higher indices
  }
  while (direction < 0) { // look for lower indices
    localSearchUpperIndex = localSearchLowerIndex;
    localSearchLowerIndex = std::max(0,localSearchLowerIndex - delta);
    delta = localSearchUpperIndex - localSearchLowerIndex;
    if (bunchCrossingNumber >= eventBunchCrossing(localSearchLowerIndex)) direction = 0;
    else if (localSearchLowerIndex == 0) {
      localSearchUpperIndex = 0;
      return -1; // local indices will be 0,0
    } else delta *= 2; // expand range and keep looking for lower indices
  }

  // already know that the result is within range
  while (delta > 1) {
    int tempIndex = localSearchLowerIndex + (delta/2);
    if (bunchCrossingNumber < eventBunchCrossing(tempIndex)) localSearchUpperIndex = tempIndex;
    else localSearchLowerIndex = tempIndex;
    delta = localSearchUpperIndex - localSearchLowerIndex;
  }
  // found
  return localSearchLowerIndex;
}

int St_tpcChargeEventC::findChargeTimes(unsigned long long bunchCrossingNumber, unsigned long long bunchCrossingWindow) {
  int idx2 = indexBeforeBunchCrossing(bunchCrossingNumber);
  int idx1 = indexBeforeBunchCrossing(bunchCrossingNumber-bunchCrossingWindow);
  int n = idx2-idx1;
  idx1++; // start from after the bunchCrossingWindow starts
  localStoreCharges.Set(n,&(eventCharges()[idx1]));
  localStoreTimesSinceCharges.Set(n);
  for (int i=0; i<n; i++) // must convert to times
    localStoreTimesSinceCharges.AddAt(timeDifference(bunchCrossingNumber,idx1+i),i);
  return n;
}

int St_tpcChargeEventC::findChargeTimes(unsigned long long bunchCrossingNumber, double timeWindow) {
  return findChargeTimes(bunchCrossingNumber,
    (unsigned long long) (timeWindow*(St_starClockOnlC::instance()->Frequency())));
}
