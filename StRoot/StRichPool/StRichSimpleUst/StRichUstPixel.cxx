#include "StRichUstPixel.h"

ClassImp(StRichUstPixel)
StRichUstPixel::StRichUstPixel() {
    charge = -999;
    rawX = -999;
    rawY = -999;
    localX = -999;
    localY = -999;
}
StRichUstPixel::StRichUstPixel(StRichUstPixel& in) {
    charge = in.GetCharge();
    rawX = in.GetRawX();
    rawY = in.GetRawY();
    localX = in.GetLocalX();
    localY = in.GetLocalY();
}
