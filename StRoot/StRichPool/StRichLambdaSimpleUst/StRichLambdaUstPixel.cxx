#include "StRichLambdaUstPixel.h"

ClassImp(StRichLambdaUstPixel)
StRichLambdaUstPixel::StRichLambdaUstPixel() {
    charge = -999;
    rawX = -999;
    rawY = -999;
    localX = -999;
    localY = -999;
}
StRichLambdaUstPixel::StRichLambdaUstPixel(StRichLambdaUstPixel& in) {
    charge = in.GetCharge();
    rawX = in.GetRawX();
    rawY = in.GetRawY();
    localX = in.GetLocalX();
    localY = in.GetLocalY();
}