#include "StRichLambdaUstPhoton.h"

ClassImp(StRichLambdaUstPhoton)
StRichLambdaUstPhoton::StRichLambdaUstPhoton() {
    ring = -999;
    track = -999;
    hit = -999;
    d = -999;
    sig = -999;
    psi = -999;
}
StRichLambdaUstPhoton::StRichLambdaUstPhoton(StRichLambdaUstPhoton& in) {
    ring = in.GetRing();
    track = in.GetTrack();
    hit = in.GetHit();
    d = in.GetD();
    sig = in.GetSig();
    psi = in.GetPsi();
}
