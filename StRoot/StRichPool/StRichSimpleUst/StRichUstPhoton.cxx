#include "StRichUstPhoton.h"

ClassImp(StRichUstPhoton)
StRichUstPhoton::StRichUstPhoton() {
    ring = -999;
    track = -999;
    hit = -999;
    d = -999;
    sig = -999;
    psi = -999;
}
StRichUstPhoton::StRichUstPhoton(StRichUstPhoton& in) {
    ring = in.GetRing();
    track = in.GetTrack();
    hit = in.GetHit();
    d = in.GetD();
    sig = in.GetSig();
    psi = in.GetPsi();
}