#ifndef ST_RICH_UST_PHOTON_HH
#define ST_RICH_UST_PHOTON_HH
#include "TObject.h"

class StRichLambdaUstPhoton: public TObject {
private:
    Int_t ring ;
    Int_t track ;
    Int_t hit ;
    Float_t d ;
    Float_t sig ;
    Float_t psi ;
public:
     StRichLambdaUstPhoton();
     StRichLambdaUstPhoton(StRichLambdaUstPhoton&);
~StRichLambdaUstPhoton(){/*noop*/}
    Int_t GetRing() const { return ring;}
    void SetRing(Int_t q) {ring = q;}

    Int_t GetTrack() const { return track;}
    void SetTrack(Int_t q) {track = q;}

    Int_t GetHit() const { return hit;}
    void SetHit(Int_t q) {hit = q;}

    Float_t GetD() const { return d;}
    void SetD(Float_t q) {d = q;}

    Float_t GetSig() const { return sig;}
    void SetSig(Float_t q) {sig = q;}

    Float_t GetPsi() const { return psi;}
    void SetPsi(Float_t q) {psi = q;}

    ClassDef(StRichLambdaUstPhoton,1)
};
#endif
