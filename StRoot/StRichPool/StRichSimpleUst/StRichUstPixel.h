#ifndef ST_RICH_UST_PIXEL_HH
#define ST_RICH_UST_PIXEL_HH
#include "TObject.h"

class StRichUstPixel: public TObject {
private:
    Int_t charge ;
    Int_t rawX ;
    Int_t rawY ;
    Float_t localX ;
    Float_t localY ;
public:
     StRichUstPixel();
     StRichUstPixel(StRichUstPixel&);
~StRichUstPixel(){/*noop*/}
    Int_t GetCharge() const { return charge;}
    void SetCharge(Int_t q) {charge = q;}

    Int_t GetRawX() const { return rawX;}
    void SetRawX(Int_t q) {rawX = q;}

    Int_t GetRawY() const { return rawY;}
    void SetRawY(Int_t q) {rawY = q;}

    Float_t GetLocalX() const { return localX;}
    void SetLocalX(Float_t q) {localX = q;}

    Float_t GetLocalY() const { return localY;}
    void SetLocalY(Float_t q) {localY = q;}

    ClassDef(StRichUstPixel,1)
};
#endif
