#ifndef ST_RICH_UST_HIT_HH
#define ST_RICH_UST_HIT_HH
#include "TObject.h"

class StRichLambdaUstHit: public TObject {
private:
    Int_t charge ;
    Int_t maxAdc ;
    Int_t nPads ;
    ULong_t flag ;
    Int_t clusterNumber ;
    Int_t clusterFirstPad ;
    Int_t nClusterPads ;
    Int_t nClusterLocalMax ;
    Int_t clusterMinAmp ;
    Float_t rawX ;
    Float_t rawY ;
    Float_t localX ;
    Float_t localY ;
    Float_t globalX ;
    Float_t globalY ;
    Float_t globalZ ;
public:
     StRichLambdaUstHit();
     StRichLambdaUstHit(StRichLambdaUstHit&);
~StRichLambdaUstHit(){/*noop*/}
    Int_t GetCharge() const { return charge;}
    void SetCharge(Int_t q) {charge = q;}

    Int_t GetMaxAdc() const { return maxAdc;}
    void SetMaxAdc(Int_t q) {maxAdc = q;}

    Int_t GetNPads() const { return nPads;}
    void SetNPads(Int_t q) {nPads = q;}

    ULong_t GetFlag() const { return flag;}
    void SetFlag(ULong_t q) {flag = q;}

    Int_t GetClusterNumber() const { return clusterNumber;}
    void SetClusterNumber(Int_t q) {clusterNumber = q;}

    Int_t GetClusterFirstPad() const { return clusterFirstPad;}
    void SetClusterFirstPad(Int_t q) {clusterFirstPad = q;}

    Int_t GetNClusterPads() const { return nClusterPads;}
    void SetNClusterPads(Int_t q) {nClusterPads = q;}

    Int_t GetNClusterLocalMax() const { return nClusterLocalMax;}
    void SetNClusterLocalMax(Int_t q) {nClusterLocalMax = q;}

    Int_t GetClusterMinAmp() const { return clusterMinAmp;}
    void SetClusterMinAmp(Int_t q) {clusterMinAmp = q;}

    Float_t GetRawX() const { return rawX;}
    void SetRawX(Float_t q) {rawX = q;}

    Float_t GetRawY() const { return rawY;}
    void SetRawY(Float_t q) {rawY = q;}

    Float_t GetLocalX() const { return localX;}
    void SetLocalX(Float_t q) {localX = q;}

    Float_t GetLocalY() const { return localY;}
    void SetLocalY(Float_t q) {localY = q;}

    Float_t GetGlobalX() const { return globalX;}
    void SetGlobalX(Float_t q) {globalX = q;}

    Float_t GetGlobalY() const { return globalY;}
    void SetGlobalY(Float_t q) {globalY = q;}

    Float_t GetGlobalZ() const { return globalZ;}
    void SetGlobalZ(Float_t q) {globalZ = q;}

    ClassDef(StRichLambdaUstHit,1)
};
#endif
