#ifndef ST_RICH_UST_TRACK_HH
#define ST_RICH_UST_TRACK_HH
#include "TObject.h"

class StRichUstTrack: public TObject {
private:
    Int_t nFitPoints ;
    Int_t trackQ ;
    Float_t originX ;
    Float_t originY ;
    Float_t originZ ;
    Float_t curvature ;
    Float_t psi ;
    Float_t firstX ;
    Float_t firstY ;
    Float_t firstZ ;
    Float_t lastX ;
    Float_t lastY ;
    Float_t lastZ ;
    Int_t otherIndex ;
    Int_t isLambda ;
    Float_t lambdaMass ;
    Float_t alphaV0 ;
    Float_t globalPx ;
    Float_t globalPy ;
    Float_t globalPz ;
    Float_t p ;
    Float_t innerPx ;
    Float_t innerPy ;
    Float_t innerPz ;
    Float_t innerP ;
    Float_t innerPt ;
    Float_t DCA ;
    Float_t DCAx ;
    Float_t DCAy ;
    Float_t DCAz ;
    Float_t DCA3d ;
    Float_t gSDCA ;
    Float_t gSDCA2d ;
    Float_t eta ;
    Float_t localRadX ;
    Float_t localRadY ;
    Float_t localRadZ ;
    Float_t localRadPx ;
    Float_t localRadPy ;
    Float_t localRadPz ;
    Float_t localPadX ;
    Float_t localPadY ;
    Float_t localPadZ ;
    Float_t globalPadX ;
    Float_t globalPadY ;
    Float_t globalPadZ ;
    Float_t localPadPx ;
    Float_t localPadPy ;
    Float_t localPadPz ;
    Float_t residX ;
    Float_t residY ;
    Float_t residZ ;
    Float_t resid ;
    Int_t mipIndex ;
    Float_t spX ;
    Float_t spY ;
    Float_t spDx ;
    Float_t spDy ;
    Float_t spCdx ;
    Float_t spCdy ;
    Float_t spMass2 ;
    Float_t spCherenkovAngle ;
    Int_t spNphotons ;
    Int_t trackFlag ;
public:
     StRichUstTrack();
     StRichUstTrack(StRichUstTrack&);
~StRichUstTrack(){/*noop*/}
    Int_t GetNFitPoints() const { return nFitPoints;}
    void SetNFitPoints(Int_t q) {nFitPoints = q;}

    Int_t GetTrackQ() const { return trackQ;}
    void SetTrackQ(Int_t q) {trackQ = q;}

    Float_t GetOriginX() const { return originX;}
    void SetOriginX(Float_t q) {originX = q;}

    Float_t GetOriginY() const { return originY;}
    void SetOriginY(Float_t q) {originY = q;}

    Float_t GetOriginZ() const { return originZ;}
    void SetOriginZ(Float_t q) {originZ = q;}

    Float_t GetCurvature() const { return curvature;}
    void SetCurvature(Float_t q) {curvature = q;}

    Float_t GetPsi() const { return psi;}
    void SetPsi(Float_t q) {psi = q;}

    Float_t GetFirstX() const { return firstX;}
    void SetFirstX(Float_t q) {firstX = q;}

    Float_t GetFirstY() const { return firstY;}
    void SetFirstY(Float_t q) {firstY = q;}

    Float_t GetFirstZ() const { return firstZ;}
    void SetFirstZ(Float_t q) {firstZ = q;}

    Float_t GetLastX() const { return lastX;}
    void SetLastX(Float_t q) {lastX = q;}

    Float_t GetLastY() const { return lastY;}
    void SetLastY(Float_t q) {lastY = q;}

    Float_t GetLastZ() const { return lastZ;}
    void SetLastZ(Float_t q) {lastZ = q;}

    Int_t GetOtherIndex() const { return otherIndex;}
    void SetOtherIndex(Int_t q) {otherIndex = q;}

    Int_t GetIsLambda() const { return isLambda;}
    void SetIsLambda(Int_t q) {isLambda = q;}

    Float_t GetLambdaMass() const { return lambdaMass;}
    void SetLambdaMass(Float_t q) {lambdaMass = q;}

    Float_t GetAlphaV0() const { return alphaV0;}
    void SetAlphaV0(Float_t q) {alphaV0 = q;}

    Float_t GetGlobalPx() const { return globalPx;}
    void SetGlobalPx(Float_t q) {globalPx = q;}

    Float_t GetGlobalPy() const { return globalPy;}
    void SetGlobalPy(Float_t q) {globalPy = q;}

    Float_t GetGlobalPz() const { return globalPz;}
    void SetGlobalPz(Float_t q) {globalPz = q;}

    Float_t GetP() const { return p;}
    void SetP(Float_t q) {p = q;}

    Float_t GetInnerPx() const { return innerPx;}
    void SetInnerPx(Float_t q) {innerPx = q;}

    Float_t GetInnerPy() const { return innerPy;}
    void SetInnerPy(Float_t q) {innerPy = q;}

    Float_t GetInnerPz() const { return innerPz;}
    void SetInnerPz(Float_t q) {innerPz = q;}

    Float_t GetInnerP() const { return innerP;}
    void SetInnerP(Float_t q) {innerP = q;}

    Float_t GetInnerPt() const { return innerPt;}
    void SetInnerPt(Float_t q) {innerPt = q;}

    Float_t GetDCA() const { return DCA;}
    void SetDCA(Float_t q) {DCA = q;}

    Float_t GetDCAx() const { return DCAx;}
    void SetDCAx(Float_t q) {DCAx = q;}

    Float_t GetDCAy() const { return DCAy;}
    void SetDCAy(Float_t q) {DCAy = q;}

    Float_t GetDCAz() const { return DCAz;}
    void SetDCAz(Float_t q) {DCAz = q;}

    Float_t GetDCA3d() const { return DCA3d;}
    void SetDCA3d(Float_t q) {DCA3d = q;}

    Float_t GetGSDCA() const { return gSDCA;}
    void SetGSDCA(Float_t q) {gSDCA = q;}

    Float_t GetGSDCA2d() const { return gSDCA2d;}
    void SetGSDCA2d(Float_t q) {gSDCA2d = q;}

    Float_t GetEta() const { return eta;}
    void SetEta(Float_t q) {eta = q;}

    Float_t GetLocalRadX() const { return localRadX;}
    void SetLocalRadX(Float_t q) {localRadX = q;}

    Float_t GetLocalRadY() const { return localRadY;}
    void SetLocalRadY(Float_t q) {localRadY = q;}

    Float_t GetLocalRadZ() const { return localRadZ;}
    void SetLocalRadZ(Float_t q) {localRadZ = q;}

    Float_t GetLocalRadPx() const { return localRadPx;}
    void SetLocalRadPx(Float_t q) {localRadPx = q;}

    Float_t GetLocalRadPy() const { return localRadPy;}
    void SetLocalRadPy(Float_t q) {localRadPy = q;}

    Float_t GetLocalRadPz() const { return localRadPz;}
    void SetLocalRadPz(Float_t q) {localRadPz = q;}

    Float_t GetLocalPadX() const { return localPadX;}
    void SetLocalPadX(Float_t q) {localPadX = q;}

    Float_t GetLocalPadY() const { return localPadY;}
    void SetLocalPadY(Float_t q) {localPadY = q;}

    Float_t GetLocalPadZ() const { return localPadZ;}
    void SetLocalPadZ(Float_t q) {localPadZ = q;}

    Float_t GetGlobalPadX() const { return globalPadX;}
    void SetGlobalPadX(Float_t q) {globalPadX = q;}

    Float_t GetGlobalPadY() const { return globalPadY;}
    void SetGlobalPadY(Float_t q) {globalPadY = q;}

    Float_t GetGlobalPadZ() const { return globalPadZ;}
    void SetGlobalPadZ(Float_t q) {globalPadZ = q;}

    Float_t GetLocalPadPx() const { return localPadPx;}
    void SetLocalPadPx(Float_t q) {localPadPx = q;}

    Float_t GetLocalPadPy() const { return localPadPy;}
    void SetLocalPadPy(Float_t q) {localPadPy = q;}

    Float_t GetLocalPadPz() const { return localPadPz;}
    void SetLocalPadPz(Float_t q) {localPadPz = q;}

    Float_t GetResidX() const { return residX;}
    void SetResidX(Float_t q) {residX = q;}

    Float_t GetResidY() const { return residY;}
    void SetResidY(Float_t q) {residY = q;}

    Float_t GetResidZ() const { return residZ;}
    void SetResidZ(Float_t q) {residZ = q;}

    Float_t GetResid() const { return resid;}
    void SetResid(Float_t q) {resid = q;}

    Int_t GetMipIndex() const { return mipIndex;}
    void SetMipIndex(Int_t q) {mipIndex = q;}

    Float_t GetSpX() const { return spX;}
    void SetSpX(Float_t q) {spX = q;}

    Float_t GetSpY() const { return spY;}
    void SetSpY(Float_t q) {spY = q;}

    Float_t GetSpDx() const { return spDx;}
    void SetSpDx(Float_t q) {spDx = q;}

    Float_t GetSpDy() const { return spDy;}
    void SetSpDy(Float_t q) {spDy = q;}

    Float_t GetSpCdx() const { return spCdx;}
    void SetSpCdx(Float_t q) {spCdx = q;}

    Float_t GetSpCdy() const { return spCdy;}
    void SetSpCdy(Float_t q) {spCdy = q;}

    Float_t GetSpMass2() const { return spMass2;}
    void SetSpMass2(Float_t q) {spMass2 = q;}

    Float_t GetSpCherenkovAngle() const { return spCherenkovAngle;}
    void SetSpCherenkovAngle(Float_t q) {spCherenkovAngle = q;}

    Int_t GetSpNphotons() const { return spNphotons;}
    void SetSpNphotons(Int_t q) {spNphotons = q;}

    Int_t GetTrackFlag() const { return trackFlag;}
    void SetTrackFlag(Int_t q) {trackFlag = q;}

    ClassDef(StRichUstTrack,1)
};
#endif
