#ifndef ST_RICH_LAMBDA_UST_TRACK_HH
#define ST_RICH_LAMBDA_UST_TRACK_HH
#include "TObject.h"

class StRichLambdaUstTrack: public TObject {
private:
    Int_t nFitPoints ;
    Int_t trackQ ;
    Double_t originX ;
    Double_t originY ;
    Double_t originZ ;
    Double_t curvature ;
    Double_t psi ;
    Double_t massSquared ;
    Double_t cherenkovAngle ;
    Double_t nPhotons ;
    Double_t firstX ;
    Double_t firstY ;
    Double_t firstZ ;
    Double_t lastX ;
    Double_t lastY ;
    Double_t lastZ ;
    Int_t otherIndex ;
    Double_t globalPx ;
    Double_t globalPy ;
    Double_t globalPz ;
    Double_t p ;
    Double_t DCA ;
    Double_t DCAx ;
    Double_t DCAy ;
    Double_t DCAz ;
    Double_t DCA3d ;
    Double_t gSDCA ;
    Double_t gSDCA2d ;
    Double_t eta ;
    Double_t localRadX ;
    Double_t localRadY ;
    Double_t localRadZ ;
    Double_t localRadPx ;
    Double_t localRadPy ;
    Double_t localRadPz ;
    Double_t globalRadX ;
    Double_t globalRadY ;
    Double_t globalRadZ ;
    Double_t globalRadPx ;
    Double_t globalRadPy ;
    Double_t globalRadPz ;
    Double_t localPadX ;
    Double_t localPadY ;
    Double_t localPadZ ;
    Double_t localPadPx ;
    Double_t localPadPy ;
    Double_t localPadPz ;
    Double_t globalPadPx ;
    Double_t globalPadPy ;
    Double_t globalPadPz ;
    Double_t globalPadX ;
    Double_t globalPadY ;
    Double_t globalPadZ ;
    Int_t crossedRich ;
    Int_t isLambda ;
    Double_t lambdaMass ;
    Double_t alphaLambda ;
    Double_t ptArmLambda ;
    Double_t globalXLambda ;
    Double_t globalYLambda ;
    Double_t globalZLambda ;
    Double_t globalPxLambda ;
    Double_t globalPyLambda ;
    Double_t globalPzLambda ;
    Int_t otherLambdaDaughter ;
    Int_t isAntiLambda ;
    Double_t antiLambdaMass ;
    Double_t alphaAntiLambda ;
    Double_t ptArmAntiLambda ;
    Double_t globalXAntiLambda ;
    Double_t globalYAntiLambda ;
    Double_t globalZAntiLambda ;
    Double_t globalPxAntiLambda ;
    Double_t globalPyAntiLambda ;
    Double_t globalPzAntiLambda ;
    Int_t otherAntiLambdaDaughter ;
    Int_t isK0Short ;
    Double_t k0ShortMass ;
    Double_t alphaK0Short ;
    Double_t ptArmK0Short ;
    Double_t globalXK0Short ;
    Double_t globalYK0Short ;
    Double_t globalZK0Short ;
    Double_t globalPxK0Short ;
    Double_t globalPyK0Short ;
    Double_t globalPzK0Short ;
    Int_t otherK0ShortDaughter ;
    Double_t residX ;
    Double_t residY ;
    Double_t resid ;
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
     StRichLambdaUstTrack();
     StRichLambdaUstTrack(StRichLambdaUstTrack&);
~StRichLambdaUstTrack(){/*noop*/}
    Int_t GetNFitPoints() const { return nFitPoints;}
    void SetNFitPoints(Int_t q) {nFitPoints = q;}

    Int_t GetTrackQ() const { return trackQ;}
    void SetTrackQ(Int_t q) {trackQ = q;}

    Double_t GetOriginX() const { return originX;}
    void SetOriginX(Double_t q) {originX = q;}

    Double_t GetOriginY() const { return originY;}
    void SetOriginY(Double_t q) {originY = q;}

    Double_t GetOriginZ() const { return originZ;}
    void SetOriginZ(Double_t q) {originZ = q;}

    Double_t GetCurvature() const { return curvature;}
    void SetCurvature(Double_t q) {curvature = q;}

    Double_t GetPsi() const { return psi;}
    void SetPsi(Double_t q) {psi = q;}

    Double_t GetMassSquared() const { return massSquared;}
    void SetMassSquared(Double_t q) {massSquared = q;}

    Double_t GetCherenkovAngle() const { return cherenkovAngle;}
    void SetCherenkovAngle(Double_t q) {cherenkovAngle = q;}

    Double_t GetNPhotons() const { return nPhotons;}
    void SetNPhotons(Double_t q) {nPhotons = q;}

    Double_t GetFirstX() const { return firstX;}
    void SetFirstX(Double_t q) {firstX = q;}

    Double_t GetFirstY() const { return firstY;}
    void SetFirstY(Double_t q) {firstY = q;}

    Double_t GetFirstZ() const { return firstZ;}
    void SetFirstZ(Double_t q) {firstZ = q;}

    Double_t GetLastX() const { return lastX;}
    void SetLastX(Double_t q) {lastX = q;}

    Double_t GetLastY() const { return lastY;}
    void SetLastY(Double_t q) {lastY = q;}

    Double_t GetLastZ() const { return lastZ;}
    void SetLastZ(Double_t q) {lastZ = q;}

    Int_t GetOtherIndex() const { return otherIndex;}
    void SetOtherIndex(Int_t q) {otherIndex = q;}

    Double_t GetGlobalPx() const { return globalPx;}
    void SetGlobalPx(Double_t q) {globalPx = q;}

    Double_t GetGlobalPy() const { return globalPy;}
    void SetGlobalPy(Double_t q) {globalPy = q;}

    Double_t GetGlobalPz() const { return globalPz;}
    void SetGlobalPz(Double_t q) {globalPz = q;}

    Double_t GetP() const { return p;}
    void SetP(Double_t q) {p = q;}

    Double_t GetDCA() const { return DCA;}
    void SetDCA(Double_t q) {DCA = q;}

    Double_t GetDCAx() const { return DCAx;}
    void SetDCAx(Double_t q) {DCAx = q;}

    Double_t GetDCAy() const { return DCAy;}
    void SetDCAy(Double_t q) {DCAy = q;}

    Double_t GetDCAz() const { return DCAz;}
    void SetDCAz(Double_t q) {DCAz = q;}

    Double_t GetDCA3d() const { return DCA3d;}
    void SetDCA3d(Double_t q) {DCA3d = q;}

    Double_t GetGSDCA() const { return gSDCA;}
    void SetGSDCA(Double_t q) {gSDCA = q;}

    Double_t GetGSDCA2d() const { return gSDCA2d;}
    void SetGSDCA2d(Double_t q) {gSDCA2d = q;}

    Double_t GetEta() const { return eta;}
    void SetEta(Double_t q) {eta = q;}

    Double_t GetLocalRadX() const { return localRadX;}
    void SetLocalRadX(Double_t q) {localRadX = q;}

    Double_t GetLocalRadY() const { return localRadY;}
    void SetLocalRadY(Double_t q) {localRadY = q;}

    Double_t GetLocalRadZ() const { return localRadZ;}
    void SetLocalRadZ(Double_t q) {localRadZ = q;}

    Double_t GetLocalRadPx() const { return localRadPx;}
    void SetLocalRadPx(Double_t q) {localRadPx = q;}

    Double_t GetLocalRadPy() const { return localRadPy;}
    void SetLocalRadPy(Double_t q) {localRadPy = q;}

    Double_t GetLocalRadPz() const { return localRadPz;}
    void SetLocalRadPz(Double_t q) {localRadPz = q;}

    Double_t GetGlobalRadX() const { return globalRadX;}
    void SetGlobalRadX(Double_t q) {globalRadX = q;}

    Double_t GetGlobalRadY() const { return globalRadY;}
    void SetGlobalRadY(Double_t q) {globalRadY = q;}

    Double_t GetGlobalRadZ() const { return globalRadZ;}
    void SetGlobalRadZ(Double_t q) {globalRadZ = q;}

    Double_t GetGlobalRadPx() const { return globalRadPx;}
    void SetGlobalRadPx(Double_t q) {globalRadPx = q;}

    Double_t GetGlobalRadPy() const { return globalRadPy;}
    void SetGlobalRadPy(Double_t q) {globalRadPy = q;}

    Double_t GetGlobalRadPz() const { return globalRadPz;}
    void SetGlobalRadPz(Double_t q) {globalRadPz = q;}

    Double_t GetLocalPadX() const { return localPadX;}
    void SetLocalPadX(Double_t q) {localPadX = q;}

    Double_t GetLocalPadY() const { return localPadY;}
    void SetLocalPadY(Double_t q) {localPadY = q;}

    Double_t GetLocalPadZ() const { return localPadZ;}
    void SetLocalPadZ(Double_t q) {localPadZ = q;}

    Double_t GetLocalPadPx() const { return localPadPx;}
    void SetLocalPadPx(Double_t q) {localPadPx = q;}

    Double_t GetLocalPadPy() const { return localPadPy;}
    void SetLocalPadPy(Double_t q) {localPadPy = q;}

    Double_t GetLocalPadPz() const { return localPadPz;}
    void SetLocalPadPz(Double_t q) {localPadPz = q;}

    Double_t GetGlobalPadPx() const { return globalPadPx;}
    void SetGlobalPadPx(Double_t q) {globalPadPx = q;}

    Double_t GetGlobalPadPy() const { return globalPadPy;}
    void SetGlobalPadPy(Double_t q) {globalPadPy = q;}

    Double_t GetGlobalPadPz() const { return globalPadPz;}
    void SetGlobalPadPz(Double_t q) {globalPadPz = q;}

    Double_t GetGlobalPadX() const { return globalPadX;}
    void SetGlobalPadX(Double_t q) {globalPadX = q;}

    Double_t GetGlobalPadY() const { return globalPadY;}
    void SetGlobalPadY(Double_t q) {globalPadY = q;}

    Double_t GetGlobalPadZ() const { return globalPadZ;}
    void SetGlobalPadZ(Double_t q) {globalPadZ = q;}

    Int_t GetCrossedRich() const { return crossedRich;}
    void SetCrossedRich(Int_t q) {crossedRich = q;}

    Int_t GetIsLambda() const { return isLambda;}
    void SetIsLambda(Int_t q) {isLambda = q;}

    Double_t GetLambdaMass() const { return lambdaMass;}
    void SetLambdaMass(Double_t q) {lambdaMass = q;}

    Double_t GetAlphaLambda() const { return alphaLambda;}
    void SetAlphaLambda(Double_t q) {alphaLambda = q;}

    Double_t GetPtArmLambda() const { return ptArmLambda;}
    void SetPtArmLambda(Double_t q) {ptArmLambda = q;}

    Double_t GetGlobalXLambda() const { return globalXLambda;}
    void SetGlobalXLambda(Double_t q) {globalXLambda = q;}

    Double_t GetGlobalYLambda() const { return globalYLambda;}
    void SetGlobalYLambda(Double_t q) {globalYLambda = q;}

    Double_t GetGlobalZLambda() const { return globalZLambda;}
    void SetGlobalZLambda(Double_t q) {globalZLambda = q;}

    Double_t GetGlobalPxLambda() const { return globalPxLambda;}
    void SetGlobalPxLambda(Double_t q) {globalPxLambda = q;}

    Double_t GetGlobalPyLambda() const { return globalPyLambda;}
    void SetGlobalPyLambda(Double_t q) {globalPyLambda = q;}

    Double_t GetGlobalPzLambda() const { return globalPzLambda;}
    void SetGlobalPzLambda(Double_t q) {globalPzLambda = q;}

    Int_t GetOtherLambdaDaughter() const { return otherLambdaDaughter;}
    void SetOtherLambdaDaughter(Int_t q) {otherLambdaDaughter = q;}

    Int_t GetIsAntiLambda() const { return isAntiLambda;}
    void SetIsAntiLambda(Int_t q) {isAntiLambda = q;}

    Double_t GetAntiLambdaMass() const { return antiLambdaMass;}
    void SetAntiLambdaMass(Double_t q) {antiLambdaMass = q;}

    Double_t GetAlphaAntiLambda() const { return alphaAntiLambda;}
    void SetAlphaAntiLambda(Double_t q) {alphaAntiLambda = q;}

    Double_t GetPtArmAntiLambda() const { return ptArmAntiLambda;}
    void SetPtArmAntiLambda(Double_t q) {ptArmAntiLambda = q;}

    Double_t GetGlobalXAntiLambda() const { return globalXAntiLambda;}
    void SetGlobalXAntiLambda(Double_t q) {globalXAntiLambda = q;}

    Double_t GetGlobalYAntiLambda() const { return globalYAntiLambda;}
    void SetGlobalYAntiLambda(Double_t q) {globalYAntiLambda = q;}

    Double_t GetGlobalZAntiLambda() const { return globalZAntiLambda;}
    void SetGlobalZAntiLambda(Double_t q) {globalZAntiLambda = q;}

    Double_t GetGlobalPxAntiLambda() const { return globalPxAntiLambda;}
    void SetGlobalPxAntiLambda(Double_t q) {globalPxAntiLambda = q;}

    Double_t GetGlobalPyAntiLambda() const { return globalPyAntiLambda;}
    void SetGlobalPyAntiLambda(Double_t q) {globalPyAntiLambda = q;}

    Double_t GetGlobalPzAntiLambda() const { return globalPzAntiLambda;}
    void SetGlobalPzAntiLambda(Double_t q) {globalPzAntiLambda = q;}

    Int_t GetOtherAntiLambdaDaughter() const { return otherAntiLambdaDaughter;}
    void SetOtherAntiLambdaDaughter(Int_t q) {otherAntiLambdaDaughter = q;}

    Int_t GetIsK0Short() const { return isK0Short;}
    void SetIsK0Short(Int_t q) {isK0Short = q;}

    Double_t GetK0ShortMass() const { return k0ShortMass;}
    void SetK0ShortMass(Double_t q) {k0ShortMass = q;}

    Double_t GetAlphaK0Short() const { return alphaK0Short;}
    void SetAlphaK0Short(Double_t q) {alphaK0Short = q;}

    Double_t GetPtArmK0Short() const { return ptArmK0Short;}
    void SetPtArmK0Short(Double_t q) {ptArmK0Short = q;}

    Double_t GetGlobalXK0Short() const { return globalXK0Short;}
    void SetGlobalXK0Short(Double_t q) {globalXK0Short = q;}

    Double_t GetGlobalYK0Short() const { return globalYK0Short;}
    void SetGlobalYK0Short(Double_t q) {globalYK0Short = q;}

    Double_t GetGlobalZK0Short() const { return globalZK0Short;}
    void SetGlobalZK0Short(Double_t q) {globalZK0Short = q;}

    Double_t GetGlobalPxK0Short() const { return globalPxK0Short;}
    void SetGlobalPxK0Short(Double_t q) {globalPxK0Short = q;}

    Double_t GetGlobalPyK0Short() const { return globalPyK0Short;}
    void SetGlobalPyK0Short(Double_t q) {globalPyK0Short = q;}

    Double_t GetGlobalPzK0Short() const { return globalPzK0Short;}
    void SetGlobalPzK0Short(Double_t q) {globalPzK0Short = q;}

    Int_t GetOtherK0ShortDaughter() const { return otherK0ShortDaughter;}
    void SetOtherK0ShortDaughter(Int_t q) {otherK0ShortDaughter = q;}

    Double_t GetResidX() const { return residX;}
    void SetResidX(Double_t q) {residX = q;}

    Double_t GetResidY() const { return residY;}
    void SetResidY(Double_t q) {residY = q;}

    Double_t GetResid() const { return resid;}
    void SetResid(Double_t q) {resid = q;}

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

    ClassDef(StRichLambdaUstTrack,1)
};
#endif
