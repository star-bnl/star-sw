#include "StRichUstHit.h"

ClassImp(StRichUstHit)
StRichUstHit::StRichUstHit() {
    charge = -999;
    maxAdc = -999;
    nPads = -999;
    flag = -999;
    clusterNumber = -999;
    clusterFirstPad = -999;
    nClusterPads = -999;
    nClusterLocalMax = -999;
    clusterMinAmp = -999;
    rawX = -999;
    rawY = -999;
    localX = -999;
    localY = -999;
    globalX = -999;
    globalY = -999;
    globalZ = -999;
}
StRichUstHit::StRichUstHit(StRichUstHit& in) {
    charge = in.GetCharge();
    maxAdc = in.GetMaxAdc();
    nPads = in.GetNPads();
    flag = in.GetFlag();
    clusterNumber = in.GetClusterNumber();
    clusterFirstPad = in.GetClusterFirstPad();
    nClusterPads = in.GetNClusterPads();
    nClusterLocalMax = in.GetNClusterLocalMax();
    clusterMinAmp = in.GetClusterMinAmp();
    rawX = in.GetRawX();
    rawY = in.GetRawY();
    localX = in.GetLocalX();
    localY = in.GetLocalY();
    globalX = in.GetGlobalX();
    globalY = in.GetGlobalY();
    globalZ = in.GetGlobalZ();
}