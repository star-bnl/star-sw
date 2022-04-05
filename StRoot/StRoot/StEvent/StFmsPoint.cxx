/***************************************************************************
 *
 * $Id: StFmsPoint.cxx,v 2.8 2017/11/20 20:01:49 smirnovd Exp $
 *
 * Author: Thomas Burton, Yuxi Pan, 2014
 ***************************************************************************
 *
 * Description: Implementation of StFmsPoint, the StEvent FMS
 *              photon structure
 *
 ***************************************************************************
 *
 * $Log: StFmsPoint.cxx,v $
 * Revision 2.8  2017/11/20 20:01:49  smirnovd
 * Remove StRoot/ from included header prefix
 *
 * Revision 2.7  2016/06/07 15:51:34  akio
 * Making code better based on Coverity reports
 *
 * Revision 2.6  2015/09/14 16:59:22  ullrich
 * Added comments and modified print out.
 *
 * Revision 2.5  2015/09/01 21:01:47  ullrich
 * Minor changes to format of print statments and \nchange to naming of data member.
 *
 * Revision 2.4  2015/09/01 18:29:01  ullrich
 * Changes due to adding StFpsSlat and interconnection between slats and points.
 *
 * Revision 2.3  2015/08/26 16:51:25  ullrich
 * Fixed bug in cluster() and added print out fct and operator.
 *
 * Revision 2.2  2015/08/19 19:22:34  ullrich
 * Major update (PID) by Akio.
 *
 *
 ***************************************************************************/
#include "StFmsPoint.h"
#include "St_base/StMessMgr.h"
#include "TMath.h"

static const char rcsid[] = "$Id: StFmsPoint.cxx,v 2.8 2017/11/20 20:01:49 smirnovd Exp $";

StFmsPoint::StFmsPoint()
: mDetectorId(0), mEnergy(-1.0), mX(-99.0), mY(-99.0),
mId(-1), mParentClusterId(-1), mNParentClusterPhotons(-1), mCluster(0)
{
    resetFps();
}

StFmsPoint::~StFmsPoint() { /* no op */ }

int   StFmsPoint::fpsNCandidate(int layer) {
    if (layer>=1 && layer<=kFpsNLayer) {
        return mFpsNCandidate[layer-1];
    }
    return 0;
}

float StFmsPoint::fpsMip(int layer, int candidate) {
    if(layer<1 || layer>kFpsNLayer) return -1;
    if(candidate>=0 && candidate<kFpsNCandidate) return mFpsMip[layer-1][candidate];  //return mip for a candidate
    if(candidate>=kFpsNCandidate && candidate<=kFpsNCandidate+2){    // candidate=kFpsNCandidate   return 1+2
        float sum=0.0;                                               // candidate=kFpsNCandidate+1 return 1+2+3
        for(int i=0; i<candidate-kFpsNCandidate+2; i++){             // candidate=kFpsNCandidate+2 return 1+2+3+4
            if(i==0 && mFpsMip[layer-1][i]==-9.0) return -9.0;       // if closest one has bad status, return bad.
            if(mFpsMip[layer-1][i]>0.0) sum+=mFpsMip[layer-1][i];
        }
        return sum;
    }
    return -1;
}

int StFmsPoint::fpsSlatId(int layer, int candidate) {
    if (layer>=1 && layer<=kFpsNLayer && candidate>=0 && candidate<mFpsNCandidate[layer-1]){
        return mFpsSlatId[layer-1][candidate];
    }
    return -1;
}

float StFmsPoint::fpsDistance(int layer, int candidate) {
    if (layer>=1 && layer<=kFpsNLayer && candidate>=0 && candidate<mFpsNCandidate[layer-1]){
        return mFpsDistance[layer-1][candidate];
    }
    return 999.0;
}

void StFmsPoint::setFps(int layer, float mip, int slatid, float d) {
    if (layer>=1 && layer<=kFpsNLayer){
        int n=mFpsNCandidate[layer-1];
        if (n>=kFpsNCandidate) {
            LOG_WARN << Form("StFmsPoint::setFps() too many FPS slats associcated with a point in layer=%d (slatid=%d distance=%6.2f mip=%6.2f) n=%d/%d"
                             ,layer,slatid,d,mip,n,kFpsNCandidate) <<endm;
            return;
        }
        mFpsMip[layer-1][n] = mip;
        mFpsSlatId[layer-1][n] = slatid;
        mFpsDistance[layer-1][n] = d;
        mFpsNCandidate[layer-1]++;
    }
    orderFpsCandidates(layer);
}

void StFmsPoint::resetFps() {
    mFpsPid = 0;
    for(int l=0; l<kFpsNLayer; l++){
        mFpsNCandidate[l]=0;
        for(int c=0; c<kFpsNCandidate; c++){
            mFpsMip[l][c] = -2.0;
            mFpsSlatId[l][c] = -1;
            mFpsDistance[l][c] = 999.0;
        }
    }
}

void StFmsPoint::orderFpsCandidates(int layer) {  //order candidates by distance
    int l1=0, l2=kFpsNLayer;
    if(layer>0) {l1=layer-1; l2=layer;}
    for(int l=l1; l<l2; l++){
        int n=mFpsNCandidate[l];
        if(n<2) continue;
        int index[kFpsNCandidate];
        TMath::Sort(n,mFpsDistance[l],index,false); //flase=increasing order
        for(int i=0; i<n-1; i++) {   //swap contents based on index
            int j=index[i];
            if(j!=i){
                float mip = mFpsMip[l][i];
                int slatid = mFpsSlatId[l][i];
                float d = mFpsDistance[l][i];
                mFpsMip[l][i] = mFpsMip[l][j];
                mFpsSlatId[l][i] = mFpsSlatId[l][j];
                mFpsDistance[l][i] = mFpsDistance[l][j];
                mFpsMip[l][j] = mip;
                mFpsSlatId[l][j] = slatid;
                mFpsDistance[l][j] = d;
                for(int k=i+i; k<n; k++){  // swap index as well
                    if(index[k]==i) {
                        index[k]=j;
                        index[i]=i;
                        break;
                    }
                }
            }
        }
    }
}

void StFmsPoint::print(int opt) {
    cout << Form("StFmsPoint: Id=%4d Det=%2d ParentId=%3d loc=%6.1f %6.1f xyz=%6.1f %6.1f %6.1f E=%7.2f ET=%6.2f FPS=",
                 id(), detectorId(), parentClusterId(),
                 x(), y(), XYZ().x(), XYZ().y(), XYZ().z(), energy(), fourMomentum().perp());
    for(int i=1; i<=kFpsNLayer; i++) {
        //int mip=fpsMip(i,kFpsNCandidate);
        float mip=fpsMip(i,0);
        if(mip<0.0)      {cout << "?";}
        else if(mip>9.0) {cout << "9";}
        else             {cout << Form("%1d",int(mip+0.5));}
    }
    cout << Form(" PID=%2d(%s) ",fpsPid(),pidName(fpsPid()));
    for(int l=1; l<=kFpsNLayer; l++){
        for(int j=0; j<fpsNCandidate(l); j++){
            int slatid=fpsSlatId(l,j);
            int mip=int(fpsMip(l,j)+0.5);
            int slat=slatid%21+1;
            int layer=(slatid/21)%3+1;
            int quad=(slatid/21/3)+1;
            cout << Form(" Q%1dL%1dS%02d=%2d ",quad,layer,slat,mip);
        }
    }
    cout << endl;
}
