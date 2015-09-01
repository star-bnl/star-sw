/***************************************************************************
 *
 * $Id: StFpsSlat.cxx,v 2.1 2015/09/01 18:26:45 ullrich Exp $
 *
 * Author: Akio Ogawa, Sep 2015
 ***************************************************************************
 *
 * Description: StFpsSlat is data for individual slat
 *
 ***************************************************************************
 *
 * $Log: StFpsSlat.cxx,v $
 * Revision 2.1  2015/09/01 18:26:45  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StFpsSlat.h"
#include "StFmsPoint.h"

ClassImp(StFpsSlat)

StFpsSlat::StFpsSlat() : mSlatId(-1), mMip(0.0)
{
    memset(mNPoint, 0, sizeof(mNPoint));
}

StFpsSlat::StFpsSlat(int slatid, float mip) : mSlatId(slatid), mMip(mip)
{
    memset(mNPoint,0,sizeof(mNPoint));
}

StFpsSlat::~StFpsSlat() {/* no op */}

StPtrVecFmsPoint& StFpsSlat::point(int type) {
    switch(type) {
        case 0:
            return mPoint1;
        case 1:
            return mPoint2;
        case 2:
            return mPoint3;
        case 3:
            return mPoint4;
        default:
            return mPoint1;
    }
}

const StPtrVecFmsPoint& StFpsSlat::point(int type) const {
    switch(type) {
        case 0:
            return mPoint1;
        case 1:
            return mPoint2;
        case 2:
            return mPoint3;
        case 3:
            return mPoint4;
        default:
            return mPoint1;
    }
}

void StFpsSlat::addPoint(StFmsPoint* point, Int_t type){
    switch(type){
        case 0:
            mPoint1.push_back(point); break;
        case 1:
            mPoint2.push_back(point); break;
        case 2:
            mPoint3.push_back(point); break;
        case 3:
            mPoint4.push_back(point); break;
        default:
            return;
    }
    mNPoint[type]++;
    mNPoint[kFpsNCandidate]++;
}

void StFpsSlat::print(int option) const {
    if(option==0 && mNPoint[0]==0 && mNPoint[1]==0) return;
    if(option==1 && mMip<0.5 && mNPoint[0]==0 && mNPoint[1]==0) return;
    int slat=mSlatId%21+1;
    int layer=(mSlatId/21)%3+1;
    int quad=(mSlatId/21/3)+1;
    cout << Form("StFpsSlat Id=%3d Q%1dL%1dS%02d Mip=%6.2f Npoint=%2d/%2d/%2d/%2d=%2d ",
                 mSlatId,quad,layer,slat,mMip,mNPoint[0],mNPoint[1],mNPoint[2],mNPoint[3],mNPoint[4]);
    if(mNPoint[0]>0) {
        cout << "1st=";
        for(unsigned int i=0; i<mNPoint[0]; i++){ cout << Form("%d(%4.1f) ",mPoint1[i]->id(),mPoint1[i]->fpsDistance(layer,0));}
    }
    if(mNPoint[1]>0){
        cout << "2nd=";
        for(unsigned int i=0; i<mNPoint[1]; i++){ cout << Form("%d(%4.1f) ",mPoint2[i]->id(),mPoint2[i]->fpsDistance(layer,1));}
    }
    if(mNPoint[2]>0){
        cout << "3rd=";
        for(unsigned int i=0; i<mNPoint[2]; i++){ cout << Form("%d(%4.1f) ",mPoint3[i]->id(),mPoint3[i]->fpsDistance(layer,2));}
    }
    if(mNPoint[3]>0){
        cout << "4th=";
        for(unsigned int i=0; i<mNPoint[3]; i++){ cout << Form("%d(%4.1f) ",mPoint4[i]->id(),mPoint4[i]->fpsDistance(layer,3));}
    }
    cout << endl;
}
