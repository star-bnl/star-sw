/***************************************************************************
 *
 * $Id: StFmsPointPair.cxx,v 2.4 2017/11/20 20:01:49 smirnovd Exp $
 *
 * Author: Akio Ogawa, Sep 2015
 ***************************************************************************
 *
 * Description: Implementation of StFmsPointPair
 *
 ***************************************************************************
 *
 * $Log: StFmsPointPair.cxx,v $
 * Revision 2.4  2017/11/20 20:01:49  smirnovd
 * Remove StRoot/ from included header prefix
 *
 * Revision 2.3  2017/02/20 16:32:58  ullrich
 * Changing F to D for StLorentzVector
 *
 * Revision 2.2  2015/10/21 14:52:54  ullrich
 * Added methods x() and y()
 *
 * Revision 2.1  2015/09/14 16:15:50  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#include "StFmsPointPair.h"
#include "StFmsPoint.h"
#include "St_base/StMessMgr.h"
#include "TMath.h"

static const char rcsid[] = "$Id: StFmsPointPair.cxx,v 2.4 2017/11/20 20:01:49 smirnovd Exp $";

StFmsPointPair::StFmsPointPair() : mFpsPid(0), mConeRadius{0.100, 0.070, 0.030}
{
    memset(mConeEnergy,0,sizeof(mConeEnergy));
    mFourMomentum.setPx(0.0);
    mFourMomentum.setPy(0.0);
    mFourMomentum.setPz(0.0);
    mFourMomentum.setE(0.0);
}

StFmsPointPair::StFmsPointPair(StFmsPoint* p) : StFmsPointPair() {
    addPoint(p);
}

StFmsPointPair::StFmsPointPair(StFmsPoint* p1, StFmsPoint* p2) : StFmsPointPair() {
    addPoint(p1);
    addPoint(p2);
}

StFmsPointPair::~StFmsPointPair() { /* no op */ }

void StFmsPointPair::addPoint(StFmsPoint* p)  {
    mPoints.push_back(p);
    mFourMomentum = mFourMomentum + StLorentzVectorD((double)p->fourMomentum().x(),
						     (double)p->fourMomentum().y(),
						     (double)p->fourMomentum().z(),
						     (double)p->fourMomentum().e());
    mFpsPid += (p->fpsPid()/10)*pow(10,nPoints()-1);
}

StFmsPoint* StFmsPointPair::point(int v) {
    if (v>=0 && v<nPoints()) return mPoints[v];
    return 0;
}

//hack need to expand for n>2
float StFmsPointPair::x() const {
    return
	(mPoints[0]->XYZ().x() * mPoints[0]->energy() +
	 mPoints[1]->XYZ().x() * mPoints[1]->energy() ) /
	(mPoints[0]->energy() + mPoints[1]->energy());
}

//hack need to expand for n>2
float StFmsPointPair::y() const {
    return 
	(mPoints[0]->XYZ().y() * mPoints[0]->energy() +
	 mPoints[1]->XYZ().y() * mPoints[1]->energy() ) /
	(mPoints[0]->energy() + mPoints[1]->energy());
}

float StFmsPointPair::dgg() const {
    if (nPoints()!=2) return -1.0;
    return (mPoints[0]->XYZ() - mPoints[1]->XYZ()).mag();
}

float StFmsPointPair::zgg() const {
    if (nPoints()!=2) return -1.0;
    float e1=mPoints[0]->energy();
    float e2=mPoints[1]->energy();
    return fabs(e1-e2)/(e1+e2);
}

float StFmsPointPair::coneRadius(int cone) const {
    if (cone>=0 && cone<kFmsPointMaxCone) return mConeRadius[cone];
    return 0.0;
}

float StFmsPointPair::coneEnergy(int cone) const {
    if (cone>=0 && cone<kFmsPointMaxCone) return mConeEnergy[cone];
    return 0.0;
}

float StFmsPointPair::coneEnergyFraction(int cone) const {
    if (cone>=0 && cone<kFmsPointMaxCone && mConeEnergy[cone]>0.0) return energy()/mConeEnergy[cone];
    return -1.0;
}

void StFmsPointPair::setConeEnergy(int cone, float energy) {
    if (cone>=0 && cone<kFmsPointMaxCone)
        mConeEnergy[cone]=energy;
}

void StFmsPointPair::print(int opt) {
    cout << Form("StFmsPointPair: N=%3d E=%6.1f pT=%6.2f eta=%6.2f Zgg=%6.3f Dgg=%7.3f M=%6.3f ",
                 nPoints(),energy(),pT(),eta(),zgg(),dgg(),mass());
    for(int cone=0; cone<kFmsPointMaxCone; cone++) {
        cout << Form("R%d=%6.3f ", int(coneRadius(cone)*1000.0+0.5),coneEnergyFraction(cone));
    }
    cout << Form("PID=%d PointId: ",fpsPid());
    for(int i=0; i<nPoints(); i++) {
        cout << Form("%3d ",mPoints[i]->id());
    }
    cout << endl;
}
