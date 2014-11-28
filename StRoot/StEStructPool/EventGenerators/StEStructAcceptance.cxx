#include "StEStructAcceptance.h"

#include "StEStructPool/AnalysisMaker/StEStructEventCuts.h"
#include "StEStructPool/AnalysisMaker/StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"

StEStructAcceptance::StEStructAcceptance() {
    mSigZVertex = 60;
    mMaxZVertex = 30;
    mMinDetectableRadius = 160;
    mgRand2Good   = false;
}
StEStructAcceptance::StEStructAcceptance( double sigZVertex,
                                          double maxZVertex,
                                          double minDetectableRadius ) {
    mSigZVertex = sigZVertex;
    mMaxZVertex = maxZVertex;
    mMinDetectableRadius = minDetectableRadius;
    mgRand2Good   = false;
};

//-------------------------------------------------------------------------
void StEStructAcceptance::SetSeed(int iseed) {
    cout << " Calling srand48(" << iseed << ") (from StEStructAcceptance class)" << endl;
    srand48(iseed);
}


//--------------------------------------------------------------------------
double StEStructAcceptance::GetNewZVertex() {
    double z = 2*mMaxZVertex;
    while (abs(z) > mMaxZVertex) {
        z = mSigZVertex * gRand48();
    }
    return z;
}   
bool StEStructAcceptance::isTrackInAcceptance( double VertZ, double pt, double eta ) {
    double r = maxRadius( eta, pt, VertZ );
    if (r < mMinDetectableRadius) {
        return false;
    }
    return true;
}

//-------------------------------------------------------------
// This method calculates maximum distance of track from beam axis.
// This point might be at the endplane or at twice the radius of curvature.
double StEStructAcceptance::maxRadius(double eta, double pt, double VertZ) {
    double pi = 3.14159265359;
    double lambda = pi/2 - 2*atan(exp(-eta));
    double s;
    if (lambda > 0) {
        s = (+200 - VertZ) / sin(lambda);
    } else {
        s = (-200 - VertZ) / sin(lambda);
    }
    double r = pt / 0.0015;
    double phi = s * cos(lambda) / r;
    if (phi < pi) {
        return r * sqrt(2 - 2*cos(phi));
    }
    return 2*r;
}

double StEStructAcceptance::gRand48() {
    double x1, x2, w;
 
    if (mgRand2Good) {
        mgRand2Good = false;
        return mgRand2;
    }
    do {
        x1 = 2.0 * drand48() - 1.0;
        x2 = 2.0 * drand48() - 1.0;
        w = x1 * x1 + x2 * x2;
    } while ( w >= 1.0 );

    w = sqrt( (-2.0 * log( w ) ) / w );
    mgRand2 = x2 * w;
    mgRand2Good = true;
    return x1*w;
}

