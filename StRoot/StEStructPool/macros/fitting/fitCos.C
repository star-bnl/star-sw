// example to illustrate how to fit excluding points in a given range
// Author: Rene Brun
#include "TH1.h"
#include "TF1.h"
Bool_t reject;

Double_t fitCos(Double_t *x, Double_t *par) {
    // Note:
    //  -0.08 is lower end of \eta_\Delta bin centered at 0 when there are 25 bins covering 4 units.
    //  -0.131 is close to lower end of \phi_\Delta bin centered at 0 when there are 24 bins covering 2pi
    //      (we usually duplicate a bin, having one centered at -pi/2 and the same one centered at 3pi/2.)
    //   3.272492 is close to upper end of bin centered at pi when there are 24 bins covering 2pi
    double etaD  = x[0];
    double phiD  = x[1];
    if ( reject &&
        ((etaD<-0.08) || (2.0 < etaD) || (phiD<-0.131) || (3.2724925<phiD)) ) {
        TF1::RejectPoint();
        return 0;
    }

    double pi = acos(-1);
    double gx = exp(-pow(etaD/par[4],2)/2);
    double gy = exp(-pow(phiD/par[5],2)/2);
    double gyp2pi = exp(-pow((phiD-2*pi)/par[5],2)/2);
    double gym2pi = exp(-pow((phiD+2*pi)/par[5],2)/2);
    double rad = sqrt( pow(etaD/par[7],2) + pow(phiD/par[8],2) );
    double rad2pi = sqrt( pow(etaD/par[7],2) + pow((phiD-2*pi)/par[8],2) );

    double func = par[0] +
            par[1] * cos(phiD) +
            par[2] * cos(2*phiD) +
            par[3] * gx * (gy + gyp2pi + gym2pi) +
            par[6] * (exp(-rad) + exp(-rad2pi)) +
            par[9]*cos(etaD) + par[10]*cos(2*etaD) + par[11]*cos(3*etaD) + par[12]*cos(4*etaD);
    return func;
}
