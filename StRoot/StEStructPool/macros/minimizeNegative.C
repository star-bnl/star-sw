#include "../Support/StEStructMinimizeNegative.h"
StEStructMinimizeNegative minData;
static void minimizeNegative(int &npar, double *gin, double &f, double *par, int iflag) {
    // We rely on following static variables to be set before minimization can be done.
    // StEStructSupport *minData.mSupport;
    // int               minData.mChargeType;     0 for LS, 1 for US.
    // int               minData.mCorrType;       0 for SYtDYt, 1 for YtYt;
    // double            minData.mLambda;         Lagrange multiplier to weight negative \Delta\rho. (normally 10)
    float sf[2];
    sf[0] = par[0];
    sf[1] = par[0];
    const char* spaceName[]={"SYtDYt","YtYt"};
    TH2D **localHists = minData.mSupport->buildChargeTypes(spaceName[minData.mCorrType],5,sf);
    if (!localHists) {
        f = 2147483648;
        return;
    }

    double posVal=0.;
    double negVal=0.;
    double numNeg=0.;
    double numPos=0.;

    if (0 == minData.mCorrType) { // SYtDYt space

        for(int ix=1;ix<=localHists[minData.mChargeType]->GetNbinsX();ix++){   // ix=13, sum_yt~3.4, yt~1.7
            for(int iy=1;iy<=localHists[minData.mChargeType]->GetNbinsY();iy++){ // iy=13, delta_yt=0
/*
 *  Code checked to see if number of counts in \rho is >= 1.
 *  I don't know why checking for error in \Delta\rho/sqrt(\rho_{ref}) being
 *  non-zero isn't better. (Allows possibility that \rho is 0 while \rho_{ref}
 *  is non-zero, but why not include those bins?)
                if (data->GetBinError(ix,iy)<=0) {
                    continue;
                }
 */
                double testVal = (double) localHists[minData.mChargeType]->GetBinContent(ix,iy);
                double testErr = fabs((double) localHists[minData.mChargeType]->GetBinError(ix,iy));
                if (testErr==0.) {
                    continue;
                }
                double val = pow(testVal/testErr,2);
                if (testVal<0.) {
                    negVal += val;
                    numNeg +=1.0;
                } else {
                    posVal += val;
                    numPos +=1.0;
                }
            }
        }

    } else if (1 ==minData.mCorrType) {  //YtYt Space 
 
        for(int ix=1;ix<=localHists[minData.mChargeType]->GetNbinsX();ix++){   // ix=5, yt~1.7
            for(int iy=ix+1;iy<=localHists[minData.mChargeType]->GetNbinsY();iy++){ // iy>ix
/*
 * See comment above.
                if (data->GetBinContent(ix,iy)<1) {
                    continue;
                }
 */
                double testVal = (double) localHists[minData.mChargeType]->GetBinContent(ix,iy);
                double testErr = fabs((double) localHists[minData.mChargeType]->GetBinError(ix,iy));
                if (testErr==0.) {
                    continue;
                }
                double val = pow(testVal/testErr,2);
                if (testVal<0.) {
                    negVal += val;
                    numNeg += 1.0;
                } else {
                    posVal += val;
                    numPos += 1.0;
                }
            }
        }
    }

    for (int iType=0;iType<4;iType++) {
        delete localHists[iType];
    }
    delete [] localHists;

    if (numPos>0.) {
        posVal = (posVal/numPos);
    }
    if (numNeg>0.) {
        negVal = (1.+minData.mLambda)*(negVal/numNeg);
    }

    f = posVal+negVal;
}
