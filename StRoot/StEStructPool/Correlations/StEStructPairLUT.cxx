/**********************************************************************
 *
 * $Id: StEStructPairLUT.cxx,v 1.4 2010/09/02 21:24:08 prindle Exp $
 *
 * Author: Duncan Prindle
 *
 **********************************************************************
 *
 * Description:  If a pair of tracks gets too close it may be lost (due to splitting
 *               or merging). Separation distance depends on reference radius, radii
 *               of the two tracks, difference in phi, difference in eta and charge
 *               signs of the tracks. (Actually, there is an eta dependence, but it is small)
 *               Create Look Up Tables (LUT) by integrating over eta and reference
 *               radius. Use these tables to accept/reject pairs.
 *
 *
 ***********************************************************************/
#include "TMath.h"
#include "StEStructPairLUT.h"


ClassImp(StEStructPairLUT)

StEStructPairLUT::StEStructPairLUT() {
    mR1   = 125.0;
    mEta1 = 0.0;
    mPhi1 = 0.0;
    mSign1 = 1;
    mR2   = 1250.0;
    mEta2 = 0.0;
    mPhi2 = 0.0;
    mSign2 = 1;
    mDelEta = 0.25;
    mDelPhi = 2.5;
    nDelEta = 25;
    nDelPhi = 50;
    mDelXYCut = 5;
    mDelZCut  = 5;
    mPi = TMath::ACos(-1);
    mRRefMin = 50;
    mRRefMax = 200;

    for (int i=0;i<4;i++) {
        mDists[i] = 0;
    }
    int iR = 0;
    for (int ir2=0;ir2<10;ir2++) {
        for (int ir1=0;ir1<=ir2;ir1++) {
            mCutLS[iR] = 0;
            mCutUS[iR] = 0;
            iR++;
        }
    }

}
StEStructPairLUT::~StEStructPairLUT() {
    for (int i=0;i<4;i++) {
        delete mDists[i];
    }
    int iR = 0;
    for (int ir2=0;ir2<10;ir2++) {
        for (int ir1=0;ir1<=ir2;ir1++) {
            delete mCutLS[iR];
            delete mCutUS[iR];
            iR++;
        }
    }
}
double StEStructPairLUT::s (double Ref, double Rad, double eta) {
    double l = lambda(eta);
    return Rad*TMath::ACos(1-0.5*TMath::Power(Ref/Rad,2))/TMath::Cos(l);
}
double StEStructPairLUT::alpha (double Ref, double Rad, double eta, double sign) {
    double l = lambda(eta);
    double arc = s(Ref,Rad,eta);
    return sign*arc*TMath::Cos(l)/Rad;
}
double StEStructPairLUT::lambda (double eta) {
    double theta = 2*atan(TMath::Exp(-eta));
    return 0.5*3.1415926 - theta;
}
double StEStructPairLUT::delZ  (double Ref) {
    double l1 = lambda(mEta1);
    double l2 = lambda(mEta2);
    double s1 = s(Ref,mR1,mEta1);
    double s2 = s(Ref,mR2,mEta2);
    return s1*TMath::Sin(l1) - s2*TMath::Sin(l2);
}
double StEStructPairLUT::delXY (double Ref) {
    // Compared to tracks.i of Dsv, phi is direction of track (pi/2 different than in tracks.i)
    // For s small and phi = 0 x is linear in s and y decreases quadratically (i.e. is negative).
    // (v x B points in -Y direction if v is in X and B is in Z directions)
    double alpha1 = alpha(Ref,mR1,mEta1,mSign1);
    double alpha2 = alpha(Ref,mR2,mEta2,mSign2);
    double x1 = +mSign1*mR1*(TMath::Sin(mPhi1)-TMath::Sin(mPhi1-alpha1));
    double y1 = -mSign1*mR1*(TMath::Cos(mPhi1)-TMath::Cos(mPhi1-alpha1));
    double x2 = +mSign2*mR2*(TMath::Sin(mPhi2)-TMath::Sin(mPhi2-alpha2));
    double y2 = -mSign2*mR2*(TMath::Cos(mPhi2)-TMath::Cos(mPhi2-alpha2));
    double d = TMath::Sqrt(TMath::Power(x1-x2,2)+TMath::Power(y1-y2,2));
    mDPhi_Ref = x1*y2-x2*y1;

    alpha1 = alpha(50,mR1,mEta1,mSign1);
    alpha2 = alpha(50,mR2,mEta2,mSign2);
    mX1_50 = +mSign1*mR1*(TMath::Sin(mPhi1)-TMath::Sin(mPhi1-alpha1));
    mY1_50 = -mSign1*mR1*(TMath::Cos(mPhi1)-TMath::Cos(mPhi1-alpha1));
    mX2_50 = +mSign2*mR2*(TMath::Sin(mPhi2)-TMath::Sin(mPhi2-alpha2));
    mY2_50 = -mSign2*mR2*(TMath::Cos(mPhi2)-TMath::Cos(mPhi2-alpha2));
    mDPhi_50 = mX1_50*mY2_50-mX2_50*mY1_50;
    if (mDPhi_50*mDPhi_Ref < 0) {
        return -d;
    } else {
        return d;
    }
}
void StEStructPairLUT::initHists() {
    for (int i=0;i<4;i++) {
        if (mDists[i]) {
            delete mDists[i];
        }
    }
    char bufferPhi[1024];
    char bufferEta[1024];
    sprintf(bufferPhi,"#phi_{1} (#phi_{2}=%f)",mPhi2);
    sprintf(bufferEta,"#eta_{1} (#eta_{2}=%f)",mEta2);
    mDists[0] = new TH2D("dists0","#delta#phi vs. Reference radius",nDelPhi,mPhi2-mDelPhi,mPhi2+mDelPhi, 100,50.0,200.0);
    mDists[0]->GetXaxis()->SetTitle(bufferPhi);
    mDists[0]->GetYaxis()->SetTitle("R_{ref}");
    mDists[0]->SetMaximum(+5);
    mDists[0]->SetMinimum(-5);
    mDists[1] = new TH2D("dists1","#delta#eta  vs. Reference radius",nDelEta,mEta2-mDelEta,mEta2+mDelEta, 100,50.0,200.0);
    mDists[1]->GetXaxis()->SetTitle(bufferEta);
    mDists[1]->GetYaxis()->SetTitle("R_{ref}");
    mDists[1]->SetMaximum(+5);
    mDists[1]->SetMinimum(-5);
    mDists[2] = new TH2D("dists2","#delta#phi vs. #delta#eta: |#delta_{XY}| < 5cm and |#delta_{Z}| < 5cm",nDelPhi,-mDelPhi,+mDelPhi, nDelEta,-mDelEta,+mDelEta);
    mDists[2]->GetXaxis()->SetTitle("#phi_{1}-#phi_{2}");
    mDists[2]->GetYaxis()->SetTitle("#eta_{1}-#eta_{2}");
    mDists[3] = new TH2D("dists3","#delta#phi vs. #delta#eta: |#delta_{Z}| < 5cm and tracks cross in #phi between 50cm and R_{ref}",nDelPhi,-mDelPhi,+mDelPhi, nDelEta,-mDelEta,+mDelEta);
    mDists[3]->GetXaxis()->SetTitle("#phi_{1}-#phi_{2}");
    mDists[3]->GetYaxis()->SetTitle("#eta_{1}-#eta_{2}");

    int iR = 0;
    char buf1[1024], buf2[1024];
    for (int ir2=0;ir2<10;ir2++) {
        for (int ir1=0;ir1<=ir2;ir1++) {
            sprintf(buf1,"LS_%i_%i",ir1,ir2);
            sprintf(buf2,"cuts for LS, rad1 bin %i, rad2 bin %i",ir1,ir2);
            if (mCutLS[iR]) delete mCutLS[iR];
            // 2 extra bins in \delta\phi to account for high p_t tracks with wrong opening angle that stay close.
            // extra bin in \delta\eta is the bin a 0 (not really extra).
            mCutLS[iR] = new TH2F(buf1,buf2, 2+nDelPhi,-2*mDelPhi/nDelPhi,mDelPhi, 1+2*nDelEta,-mDelEta,+mDelEta);
            sprintf(buf1,"US_%i_%i",ir1,ir2);
            sprintf(buf2,"cuts for US, rad1 bin %i, rad2 bin %i",ir1,ir2);
            if (mCutUS[iR]) delete mCutUS[iR];
            mCutUS[iR] = new TH2F(buf1,buf2, 2+nDelPhi,-2*mDelPhi/nDelPhi,mDelPhi, 1+2*nDelEta,-mDelEta,+mDelEta);
            iR++;
        }
    }
}
void StEStructPairLUT::fillDists() {
    for (int i=0;i<4;i++) {
        mDists[i]->Reset();
    }

    double dZ, dXY;
    int ix = 1;
    double saveEta = mEta1;
    for (mEta1=mEta2-mDelEta;mEta1<mEta2+mDelEta;mEta1+=2*mDelEta/nDelEta) {
        int iy = 1;
        for (double rRef=50;rRef<200;rRef+=150/100.0) {
            dZ  = delZ(rRef);
            mDists[1]->SetBinContent(ix,iy,dZ);
            iy++;
        }
        ix++;
    }
    mEta1 = saveEta;
    ix = 1;
    double savePhi = mPhi1;
    for (mPhi1=mPhi2-mDelPhi;mPhi1<mPhi2+mDelPhi;mPhi1+=2*mDelPhi/nDelPhi) {
        int iy = 1;
        for (double rRef=50;rRef<200;rRef+=150/100.0) {
            dXY = delXY(rRef);
            mDists[0]->SetBinContent(ix,iy,dXY);
            iy++;
        }
        ix++;
    }
    mPhi1 = savePhi;

    // For each phi1 bin scan over R_ref (from 65cm to 200cm) for dXY.
    // If |dXY| < 5cm scan over eta1 for this R_ref. For each bin with |dZ| < 5cm increment mDists[2](phi1,eta1)
    // If dXY < 0 scan over eta1 for this R_ref. For each bin with |dZ| < 5cm increment mDists[3](phi1,eta1)
    ix = 1;
    for (mPhi1=mPhi2-mDelPhi;mPhi1<mPhi2+mDelPhi;mPhi1+=2*mDelPhi/nDelPhi) {
        int iy = 11;
        for (double rRef=65;rRef<200;rRef+=150/100.0) {
            dXY = mDists[0]->GetBinContent(ix,iy);
            if (TMath::Abs(dXY) < 5) {
                int iz = 1;
                for (mEta1=mEta2-mDelEta;mEta1<mEta2+mDelEta;mEta1+=2*mDelEta/nDelEta) {
                    dZ  = mDists[1]->GetBinContent(iz,iy);
                    if (TMath::Abs(dZ) < 5) {
                        mDists[2]->Fill(mPhi1-mPhi2,mEta1-mEta2,1.0);
                    }
                    iz++;
                }
                mEta1 = saveEta;
            }
            if (dXY < 0) {
                int iz = 1;
                for (mEta1=mEta2-mDelEta;mEta1<mEta2+mDelEta;mEta1+=2*mDelEta/nDelEta) {
                    dZ  = mDists[1]->GetBinContent(iz,iy);
                    if (TMath::Abs(dZ) < 5) {
                        mDists[3]->Fill(mPhi1-mPhi2,mEta1-mEta2,1);
                    }
                    iz++;
                }
                mEta1 = saveEta;
            }
            iy++;
        }
        ix++;
    }
    mPhi1 = savePhi;
}

void StEStructPairLUT::integrateEta(TH2F *h) {
    h->Reset();

    // Can only depend on \delta\phi. Choose \phi_1 = 0, scan over \phi_2.
    // For each \phi_2 bin scan over R_ref for dXY.
    // If |dXY| < mDelXYCut and TMath::Abs(dZ) < mDelZCut we think of this as a merged pair.
    // If dXY   < 0 and TMath::Abs(dZ) < mDelZCut we think of this a a crosTMath::Sing pair.
    // Approximately only depends on \delta\eta.
    // Scan over \eta_2 and for each value scan over \eta_1 within some range.
    // Need to scan over \eta_1 for R_ref where dXY < mDelXYCut to see if TMath::Abs(dZ) is ever < mDelZCut.
    // Note: 

    double savePhi1 = mPhi1;
    double savePhi2 = mPhi2;
    mPhi2 = 0;
    int ix = 1;
    for (mPhi2=-1.5*mDelPhi/nDelPhi;mPhi2<mDelPhi;mPhi2+=mDelPhi/nDelPhi) {
        int iy = 1;
        for (double rRef=mRRefMin;rRef<mRRefMax;rRef+=150/100.0) {
            double dXY = delXY(rRef);
            if (dXY < mDelXYCut) {
                double saveEta1 = mEta1;
                double saveEta2 = mEta2;
                int iz2 = 1;
                for (mEta2=0;mEta2<1.0;mEta2+=0.1) {
                    int iz1 = 1;
                    for (mEta1=mEta2-mDelEta;mEta1<mEta2+mDelEta;mEta1+=mDelEta/nDelEta) {
                        if (TMath::Abs(mEta1) <= 1) {
                            double dZ  = delZ(rRef);
                            if (TMath::Abs(dZ) < mDelZCut) {
                                h->Fill(mPhi2,mEta1-mEta2,1.0);
                            }
                        }
                        iz1++;
                    }
                    iz2++;
                }
                mEta1 = saveEta1;
                mEta2 = saveEta2;
            }
            iy++;
        }
        ix++;
    }
    mPhi1 = savePhi1;
    mPhi2 = savePhi2;
}

void StEStructPairLUT::fillLUTs() {

    // Ten equal steps in 1/R. Maximum value of 1/R is 1/75, close to 150MeV/c.
    for (int ir=9;ir>=0;ir--) {
        mRad[ir] = 750.0/(ir+0.5);
    }

    int iR = 0;
    // Choose mR1 >= mR2 => for LS need phi2 > phi1. (and can choose phi1 = 0)
    // For US case choose mSign2 < 0 => phi2 > phi1  (and can choose phi1 = 0)
    for (int ir2=0;ir2<10;ir2++) {
        for (int ir1=0;ir1<=ir2;ir1++) {
            mSign1 = +1;
            mR1    = mRad[ir1];
            mSign2 = +1;
            mR2    = mRad[ir2];
            integrateEta(mCutLS[iR]);
            mSign1 = -1;
            mR1    = mRad[ir1];
            mSign2 = +1;
            mR2    = mRad[ir2];
            integrateEta(mCutUS[iR]);
            iR++;
        }
    }
}
int StEStructPairLUT::cut(double curvature1, double curvature2, double delPhi, double delEta) {
    // For high enough pt we can have phi1-phi2 with wrong sign  but tracks still merged.
    // We are trying to offset by 2 bins in delPhi to catch this.

    // Probably called with phi1-phi2 as argument. They might be on opposite sides of cut.
    while (delPhi > +2*mPi) delPhi -= 2*mPi;
    while (delPhi < -2*mPi) delPhi += 2*mPi;

    // Calculate indices into Look Up Table.
    if (TMath::Abs(delEta) > mDelEta) return 0;
    if (TMath::Abs(delPhi) > mDelPhi) return 0;
    int idEta = int( delEta / (mDelEta/nDelEta) );
    int idPhi = int( delPhi / (mDelPhi/nDelPhi) );
    int ir1 = int(750*TMath::Abs(curvature1) -0.5);
    if (ir1  > 9) ir1 = 9;
    int ir2 = int(750*TMath::Abs(curvature2) -0.5);
    if (ir2  > 9) ir2 = 9;

    // LUT has R1 >= R2.
    // Calculate iR  and adjust idEta and idPhi appropriately
    int iR, is1, is2;
    if (TMath::Abs(curvature1) < TMath::Abs(curvature2)) {
        iR = ir2*(ir2+1)/2 + ir1;
        idEta = nDelEta + idEta;
        is1 = (curvature1 < 0) ? -1 : 1;
        is2 = (curvature2 < 0) ? -1 : 1;
    } else {
        iR = ir1*(ir1+1)/2 + ir2;
        idEta = nDelEta - idEta;
        idPhi = -idPhi;
        is1 = (curvature2 < 0) ? -1 : 1;
        is2 = (curvature1 < 0) ? -1 : 1;
    }

    if (is1 > 0) {
        if (is2 > 0) {
            // Need delPhi = phi_1 - phi_2 < 0
            // Rely on GetBinContent returning 0 for out of bound index.
            // Remember, root starts histograms with bin=1, _not_ 0.
            return int(mCutLS[iR]->GetBinContent(-idPhi+3,idEta+1));
        } else {
            return int(mCutUS[iR]->GetBinContent(idPhi+3,idEta+1));
        }
    } else {
        if (is2 > 0) {
            // Need delPhi = phi_1 - phi_2 < 0
            return int(mCutUS[iR]->GetBinContent(-idPhi+3,idEta+1));
        } else {
            return int(mCutLS[iR]->GetBinContent(idPhi+3,idEta+1));
        }
    }
}
