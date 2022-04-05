/**********************************************************************
 *
 * $Id: StEStructPairLUT.h,v 1.2 2010/09/02 21:24:08 prindle Exp $
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

#ifndef _StEStructPairLUT
#define _StEStructPairLUT

#include "TH2F.h"
#include "TH2D.h"

class StEStructPairLUT {
    public:
        TH2D  *mDists[4];
        TH2F  *mCutLS[55], *mCutUS[55];
        double mR1, mEta1, mPhi1, mSign1;
        double mR2, mEta2, mPhi2, mSign2;
        double mDelEta, mDelPhi;
        int    nDelEta, nDelPhi;
        double mX1_50, mY1_50, mX2_50, mY2_50, mDPhi_50, mDPhi_Ref;
        double mDelXYCut, mDelZCut;
        double mRad[10];
        double mPi;
        double mRRefMin, mRRefMax;
 
        StEStructPairLUT();
        virtual ~StEStructPairLUT();
        double s (double Ref, double Rad, double eta);
        double alpha (double Ref, double Rad, double eta, double sign);
        double lambda (double eta);
        double delZ  (double Ref);
        double delXY (double Ref);
        void initHists();
        void fillDists();
        void integrateEta(TH2F *h);
        void fillLUTs();
        // Note that curvature is signed.
        int  cut(double curvature1, double curvature2, double delPhi, double delEta);

  ClassDef(StEStructPairLUT,1)
};

#endif
