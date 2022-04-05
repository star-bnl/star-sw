/**********************************************************************
 *
 * $Id: Pileup.cxx,v 1.1 2008/12/02 23:47:43 prindle Exp $
 *
 * Author: Duncan Prindle
 *
 **********************************************************************
 *
 * Description:  Using first/last points on tracks characterize event vertex structure.
 *
 **********************************************************************/

#include "TAxis.h"
#include "Pileup.h"

ClassImp(Pileup)

//-------------------------------------------------------
Pileup::Pileup() {

    mHPileup[0] = new TH1D("zFirst +","zFirst +",200,1,200);
    mHPileup[1] = new TH1D("zFirst -","zFirst -",200,-200,-1);
    mHPileup[2] = new TH1D("zLast +","zLast +",200,1,200);
    mHPileup[3] = new TH1D("zLast -","zLast -",200,-200,-1);
    mHPileup[4] = new TH1D("V_z +","V_z +",500,-250,250);
    mHPileup[5] = new TH1D("V_z -","V_z -",500,-250,250);

    mHQA2D[0] = new TH2D("zFirst vs. zLast","zFirst vs. zLast",100,-200,200, 100,-200,200);
    mHQA2D[1] = new TH2D("zFirst vs. zLast scaled","zFirst vs. zLast scaled",100,-200,200, 100,-250,250);
    mHQA2D[2] = new TH2D("V_z vs. zLast","V_z vs. zLast",100,-200,200, 100,-250,250);

    mHQA2D[0]->SetMarkerStyle(20);
    mHQA2D[0]->SetMarkerSize(0.5);
    TAxis *x = mHQA2D[0]->GetXaxis();
    TAxis *y = mHQA2D[0]->GetYaxis();
    x->SetTitle("Z_{first}");
    y->SetTitle("Z_{last}");
    mHQA2D[1]->SetMarkerStyle(20);
    mHQA2D[1]->SetMarkerSize(0.5);
    x = mHQA2D[1]->GetXaxis();
    y = mHQA2D[1]->GetYaxis();
    x->SetTitle("Z_{first} at 65cm");
    y->SetTitle("Z_{last} at 190cm");
    mHQA2D[2]->SetMarkerStyle(20);
    mHQA2D[2]->SetMarkerSize(0.5);
    x = mHQA2D[2]->GetXaxis();
    y = mHQA2D[2]->GetYaxis();
    x->SetTitle("V_z");
    y->SetTitle("Z_{last} at 190cm");

    mHQA1D[0] = new TH1D("QA zLast -","zLast -",400,-200,200);
    mHQA1D[1] = new TH1D("QA V_z +","V_z +",500,-250,250);
    mHQA1D[2] = new TH1D("QA V_z -","V_z -",500,-250,250);

    mDead[0] = 5;
    mDead[1] = 5;
    mDead[2] = 5;
    mDead[3] = 5;
    mDead[4] = 0;
    mDead[5] = 0;
    mDead[6] = 5;
    mZEndplate  = 212.75;
    // Reasonably tight cuts on what we call matched pileup, vertex.
    mZStartStopMatchMin = -10;
    mZStartStopMatchMax = +10;
    mZPeakMatchMin      =  -5;
    mZPeakMatchMax      =  +3;
    // Expand the pileup-good vertex range a bit (by 5cm on each side)
    // Idea is that if a pileup is within 10cm of fitted vertex it may contribute
    // tracks to the fitted or affect it in some other way.
    //mZPileMatchMin      = -12;
    //mZPileMatchMax      =  +2;
    mZPileMatchMin      = -17;
    mZPileMatchMax      =  +7;
    mVerbose = 0;
}

//-------------------------------------------------------
Pileup::~Pileup(){
cout << "Deleting a pileup object. " << endl;
    for (int ih=0;ih<6;ih++) {
        delete mHPileup[ih];
    }
    for (int ih=0;ih<3;ih++) {
        delete mHQA1D[ih];
        delete mHQA2D[ih];
    }
};  

//-------------------------------------------------------
int Pileup::verbose() {
    return mVerbose;
}
void Pileup::verbose(int val) {
    // If non zero we print some possibly relevant information.
    mVerbose = val;
}

//-------------------------------------------------------
double Pileup::zEndplate() {
    return mZEndplate;
}
void Pileup::zEndplate(double val) {
    // Don't know where endplate is yet.
    mZEndplate = val;
}

//-------------------------------------------------------
double Pileup::zStartStopMatchMin() {
    return mZStartStopMatchMin;
}
void Pileup::zStartStopMatchMin(double val) {
    mZStartStopMatchMin = val;
}

//-------------------------------------------------------
double Pileup::zPileMatchMin() {
    return mZPileMatchMin;
}
void Pileup::zPileMatchMin(double val) {
    mZPileMatchMin = val;
}

//-------------------------------------------------------
double Pileup::zPeakMatchMin() {
    return mZPeakMatchMin;
}
void Pileup::zPeakMatchMin(double val) {
    mZPeakMatchMin = val;
}

//-------------------------------------------------------
double Pileup::zStartStopMatchMax() {
    return mZStartStopMatchMax;
}
void Pileup::zStartStopMatchMax(double val) {
    mZStartStopMatchMax = val;
}

//-------------------------------------------------------
double Pileup::zPileMatchMax() {
    return mZPileMatchMax;
}
void Pileup::zPileMatchMax(double val) {
    mZPileMatchMax = val;
}

//-------------------------------------------------------
double Pileup::zPeakMatchMax() {
    return mZPeakMatchMax;
}
void Pileup::zPeakMatchMax(double val) {
    mZPeakMatchMax = val;
}

//-------------------------------------------------------
double Pileup::dead(int ih) {
    if (0 <= ih && ih < 7) {
        return mDead[ih];
    } else {
        return -1;
    }
}
void Pileup::dead(int ih, double dead) {
    // Points are sometimes projected beyond central membrane.
    // Want to ignore these as peaks. (Typically in ih = 0 to 3).
    // We usually want to find all peaks in ih = 4 and 5 but in deciding
    // whether or not point goes in 4 or 5 we use mDead[6] as a keep out.
    if (0 <= ih && ih < 7) {
        mDead[ih] = dead;
    }
}



//-------------------------------------------------------
 double Pileup::z(int iv, int which) {
    if (iv < 0 || mNVerts < iv) {
        return -9999;
    }
    if (1 == mFlag[iv] || 2 == mFlag[iv] || 3 == mFlag[iv]) {
        if (1 == which) {
            return 0.5*(mVerts[iv][0] + mVerts[iv][2]);
        } else if (2 == which) {
            return mVerts[iv][0];
        } else if (3 == which) {
            return mVerts[iv][2];
        } else if (4 == which) {
            return mVerts[iv][4];
        } else {
            return -9999;
        }
    } else if (4 == mFlag[iv] || 5 == mFlag[iv]) {
        if (1 == which) {
            return mVerts[iv][0];
        } else if (2 == which) {
            return mVerts[iv][2];
        } else if (3 == which) {
            return mVerts[iv][4];
        } else if (4 == which) {
            return mVerts[iv][5];
        } else {
            return -9999;
        }
    } else if (6 == mFlag[iv]) {
        if (1 == which) {
            return 0.5*(mVerts[iv][0] + mVerts[iv][2]);
        } else if (2 == which) {
            return mVerts[iv][0];
        } else if (3 == which) {
            return mVerts[iv][2];
        } else {
            return -9999;
        }
    } else if (7 == mFlag[iv]) {
        if (1 == which) {
            return mVerts[iv][0];
        } else {
            return -9999;
        }
    }
    return -9999;
}
//-------------------------------------------------------
int Pileup::n(int iv, int which) {
    if (iv < 0 || mNVerts < iv) {
        return -9999;
    }
    double num = -9999;
    if (1 == mFlag[iv] || 2 == mFlag[iv] || 3 == mFlag[iv]) {
        if (1 == which) {
            num = mVerts[iv][1] + mVerts[iv][3];
        } else if (2 == which) {
            num = mVerts[iv][1];
        } else if (3 == which) {
            num = mVerts[iv][3];
        } else {
            num = -9999;
        }
    } else if (4 == mFlag[iv] || 5 == mFlag[iv]) {
        if (1 == which) {
            num = mVerts[iv][1];
        } else if (2 == which) {
            num = mVerts[iv][3];
        } else {
            num = -9999;
        }
    } else if (6 == mFlag[iv]) {
        if (1 == which) {
            num = mVerts[iv][1] + mVerts[iv][3];
        } else if (2 == which) {
            num = mVerts[iv][1];
        } else if (3 == which) {
            num = mVerts[iv][3];
        } else {
            num = -9999;
        }
    } else if (7 == mFlag[iv]) {
        if (1 == which) {
            num = mVerts[iv][1];
        } else {
            num = -9999;
        }
    }
    return int(num);
}

//-------------------------------------------------------
 int Pileup::flag(int iv) {
    if (iv < 0 || mNVerts < iv) {
        return 0;
    }
    return mFlag[iv];
}

//-------------------------------------------------------
int  Pileup::nearest(StMuDst *muDst, double z, double *dist, int *mult) {

    fillHistos(muDst);
    findPiles();

    // Given the pileup candidate distances we go through all pairs in V_z+ and V_z-
    // looking for separation distance consistent with pileup distance.
    // Keep track of which vertex is closest to z.
    int nPileFound = 0;
    double minDist = 9999.0;
    int    minNum  = 0;
    for (int ip=0;ip<mNPiles;ip++) {
        for (int ip4=0;ip4<mNPeaks[4];ip4++) {
            for (int ip5=0;ip5<mNPeaks[5];ip5++) {
                double dist = -9999;
                if (2 == mPileFlag[ip]) {
                    dist = mPos[4][ip4]-mPos[5][ip5] - mPileDist[ip];
                } else if (3 == mPileFlag[ip]) {
                    dist = mPos[5][ip5]-mPos[4][ip4] - mPileDist[ip];
                }
                if ( mZPileMatchMin < dist && dist < mZPileMatchMax ) {
                    nPileFound += 2;
                    if (fabs(mPos[4][ip4]-z) < fabs(minDist)) {
                        minDist = mPos[4][ip4] - z;
                        minNum  = int(mArea[4][ip4]);
                    }
                    if (fabs(mPos[5][ip5]-z) < fabs(minDist)) {
                        minDist = mPos[5][ip5] - z;
                        minNum  = int(mArea[5][ip5]);
                    }
                }
            }
        }
    }
    *dist = minDist;
    *mult = minNum;
    return nPileFound;
}

//-------------------------------------------------------
int Pileup::find(StMuDst *muDst) {
    // This invokes findPeaks to make lists of peaks in each of the pileup histograms.
    // First try finding predicted pileup separation.
    //     Pre-pileup should have match of h[0] in h[3]  or h[2] in h[1].
    //     Post-pileup should have match of h[2] in h[3]
    //     These will be used to predict the h[4]-h[5] separations.
    // First pass through h[4], h[5] is to look for matches which we call good vertices.
    //   flag = 1;
    // Then offset unmatched h[4] by predicted pileup distance. If we find unmatched we call it pileup.
    //   flag = 2 for pre-pileup
    //   flag = 3 for post-pileup
    // If an unmatched matches an already matched flag according to whether matched was called
    //   good or not. (We are quite concerned about pileup reconstructing to actual vertex.)
    //   flag = 4 if h[4] was good
    //   flag = 5 if h[5] was good
    // Finally, take all unmatched and copy into verts. If we have one h[4] and one h[5]
    // pair them for convenience.
    //   flag = 6 we have pair of left overs
    //   flag = 7 otherwise


    fillHistos(muDst);
    findPiles();


    // Now look for good, matching histograms in V_z.
    for (int i=0;i<5;i++) {
        mMatchM[i] = 0;
        mUsedM[i]  = 0;
        mMatchP[i] = 0;
        mUsedP[i]  = 0;
    }
    int nVerts = 0;
    for (int ip4=0;ip4<mNPeaks[4];ip4++) {
        for (int ip5=0;ip5<mNPeaks[5];ip5++) {
            if ( mZPeakMatchMin < mPos[4][ip4]-mPos[5][ip5] &&
                 mPos[4][ip4]-mPos[5][ip5] < mZPeakMatchMax) {
                if (nVerts < 10) {
                    mVerts[nVerts][0] = mPos[4][ip4];
                    mVerts[nVerts][1] = mArea[4][ip4];
                    mVerts[nVerts][2] = mPos[5][ip5];
                    mVerts[nVerts][3] = mArea[5][ip5];
                    mVerts[nVerts][4] = mPos[4][ip4]-mPos[5][ip5];
                    mFlag[nVerts]     = 1;
                }
                nVerts++;
                mMatchP[ip4] = nVerts;
                mMatchM[ip5] = nVerts;
                mUsedP[ip4] = nVerts;
                mUsedM[ip5] = nVerts;
                if (mVerbose) {
                    cout << "Good vertex. pos4 = " << mPos[4][ip4] << ", pos5 = " << mPos[5][ip5] << endl;
                }
            }
        }
    }
    // Try matching vertices to pileup.
    for (int ip=0;ip<mNPiles;ip++) {
        for (int ip4=0;ip4<mNPeaks[4];ip4++) {
            for (int ip5=0;ip5<mNPeaks[5];ip5++) {
                double dist = -9999;
                if (2 == mPileFlag[ip]) {
                    dist = mPos[4][ip4]-mPos[5][ip5] - mPileDist[ip];
                } else if (3 == mPileFlag[ip]) {
                    dist = mPos[5][ip5]-mPos[4][ip4] - mPileDist[ip];
                }
                if ( mZPileMatchMin < dist &&
                     dist < mZPileMatchMax ) {
                    if (!mMatchP[ip4] && !mMatchM[ip5]) {
                        if (nVerts < 10) {
                            mVerts[nVerts][0] = mPos[4][ip4];
                            mVerts[nVerts][1] = mArea[4][ip4];
                            mVerts[nVerts][2] = mPos[5][ip5];
                            mVerts[nVerts][3] = mArea[5][ip5];
                            mVerts[nVerts][4] = dist;
                            mFlag[nVerts]     = mPileFlag[ip];
                        }
                        nVerts++;
                        mUsedP[ip4] = nVerts;
                        mUsedM[ip5] = nVerts;
                        if (mVerbose) {
                            cout << "Matched pileup at " << mPos[4][ip4] << " to " << mPos[5][ip5] << ". Expected distance of " << mPileDist[ip] << endl;
                        }
                    } else if (mMatchP[ip4]) {
                        if (nVerts < 10) {
                            mVerts[nVerts][0] = mPos[4][ip4];
                            mVerts[nVerts][1] = mArea[4][ip4];
                            mVerts[nVerts][2] = mPos[5][ip5];
                            mVerts[nVerts][3] = mArea[5][ip5];
                            // For this case we are storing predicted position - previously matched.
                            mVerts[nVerts][4] = dist;
                            // Also store predicted position.
                            if (2 == mPileFlag[ip]) {
                                mVerts[nVerts][5] = mPos[5][ip5] + mPileDist[ip];
                            } else if (3 == mPileFlag[ip]) {
                                mVerts[nVerts][5] = mPos[5][ip5] - mPileDist[ip];
                            }
                            mFlag[nVerts]     = mPileFlag[ip] + 2;
                        }
                        nVerts++;
                        mUsedM[ip5] = nVerts;
                        if (mVerbose) {
                            cout << "!!!!!Matched pileup with good vertex? 4 ->" << mPos[4][ip4] << " 5 -> " << mPos[5][ip5] << ". Expected distance of " << mPileDist[ip] << endl;
                        }
                    } else if (mMatchM[ip5]) {
                        if (nVerts < 10) {
                            mVerts[nVerts][0] = mPos[5][ip5];
                            mVerts[nVerts][1] = mArea[5][ip5];
                            mVerts[nVerts][2] = mPos[4][ip4];
                            mVerts[nVerts][3] = mArea[4][ip4];
                            // For this case we are storing predicted position - previously matched.
                            mVerts[nVerts][4] = dist;
                            // Also store predicted position.
                            if (2 == mPileFlag[ip]) {
                                mVerts[nVerts][5] = mPos[4][ip4] - mPileDist[ip];
                            } else if (3 == mPileFlag[ip]) {
                                mVerts[nVerts][5] = mPos[4][ip4] + mPileDist[ip];
                            }
                            mFlag[nVerts]     = mPileFlag[ip] + 2;
                        }
                        nVerts++;
                        mUsedP[ip4] = nVerts;
                        if (mVerbose) {
                            cout << "!!!!!Matched pileup with good vertex? 5 ->" << mPos[4][ip4] << " 5 -> " << mPos[5][ip5] << ". Expected distance of " << mPileDist[ip] << endl;
                        }
                    }
                }
            }
        }
    }
    // Have singleton peaks left over. If a single pair (one in 4 and one in 5) put them together.
    int iPlus[5], nPlus = 0;
    for (int ip4=0;ip4<mNPeaks[4];ip4++) {
        if (!mUsedP[ip4]) {
            iPlus[nPlus] = ip4;
            nPlus++;
        }
    }
    int iMinus[5], nMinus = 0;
    for (int ip5=0;ip5<mNPeaks[5];ip5++) {
        if (!mUsedM[ip5]) {
            iMinus[nMinus] = ip5;
            nMinus++;
        }
    }
    if ( nPlus==1 && nMinus==1) {
        if (nVerts < 10) {
            mVerts[nVerts][0] = mPos[4][iPlus[0]];
            mVerts[nVerts][1] = mArea[4][iPlus[0]];
            mVerts[nVerts][2] = mPos[5][iMinus[0]];
            mVerts[nVerts][3] = mArea[5][iMinus[0]];
            mVerts[nVerts][4] = mPos[4][iPlus[0]]-mPos[5][iMinus[0]];
            mFlag[nVerts]     = 6;
        }
        nVerts++;
    } else {
        for (int ip=0;ip<nPlus;ip++) {
            if (nVerts < 10) {
                mVerts[nVerts][0] = mPos[4][iPlus[ip]];
                mVerts[nVerts][1] = mArea[4][iPlus[ip]];
                mFlag[nVerts]     = 7;
            }
            nVerts++;
        }
        for (int im=0;im<nMinus;im++) {
            if (nVerts < 10) {
                mVerts[nVerts][0] = mPos[5][iMinus[im]];
                mVerts[nVerts][1] = mArea[5][iMinus[im]];
                mFlag[nVerts]     = 7;
            }
            nVerts++;
        }
    }
    if (nVerts > 10) {
        cout << "Found too many vertices; nVerts = " << nVerts << endl;
        nVerts = 10;
    }
    mNVerts = nVerts;
    return nVerts;
}
//-------------------------------------------------------
int Pileup::fillQAHistos(StMuDst *muDst) {
    float x1, y1, z1, x2, y2, z2;
    float zS1, zS2;
    float r1, r2;
    double phi1, phi2, pt;
    int isec1, isec2;
    int flag;
 
    for (int i=0;i<3;i++) {
        mHQA1D[i]->Reset();
        mHQA2D[i]->Reset();
    }

    StMuTrack* track;
    for (int it=0;it<muDst->globalTracks()->GetEntries();it++) {
        track = muDst->globalTracks(it);
        flag = track->flag();
        if (flag < 0 || 700 <= flag) {
            continue;
        }
        StThreeVectorF first = track->firstPoint();
        StThreeVectorF last  = track->lastPoint();
        x1 = first.x();
        y1 = first.y();
        z1 = first.z();
        x2 = last.x();
        y2 = last.y();
        z2 = last.z();
        pt = track->pt();
        if (pt < 0.4) {
            continue;
        }
        phi1 = fabs(atan2(y1,x1)*360/(2*3.1415926));
        isec1 = int((phi1+15.0)/30.0);
        phi1 = fabs(phi1-30*isec1);
        phi2 = fabs(atan2(y2,x2)*360/(2*3.1415926));
        isec2 = int((phi2+15.0)/30.0);
        phi2 = fabs(phi2-30*isec2);
        r1 = sqrt( x1*x1 + y1*y1 );
        r2 = sqrt( x2*x2 + y2*y2 );
        if (r1 > 50 && fabs(z1) < 190.0 && fabs(z2) < 190.0 && phi1 < 12.0 && phi2 < 12.0) {
            mHQA1D[0]->Fill(z1);
            mHQA1D[1]->Fill(z2);
        }
        mHQA2D[0]->Fill(z1,z2);
        zS1 = z1 + (z2-z1)*(65.0-r1)/(r2-r1);
        zS2 = z2 + (z1-z2)*(190.0-r2)/(r1-r2);
        mHQA2D[1]->Fill(zS1,zS2);
        mHQA2D[2]->Fill((65.0*zS2-190.0*zS1)/(65.0-190.0),zS2);
        mHQA1D[2]->Fill( (65.0*zS2-190.0*zS1) / (65.0-190.0) );
 
    }
    return 1;
}

//-------------------------------------------------------
int Pileup::fillHistos(StMuDst *muDst) {

    float x1, y1, z1, x2, y2, z2;
    float r1, r2, Vz;
    double phi1, phi2, pt;
    int isec1, isec2;
    int flag;

    for (int i=0;i<6;i++) {
        mHPileup[i]->Reset();
    }

    StMuTrack* track;
    for (int it=0;it<muDst->globalTracks()->GetEntries();it++) {
        track = muDst->globalTracks(it);
        flag = track->flag();
        if (flag < 0 || 700 <= flag) {
            continue;
        }
        StThreeVectorF first = track->firstPoint();
        StThreeVectorF last  = track->lastPoint();
        x1 = first.x();
        y1 = first.y();
        z1 = first.z();
        x2 = last.x();
        y2 = last.y();
        z2 = last.z();
        pt = track->pt();
        if (pt < 0.2) {
            continue;
        }
        r1 = sqrt( x1*x1 + y1*y1 );
        r2 = sqrt( x2*x2 + y2*y2 );
        Vz = (r1*z2-r2*z1)/(r1-r2);
        // Note: It appears electronic readout continues past the central membrane (a good thing)
        //       and some points are reconstructed on the other side (a bad thing)
        //       For now use a "dead" zone of 5cm where we ignore tracks with a first or
        //       last point closer to the central membrane than that.
        if (z2 > mDead[6]) {
            mHPileup[4]->Fill(Vz);
        } else if (z2 < -mDead[6]) {
            mHPileup[5]->Fill(Vz);
        }
        // Tracks with first point in inner tracker probably not pileup
        if (r1 < 50) {
            continue;
        }
        // Tracks leaving via the endplate or starting on a sector
        // boundary probably wash out start peak we are looking for.
        phi1  = fabs(atan2(y1,x1)*360/(2*3.1415926));
        isec1 = int((phi1+15.0)/30.0);
        phi1  = fabs(phi1-30*isec1);
        if (phi1 < 12) {
            if (0 < z2 && z2 < 190) {
                mHPileup[0]->Fill(z1);
            } else if (-190 < z2 && z2 < 0) {
                mHPileup[1]->Fill(z1);
            }
        }
        // Tracks ending on a sector boundary probably wash out stop peak we are looking for.
        // We also ignore tracks leaving via the endplate. (Could leave them in, but
        // then in searching for peaks we have to ignore them)
        phi2  = fabs(atan2(y2,x2)*360/(2*3.1415926));
        isec2 = int((phi2+15.0)/30.0);
        phi2  = fabs(phi2-30*isec2);
        if (phi2 < 12 && fabs(z2) < 190) {
            if (0 < z1) {
                mHPileup[2]->Fill(z2);
            } else {
                mHPileup[3]->Fill(z2);
            }
        }
    }
    return 1;
}

//-------------------------------------------------------
// This algorithm has problem when peaks are too close because we
// are trying to subtract local background.
int Pileup::findPeaks(int ih) {
    int nBins = mHPileup[ih]->GetNbinsX();
    double binStart = mHPileup[ih]->GetBinLowEdge(1);
    double binWidth = mHPileup[ih]->GetBinWidth(1);
    double avg = mHPileup[ih]->Integral() / 400.0;
    double min = 2.5+mHPileup[ih]->Integral()*7/1000.0;
    double sum, w, mean;

    int nPeaks = 0;
    for (int i=2;i<nBins-1;i++) {
        if (mHPileup[ih]->GetBinContent(i) > min) {
            // Found bin that is statistically significantly above fluctuation.
            // Add bins with higher than twice average content on both sides 
            sum   = mHPileup[ih]->GetBinContent(i);
            w     = 1;
            mean  = i*mHPileup[ih]->GetBinContent(i);
            int j = i-1;
            while (mHPileup[ih]->GetBinContent(j) > 2*avg+1) {
                sum   += mHPileup[ih]->GetBinContent(j);
                w     += 1;
                mean  += j*mHPileup[ih]->GetBinContent(j);
                j -= 1;
                if (j == 0) {
                    break;
                }
            }
            int imin = j-5;
            j = i+1;
            // Require consecutive bins below cut-off before terminating this peak.
            while (mHPileup[ih]->GetBinContent(j) > 2*avg+1 || mHPileup[ih]->GetBinContent(j+1) > 2*avg+1) {
                sum   += mHPileup[ih]->GetBinContent(j);
                w     += 1;
                mean  += j*mHPileup[ih]->GetBinContent(j);
                j += 1;
                if (j == nBins) {
                    break;
                }
            }
            int imax = j+5;
            mean /= sum;
            // Require peak area to be at least twice min.
            // if (sum > 2*min) {
            // Require signigficant peak if background flat.
            if (sum-w*avg > 5*sqrt(w*avg)) {
                // If peak is not significantly above its neighborhood we ignore it.
                // sum five bins on either side, starting from min and max of current peak.
                double back = 0;
                if (imin > 5) {
                    int k = imin - 5;
                    for (int ib=0;ib<5;ib++) {
                        back += mHPileup[ih]->GetBinContent(ib+k);
                    }
                }
                if (imax < nBins-6) {
                    int k = imax;
                    for (int ib=0;ib<5;ib++) {
                        back += mHPileup[ih]->GetBinContent(ib+k);
                    }
                }
                if (imin <= 5 || imax > nBins-5) {
                    back *= 2;
                }
                back = w*back/10;
                mean = mean*binWidth + binStart;
                if (fabs(mean) > mDead[ih]) {
                    if (sum-back > 5*sqrt(back)) {
                        if (nPeaks < 5) {
                            mPos[ih][nPeaks] = mean;
                            mArea[ih][nPeaks] = sum;
                            mWidth[ih][nPeaks] = w;
                        }
                        nPeaks++;
                        if (mVerbose) {
                            printf("ih = %i: Width of peak = %f, area = %f, position = %f, background = %f\n",ih,w,sum,mean,back);
                        }
                    }
                } else {
                    if (mVerbose) {
                        printf("Found a peak but it is in dead region. ");
                        printf("ih = %i:Width of peak = %f, area = %f, position = %f, background = %f\n",ih,w,sum,mean,back);
                    }
                }
            }
            i = j+1;
        }
    }
    return nPeaks;
}
//-------------------------------------------------------
int Pileup::testFindPeaks(TH1D *h, int ih) {
    TH1D *lp = lowPass(h,1);

    int nBins = lp->GetNbinsX();
    double binStart = lp->GetBinLowEdge(1);
    double binWidth = lp->GetBinWidth(1);
    double avg = lp->Integral() / nBins;
    double min = 5*sqrt(avg);
    double sum, w, mean;

    int nPeaks = 0;
    for (int i=2;i<nBins-1;i++) {
        if (lp->GetBinContent(i) > min) {
            // Found bin that is statistically significantly above fluctuation.
            // Add bins with higher than twice average content on both sides 
            sum   = lp->GetBinContent(i);
            w     = 1;
            mean  = i*lp->GetBinContent(i);
            int j = i-1;
            // I bet we will need to split some peaks.
            while (lp->GetBinContent(j) > 2*sqrt(avg)) {
                sum   += lp->GetBinContent(j);
                w     += 1;
                mean  += j*lp->GetBinContent(j);
                j -= 1;
                if (j == 0) {
                    break;
                }
            }
            j = i+1;
            while (lp->GetBinContent(j) > 2*sqrt(avg)) {
                sum   += lp->GetBinContent(j);
                w     += 1;
                mean  += j*lp->GetBinContent(j);
                j += 1;
                if (j == nBins) {
                    break;
                }
            }
            mean /= sum;
            // Require signigficant peak if background flat.
            if (sum-w*avg > 5*sqrt(w*avg)) {
                mean = mean*binWidth + binStart;
                if (fabs(mean) > mDead[ih]) {
                    if (nPeaks < 5) {
                        mPos[ih][nPeaks] = mean;
                        mArea[ih][nPeaks] = sum;
                        mWidth[ih][nPeaks] = w;
                    }
                    nPeaks++;
                    if (mVerbose) {
                        printf("ih = %i: Width of peak = %f, area = %f, position = %f\n",ih,w,sum,mean);
                    }
                } else {
                    if (mVerbose) {
                        printf("Found a peak but it is in dead region. ");
                        printf("ih = %i:Width of peak = %f, area = %f, position = %f\n",ih,w,sum,mean);
                    }
                }
            }
            i = j+1;
        }
    }
    delete lp;
    return nPeaks;
}
//-------------------------------------------------------
TH1D* Pileup::lowPass(TH1D *h, int width) {
    TH1D *lp = (TH1D *) h->Clone();
    lp->Reset();
    int upper = h->GetNbinsX();

    int nSum, jMin, jMax;
    double sum;
    for (int i=1;i<upper;i++) {
        jMin = i-width < 1 ? 1 : i-width;
        jMax = i+width >upper ? upper : i+width;
        sum = 0;
        nSum = 0;
        for (int j=jMin;j<jMax;j++) {
            sum +=  h->GetBinContent(j);
            nSum++;
        }
        lp->SetBinContent(i,sum/nSum);
    }
    return lp;
}
//-------------------------------------------------------
int  Pileup::findPiles() {
    // Get list of possible vertices for all histograms.
    for (int ih=0;ih<6;ih++) {
        mNPeaks[ih] = findPeaks(ih);
        if (mNPeaks[ih] > 5) {
            cout << "Vertex " << ih << " had " << mNPeaks[ih] << " peaks found which is too big for my arrays." << endl;
            mNPeaks[ih] = 5;
        }
    }

    mNPiles = 0;
    // Look for pre pileup
    // For each peak in 0 and 1 look for match in 3 and 2
    for (int ip0=0;ip0<mNPeaks[0];ip0++) {
        for (int ip3=0;ip3<mNPeaks[3];ip3++) {
            if (  mZStartStopMatchMin < mPos[0][ip0]+mPos[3][ip3] &&
                  mPos[0][ip0]+mPos[3][ip3] < mZStartStopMatchMax) {
                if (mVerbose) {
                    cout << "Possible match for pre + -. pos0 = " << mPos[0][ip0] << ", pos3 = " << mPos[3][ip3] << endl;
                }
                if (mNPiles < 10) {
                    // For pre pileup the drift distance is from the central membrane, so the vertex
                    // separation is twice that.
                    mPileDist[mNPiles] = mPos[0][ip0] - mPos[3][ip3];
                    mPileFlag[mNPiles] = 2;
                }
                mNPiles++;
            }
        }
    }
    for (int ip2=0;ip2<mNPeaks[2];ip2++) {
        for (int ip1=0;ip1<mNPeaks[1];ip1++) {
            if ( mZStartStopMatchMin < mPos[2][ip2]+mPos[1][ip1] &&
                 mPos[2][ip2]+mPos[1][ip1] < mZStartStopMatchMax) {
                if (mVerbose) {
                    cout << "Possible match for pre - +. pos2 = " << mPos[2][ip2] << ", pos1 = " << mPos[1][ip1] << endl;
                }
                if (mNPiles < 10) {
                    mPileDist[mNPiles] = mPos[2][ip2] - mPos[1][ip1];
                    mPileFlag[mNPiles] = 2;
                }
                mNPiles++;
            }
        }
    }
    // Look for post pileup.
    for (int ip2=0;ip2<mNPeaks[2];ip2++) {
        for (int ip3=0;ip3<mNPeaks[3];ip3++) {
            if ( mZStartStopMatchMin < mPos[2][ip2]+mPos[3][ip3] &&
                 mPos[2][ip2]+mPos[3][ip3] < mZStartStopMatchMax) {
                if (mVerbose) {
                    cout << "Possible match for post. pos2 = " << mPos[2][ip2] << ", pos3 = " << mPos[3][ip3] << endl;
                }
                if (mNPiles < 10) {
                    // For post pileup the drift distance is from the endplate.
                    mPileDist[mNPiles] = mZEndplate-mPos[2][ip2] + mZEndplate+mPos[3][ip3];
                    mPileFlag[mNPiles] = 3;
                }
                mNPiles++;
            }
        }
    }
    if (mNPiles > 10) {
        cout << "Found too many piles; nPiles = " << mNPiles << endl;
        mNPiles = 10;
    }
    return mNPiles;
}
//-------------------------------------------------------
TH1D *Pileup::hist(int ih) {
    // Return histograms used for examining vertex structure.
    // For pileup track zStart and zEnd have to be in same half of TPC (except when point
    // projected past the central membrane)
    // A pileup candidate in one half of TPC must match candidate in other half.
    // ih = 0 is zStart for zEnd > 0    pre pileup would match peak in ih = 3
    // ih = 1 is zStart for zEnd < 0    pre pileup would match peak in ih = 2
    // ih = 2 is zEnd   for zStart > 0  post pileup would match -peak in ih=3
    // ih = 3 is zEnd   for zStart < 0  post pileup would match -peak in ih=2
    // ih = 4 is sheared zEnd for zEnd > 0
    // ih = 5 is sheared zEnd for zEnd < 0
    // Good vertex should have peak in ih = 4 matched to peak in ih = 5.
    // Pileup vertices will be offset in 4 and 5 by amount determined from 0-3.
    if (ih < 0 || 5 < ih) {
        return NULL;
    }
    return mHPileup[ih];
}
//-------------------------------------------------------
TH1D *Pileup::histQA1D(int ih) {
    // Return 1D QA histogram
    // ih = 0 is zStart (should contain peaks if pre pileup)
    // ih = 1 is zEnd   (should contain peaks if pre or post pileup)
    // ih = 2 is sheared zEnd (should contain peaks for vertex positions)
    if (ih < 0 || 2 < ih) {
        return NULL;
    }
    return mHQA1D[ih];
}
//-------------------------------------------------------
TH2D *Pileup::histQA2D(int ih) {
    // Return 2D QA histogram
    // ih = 0 is raw zStart vs. zEnd (compare to 1D ih = 0 and ih = 1;
    // ih = 1 is zStart vs. zEnd with values scaled to reference radii.
    // ih = 2 is zStart vs sheared zEnd.
    if (ih < 0 || 2 < ih) {
        return NULL;
    }
    return mHQA2D[ih];
}




/**********************************************************************
 *
 * $Log: Pileup.cxx,v $
 * Revision 1.1  2008/12/02 23:47:43  prindle
 * Code to check for possible pileup. Really belongs in some vertex finding
 * package but is here for now.
 *
 * initial check in of Pileup characterizer
 *
 *
 *********************************************************************/
