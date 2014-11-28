/**********************************************************************
 *
 * $Id: Pileup.h,v 1.2 2011/02/12 00:07:04 prindle Exp $
 *
 * Author: Duncan Prindle
 *
 **********************************************************************
 *
 * Description:  Using first/last points on tracks characterize event vertex structure.
 *     An event, meaning the TPC readout for a trigger, can have multiple vertices.
 *     Usually only one of these will be `good', although it is possible for two (or more)
 *     collisions on the same beam crossing.
 *
 *
 *
 *     Probably you want to make a Pileup object at the beginning of your job and call
 *         int  nearest(StMuDst *muDst, double z, double *dist, int *mult);
 *     for each event.
 *     Inputs are:
 *         muDst    pointer to the StMuDst object containing the event
 *         z        Usually the z position of the primary vertex of interest.
 *     Output arguments are:
 *         dist     Distance from z to the nearest pileup vertex (if a pileup is found).
 *         mult     Multiplicity associated with this pileup vertex.
 *     Method returns the number of pileup vertices found.
 *
 *     I usually reject events with fabs(z) < 20cm. If you keep a histogram of z you can
 *     estimate how many good events you rejected, and for 20cm this is usually a small fraction.
 *
 *
 *
 *
 *     We flag vertices with seven possible meanings (in flag).
 *          1) Found vertex in +Z and -Z at same point. Probably good.
 *             Which = 1 -> Average position(z), sum of tracks (n).
 *                     2 -> Position determined by +Z TPC (z), tracks in +Z TPC (n), width of +Z TPC peak (width)
 *                     3 -> Position determined by -Z TPC (z), tracks in -Z TPC (n), width of -Z TPC peak (width)
 *                     4 -> Difference between +Z TPC and -Z TPC (z).
 *          2) Offset +Z, -Z vertex matches distance expected for pre pileup
 *             Which = 1 -> Average position (should be actual vertex position), sum of tracks.
 *                     2 -> Apparent position determined by +Z TPC, tracks in +Z TPC
 *                     3 -> Apparent position determined by -Z TPC, tracks in -Z TPC
 *                     4 -> Distance between vertices minus expected distance
 *          3) Offset +Z, -Z vertex matches distance expected for post pileup
 *             Which : Same meanings as for 2)
 *          4) Expected pre pileup. Found vertex with mFlag=1 is consistent with pre pileup.
 *             Which = 1 -> Position of `good' vertex, number of `good tracks.
 *                     2 -> Position of pileup vertex, number of pileup tracks.
 *                     3 -> Distance between vertices minus expected pre pileup distance
 *                     4 -> Expected position of pileup near good vertex.
 *          5) Expected post pileup. Found vertex with mFlag=1 is consistent with post pileup.
 *             Which = 1 -> Position of `good' vertex, number of `good tracks.
 *                     2 -> Position of pileup vertex, number of pileup tracks.
 *                     3 -> Distance between vertices minus expected post pileup distance
 *                     4 -> Expected position of pileup near good vertex.
 *          6) Have two vertices left over that could be pre or post pileup, but don't match expected distance.
 *             Which = 1 -> Average position, sum of tracks
 *                     2 -> Position of first vertex, number of first tracks.
 *                     3 -> Position of second vertex, number of second tracks.
 *          7) Vertex left over which doesn't match another pileup candidate
 *             Which = 1 -> Position of vertex, number of tracks.
 *
 *    Note: When we find a peak we determine the position as the weighted mean of bins in the peak.
 *          If there is only one bin then the positions will be quantized to the size of our
 *          internal histograms, currently 1cm.
 **********************************************************************/

#ifndef _Pileup
#define _Pileup

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "TH1D.h"
#include "TH2D.h"
#include "Stiostream.h"

class TH1F;

class Pileup : public TObject {

 private:
    TH1D *mHPileup[6];
    TH1D *mHQA1D[3];
    TH2D *mHQA2D[3];
    StMuDst *mMuDst;
    double mDead[7];
    double mZEndplate;
    double mZStartStopMatchMin;
    double mZStartStopMatchMax;
    double mZPileMatchMin;
    double mZPileMatchMax;
    double mZPeakMatchMin;
    double mZPeakMatchMax;
    int    mFlag[10];
    int  mVerbose;

 public:
    Pileup();
    virtual ~Pileup();
    int mNVerts;

    // Given position z find nearest vertex identified as pileup.
    // Return distance to that vertex and its multiplicity in arguments.
    // Return total number of pileup vertices found in this event.
    int  nearest(StMuDst *muDst, double z, double *dist, int *mult);

    int  find(StMuDst *muDst);
    int  findPeaks(int ih);
    int  testFindPeaks(TH1D *h, int ih);
    TH1D* lowPass(TH1D *h, int width);
    int  findPiles();
    double z(int iv, int which = 1);
    int  n(int iv, int which = 1);
    int  flag(int iv);
    int  fillHistos(StMuDst *muDst);
    int  fillQAHistos(StMuDst *muDst);
    TH1D *hist(int ih);
    TH1D *histQA1D(int ih);
    TH2D *histQA2D(int ih);
    int  verbose();
    void verbose(int val);
    double zEndplate();
    void   zEndplate(double val);
    double zStartStopMatchMin();
    void   zStartStopMatchMin(double val);
    double zPileMatchMin();
    void   zPileMatchMin(double val);
    double zPeakMatchMin();
    void   zPeakMatchMin(double val);
    double zStartStopMatchMax();
    void   zStartStopMatchMax(double val);
    double zPileMatchMax();
    void   zPileMatchMax(double val);
    double zPeakMatchMax();
    void   zPeakMatchMax(double val);
    double dead(int ih);
    void   dead(int ih, double dead);

    // Some "private" stuff I don't want to write accessor functions for.
    // Useful for optimizing code.
    // Allow up to 5 peaks in each of the pileup histograms.
    int mNPeaks[6];
    double mPos[6][5];
    double mArea[6][5];
    double mWidth[6][5];
    // Allow as many as 10 pileup candidates. Why ten?
    int mNPiles;
    double mPileDist[10];
    int    mPileFlag[10];
    // Each of + and - vertex histograms can have at most 5 peaks.
    int    mMatchM[5];
    int    mMatchP[5];
    int    mUsedM[5];
    int    mUsedP[5];
    // Combined we allow ten vertex candidates.
    double mVerts[10][5];

  ClassDef(Pileup,1)
};



#endif


/**********************************************************************
 *
 * $Log: Pileup.h,v $
 * Revision 1.2  2011/02/12 00:07:04  prindle
 *   Just changed comments. Hopefully it is understandble how to use the package
 * just from the comments now.
 *
 * Revision 1.1  2008/12/02 23:47:44  prindle
 * Code to check for possible pileup. Really belongs in some vertex finding
 * package but is here for now.
 *
 * initial check in of Pileup characterizer
 *
 *
 *********************************************************************/
