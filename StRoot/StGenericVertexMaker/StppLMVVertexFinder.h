/*!
 * \class StppLMVVertexFinder
 * \author Jan Balewski, July 2004
 *
 *  StGenericVertexFinder implementation of ppLMV
 * $Id: StppLMVVertexFinder.h,v 1.5 2004/08/04 21:57:56 balewski Exp $
 *
 */

#include <vector>
#include <StThreeVectorD.hh>
#include <StPhysicalHelixD.hh>
#include "StGenericVertexFinder.h"
#include "StCtbUtility.h"

class StEvent;
class StTrack;

class StppLMVVertexFinder: public StGenericVertexFinder , StCtbUtility {
 public:
    StppLMVVertexFinder();

    // mandatory implementations
    virtual         ~StppLMVVertexFinder();
    bool            fit(StEvent*);         
    void            printInfo(ostream& = cout) const;
    void            UseVertexConstraint(double x0, double y0, double dxdz, double dydz, double weight);

private:
    // double                   mWidthScale;
    double                   mX0  ; // starting point of beam parameterization
    double                   mY0  ; // starting point of beam parameterization
    double                   mdxdz; // beam slope
    double                   mdydz; // beam slope

    //jan--------------------
 private:

    bool matchTrack2CTB (StTrack* rTrack, float & sigma);
    bool ppLMV5();
    double mMaxTrkDcaRxy;    //DCA to nominal beam line for each track
    double mMinTrkPt;        //~ pT=0.16(GeV/c) == R=2 (m )in 2001
    float  mMatchCtbMax_eta;
    float  mMatchCtbMax_phi;
    float  mDVtxMax;
    uint    mMinMatchTr; // minimal # of tracks matched to CTB for valid vertex
    int    mBLequivNtr;
    int n1,n2,n3,n4,n5,n6;
    float mBfield;// magnetic field
    int mTotEve;
    int eveID;
    int NCtbMatches();
    int NCtbSlats();

    /*!
     * \struct
     */
    struct JHelix {StPhysicalHelixD helix; float sigma; };

    vector<JHelix> mPrimCand;

};



/***************************************************************************
 *
 * $Log: StppLMVVertexFinder.h,v $
 * Revision 1.5  2004/08/04 21:57:56  balewski
 * toward smarter ppLMV5
 *
 * Revision 1.4  2004/07/24 02:57:40  balewski
 * clean up of ppLMV, CTB-util separated
 *
 * Revision 1.3  2004/07/23 02:24:39  jeromel
 * Oops ... Worng swithc (had twice Minuit). Now corrected.
 *
 * Revision 1.2  2004/07/23 01:00:52  jeromel
 * Removed methods/data members (moved in base class) + doxygenized
 *
 * Revision 1.1  2004/07/21 01:53:18  balewski
 * first

 *
 * ppLMV use new set of params
 *  INT:  CtbThres/ch=2   MinTrkPonits=10   i2=0   i3=0   i4=0   i5=0   i6=0   i7=0   i8=0   i9=9999
 *  FLOAT:  CtbThres/MeV=1.000000  MaxTrkDcaRxy=3.900000  MinTrkPt/GeV=0.200000  
 *          CtbEtaErr=0.020000  CtbPhiErr/deg=1.000000  
 *          MaxTrkDcaZ=180.000000  
 *          f6=0.000000  f7=0.000000  f8=0.000000  f9=8888.000000
 *
 *
 **************************************************************************/

