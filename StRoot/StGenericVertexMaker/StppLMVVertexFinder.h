/*!
 * \class StppLMVVertexFinder
 * \author Jan Balewski, July 2004
 *
 *  StGenericVertexFinder implementation of ppLMV
 *
 * ppLMV use new set of params
 *  INT:  CtbThres/ch=2   MinTrkPonits=10   i2=0   i3=0   i4=0   i5=0   i6=0   i7=0   i8=0   i9=9999
 *  FLOAT:  CtbThres/MeV=1.000000  MaxTrkDcaRxy=3.900000  MinTrkPt/GeV=0.200000  
 *          CtbEtaErr=0.020000  CtbPhiErr/deg=1.000000  
 *          MaxTrkDcaZ=180.000000  
 *          f6=0.000000  f7=0.000000  f8=0.000000  f9=8888.000000
 *
 * $Id: StppLMVVertexFinder.h,v 1.2 2004/07/23 01:00:52 jeromel Exp $
 *
 */

#include <vector>
#include <StThreeVectorD.hh>
#include <StPhysicalHelixD.hh>
#include "StGenericVertexFinder.h"

class StEvent;
class StTrack;
class StMaker;

class StppLMVVertexFinder: public StGenericVertexFinder {
 public:
    StppLMVVertexFinder();

    // mandatory implementations
    virtual         ~StppLMVVertexFinder();
    bool            fit(StEvent*);         
    int             NCtbMatches();

    // Added, not part of the base-class
    void            printInfo(ostream& = cout) const;
    void            UseVertexConstraint(double x0, double y0, double dxdz, double dydz, double weight);

private:
    double                   mWidthScale;
    double                   mX0  ; // starting point of beam parameterization
    double                   mY0  ; // starting point of beam parameterization
    double                   mdxdz; // beam slope
    double                   mdydz; // beam slope
    //inline void setFlagBase(UInt_t base){mFlagBase=base;};
    

    double                   mFmin;       // best function value found
    double                   mFedm;       // estimated vertical distance remaining to minimum
    double                   mErrdef;     // value of UP defining parameter uncertainty
    int                      mNpari;      // number of variable parameters
    int                      mNparx;      // highest parameter number defined


    //jan--------------------
 private:
    /*!
     * \struct ctbHit 
     */
    struct ctbHit {
      float phi;
      float eta;
      float adc;
    };
    vector<ctbHit> mCtbHits;

    bool requireCTB;    
    bool matchTrack2CTB (StTrack* rTrack, float & sigma);
    void collectCTBhitsData(StEvent* );
    bool collectCTBhitsMC();
    void ctb_get_slat_from_data(int slat, int tray, float & phiDeg, float &eta);
    bool ppLMV4();
    int mCtbThres_ch;// to reject slats below threshold
    float mCtbThres_mev;
    double mMaxTrkDcaRxy;    //DCA to nominal beam line for each track
    double mMinTrkPt;        //~ pT=0.16(GeV/c) == R=2 (m )in 2001
    float  mMatchCtbMax_eta;
    float  mMatchCtbMax_phi;
    float  mDVtxMax;
    int    mBLequivNtr;
    int n1,n2,n3,n4,n5,n6;
    float mBfield;// magnetic field
    float  mCtbEtaSeg, mCtbPhiSeg;
    int mTotEve;
    int eveID;
    StMaker *mDumMaker;

    /*!
     * \struct
     */
    struct JHelix {StPhysicalHelixD helix; float sigma; };

    vector<JHelix> mPrimCand;

};



/***************************************************************************
 *
 * $Log: StppLMVVertexFinder.h,v $
 * Revision 1.2  2004/07/23 01:00:52  jeromel
 * Removed methods/data members (moved in base class) + doxygenized
 *
 * Revision 1.1  2004/07/21 01:53:18  balewski
 * first
 *
 **************************************************************************/

