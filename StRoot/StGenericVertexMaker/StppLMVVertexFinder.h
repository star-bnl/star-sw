/*!
 * \class StppLMVVertexFinder
 * \author Jan Balewski, July 2004
 *
 *  StGenericVertexFinder implementation of ppLMV
 * $Id: StppLMVVertexFinder.h,v 1.12 2012/12/12 22:09:58 fisyak Exp $
 *
 */

#ifdef __APPLE__
#include <sys/types.h>
#endif
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
    int             fit(StEvent*);         
    void            printInfo(ostream& = cout) const;
    void            UseVertexConstraint(double x0, double y0, double dxdz, double dydz, double weight);
    void            Clear();

    // over-written method
    virtual void    Init();
    void addFakeVerex(float z);

 private:
  
    double          mX0  ;     // starting point of beam parameterization
    double          mY0  ;     // starting point of beam parameterization
    double          mdxdz;     // beam slope
    double          mdydz;     // beam slope
    unsigned int           mMinNumberOfFitPointsOnTrack;
    double                 mWeight ;          // Weight in fit for vertex contraint
    StPhysicalHelixD*      mBeamHelix;        // Beam Line helix

    //jan--------------------
    bool   matchTrack2CTB (StTrack* rTrack, float & sigma);
    bool   ppLMV5();
    double mMaxTrkDcaRxy;    //DCA to nominal beam line for each track
    double mMinTrkPt;        //~ pT=0.16(GeV/c) == R=2 (m )in 2001
    float  mMatchCtbMax_eta;
    float  mMatchCtbMax_phi;
    float  mDVtxMax;
    uint   mMinMatchTr; // minimal # of tracks matched to CTB for valid vertex
    float  mMaxZrange;// for tracks used by the vertex finder.
    int    mBLequivNtr;
    int    n1,n2,n3,n4,n5,n6; // private counters
    float  mBfield;// magnetic field
    int    mTotEve;
    int    eveID;
    int    NCtbMatches();

    /*!
     * \struct
     */
    struct JHelix {StPhysicalHelixD helix; float sigma; };

    vector<JHelix> mPrimCand;

};



/***************************************************************************
 *
 * $Log: StppLMVVertexFinder.h,v $
 * Revision 1.12  2012/12/12 22:09:58  fisyak
 * add sys/types.h include for APPLE
 *
 * Revision 1.11  2010/01/26 21:01:49  fisyak
 * Clean up, switch from bit mask to attributes
 *
 * Revision 1.10  2005/07/19 21:57:40  perev
 * MultiVertex
 *
 * Revision 1.9  2005/06/21 02:16:36  balewski
 * multiple prim vertices are stored in StEvent
 *
 * Revision 1.8  2004/09/03 00:09:08  jeromel
 * Modified code to Implement Init() and SetMode() and allow passing a switch
 * to chose the vertex finder from within the same code implementation. Was
 * needed for ppLMV (one implementation, two algorithm)
 *
 * Revision 1.7  2004/09/01 18:45:01  balewski
 * ppLMV5/4 switch added
 *
 * Revision 1.6  2004/08/05 22:08:04  balewski
 * toward working point
 *
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

