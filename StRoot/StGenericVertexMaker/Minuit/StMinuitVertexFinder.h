/*!
 * \class StMinuitVertexFinder
 * \author Thomas Ullrich, Feb 2002
 *
 *  Modified for pp by David Hardtke, Summer 2002
 *
 *  StEvent based vertex fitter using a robust potential.
 *  The actual fit is performed by MINUIT (TMinuit).
 *  For documentation the following links and documents
 *  are very useful:
 *  http://wwwinfo.cern.ch/asdoc/minuit/minmain.html
 *  http://root.cern.ch/root/html/TMinuit.html
 *  http://www-glast.slac.stanford.edu/software/root/GRUG/docs/Feature/GRUGminuit.pdf
 *
 * Member Functions:
 * -----------------
 * StMinuitVertexFinder::fit(StEvent* evt)
 * Find and fit the vertex for given event.
 *
 * StThreeVectorD StMinuitVertexFinder::result()
 * Returns the found vertex.
 *
 * int StMinuitVertexFinder::status()
 * The meaning of the return values of status() is as follows:
 *   -1 = not enough good tracks for fit
 *        in this case fit() returns false.
 *   All other values are related to the actual fit
 *   and reflect the status of the covariant matrix
 *   and thus the quality of the fit.
 *   (See also MNSTAT in Minuit documentation)
 *   0 = not calculated at all
 *   1 = diagonal approximation only
 *   2 = full matrix, but forced positive-definite
 *   3 = full accurate covariant matrix
 *
 * void StMinuitVertexFinder::setExternalSeed(const StThreeVectorD& seed);
 * If the seed is known, e.g. from pVPD, ZDC, or BBC, the estimated
 * position can be passed to the fitter. In this case the fit performs
 * faster, but not necessarily more accurate.
 * The seed has to be provided for every fit (fit()). It will only
 * be used for the next fit.
 *
 * void StMinuitVertexFinder::setPrintLevel(int level);
 * Set Minuit print level: 0-3
 * 0 means essentially no output
 * 3 prints a lot, for debugging only
 * 1 current default level
 *
 * void StMinuitVertexFinder::printInfo([ostream& os]);
 * Prints information of the last fit to output stream os.
 * If no argument is given the info is printed to cout.
 *
 * Example code:
 *
 * StEvent *event = ...;
 * StMinuitVertexFinder myfinder;
 * StThreeVectorD myvertex;
 * if (myfinder.fit(event)) {
 *     myvertex = myfinder.result();
 *     myfinder.printInfo();
 * }
 * else
 *     cout << "Error: vertex fit failed, no vertex." << endl;
 *
 * PP vertex finding:
 * For proton-proton (and presumable dAu) vertex finding, we only do a 
 * 1D fit and use the beamline constraint to get the x and y positions of the 
 * vertex.  To enable this mode, use:
 *
 *  myvertex.UseVertexConstraint(x0,y0,dzdy,dydz,weight)
 *
 *
 *  $Id: StMinuitVertexFinder.h,v 1.3 2006/04/25 13:06:44 mvl Exp $
 *
 */


#include <vector>
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "StGenericVertexFinder.h"

class StEvent;
class StTrack;
class TMinuit;
class StEmcDetector;

class StMinuitVertexFinder: public StGenericVertexFinder {
public:
    StMinuitVertexFinder();

    // mandatory implementations
    virtual         ~StMinuitVertexFinder();
    int             fit(StEvent*);       
    void            printInfo(ostream& = cout) const;
    void            UseVertexConstraint(double x0, double y0, double dxdz, double dydz, double weight);
    void           Clear();


    // Added, not part of base-class  and used by the Minuit vertex finder
    int            NCtbMatches();    // returns the number of tracks matched to CTB                                                               
    int            NCtbSlats();   // returns the number of CTB slats above threshold
    void                   CTBforSeed(){   mRequireCTB = true;}
    void                   NoCTBforSeed(){ mRequireCTB = false;}
    void                   setExternalSeed(const StThreeVectorD&);

    void            setPrintLevel(int = 0);
    int            statusMin() const {return mStatusMin;}     // Minuit status flag
    void                   DoUseITTF(){    mUseITTF=kTRUE; }
    void                   DoNotUseITTF(){ mUseITTF=kFALSE;}
    void                   setFlagBase();
    void                   SetFitPointsCut(int fitpoints) {mMinNumberOfFitPointsOnTrack = fitpoints;}

private:
    enum {kFlagDcaz = 1, kFlagCTBMatch = 2, kFlagBEMCMatch = 4, kFlagCrossMembrane = 8};

    bool accept(StTrack*) const;   // track filter
    void  fillBemcHits(StEmcDetector *);
    int   matchTrack2BEMC(const StTrack *);
    int   checkCrossMembrane(const StTrack *);
    void  calculateRanks();
    int   findSeeds();

    static void fcn(int&, double*, double&, double*, int); // fit function
    static void fcn1D(int&, double*, double&, double*, int); // fit function
    
    bool                   mUseITTF;          // Use only tracks with ITTF encoded method
    UInt_t                 mFlagBase;         // ITTF track flag
    bool                   mRequireCTB;       // Set maker to use CTB
    unsigned int           mMinNumberOfFitPointsOnTrack;
    float                  mDcaZMax;
    double                 mWeight ;          // Weight in fit for vertex contraint
    double                 mRImpactMax;       // Max distance between helix and nominal beamline (0,0,z)
    int                    mMinTrack;         // Min number of tracks

    StPhysicalHelixD*      mBeamHelix;        // Beam Line helix
    
    enum                   {maxSeed=20};
    Int_t                  mNSeed;
    Float_t                mSeedZ[maxSeed];
    Int_t                  mBemcHit[120][20][2];  // modules, eta, sub
    static vector<StPhysicalHelixD> mHelices;
    static vector<unsigned short>   mHelixFlags;
    static vector<double>           mSigma;
    static vector<double>           mZImpact;
    //static vector<bool>             mCTB;
    static bool                     requireCTB;
    static int                      nCTBHits;
    static double                   mWidthScale;
    static double                   mX0  ; // starting point of beam parameterization
    static double                   mY0  ; // starting point of beam parameterization
    static double                   mdxdz; // beam slope
    static double                   mdydz; // beam slope
    static double beamX(double z); // beamline parameterization
    static double beamY(double z); // beamline parameterization
    
    int                    mStatusMin;           // Minuit status flag 
    StThreeVectorD         mExternalSeed;
    bool                   mExternalSeedPresent;

    StPrimaryVertex       *mBestVtx;    // pointer to best vertex of this event
    float                  mBestRank;   // store rank of best vertex

    TMinuit*               mMinuit;
};




/***************************************************************************
 *
 * $Log: StMinuitVertexFinder.h,v $
 * Revision 1.3  2006/04/25 13:06:44  mvl
 * Seed-finding range extended to -200<vtx_z<200
 *
 * Revision 1.2  2006/04/08 19:06:29  mvl
 * Update for multiple vertex finding and rank calculation for identifying the
 * triggered vertex. Ranks are based on mean dip angle of tracks, BEMC matches
 * and tracks crossing the central membrane and optimised for Cu+Cu.
 * The track cuts are now bit tighter (dca<2 in transverse direction and
 * nfitpoints > 15) to suppress 'fake' vertices.
 * In addition, a lower multiplicity cut of 5 tracks is implemented.
 *
 * Revision 1.9  2005/07/19 21:53:27  perev
 * MultiVertex
 *
 * Revision 1.8  2005/06/21 02:16:36  balewski
 * multiple prim vertices are stored in StEvent
 *
 * Revision 1.7  2004/08/04 21:57:56  balewski
 * toward smarter ppLMV5
 *
 * Revision 1.6  2004/07/23 02:24:39  jeromel
 * Oops ... Worng swithc (had twice Minuit). Now corrected.
 *
 * Revision 1.5  2004/07/23 00:59:36  jeromel
 * Removed methods (moved in base class) + doxygenized
 *
 * Revision 1.4  2004/04/06 02:43:43  lbarnby
 * Fixed identification of bad seeds (no z~0 problem now). Better flagging. Message manager used.
 *
 * Revision 1.3  2003/05/12 21:10:06  lbarnby
 * Made destructor virtual
 *
 * Revision 1.2  2003/05/09 22:19:51  lbarnby
 * Now also calculates and reports error on vertex. Corrected filter to use ITTF tracks. Some temporary protections against inf/Nan. Skip delete of TMinuit class since causing seg. fault.
 *
 * Revision 1.1  2002/12/05 23:42:46  hardtke
 * Initial Version for development and integration
 *
 **************************************************************************/
