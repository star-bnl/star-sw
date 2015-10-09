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
 *  Modified for multiple veretx finding by Marco van Leeuwen April/May 2006
 *
 *  Vertexfinding process has been split into two stages: a seedfinding and a 
 *  fitting stage. Multiple vertices can be found for each event and are 
 *  sorted by 'ranking'. The highets ranked vertex is most likely the 
 *  triggered vertex.
 *
 *  Vertex rank calculation is done in calculateRanks(). Three ranks are
 *  calculated, based on mean-dip, number of tracks crossing central membrane
 *  and number of tracks matched to BEMC. The ranks are normalised to have
 *  mean value close to 0 for triggered vertices and a sigma of about 1 
 *  (independent of multiplicity). Each individual rank is bounded to [-5,1].
 *  For data analysis, a quality cut on the rank of the best vertex is 
 *  recommended. This needs to be refined, but requiring rank > -3 seems to
 *  work OK.
 *
 *  for more info, see: http://www.star.bnl.gov/protected/highpt/mvl/multi_vertex/
 *
 * Member Functions:
 * -----------------
 * StMinuitVertexFinder::fit(StEvent* evt)
 * Find and fit the vertex for given event.
 *
 * StThreeVectorD StMinuitVertexFinder::result()
 * Returns the found vertex.
 *
 * Int_t StMinuitVertexFinder::status()
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
 * void StMinuitVertexFinder::setPrintLevel(Int_t level);
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
 *  $Id: StMinuitVertexFinder.h,v 1.13 2015/10/09 18:54:02 genevb Exp $
 *
 */


#include <vector>
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "StGenericVertexFinder.h"

class StEvent;
class StTrack;
class TMinuit;
class StDcaGeometry;
class StMinuitVertexFinder: public StGenericVertexFinder {
public:
    StMinuitVertexFinder();

    // mandatory implementations
    virtual        ~StMinuitVertexFinder();
    Int_t           fit(StEvent*);       
    void            printInfo(ostream& = cout) const;
    void            UseVertexConstraint(Double_t x0, Double_t y0, Double_t dxdz, Double_t dydz, Double_t weight);
    virtual void    InitRun  (Int_t runumber);
    void            Clear();


    // Added, not part of base-class  and used by the Minuit vertex finder
    Int_t           NCtbMatches();    // returns the number of tracks matched to CTB                                                               
    Int_t           NCtbSlats();   // returns the number of CTB slats above threshold
    void            CTBforSeed(){   mRequireCTB = kTRUE;}
    void            NoCTBforSeed(){ mRequireCTB = kFALSE;}
    void            setExternalSeed(const StThreeVectorD&);

    void            setPrintLevel(Int_t = 0);
    Int_t           statusMin() const {return mStatusMin;}     // Minuit status flag
    void            DoUseITTF(){    mUseITTF = kTRUE; }
    void            DoNotUseITTF(){ mUseITTF = kFALSE;}
    void            useOldBEMCRank() { mUseOldBEMCRank = kTRUE; }
    void            lowerSplitVtxRank() { mLowerSplitVtxRank = kTRUE; }
    void            setFlagBase();
    void            SetFitPointsCut(Int_t fitpoints) {mMinNumberOfFitPointsOnTrack = fitpoints;}
    void            SetMinimumTracks(Int_t n) {mMinTrack = n;}

private:
    enum  {kFlagDcaz = 1, kFlagCTBMatch = 2, kFlagBEMCMatch = 4, kFlagCrossMembrane = 8};

    bool    accept(StTrack*) const;   // track filter
    void    fillBemcHits(StEvent *);
    Int_t   matchTrack2BEMC(const StTrack *);
    Int_t   checkCrossMembrane(const StTrack *);
    void    calculateRanks();
    Int_t   findSeeds();

    static void fcn(Int_t&, Double_t*, Double_t&, Double_t*, Int_t); // fit function
    static void fcn1D(Int_t&, Double_t*, Double_t&, Double_t*, Int_t); // fit function
    static Double_t Chi2atVertex(StThreeVectorD &vtx);
    
    bool                   mUseITTF;          // Use only tracks with ITTF encoded method
    bool                   mUseOldBEMCRank;   // Use old BEMC rank calculation (Cu+Cu production)
    bool                   mLowerSplitVtxRank;// Use lower rank for split vertices
    UInt_t                 mFlagBase;         // ITTF track flag
    bool                   mRequireCTB;       // Set maker to use CTB
    UInt_t                 mMinNumberOfFitPointsOnTrack;
    Float_t                mDcaZMax;
    Double_t               mWeight ;          // Weight in fit for vertex contraint
    Double_t               mRImpactMax;       // Max distance between helix and nominal beamline (0,0,z)
    Int_t                  mMinTrack;         // Min number of tracks
    Float_t                mZMin;             // Min z of possible vertex positions
    Float_t                mZMax;             // Max z of possible vertex positions

    StPhysicalHelixD*      mBeamHelix;        // Beam Line helix
    
    enum                   {maxSeed=500};
    Int_t                  mNSeed;
    Float_t                mSeedZ[maxSeed];
    Int_t                  mBemcHit[120][20][2];  // modules, eta, sub
    static vector<StDcaGeometry*>   mDCAs;
    static vector<StPhysicalHelixD> mHelices;
    static vector<UShort_t>         mHelixFlags;
    static vector<Double_t>         mSigma;
    static vector<Double_t>         mZImpact;
    //static vector<Bool_t>         mCTB;
    static Bool_t                   requireCTB;
    static Int_t                    nCTBHits;
    static Double_t                 mWidthScale;
    static Double_t                 mX0  ; // starting point of beam parameterization
    static Double_t                 mY0  ; // starting point of beam parameterization
    static Double_t                 mdxdz; // beam slope
    static Double_t                 mdydz; // beam slope
    static Double_t beamX(Double_t z); // beamline parameterization
    static Double_t beamY(Double_t z); // beamline parameterization
    Int_t                    mStatusMin;           // Minuit status flag 
    StThreeVectorD           mExternalSeed;
    Bool_t                   mExternalSeedPresent;
    StPrimaryVertex         *mBestVtx;    // pointer to best vertex of this event
    Float_t                  mBestRank;   // store rank of best vertex
    Float_t                  mCTBSum;
    TMinuit*                 mMinuit;
};
/***************************************************************************
 *
 * $Log: StMinuitVertexFinder.h,v $
 * Revision 1.13  2015/10/09 18:54:02  genevb
 * Use new vertex-finding parameters ZMin,ZMax
 *
 * Revision 1.12  2010/12/06 20:07:23  fisyak
 * Increase maximum number of possible seeds
 *
 * Revision 1.11  2010/01/26 21:01:49  fisyak
 * Clean up, switch from bit mask to attributes
 *
 * Revision 1.10  2008/07/31 18:11:10  genevb
 * VFMinuit3 chain option for lower ranking of split vertices
 *
 * Revision 1.9  2008/04/12 10:53:20  mvl
 * Changed calculation of BEMC matches based ranking to fix problems with run-7 Au+Au.
 * See also: http://www.star.bnl.gov/protected/highpt/mvl/multi_vertex/update_R7.html
 * Old calculation can be selected with UseOldBEMCRank()
 *
 * Revision 1.8  2007/10/23 05:29:44  genevb
 * Replace minimum 1 track vertex code with minimum N tracks
 *
 * Revision 1.7  2007/05/17 01:50:35  fisyak
 * Use PrimaryVertexCuts table
 *
 * Revision 1.6  2006/05/31 04:09:52  fisyak
 * Use dca track parameters for primary vertex fit
 *
 * Revision 1.5  2006/05/10 17:16:21  mvl
 * Added some comments to describe changes for multiple veretx finding (most importantly: ranking system)
 *
 * Revision 1.4  2006/05/09 17:51:05  mvl
 * Added protection against event->emcCollection()==0
 *
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
