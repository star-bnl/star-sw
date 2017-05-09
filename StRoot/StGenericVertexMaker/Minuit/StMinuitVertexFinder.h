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
 * vertex.
 *
 *  $Id: StMinuitVertexFinder.h,v 1.30 2017/05/09 12:29:40 smirnovd Exp $
 *
 */


#include <vector>

#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "StGenericVertexFinder.h"

class StEvent;
class StTrack;
class TMinuit;


class StMinuitVertexFinder: public StGenericVertexFinder
{
public:

    StMinuitVertexFinder(VertexFit_t fitMode=VertexFit_t::NoBeamline);

    // mandatory implementations
    virtual        ~StMinuitVertexFinder();
    Int_t           fit(StEvent*);       
    void            printInfo(ostream& = cout) const;
    virtual void    InitRun(int run_number, const St_db_Maker* db_maker);
    void            Clear();


    // Added, not part of base-class  and used by the Minuit vertex finder
    Int_t           NCtbMatches();    // returns the number of tracks matched to CTB                                                               
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

    virtual void UseVertexConstraint() {}

    virtual double CalcChi2DCAs(const StThreeVectorD &vtx);
    
    bool                   mUseITTF;          // Use only tracks with ITTF encoded method
    bool                   mUseOldBEMCRank;   // Use old BEMC rank calculation (Cu+Cu production)
    bool                   mLowerSplitVtxRank;// Use lower rank for split vertices
    UInt_t                 mFlagBase;         // ITTF track flag
    bool                   mRequireCTB;       // Set maker to use CTB
    UInt_t                 mMinNumberOfFitPointsOnTrack;
    Float_t                mDcaZMax;
    Double_t               mRImpactMax;       // Max distance between helix and nominal beamline (0,0,z)
    Int_t                  mMinTrack;         // Min number of tracks
    Float_t                mZMin;             // Min z of possible vertex positions
    Float_t                mZMax;             // Max z of possible vertex positions

    enum                   {maxSeed=500};

    /// The number of vertex seeds found in current event
    Int_t  mNSeed;

    Float_t                mSeedZ[maxSeed];
    Int_t                  mBemcHit[120][20][2];  // modules, eta, sub
    static std::vector<StPhysicalHelixD> mHelices;
    static std::vector<UShort_t>         mHelixFlags;
    static std::vector<Double_t>         mZImpact;
    static Bool_t                   requireCTB;
    static Int_t                    nCTBHits;
    Int_t                    mStatusMin;           // Minuit status flag 
    StThreeVectorD           mExternalSeed;
    Bool_t                   mExternalSeedPresent;
    StPrimaryVertex         *mBestVtx;    // pointer to best vertex of this event
    Float_t                  mBestRank;   // store rank of best vertex
    Float_t                  mCTBSum;
};
