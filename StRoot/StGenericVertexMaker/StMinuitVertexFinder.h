/***************************************************************************
 *
 * $Id: StMinuitVertexFinder.h,v 1.1 2002/12/05 23:42:46 hardtke Exp $
 *
 * Author: Thomas Ullrich, Feb 2002
 * Modified for pp by David Hardtke, Summer 2002
 ***************************************************************************
 *
 * Description:
 * StEvent based vertex fitter using a robust potential.
 * The actual fit is performed by MINUIT (TMinuit).
 * For documentation the following links and documents
 * are very useful:
 * http://wwwinfo.cern.ch/asdoc/minuit/minmain.html
 * http://root.cern.ch/root/html/TMinuit.html
 * http://www-glast.slac.stanford.edu/software/root/GRUG/docs/Feature/GRUGminuit.pdf
 *
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
 * We also have the option of requiring that at least one track matches
 * the CTB during the first two scans for the vertex (these are coarse scans
 * to locate the probable z vertex
 *
 * myvertex.CTBforSeed();
 *
 * During the final fit (once the z position of the vertex has been constrained)
 * there is no CTB requirement.  To get the number of tracks with CTB match:
 *
 *  myvertex.NCtbMatches()
 *  
 ***************************************************************************
 *
 * $Log: StMinuitVertexFinder.h,v $
 * Revision 1.1  2002/12/05 23:42:46  hardtke
 * Initial Version for development and integration
 *
 **************************************************************************/
#include <vector>
//#include "/afs/rhic/star/packages/DEV/include/StThreeVectorD.hh"
//#include "/afs/rhic/star/packages/DEV/include/StPhysicalHelixD.hh"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
class StEvent;
class StTrack;
class TMinuit;

class StMinuitVertexFinder {
public:
    StMinuitVertexFinder();
    ~StMinuitVertexFinder();

    bool            fit(StEvent*);       // fit the vertex
    StThreeVectorD  result() const;      // result of fit
    int             status() const;      // error and status flag

    void            setExternalSeed(const StThreeVectorD&);
    void            setPrintLevel(int = 0);
    void            printInfo(ostream& = cout) const;
    void            UseVertexConstraint(double x0, double y0, double dxdz, double dydz, double weight);
    void            NoVertexConstraint();
    void            CTBforSeed();
    void            NoCTBforSeed();
    int             NCtbMatches();
    bool            use_ITTF;    //Use only tracks with ITTF encoded method
    void            SetFitPointsCut(int fitpoints);

private:
    bool accept(StTrack*) const;   // track filter
    static void fcn(int&, double*, double&, double*, int); // fit function
    static void fcn1D(int&, double*, double&, double*, int); // fit function
    
private:
    static vector<StPhysicalHelixD> mHelices;
    static vector<double>           mSigma;
    static vector<bool>             mCTB;
    static bool                     requireCTB;
    static int                      nCTBHits;
    static double                   mWidthScale;
    static double                   mX0  ; // starting point of beam parameterization
    static double                   mY0  ; // starting point of beam parameterization
    static double                   mdxdz; // beam slope
    static double                   mdydz; // beam slope
    static double beamX(double z); // beamline parameterization
    static double beamY(double z); // beamline parameterization

    
    TMinuit*                 mMinuit;
    StThreeVectorD           mFitResult;
    unsigned int             mMinNumberOfFitPointsOnTrack;
    bool                     mExternalSeedPresent;
    StThreeVectorD           mExternalSeed;
    double                   mFmin;       // best function value found
    double                   mFedm;       // estimated vertical distance remaining to minimum
    double                   mErrdef;     // value of UP defining parameter uncertainty
    int                      mNpari;      // number of variable parameters
    int                      mNparx;      // highest parameter number defined
    int                      mStatus;     // status flag 
    bool                     mVertexConstrain; // Use vertex constraint from db
    bool                     mRequireCTB; // require CTB for seed
    double                   mWeight ; // Weight in fit for vertex contraint
    StPhysicalHelixD*        mBeamHelix ; // Beam Line helix
};




