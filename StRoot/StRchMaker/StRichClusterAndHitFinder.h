/***************************************************************************
 *
 * $Id: StRichClusterAndHitFinder.h,v 2.3 2000/11/07 14:12:47 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Definition of the Cluster Finder
 *  The work is done in:
 *
 ***************************************************************************
 *
 * $Log: StRichClusterAndHitFinder.h,v $
 * Revision 2.3  2000/11/07 14:12:47  lasiuk
 * init() information and
 * quadrant threshold cuts default is 200 ADC counts
 *
 * Revision 2.2  2000/09/29 19:04:43  lasiuk
 * hit calculation factorized to allow
 * deconvolution.  A flag was added to denote
 * this process (eDeconvolution).
 * MC info restored in classifyHit() member
 * cut parameters (for decon) added in initializeCutParameters()
 * startAmplitude set to 0.  This keeps track of the local
 * max of the hit now.
 *
 * Revision 2.1  2000/09/13 21:00:42  lasiuk
 * Begin modification for cluster/hit deconvolution
 * - remove matrix interface
 * - add necessary members
 * - unify cog calculation
 * - mapping code is not in place
 *
 * Revision 2.0  2000/08/09 16:22:12  gans
 * Cosmetic Changes. Naming convention for TDrawable objects
 *
 * Revision 1.4  2000/05/31 19:26:15  dunlop
 * Filling non-ctor entries in persistent hits + support for this
 *
 * Revision 1.3  2000/05/23 16:55:44  lasiuk
 * Incorporate new MC info
 * add clone() where necessary
 * accomodate name changes
 *
 * Revision 1.2  2000/05/18 11:42:28  lasiuk
 * mods for pre StEvent writing
 *
 * Revision 1.1  2000/04/05 16:39:30  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#ifndef ST_RICH_CLUSTER_AND_HOT_FINDER
#define ST_RICH_CLUSTER_AND_HOT_FINDER

#include <iostream.h>
#include <vector>
#include <stack>

#ifndef ST_NO_NAMESPACES
using std::vector;
using std::stack;
#endif

#include "StRrsMaker/StRichSinglePixel.h"
#include "StRrsMaker/StRichSingleMCPixel.h"
#include "StRrsMaker/StRichPadPlane.h"

#include "StRichSimpleCluster.h"
#include "StRichSimpleHit.h"
#include "StRichSimpleMCHit.h"
#include "StRichSinglePixelCollection.h"
#include "StRichHitInformation.h"

class StRrsReader;
class StRichGeometryDb;
class StRichCoordinateTransform;
#ifdef NEVER
class StRichReaderInterface;
#endif
// #ifndef ST_NO_TEMPLATE_DEF_ARGS
// typedef stack<StRichSinglePixel*> PixelStack;
// typedef vector<StRichCluster*>    ClusterVector;
// typedef vector<StRichHit*>        HitVector;
// #else
// typedef stack<StRichSinglePixel*, allocator<StRichSinglePixel*> > PixelStack;
// typedef vector<StRichCluster*, allocator<StRichSinglePixel*> >    ClusterVector;
// typedef vector<StRichHit*, allocator<StRichHit*> >                HitVector;
// #endif

enum StRichBoxType {eOdd, eEven};

class StRichClusterAndHitFinder {
public:
    StRichClusterAndHitFinder();
    StRichClusterAndHitFinder(unsigned int x, unsigned int y);
    ~StRichClusterAndHitFinder();

    void init();
    
    //StRichClusterAndHitFinder(const StRichClusterAndHitFinder&){/* use default*/}
    //StRichClusterAndHitFinder operator=(const StRichClusterAndHitFinder&){/* use default*/}

    void printList(ostream& os=cout);
    void printPadPlane(ostream& os=cout);

    //
    // Load the pixels by either passing:
    // a) the interface to access data
    // b) a collection (vector) of pixels
    // OR
#ifdef NEVER
    void loadPixels(StRichReaderInterface*);
#endif
    void loadPixels(vector<StRichSinglePixel>&);
    void loadPixels(vector<StRichSinglePixel*>&);
    
    void addSinglePixel(StRichSinglePixel*);
    void setBorderFlags();
    
    const StRichSinglePixelCollection& getPixels();
    const ClusterVector&               getClusters() const;
    const HitVector&                   getHits()     const;
    
    bool isSet(StRichSinglePixelFlag f)      const;

    //
    // Gain parameters
    void setQuadrantThreshold(int,int,int,int);
    void printQuadrantThreshold(ostream& os=cout)   const;
    
    //Main control!  calls functions below!
    bool makeTheClustersAndFilter(); 
    bool simpleHitsFromClusters();

    void calculateHitsInLocalCoordinates();
    void calculateHitsInGlobalCoordinates();
    
    void dumpClusterInformation(ostream& os=cout) const;
    void dumpHitInformation(ostream& os=cout)     const;

    void clearAndDestroyAll();

private:
    // fill cut parameters.  Should be from "Performance DB"
    void initializeCutParameters();
    //
    // pixel selection
    void calculateNeighborVector(int, vector<int>&, StRichBoxType=eOdd);
    bool constructNeighborMatrixFromVector(StRichSinglePixel*, vector<int>&,
					   vector<StRichSinglePixel*>*);
    
    bool constructSquareMatrix(StRichSinglePixel*, int, vector<StRichSinglePixel*>*);
    bool constructSquareMatrix(StRichSimpleCluster*, int, vector<StRichSinglePixel*>*);
    bool constructTheAdjacentNeighbors(StRichSinglePixel*, int, vector<StRichSinglePixel*>*);
    bool constructTheNearestNeighbors(StRichSinglePixel*, int, vector<StRichSinglePixel*>*);

    //
    // centroid determination
    bool centerOfGravity(vector<StRichSinglePixel*>&, StRichHitInformation*);

    //
    // Topology routines
    size_t findTheLocalMaximaInCluster(StRichSimpleCluster*, vector<StRichSinglePixel*>&);
    
    bool fillHitInformation(StRichHitInformation&);

    //
    // Make a hit based on presence of MC pixels or not
    bool classifyHitType(vector<StRichSinglePixel*>&);

    
    bool makeClusters(double minAmp);
    bool useTheMovingMatrix(StRichSinglePixel*, int, StRichHitInformation*);

    //void removeNoiseClusters();
    //bool filterClusters();
    //bool splitClusters();
    //bool removeSmallClusters();
    //bool removeBigClusters();
    void clearAndDestroyThePixels();
    void clearAndDestroyTheClusters();
    void clearAndDestroyTheHits();
    
private:
    StRichGeometryDb*            mGeometryDb;
    StRichCoordinateTransform*   mTransform;
    
    // Dimensions X x Y
    int mX;
    int mY;

    // Pixel collection
    StRichSinglePixelCollection  mThePixels;

    // cut parameters (loaded from "Performance DB")
    int mMaxSaturatedPads;
    int mMaxClusterLength;
    int mMaxClusterWidth;
    float mMaxAspectRatio; // length/width
    float mMinChargeFraction;  // fraction of charge central pad possess

    double mQuadrantThresholdCharge[5][2];
    int    mGainVoltage;
    
    // tmp storage for comparison to cuts
    int mNumberOfSaturatedPads;
    int mClusterLength;
    int mClusterWidth;
    
    // the output: clusters and hits
    ClusterVector                mTheClusters;
    
    //output
    HitVector                    mTheHits;
    unsigned short               mAnMCHit;
    anIDList                     mMCInfo;
};

inline const StRichSinglePixelCollection& StRichClusterAndHitFinder::getPixels()
{ return mThePixels; }
inline const ClusterVector& StRichClusterAndHitFinder::getClusters() const
{ return mTheClusters; }   
inline const HitVector& StRichClusterAndHitFinder::getHits() const
{ return mTheHits;}

#endif
