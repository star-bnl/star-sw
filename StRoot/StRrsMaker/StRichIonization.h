/***********************************************************************
 *  $Id: StRichIonization.h,v 1.7 2000/04/05 16:00:42 lasiuk Exp $
 *
 * Description:
 *   StRichIonization function object contains ionization algorithm
 *   for a particle track. 
 *   
 *   StRichIonization.h is used like a normal function, i.e. StRichIonization(GHit);
 *   The algorithm used is the following:
 *   StRichIonization simulates the charged particle track through the gas.
 *   The main parameters of the process are the average number of
 *   ionization clusters per length unit and the distribution of
 *   ionized electrons in each of those clusters.
 *
 ***************************************************************************
 * $Log: StRichIonization.h,v $
 * Revision 1.7  2000/04/05 16:00:42  lasiuk
 * viewer, gid, update
 *
 * Revision 1.6  2000/03/17 14:54:46  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.5  2000/03/12 23:56:34  lasiuk
 * new coordinate system
 * exchange MyRound with inline templated funtion
 *
 * Revision 1.4  2000/02/12 00:38:29  lasiuk
 * rename probability (max)
 *
 * Revision 1.3  2000/02/08 16:27:36  lasiuk
 * change to class.  Put dbs and random generators into
 * class as data members
 *
 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
 *
 *   revision history:
 *     - 7/21/1999 created the struct,        Alexandre Nevski.
 *     - 7/27/1999 updated operator() to void. results 
 *         returned by reference,             Alexandre Nevski.
 *     - 8/10/1999 main implementation                   C & A.
 *
 ****************************************************************************/
#ifndef ST_RICH_IONIZATION_H
#define ST_RICH_IONIZATION_H


#include <list>
#ifndef ST_NO_NAMESPACES
using std::list;
#endif

#include "StRichRrsMacros.h"    
#include "StRichGHit.h"
#include "StRichMiniHit.h"
#include "StRichOtherAlgorithms.h"
class StRichPhysicsDb;

class StRichIonization {
public:
    StRichIonization();
    ~StRichIonization();

    //StRichIonization(const StRichIonization&) {/* use default */}
    //StRichIonization& operator=(const StRichIonization&) {/* use default */}
	
    void splitSegment(const StRichGHit*, list<StRichMiniHit*>&) const;
    double betheBloch(double bg);
    
private:
    StRichPhysicsDb* mPhysicsDb;
    Randoms          mRandom;
    
    double           mAverageNumberOfInteractions;
    double           mMaximumElectronEnergyProbability;

    //
    //  parameters for bethe-bloch parameterization
    //
    // mKonstant = constant in Bethe-Bloch curve formalism
    // mAlfat    = (alpha)(t)
    // mZa       = Z/A for CH4
    // mIonize   = ionization potential for CH4
    // mDensity  = density of CH4
    // mSaturationValue = low enery cutoff
    //                    in 1/beta**2 part of the curve
    //
    // *** SEE paper for description
    // mFirstCorner = x0
    // mPlateau     = x1
    // mShapeOfRise = m
    // interSectionOfRiseAndPlateau = xa
    // bigx           = log(bg) in base 10
    // saturationTerm = d
    
    double mKonstant;
    double mZa;
    double mAlfat;
    double mIonize;
    double mDensity;
    double mSaturationValue;

    double mFirstCorner;
    double mPlateau;
    double mShapeOfRise;
    double mIntersectionOfRiseAndPlateau;
    double mF;
};

#endif
