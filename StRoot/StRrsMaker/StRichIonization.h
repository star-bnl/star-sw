/***********************************************************************
 *  $Id: StRichIonization.h,v 1.1 2000/01/18 21:32:02 lasiuk Exp $
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
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
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
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
 *
 ****************************************************************************/
#include <functional>
    
using std::unary_function;
#include <list>
#ifndef ST_NO_NAMESPACES
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {

    struct StRichIonization : public unary_function<StRichGHit,void> {
	void operator()( const StRichGHit& );
    };

    StRichPhysicsDb* mPhysicsDb;
    Randoms          mRandom;
    double mShapeOfRise;

#ifndef ST_NO_NAMESPACES
//} 
#endif
    double mIntersectionOfRiseAndPlateau;
    double mF;
};

#endif
