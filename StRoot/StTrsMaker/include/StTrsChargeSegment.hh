/***************************************************************************
 *
 * $Id: StTrsChargeSegment.hh,v 1.12 2012/06/11 15:04:55 fisyak Exp $
 *
 * Author: brian May 18, 1998
 *
 ***************************************************************************
 *
 * Description: Wrapper for the g2t structures with added functionality
 *
 ***************************************************************************
 *
 * $Log: StTrsChargeSegment.hh,v $
 * Revision 1.12  2012/06/11 15:04:55  fisyak
 * std namespace
 *
 * Revision 1.11  2011/01/18 14:40:15  fisyak
 * Clean up TpcDb interfaces and Tpc coordinate transformation
 *
 * Revision 1.10  2003/12/24 13:44:51  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.9  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.8  2000/01/10 23:11:31  lasiuk
 * Include MACROS for compatibility with SUN CC5.0
 *
 * Revision 1.7  1999/07/20 02:17:51  lasiuk
 * remove CVS merge conflicts
 *
 * Revision 1.6  1999/07/19 21:37:59  lasiuk
 * - tssSplit() and associated parameterizations from
 *   tss are included (requires linking with cernlib)
 * - introduce static random number generators
 * - whichGEANTParticle() introduced for g2t input
 *
 * Revision 1.5  1999/02/18 21:18:43  lasiuk
 * rotate() mods to StTpcCoordinateTranform
 *
 * Revision 1.4  1999/02/10 18:02:09  lasiuk
 * ostream (const)
 *
 * Revision 1.3  1999/01/22 20:59:36  fisyak
 * remove iostream from private list
 *
 * Revision 1.2  1999/01/15 11:02:26  lasiuk
 * remove g2t pointer
 * add pid member; add systemofunits; mv access fcts to .hh
 * add ostream operator
 *
 * Revision 1.1  1998/11/10 17:12:09  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.5  1998/11/08 17:04:35  lasiuk
 * use built in types,
 * namespace macro
 * vector allocators
 *
 * Revision 1.4  1998/10/22 00:23:19  lasiuk
 * Oct 22
 *
 * Revision 1.3  1998/06/04 23:15:07  lasiuk
 * remove transportToWire(); add gas and magfield db;
 * **Still requires work on the saving of the g2t as a pointer**
 *
 * Revision 1.2  1998/05/21 21:27:39  lasiuk
 * Initial revision
 *
 * Revision 1.1.1.1  1998/05/19 22:33:44  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_CHARGE_SEGMENT_HH
#define ST_TRS_CHARGE_SEGMENT_HH

#include <Stiostream.h>
#include <list>

using std::list;

#include "SystemOfUnits.h"
#include "StGlobals.hh"
#include "StThreeVector.hh"
#include "StMatrix.hh"
#include "Randomize.h"

//#include "g2t_tpc_hit.hh"
#include "StTpcSlowControl.hh"
#include "StTpcGeometry.hh"
#include "StTpcElectronics.hh"
#include "StMagneticField.hh"
#include "StTrsDeDx.hh"
#include "StTrsMiniChargeSegment.hh"

class StTrsChargeSegment {

public:
    StTrsChargeSegment();
    StTrsChargeSegment(StThreeVector<double>& pos,
		       StThreeVector<double>& mom,
		       int    id,                  // geant track id
		       double de,                  // energy deposited
		       double ds,                  // pathlength
		       int    pid = -1,            // pid #
		       double ne  = -1);           // numberOfElectrons

    ~StTrsChargeSegment();
    //StTrsChargeSegment(const StTrsChargeSegment&);
    //StTrsChargeSegment& operator=(cont StTrsChargeSegment&);

    //
    // access functions
    //
    const StThreeVector<double>& position()          const;
    const StThreeVector<double>& momentum()          const;
    double                 dE()                const;
    double                 ds()                const;
    int                    id()                const; 
    int                    pid()               const;
    double                 numberOfElectrons() const;
    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    void  split(StTrsDeDx*, StMagneticField*, int, list<StTrsMiniChargeSegment>*);
    void  tssSplit(StTrsDeDx*, StMagneticField*, int, list<StTrsMiniChargeSegment>*);
#else
    void  split(StTrsDeDx*, StMagneticField*, int, list<StTrsMiniChargeSegment, allocator<StTrsMiniChargeSegment> >*);
    void  tssSplit(StTrsDeDx*, StMagneticField*, int, list<StTrsMiniChargeSegment, allocator<StTrsMiniChargeSegment> >*);
#endif

public:
    // TSS segment splitting functions
    double sigmaParameter(double, double, double, int) const;
    double meanParameter(double,double, double, int)   const;
    double xReflectedGauss(double, double)             const;
    double xReflectedLandau(double, double)            const;
    double binaryPartition(double, double, double)     const;

    

private:
    void whichGEANTParticle(double&, int&);
    
private:
    StThreeVector<double> mPosition;
    StThreeVector<double> mSector12Position;
    StThreeVector<double> mMomentum;
    int                   mId;
    double                mDE;
    double                mDs;
    int                   mPid;
    
    double                mNumberOfElectrons;
    int                   mSectorOfOrigin;

    static HepJamesRandom  mEngine;
    static RandFlat        mFlatDistribution;
};
inline const StThreeVector<double>& StTrsChargeSegment::position() const {return mPosition;}
inline const StThreeVector<double>& StTrsChargeSegment::momentum() const {return mMomentum;}
inline double StTrsChargeSegment::dE() const {return mDE;}
inline double StTrsChargeSegment::ds() const {return mDs;}
inline int StTrsChargeSegment::id()    const {return mId;}
inline int StTrsChargeSegment::pid() const {return mPid;}
inline double StTrsChargeSegment::numberOfElectrons() const {return mNumberOfElectrons;}

// Non-member Function
ostream& operator<<(ostream&, const StTrsChargeSegment&);
#endif
