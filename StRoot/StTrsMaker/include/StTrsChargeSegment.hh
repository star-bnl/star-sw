/***************************************************************************
 *
 * $Id: StTrsChargeSegment.hh,v 1.3 1999/01/22 20:59:36 fisyak Exp $
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
 * Revision 1.3  1999/01/22 20:59:36  fisyak
 * remove iostream from private list
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
#include <iostream.h>
#include <list>

#include "SystemOfUnits.h"
#include "StGlobals.hh"
#include "StThreeVector.hh"
#include "StMatrix.hh"

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
		       double de,                  // energy deposited
		       double ds,                  // pathlength
		       int    pid = -1,            // pid #
		       float  ne  = -1);           // numberOfElectrons

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
    void  rotate(StTpcGeometry*, StTpcSlowControl*);
    float                  numberOfElectrons() const;
    
    void  rotate(StTpcGeometry*, StTpcSlowControl*, StTpcElectronics*);

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    void  split(StTrsDeDx*, StMagneticField*, int, list<StTrsMiniChargeSegment>*);
#else
        void  split(StTrsDeDx*, StMagneticField*, int, list<StTrsMiniChargeSegment, allocator<StTrsMiniChargeSegment> >*);
#endif

private:
    StThreeVector<double> mPosition;
    StThreeVector<double> mSector12Position;
    StThreeVector<double> mMomentum;
    double                mDE;
    double                mDs;
    int                   mPid;
    
    float                 mNumberOfElectrons;
    int                   mSectorOfOrigin;

    // NOTE: This should be a wrapped table so we don't have to copy...
    //       For now copy the vectors...
    //g2t_tpc_hit*  mG2tTpcHit ;
};
inline const StThreeVector<double>& StTrsChargeSegment::position() const {return mPosition;}
inline const StThreeVector<double>& StTrsChargeSegment::momentum() const {return mMomentum;}
ostream& operator<<(ostream&, StTrsChargeSegment&);
inline double StTrsChargeSegment::ds() const {return mDs;}
inline int StTrsChargeSegment::pid() const {return mPid;}
inline float StTrsChargeSegment::numberOfElectrons() const {return mDs;}

// Non-member Function
ostream& operator<<(ostream&, const StTrsChargeSegment&);
#endif
