/***************************************************************************
 *
 * $Id: StTrsChargeSegment.hh,v 1.1 1998/11/10 17:12:09 fisyak Exp $
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
 * Revision 1.1  1998/11/10 17:12:09  fisyak
 * Put Brian trs versin into StRoot
 *
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

 * Initial Revision
 *
#ifndef ST_TRS_CHARGE_SEGMENT_HH
#define ST_TRS_CHARGE_SEGMENT_HH
#include <iostream.h>
#include <list>
#include "g2t_tpc_hit.hh"
#include "SystemOfUnits.h"
#include "StGlobals.hh"
#include "StThreeVector.hh"
#include "StMatrix.hh"

#include "StTpcSlowControl.hh"
#include "StTpcGeometry.hh"
#include "StTpcElectronics.hh"
#include "StMagneticField.hh"
#include "StTrsDeDx.hh"
    StTrsChargeSegment(StThreeVector<double>&, StThreeVector<double>&, g2t_tpc_hit*);
    StTrsChargeSegment(StThreeVector<double>& pos,
		       StThreeVector<double>& mom,
		       double de,                  // energy deposited
		       double ds,                  // pathlength
		       int    pid = -1,            // pid #
		       float  ne  = -1);           // numberOfElectrons

    ~StTrsChargeSegment();
    StThreeVector<double>& position();
    StThreeVector<double>& momentum();
    double                 dE();
    double                 ds();
    const StThreeVector<double>& position()          const;
    const StThreeVector<double>& momentum()          const;
    double                 dE()                const;
    double                 ds()                const;
    void  split(StTrsDeDx*, StMagneticField*, int, double, list<StTrsMiniChargeSegment>*);
    float                  numberOfElectrons() const;
        void  split(StTrsDeDx*, StMagneticField*, int, double, list<StTrsMiniChargeSegment, allocator<StTrsMiniChargeSegment> >*);
    void  rotate(StTpcGeometry*, StTpcSlowControl*, StTpcElectronics*);

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    // NOTE: This should be a wrapped table so we don't have to copy...
    //       For now copy the vectors...
    g2t_tpc_hit*  mG2tTpcHit ;
    void  split(StTrsDeDx*, StMagneticField*, int, list<StTrsMiniChargeSegment>*);
#else
        void  split(StTrsDeDx*, StMagneticField*, int, list<StTrsMiniChargeSegment, allocator<StTrsMiniChargeSegment> >*);
#endif

    StThreeVector<double> mPosition;
    StThreeVector<double> mSector12Position;
    StThreeVector<double> mMomentum;
    double                mDE;
    float                 mNumberOfElectrons;
inline const StThreeVector<double>& StTrsChargeSegment::position() const {return mPosition;}
inline double StTrsChargeSegment::ds() const {return mDs;}
inline int StTrsChargeSegment::pid() const {return mPid;}
inline float StTrsChargeSegment::numberOfElectrons() const {return mDs;}

// Non-member Function
ostream& operator<<(ostream&, const StTrsChargeSegment&);
#endif
