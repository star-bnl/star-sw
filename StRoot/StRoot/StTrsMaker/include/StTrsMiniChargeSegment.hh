/***************************************************************************
 *
 * $Id: StTrsMiniChargeSegment.hh,v 1.4 2003/12/24 13:44:51 fisyak Exp $
 *
 * Author: brian
 *
 ***************************************************************************
 *
 * Description: piece of a decomposed g2t segmetn
 *
 ***************************************************************************
 *
 * $Log: StTrsMiniChargeSegment.hh,v $
 * Revision 1.4  2003/12/24 13:44:51  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.3  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  1999/01/18 21:03:30  lasiuk
 * Jan 18,1999
 *
 * Revision 1.1  1998/11/10 17:12:11  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.6  1998/11/08 17:03:41  lasiuk
 * inline arguments for LINUX use built-in types
 *
 * Revision 1.5  1998/11/02 22:49:10  lasiuk
 * add setCharge()
 * overload << operator
 *
 * Revision 1.4  1998/10/22 00:23:25  lasiuk
 * Oct 22
 *
 * Revision 1.3  1998/06/04 23:17:25  lasiuk
 * define access functions
 *
 * Revision 1.1.1.1  1998/05/19 22:33:44  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_MINI_CHARGE_SEGMENT_HH
#define ST_TRS_MINI_CHARGE_SEGMENT_HH

#include <Stiostream.h>
#include <vector>

#include "StGlobals.hh"
#include "StThreeVector.hh"
#include "StMatrix.hh"

class StTrsMiniChargeSegment {

public:
    StTrsMiniChargeSegment(StThreeVector<double>, double, double, int);
    ~StTrsMiniChargeSegment();
    //StTrsMiniChargeSegment(const StTrsMiniChargeSegment&);
    //StTrsMiniChargeSegment& operator=(const StTrsMiniChargeSegment&);

#ifdef ST_NO_TEMPLATE_DEF_ARGS  // need for allocator
  StTrsMiniChargeSegment() {}
#endif
    // access functions
    StThreeVector<double>&       position()       ; // this is coordinate in sector 12 frame
    const StThreeVector<double>& position()  const;
    double                       dl()        const;
    const double                 charge()    const;

    void                         setCharge(double);
    int                          id()        const {return mId;}
private:
    double                   mNumberOfElectrons;
    double                   mLength;
    StThreeVector<double>    mPosition;
    int                      mId;                  // geant track id
};

inline StThreeVector<double>&
StTrsMiniChargeSegment::position()  {return mPosition;}
inline const StThreeVector<double>&
StTrsMiniChargeSegment::position() const {return mPosition;}

inline double StTrsMiniChargeSegment::dl() const {return mLength;}
inline const double StTrsMiniChargeSegment::charge() const {return mNumberOfElectrons;}
inline void StTrsMiniChargeSegment::setCharge(double n) {mNumberOfElectrons = n;}

// Non-Member Function
ostream& operator<<(ostream&, const StTrsMiniChargeSegment&);
#endif
