/***************************************************************************
 *
 * $Id: StTrsMiniChargeSegment.cc,v 1.1 1998/11/10 17:12:25 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrsMiniChargeSegment.cc,v $
 * Revision 1.1  1998/11/10 17:12:25  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/10 17:12:25  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.4  1998/11/02 22:49:20  lasiuk
 * overload << operator
 *
 * Revision 1.3  1998/10/22 00:24:26  lasiuk
 * Oct 22
 *
 * Revision 1.2  1998/06/04 23:22:10  lasiuk
 * define access functions
 *
 * Revision 1.1.1.1  1998/05/19 22:33:44  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include "StTrsMiniChargeSegment.hh"

StTrsMiniChargeSegment::StTrsMiniChargeSegment(StThreeVector<double> pos, double de, double ds)
    : mPosition(pos), mNumberOfElectrons(de), mLength(ds) { /*nopt*/ }

StTrsMiniChargeSegment::~StTrsMiniChargeSegment() { /*nopt*/ }

ostream& operator<<(ostream& os, StTrsMiniChargeSegment& seg)
{
    return os << '(' << seg.position() << ", " << seg.charge() << ", " << seg.dl() << ')';
}
