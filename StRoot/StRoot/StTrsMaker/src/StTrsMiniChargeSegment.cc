/***************************************************************************
 *
 * $Id: StTrsMiniChargeSegment.cc,v 1.4 2003/12/24 13:44:53 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrsMiniChargeSegment.cc,v $
 * Revision 1.4  2003/12/24 13:44:53  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.3  1999/12/08 02:10:42  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.2  1999/01/18 21:02:53  lasiuk
 * comment diagnostics
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

StTrsMiniChargeSegment::StTrsMiniChargeSegment(StThreeVector<double> pos, double de, double ds, int id)
    : mNumberOfElectrons(de), mLength(ds), mPosition(pos), mId(id) { /*nopt*/ }

StTrsMiniChargeSegment::~StTrsMiniChargeSegment() { /*nopt*/ }

ostream& operator<<(ostream& os, const StTrsMiniChargeSegment& seg)
{
  return os << "(Pos:" << seg.position() << ",Charge:" << seg.charge() << ",id:" << seg.id() << ")";
}
