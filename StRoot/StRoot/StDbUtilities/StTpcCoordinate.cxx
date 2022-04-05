/***********************************************************************
 * $Id: StTpcCoordinate.cxx,v 1.2 2011/01/18 14:34:28 fisyak Exp $
 ***********************************************************************/
#include "StTpcLocalDirection.hh"
#include "StTpcLocalCoordinate.hh"
#include "StTpcLocalSectorAlignedDirection.hh"
#include "StTpcLocalSectorAlignedCoordinate.hh"
#include "StTpcLocalSectorDirection.hh"
#include "StTpcLocalSectorCoordinate.hh"
//________________________________________________________________________________
StTpcLocalCoordinate::StTpcLocalCoordinate(double x, double y, double z) : StTpcCoordinate(x,y,z,0,0) {}
//________________________________________________________________________________
StTpcLocalCoordinate::StTpcLocalCoordinate(const StThreeVector<double>& xyz) : StTpcCoordinate(xyz,0,0) {}
//________________________________________________________________________________
#define OS "( (" <<  a.position().x() << ", " \
    << a.position().y() << ", " \
    << a.position().z() << ") " \
    << ", " << a.fromSector() << "," << a.fromRow() << " )"
// Non-member Functions
ostream& operator<<(ostream& os, const StTpcCoordinate& a) {
  return os << OS;
}
//________________________________________________________________________________
ostream& operator<<(ostream& os, const StTpcLocalDirection& a) {
  return os << "TPC_Local Direction( (" << OS;
}
//________________________________________________________________________________
ostream& operator<<(ostream& os, 
const StTpcLocalCoordinate& a) {
  return os << "TPC_Local( (" << OS;
}
//________________________________________________________________________________
ostream& operator<<(ostream& os, const StTpcLocalSectorCoordinate& a) {
  return os << "TPC_Local_Sector( (" << OS;
}
ostream& operator<<(ostream& os, const StTpcLocalSectorDirection& a) {
  return os << "TPC_Local_Sector Direction( (" << OS;
}
#undef OS
