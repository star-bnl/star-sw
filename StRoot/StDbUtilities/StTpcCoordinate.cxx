/***********************************************************************
 * $Id: StTpcCoordinate.cxx,v 1.1 2004/06/05 23:31:09 fisyak Exp $
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
ostream& operator<<(ostream& os, const StTpcLocalSectorAlignedDirection& a) {
  return os << "TPC_Local_Aligned_Sector Direction( (" << OS;
}
//________________________________________________________________________________
ostream& operator<<(ostream& os, const StTpcLocalSectorAlignedCoordinate& a){
  return os << "TPC_Local_Aligned_Sector( (" << OS;
}
ostream& operator<<(ostream& os, const StTpcLocalSectorCoordinate& a) {
  return os << "TPC_Local_Sector( (" << OS;
}
ostream& operator<<(ostream& os, const StTpcLocalSectorDirection& a) {
  return os << "TPC_Local_Sector Direction( (" << OS;
}
#undef OS
