#include "StTpcLocalSectorAlignedCoordinate.hh"
// Non-member Functions
ostream& operator<<(ostream& os, const StTpcLocalSectorAlignedCoordinate& a)
{
    return os << "TPC_Local_Aligned_Sector( ("
	      << a.position().x()  << ", "
	      << a.position().y()  << ", "
	      << a.position().z()  << "),"
	      << a.fromSector() << ")";
}
