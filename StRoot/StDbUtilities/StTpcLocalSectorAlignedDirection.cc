#include "StTpcLocalSectorAlignedDirection.hh"
// Non-member Functions
ostream& operator<<(ostream& os, const StTpcLocalSectorAlignedDirection& a)
{
    return os << "TPC_Local_Aligned_Sector Direction( ("
	      << a.position().x()  << ", "
	      << a.position().y()  << ", "
	      << a.position().z()  << "), sector "
	      << a.fromSector() << ", row " << a.fromRow() << ")";
}
