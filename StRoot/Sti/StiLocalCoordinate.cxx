#include "StiLocalCoordinate.h"

StiLocalCoordinate::StiLocalCoordinate() {
} // StiLocalCoordinate

StiLocalCoordinate::StiLocalCoordinate(
    const double x, const double y, const double z)
    : mPosition(x,y,z) {
} // StiLocalCoordinate

StiLocalCoordinate::StiLocalCoordinate(const StThreeVector<double>& position)
    : mPosition(position) {
} // StiLocalCoordinate

StiLocalCoordinate::~StiLocalCoordinate() {
} // ~StiLocalCoordinate

int StiLocalCoordinate::operator==(const StiLocalCoordinate& p) const{
    return p.mPosition == mPosition;
} // operator==

int StiLocalCoordinate::operator!=(const StiLocalCoordinate& p) const{
    return !(*this == p);  // use operator==()
} // operator!=

ostream& operator<<(ostream& os, const StiLocalCoordinate& a){
    return os << "TPC_Local ("
	      << a.position().x() << ", "
	      << a.position().y() << ", "
	      << a.position().z() << ")";
} // operator<<
