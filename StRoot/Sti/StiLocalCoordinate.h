/**
 * @file StiLocalCoordinate.h
 * @class StiLocalCoordinate
 * @brief Represents coordinates in the Sti Local system
 *
 * The Sti local system is defined as followed:
 * <ul>
 * <li>z points in the direction of global (star magnet iron) z
 * <li>x points radially outward from the center of the detector plane
 * <li>y follows the right hand rule.
 * <li>origin = global origin
 * </ul>
 * 
 * @author Ben Norman, Kent State University
 * @date March 2002
 */

#ifndef STI_LOCAL_COORDINATE_H
#define STI_LOCAL_COORDINATE_H

#include <Stiostream.h>

#include "StThreeVector.hh"

class StiLocalCoordinate
{
public:
    StiLocalCoordinate();
    StiLocalCoordinate(const double, const double, const double);
    StiLocalCoordinate(const StThreeVector<double>&);

    virtual ~StiLocalCoordinate();
    
    int operator==(const StiLocalCoordinate&) const;
    int operator!=(const StiLocalCoordinate&) const;

    const StThreeVector<double>& position()  const;
    StThreeVector<double>& position();
    void setPosition(const StThreeVector<double>&);

protected:
    StThreeVector<double> mPosition;

};

inline const StThreeVector<double>& StiLocalCoordinate::position() const { 
  return(mPosition); 
}
inline StThreeVector<double>& StiLocalCoordinate::position() { 
  return(mPosition);
}
inline void StiLocalCoordinate::setPosition(const StThreeVector<double>& val){ 
  mPosition = val; 
}

// Non-member
ostream& operator<<(ostream&, const StiLocalCoordinate&);

#endif
