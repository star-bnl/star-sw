/***************************************************************************
 *
 * $Id: StLorentzVectorF.cc,v 1.1 1999/01/30 03:59:03 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 * Remarks:   This is a 'handmade' specialisation of StLorentzVector<T>
 *            for T=float. This code contains no templates.
 *
 ***************************************************************************
 *
 * $Log: StLorentzVectorF.cc,v $
 * Revision 1.1  1999/01/30 03:59:03  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.2  1999/10/15 15:56:34  ullrich
 * Changed output format in operator<<, added operator>>
 *
 * Revision 1.1  1999/01/30 03:59:03  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:18  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StLorentzVectorF.hh"

#ifdef __ROOT__
#include "TBuffer.h"
ClassImp(StLorentzVectorF)
#endif

StLorentzVectorF::StLorentzVectorF(const StLorentzVectorD &vec)
    : mThreeVector(vec.vect()), mX4(vec.t()) { /* nop */ }

StLorentzVectorF
StLorentzVectorF::boost(const StLorentzVectorF& pframe) const
{
    double mass        = abs(pframe);
    StThreeVectorD eta = (-1./mass)*pframe.vect();            // gamma*beta
    double gamma       = pframe.e()/mass;
    StThreeVectorD pl  = ((this->vect()*eta)/(eta*eta))*eta;  // longitudinal momentum
    return StLorentzVectorF(gamma*this->e() - this->vect()*eta,
			    this->vect() + (gamma-1.)*pl - this->e()*eta);
}

StLorentzVectorF
StLorentzVectorF::boost(const StLorentzVectorD& pframe) const
{
    double mass               = abs(pframe);
    StThreeVectorD eta        = (-1./mass)*pframe.vect();            // gamma*beta
    double gamma              = pframe.e()/mass;
    StThreeVectorD pl         = ((this->vect()*eta)/(eta*eta))*eta;  // longitudinal momentum
    return StLorentzVectorF(gamma*this->e() - this->vect()*eta,
			    this->vect() + (gamma-1.)*pl - this->e()*eta);
}

StLorentzVectorF&
StLorentzVectorF::operator=(const StLorentzVectorD& vec)
{
    mThreeVector = vec.vect();
    mX4 = vec.t();
    return *this;
}

int
StLorentzVectorF::operator== (const StLorentzVectorD& v) const
{
    return (mThreeVector == v.vect()) && (mX4 == v.t());
}

int
StLorentzVectorF::operator!= (const StLorentzVectorD& v) const
{
    return !(*this == v);
}

StLorentzVectorF&
StLorentzVectorF::operator+= (const StLorentzVectorD& v)
{
    mThreeVector += v.vect();
    mX4 += v.t();
    return *this;
}

StLorentzVectorF&
StLorentzVectorF::operator-= (const StLorentzVectorD& v)
{
    mThreeVector -= v.vect();
    mX4 -= v.t();
    return *this;
}
  return os << '(' << v.vect() << ',' << v.t() << ')';
      R__b << mX4;
   }

}

