///////////////////////////////////////////////////////////////////////////
// $Id: TpcMapUtilities.cxx,v 1.1 2000/08/09 18:57:44 lansdell Exp $
//
// Author: M.L. Miller, Yale
//
///////////////////////////////////////////////////////////////////////////
//
// Description: TPC sector gains map utilities
//
///////////////////////////////////////////////////////////////////////////
//
// $Log: TpcMapUtilities.cxx,v $
// Revision 1.1  2000/08/09 18:57:44  lansdell
// improvements in TPC gains code reduces CPU time per event by factor of 2
//
//
///////////////////////////////////////////////////////////////////////////

#include "TpcMapUtilities.h"

//Equality defined by same sector and padrow
bool HitMapKey::operator==(const HitMapKey& key2) const
{
    if (this->sector==key2.sector && this->padrow==key2.padrow)
	{return true;}
    else {return false;}
}

//Return true if key2 < key1.  Order first by sector, then by padrow.
bool MapKeyLessThan::operator() (const HitMapKey& key1, const HitMapKey& key2) const
{
    bool val = false;
    if (key1.sector > key2.sector) {val =  false;}
    if (key1.sector < key2.sector) {val = true;}
    if (key1.sector == key2.sector) {
	if (key1.padrow > key2.padrow) {val = false;}
	if (key1.padrow < key2.padrow) {val = true;}
	if (key1.padrow == key2.padrow) {val = false;}
    }
    return val;
}

//---------------------------------------
PadrowLocation::PadrowLocation() {};

PadrowLocation::PadrowLocation(const StThreeVectorD& out, const StThreeVectorD& cent, const StThreeVectorD& in)
{
    m_TopPoint = out;
    m_MidPoint = cent;
    m_BotPoint = in;
}

PadrowLocation::~PadrowLocation() {};

const StThreeVectorD& PadrowLocation::outsidePoint() const { return m_TopPoint;}
const StThreeVectorD& PadrowLocation::centerPoint() const { return m_MidPoint;}
const StThreeVectorD& PadrowLocation::insidePoint() const { return m_BotPoint;}

void PadrowLocation::print() const
{
    cout<<m_TopPoint<<"\t"<<m_MidPoint<<"\t"<<m_BotPoint<<endl;
    return;
}

