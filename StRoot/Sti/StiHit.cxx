//StiHit.cxx
//M.L. Miller (Yale Software)
//04/01
#include <iostream.h>
#include "StEventTypes.h"
#include "StiHit.h"
#include "StiDetector.h"
#include "StiPlacement.h"

StiHit::StiHit()
{
    reset();
}

StiHit::~StiHit()
{}

void StiHit::setError(const StMatrixF& matrix)
{
    enum Labels {x=1, y=2, z=3};

    //Set Diagonal elements
    msxx = matrix(x,x);
    msyy = matrix(y,y);
    mszz = matrix(z,z);
    //Off Diagonal
    msxy = matrix(x,y);
    msxz = matrix(x,z);
    msyz = matrix(y,z);
    return;
}


/*! Streamer for StiHit objects. */
ostream& operator<<(ostream& os, const StiHit& hit)
{
    return os <<hit.refangle()<<" "<<hit.position()<<"\t\t" //Key
	      <<hit.x()<<" "<<hit.y()<<" "<<hit.z()<<"\t\t" //Position
	      <<hit.sxx()<<" "<<hit.syy()<<" "<<hit.szz()<<"\t" //Diagonal Error
	      <<hit.sxy()<<" "<<hit.sxz()<<" "<<hit.syz()<<"\t" //Off-diagonal error
	      <<hit.detector() //pointer to detector
	      <<"times Used: "<<hit.timesUsed();
}


