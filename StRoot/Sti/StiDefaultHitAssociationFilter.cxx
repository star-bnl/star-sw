///\file StiDefaultHitAssociationFilter.cxx
///\author Claude A Pruneau, Wayne State University
///\date April 30, 03
///Implementation of the StiDefaultHitAssociationFilter class.
#include "Sti/Base/Parameter.h"
#include "Sti/Base/EditableParameter.h"
#include "Sti/StiHit.h"
#include "StiDefaultHitAssociationFilter.h"

StiDefaultHitAssociationFilter::StiDefaultHitAssociationFilter()
  :  EditableAssociationFilter<StiHit>("StiDefaultHitAssociationFilter","StiDefaultHitAssociationFilter")
{}

StiDefaultHitAssociationFilter::StiDefaultHitAssociationFilter(const string & name, const string & description)
  :  EditableAssociationFilter<StiHit>(name,description)
{}

StiDefaultHitAssociationFilter::~StiDefaultHitAssociationFilter()
{}

void StiDefaultHitAssociationFilter::initialize()
{
  cout << "StiDefaultHitFilter::initialize() - INFO - Starting" << endl;
  parameterVector.clear();
  add(new EditableParameter("DistanceMax","Max Distance",&_maxDistance, 6., 0., 20., 0.02, 0));
}

///Determine whether given hits satisfy this association filter current 
///settings. The filtering is based on the relative distance of the two
///points in the local reference frame of the detector. The 2 points
///must also be in the same detector.
bool StiDefaultHitAssociationFilter::accept(const StiHit * h1,const StiHit * h2)
{
  double dy = h1->y()- h2->y();
  double dz = h1->z()- h2->z();
  double dr = ::sqrt(dy*dy+dz*dz);
  if (dr>0)
    _quality = 1/dr;
  else
    _quality = 1.e20;
    
  return h1->position()==h2->position() && h1->refangle()==h2->refangle() && dr<6.;//_maxDistance;
}

