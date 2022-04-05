// $Id: StiGenericDetectorGroup.cxx,v 2.7 2018/06/29 21:46:26 smirnovd Exp $
// Author: Valeri Fine, Dec 2006

#include <cassert>
#include <stdexcept>
#include "StiGenericDetectorGroup.h"
#include "Sti/StiDetectorBuilder.h"

//_____________________________________________________________________________
StiGenericDetectorGroup::StiGenericDetectorGroup(const string & name)
  : Named(name),
     _detectorBuilder(0),
     _groupId(-1)
{
  
}

//_____________________________________________________________________________
StiGenericDetectorGroup::StiGenericDetectorGroup(const string & name,
		   StiDetectorBuilder * detectorBuilder)
    :  Named(name),
     _detectorBuilder(detectorBuilder),
     _groupId(-1)
{
}

//_____________________________________________________________________________
StiGenericDetectorGroup::~StiGenericDetectorGroup()
{
  delete _detectorBuilder;
}
//_____________________________________________________________________________
StiDetectorBuilder *StiGenericDetectorGroup::getDetectorBuilder()
{
  assert(_detectorBuilder);
  return _detectorBuilder; 
}


//_____________________________________________________________________________
void StiGenericDetectorGroup::setGroupId(int id)
{
  if (_detectorBuilder) _detectorBuilder->setGroupId(id);
  _groupId = id;
}

//_____________________________________________________________________________
int  StiGenericDetectorGroup::getGroupId() const
{
  return _groupId;
}
