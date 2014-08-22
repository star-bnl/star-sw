// $Id: StiGenericDetectorGroup.cxx,v 2.3 2014/08/22 16:29:14 perev Exp $
// Author: Valeri Fine, Dec 2006

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
  if (_detectorBuilder==0)
    {
      string message = "StiDetectorGroup::getDetectorBuilder() - ERROR - builder == 0 for detector:";
      message += getName();
      throw logic_error(message.c_str());
    }
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
