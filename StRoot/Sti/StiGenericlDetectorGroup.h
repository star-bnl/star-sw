#ifndef STAR_StiGenericDetectorGroup_H_INCLUDED
#define STAR_StiGenericDetectorGroup_H_INCLUDED
// $Id: StiGenericlDetectorGroup.h,v 2.1 2006/12/04 01:03:31 fine Exp $
// Author: Valeri Fine, Dec 2006

#include <stdexcept>
#include "Sti/Base/Named.h"
#include "Sti/StiDetectorBuilder.h"

class StiDetectorBuilder;
class StiElossCalculator;

class StiGenericDetectorGroup  : public Named
{
   protected:
      StiGenericDetectorGroup(const string & name);
      StiGenericDetectorGroup(const string & name,
		   StiDetectorBuilder * detectorBuilder,
		   StiElossCalculator * elossCalculator);
      virtual ~StiGenericDetectorGroup();

      StiDetectorBuilder * _detectorBuilder;
      StiElossCalculator * _elossCalculator; 
      /// Detector group identifier.
      int _groupId;
  public:
    virtual void initialize() {}
    /// Get a detector builder appropriate for this detector group
    virtual StiDetectorBuilder * getDetectorBuilder();

    /// Get a pid calculator appropriate for this detector group
    /// A dedx calculator is used after the track are fitted
    /// to determine the average (or appropriate measure) dedx.
    /// Get an energy loss calculator appropriate for this detector group
    /// An eloss calculator is used in the kalman propagation to determine
    /// the track energy loss.
    StiElossCalculator * getElossCalculator();
    void setGroupId(int id);
    int  getGroupId() const;
};

//_____________________________________________________________________________
StiGenericDetectorGroup::StiGenericDetectorGroup(const string & name)
  : Named(name),
     _detectorBuilder(0),
     _elossCalculator(0),
     _groupId(-1)
{
   initialize();
}

//_____________________________________________________________________________
StiGenericDetectorGroup::StiGenericDetectorGroup(const string & name,
		   StiDetectorBuilder * detectorBuilder,
		   StiElossCalculator * elossCalculator)
    :  Named(name),
     _detectorBuilder(detectorBuilder),
     _elossCalculator(elossCalculator),
     _groupId(-1)
{
   initialize();
}

//_____________________________________________________________________________
StiGenericDetectorGroup::~StiGenericDetectorGroup()
{
  delete _detectorBuilder;
}
//_____________________________________________________________________________
inline StiDetectorBuilder *StiGenericDetectorGroup::getDetectorBuilder()
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
inline StiElossCalculator *StiGenericDetectorGroup::getElossCalculator()
{
   if (_elossCalculator==0)
   {
      string message = "StiDetectorGroup::getElossCalculator() - ERROR - elossCalculator == 0 for detector:";
      message += getName();
      throw logic_error(message.c_str());
   }
   return _elossCalculator; 
}

//_____________________________________________________________________________
inline void StiGenericDetectorGroup::setGroupId(int id)
{
  if (_detectorBuilder) _detectorBuilder->setGroupId(id);
  _groupId = id;
}

//_____________________________________________________________________________
inline int  StiGenericDetectorGroup::getGroupId() const
{
  return _groupId;
}

#endif
