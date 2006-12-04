#ifndef STAR_StiGenericDetectorGroup_H_INCLUDED
#define STAR_StiGenericDetectorGroup_H_INCLUDED
// $Id: StiGenericDetectorGroup.h,v 2.1 2006/12/04 02:37:03 fine Exp $
// Author: Valeri Fine, Dec 2006

#include <stdexcept>
#include "Sti/Base/Named.h"

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
     virtual void initialize(){} ; // FIXME:  this method must be abstract = 0;
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
#endif
