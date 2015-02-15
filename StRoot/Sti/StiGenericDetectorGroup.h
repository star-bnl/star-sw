#ifndef STAR_StiGenericDetectorGroup_H_INCLUDED
#define STAR_StiGenericDetectorGroup_H_INCLUDED
// $Id: StiGenericDetectorGroup.h,v 2.2 2014/08/22 16:28:36 perev Exp $
// Author: Valeri Fine, Dec 2006

#include <stdexcept>
#include "Sti/Base/Named.h"

class StiDetectorBuilder;

class StiGenericDetectorGroup  : public Named
{
   protected:
      StiGenericDetectorGroup(const string & name);
      StiGenericDetectorGroup(const string & name,
		   StiDetectorBuilder * detectorBuilder);
      virtual ~StiGenericDetectorGroup();

      StiDetectorBuilder * _detectorBuilder;
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
    void setGroupId(int id);
    int  getGroupId() const;
};
#endif
