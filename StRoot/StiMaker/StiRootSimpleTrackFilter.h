#ifndef StiRootSimpleTrackFilter_H
#define StiRootSimpleTrackFilter_H 1
#include "Sti/StiTrackFilter.h"
#include "Sti/EditableParameters.h"

class StiRootSimpleTrackFilter : public StiTrackFilter, public EditableParameters
{
 public:
  StiRootSimpleTrackFilter();
  virtual ~StiRootSimpleTrackFilter();
  /*virtual void add(const string & name, 
		   const string & description,
		   double value, 
		   double defaultValue, 
		   double min, 
		   double max,
		   double increment,
		   int    type);*/
  virtual bool accept(StiTrack * t) const;
  virtual void initialize();
  virtual void setDefaults();

};

/*! StiRootSimpleTrackFilter factory
 */
class StiRootSimpleTrackFilterFactory : public StiTrackFilterFactory
{
public:
  ///This is the only constructor available.
  StiRootSimpleTrackFilterFactory(const string& newName, 
				  int original=-1, int 
				  incremental=-1, 
				  int maxInc=-1);
  ///Default destructor.
  virtual ~StiRootSimpleTrackFilterFactory();
  
 protected:
  ///Return a pointer to a new StiRootSimpleTrackFilter object on the heap.
  virtual void * makeNewObject() const
    {
      return new StiRootSimpleTrackFilter();
    }
  
 private:
  StiRootSimpleTrackFilterFactory(); //Not implemented
};

#endif
