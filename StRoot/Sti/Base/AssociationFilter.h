#ifndef AssociationFilter_H
#define AssociationFilter_H 1

/*!
  Abstract base class defining a  filtering mechanism for an
  associator.
  <p>
  This class does not implement a particular filter but 
  rather simply define an interface for  association filtering classes 
  which inherit from it. 
  <p>
  The construction of this class is quite similar to that of 
  Filter. However a filter works on a single object whereas 
  an association filter works on two objects to decide they
  whether they can be matched or "associated".
  <p>
  Such an associator can be used on hits or tracks. 
  <p>
  Usage: Derived classes should use the following methods to 
  implement the functionality of the filter.
  "accept( * )     overloaded to determine whether given
  filtered objet passes the filter requirements.
*/
template<class Filtered>
class AssociationFilter 
{
 public:
  AssociationFilter();
  virtual ~AssociationFilter();
  virtual bool accept(const Filtered *f1,const Filtered *f2) const;
  virtual bool operator()(const Filtered *f1,const Filtered *f2) const;
  virtual void reset();
  bool filter(const Filtered *f1,const Filtered *f2);
  int  getAnalyzedCount();
  int  getAcceptedCount();  
  int  getQuality();
 protected:
  int _quality;
  int _analyzedCount;
  int _acceptedCount;
};


template<class Filtered>
AssociationFilter<Filtered>::AssociationFilter()
  :  _quality(0),
     _analyzedCount(0),
     _acceptedCount(0)
{}

template<class Filtered>
AssociationFilter<Filtered>::~AssociationFilter()
{}
  
template<class Filtered>
inline bool AssociationFilter<Filtered>::filter(const Filtered *f1,const Filtered *f2)
{
  _analyzedCount++;
  bool acc = accept(f1,f2);
  if (acc) _acceptedCount++;
  return acc;
}

template<class Filtered>
inline bool AssociationFilter<Filtered>::accept(const Filtered *f1,const Filtered *f2) const
{
  return true;
}

template<class Filtered>
inline void AssociationFilter<Filtered>::reset()
{
  _analyzedCount = 0;
  _acceptedCount = 0;
}

template<class Filtered>
int  AssociationFilter<Filtered>::getAnalyzedCount() 
{
  return _analyzedCount; 
}

template<class Filtered>
int  AssociationFilter<Filtered>::getAcceptedCount() 
{ 
  return _acceptedCount; 
}

template<class Filtered>
bool AssociationFilter<Filtered>::operator()(const Filtered *f1,const Filtered *f2) const
{
  return f1<f2;
}

template<class Filtered>
int  AssociationFilter<Filtered>::getQuality()
{ 
  return _quality; 
}

#endif

