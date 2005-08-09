#ifndef Filter_H
#define Filter_H 1

/*!
  Abstract base class defining a  filtering mechanism
  <p>
  This class does not implement a particular filter but 
  rather simply define an interface for  filtering classes 
  which inherit from it. 
  <p>
  Usage: Derived classes should use the following methods to 
  implement the functionality of the filter.
  "accept( * )     overloaded to determine whether given
  filtered objet passes the filter requirements.
*/
template<class Filtered>
class Filter 
{
 public:
  
  Filter();
  virtual ~Filter();
  
  //virtual void initialize()=0;
  virtual bool accept(const Filtered *filtered) const=0;
  virtual void reset();
  virtual void unset(){;}
  
  bool filter(const Filtered * filtered);
  int  getAnalyzedCount();
  int  getAcceptedCount();
  
 protected:
  
  int _analyzedCount;
  int _acceptedCount;
  
};


template<class Filtered>
Filter<Filtered>::Filter()
  :  _analyzedCount(0),
     _acceptedCount(0)
{}

template<class Filtered>
Filter<Filtered>::~Filter()
{}
  
template<class Filtered>
inline bool Filter<Filtered>::filter(const Filtered *filtered)
{
  _analyzedCount++;
  bool acc = accept(filtered);
  if (acc) _acceptedCount++;
  return acc;
}

template<class Filtered>
inline void Filter<Filtered>::reset()
{
  _analyzedCount = 0;
  _acceptedCount = 0;
}

template<class Filtered>
int  Filter<Filtered>::getAnalyzedCount() 
{
  return _analyzedCount; 
}

template<class Filtered>
int  Filter<Filtered>::getAcceptedCount() 
{ 
  return _acceptedCount; 
}

#endif

