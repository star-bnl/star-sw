//VectorizedFactory

#ifndef VectorizedFactory_HH
#define VectorizedFactory_HH
#include <stdexcept>
#include "Stiostream.h"
#include <vector>

using std::vector;
using std::cout;
using std::endl;
using std::ostream;

#include "Sti/Base/Factory.h"

// Concrete templated vectorized object factory implementing the Factory interface.
//Author of derived classes must call initialize after construction.
//Internal protection against re-initialization;
template <class Concrete, class Abstract>
class VectorizedFactory : public Factory<Abstract>
{
public:
    VectorizedFactory(const string& newName, 
		      int original, 
		      int incremental, 
		      int maxInc);
    virtual ~ VectorizedFactory();
    
    virtual Abstract * getInstance();

    // Reset the vectorized factory. The reset effectively declares
    // all instances owned by this factory as "unused".
    void reset();
    void clear();
    void initialize();
    void setIncrementalSize(int);
    void setMaxIncrementCount(int);
    int getIncrementalSize() const;
    int getMaxIncrementCount() const;
    int getCurrentSize() const;
    
    static const int defaultMaxIncrementCount; // 10
    static const int defaultIncrementSize;     // 5000
    static const int defaultOriginalSize;      // 100000

protected:
    void destroy();
    typedef vector<Abstract*> t_vector;
    virtual void instantiate(int);
    
private:
    VectorizedFactory(); //Not implemented

    int _originalSize;
    int _incrementalSize;
    int _maxIncrementCount;
    int _incrementCount;

    t_vector _container;
    typename t_vector::iterator _current;    
};

//Implementation
template <class Concrete, class Abstract>
const int VectorizedFactory<Concrete,Abstract>::defaultMaxIncrementCount   = 10;
template <class Concrete, class Abstract>
const int VectorizedFactory<Concrete,Abstract>::defaultIncrementSize = 5000;
template <class Concrete, class Abstract>
const int VectorizedFactory<Concrete,Abstract>::defaultOriginalSize = 100000;

template <class Concrete, class Abstract>
ostream& operator<<(ostream& os, const VectorizedFactory<Concrete,Abstract> & f)
{
  os  << "                Name :"<< f.name<<endl
      << "       Current  Size :"<< f.getCurrentSize()<<endl;
  return os;
}

template <class Concrete, class Abstract>
VectorizedFactory<Concrete,Abstract>::VectorizedFactory(const string& name,
					int original, 
					int incremental, 
					int maxInc)
  :  Factory<Abstract>(name),
     _originalSize(original),
     _incrementalSize(incremental),
     _maxIncrementCount(maxInc),
     _incrementCount(0)
{
  cout << "VectorizedFactory<Concrete,Abstract>::VectorizedFactory() - INFO - "<<endl
       << "                Name :"<< name<<endl
       << "       Original Size :"<< _originalSize<<endl
       << "     IncrementalSize :"<< _incrementalSize<<endl
       << " Max Increment Count :"<< _maxIncrementCount<<endl;
  //initialize();
  reset();
}

template <class Concrete, class Abstract>
VectorizedFactory<Concrete,Abstract>::~VectorizedFactory()
{
    clear();
}

template <class Concrete, class Abstract>
inline Abstract* VectorizedFactory<Concrete,Abstract>::getInstance()
{
  if (!( _current < _container.end()) )
    {
    if (_incrementCount<1)
      {
      initialize();
      }
    else if (_incrementCount< _maxIncrementCount)
      {
      instantiate(_incrementalSize);
      }
    else
      {
      cout << "VectorizedFactory<"<<getName()<<">getInstance() -F- Too many expension requests - ABORT!" << endl;
      throw logic_error("VectorizedFactory::getInstance() -F- Too many expension requests");
      }
    }
  return *_current++;
}

template <class Concrete, class Abstract> 
void VectorizedFactory<Concrete,Abstract>::reset()  
{
  _current = _container.begin();
}

template <class Concrete, class Abstract> 
void VectorizedFactory<Concrete,Abstract>::setIncrementalSize(int increment) 
{
  _incrementalSize = increment;
}   

template <class Concrete, class Abstract> 
void VectorizedFactory<Concrete,Abstract>::setMaxIncrementCount(int maxCount) 
{
    _maxIncrementCount = maxCount;
}    

template <class Concrete, class Abstract> 
int VectorizedFactory<Concrete,Abstract>::getIncrementalSize() const 
{
    return _incrementalSize;
}

template <class Concrete, class Abstract> 
int VectorizedFactory<Concrete,Abstract>::getMaxIncrementCount() const 
{
  return _maxIncrementCount;
}

template <class Concrete, class Abstract> 
int VectorizedFactory<Concrete,Abstract>::getCurrentSize() const 
{
  return _container.size();
}

template <class Concrete, class Abstract> 
inline  void VectorizedFactory<Concrete,Abstract>::initialize()
{
    if (_incrementCount<1)
	instantiate(_originalSize);
    reset();
}

/// Clear this factory by deleting all object instances it owns, and resetting internal variables
///  <p>
///  First delete all objects/instances, and then clear the _container. Finally reset internal variables.
template <class Concrete, class Abstract> 
void VectorizedFactory<Concrete,Abstract>::clear()
{
    for (typename t_vector::iterator it=_container.begin(); it!=_container.end(); ++it) 
      delete *it;
    _container.clear();
    _incrementCount = 0;
    _current = _container.begin();
}

/// Destroy this factory by deleting all object instances it owns, and resetting internal variables
///  <p>
///  First delete all objects/instances, and then clear the _container. Finally reset internal variables.
template <class Concrete, class Abstract> 
void VectorizedFactory<Concrete,Abstract>::destroy()
{
    for (typename t_vector::iterator it=_container.begin(); it!=_container.end(); ++it) 
      delete *it;
    _container.clear();
}


/// Instantiate "n" objects of class Concrete and add them to the internal _container.
template <class Concrete, class Abstract> 
void VectorizedFactory<Concrete,Abstract>::instantiate(int n) 
{
  cout << "VectorizedFactory<"<<getName()<<">::instantiate("<<n<<") -I- Size:"<<_container.size()
  <<"(" << sizeof(Concrete)*_container.size()<<"bytes)";
  int currentDistance = _current-_container.begin();
  _container.reserve( _container.size()+n );
  for (int i=0;i<n; ++i) {
    _container.push_back( new Concrete() );
  }
  _current = _container.begin()+currentDistance;
  _incrementCount++;
  cout << " -> "<< _container.size()
    <<"(" << sizeof(Concrete)*_container.size()<<"bytes)  Done" << endl;
}


#endif
