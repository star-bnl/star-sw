#ifndef Vectorized_H_Included 
#define Vectorized_H_Included 
#include <vector>
using std::vector;

/// Class  Vectorized is a facade to the <vector> class
/// to be used by classes that need an internal vector, and iterators
/// without exposing the guts of the stl vector class.
template<class OBJECT> 
class Vectorized : public vector<OBJECT*>
{ 
 public:
  virtual ~Vectorized();
  Vectorized();
  OBJECT* add(OBJECT * object);
  /*void remove(OBJECT *object);
  void clear();
  vector<OBJECT*>::iterator begin();
  vector<OBJECT*>::const_iterator begin() const;
  vector<OBJECT*>::iterator end();
  vector<OBJECT*>::const_iterator end() const;
  const OBJECT* &operator[](int index) const;
  OBJECT*& operator[](int index);*/
 protected:

  //vector<OBJECT*> _objects;
};

///The destructor deletes the object held by the internal vector
///because it assumes ownership of those objects.
template<class OBJECT>
Vectorized<OBJECT>::~Vectorized()
{
  for (vector<OBJECT*>::iterator i=begin();i!=end();i++)
    delete *i;
  //_objects.clear();
  clear();
}

template<class OBJECT>
OBJECT * Vectorized<OBJECT>::add(OBJECT * object)
{
  //_objects.push_back(object);
  push_back(object);
  return object;
}

/*
template<class OBJECT>
void Vectorized<OBJECT>::remove(OBJECT *object)
{
  
}

template<class OBJECT>
void Vectorized<OBJECT>::clear()
{
  _objects.clear();
}

template<class OBJECT>
vector<OBJECT*>::iterator Vectorized<OBJECT>::begin()
{
  return _objects.begin();
}

template<class OBJECT>
vector<OBJECT*>::const_iterator Vectorized<OBJECT>::begin() const
{
  return _objects.begin();
}

template<class OBJECT>
vector<OBJECT*>::iterator Vectorized<OBJECT>::end()
{
  return _objects.end();
}

template<class OBJECT>
vector<OBJECT*>::const_iterator Vectorized<OBJECT>::end() const
{
  return _objects.end();
}


template<class OBJECT>
Vectorized<OBJECT>::Vectorized()
{
}

template<class OBJECT>
const OBJECT*& Vectorized<OBJECT>::operator[](int index) const
{
  return &_objects[index];
}

template<class OBJECT>
OBJECT*& Vectorized<OBJECT>::operator[](int index)
{
  return &_objects[index];
}

*/
#endif

