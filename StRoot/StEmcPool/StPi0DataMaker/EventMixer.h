#ifndef StPi0DataMaker_EventMixer_H
#define StPi0DataMaker_EventMixer_H

/*
TMixer and TMixerParameters
Template class for mixing any number of data lists (of identical data types) from any number of events.
Plain C++ and STL containers.

Oleksandr Grebenyuk
*/

#include <deque>
#include <vector>
#include <list>
using std::deque;
using std::vector;
using std::list;

template <class _value_type, class _list_type = list<_value_type>, class _pool_type = vector<_list_type>, class _pools_type = vector<_pool_type> >
class TMixer {
public: 
  typedef _value_type value_type;
  typedef _list_type list_type;
  typedef _pool_type pool_type;
  typedef _pools_type pools_type;
  typedef const value_type &const_value_reference;
  typedef const list_type &const_list_reference;
  typedef typename pool_type::size_type dimension_size_type;
  typedef typename pool_type::const_iterator pool_const_iterator;
  typedef pool_type &pool_reference;
  typedef const pool_type &const_pool_reference;
  typedef typename pools_type::size_type size_type;

  TMixer(size_type _size, dimension_size_type _dimension) : mSize(_size), mDimension(_dimension) {}

  size_type getSize() const {return this->mSize;}
  size_type getCount() const {return this->mEvents.size();}
  dimension_size_type getDimension() const {return this->mDimension;}

  void addEvent(const_pool_reference _pool) {
    while (this->getCount() >= this->getSize()) this->mEvents.erase(this->mEvents.begin());
    this->mEvents.push_back(_pool);
  }
  void addEvent(const_list_reference _list) {this->addEvent(pool_type(this->getDimension(), _list));}

  const_list_reference getList(size_type _index, dimension_size_type _dimensionIndex) const {return (this->mEvents[_index])[_dimensionIndex];}

protected:
  size_type mSize; //! Number of events to mix
  dimension_size_type mDimension; //! Number of lists to mix
  pools_type mEvents; //! Event pools
};

template <class _mixer_type, class _value_type = float/*, class _mixer_list = vector<_mixer_type>, class _value_list = list<_value_type>, class _size_list = list<typename _mixer_list::size_type>*/ >
class TMixerParameters {
public:
  typedef vector<_mixer_type> _mixer_list;
  typedef list<_value_type> _value_list;
  typedef list<typename _mixer_list::size_type> _size_list;

  typedef _mixer_type mixer_type;
  typedef _value_type value_type;
  typedef _mixer_list mixer_list;
  typedef _value_list value_list;
  typedef _size_list size_list;
  typedef mixer_type &mixer_reference;
  typedef const mixer_type &const_mixer_reference;
  typedef typename mixer_type::dimension_size_type mixer_dimension_size_type;
  typedef typename mixer_type::size_type mixer_size_type;

  typedef typename mixer_list::size_type mixer_list_size_type;

  typedef const value_list &const_value_list_reference;
  typedef typename value_list::const_iterator value_list_const_iterator;

  typedef const size_list &const_size_list_reference;
  typedef typename size_list::const_iterator size_list_const_iterator;

  TMixerParameters(const_value_list_reference _min, const_value_list_reference _max, const_size_list_reference _size, mixer_size_type _mixerSize, mixer_dimension_size_type _mixerDimension) : mMin(_min), mMax(_max), mSize(_size) {
    mixer_list_size_type mixNumber(1);
    for (size_list_const_iterator sizeIter = this->mSize.begin();sizeIter != this->mSize.end();mixNumber *= ((*sizeIter++) + 2));
    ++mixNumber;
    this->mMixers.resize(mixNumber, mixer_type(_mixerSize, _mixerDimension));
  }
  
  bool withinParameters(const_value_list_reference _val) const {bool within = true; this->getParameterIndex(_val, &within); return within;}
  
  mixer_reference getMixer(const_value_list_reference _val) {return this->mMixers[this->getParameterIndex(_val)];}
  const_mixer_reference getMixer(const_value_list_reference _val) const {return this->mMixers[this->getParameterIndex(_val)];}
  
  const_value_list_reference getMin() const {return this->mMin;}
  const_value_list_reference getMax() const {return this->mMax;}
  const_size_list_reference getSize() const {return this->mSize;}

protected:
  value_list mMin; // mixing class definitions
  value_list mMax; // mixing class definitions
  size_list mSize; // mixing class definitions
  mixer_list mMixers; // mixers for each class
  
  mixer_list_size_type getParameterIndex(const_value_list_reference _val, bool *withinParameters = 0) const {
    mixer_list_size_type index = 0;
    mixer_list_size_type prevParamsSize = 1;
    if (withinParameters) *withinParameters = true;
    value_list_const_iterator minIter = this->mMin.begin();
    value_list_const_iterator maxIter = this->mMax.begin();
    size_list_const_iterator sizeIter = this->mSize.begin();
    value_list_const_iterator valIter = _val.begin();
    while ((minIter != this->mMin.end()) && (maxIter != this->mMax.end()) && (sizeIter != this->mSize.end()) && (valIter != _val.end())) {
      value_type min = *minIter;
      value_type max = *maxIter;
      mixer_list_size_type size = *sizeIter;
      value_type val = *valIter;
      if (val < min) {
	    index += (mixer_list_size_type)(0) * prevParamsSize;
	    if (withinParameters) *withinParameters = false;
      } else if (val >= max) {
	    index += (mixer_list_size_type)(size + 1) * prevParamsSize;
	    if (withinParameters) *withinParameters = false;
      } else {
	    index += (mixer_list_size_type)(((value_type)((val - min) * size) / (max - min)) + 1) * prevParamsSize;
      }
      prevParamsSize *= size;
      ++minIter;
      ++maxIter;
      ++sizeIter;
      ++valIter;
    }
    return index;
  }
};

#endif
