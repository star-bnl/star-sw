//CombinationIterator.h
//M.L. Miller (Yale Software)
//03/01

//A class to make combinations of elements stored in vectors, over a variable number of vectors.
//i.e., you can make combinations from points from set 1 with those  in set 2, set 3, ..., set n,
//with a "psuedo-iterator" like interface.  In reality, this is really a poor-man's forward
//iterator, and doesn't properly mimic the STL interface.

//Templated, 3/29/01, MLM

#ifndef CombinationIterator_HH
#define CombinationIterator_HH

#include <vector>

template <class T>
class CombinationIterator
{
 public:
  typedef vector<T> tvector;
  
  //Nested Helper class:
  class VectorEntry
    {
    public:

      VectorEntry(const tvector& vec) : thevec(vec) {};
      
      void init() {
	theit = thevec.begin();
      }

      bool operator==(const VectorEntry& other) {
	return (&thevec==&other.thevec && theit==other.theit);
      }      
 
      //Don't know why, but STL requires this to be defined and implemented if VectorEntry
      //has a reference for a member.  It never gets called
      VectorEntry& operator=(const VectorEntry& other) {
	cout <<"Error:\t Using Invalid Assigninment Operator"<<endl;
	//if (this == &other) return *this;
	//this->thevec = other.thevec;
	//this->theit = other.theit;
	return *this;
      }
      
      const tvector& thevec;
      tvector::const_iterator theit;

    }; //End nested class VectorEntry definition
  
  typedef vector<VectorEntry> vector_entry_vector;
  
 public:

  CombinationIterator() {
    //cout <<"CombinationIterator::CombinationIterator()"<<endl;
  }
  
  virtual ~CombinationIterator() {
    //cout <<"CombinationIterator::~CombinationIterator()"<<endl;
  }
  
  virtual void push_back(const tvector& vec) {
    VectorEntry entry(vec);
    entry.init();
    mvector.push_back(entry);
    return;
  }
  
  virtual void clear() {
    mvector.clear();
    return;
  }
  
  //Return number of possible combinations
  virtual int size() const {
    return msize;
  }
  
  virtual tvector::const_iterator end() const {
    return mvector[ mvector.size()-1 ].thevec.end();
  }
  
  virtual int current() const {
    return mcounter;
  }
  
  virtual void init() {
    mcounter=0;
    setSize();
    for (vector_entry_vector::iterator it=mvector.begin(); it!=mvector.end(); ++it) {
      (*it).init();
    }
    return;
  }
  
  virtual void operator++ () { 
    if (current()==size()-1) {
      ++mcounter;
      return;
    }
    bool incremented=false;
    for (vector_entry_vector::iterator mvectorit=mvector.begin(); mvectorit!=mvector.end() && 
	   !incremented; ++mvectorit) {
      if ((*mvectorit).theit==(*mvectorit).thevec.end()-1) {
	//We're at the end, reset to beginning and increment the next guy
	//cout <<"Not incrementing"<<endl;
	(*mvectorit).init();
      }
      else {
	//cout <<"Incrementing"<<endl;
	++(*mvectorit).theit;
	incremented=true;
	++mcounter;
      }
    }
    return;
  }
  
  //Get a vector containing pts of current combination
  virtual const tvector& operator() () {
    mreturnvector.clear();
    for (vector_entry_vector::const_iterator it=mvector.begin(); it!=mvector.end(); ++it) {
      mreturnvector.push_back( * (*it).theit );
    }
    return mreturnvector;
  }
  
  virtual void print() const {
    cout <<"\n-----------Contents of Mvector---------------"<<endl;
    for (vector_entry_vector::const_iterator it=mvector.begin(); it!=mvector.end(); ++it) {    
      cout <<"\t Contents of next entry \t"<<endl;
      if ( (*it).thevec.size() != 0 ) {
	cout <<"\t Tvector:"<<endl;
	for (tvector::const_iterator it2=(*it).thevec.begin(); it2!=(*it).thevec.end(); ++it2) {
	    //cout <<"\t\t"<<*(*it2)<<endl;
	}
	cout <<"\t Iterator:\t"<<endl;
	//cout <<"\t\t"<<*(*((*it).theit))<<endl;
      }
    }  
    return;
  }
  
 protected:
  
  void setSize() {
    int storesize=1;
    for (vector_entry_vector::const_iterator it=mvector.begin(); it!=mvector.end(); ++it) {
      storesize *= (*it).thevec.size();
    }
    msize=storesize;
    return;
  }

  int msize; //Eager-Cache the endpoint
  int mcounter; //Used to implement increment operator
  vector_entry_vector mvector;
  tvector mreturnvector; //Keep one copy for utilities to avoid constructor calls  
};

#endif
