//CombinationIterator.h
//M.L. Miller (Yale Software)
//03/01

/*A class to make combinations of elements stored in vectors, over a variable number of vectors.
  i.e., you can make combinations from points from set 1 with those  in set 2, set 3, ..., set n,
  with an stl iterator interface.

  This class meets the requirements of an STL input iterator and can thus be used in any STL agloorthim
  that requires only an input iterator (e.g., find, find_if, etc).
*/

//Templated, 3/29/01, MLM

#ifndef CombinationIterator_HH
#define CombinationIterator_HH

#include <vector>
using std::vector;

template <class T>
class CombinationIterator
{
public:
    typedef vector<T> tvector;

    CombinationIterator() {};
    
    virtual ~CombinationIterator() {};
    
    //Add sequence
    void push_back(tvector::const_iterator begin, tvector::const_iterator end);
    
    //Full internal reset
    void clear();
  
    //Return number of possible combinations
    int size() const;
  
    //Access to end, marks termination of forward traversal
    const tvector::const_iterator& end() const;
    
    //Reset iterator to first combination
    void init();
    
    //equality
    bool operator==(const tvector::const_iterator& rhs) const;

    //inequality
    bool operator!=(const tvector::const_iterator& rhs) const;
    
    //prefix
    CombinationIterator& operator++ ();
    
    //postfix
    CombinationIterator operator++(int);
    
    //dereference iterator
    //Get a vector containing pts of current combination
    const tvector& operator* ();
    
    //print utility
    void print() const;
    
private:
    
    //Nested Helper class:
    class VectorEntry
    {
    public:
	VectorEntry(tvector::const_iterator begin, tvector::const_iterator end) 
	    : beginIt(begin), endIt(end) {init();}
	
	void init() {currentIt = beginIt;}
	
	tvector::const_iterator beginIt;
	tvector::const_iterator endIt;
	tvector::const_iterator currentIt;
	
    private:
	VectorEntry(); //not implemented
    }; //End nested class VectorEntry definition
    
    
    typedef vector<VectorEntry> vector_entry_vector;
    
private:
    
    vector_entry_vector mvector;
    tvector mreturnvector; //Keep for dereference, avoid constructor calls  
};

//implementation and inlines

template <class T>
void CombinationIterator<T>::push_back(tvector::const_iterator begin, 
				       tvector::const_iterator end) 
{
    if (end==begin) {
	cout <<"template <class T> StiCombinationIterator::push_back()\tError: vec is empty"<<endl;
    }
    VectorEntry entry(begin, end);
    mvector.push_back(entry);
    return;
}

template <class T>
inline void CombinationIterator<T>::clear() 
{
    mvector.clear();
    mreturnvector.clear();
}

template <class T>
CombinationIterator<T>& CombinationIterator<T>::operator++ ()
{ 
    bool incremented=false;
    for (vector_entry_vector::iterator mvectorit=mvector.begin(); 
	 mvectorit!=mvector.end() && !incremented; ++mvectorit) {
	if ( (mvectorit!=(mvector.end()-1)) && (*mvectorit).currentIt == ((*mvectorit).endIt-1) ) {
	    //We're at the end, reset to beginning and increment the next guy
	    (*mvectorit).init();
	}
	else {
	    //cout <<"Incrementing"<<endl;
	    ++(*mvectorit).currentIt;
	    incremented=true;
	}
    }
    return *this;
}

template <class T>
CombinationIterator<T> CombinationIterator<T>::operator++(int)
{
    CombinationIterator temp = *this;
    ++(*this);
    return temp;
}

template <class T>
int CombinationIterator<T>::size() const 
{
    int storesize=1;
    for (vector_entry_vector::const_iterator it=mvector.begin(); it!=mvector.end(); ++it) {
	storesize *= (*it).endIt-(*it).beginIt;
    }
    return storesize;
}

template <class T>
inline bool CombinationIterator<T>::operator==(const tvector::const_iterator& rhs) const
{
    return (mvector.back().currentIt == rhs);
}

template <class T>
inline bool CombinationIterator<T>::operator!=(const tvector::const_iterator& rhs) const
{
    return !(operator==(rhs));
}

template <class T>
inline const CombinationIterator<T>::tvector::const_iterator&  //return type
CombinationIterator<T>::end() const 
{
    return mvector.back().endIt;
}

template <class T>
void CombinationIterator<T>::init() 
{
    for (vector_entry_vector::iterator it=mvector.begin(); it!=mvector.end(); ++it) {
	(*it).init();
    }
    return;
}

template <class T>
const CombinationIterator<T>::tvector& 
CombinationIterator<T>::operator* () 
{
    mreturnvector.clear();
    for (vector_entry_vector::const_iterator it=mvector.begin(); it!=mvector.end(); ++it) {
	mreturnvector.push_back( * (*it).currentIt );
    }
    return mreturnvector;
}

template <class T>  
void CombinationIterator<T>::print() const 
{
    cout <<"\n-----------Contents of Mvector---------------"<<endl;
    for (vector_entry_vector::const_iterator it=mvector.begin(); it!=mvector.end(); ++it) {    
	cout <<"\t Contents of next entry \t"<<endl;
	cout <<"\t Tvector:"<<endl;
	for (tvector::const_iterator it2=(*it).beginIt; it2!=(*it).endIt; ++it2) {
	    cout <<"\t\t"<<(*it2)<<endl;
	}
	cout <<"\t Iterator:\t"<<endl;
	cout <<"\t\t"<<(*((*it).currentIt))<<endl;
    }  
    return;
}

#endif

