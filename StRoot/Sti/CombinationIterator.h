//CombinationIterator.h
//M.L. Miller (Yale Software)
//03/01

/*! \class CombinationIterator
  A class to make combinations of elements stored in vectors,
  over a variable number of vectors.
  i.e., you can make combinations from points from set 1 with
  those  in set 2, set 3, ..., set n,
  with an stl iterator interface.  This class meets the requirements
  of an STL input iterator and can thus be used in any STL agloorthim
  that requires only an input iterator (e.g., find, find_if, etc).
  Additionally, we define validity via bool valid() s.t. the iterator
  is vailid iff each set is non-empty.
  <p>
  Currently, the iterator works only with sets that are defined by iterators
  int std::vector<T>.  However, with support of templated member functions
  one could easily extend the calss to deal with ranges defined by iterators
  from any type of STL container.
  <p>
  As usual, validity is defined via [begin,end).
  
  \author M.L. Miller (Yale Software)
*/

/*! \example CombinationIterator_ex.cxx */

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

    ///Defualt Constructor
    CombinationIterator() {}; 

    ///Default Destructor
    virtual ~CombinationIterator() {};
    
    ///Add sequence
    void push_back(tvector::const_iterator begin, tvector::const_iterator end);
    
    ///Full internal reset
    void clear();
  
    ///Return number of possible combinations
    int size() const;

    ///check that each range of points has size>0.
    bool valid() const;
  
    ///Access to end, marks termination of forward traversal
    const tvector::const_iterator& end() const;
    
    ///Reset iterator to first combination
    void init();
    
    ///equality
    bool operator==(const tvector::const_iterator& rhs) const;

    ///inequality
    bool operator!=(const tvector::const_iterator& rhs) const;
    
    ///prefix
    CombinationIterator& operator++ ();
    
    ///postfix
    CombinationIterator operator++(int);
    
    ///dereference iterator
    const tvector& operator* ();
    
    ///print utility
    void print() const;
    
private:
    
    ///Nested Helper class:
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
	VectorEntry(); ///not implemented
    }; //End nested class VectorEntry definition
    
    
    typedef vector<VectorEntry> vector_entry_vector;
    
private:
    
    vector_entry_vector mvector;

    ///Keep for dereference, avoid constructor calls
    tvector mreturnvector;
};

//implementation and inlines

/*! A sequence is defined to be valid over the range [begin,end).
  If the condition begin==end arises, this sequence is deemed to be
  invalid and invalidates the instance of CombinationIterator (see
  method valid()).
*/
template <class T>
void CombinationIterator<T>::push_back(tvector::const_iterator begin, 
				       tvector::const_iterator end) 
{
    if (end==begin) {
	cout <<"StiCombinationIterator<T>::push_back()\tError:";
	cout <<"range is empty is empty"<<endl;
    }
    VectorEntry entry(begin, end);
    mvector.push_back(entry);
    return;
}

/*! A call to clear() removes all sequences that have been added to the
  iterator via the push_back() method.  That is, once clear() has been called
  one must again add all ranges via push_back().
*/
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
	if ( (mvectorit!=(mvector.end()-1)) &&
	     (*mvectorit).currentIt == ((*mvectorit).endIt-1) ) {
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

/*! This postfix version of operator++ is implemented via a call to the
  prefi version.  It should be noted that a call to the postfix version
  requires (as usual) a constructor call to CombinationIterator, and can
  thus be significantly less efficient than the prefix version.
*/
template <class T>
CombinationIterator<T> CombinationIterator<T>::operator++(int)
{
    CombinationIterator temp = *this;
    ++(*this);
    return temp;
}

/*! Size is defined by summation of end-begin for each range that has
  been added to the iterator.
*/
template <class T>
int CombinationIterator<T>::size() const 
{
    int storesize=1;
    for (vector_entry_vector::const_iterator it=mvector.begin();
    it!=mvector.end(); ++it) {
	storesize *= (*it).endIt-(*it).beginIt;
    }
    return storesize;
}

/*! We provied an equality operator that takes a std::vector<T>::const_iterator
  as an arguement.
 */
template <class T>
inline bool
CombinationIterator<T>::operator==(const tvector::const_iterator& rhs) const
{
    return (mvector.back().currentIt == rhs);
}

/*! We provide an inequality operator that takes a
  std::vector<T>::const_iterator as an arguement.
 */
template <class T>
inline bool CombinationIterator<T>::operator!=(const tvector::const_iterator& rhs) const
{
    return !(operator==(rhs));
}

/*! We provide a const_iterator that marks the point one past the last valid
  combination of the CombinationIterator.
*/
template <class T>
inline const CombinationIterator<T>::tvector::const_iterator&  //return type
CombinationIterator<T>::end() const 
{
    return mvector.back().endIt;
}

/*! Init resets the iterator to the first possible combination.  That is,
  after forward traversal one can return to to the beginning of the traversal
  via a call to init().  
 */
template <class T>
void CombinationIterator<T>::init() 
{
    for (vector_entry_vector::iterator it=mvector.begin(); it!=mvector.end();
	 ++it) {
	(*it).init();
    }
    return;
}

/*! Validity is defined s.t. end_i!=begin_i for all ranges i.  If one does
  not test the validity of each range i, then an attempt to dereference the
  CombinationIterator will result in dereferencing an invalid stl iterator.
*/
template <class T>
bool CombinationIterator<T>::valid() const
{
    bool ok = true;
    for (vector_entry_vector::const_iterator it=mvector.begin();
	 it!=mvector.end(); ++it) {
	if ( (*it).endIt - (*it).beginIt <=1) {
	    ok = false;
	}
    }
    return ok;
}

/*! Dereferencing the iterator returns a reference to a vector that containes
  the current possible combination.  This reference points to a private
  vector member of CombinationIterator.
*/
template <class T>
const CombinationIterator<T>::tvector& 
CombinationIterator<T>::operator* () 
{
    mreturnvector.clear();
    for (vector_entry_vector::const_iterator it=mvector.begin();
	 it!=mvector.end(); ++it) {
	mreturnvector.push_back( * (*it).currentIt );
    }
    return mreturnvector;
}

/*! A call to print streams each range known to the iterator to the screen
 */
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

