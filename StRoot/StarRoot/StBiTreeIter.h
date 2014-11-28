#ifndef STAR_STBITREEITER
#define STAR_STBITREEITER
#include <StBiTree.h>
#include <iterator>
// using namespace std;

namespace std {
template <class T> class StBiTree;

template <> 
template <class T>
class iterator<input_iterator_tag, class StBiTree<T> > {    
 private:
    StBiTree<T>  *p;
    int  fCurrentBranch;
    iterator<input_iterator_tag, class StBiTree<T> > *fCurrentIterator;
    iterator& operator++(int) { return *this; }
 public:
  iterator():p(), fCurrentBranch(StBiTree<T>::kLeaf),fCurrentIterator()  {}
  iterator(StBiTree<T>* x):p(x), fCurrentBranch(StBiTree<T>::kLeaf),fCurrentIterator()  
  {
     if (p && !(p->IsLeaf() || p->IsEmpty() )  ) fCurrentBranch = 0;
  }
  iterator(const iterator& mit) : p(mit.p), fCurrentBranch(mit.fCurrentBranch), 
      fCurrentIterator(mit.fCurrentIterator ? new iterator(*(mit.fCurrentIterator)) : 0)  {}
  ~iterator() { delete fCurrentIterator; fCurrentIterator = 0; }
  bool operator==(const iterator& rhs) {return p==rhs.p;}
  bool operator!=(const iterator& rhs) {return p!=rhs.p;}
  StBiTree<T>& operator*() {
    return fCurrentIterator ?  fCurrentIterator->operator*(): 
       (fCurrentBranch == StBiTree<T>::kLeaf) ? 
       *(p) 
       :
       *(p->Branch(fCurrentBranch? StBiTree<T>::kRight:StBiTree<T>::kLeft));
  }
  iterator& next(int direction) {
     if (p) {
        if (fCurrentBranch == StBiTree<T>::kLeaf ) {
           delete fCurrentIterator;  fCurrentIterator = 0;
        } else {
           int newDirection = direction ? StBiTree<T>::kRight: StBiTree<T>::kLeft;
           if (newDirection != fCurrentBranch) {
              delete fCurrentIterator;  fCurrentIterator = 0;
              fCurrentBranch = newDirection;
           }
           if (!fCurrentIterator) {
              StBiTree<T> *b = fCurrentBranch ? p->Right() : p->Left();
              if (b) fCurrentIterator = b->CreateIterator();
           } else if (*fCurrentIterator != p->end() )  fCurrentIterator->next();
           if ( fCurrentIterator && (*fCurrentIterator == p->end() ) && fCurrentBranch == StBiTree<T>::kLeft ) {
              delete fCurrentIterator;  fCurrentIterator = 0;
              fCurrentBranch = StBiTree<T>::kRight;
              StBiTree<T> *b = p->Right();
              fCurrentIterator =  b? b->CreateIterator():0;
           }
        }
        if (!fCurrentIterator) p=0; // end iteration
     }
     return *this;
  }
  iterator& next()       {   return next(fCurrentBranch);  }
  iterator& operator++() {   return next();  }

};

}
#endif
