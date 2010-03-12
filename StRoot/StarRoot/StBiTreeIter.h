#ifndef STAR_STBITREEITER
#define STAR_STBITREEITER
#include <StBiTree.h>
#include <iterator>
// using namespace std;

namespace std {
template <> 
template <class T>
class iterator<input_iterator_tag, class StBiTree<T> > {    
 private:
    StBiTree<T>  *p;
    int  fCurrentBranch;
    iterator<input_iterator_tag, class StBiTree<T> > *fCurrentIterator;
    iterator& operator++(int) { return *this; }
 public:
  iterator():p(0), fCurrentBranch(StBiTree<T>::kLeaf),fCurrentIterator()  {}
  iterator(StBiTree<T>* x):p(x), fCurrentBranch(p->WhereAmI()),fCurrentIterator()  {}
  iterator(const iterator& mit) : p(mit.p), fCurrentBranch(mit.fCurrentBranch), 
      fCurrentIterator(mit.fCurrentIterator ? new iterator(*(mit.fCurrentIterator)) : 0)  {}
  ~iterator() { delete fCurrentIterator; fCurrentIterator = 0; }
  iterator& operator++() {
     if (p) {
        switch (fCurrentBranch) {
        case StBiTree<T>::kLeft:  break;
        case StBiTree<T>::kRight: break;
        case StBiTree<T>::kLeaf: fCurrentBranch = StBiTree<T>::kLeft; break;
        }
        if (!fCurrentIterator) fCurrentIterator = p->Branch(fCurrentBranch)->begin();
        else if (*fCurrentIterator != p->end() )  ++(*fCurrentIterator);
        if ( (*fCurrentIterator != p->end() ) && fCurrentBranch == StBiTree<T>::kLeft ) {
           delete fCurrentIterator;  fCurrentIterator = 0;
           fCurrentBranch = StBiTree<T>::kRight;
           fCurrentIterator =  p->Branch(fCurrentBranch)->begin();
        }
     }
     return *this;
  }
  bool operator==(const iterator& rhs) {return p==rhs.p;}
  bool operator!=(const iterator& rhs) {return p!=rhs.p;}
  StBiTree<T>& operator*() {
    return fCurrentIterator ?  fCurrentIterator->operator*():
                           *(p->Branch(fCurrentBranch));
  }
  iterator& next(int direction) {
     switch (direction) {
        case StBiTree<T>::kLeft:  break;
        case StBiTree<T>::kRight: break;
        case StBiTree<T>::kLeaf: fCurrentBranch = StBiTree<T>::kLeft; break;
     }
     if (!fCurrentIterator) fCurrentIterator = p->Branch(fCurrentBranch)->begin();
     else if (*fCurrentIterator != p->end() )  ++(*fCurrentIterator);
     if ( (*fCurrentIterator != p->end() ) && fCurrentBranch == StBiTree<T>::kLeft ) {
        delete fCurrentIterator;  fCurrentIterator = 0;
        fCurrentBranch = StBiTree<T>::kRight;
        fCurrentIterator =  p->Branch(fCurrentBranch)->begin();
     }
     return *this;
  }

};

}
#endif
