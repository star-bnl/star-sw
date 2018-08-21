//StxDetectorContainer.h
//M.L. Miller (Yale Software)
//02/02/01

/*! \class StxDetectorContainer
  StxDetectorContainer is an interface to the representation of the STAR
  detector
  material.  It is an implementation of the 'facade' pattern.  That is, it is
  meant
  to provide an unchanging interface to the detector model while the actual
  underlying
  reperesntation of the detector itself can change.  In reality, the underlying
  model
  has undergone at least five significant changes, while the public interface of
  StxDetectorContainer has remained constant.
  <p>
  Because there is only one STAR detector, there is also only one instance of
  StxDetectorContainer.  This is guarunteed by implementing
  StxDetectorContainer via
  the singelton design pattern.  See the example below for more information on
  singleton
  access.
  <p>
  StxDetectorContainer behaves as an iterator.  That is, once built it always
  points to
  a valid StxDetector object, which can be accessed via:
  <code>*(StxDetectorContainer::instance())</code>.  One can set the location
  of the
  current detector position via the SetToDetector() methods.
  <p>
  Internally, the STAR detector is modeld as a sorted tree structurere
  implemented
  via StxDetectorNode objects.  Additionally, StxDetectorContainer uses an
  instance of StxCompositeLeafIterator to implement the SetToDetector() methods.
  However, the navigation methods (e.g., MoveIn()) are implemented by using
  the sorted
  nature of the tree structure.  As such, MoveIn(), moveOut(), movePlusPhi(),
  and
  moveMinusPhi() require no searching or expensive computation.  Instead, they
  are
  implemented via simple increment (++) or decrement (--) of STL random access
  iterators
  provided by StxDetectorNode.  Therefore, once StxDetectorContainer is
  Initialized
  for propogation via a SetToDetector() call, navigation should be extremely
  efficient.
  
  \author M.L. Miller (Yale Software)
  
  \warning You do not have to call StxDetectorContainer::Kill() to avoid a
  memory leak. When you call Kill(), you invalidate all pre-existing pointers to
  instance().  Because termination of program execution will automatically
  clean up the heap, it <b>is</b> generally good practice not to call Kill().
*/

/** \example StxDetectorContainer_ex.cxx
 */

#ifndef StxDetectorContainer_HH
#define StxDetectorContainer_HH

#include <vector>
using std::vector;
#include <map>
using std::map;
#include "TNamed.h"
#include "StxFactory.h"
#include "StxDetector.h"
#include "StxDetectorNode.h"
#include "StxMapUtilities.h"
#include "THashList.h"
using namespace std;
using std::vector;
using std::map;
//________________________________________________________________________________
class RPhiLessThan {
 public:
  Bool_t operator()(const StxDetector* lhs, const StxDetector* rhs);
};
//________________________________________________________________________________
class StxDetectorContainer : public TNamed {
 public:
  StxDetectorContainer(const Char_t *name, const Char_t *description) :
    TNamed(name,description),  mRoot(0) {}
    //    mnodefactory(StxToolkit::instance()->DetectorNodeFactory()){}
  virtual ~StxDetectorContainer() {}
  void Initialize();
  ///Builds the detector tree given a pointer to the detector Builder
  virtual void Build();
  /// Get the Root detector node of this tree.
  const StxDetectorNode* Root() const  { return mRoot; }
  /// Add a detector element to the sorted vector
  void Add(StxDetector* det);
  ///This performs a full internal Reset of interator structure.
  void Reset();
  void Unset() {}
  ///Dereference current iterator and return a pointer to current StxDetector.
  StxDetector* operator*() const;
  StxDetector* CurrentDetector() const;
  ///Step in radially in STAR TPC global coordinates.
  Bool_t MoveIn();
  //!Set iterators to the detector nearest to the passed StxDetector pointer.
  void SetToDetector(const StxDetector* layer);
  //!Set iterators to the first detector in the radial layer closest to the
  //!specified position.
  void SetToDetector(Double_t position);
  //!Set iterators to the detector closest to the given position and angle.
  void SetToDetector(Double_t position, Double_t angle);
  void LoopOnDetectors();
  vector<StxDetector*> & Detectors();
  vector<StxDetector*>::const_iterator  begin() const;
  vector<StxDetector*>::const_iterator  end() const;
  vector<StxDetector*>::iterator begin();
  vector<StxDetector*>::iterator end();
  const THashList       *DetectorsHash() const {return _detectorsHash;}
  const StxDetector     *FindDetector(const Char_t *name) const {
    return (const StxDetector *) _detectorsHash->FindObject(name);}
  vector<StxDetectorNode*>::const_iterator beginRadial(const StxDetector * detector)     {
    SetToDetector(detector);
    return mradial_it;//(*mregion)->begin();
  }
  
  vector<StxDetectorNode*>::const_iterator endRadial()  {
    return (*mregion)->end();
  }  

  vector<StxDetectorNode*>::reverse_iterator rbeginRadial(const StxDetector * detector)  {
    SetToDetector(detector);
    vector<StxDetectorNode*>::reverse_iterator it;
    for (it =(*mregion)->rbegin(); 
	 it != (*mregion)->rend(); 
	 ++it)  {
      if (*it==*mradial_it) break;       //if they point to the same node, we are done...
    }
    return  it;
  }

  StxDetectorNode::StxDetectorNodeVector::const_reverse_iterator rendRadial() 
    {
      return (*mregion)->rend();
    }

  StxDetectorNode::StxDetectorNodeVector::const_iterator 
    beginPhi(StxDetectorNode::StxDetectorNodeVector::const_iterator &radialIterator) {
    return (*radialIterator)->begin();
  }
  StxDetectorNode::StxDetectorNodeVector::const_iterator 
    endPhi(StxDetectorNode::StxDetectorNodeVector::const_iterator &radialIterator) {
    return (*radialIterator)->end();
  }
  StxDetectorNode::StxDetectorNodeVector::const_iterator 
    beginPhi(StxDetectorNode::StxDetectorNodeVector::const_reverse_iterator &radialIterator) {
    //cout << " beginPhi: size:"<< (*radialIterator)->ChildCount()<<endl;
    return (*radialIterator)->begin();
  }
  StxDetectorNode::StxDetectorNodeVector::const_iterator 
    endPhi(StxDetectorNode::StxDetectorNodeVector::const_reverse_iterator &radialIterator)  {
    return (*radialIterator)->end();
  }
  virtual void        Print(Option_t *option="") const;

 private:
  Bool_t SetPhi(const StxOrderKey& oldOrder);
  // Utility function for MoveIn(), moveOut() functions
  Bool_t SetPhiIterator(Double_t oldOrder, UInt_t oldNDaughters,
			StxDetectorNode::StxDetectorNodeVector::difference_type oldDistance);
  //!The Root of the tree representation of the detector material.
  StxDetectorNode* mRoot;
  //!An iterator over the leaves of the detector tree.
  //! It is declared on heap for size concerns.
  //!An iterator representing the current region
  //!(mid/forward/backward rapidity, etc).
  StxDetectorNode::StxDetectorNodeVector::const_iterator mregion;
  //!An iterator representing the current radial position.
  StxDetectorNode::StxDetectorNodeVector::const_iterator mradial_it;
  //!An iterator representing the current azimuthal position.
  StxDetectorNode::StxDetectorNodeVector::const_iterator mphi_it;
  //!This is an internal function that is used to set the internal iterator
  //!structure
  //! to point to the position (or position closest to) that given by node.
  void SetToLeaf(StxDetectorNode* node);
  vector<StxDetector *> _sortedDetectors;
  vector<StxDetector *> _selectedDetectors;
  THashList           *_detectorsHash;
  ClassDef(StxDetectorContainer,0)
};
#endif
