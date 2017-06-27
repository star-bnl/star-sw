//StiDetectorContainer.h
//M.L. Miller (Yale Software)
//02/02/01

/*! \class StiDetectorContainer
  StiDetectorContainer is an interface to the representation of the STAR
  detector
  material.  It is an implementation of the 'facade' pattern.  That is, it is
  meant
  to provide an unchanging interface to the detector model while the actual
  underlying
  reperesntation of the detector itself can change.  In reality, the underlying
  model
  has undergone at least five significant changes, while the public interface of
  StiDetectorContainer has remained constant.
  <p>
  Because there is only one STAR detector, there is also only one instance of
  StiDetectorContainer.  This is guarunteed by implementing
  StiDetectorContainer via
  the singelton design pattern.  See the example below for more information on
  singleton
  access.
  <p>
  StiDetectorContainer behaves as an iterator.  That is, once built it always
  points to
  a valid StiDetector object, which can be accessed via:
  <code>*(StiDetectorContainer::instance())</code>.  One can set the location
  of the
  current detector position via the SetToDetector() methods.
  <p>
  Internally, the STAR detector is modeld as a sorted tree structurere
  implemented
  via StiDetectorNode objects.  Additionally, StiDetectorContainer uses an
  instance of StiCompositeLeafIterator to implement the SetToDetector() methods.
  However, the navigation methods (e.g., MoveIn()) are implemented by using
  the sorted
  nature of the tree structure.  As such, MoveIn(), moveOut(), movePlusPhi(),
  and
  moveMinusPhi() require no searching or expensive computation.  Instead, they
  are
  implemented via simple increment (++) or decrement (--) of STL random access
  iterators
  provided by StiDetectorNode.  Therefore, once StiDetectorContainer is
  Initialized
  for propogation via a SetToDetector() call, navigation should be extremely
  efficient.
  
  \author M.L. Miller (Yale Software)
  
  \warning You do not have to call StiDetectorContainer::Kill() to avoid a
  memory leak. When you call Kill(), you invalidate all pre-existing pointers to
  instance().  Because termination of program execution will automatically
  clean up the heap, it <b>is</b> generally good practice not to call Kill().
*/

/** \example StiDetectorContainer_ex.cxx
 */

#ifndef StiDetectorContainer_HH
#define StiDetectorContainer_HH

#include <vector>
using std::vector;
#include <map>
using std::map;
#include "TNamed.h"
#include "StiFactory.h"
#include "StiDetector.h"
#include "StiDetectorNode.h"
#include "StiMapUtilities.h"
#include "THashList.h"
using namespace std;
using std::vector;
using std::map;
//________________________________________________________________________________
class RPhiLessThan {
 public:
  Bool_t operator()(const StiDetector* lhs, const StiDetector* rhs);
};
//________________________________________________________________________________
class StiDetectorContainer : public TNamed {
 public:
  StiDetectorContainer(const Char_t *name, const Char_t *description) :
    TNamed(name,description),  mRoot(0) {}
    //    mnodefactory(StiToolkit::instance()->DetectorNodeFactory()){}
  virtual ~StiDetectorContainer() {}
  void Initialize();
  ///Builds the detector tree given a pointer to the detector Builder
  virtual void Build();
  /// Get the Root detector node of this tree.
  const StiDetectorNode* Root() const  { return mRoot; }
  /// Add a detector element to the sorted vector
  void Add(StiDetector* det);
  ///This performs a full internal Reset of interator structure.
  void Reset();
  void Unset() {}
  ///Dereference current iterator and return a pointer to current StiDetector.
  StiDetector* operator*() const;
  StiDetector* CurrentDetector() const;
  ///Step in radially in STAR TPC global coordinates.
  Bool_t MoveIn();
  //!Set iterators to the detector nearest to the passed StiDetector pointer.
  void SetToDetector(const StiDetector* layer);
  //!Set iterators to the first detector in the radial layer closest to the
  //!specified position.
  void SetToDetector(Double_t position);
  //!Set iterators to the detector closest to the given position and angle.
  void SetToDetector(Double_t position, Double_t angle);
  void LoopOnDetectors();
  vector<StiDetector*> & Detectors();
  vector<StiDetector*>::const_iterator  begin() const;
  vector<StiDetector*>::const_iterator  end() const;
  vector<StiDetector*>::iterator begin();
  vector<StiDetector*>::iterator end();
  const THashList       *DetectorsHash() const {return _detectorsHash;}
  const StiDetector     *FindDetector(const Char_t *name) const {
    return (const StiDetector *) _detectorsHash->FindObject(name);}
  vector<StiDetectorNode*>::const_iterator beginRadial(const StiDetector * detector)     {
    SetToDetector(detector);
    return mradial_it;//(*mregion)->begin();
  }
  
  vector<StiDetectorNode*>::const_iterator endRadial()  {
    return (*mregion)->end();
  }  

  vector<StiDetectorNode*>::reverse_iterator rbeginRadial(const StiDetector * detector)  {
    SetToDetector(detector);
    vector<StiDetectorNode*>::reverse_iterator it;
    for (it =(*mregion)->rbegin(); 
	 it != (*mregion)->rend(); 
	 ++it)  {
      if (*it==*mradial_it) break;       //if they point to the same node, we are done...
    }
    return  it;
  }

  StiDetectorNode::StiDetectorNodeVector::const_reverse_iterator rendRadial() 
    {
      return (*mregion)->rend();
    }

  StiDetectorNode::StiDetectorNodeVector::const_iterator 
    beginPhi(StiDetectorNode::StiDetectorNodeVector::const_iterator &radialIterator) {
    return (*radialIterator)->begin();
  }
  StiDetectorNode::StiDetectorNodeVector::const_iterator 
    endPhi(StiDetectorNode::StiDetectorNodeVector::const_iterator &radialIterator) {
    return (*radialIterator)->end();
  }
  StiDetectorNode::StiDetectorNodeVector::const_iterator 
    beginPhi(StiDetectorNode::StiDetectorNodeVector::const_reverse_iterator &radialIterator) {
    //cout << " beginPhi: size:"<< (*radialIterator)->ChildCount()<<endl;
    return (*radialIterator)->begin();
  }
  StiDetectorNode::StiDetectorNodeVector::const_iterator 
    endPhi(StiDetectorNode::StiDetectorNodeVector::const_reverse_iterator &radialIterator)  {
    return (*radialIterator)->end();
  }
  virtual void        Print(Option_t *option="") const;

 private:
  Bool_t SetPhi(const StiOrderKey& oldOrder);
  // Utility function for MoveIn(), moveOut() functions
  Bool_t SetPhiIterator(Double_t oldOrder, UInt_t oldNDaughters,
			StiDetectorNode::StiDetectorNodeVector::difference_type oldDistance);
  //!The Root of the tree representation of the detector material.
  StiDetectorNode* mRoot;
  //!An iterator over the leaves of the detector tree.
  //! It is declared on heap for size concerns.
  //!An iterator representing the current region
  //!(mid/forward/backward rapidity, etc).
  StiDetectorNode::StiDetectorNodeVector::const_iterator mregion;
  //!An iterator representing the current radial position.
  StiDetectorNode::StiDetectorNodeVector::const_iterator mradial_it;
  //!An iterator representing the current azimuthal position.
  StiDetectorNode::StiDetectorNodeVector::const_iterator mphi_it;
  //!This is an internal function that is used to set the internal iterator
  //!structure
  //! to point to the position (or position closest to) that given by node.
  void SetToLeaf(StiDetectorNode* node);
  vector<StiDetector *> _sortedDetectors;
  vector<StiDetector *> _selectedDetectors;
  THashList           *_detectorsHash;
  ClassDef(StiDetectorContainer,0)
};
#endif
