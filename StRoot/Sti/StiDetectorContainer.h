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
  current detector position via the setToDetector() methods.
  <p>
  Internally, the STAR detector is modeld as a sorted tree structurere
  implemented
  via StiCompositeTreeNode objects.  Additionally, StiDetectorContainer uses an
  instance of StiCompositeLeafIterator to implement the setToDetector() methods.
  However, the navigation methods (e.g., moveIn()) are implemented by using
  the sorted
  nature of the tree structure.  As such, moveIn(), moveOut(), movePlusPhi(),
  and
  moveMinusPhi() require no searching or expensive computation.  Instead, they
  are
  implemented via simple increment (++) or decrement (--) of STL random access
  iterators
  provided by StiCompositeTreeNode.  Therefore, once StiDetectorContainer is
  initialized
  for propogation via a setToDetector() call, navigation should be extremely
  efficient.
  
  \author M.L. Miller (Yale Software)

  \warning You do not have to call StiDetectorContainer::kill() to avoid a
  memory leak. When you call kill(), you invalidate all pre-existing pointers to
  instance().  Because termination of program execution will automatically
  clean up the heap, it <b>is</b> generally good practice not to call kill().
*/

/** \example StiDetectorContainer_ex.cxx
 */

#ifndef StiDetectorContainer_HH
#define StiDetectorContainer_HH

#include <vector>
using std::vector;
#include <map>
using std::map;
#include "StiMapUtilities.h"

#include "StiFactoryTypes.h"
#include "StiObjectFactoryInterface.h"
#include "StiCompositeTreeNode.h"
#include "StiCompositeLeafIterator.h"

using std::map;

class StiDetector;
class StiMaterial;
class Messenger;

class StiDetectorContainer
{
public:

    ///We inclue this to avoid compiler warnings generated because of the
    /// singleton design pattern.
    friend class nobody;

    ///Access to singleton instance
    static StiDetectorContainer* instance();

    ///Kill the current instance.  Use this wisely!
    static void kill();
    
    ///This calls builds the detector tree given a pointer to the necessary
    ///factories.
    virtual void buildDetectors(StiObjectFactoryInterface<StiDetectorNode>*
				nodefactory,
				StiObjectFactoryInterface<StiDetector>*
				detfactory);

    //Action
    
    ///This performs a full internal reset of interator structure.
    void reset(); 
    
    //Navigation

    ///Dereference current iterator and return a pointer to current StiDetector.
    StiDetector* operator*() const;
    
    ///Step out radially in STAR TPC global coordinates.
    void moveOut();
    
    ///Step in radially in STAR TPC global coordinates.
    void moveIn();

    ///Step around in increasing phi.
    void movePlusPhi();

    ///Step around in decreasing phi.
    void moveMinusPhi();

    ///Set iterators to the detector nearest to the passed StiDetector pointer.
    void setToDetector(StiDetector* layer);
    
    ///Set iterators to the first detector in the radial layer closest to the
    ///specified position.
    void setToDetector(double position);
    
    ///Set iterators to the detector closest to the given position and angle.
    void setToDetector(double position, double angle);

    //Utilities

    ///This is not currently implemented.
    void print() const;
    
private:

    ///The root of the tree representation of the detector material.
    data_node* mroot;

    ///An iterator representing the current region
    ///(mid/forward/backward rapidity, etc).
    data_node* mregion;

    ///An iterator over the leaves of the detector tree.
    /// It is declared on heap for size concerns.
    StiCompositeLeafIterator<data_t>* mLeafIt; 

    //A message stream
    Messenger& mMessenger;
    
    ///An iterator representing the current radial position.
    data_node_vec::const_iterator mradial_it;

    ///An iterator representing the current azimuthal position.
    data_node_vec::const_iterator mphi_it;

private:

    ///This is an internal function that is used to set the internal iterator
    ///structure
    /// to point to the position (or position closest to) that given by node.
    void setToLeaf(data_node* node);
    

private:

    //Singleton Management

    ///Private destructor: implementation of singleton pattern.
    virtual ~StiDetectorContainer();

    ///Private destructor: implementation of singleton pattern.
    StiDetectorContainer();

    ///Static pointer to StiDetectorContainer: implementation of singelton
    ///pattern.
    static StiDetectorContainer* sinstance;
};

#endif
