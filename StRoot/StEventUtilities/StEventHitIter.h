// Author: Victor Perev   08/04/01
#ifndef ROOT_StEventHitIter_HH
#define ROOT_StEventHitIter_HH


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventHitIter                                                       //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "Rtypes.h"
#include "TObject.h"
#include "StEnumerations.h"
#include "vector"
class StEvent;
class StHit;


//namespace QWERTY {
//..............................................................................

/*!
 * \class StHitIter
 * \brief StHitIter is an abstract base class, establishing the interface between hit collections in StEvent
 *        and the Stv track reconstruction machinery.  
 *
 * Integrating hits into the Stv framework begins here.  The devolper needs to implement one or more 
 * iterators based on StHitIter. 
 *
 * <ul>
 *
 *    <li>The simple case:  Iteration over a container of hits
 *
 *        When iterating over a container of hits, you only need to implement two methods:
 *        StYourHitIter::GetObject() and StYourHitIter::GetSize().  Indeed, since these
 *        have no implementation in StHitIter, you will not be able to compile your code
 *        without them.
 *
 *        GetObject(I) should return the Ith hit in your collection.
 *        GetSize()    should return the number of hits in your collection.
 *
 *        In addition, if this is your top-level iterator you should override the behavior
 *        of DetectorId() and implement Reset().  DetectorId() needs to return the detector
 *        id of your detector.  Reset needs to take a pointer to an object.  If the object
 *        exists, it is recast to StEvent and your hit collection is obtained from it.  Then
 *        the base method StHitIter::Reset( ) is invoked on your collection.  (Basically you
 *        follow the example provided by StTpcHitIter::Reset(), substituting your classes for
 *        the TPC ones).
 *
 *    <li>The hard case:  Iteration over a container of hit containers 
 *
 *        Slightly harder is the case where your hit collections are nested, e.g. you have
 *        a top level collection which contains sector collections, which contain your 
 *        actual hits (or even another collection).
 *
 *        To handle this case, you will need to implement an iterator for each of the
 *        nested collections.
 *
 *        Probably the best way to describe this process is by example.  Examine StTpcHitIter.
 *
 *        The TPC collections are nested three deep:  tpc, sectors, padrows.
 *
 *        The top-level TPC hit iterator is StTpcHitIter.  It implements four methods, as
 *        described for the simple case above.
 *        <ul>
 *          <li> Reset()
 *          <li> GetObject(idx)
 *          <li> GetSize()
 *          <li> DetectorId()
 *        </ul>
 *        Examining the constructor for StTpcHitIter, we see that two hit iterators are
 *        created.  First a new StTpcSectorHitIter is created.  This is registered with StHitIter
 *        using the SetDowIter method.  This tells StTpcHitIter that it will use a nested iterator.
 *        After StTpcSectorHitIter is created, we create StTpcPadrowIter and register it with
 *        StTpcSectorHitIter.   
 *
 *        StTpcHitIter::GetObject( Int_t idx ) returns the sector collection.
 *        StTpcSectorIter::GetObject( Int_t idx ) returns the padrow collection
 *        StTpcPadrowHitIter::GetObject( Int_t idx ) returns a hit within the padrow
 *
 *        The Size() methods return the size of the corresponding collections.
 *
 * </ul>
 *
 * \author Victor Perev <perev@bnl.gov>
 * 
 */

class StHitIter : public TObject
{
public: 
    		StHitIter();
virtual        ~StHitIter();
virtual const TObject *Reset(const TObject *cont);
/// Increments the iterator, returning the next object in the collection
virtual const TObject *operator++();
/// Gets the current object in the collection
virtual const TObject *Get () const;
/// Gets an object at a specified position in the collection
virtual const TObject *GetObject (int idx) const=0;
/// Returns the number of entries in the collection
virtual           int  GetSize   ()        const=0;
/// Returns the STAR ID of the detector
virtual  StDetectorId  DetectorId() const {return kUnknownId;}
/// For the case of nested collections, sets the iterator over another collection.
                 void  SetDowIter(StHitIter *it) {fDowIter=it;}
virtual          void  UPath(ULong64_t &upath) const;
protected:

const TObject *fCont;
StHitIter  *fDowIter;
int fJIter;
int fNIter;

ClassDef(StHitIter,0)//

};
//..............................................................................
class StHitIterGroup : public StHitIter {
public: 
    		StHitIterGroup();
virtual        ~StHitIterGroup();
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *operator++();
virtual const TObject *Get () const;
virtual const StHit *operator*() const 		{return (StHit*)Get();}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize   () const 	{return fGroup.size();};
virtual StDetectorId DetectorId()   const 	{return fDetectorId;}
                void UPath(ULong64_t &upa) const;
              UInt_t UPath() const;
virtual void Add(StHitIter* iter);
protected:
mutable StDetectorId fDetectorId;
std::vector<StHitIter*> fGroup;

ClassDef(StHitIterGroup,0)//

};

//..............................................................................

/*!
  \class StEventHitIter
  \brief StEventHitIter is the top level hook, providing hits to the Stv tracking code.

  StEventHitIter is the top level hook, providing hits to the Stv tracking framework.  To expose a
  new detector to the framework, developers need to do two things.  First, the need to create a
  hit iterator class derived from StHitIter.  This will iterate over a collection of hits, returning
  each one in turn in an arbitrary order.  The second thing is that logic should be added to the
  AddDetector method of StEventHitIter, creating the concrete hit iterator for the new detector.
  
*/

class StEventHitIter : public StHitIterGroup {
public:
  StEventHitIter(const TObject *ev=0){if (ev) Reset(ev);}
  virtual ~StEventHitIter(){;}
  int   AddDetector(StDetectorId detId);
private:
ClassDef(StEventHitIter,0)//

};

//}// QWERTY
#endif //ROOT_StEventHelper
