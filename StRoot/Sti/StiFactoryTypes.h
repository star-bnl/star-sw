//StiFactoryTypes.h

#ifndef StiFactoryTypes_HH
#define StiFactoryTypes_HH

#include <string>
using std::string;

#include "StiObjectFactoryInterface.h"

// Now derive from the interface

//Factory for StiHit objects
class StiHit;

/*! \class StiHitFactory
  StiHitFactory is derived from the abstract factory interface class
  StiObjectFactoryInterface<StiHit>.  This class has the sole purpose of
  creating
  an StiHit object on the heap for each call to makeNewObject().
  \author M.L. Miller (Yale Software)
 */
class StiHitFactory : public StiObjectFactoryInterface<StiHit>
{
public:
    ///This is the only constructor available.
    StiHitFactory(const string& newName, int original=-1, int 
		  incremental=-1, int maxInc=-1);

    ///Default destructor
    virtual ~StiHitFactory();
    
protected:
    ///Return a pointer to a new StiHit object on the heap.
    virtual void* makeNewObject() const;
    
private:
    StiHitFactory(); //Not implemented
};

/*! \class StiKalmanTrackFactory
  StiKalmanTrackFactory is derived from the abstract factory interface class
  StiObjectFactoryInterface<StiKalmanTrack>.  This class has the sole purpose
  of creating
  an StiKalmanTrack object on the heap for each call to makeNewObject().
  \author M.L. Miller (Yale Software)
 */
class StiKalmanTrack;

class StiKalmanTrackFactory : public 
StiObjectFactoryInterface<StiKalmanTrack>
{
public:
    ///This is the only constructor available.
    StiKalmanTrackFactory(const string& newName, int original=-1, int 
			  incremental=-1, int maxInc=-1);
    ///Default destructor.
    virtual ~StiKalmanTrackFactory();
    
protected:
    ///Return a pointer to a new StiKalmanTrack object on the heap.
    virtual void* makeNewObject() const;
    
private:
    StiKalmanTrackFactory(); //Not implemented
};

//Factory for StiEvaluableTrack objects

/*! \class StiEvaluableTrackFactory
  StiEvaluableTrackFactory is derived from the abstract factory interface class
  StiObjectFactoryInterface<StiKalmanTrack>.  This class has the sole purpose of
  creating
  an StiEvaluableTrack object on the heap for each call to makeNewObject().
  \author M.L. Miller (Yale Software)
 */

class StiEvaluableTrack;

//class StiEvaluableTrackFactory : public StiObjectFactoryInterface<StiKalmanTrack>
class StiEvaluableTrackFactory : public StiKalmanTrackFactory
{
public:
    ///Thi is the only constructor available.
    StiEvaluableTrackFactory(const string& newName, int original=-1, int 
			     incremental=-1, int maxInc=-1);
    ///Default destructor.
    virtual ~StiEvaluableTrackFactory();
    
protected:
    ///Return a pointer to a new StiEvaluable track on the heap.
    virtual void* makeNewObject() const;
    
private:
    StiEvaluableTrackFactory(); //Not implemented
};

//Factory for StiDetector objects
/*! \class StiDetectorFactory
  StiDetectorFactory is derived from the abstract factory interface class
  StiObjectFactoryInterface<StiDetector>.  This class has the sole purpose of
  creating
  an StiDetector object on the heap for each call to makeNewObject().
  \author M.L. Miller (Yale Software)
 */
class StiDetector;

class StiDetectorFactory : public StiObjectFactoryInterface<StiDetector>
{
public:
    ///This is the only constructor available.
    StiDetectorFactory(const string& newName, int original=-1, int 
		       incremental=-1, int maxInc=-1);
    ///Default destructor.
    virtual ~StiDetectorFactory();
    
protected:
    ///Return a pointer to a new StiDetector object on the heap.
    virtual void* makeNewObject() const;
    
private:
    StiDetectorFactory(); //Not implemented
};


//Factory for StiKalmanTrackNode objects

/*! \class StiKalmanTrackNodeFactory
  StiKalmanTrackNodeFactory is derived from the abstract factory interface class
  StiObjectFactoryInterface<StiKalmanTrackNode>.  This class has the sole
  purpose of creating
  an StiKalmanTrackNode object on the heap for each call to makeNewObject().
  \author M.L. Miller (Yale Software)
 */

class StiKalmanTrackNode;

class StiKalmanTrackNodeFactory
    : public StiObjectFactoryInterface<StiKalmanTrackNode>
{
public:
    ///This is the only constructor available.
    StiKalmanTrackNodeFactory(const string& newName, int original=-1, int 
			      incremental=-1, int maxInc=-1);
    ///Default destructor.
    virtual ~StiKalmanTrackNodeFactory();
    
protected:
    ///Return a pointer to a new StiKalmanTrackNode object on the heap.
    virtual void* makeNewObject() const;
    
private:
    StiKalmanTrackNodeFactory(); //Not implemented
};


//Factory for DetectorNode objects

/*! \class StiDetectorNodeFactory
  StiHitFactory is derived from the abstract factory interface class
  StiObjectFactoryInterface<StiCompositeTreeNode<StiDetector>>.  This class
  has the sole purpose of creating
  an StiCompositeTreeNode<StiDetector> object on the heap for each call to
  makeNewObject().  It also contains some typedefs that are hidden from
  CINT (via preprocessor catch).
  \author M.L. Miller (Yale Software)
 */

#ifndef __CINT__
     template <class T> class StiCompositeTreeNode; //forward declare
     typedef StiCompositeTreeNode<StiDetector> StiDetectorNode;
#else
     class StiDetectorNode;
#endif

class StiDetectorNodeFactory : public StiObjectFactoryInterface<StiDetectorNode>
{
public:
    ///This is the only constructor available
    StiDetectorNodeFactory(const string& newName, int original=-1, int 
			   incremental=-1, int maxInc=-1);
    ///Default destructor
    virtual ~StiDetectorNodeFactory();
    
protected:
    ///Return a pointer to a new StiCompositeTreeNode<StiDetector> on the heap.
    virtual void* makeNewObject() const;
    
private:
    StiDetectorNodeFactory(); //Not implemented
};

#endif
