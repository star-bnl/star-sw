//StiFactoryTypes.h

#ifndef StiFactoryTypes_HH
#define StiFactoryTypes_HH

#include <string>
using std::string;

#include "StiObjectFactoryInterface.h"

// Now derive from the interface

//Factory for StiHit objects
class StiHit;

class StiHitFactory : public StiObjectFactoryInterface<StiHit>
{
public:
    StiHitFactory(const string& newName, int original=-1, int 
		  incremental=-1, int maxInc=-1);
    virtual ~StiHitFactory();
    
protected:
    virtual void* makeNewObject() const;
    
private:
    StiHitFactory(); //Not implemented
};

//Factory for StiKalmanTrack objects
class StiKalmanTrack;

class StiKalmanTrackFactory : public 
StiObjectFactoryInterface<StiKalmanTrack>
{
public:
    StiKalmanTrackFactory(const string& newName, int original=-1, int 
			  incremental=-1, int maxInc=-1);
    virtual ~StiKalmanTrackFactory();
    
protected:
    virtual void* makeNewObject() const;
    
private:
    StiKalmanTrackFactory(); //Not implemented
};

//Factory for StiEvaluableTrack objects
class StiEvaluableTrack;

class StiEvaluableTrackFactory : public StiObjectFactoryInterface<StiKalmanTrack>
{
public:
    StiEvaluableTrackFactory(const string& newName, int original=-1, int 
			     incremental=-1, int maxInc=-1);
    virtual ~StiEvaluableTrackFactory();
    
protected:
    virtual void* makeNewObject() const;
    
private:
    StiEvaluableTrackFactory(); //Not implemented
};

//Factory for StiDetector objects
class StiDetector;

class StiDetectorFactory : public StiObjectFactoryInterface<StiDetector>
{
public:
    StiDetectorFactory(const string& newName, int original=-1, int 
		       incremental=-1, int maxInc=-1);
    virtual ~StiDetectorFactory();
    
protected:
    virtual void* makeNewObject() const;
    
private:
    StiDetectorFactory(); //Not implemented
};


//Factory for StiKalmanTrackNode objects
class StiKalmanTrackNode;

class StiKalmanTrackNodeFactory : public StiObjectFactoryInterface<StiKalmanTrackNode>
{
public:
    StiKalmanTrackNodeFactory(const string& newName, int original=-1, int 
			      incremental=-1, int maxInc=-1);
    virtual ~StiKalmanTrackNodeFactory();
    
protected:
    virtual void* makeNewObject() const;
    
private:
    StiKalmanTrackNodeFactory(); //Not implemented
};


//Factory for DetectorNode objects

#ifndef __CINT__
     template <class T> class StiCompositeTreeNode; //forward declare
     typedef StiCompositeTreeNode<StiDetector> StiDetectorNode;
#else
     class StiDetectorNode;
#endif

class StiDetectorNodeFactory : public StiObjectFactoryInterface<StiDetectorNode>
{
public:
    StiDetectorNodeFactory(const string& newName, int original=-1, int 
			   incremental=-1, int maxInc=-1);
    virtual ~StiDetectorNodeFactory();
    
protected:
    virtual void* makeNewObject() const;
    
private:
    StiDetectorNodeFactory(); //Not implemented
};

#endif
