//StiGuiFactoryTypes.h
//M.L. Miller (Yale Software)
//09/01

#ifndef StiGuiFactoryTypes_HH
#define StiGuiFactoryTypes_HH

#include <string>
using std::string;

#include "Sti/StiObjectFactoryInterface.h"

//Factory for StiRDEvaluableTrack objects
class StiRootDrawableStiEvaluableTrack;

class StiRDEvaluableTrackFactory : public StiObjectFactoryInterface<StiKalmanTrack>
{
public:
    StiRDEvaluableTrackFactory(const string& newName, int original=-1, int 
			       incremental=-1, int maxInc=-1);
    virtual ~StiRDEvaluableTrackFactory();
    
protected:
    virtual void* makeNewObject() const;
    
private:
    StiRDEvaluableTrackFactory(); //Not implemented
};

//Factory for StiRDEvaluableTrack objects
class StiRootDrawableKalmanTrack;

class StiRDKalmanTrackFactory : public StiObjectFactoryInterface<StiKalmanTrack>
{
public:
    StiRDKalmanTrackFactory(const string& newName, int original=-1, int 
			       incremental=-1, int maxInc=-1);
    virtual ~StiRDKalmanTrackFactory();
    
protected:
    virtual void* makeNewObject() const;
    
private:
    StiRDKalmanTrackFactory(); //Not implemented
};

//Factory for StiRDDetector objects
class StiRootDrawableDetector;

class StiRDDetectorFactory : public StiObjectFactoryInterface<StiDetector>
{
public:
    StiRDDetectorFactory(const string& newName, int original=-1, int 
			 incremental=-1, int maxInc=-1);
    virtual ~StiRDDetectorFactory();
    
protected:
    virtual void* makeNewObject() const;
    
private:
    StiRDDetectorFactory(); //Not implemented
};

#endif
