//StiIOBroker.h
//M.L. Miller (Yale Software)
//11/01

#ifndef StiIOBroker_HH
#define StiIOBroker_HH

#include "SubjectObserver.h"

class StiIOBroker : public Subject
{
public:
    friend class nobody;

    //enum StiIOBrokerType {kUndefined=0, kRootIO=1, kMySql=2};
    static StiIOBroker* instance();

public:

    //Main Program flow

    ///Are we using simulated data?
    virtual void setSimulated(bool) = 0;
    virtual bool simulated() const = 0;

    ///Are we running in GUI version?
    virtual void setUseGui(bool) = 0;
    virtual bool useGui() const = 0;

    ///Toggle the track find/fit option, True->doFit, false->doFind
    virtual void setDoTrackFit(bool) = 0;
    virtual bool doTrackFit() const = 0;

    ///SeedFinderType
    enum SeedFinderType {kUndefined=0, kComposite=1, kEvaluable=2};
    virtual void setSeedFinderType(SeedFinderType) = 0;
    virtual SeedFinderType seedFinderType() const = 0;

    //Evaluable Seed Finder IO

    virtual void setTPHFMinPadrow(unsigned int) = 0;
    virtual unsigned int tphfMinPadrow() const = 0;
    virtual void setTPHFMaxPadrow(unsigned int) = 0;
    virtual unsigned int tphfMaxPadrow() const = 0;

    virtual void setETSFLowerBound(unsigned int) = 0;
    virtual unsigned int etsfLowerBound() const = 0;
    virtual void setETSFMaxHits(unsigned int) = 0;
    virtual unsigned int etsfMaxHits() const = 0;
    
protected:
    //singleton management
    StiIOBroker();
    virtual ~StiIOBroker();
    static StiIOBroker* sInstance;

    friend ostream& operator<<(ostream&, const StiIOBroker&);
};

inline ostream& operator<<(ostream& os, const StiIOBroker& b)
{
    return os <<"simulated():\t"<<b.simulated()<<endl
	      <<"useGui():\t"<<b.useGui()<<endl
	      <<"doTrackFit():\t"<<b.doTrackFit()<<endl
	      <<"seedFinderType():\t"<<static_cast<int>(b.seedFinderType())<<endl
	      <<"tphfMinPadrow():\t"<<b.tphfMinPadrow()<<endl
	      <<"tphfMaxPadrow():\t"<<b.tphfMaxPadrow()<<endl
	      <<"etsfLowerBound():\t"<<b.etsfLowerBound()<<endl
	      <<"etsfMaxHits():\t"<<b.etsfMaxHits()<<endl;
}

#endif
