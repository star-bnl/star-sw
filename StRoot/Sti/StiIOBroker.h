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
    return os <<"tphfMinPadrow():\t"<<b.tphfMinPadrow()<<endl
	      <<"tphfMaxPadrow():\t"<<b.tphfMaxPadrow()<<endl
	      <<"etsfLowerBound():\t"<<b.etsfLowerBound()<<endl
	      <<"etsfMaxHits():\t"<<b.etsfMaxHits()<<endl;
}

#endif
