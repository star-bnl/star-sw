//StiRootIOBroker.h
//M.L. Miller (Yale Software)
//11/01

#ifndef StiRootIOBroker_HH
#define StiRootIOBroker_HH

//Root
#include "TObject.h"

//Sti
#include "Sti/StiIOBroker.h"

class StiRootIOBroker : public StiIOBroker
{
public:
    
    /* acronyms:
       TPHF: TpcPadrowHitFilter
       ETSF: EvaluableTrackSeedFinder
    */
    
    //You should only call this once.
    //Cint forces us to violate singleton pattern.
    StiRootIOBroker();

    //Evaluable Track Seed Finder
    
    virtual void setTPHFMinPadrow(unsigned int);
    virtual unsigned int tphfMinPadrow() const;
    virtual void setTPHFMaxPadrow(unsigned int);
    virtual unsigned int tphfMaxPadrow() const;

    virtual void setETSFLowerBound(unsigned int);
    virtual unsigned int etsfLowerBound() const;
    virtual void setETSFMaxHits(unsigned int);
    virtual unsigned int etsfMaxHits() const;

protected:
    friend class StiIOBroker;
    
    virtual ~StiRootIOBroker();

    unsigned int mVal;
    unsigned int mTPHFMinPadrow;
    unsigned int mTPHFMaxPadrow;
    unsigned int mETSFLowerBound;
    unsigned int mETSFMaxHits;

private:
    ClassDef(StiRootIOBroker, 1)
};

//inlines
inline void StiRootIOBroker::setETSFLowerBound(unsigned int val)
{
    mETSFLowerBound=val;
    notify();
}

inline unsigned int StiRootIOBroker::etsfLowerBound() const
{
    return mETSFLowerBound;
}

inline void StiRootIOBroker::setETSFMaxHits(unsigned int val)
{
    mETSFMaxHits=val;
    notify();
}

inline unsigned int StiRootIOBroker::etsfMaxHits() const
{
    return mETSFMaxHits;
}

inline void StiRootIOBroker::setTPHFMinPadrow(unsigned int val)
{
    mTPHFMinPadrow=val;
    notify();
}

inline unsigned int StiRootIOBroker::tphfMinPadrow() const
{
    return mTPHFMinPadrow;
}

inline void StiRootIOBroker::setTPHFMaxPadrow(unsigned int val)
{
    mTPHFMaxPadrow=val;
    notify();
}

inline unsigned int StiRootIOBroker::tphfMaxPadrow() const
{
    return mTPHFMaxPadrow;
}

#endif
