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
    
    //You should only call this once.
    //Cint forces us to violate singleton pattern.
    StiRootIOBroker();

    //Main Program flow

    ///Are we using simulated data?
    virtual void setSimulated(bool);
    virtual bool simulated() const;

    ///Are we running in GUI version?
    virtual void setUseGui(bool);
    virtual bool useGui() const;

    ///Toggle the track find/fit option, True->doFit, false->doFind
    virtual void setDoTrackFit(bool);
    virtual bool doTrackFit() const;

    ///SeedFinderType
    //enum SeedFinderType {kUndefined=0, kComposite=1, kEvaluable=3};
    virtual void setSeedFinderType(SeedFinderType);
    virtual SeedFinderType seedFinderType() const;

    
    //Evaluable Track Seed Finder
    
    /// acronyms: TPHF: TpcPadrowHitFilter
    ///           ETSF: EvaluableTrackSeedFinder
        
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

    //Program Flow
    bool mSimulated;
    bool mUseGui;
    bool mDoTrackFit;
    SeedFinderType mSeedFinderType;
    
    //Evaluable Track Seed Finder
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

inline void StiRootIOBroker::setSimulated(bool val)
{
    mSimulated = val;
}

inline bool StiRootIOBroker::simulated() const
{
    return mSimulated;
}

///Are we running in GUI version?
inline void StiRootIOBroker::setUseGui(bool val)
{
    mUseGui=val;
}

inline bool StiRootIOBroker::useGui() const
{
    return mUseGui;
}

///Toggle the track find/fit option, True->doFit, false->doFind
inline void StiRootIOBroker::setDoTrackFit(bool val)
{
    mDoTrackFit=val;
}

inline bool StiRootIOBroker::doTrackFit() const
{
    return mDoTrackFit;
}

///SeedFinderType
inline void StiRootIOBroker::setSeedFinderType(SeedFinderType val)
{
    if ( !(val==kComposite || val==kEvaluable) ) {
	cout <<"StiRootIOBroker::setSeedFinderType(). ERROR:\t"
	     <<"Unknown seed finder type.  Set to kUndefined"<<endl;
	mSeedFinderType=kUndefined;
    }
    else {
	mSeedFinderType=val;
    }
}
inline StiIOBroker::SeedFinderType StiRootIOBroker::seedFinderType() const
{
    return mSeedFinderType;
}


#endif
