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
    
    //Local Track Seed Finder (LTSF) IO
    
    virtual void addLTSFPadrow(unsigned int);
    virtual void addLTSFSector(unsigned int);
    virtual const vector<unsigned int>& ltsfPadrows() const;
    virtual const vector<unsigned int>& ltsfSectors() const;

    virtual void setLTSFZWindow(double);
    virtual double ltsfZWindow() const;

    virtual void setLTSFYWindow(double);
    virtual double ltsfYWindow() const;

    virtual void setLTSFSeedLength(unsigned int);
    virtual unsigned int ltsfSeedLength() const;
    
    virtual void setLTSFUseVertex(bool);
    virtual bool ltsfUseVertex() const;

    virtual void setLTSFDoHelixFit(bool);
    virtual bool ltsfDoHelixFit() const;

    //Kalman Track Finder (KTF) IO
    
    //whether MCS must be calc-ed
    virtual void setKTFMcsCalculated(bool);
    virtual bool ktfMcsCalculated() const;

     //whether E-loss must be calculate
    virtual void setKTFElossCalculated(bool);
    virtual bool ktfElossCalculated() const;

    //Max chi1 increment allowed/hit
    virtual void setKTFMaxChi2ForSelection(double);
    virtual double ktfMaxChi2ForSelection() const;

    //Tesla magnetic field
    virtual void setKTFBField(double); 
    virtual double ktfBField() const;

    //GeV mass used in MCS calcualtions
    virtual void setKTFMassHypothesis(double); 
    virtual double ktfMassHypothesis() const;

    //?
    virtual void setKTFMinContiguousHitCount(unsigned int); 
    virtual unsigned int ktfMinContiguousHitCount() const;

    //max # layers w/o a hit
    virtual void setKTFMaxNullCount(unsigned int); 
    virtual unsigned int ktfMaxNullCount() const;

    //max # contiguous layers w/o hit
    virtual void setKTFMaxContiguousNullCount(unsigned int); 
    virtual unsigned int ktfMaxContiguousNullCount() const;

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

    //Local Track Seed Finder
    vector<unsigned int> mLTSFPadrows;
    vector<unsigned int> mLTSFSectors;
    double mLTSFZWindow;
    double mLTSFYWindow;
    unsigned int mLTSFSeedLength;
    bool mLTSFUseVertex;
    bool mLTSFDoHelixFit;

    //Kalman Track Finder
    bool mKTFMcsCalculated;
    bool mKTFElossCalculated;
    double mKTFMaxChi2ForSelection;
    double mKTFBField;
    double mKTFMassHypothesis;
    unsigned int mKTFMinContiguousHitCount;
    unsigned int mKTFMaxNullCount;
    unsigned int mKTFMaxContiguousNullCount;
    
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

///Local Track Seed Finder

inline void StiRootIOBroker::addLTSFPadrow(unsigned int val)
{
    mLTSFPadrows.push_back(val);
}

inline void StiRootIOBroker::addLTSFSector(unsigned int val)
{
    mLTSFSectors.push_back(val);
}

inline const vector<unsigned int>& StiRootIOBroker::ltsfPadrows() const
{
    return mLTSFPadrows;
}

inline const vector<unsigned int>& StiRootIOBroker::ltsfSectors() const
{
    return mLTSFSectors;
}

inline void StiRootIOBroker::setLTSFZWindow(double val)
{
    mLTSFZWindow = val;
    notify();
}

inline double StiRootIOBroker::ltsfZWindow() const
{
    return mLTSFZWindow;
}

inline void StiRootIOBroker::setLTSFYWindow(double val)
{
    mLTSFYWindow = val;
    notify();
}

inline double StiRootIOBroker::ltsfYWindow() const
{
    return mLTSFYWindow;
}

inline void StiRootIOBroker::setLTSFSeedLength(unsigned int val)
{
    mLTSFSeedLength = val;
    notify();
}

inline unsigned int StiRootIOBroker::ltsfSeedLength() const
{
    return mLTSFSeedLength;
}

inline void StiRootIOBroker::setLTSFUseVertex(bool val)
{
    mLTSFUseVertex = val;
    notify();
}
inline bool StiRootIOBroker::ltsfUseVertex() const
{
    return mLTSFUseVertex;
}

inline void StiRootIOBroker::setLTSFDoHelixFit(bool val)
{
    mLTSFDoHelixFit = val;
    notify();
}

inline bool StiRootIOBroker::ltsfDoHelixFit() const
{
    return mLTSFDoHelixFit;
}

//Kalman Track Finder (KTF) IO
    
inline void StiRootIOBroker::setKTFMcsCalculated(bool val)
{
    mKTFMcsCalculated = val;
    notify();
}

inline bool StiRootIOBroker::ktfMcsCalculated() const
{
    return mKTFMcsCalculated;
}

inline void StiRootIOBroker::setKTFElossCalculated(bool val)
{
    mKTFElossCalculated = val;
    notify();
}
    
inline bool StiRootIOBroker::ktfElossCalculated() const
{
    return mKTFElossCalculated;
}

inline void StiRootIOBroker::setKTFMaxChi2ForSelection(double val)
{
    mKTFMaxChi2ForSelection = val;
    notify();
}

inline double StiRootIOBroker::ktfMaxChi2ForSelection() const
{    
    return mKTFMaxChi2ForSelection;
}

inline void StiRootIOBroker::setKTFBField(double val)
{
    mKTFBField = val;
    notify();
}

inline double StiRootIOBroker::ktfBField() const
{
    return mKTFBField;
}

inline void StiRootIOBroker::setKTFMassHypothesis(double val)
{
    mKTFMassHypothesis = val;
    notify();
}

inline double StiRootIOBroker::ktfMassHypothesis() const
{
    return mKTFMassHypothesis;
}

inline void StiRootIOBroker::setKTFMinContiguousHitCount(unsigned int val)
{
    mKTFMinContiguousHitCount = val;
    notify();
}

inline unsigned int StiRootIOBroker::ktfMinContiguousHitCount() const
{
    return mKTFMinContiguousHitCount;
}

inline void StiRootIOBroker::setKTFMaxNullCount(unsigned int val)
{
    mKTFMaxNullCount = val;
    notify();
}

inline unsigned int StiRootIOBroker::ktfMaxNullCount() const
{
    return mKTFMaxNullCount;
}

inline void StiRootIOBroker::setKTFMaxContiguousNullCount(unsigned int val)
{
    mKTFMaxContiguousNullCount = val;
    notify();
}

inline unsigned int StiRootIOBroker::ktfMaxContiguousNullCount() const
{
    return mKTFMaxContiguousNullCount;
}

#endif
