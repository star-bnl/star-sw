//StiIOBroker.h
//M.L. Miller (Yale Software)
//11/01

#ifndef StiIOBroker_HH
#define StiIOBroker_HH

#include <vector>
using std::vector;
#include <algorithm>
using std::copy;
using std::ostream_iterator;

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

    //Local Track Seed Finder (LTSF) IO
    
    virtual void addLTSFPadrow(unsigned int) = 0;
    virtual void addLTSFSector(unsigned int) = 0;
    virtual const vector<unsigned int>& ltsfPadrows() const = 0;
    virtual const vector<unsigned int>& ltsfSectors() const = 0;

    virtual void setLTSFZWindow(double) = 0;
    virtual double ltsfZWindow() const = 0;
    
    virtual void setLTSFYWindow(double) = 0;
    virtual double ltsfYWindow() const = 0;
    
    virtual void setLTSFSeedLength(unsigned int) = 0;
    virtual unsigned int ltsfSeedLength() const = 0;
    
    virtual void setLTSFUseVertex(bool) = 0;
    virtual bool ltsfUseVertex() const = 0;

    virtual void setLTSFDoHelixFit(bool) = 0;
    virtual bool ltsfDoHelixFit() const = 0;

    //Kalman Track Finder (KTF) IO
    
    //whether MCS must be calc-ed
    virtual void setKTFMcsCalculated(bool) = 0;
    virtual bool ktfMcsCalculated() const = 0;

     //whether E-loss must be calculate
    virtual void setKTFElossCalculated(bool) = 0;
    virtual bool ktfElossCalculated() const = 0;

    //Max chi1 increment allowed/hit
    virtual void setKTFMaxChi2ForSelection(double) = 0;
    virtual double ktfMaxChi2ForSelection() const = 0;

    //Tesla magnetic field
    virtual void setKTFBField(double) = 0; 
    virtual double ktfBField() const = 0;

    //GeV mass used in MCS calcualtions
    virtual void setKTFMassHypothesis(double) = 0; 
    virtual double ktfMassHypothesis() const = 0;

    //?
    virtual void setKTFMinContiguousHitCount(unsigned int) = 0; 
    virtual unsigned int ktfMinContiguousHitCount() const = 0;

    //max # layers w/o a hit
    virtual void setKTFMaxNullCount(unsigned int) = 0; 
    virtual unsigned int ktfMaxNullCount() const = 0;

    //max # contiguous layers w/o hit
    virtual void setKTFMaxContiguousNullCount(unsigned int) = 0; 
    virtual unsigned int ktfMaxContiguousNullCount() const = 0;
    
protected:
    
    //singleton management
    StiIOBroker();
    virtual ~StiIOBroker();
    static StiIOBroker* sInstance;

    friend ostream& operator<<(ostream&, const StiIOBroker&);
};

inline ostream& operator<<(ostream& os, const StiIOBroker& b)
{
    os <<"simulated():\t"<<b.simulated()<<endl
       <<"useGui():\t"<<b.useGui()<<endl
       <<"doTrackFit():\t"<<b.doTrackFit()<<endl
       <<"seedFinderType():\t"<<static_cast<int>(b.seedFinderType())<<endl
       <<"tphfMinPadrow():\t"<<b.tphfMinPadrow()<<endl
       <<"tphfMaxPadrow():\t"<<b.tphfMaxPadrow()<<endl
       <<"etsfLowerBound():\t"<<b.etsfLowerBound()<<endl
       <<"etsfMaxHits():\t"<<b.etsfMaxHits()<<endl
       <<"ltsfPadrows():\t";
    copy( b.ltsfPadrows().begin(), b.ltsfPadrows().end(), ostream_iterator<unsigned int>(os, " "));
    os <<endl
       <<"ltsfSectors():\t";
    copy( b.ltsfSectors().begin(), b.ltsfSectors().end(), ostream_iterator<unsigned int>(os, " "));
    os <<endl
	
       <<"ltsfZWindow():\t"<<b.ltsfZWindow()<<endl
       <<"ltsfYWindow():\t"<<b.ltsfYWindow()<<endl
       <<"ltsfSeedLength():\t"<<b.ltsfSeedLength()<<endl
       <<"ltsfUseVertex():\t"<<b.ltsfUseVertex()<<endl
       <<"ltsfDoHelixFit():\t"<<b.ltsfDoHelixFit()<<endl
	
       <<"ktfMcsCalculated():\t"<<b.ktfMcsCalculated()<<endl
       <<"ktfElossCalculated():\t"<<b.ktfElossCalculated()<<endl
       <<"ktfMaxChi2ForSelection():\t"<<b.ktfMaxChi2ForSelection()<<endl
       <<"ktfBField():\t"<<b.ktfBField()<<endl
       <<"ktfMassHypothesis():\t"<<b.ktfMassHypothesis()<<endl
       <<"ktfMinContiguousHitCount():\t"<<b.ktfMinContiguousHitCount()<<endl
       <<"ktfMaxNullCount():\t"<<b.ktfMaxNullCount()<<endl
       <<"ktfMaxContiguousNullCount():\t"<<b.ktfMaxContiguousNullCount()<<endl;

    return os;
}

#endif
