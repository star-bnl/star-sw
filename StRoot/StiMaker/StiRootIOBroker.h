//StiRootIOBroker.h
//M.L. Miller (Yale Software)
//11/01

#ifndef StiRootIOBroker_HH
#define StiRootIOBroker_HH

//Root
#include "TObject.h"

//Sti
#include "Sti/StiIOBroker.h"

class StiRootIOBroker : public StiIOBroker, public TObject
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
	
	virtual void setLTSFExtrapZWindow(double);
	virtual double ltsfExtrapZWindow() const;
	
	virtual void setLTSFExtrapYWindow(double);
	virtual double ltsfExtrapYWindow() const;
	
	virtual void setLTSFExtrapMaxSkipped(unsigned int);
	virtual unsigned int ltsfExtrapMaxSkipped() const;
	
	virtual void setLTSFExtrapMinLength(unsigned int);
	virtual unsigned int ltsfExtrapMinLength() const;
	
	virtual void setLTSFExtrapMaxLength(unsigned int);
	virtual unsigned int ltsfExtrapMaxLength() const;
	
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
	
	//Min search radius to be passed to track finder
	virtual void setKTFMinSearchRadius(double);
	virtual double ktfMinSearchRadius() const;
	
	//Maxs search radius to be passed to track finder
	virtual void setKTFMaxSearchRadius(double);
	virtual double ktfMaxSearchRadius() const;
	
	virtual void setKTFSearchWindowScale(double);
	virtual double ktfSearchWindowScale() const;
	
	//Use helix extrapolation (defaults to line)
	virtual void setKTFUseHelixExtrapolation(bool);
	virtual bool ktfUseHelixExtrapolation() const;
	
	
	// LocalTrackMerger (ltm)
	
	virtual void setLTMDeltaR(double);
	virtual double ltmDeltaR() const;
	
	// Track Filter (filter)
	
	//This enum must be identical to the one in StiDynamicTrackFilter.h
	//enum FilterType {kPtFilter=0, kEtaFilter=1, kChi2Filter=2, kNptsFilter=3, kNFitPtsFilter=4,
	//kNGapsFilter=5, kFitPointRatioFilter=6, kPrimaryDcaFilter=7};
	virtual void addFilterType(FilterType t) {mFilterTypes.push_back(t); notify();}
	virtual void clearFilterTypes() {mFilterTypes.clear();notify();}
	virtual const vector<int>& filterTypes() const {return mFilterTypes;}
	
	virtual void setFilterPtMin(double);
	virtual double filterPtMin() const;
	
	virtual void setFilterPtMax(double);
	virtual double filterPtMax() const;
	
	virtual void setFilterEtaMin(double);
	virtual double filterEtaMin() const;
	
	virtual void setFilterEtaMax(double);
	virtual double filterEtaMax() const;
	
	virtual void setFilterChi2Max(double);
	virtual double filterChi2Max() const;
	
	virtual void setFilterNptsMin(unsigned int);
	virtual unsigned int filterNptsMin() const;
	
	virtual void setFilterNFitPtsMin(unsigned int);
	virtual unsigned int filterNFitPtsMin() const;
	
	virtual void setFilterNGapsMax(unsigned int);
	virtual unsigned int filterNGapsMax() const;
	
	virtual void setFilterFitPointRatioMin(double);
	virtual double filterFitPointRatioMin() const;
	
	virtual void setFilterPrimaryDcaMax(double);
	virtual double filterPrimaryDcaMax() const;
	
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
	double mLTSFExtrapZWindow;
	double mLTSFExtrapYWindow;
	unsigned int mLTSFExtrapMaxSkipped;
	unsigned int mLTSFExtrapMinLength;
	unsigned int mLTSFExtrapMaxLength;
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
	double mKTFMinSearchRadius;
	double mKTFMaxSearchRadius;
	double mKTFSearchWindowScale;
	bool mKTFUseHelixExtrapolation;
	
	//LocalTrackMerger
	double mLTMDeltaR;
	
	//Filter
	vector<int> mFilterTypes;
	double mFilterPtMin;
	double mFilterPtMax;
	double mFilterEtaMin;
	double mFilterEtaMax;
	double mFilterChi2Max;
	unsigned int mFilterNptsMin;
	unsigned int mFilterNFitPtsMin;
	unsigned int mFilterNGapsMax;
	double mFilterFitPointRatioMin;
	double mFilterPrimaryDcaMax;
	
 protected:

	//	static StiRootIOBroker * sInstance;

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

inline void StiRootIOBroker::setLTSFExtrapZWindow(double val)
{
    mLTSFExtrapZWindow=val;
    notify();
}

inline double StiRootIOBroker::ltsfExtrapZWindow() const
{
    return mLTSFExtrapZWindow;
}

inline void StiRootIOBroker::setLTSFExtrapYWindow(double val)
{
    mLTSFExtrapYWindow = val;
    notify();
}

inline double StiRootIOBroker::ltsfExtrapYWindow() const
{
    return mLTSFExtrapYWindow;
}

inline void StiRootIOBroker::setLTSFExtrapMaxSkipped(unsigned int val)
{
    mLTSFExtrapMaxSkipped = val;
    notify();
}

inline unsigned int StiRootIOBroker::ltsfExtrapMaxSkipped() const
{
    return mLTSFExtrapMaxSkipped;
}

inline void StiRootIOBroker::setLTSFExtrapMinLength(unsigned int val)
{
    mLTSFExtrapMinLength=val;
    notify();
}

inline unsigned int StiRootIOBroker::ltsfExtrapMinLength() const
{
    return mLTSFExtrapMinLength;
}

inline void StiRootIOBroker::setLTSFExtrapMaxLength(unsigned int val)
{
    mLTSFExtrapMaxLength=val;
    notify();
}

inline unsigned int StiRootIOBroker::ltsfExtrapMaxLength() const
{
    return mLTSFExtrapMaxLength;
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

inline void StiRootIOBroker::setKTFMinSearchRadius(double val)
{
    mKTFMinSearchRadius = val;
    notify();
}

inline double StiRootIOBroker::ktfMinSearchRadius() const
{
    return mKTFMinSearchRadius;
}

inline void StiRootIOBroker::setKTFMaxSearchRadius(double val)
{
    mKTFMaxSearchRadius = val;
    notify();
}
inline double StiRootIOBroker::ktfMaxSearchRadius() const
{
    return mKTFMaxSearchRadius;
}

inline void StiRootIOBroker::setKTFSearchWindowScale(double val)
{
    mKTFSearchWindowScale = val;
    notify();
}

inline double StiRootIOBroker::ktfSearchWindowScale() const
{
  return mKTFSearchWindowScale;
}

inline void StiRootIOBroker::setKTFUseHelixExtrapolation(bool val)
{
    mKTFUseHelixExtrapolation = val;
    notify();
}

inline bool StiRootIOBroker::ktfUseHelixExtrapolation() const
{
    return mKTFUseHelixExtrapolation;
}


inline void StiRootIOBroker::setLTMDeltaR(double val)
{
    mLTMDeltaR = val;
    notify();
}

inline double StiRootIOBroker::ltmDeltaR() const
{
    return mLTMDeltaR;
}

inline void StiRootIOBroker::setFilterPtMin(double v) {
    mFilterPtMin=v;
    notify();
}

inline double StiRootIOBroker::filterPtMin() const
{
    return mFilterPtMin;
}

inline void StiRootIOBroker::setFilterPtMax(double v)
{
    mFilterPtMax=v;
    notify();
}

inline double StiRootIOBroker::filterPtMax() const
{
    return mFilterPtMax;
}

inline void StiRootIOBroker::setFilterEtaMin(double v)
{
    mFilterEtaMin=v;
    notify();
}

inline double StiRootIOBroker::filterEtaMin() const
{
    return mFilterEtaMin;
}
    
inline void StiRootIOBroker::setFilterEtaMax(double v)
{
    mFilterEtaMax = v;
    notify();
}

inline double StiRootIOBroker::filterEtaMax() const
{
    return mFilterEtaMax;
}

inline void StiRootIOBroker::setFilterChi2Max(double v)
{
    mFilterChi2Max = v;
    notify();}

inline double StiRootIOBroker::filterChi2Max() const
{
    return mFilterChi2Max;
}

inline void StiRootIOBroker::setFilterNptsMin(unsigned int v)
{
    mFilterNptsMin = v;
    notify();}

inline unsigned int StiRootIOBroker::filterNptsMin() const
{
    return mFilterNptsMin;
}

inline void StiRootIOBroker::setFilterNFitPtsMin(unsigned int v)
{
    mFilterNFitPtsMin = v;
    notify();}

inline unsigned int StiRootIOBroker::filterNFitPtsMin() const
{
    return mFilterNFitPtsMin;
}

inline void StiRootIOBroker::setFilterNGapsMax(unsigned int v)
{
    mFilterNGapsMax = v;
    notify();}

inline unsigned int StiRootIOBroker::filterNGapsMax() const
{
    return mFilterNGapsMax;
}

inline void StiRootIOBroker::setFilterFitPointRatioMin(double v)
{
    mFilterFitPointRatioMin = v;
    notify();}

inline double StiRootIOBroker::filterFitPointRatioMin() const
{
    return mFilterFitPointRatioMin;
}
    
inline void StiRootIOBroker::setFilterPrimaryDcaMax(double v)
{
    mFilterPrimaryDcaMax = v;
    notify();}

inline double StiRootIOBroker::filterPrimaryDcaMax() const
{
    return mFilterPrimaryDcaMax;
}

#endif
