#ifndef StiKalmanTrackFinderParameters_H
#define StiKalmanTrackFinderParameters_H 1

class StiKalmanTrackFinder;
class StiKalmanTrackNode;
class StiKalmanTrack;

class StiKalmanTrackFinderParameters
{
public: 


    StiKalmanTrackFinderParameters()
    {
	//setDefaults();
    }

    ~StiKalmanTrackFinderParameters()
    {}

    void setUseTrackFilter(bool option)
    {
	useTrackFilter = option;
    }

    void setXtrapolateToMainVertex(bool option)
    {
	xtrapolateToMainVertex = option;
    }

    void setElossCalculated(bool option)
    {
	elossCalculated = option;
    }
	
    void setMCSCalculated(bool option)
    {
	mcsCalculated = option;
    }

    void setField(double f)
    {
	field = f;
    }

    void setMassHypothesis(double m)
    {
	massHypothesis = m;
    }

    void   setMinContiguousHitCount(int count)
    {
	minContiguousHitCountForNullReset = count;
    }
	
    void   setMaxNullCount(int count)
    {
	maxNullCount = count;
    }
	
    void   setMaxContiguousNullCount(int count)
    {
	maxContiguousNullCount = count;
    }
	
    void   setMaxChi2ForSelection(double chi)
    {
	maxChi2ForSelection = chi;
    }
	
    void   setMinSearchWindow(double val)
    {
	minSearchWindow = val;
    }
	
    void   setMaxSearchWindow(double val)
    {
	maxSearchWindow = val;
    }
	
    void   setSearchWindowScale(double val)
    {
	searchWindowScale = val;
    }
	
    bool     getXtrapolateToMainVertex()
    {
	return xtrapolateToMainVertex;
    }

    bool     getUseTrackFilter()
    {
	return useTrackFilter;
    }

    double   getMinSearchWindow()
    {
	return minSearchWindow;
    }
	
    double   getMaxSearchWindow()
    {
	return maxSearchWindow;
    }
	
    double   getSearchWindowScale()
    {
	return searchWindowScale;
    }

    double   getMassHypothesis()
    {
	return massHypothesis;
    }

    friend StiKalmanTrackFinder;
    friend StiKalmanTrack;
    friend StiKalmanTrackNode;
	
protected:
	
    bool   useTrackFilter;
    bool   xtrapolateToMainVertex;

    //The following are set by StiKalmanTrackFinder::getNewState()
    bool   elossCalculated; //check
    bool   mcsCalculated; //check
    double field; //check
    
    int    maxNullCount; //check
    int    maxContiguousNullCount; //check
    int    minContiguousHitCountForNullReset; //check
    
    double minSearchWindow; //check
    double maxSearchWindow; //check
    double searchWindowScale; //check
    double maxChi2ForSelection; //check
    double massHypothesis; //check
};


#endif
