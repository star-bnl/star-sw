/***************************************************************************
 *
 * $Id: StVpdSimMaker.h,v 1.1 2017/03/02 18:38:17 jeromel Exp $
 *
 * Author: Nickolas Luttrell (Rice University)
 ***************************************************************************
 *
 * Description: StVpdSimMaker.h   - Virtual base class for Vertex Position
 *  detector simulations
 *
 ***************************************************************************
 *
 * $Log: StVpdSimMaker.h,v $
 * Revision 1.1  2017/03/02 18:38:17  jeromel
 * First version of Vpd simulations
 *
 *
 ***************************************************************************/

#ifndef StVpdSimMaker_HH
#define StVpdSimMaker_HH

#include "StMaker.h"
#include "St_DataSet.h"
class TH1F;
class TH2F;
class TH3F;
class TNtuple;
class StEvent;
class StBTofCollection;
class StVpdSimConfig;

#include "tables/St_g2t_vpd_hit_Table.h"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcBTofHitCollection.hh"
#include "StVpdSimConfig.h"

#include <vector>
#ifndef ST_NO_NAMESPACES

#endif

class StVpdSimMaker : public StMaker {

public:
	StVpdSimMaker(const char *name = "VpdSim");
	virtual ~StVpdSimMaker();

	void           Reset();
	virtual int  Init();
	int          InitRun(int);
	int          FinishRun(int);
	virtual int  Make();
	virtual int  Finish();
    
    // Define get and set functions

    //! Returns the StBTofCollection of Vpd hits
	StBTofCollection*  GetVpdCollection()  const { return mVpdCollection; }
    //! Returns the StMcBTofHitCollection of Mc Vpd hits
	StMcBTofHitCollection* GetMcBTofHitCollection() const { return mMcBTofHitCollection; }
    
	string   pullHistFileName();
    string   getParamsFileName() { return mParamsFileName; }
    void    setParamsFile(string fileName = "db/vpdSimParams/vpdSimParams.dat") { mParamsFileName = fileName; }
    void    setBookHisto(bool bookHist) { mBookHisto = bookHist; }

	virtual const char *GetCVS() const{
		static const char cvs[] = "Tag $Name:  $ $Id: StVpdSimMaker.h,v 1.1 2017/03/02 18:38:17 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;
	}

protected:


    StMcBTofHitCollection * mMcBTofHitCollection = nullptr;     //!< The Mc hit collection
    St_DataSet            * mGeantData           = nullptr;     //!< Geant data passed into the StVpdSimMaker
    StEvent               * mEvent               = nullptr;     //!< The StEvent info
    StMcEvent             * mMcEvent             = nullptr;     //!< The McEvent info
    StBTofCollection      * mVpdCollection      = nullptr;      //!< The StBTofCollection of StBTofHit's (in this case, vpd hits)
    std::map<int, StVpdSimConfig::SingleTubeParams>	mSimParams; //!< Map of the calibration parameters to be applied

    //! VpdSingleHit contains the parameters that describe a vpd hit
	struct VpdSingleHit {
        int tray;       //!< The vpd tray (121==West, 122==East)
        int tubeId;     //!< The tube id of a given Vpd, with values [1,19]
        double tof;     //!< Time of flight given in ns
        double t0;      //!< Time offset (currently unused)
        double de;      //!< Energy deposition in GeV
        double pathL;   //!< Path length in cm (currently set to -9999)
        double q;       //!< Charge (currently set to 0 for Vpd hits)
	};

	//  Various general use variables

    StVpdSimConfig* mSimConfig; //!< The calibration parameters for Vpd
	bool mBookHisto;            //!< Default is kFALSE
    bool mUseFileParameters;    //!< Default is kFALSE
    string mParamsFileName;     //!< path/name of the calibration file to be passed

	int       mNHits = 0;
	double    mVx = 0;
	double    mVy = 0;
	double    mVz = 0;
	double    mSumTubeTime = 0;      //!< Tracks the time measured by each Vpd tube and sums them
	double    mTubeTAvg = 0;         //!< Average time lapse seen by the east or west Vpd

    double    mTStart = 0;           //!< Start time for an event
	double    mTubeTAvgWest = 0;     //!< Corrected event time for the west Vpd
	double    mTubeTAvgEast = 0;     //!< Corrected event time for the east Vpd
	float     mVpdVertex = 0;        //!< The calculated vertex as seen by the Vpd

	// Histogram variables
	string   mHistoFileName = "";    //!< histogram file name
	TH1F* mNRawHitsWest;    //!< Number of hits on each west Vpd tube before threshold cuts
    TH1F* mNRawHitsEast;    //!< Number of hits on each east Vpd tube before threshold cuts
	TH1F* mTubeHitsWest;    //!< Number of hits on each west Vpd tube after threshold cuts
	TH1F* mTubeHitsEast;    //!< Number of hits on each east Vpd tube after threshold cuts
	TH1F* mNHitsWest;       //!< Number of tubes hit across events for west Vpd
	TH1F* mNHitsEast;       //!< Number of tubes hit across events for east Vpd
	TH1F* mLeTimeWest;      //!< Leading edge times (currently equal to Time of Flight) for west Vpd
	TH1F* mLeTimeEast;      //!< Leading edge times (currently equal to Time of Flight) for east Vpd
	TH1F* mTStartHist;      //!< mTStart times (in ns)
	TH2F* mLeTubeHitsWest;  //!< Leading edge times and Number of tubes hit across events for West Vpd
    TH2F* mLeTubeHitsEast;  //!< Leading edge times and Number of tubes hit across events for East Vpd
	TH1F* mZVertexHist;     //!< Histogram of the provided Mc Vertices for all events
	TH1F* mVpdVertexHist;   //!< Histogram of the calculated Vpd vertices for all events
	TH1F* mVpdResHist;      //!< Histogram of the difference between Mc and calculated vertices
    TH3F* mResVsNumHits;    //!< Histogram fo the difference between Mc and calculated vertices vs. number of hits

							// Functions

    //! Extracts relevant parameters from a Vpd hit
	int        vpdResponse(VpdSingleHit &Hit, g2t_vpd_hit_st* vpd_hit, int vId);
    //! Determines average time information from East and West Vpd, cuts zero-velocity particles
	double     thresholdCut(std::vector<VpdSingleHit> Hits, std::vector<int> Tube_Counts, TH1F* TubeHits, TH1F* NHits);
    //! Builds the McBTofCollection, insures no duplicate hits
	int        storeMcVpdHit(StMcBTofHit* mcVpdHit);
    //! Fill StEvent from the McBTofCollection
	int        fillEvent();
    //! Creat the QA histograms
	int        bookHistograms();

	ClassDef(StVpdSimMaker, 2)
};

#endif /* StVpdSimMaker_h */



// end of StVpdSimMaker.h
