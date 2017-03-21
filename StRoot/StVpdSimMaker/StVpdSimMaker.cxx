/***************************************************************************
 *
 * $Id: StVpdSimMaker.cxx,v 1.2 2017/03/21 23:43:25 nluttrel Exp $
 *
 * Author: Nickolas Luttrell (Rice University)
 ***************************************************************************
 *
 * Description: StVpdSimMaker.cxx   - This simulation maker was adapted from
 * existing code in the StBTofSimMaker circa 2016. It takes Vpd hits from
 * geant, determines the earliest hit in each tube, then passes these hits to
 * StEvent. For QA purposes it also calculates tStart and the Vpd Vertex.
 *
 ***************************************************************************
 *
 * $Log: StVpdSimMaker.cxx,v $
 * Revision 1.2  2017/03/21 23:43:25  nluttrel
 * Added checks for Mc vertex and tube Id
 *
 * Revision 1.1  2017/03/02 18:38:17  jeromel
 * First version of Vpd simulations
 *
 *
 ***************************************************************************/

#include <Stiostream.h>
#include <string>         // std::string
#include <cstddef>         // std::size_t
#include <math.h>
#include "StVpdSimMaker.h"
#include "StVpdSimConfig.h"

#include <TRandom3.h>
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "phys_constants.h"
#include <vector>
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"

// Tables
#include "tables/St_g2t_vpd_hit_Table.h"

#include "Random.h"
#include "RanluxEngine.h"
#include "RandGauss.h"

#include "StMcEvent/StMcVertex.hh"
#include "StEvent/StPrimaryVertex.h"
#include "StMcEvent/StMcBTofHit.hh"
#include "StEventTypes.h"
#include "StEvent/StBTofCollection.h"
#include "StChain/StChainOpt.h"

//static RanluxEngine engine;
//static RandGauss ranGauss(engine);

ClassImp(StVpdSimMaker)

//_____________________________________________________________________________
StVpdSimMaker::StVpdSimMaker(const char *name):StMaker(name)
{
    //! Set default values
    mMcBTofHitCollection = 0;
    mBookHisto=kFALSE; //!< Bool for histogram creation
    mUseFileParameters=kFALSE; //!< Choose if calibrations come from file
    Reset();
}


//_____________________________________________________________________________
StVpdSimMaker::~StVpdSimMaker()
{
    //! Destructor
    /**
     * Delete any DaqMaps or similar that are no longer needed.
     */
    
    delete mSimConfig;
}

//_____________________________________________________________________________
int StVpdSimMaker::Init()
{
    //! Initializer
    /**
     * Reset values, create histograms
     */
    
    Reset();
    if (mBookHisto) {
        bookHistograms();
    }
    return StMaker::Init();
}

//_____________________________________________________________________________
void StVpdSimMaker::Reset()
{
    //! Reset values
    
    mGeantData = 0;
    mEvent = 0;
    mMcEvent = 0;
    mSimConfig = 0;
    delete mMcBTofHitCollection;
}
//_____________________________________________________________________________
int StVpdSimMaker::InitRun(int runnumber)
{
    LOG_DEBUG << "StVpdSimMaker::InitRun  -- reading configuration file --" << endm;

    mSimConfig = new StVpdSimConfig;
    if (mUseFileParameters) {
        setParamsFile();
        mSimConfig->loadVpdSimParams(getParamsFileName());
        LOG_DEBUG << "Vpd Simulation Parameters loaded from file." << endm;
    }
    else {
        mSimConfig->loadVpdSimParams();
        LOG_DEBUG << "Vpd Simulation Parameters loaded from database." << endm;
    }
    mSimParams = mSimConfig->getParams();
    return kStOK;
}

//_____________________________________________________________________________
int StVpdSimMaker::FinishRun(int runnumber)
{

    return kStOk;
}


//_____________________________________________________________________________
int StVpdSimMaker::Finish()
{
    return kStOK;
}


//_____________________________________________________________________________
int StVpdSimMaker::Make()
{
    mMcBTofHitCollection = new StMcBTofHitCollection();

    VpdSingleHit singleHit;
    singleHit.tof = 0;
    int nWest = 0;
    int nEast = 0;
    double tubeTimeWest = 0;
    double tubeTimeEast = 0;
    std::vector<VpdSingleHit> hitsWest;
    std::vector<VpdSingleHit> hitsEast;

    std::vector<int> tubeCountsWest(19);
    std::vector<int> tubeCountsEast(19);

    //! Check to see that there are GEANT hits
    mGeantData = GetInputDS("geant"); //! in bfc chain
    if(!mGeantData) {   //! when reading the geant.root file
        mGeantData = GetInputDS("geantBranch");
    }
    if(!mGeantData) {
        LOG_WARN << " No GEANT data loaded. Exit! " << endm;
        return kStWarn;
    }
    LOG_INFO << " Found GEANT data -- loading Vpd hits... " << endm;

    mMcEvent = (StMcEvent*)GetInputDS("StMcEvent");
    if (!mMcEvent) {
        LOG_ERROR << "No StMcEvent! Bailing out ..." << endm;
    }
    else if (mMcEvent->primaryVertex() != NULL){
        StThreeVectorF Vtx = mMcEvent->primaryVertex()->position();
        mVz = Vtx.z();       //! VertexZ in cm
        LOG_DEBUG << "The simulated vZ is: " << mVz << endm;
    }
    else {
        LOG_WARN << "No primary vertex in McEvent! Bailing out ..." << endm;
        return kStWarn;
    }
    
    //! Look for Vpd hits
    St_g2t_vpd_hit* g2t_vpd_hits = 0;
    g2t_vpd_hits = dynamic_cast<St_g2t_vpd_hit*>(mGeantData->Find("g2t_vpd_hit"));
    if(!g2t_vpd_hits){
        LOG_WARN << " No Vpd hits in GEANT" << endm;
    }
    else {
        int nVpdHits = g2t_vpd_hits->GetNRows();
        LOG_DEBUG << " Found Vpd hits: " << nVpdHits << endm;
        g2t_vpd_hit_st* vpd_hit = g2t_vpd_hits->begin();

        //! Check for vpd_hit (only applies to the first one)
        if(!vpd_hit) {
            LOG_WARN << " No Vpd hit!" << endm;
            return kStWarn;
        }
        else {
            for(int i=0; i<nVpdHits; i++, vpd_hit++) {
                int vId = vpd_hit->volume_id;  //! volume id used the tube index
                int iTray = vId/1000 + 120;    //! 121: west, 122: east
                int tubeIndex = (vId % 100)-1; //! index of the tube [0,18]
                
                if ( (tubeIndex<0) || (tubeIndex>18) ) { //! Check that tube is within accepted range
                    LOG_WARN << "Invalid Vpd Tube Id!" << endm;
                    continue;
                }
                
                /** Check tray number and status flag of the current tube hit
                 * then calculate tof, extract relevant values and append hits
                 * to a vector
                 */

                if ((iTray == 121) && (mSimParams[tubeIndex].tubeStatusFlag == 1)) {   //! West Vpd, check that tube is active
                    singleHit.tof = mSimConfig->getVpdDistance()*1e9/C_C_LIGHT - mVz*1e9/C_C_LIGHT;
                    vpdResponse(singleHit, vpd_hit, vId);
                    tubeCountsWest[singleHit.tubeId - 1] += 1;
                    hitsWest.push_back(singleHit);
                    if (mBookHisto) { mLeTimeWest->Fill(singleHit.tof); }
				}
                //! If tray is 122, access index must be tubeIndex+19
                else if ((iTray == 122) && (mSimParams[tubeIndex+19].tubeStatusFlag) == 1) { //! East Vpd, check that tube is active
                    singleHit.tof = mSimConfig->getVpdDistance()*1e9/C_C_LIGHT + mVz*1e9/C_C_LIGHT;
					vpdResponse(singleHit, vpd_hit, vId);
                    tubeCountsEast[singleHit.tubeId - 1] += 1; //! singleHit.tubeId runs [1,19]
					hitsEast.push_back(singleHit);
                    if (mBookHisto) { mLeTimeEast->Fill(singleHit.tof); }
				}
            }
        }
    }

    if (mBookHisto) {
        for(unsigned int i = 0; i < tubeCountsWest.size(); i++ ){
            mNRawHitsWest->Fill( i + 1,  tubeCountsWest[i]  );
            mLeTubeHitsWest->Fill(tubeCountsWest[i], singleHit.tof);
        }
        for(unsigned int i = 0; i < tubeCountsEast.size(); i++ ){
            mNRawHitsEast->Fill( i + 1,  tubeCountsEast[i] );
            mLeTubeHitsEast->Fill(tubeCountsEast[i], singleHit.tof);
        }
    }

    if(hitsWest.size() > 0) {       //! Make sure there is at least one hit on the west.
        mTubeTAvgWest = thresholdCut(hitsWest, tubeCountsWest, mTubeHitsWest, mNHitsWest);
        tubeTimeWest = mSumTubeTime;
        nWest = mNHits;
    }
    else mTubeTAvgWest = -9999;

    if(hitsEast.size() > 0) {       //! Make sure there is at least one hit on the east.
        mTubeTAvgEast = thresholdCut(hitsEast, tubeCountsEast, mTubeHitsEast, mNHitsEast);
        tubeTimeEast = mSumTubeTime;
        nEast = mNHits;
    }
    else mTubeTAvgEast = -9999;

    if ((mTubeTAvgEast == -9999) or (mTubeTAvgWest == -9999)) {
        mVpdVertex = -9999;     //! Check for at least one hit on both east and west
    }
    else {
        mVpdVertex = C_C_LIGHT*1.e-9*(mTubeTAvgEast - mTubeTAvgWest)/2;
        mTStart = ((tubeTimeEast+tubeTimeWest)-(nEast-nWest)*(mVz)/(C_C_LIGHT*1.e9))/(nEast+nWest) - mSimConfig->getVpdDistance()/(C_C_LIGHT*1.e-9);
        LOG_DEBUG << "The vpdVz is: " << mVpdVertex << " cm" << endm;
        LOG_DEBUG << "The tStart is " << mTStart << " ns" << endm;
    }

    fillEvent();

    if (mBookHisto) {
        mTStartHist->Fill(mTStart);
        mVpdVertexHist->Fill(mVpdVertex);
        mVpdResHist->Fill(mVz-mVpdVertex);
        mResVsNumHits->Fill(nWest, nEast, mVz-mVpdVertex);
    }

    return kStOK;
}



//_____________________________________________________________________________
// vpdResponse takes a single Vpd hit in a PMT and stores the relevant information
// for that hit, including tray, tubeId, and tof
//
int StVpdSimMaker::vpdResponse(VpdSingleHit &singleHit, g2t_vpd_hit_st* vpd_hit, int vId)
{
	double randNum;
    TRandom3& randengine = *((TRandom3 *)gRandom);

    singleHit.tray = vId/1000 + 120;    //! 121: west, 122: east
    singleHit.tubeId = vId%100;           //! 1-19

    if (singleHit.tray == 122) {
        randNum = randengine.Gaus(0, mSimParams[singleHit.tubeId-1+19].singleTubeRes);
    }
    else {
        randNum = randengine.Gaus(0, mSimParams[singleHit.tubeId-1].singleTubeRes);
    }

    singleHit.tof += randNum/1000;    // Applies single-tube resolution smearing
    singleHit.t0 = vpd_hit->tof*1./nanosecond;     // Stores value in nanoseconds.

    singleHit.de = vpd_hit->de;
    singleHit.pathL = -9999;
    singleHit.q = 0.;

    return kStOK;
}



//_____________________________________________________________________________
// thresholdCut applies cuts on the raw Hits(West/East) based on an assigned threshold value, then finds the earliest time in each tube and returns average time information from a given Vpd.

double StVpdSimMaker::thresholdCut(std::vector<VpdSingleHit> singleHitsVec, std::vector<int> tubeCounts, TH1F* TubeHits, TH1F* NHits) {

    std::vector<double> timesVec;
    mSumTubeTime = 0;
    mTubeTAvg = 0;
    
    int mCounter = 0;

    for(int i = 0; i < (int)tubeCounts.size(); i++) {   //! Iterate through each of the 19 tubes
        int dex = -1;
        double lowTime = 1.e+9;     //! lowTime initially set arbitrarily high

        if (tubeCounts[i] >= mSimConfig->getThreshold()) {     //! Check if the tube count is over the minimum threshold (default is 1 hit)
            if (mBookHisto) { TubeHits->Fill( i + 1,  1  ); }
            for(int j = 0; j < (int)singleHitsVec.size(); j++) {  //! Iterate through all the hits
                if ((i == singleHitsVec[j].tubeId-1) and (singleHitsVec[j].tof > 1.e-4)) {    //! Find the hits corresponding to the current tube (that's over threshold)
                    if (singleHitsVec[j].tof < lowTime) {
                        lowTime = singleHitsVec[j].tof;  //! Stores the lowest ToF amongst the hits in a tube
                        dex = j;
                    }
                }
            }
        }
        else if (mBookHisto) { NHits->Fill(0); } //! Populate zero bin

        if (dex >= 0) {
            StMcBTofHit *mcHit = new StMcBTofHit(singleHitsVec[dex].tray,0,singleHitsVec[dex].tubeId,singleHitsVec[dex].de,singleHitsVec[dex].pathL,singleHitsVec[dex].t0,singleHitsVec[dex].tof,singleHitsVec[dex].q);
            storeMcVpdHit(mcHit); //! Pass the hit with the lowest time value to store in the hit collection.

            mCounter += 1;
            mSumTubeTime += singleHitsVec[dex].tof;
            timesVec.push_back(singleHitsVec[dex].tof);
        }
    }

    //! Calculate the average leading edge time for a given Vpd (east or west)
    if (timesVec.size() != 0) {
        mNHits = timesVec.size();
        mTubeTAvg = mSumTubeTime/mNHits;
    }
    else { mTubeTAvg = -9999; }

    if (mBookHisto) { NHits->Fill(mCounter); }

    return mTubeTAvg;
}



//_____________________________________________________________________________
// storeMcVpdHit replaces duplicate hits (hits that match the same cell location),
// or it stores the new hit (the last part below)
int StVpdSimMaker::storeMcVpdHit(StMcBTofHit* mcVpdHit)
{

    for(size_t j=0;j<mMcBTofHitCollection->hits().size();j++) {
        StMcBTofHit *tempHit = mMcBTofHitCollection->hits()[j];
        if(!tempHit) {
            LOG_DEBUG << " No hit stored in mMcBTofHitCollection." << endm;
        }
        if(mcVpdHit->sameCell(*tempHit)) {
            LOG_WARN << " Multiple hits passed to same cell. Exit! " << endm;
            return kStWarn;
        }
    }

    LOG_DEBUG << " Storing mcVpdHit to Collection." << endm;
    mMcBTofHitCollection->addHit(mcVpdHit);

    return kStOk;
}


//___________________________________________________________________________
// fillEvent stores the btofCollection from McEvent into StEvent
//
int StVpdSimMaker::fillEvent()
{
    LOG_DEBUG << "Filling McEvent and Event"<<endm;

    // send off to StMcEvent
    mMcEvent = (StMcEvent*)GetInputDS("StMcEvent");
    if (!mMcEvent) {
        LOG_ERROR << "No StMcEvent! Bailing out ..." << endm;
    }
    else {
        mMcEvent->setBTofHitCollection(mMcBTofHitCollection);       // Replaces existing collection with the passed argument
        LOG_INFO << " ... StMcVpdHitCollection stored in StMcEvent" << endm;
    }

    mEvent = (StEvent*)GetInputDS("StEvent");
    if (!mEvent) {
        LOG_ERROR << "No StEvent! Bailing out ..." << endm;
    }
    else { // mEvent non-zero
        LOG_DEBUG << "mEvent = " << mEvent << endm;

        //! Check for the simulated vertex
        if (mMcEvent->primaryVertex() != NULL){
            StThreeVectorF Vtx = mMcEvent->primaryVertex()->position();
            mVx = Vtx.x();
            mVy = Vtx.y();
            mVz = Vtx.z();
            LOG_DEBUG << " mVx, mVy, vZ: "
            << mVx << " "
            << mVy << " "
            << mVz << " " << endm;
            if (mBookHisto) { mZVertexHist->Fill(mVz); }
        }

        //! Store Collections
        mVpdCollection = mEvent->btofCollection();
        if(!mVpdCollection) {
            LOG_INFO << "Creating new StBTofCollection" << endm;
            mVpdCollection = new StBTofCollection();
            mEvent->setBTofCollection(mVpdCollection);
        }

        for(int jj = 0; jj < (int)mMcBTofHitCollection->hits().size(); jj++) {
            StMcBTofHit *aMcVpdHit = mMcBTofHitCollection->hits()[jj];

            if(!aMcVpdHit) continue;

            //! Fill the StBTofHit
            StBTofHit aVpdHit;
            aVpdHit.Clear();

            float mcTof = aMcVpdHit->tof();

            aVpdHit.setHardwarePosition(kBTofId);
            aVpdHit.setTray((int)aMcVpdHit->tray());
            aVpdHit.setModule((unsigned char)aMcVpdHit->module());
            aVpdHit.setCell((int)aMcVpdHit->cell());
            aVpdHit.setLeadingEdgeTime((double)mcTof);
            aVpdHit.setTrailingEdgeTime((double)mcTof);
            aVpdHit.setAssociatedTrack(NULL);          //! associated track set in StBTofMatchMaker
            aVpdHit.setIdTruth(aMcVpdHit->parentTrackId(), 1);  //! Set qaTruth to 1 so simulated hits can be tracked in the Calib Makers
            mVpdCollection->addHit(new StBTofHit(aVpdHit));

            //! Fill the StBTofRawHit
            StBTofRawHit aVpdRawHit;
            aVpdRawHit.Clear();
            aVpdRawHit.setTray((int)aMcVpdHit->tray());
            aVpdRawHit.setChannel(6*(aMcVpdHit->module() - 1) + (int)aMcVpdHit->cell());
            aVpdRawHit.setFlag(1);
            mVpdCollection->addRawHit(new StBTofRawHit(aVpdRawHit));
        }

        //! Fill StBTofHeader --
        StBTofHeader aHead;
        mVpdCollection->setHeader(new StBTofHeader(aHead));

        LOG_INFO << "... StBTofCollection Stored in StEvent! " << endm;

    }

    return kStOK;
}


//_____________________________________________________________________________
// pullHistFileName uses the string argument from the chain being run to set
// the name of the output histogram file.
//
string StVpdSimMaker::pullHistFileName() {

    string extension = ".VpdSim.root";

    if (GetChainOpt()->GetFileOut() != NULL) {
        TString outFile = GetChainOpt()->GetFileOut();
        mHistoFileName = (string)outFile;
        size_t lastindex = mHistoFileName.find_last_of(".");
        mHistoFileName = mHistoFileName.substr(0, lastindex);
        lastindex = mHistoFileName.find_last_of("/");
        mHistoFileName = mHistoFileName.substr(lastindex+1, mHistoFileName.length());
        mHistoFileName = mHistoFileName + extension;
    }

    return mHistoFileName;
}



//_____________________________________________________________________________
int StVpdSimMaker::bookHistograms()
{
    // Create new histograms

    AddHist( mNRawHitsWest = new TH1F("mNRawHitsWest_tubeId","mNRawHitsWest vs. tubeId; Tube Number; # of Hits", 21, 0, 21) );
    AddHist( mNRawHitsEast  = new TH1F("mNRawHitsEast_tubeId","mNRawHitsEast vs. tubeId; Tube Number; # of Hits",21, 0, 21) );

    AddHist( mTubeHitsWest = new TH1F("mTubeHitsWest_tubeId","mTubeHitsWest vs. tubeId (Threshold Cut); Tube Number; # of Hits", 21, 0, 21) );
    AddHist( mTubeHitsEast = new TH1F("mTubeHitsEast_tubeId","mTubeHitsEast vs. tubeId (Threshold Cut); Tube Number; # of Hits",21, 0, 21) );

    AddHist( mNHitsWest = new TH1F("mNHitsWest","# Tubes Hit per Event West; # Tubes; Counts", 21, 0, 21) );
    AddHist( mNHitsEast = new TH1F("mNHitsEast","# Tubes Hit per Event East; # Tubes; Counts",21, 0, 21) );

    AddHist( mLeTimeWest = new TH1F("mLeTimeWest","Leading Edge Times West (No Cuts); LE (ns); Counts", 300, 0, 30) );
    AddHist( mLeTimeEast = new TH1F("mLeTimeEast","Leading Edge Times East (No Cuts); LE (ns); Counts", 300, 0, 30) );
    AddHist( mTStartHist = new TH1F("mTStart","mTStart; Time (ns); Counts", 300, -30, 30) );

    AddHist( mLeTubeHitsWest = new TH2F("mLeTubeHitsWest"," mTubeHitsWest & LE Times (No Cuts); LE (ns); # of Hits; Counts", 300, 0, 30, 25, 0, 25) );
    AddHist( mLeTubeHitsEast = new TH2F("mLeTubeHitsEast","mTubeHitsEast & LE Times (No Cuts); LE (ns); # of Hits; Counts", 300, 0, 30, 25, 0, 25) );

    AddHist( mZVertexHist = new TH1F("mZVertexHist","True Vertices; Position (cm); Counts", 300, -50, 50) );
    AddHist( mVpdVertexHist = new TH1F("mVpdVertexHist","Calculated Vpd Vertices; Position (cm); Counts", 300, -50, 50) );
    AddHist( mVpdResHist = new TH1F("mVpdResHist","True - Vpd Vertex Position; TruePos - CalcPos (cm); Counts", 300, -15, 15) );

    AddHist( mResVsNumHits = new TH3F("mResVsNumHits","mResVsNumHits; numTubesWest; numTubesEast; vZ-vpdVz", 21, 0, 21, 21, 0, 21, 300, -10, 10) );

    return kStOK;
}


// End StVpdSimMaker.cpp
