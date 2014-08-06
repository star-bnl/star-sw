#ifndef STAR_StEmcSimulatorMaker
#define STAR_StEmcSimulatorMaker

// $Id: StEmcSimulatorMaker.h,v 1.28 2014/08/06 11:43:07 jeromel Exp $

#include "StMaker.h"
#include "StEmcRawMaker/defines.h"
#include "StEvent/StEnumerations.h"
#include "StEmcVirtualSimulator.h"

class StBemcTables;
class StEmcPosition;
class StEmcGeom;
class StMcEmcHitCollection;
class StMcEvent;
class StMcTrack;
class StEmcCollection;

/*****************************************************************************
 * @class StEmcSimulatorMaker
 * @author A.Pavlinov -> A.Suaide -> A.Kocoloski
 * 
 * This is the slow simulator for the BEMC.  It translates energy depositions 
 * from GEANT into ADCs, which means it needs to run with StIOMaker in the 
 * chain.  The BSMD planes are simulated using instances of StEmcSimpleSimulator, 
 * while the BTOW and BPRS use the StEmcPmtSimulator class which offers better 
 * accounting for primary and secondary photostatistics.
 *****************************************************************************/
class StEmcSimulatorMaker : public StMaker
{
private:
    StBemcTables*           mTables;
    StEmcPosition*          mPosition;
    StEmcGeom*              mGeom[MAXDETBARREL];
    
    StEmcVirtualSimulator*  mSimulator[MAXDETBARREL];
    StEmcVirtualSimulator::StEmcSimulatorMode mSimulatorMode[MAXDETBARREL];
    
    /// these are just the collections from StMcEvent, stored for convenience
    StMcEmcHitCollection*   mEmcMcHits[MAXDETBARREL];
    
    StMcEvent*              mMcEvent;
    StEmcCollection*        mEmcCollection;
    
    bool                    mMakeFullDetector[MAXDETBARREL];
    bool                    mCheckStatus[MAXDETBARREL];
    bool                    mDoZeroSuppression[MAXDETBARREL];
    float                   mPedestalCut[MAXDETBARREL];
    float                   mCalibOffset[MAXDETBARREL];
    float                   mCalibSpread[MAXDETBARREL];
    float                   mMaxAdc[MAXDETBARREL];
    float                   mMaxAdcSpread[MAXDETBARREL];
    
    float                   mCrossTalk[MAXDETBARREL];
    
    /// The simulators should not add pedestal noise if we're doing embedding.  This flag
    /// is set automatically by looking for StEmcRawMaker or StEmcADCtoEMaker in the chain.
    bool                    mEmbeddingMode;
    
    /// if we're running in BFC, we should set raw hit energy == GEANT dE so it
    /// gets stored in the MuDST.  If we're running in analysis mode, we should
    /// just store reco energy as usual.
    bool                    mIsBFC;
    
    /// convert StMcCalorimeterHits to StEmcRawHits here
    void                    makeRawHits();
    
    /// Simulate the effects of optical cross talk
    void                    makeCrossTalk(StMcTrack *track);
    
public:
    /// check for embedding chain by looking for StEmcMixerMaker.
    /// initialize control flags to defaults
    /// initialize geometry and database classes
    StEmcSimulatorMaker(const char *name="EmcSimulator");
    
    virtual                 ~StEmcSimulatorMaker();
    
    /// creates the detector simulators and sets their properties
    virtual Int_t           Init();
    
    /// resets the pointer to the current StMcEvent
    virtual void            Clear(const char*);
    
    /// gets StMcEmcHitCollections from StMcEvent and adds noise if requested
    /// feeds these hits through the detector simulators 
    /// generates StEmcRawHits which are dropped into the StEvent emcCollection.
    virtual Int_t           Make();
    
    /// require raw hits to have status == 1 in order to be saved in the StEmcCollection.
    /// Default is true.
    void setCheckStatus(StDetectorId det, bool flag) { mCheckStatus[det-kBarrelEmcTowerId] = flag; }
    
    /// simulate pedestal noise where no MC hits are found.  Default is true for BTOW, false otherwise.
    void setMakeFullDetector(StDetectorId det, bool flag) { mMakeFullDetector[det-kBarrelEmcTowerId] = flag; }
    
    /// only save hits which pass pedestal cut.  Default is true for simulations, false for embedding.
    /// BTOW default is always false to match adc2e maker behavior.
    void setDoZeroSuppression(StDetectorId det, bool flag) { mDoZeroSuppression[det-kBarrelEmcTowerId] = flag; }
    
    /// pedestal cut requires (ADC-pedMean) > n*pedRMS.
    void setPedestalCut(StDetectorId det, float nRMS) { mPedestalCut[det-kBarrelEmcTowerId] = nRMS; }
    
    /// scale simulator calibration coefficients by 1.0 + offset.  Default is zero
    void setCalibOffset(StDetectorId det, float offset) { mCalibOffset[det-kBarrelEmcTowerId] = offset; }
    
    /// smear simulator calibration coefficients using Gaussian with this RMS.
    void setCalibSpread(StDetectorId det, float spread) { mCalibSpread[det-kBarrelEmcTowerId] = spread; } 
    
    /// choose the simulator mode for each detector.  Defaults are kPrimarySecondaryFullMode (BTOW, BPRS) and kSimpleMode (SMDs)
    void setSimulatorMode(StDetectorId det, StEmcVirtualSimulator::StEmcSimulatorMode mode) { mSimulatorMode[det-kBarrelEmcTowerId] = mode; }
    
    /// maximum possible ADC for a channel.  Defaults are 4095 (TOW), 1023 (PRS), 1023 (SMDs)
    void setMaximumAdc(StDetectorId det, float adc) { mMaxAdc[det-kBarrelEmcTowerId] = adc; }
    
    /// maximum possible ADC will be calculated by sampling Gaussian with this spread.  Default is 0 for all detectors.
    void setMaximumAdcSpread(StDetectorId det, float spread) { mMaxAdcSpread[det-kBarrelEmcTowerId] = spread; }
    
    /// this setting has no effect if det!=kBarrelSmdEtaStripId.  Simulate cross-talk from cross-capacitance, which falls linearly to 0 as eta => 1.0
    void setMaxCrossTalkPercentage(StDetectorId det, float percentage) { mCrossTalk[det - kBarrelEmcTowerId] = percentage / 100.0; }
    
    StMcEmcHitCollection*   getEmcMcHits(Int_t det) { return mEmcMcHits[det-1]; }
    StMcEmcHitCollection*   getBemcMcHits()  { return getEmcMcHits(BTOW);  }
    StMcEmcHitCollection*   getBprsMcHits()  { return getEmcMcHits(BPRS);  }
    StMcEmcHitCollection*   getBsmdeMcHits() { return getEmcMcHits(BSMDE); }
    StMcEmcHitCollection*   getBsmdpMcHits() { return getEmcMcHits(BSMDP); }

    /// collection of reconstructed hits.  If embedding mode is set Maker DOES NOT clean up this collection.
    /// Instead, StEmcMixerMaker is expected to put this collection into a new StEvent and take care of 
    /// cleaning it up.
    StEmcCollection*        getEmcCollection() { return mEmcCollection; }

    /// pointer to database tables
    StBemcTables*           getTables() { return mTables; }

    virtual const char*     GetCVS() const {
        static const char cvs[]="Tag $Name:  $ $Id: StEmcSimulatorMaker.h,v 1.28 2014/08/06 11:43:07 jeromel Exp $ built " __DATE__ " " __TIME__ ;
        return cvs;
    }

    ClassDef(StEmcSimulatorMaker, 1)
};

#endif

/*****************************************************************************
 * $Log: StEmcSimulatorMaker.h,v $
 * Revision 1.28  2014/08/06 11:43:07  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.27  2008/11/17 21:35:21  kocolosk
 * store GEANT dE in MuDST raw hit energy data member
 *
 * Revision 1.26  2007/12/12 23:29:48  kocolosk
 * full pedestal simulation is now default for BTOW
 *
 * Revision 1.25  2007/11/28 16:18:58  kocolosk
 * optical cross-talk simulation by Mike Betancourt
 * http://www.star.bnl.gov/HyperNews-star/protected/get/phana/144.html
 *
 * Revision 1.24  2007/10/08 15:28:38  kocolosk
 * setMaximumAdc(Spread) methods allow for better simulation of BSMD ADC response
 * http://www.star.bnl.gov/HyperNews-star/get/emc2/2507.html
 *
 * Revision 1.23  2007/09/15 18:36:35  kocolosk
 * changed defaults so makeFullDetector is false and so zero suppression is turned off for BTOW
 *
 * Revision 1.22  2007/09/12 03:06:13  kocolosk
 * embedding mode also set if StEmcADCtoEMaker is in chain (non-bfc embedding)
 *
 * Revision 1.21  2007/09/12 02:58:53  kocolosk
 * look for emcRaw, not emcEmbed, to determine if it's an embedding chain
 *
 * Revision 1.20  2007/09/12 02:55:15  kocolosk
 * don't do zero suppression on embedding hits (they have no pedestal)
 *
 * Revision 1.19  2007/09/12 01:29:32  kocolosk
 * added method to access StBemcTables
 *
 * Revision 1.18  2007/09/11 22:40:48  kocolosk
 * small correction to documentation
 *
 * Revision 1.17  2007/09/11 21:49:14  kocolosk
 * complete overhaul of the BEMC simulator
 * http://www.star.bnl.gov/HyperNews-star/get/emc2/2486.html
 *
 * Revision 1.16  2007/01/22 19:13:40  kocolosk
 * use STAR logger for all output
 *
 * Revision 1.15  2005/03/21 21:36:39  suaide
 * fixed problem with chain
 *
 * Revision 1.14  2004/08/09 19:43:28  suaide
 * moved global variables to private members and
 * made small modifications to run in embedding mode
 *
 * Revision 1.13  2004/08/06 13:24:48  suaide
 * New features added and fixed some bugs in the database
 *
 * Revision 1.12  2004/04/08 21:35:45  perev
 * Leak off
 *
 * Revision 1.11  2003/09/23 15:19:55  suaide
 * fixed bugs and modifications for embedding
 *
 * Revision 1.10  2003/09/10 19:47:12  perev
 * ansi corrs
 *
 * Revision 1.9  2003/01/23 03:09:02  jeromel
 * Include modif
 *
 * Revision 1.8  2002/09/10 16:51:32  pavlinov
 * Discard line with mDbMaker->SetDateTime
 *
 * Revision 1.7  2002/06/04 16:09:37  pavlinov
 * added option with DB(pedestal ans calibration  coefficients
 *
 * Revision 1.6  2002/06/03 23:35:11  pavlinov
 * Last correction without DB for ped and calib. coeff.
 *
 * Revision 1.5  2001/09/22 00:29:47  pavlinov
 * No public constructor for StEmcGeom
 *
 * Revision 1.4  2001/03/22 22:04:45  pavlinov
 * Clean up for mdc4
 *
 * Revision 1.3  2001/02/03 00:00:01  pavlinov
 * New function Browse() and cleanup for new version of BFC
 *
 * Revision 1.2  2000/10/28 00:33:45  pavlinov
 * added methods getEmcCollectin()
 *
 * Revision 1.1  2000/10/23 22:53:14  pavlinov
 * First working C++ version
 *****************************************************************************/
