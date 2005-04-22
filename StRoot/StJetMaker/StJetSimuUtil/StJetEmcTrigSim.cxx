//StJetEmcTrigSim.cxx
//M.L. Miller (MIT)
//02/05

//std
#include <map>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
using namespace std;

//St_base stuff
#include "St_db_Maker/St_db_Maker.h"

//StEvent
#include "StEventTypes.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"

//StEmc
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/StBemcTables.h"

//local
#include "StJetMaker/StJetSimuUtil/StJetEmcTrigSim.h"

ClassImp(StJetEmcTrigSim)

    StJetEmcTrigSim::StJetEmcTrigSim(const char* name, St_db_Maker* db, StMuDstMaker* m)
	: StMaker(name), mDbMaker(db), mMuDstMaker(m), mPrint(false), mTables(new StBemcTables())
{
    cout <<"StJetEmcTrigSim::StJetEmcTrigSim()"<<endl;
}

StJetEmcTrigSim::~StJetEmcTrigSim()
{
    cout <<"StJetEmcTrigSim::~StJetEmcTrigSim()"<<endl;
}

Int_t StJetEmcTrigSim::Init()
{
    return StMaker::Init();
}

Int_t StJetEmcTrigSim::InitRun(Int_t runId)
{
    cout <<"Welcome to StJetHistMaker::InitRun()"<<endl;
    mTables->loadTables((StMaker*)this);

    return kStOk;
    
}

Int_t StJetEmcTrigSim::Make()
{
    mHiTowerAdc12bit = 0;
    mHiTowerAdc6bit = 0;
    mHiTowerTrigAdc = 0;
    mHiTowerEt = 0.;
    mHiTowerId = -1;
    mIs2004Ht = -1;

    StEmcCollection* emccol = 0;
    StEvent* event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
    if (event) {
	cout <<"StJetEmcTrigSim::Make()\tRetrieve StEmcCollection from StEvent"<<endl;
	emccol = event->emcCollection();
    }
    else {
	cout <<"StJetEmcTrigSim::Make()\tRetrieve StEmcCollection from MuDst"<<endl;
	emccol = mMuDstMaker->muDst()->emcCollection();
    }
    assert(emccol);

    mIs2004Ht = is2004HighTowerTriggeredEvent(emccol, mHiTowerAdc6bit);

    return kStOk;
}

Int_t StJetEmcTrigSim::Finish()
{
    return StMaker::Finish();
}

Int_t StJetEmcTrigSim::getHiTower(StEmcCollection* emccol, Float_t& hiTowerEt, UInt_t& hiTowerTrigADC, int& hiTowerAdc12bit)
{
    // Energy is converted to Et assuming z(vertex)=0
    // because this is how it's done in the trigger

    // return value is high tower ID

    hiTowerEt = 0;
    hiTowerTrigADC = 0;
    Int_t hiTowerId = 0;

    StEmcDetector* bemc = emccol->detector(kBarrelEmcTowerId);
    if(!bemc)	{
	if(mPrint)
	    cout << "***** No BEMC detector" << endl;
	return 0;
    }
    
    StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
    Float_t e, eT, theta;
    UInt_t adc;
    for(UInt_t i=1; i<=bemc->numberOfModules(); i++)
	{
	    StEmcModule* module = bemc->module(i);
	    StSPtrVecEmcRawHit& hits = module->hits();
	    StSPtrVecEmcRawHitIterator hIter;
	    for(hIter = hits.begin(); hIter != hits.end(); hIter++)  {
		e = (*hIter)->energy();
		adc = (*hIter)->adc();
		emcGeom->getTheta((*hIter)->module(),(*hIter)->eta(), theta);
		eT = e * sin(theta);
		int tempId;
		emcGeom->getId((*hIter)->module(), (*hIter)->eta(), (*hIter)->sub(), tempId);
		
		int status = -1;
		mTables->getStatus(BTOW, tempId, status);
		    
		if(status==1 && adc>hiTowerTrigADC) {
		    hiTowerEt = eT;
		    hiTowerTrigADC = adc;
		    hiTowerId = tempId;
		}
	    }
	}
    
    hiTowerAdc12bit = hiTowerTrigADC;
    hiTowerTrigADC = ((hiTowerTrigADC >> 5) & 63) |  (((hiTowerTrigADC >> 5) & 64) >> 1);
    // how it works:
    // From tower ADC which is 12 bits (maximum 111111111111) 5 least significant
    // bits are truncated (we're left with maximum 1111111). Trigger ADC is 6 bits,
    // so we have to reduce a 7-bit number to a 6-bit one. Truncated ADC value
    // up to 111111 should be converted to itself; values 1000000 and above should
    // be converted to 111111. Set up this way, our trigger ADC is proportional to
    // the lower half of the tower ADC range, and is maximum for the upper half.
  
    return hiTowerId;
}

int StJetEmcTrigSim::is2004HighTowerTriggeredEvent(StEmcCollection* emccol, int & hiTowerAdc6bit)//, emcPed_st* pedestalTable)
{
    hiTowerAdc6bit = 0;

    assert(mDbMaker);

    int date = mDbMaker->GetDateTime().GetDate();
    if(date < 20040101 || date > 20040516) {
	cout << "StPi0PeakMaker::is2004HighTowerTriggeredEvent() -- timestamp is not from run IV. Why are you using this method? (hint: you should not!)" << endl;
    }

    assert(mTables);
    /*
    if(!pedestalTable)
	{
	    if(mPrint)
		cout << "StPi0PeakMaker::is2004HighTowerTriggeredEvent() -- didn't get pedestal table!" << endl;
	    return kProblem;
	}
    */

    // For JetPatch trigger details see:
    // http://www.star.bnl.gov/HyperNews-star/protected/get/triggerboard/39/2/1/2/1/1.html
    // 1) We start out with 12-bit single tower ADC.
    // 2) We chop off 2 least significant bits and end up with 10-bit single tower
    // ADC.
    // 3) Subtract pedestal. In doing so, we want the resulting value to be
    // away from the multiples of 16
    // (multiples of 4 on 10-bit level) to avoid "jumping bits" that are
    // responsible for noise in the JetPatch sum. An outcome from some earlier
    // discussion was that the goal is to set the value at 24, half-way between 16
    // and 32, as opposed to 8, to distinguish live channels from the ones that are
    // masked out (pedestal of 24 will result in trigger pedestal 1).
    //  The maximum value that can be subtracted is 60 (15 on 10-bit
    // level). Some pedestals are too high to be subtracted down to 24 (or 8). In this
    // case I make them oddNumber*8.
    // 4) Lose 2 more bits; now we have 8-bit ADC. [1 ADC at this level is worth
    // ~8MeV (our calibration const.) times 16 = ~125MeV]
    // 5) Sum 16 towers to form a 4x4 patch sum. The sum is a 12-bit number.
    // 6) Apply LUT, so that 4x4 sums with values under Ped4x4 become 0; values
    // from Ped4x4 to (Ped4x4 + 60) are shifted by Ped4x4 to take values from 0 to
    // 60, and everything above (4x4Ped + 60) is 60.
    // 7) Sum 25 patch sums of 4x4. This is JetPatch ADC.

    const int pedestalTargetValue = 8;
    const int hiTowerThreshold2004 = 10; // verified with actual trigger ADC

    mHiTowerAdc12bit = 0;
    mHiTowerTrigAdc = 0;

    //Float_t dummyHiTowerEt;
    //UInt_t  hiTowerTriggerADC_uncorrected;

    int hiTowerId = getHiTower(emccol, mHiTowerEt, mHiTowerTrigAdc, mHiTowerAdc12bit);
    if(hiTowerId==0)  {
	if(mPrint) 
	    cout << "StPi0PeakMaker::is2004HighTowerTriggeredEvent() -- didn't get high tower ID!" << endl;
	mHiTowerId = -1;
	return -1;
    }
    mHiTowerId = hiTowerId;


    int hiTowerAdc10bit = mHiTowerAdc12bit/4;

    //int ped12bit = pedestalTable->AdcPedestal[hiTowerId-1]/100; //Alex's old line (replaced, MLM 3/28/05)
    
    float pedestal, rms;
    int CAP=0; //this arument matters only for SMD
    mTables->getPedestal(BTOW, hiTowerId, CAP, pedestal, rms);
    int ped12bit = static_cast<int>(pedestal);

    if(mPrint)	{
	cout << "StPi0PeakMaker::is2004HighTowerTriggeredEvent() --\t hiTowerAdc12bit = \t"
	     << mHiTowerAdc12bit << endl
	     << "\t ped12bit = \t" << ped12bit << endl;
    }

    int operation = 1; // subtract: +1 (default)
    //      add:  0

    int val12bit = ped12bit - pedestalTargetValue; // this is by how much we want to change the ped
    if(val12bit<0)	{
	val12bit = -val12bit;
	operation = 0;
    }
    int val10bit = val12bit/4;

    if(val12bit - val10bit*4 > 2)
	val10bit+=1;

    if(val10bit>15)
	val10bit = val10bit - 4*((val10bit-11)/4);
  
    // subtract (or add) pedestal from 10-bit ADC
    if(operation==1)
	hiTowerAdc10bit -= val10bit;
    else
	hiTowerAdc10bit += val10bit;

    // PIECE OF TCL CODE: (FROM EMC.TCL located in $bemc dir on sc3.starp.bnl.gov)
    //                         set scale10bits 4
    //                         set operationBit 1
    //                         # operationBit == 1 means subtract (default)
    //                         # operationBit == 0 means add
    //                         # PedestalShift comes from BemcConfig.dat
    //                         set pedestal1 [expr $pedestal - $PedestalShift]
    //                         if ($pedestal1<0) then {
    //                             set pedestal1 [expr $pedestal1*(-1)]
    //                             set operationBit 0
    //                         }
    //                         set operationBit [format "%1.0f" $operationBit]
    //                         set value2 [expr $pedestal1/$scale10bits]
    //                         set value1 [format "%3.0f" $value2]
  
    //                         set value2 [expr $pedestal1 - $value1*$scale10bits]
  
    //                         if ($value2>2) then {
    //                             set value2 [expr $value1 + 1]
    //                             set value1 [format "%3.0f" $value2]
    //                         }
  
    //                         if ($value1>15) then {
    //                             set value3 [expr ($value1-11)/$scale10bits]
    //                             set value3 [format "%3.0f" $value3]
    //                             set value3 [expr $value3*$scale10bits]
    //                             set value2 [expr $value1-$value3]
    //                             set value1 [format "%3.0f" $value2]
    //                         }
  
    //                         if {$operationBit == 1} {
    //                             set value [expr ($value1&0x0F)|(0x10)]
    //                         }
    //                         if {$operationBit == 0} {
    //                             set value [expr ($value1&0x0F)]
    //                         }
    //                         set cmd "$cmd 0x$add $value"
  
    hiTowerAdc6bit = ((hiTowerAdc10bit >> 3) & 63) | (((hiTowerAdc10bit >> 3) & 64) >> 1);
  
    if(mPrint)	{
	cout << "StPi0PeakMaker::is2004HighTowerTriggeredEvent() --\t hiTowerAdc6bit = \t"
	     << hiTowerAdc6bit << endl
	     << "\t value subtracted (12-bit level) = \t" << val10bit << endl
	     << "\t operation (sign) = \t" << operation << "(+1==\"-\" (def), 0==\"+\")" << endl;
    }

    //mHistCorrection6bitVsPed->Fill(ped12bit, hiTowerAdc6bit - int(hiTowerTriggerADC_uncorrected));

    if(hiTowerAdc6bit > hiTowerThreshold2004)
	return kTRUE;
  
    return kFALSE;
}
