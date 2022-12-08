/***************************************************************************
 * $Id: StRHICfDbMaker.cxx,v 1.37 2018/05/22 20:04:45 akio Exp $
 * \author: Akio Ogawa, Minho Kim, Seunghwan Lee
 ***************************************************************************
 *
 * Description: This maker is the interface between RHICf and the STAR database
 *
 ***************************************************************************
 *
 * $Log: StRHICfDbMaker.cxx,v $
 *
 ***************************************************************************/

#include "StRHICfDbMaker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StMessMgr.h"
#include "tables/St_rhicfPedestal_Table.h"
#include "tables/St_rhicfPedestalF_Table.h"
#include "tables/St_rhicfGain_Table.h"
#include "tables/St_rhicfPlateRange_Table.h"
#include "tables/St_rhicfBarMap_Table.h"
#include "tables/St_rhicfBarPos_Table.h"
#include "tables/St_rhicfCrossTalk_Table.h"

#include "tables/St_rhicfNeutronLeakageOut_Table.h"
#include "tables/St_rhicfPhotonLeakageOut_Table.h"
#include "tables/St_rhicfPhotonLeakageIn_Table.h"

ClassImp(StRHICfDbMaker)

StRHICfDbMaker::StRHICfDbMaker(const Char_t *name) : StMaker(name)
{
} 

StRHICfDbMaker::~StRHICfDbMaker()
{
    deleteArrays();
}

Int_t StRHICfDbMaker::Init()
{
	return StMaker::Init();
}

Int_t StRHICfDbMaker::Make()
{
    return kStOK;
}

void StRHICfDbMaker::Clear(const Char_t*)
{
    deleteArrays(); StMaker::Clear();
}

void StRHICfDbMaker::deleteArrays()
{
}

Int_t StRHICfDbMaker::Finish()
{
    return kStOK;
}

Int_t StRHICfDbMaker::InitRun(Int_t runNumber) 
{
	//! Accessing DB.
	if(mDebug>0) {
		St_db_Maker* dbmaker = (St_db_Maker*)GetMaker("db");
		LOG_INFO << "StRHICfDbMaker::InitRun - Date&time from St_db_Maker="<<dbmaker->GetDate()<<","<< dbmaker->GetTime() << endm;
	}

	// Geometry DB
	TDataSet *DBgeometry = 0;
	DBgeometry = GetInputDB("Geometry/rhicf");
	if(!DBgeometry){
		LOG_ERROR << "StRHICfDbMaker::InitRun - No Geometry/rhicf"<<endm; 
		//return kStOK;
		return kStFatal;
	}

    // RHICf GSOBar mapping 
	St_rhicfBarMap *dbBarMap = 0;
	dbBarMap = (St_rhicfBarMap*) DBgeometry->Find("rhicfBarMap");

	if(dbBarMap){
		int mMaxBarMap = dbBarMap->GetNRows();
        if(mMaxBarMap==1){
            rhicfBarMap_st* barmap = (rhicfBarMap_st*) dbBarMap->GetTable();
            mBarMap = new rhicfBarMap_st;
            memcpy(mBarMap, barmap, sizeof(*barmap));
        }
	}

    // RHICf GSOBar position
	St_rhicfBarPos *dbBarPos = 0;
    dbBarPos = (St_rhicfBarPos*) DBgeometry->Find("rhicfBarPos");

    if(dbBarPos){
        int mMaxBarPos = dbBarPos->GetNRows();
        if(mMaxBarPos==1){
            rhicfBarPos_st* barpos = (rhicfBarPos_st*) dbBarPos->GetTable();
            mBarPos = new rhicfBarPos_st;
            memcpy(mBarPos, barpos, sizeof(*barpos));
        }
    }

	// Calibration DB.
	TDataSet *DBcalibrations = 0;
	DBcalibrations = GetInputDB("Calibrations/rhicf");
	if(!DBcalibrations){
		LOG_ERROR << "StRHICfDbMaker::InitRun - No Calibrations/rhicf"<<endm;
		return kStFatal;
	}

    // RHICf plate ADC range
	St_rhicfPlateRange * dbPlateRange = 0;
	dbPlateRange = (St_rhicfPlateRange*) DBcalibrations -> Find("rhicfPlateRange");
	
	if(dbPlateRange){
		int mMaxPlateRange = dbPlateRange->GetNRows();
		if(mMaxPlateRange==1){
			rhicfPlateRange_st* platerange = (rhicfPlateRange_st*) dbPlateRange->GetTable();
			mPlateRange = new rhicfPlateRange_st;
			memcpy(mPlateRange, platerange, sizeof(*platerange));
		}
	}

    // RHICf pedestal
	St_rhicfPedestal* dbPedestal = 0;
    dbPedestal = (St_rhicfPedestal*) DBcalibrations -> Find("rhicfPedestal");

    if(dbPedestal){
        int mMaxPedestal = dbPedestal->GetNRows();
        if(mMaxPedestal==1){
                rhicfPedestal_st* pedestal = (rhicfPedestal_st*) dbPedestal->GetTable();
                mPedestal = new rhicfPedestal_st;
                memcpy(mPedestal, pedestal, sizeof(*pedestal));
        }
    }

    // RHICf pedestal float part
	St_rhicfPedestalF* dbPedestalF = 0;
	dbPedestalF = (St_rhicfPedestalF*) DBcalibrations -> Find("rhicfPedestalF");

	if(dbPedestalF){
        int mMaxPedestal = dbPedestalF->GetNRows();
        if(mMaxPedestal==1){
            rhicfPedestalF_st* pedestalF = (rhicfPedestalF_st*) dbPedestalF->GetTable();
            mPedestalF = new rhicfPedestalF_st;
            memcpy(mPedestalF, pedestalF, sizeof(*pedestalF));
        }
    }

    // RHICf gain
	St_rhicfGain* dbGain = 0;
    dbGain = (St_rhicfGain*) DBcalibrations -> Find("rhicfGain");

    if(dbGain){
        int mMaxGain = dbGain->GetNRows();
        if(mMaxGain==1){
            rhicfGain_st* gain = (rhicfGain_st*) dbGain->GetTable();
            mGain = new rhicfGain_st;
            memcpy(mGain, gain, sizeof(*gain));
        }
    }

    // RHICf crossTalk
    St_rhicfCrossTalk* dbCrossTalk = 0;
    dbCrossTalk = (St_rhicfCrossTalk*) DBcalibrations -> Find("rhicfCrossTalk");

    if(dbCrossTalk){
        int mMaxCrossTalk = dbCrossTalk->GetNRows();
        if(mMaxCrossTalk==1){
            rhicfCrossTalk_st* crosstalk = (rhicfCrossTalk_st*) dbCrossTalk->GetTable();
            mCrossTalk = new rhicfCrossTalk_st;
            memcpy(mCrossTalk, crosstalk, sizeof(*crosstalk));
        }
    }

	// Leakage map DB.
	h2_TSneuLeakOut = new TH2D("h2_TSneuLeakOut", "", 20, 0, 20, 20, 0, 20);
	h2_TLneuLeakOut = new TH2D("h2_TLneuLeakOut", "", 40, 0, 40, 40, 0, 40);

	for(int i=0; i<15; i++){
		h2_TSphoLeakOut[i] = new TH2D(Form("h2_TSphoLeakOut_%d", i), "", 20, 0, 20, 20, 0, 20);
		h2_TLphoLeakOut[i] = new TH2D(Form("h2_TLphoLeakOut_%d", i), "", 40, 0, 40, 40, 0, 40);
		h2_TSphoLeakIn[i] = new TH2D(Form("h2_TSphoLeakIn_%d", i), "", 20, 0, 20, 20, 0, 20);
        h2_TLphoLeakIn[i] = new TH2D(Form("h2_TLphoLeakIn_%d", i), "", 40, 0, 40, 40, 0, 40);
	}	

	St_rhicfNeutronLeakageOut* dbNeutronLeakageOut = 0;
    dbNeutronLeakageOut = (St_rhicfNeutronLeakageOut*) DBcalibrations -> Find("rhicfNeutronLeakageOut");

    if(dbNeutronLeakageOut){
        int mMaxNeutronLeakageOut = dbNeutronLeakageOut->GetNRows();
        if(mMaxNeutronLeakageOut==1){
            rhicfNeutronLeakageOut_st* neuleakout = (rhicfNeutronLeakageOut_st*) dbNeutronLeakageOut->GetTable();
            mNeutronLeakageOut = new rhicfNeutronLeakageOut_st;
            memcpy(mNeutronLeakageOut, neuleakout, sizeof(*neuleakout));
        }

		for(int ix=0; ix<20; ix++){
			for(int iy=0; iy<20; iy++){
				h2_TSneuLeakOut -> SetBinContent(ix+1, iy+1, mNeutronLeakageOut->smallplate[20*ix + iy]);
			}
		}

		for(int ix=0; ix<40; ix++){
            for(int iy=0; iy<40; iy++){
                h2_TLneuLeakOut -> SetBinContent(ix+1, iy+1, mNeutronLeakageOut->largeplate[40*ix + iy]);
            }
        }
    }

	St_rhicfPhotonLeakageOut* dbPhotonLeakageOut = 0;
    dbPhotonLeakageOut = (St_rhicfPhotonLeakageOut*) DBcalibrations -> Find("rhicfPhotonLeakageOut");

    if(dbPhotonLeakageOut){
        int mMaxPhotonLeakageOut = dbPhotonLeakageOut->GetNRows();
        if(mMaxPhotonLeakageOut==1){
            rhicfPhotonLeakageOut_st* pholeakout = (rhicfPhotonLeakageOut_st*) dbPhotonLeakageOut->GetTable();
            mPhotonLeakageOut = new rhicfPhotonLeakageOut_st;
            memcpy(mPhotonLeakageOut, pholeakout, sizeof(*pholeakout));
        }

		for(int il=0; il<15; il++){
			for(int ix=0; ix<20; ix++){
				for(int iy=0; iy<20; iy++){
					h2_TSphoLeakOut[il] -> SetBinContent(ix+1, iy+1, mPhotonLeakageOut->smallplate[400*il + 20*ix + iy]);
				}
			}

			for(int ix=0; ix<40; ix++){
				for(int iy=0; iy<40; iy++){
					h2_TLphoLeakOut[il] -> SetBinContent(ix+1, iy+1, mPhotonLeakageOut->largeplate[1600*il + 40*ix + iy]);
				}
			}
		}
    }

	St_rhicfPhotonLeakageIn* dbPhotonLeakageIn = 0;
    dbPhotonLeakageIn = (St_rhicfPhotonLeakageIn*) DBcalibrations -> Find("rhicfPhotonLeakageIn");

    if(dbPhotonLeakageIn){
        int mMaxPhotonLeakageIn = dbPhotonLeakageIn->GetNRows();
        if(mMaxPhotonLeakageIn==1){
                rhicfPhotonLeakageIn_st* pholeakin = (rhicfPhotonLeakageIn_st*) dbPhotonLeakageIn->GetTable();
                mPhotonLeakageIn = new rhicfPhotonLeakageIn_st;
                memcpy(mPhotonLeakageIn, pholeakin, sizeof(*pholeakin));
        }   

        for(int il=0; il<15; il++){
            for(int ix=0; ix<20; ix++){
                for(int iy=0; iy<20; iy++){
                    h2_TSphoLeakIn[il] -> SetBinContent(ix+1, iy+1, mPhotonLeakageIn->smallplate[400*il + 20*ix + iy]);
                }   
            }   

            for(int ix=0; ix<40; ix++){
                for(int iy=0; iy<40; iy++){
                    h2_TLphoLeakIn[il] -> SetBinContent(ix+1, iy+1, mPhotonLeakageIn->largeplate[1600*il + 40*ix + iy]);
                }   
            }   
        }
    }
    return kStOK;
}

//=========== Address ===========//
unsigned int StRHICfDbMaker::getRunTRGMAddress() const {return 4;}
unsigned int StRHICfDbMaker::getTriggerAddress() const {return 623;}
unsigned int StRHICfDbMaker::getBunchNumberAddress() const {return 649;}
unsigned int StRHICfDbMaker::getRunTimeAddress(unsigned int idx) const {return 2+idx;}
unsigned int StRHICfDbMaker::getTDCAddress(unsigned int idx) const {return 367+idx;}
unsigned int StRHICfDbMaker::getCAD0Address(unsigned int idx) const {return 101+idx;}
unsigned int StRHICfDbMaker::getGPI0Address(unsigned int idx) const {return 623+idx;}
unsigned int StRHICfDbMaker::getGPI1Address(unsigned int idx) const {return 642+idx;}

unsigned int StRHICfDbMaker::getAdcAddress(unsigned int tower, unsigned int plate, unsigned int range) const
{
	if(tower==0){
		return 10+2*plate+(1-range);
	}
	if(tower==1){
		return 74+2*plate+(1-range);
	}
    return kStFatal;
}

unsigned int StRHICfDbMaker::getAdcDAddress(unsigned int tower, unsigned int plate, unsigned int range) const
{
    if(tower==0){
        return 42+2*plate+(1-range);
    }
	if(tower==1){
		return 106+2*plate+(1-range);
	}
    return kStFatal;
}

unsigned short StRHICfDbMaker::getScifiAddress(unsigned int tower, unsigned int layer, unsigned xy, unsigned pos) const
{
    if(mBarMap){
        if(tower==0) return 222 + mBarMap->smallbar[40*layer + 20*xy + pos];
        if(tower==1) return 222 + mBarMap->largebar[80*layer + 40*xy + pos];
    }
    return kStFatal;
}

//=========== Table values ===========//
Float_t StRHICfDbMaker::getPlatePedestal(unsigned int tower, unsigned int plate) const
{
    if(mPedestal) return mPedestal->plate[16*tower + plate];
    return kStFatal;
}

Float_t StRHICfDbMaker::getBarPedestal(unsigned int tower, unsigned int layer, unsigned int xy, unsigned int pos) const
{
    if(mPedestal){ 
		if(tower==0) return mPedestal->smallbar[40*layer + 20*xy + pos];
		if(tower==1) return mPedestal->largebar[80*layer + 40*xy + pos];
	}
    return kStFatal;
}

Float_t StRHICfDbMaker::getBarPedestalF(unsigned int tower, unsigned int layer, unsigned int xy, unsigned int pos) const
{
    if(mPedestalF){
        if(tower==0) return mPedestalF->smallbar[40*layer + 20*xy + pos];
        if(tower==1) return mPedestalF->largebar[80*layer + 40*xy + pos];
    }
    return kStFatal;
}

Float_t StRHICfDbMaker::getPlateGain(unsigned int tower, unsigned int plate) const
{
    if(mGain) return mGain->plate[16*tower + plate];
    return kStFatal;
}

Float_t StRHICfDbMaker::getBarGain(unsigned int tower, unsigned int layer, unsigned int xy, unsigned int pos) const
{
	if(mGain){
		if(tower==0) return mGain->smallbar[40*layer + 20*xy + pos];
		if(tower==1) return mGain->largebar[80*layer + 40*xy + pos];
	}
    return kStFatal;
}

Float_t StRHICfDbMaker::getPlateRangePar(unsigned int tower, unsigned int plate, unsigned int parnum) const
{
    if(mPlateRange) return mPlateRange->plate[32*tower + 2*plate + parnum];
    return kStFatal;
}

Float_t StRHICfDbMaker::getBarPos(unsigned int tower, unsigned int layer, unsigned xy, unsigned pos) const
{
    if(mBarPos){
        if(tower==0) return mBarPos->smallbar[40*layer + 20*xy + pos];
        if(tower==1) return mBarPos->largebar[80*layer + 40*xy + pos];
    }
    return kStFatal;
}

Float_t StRHICfDbMaker::getCrossTalk(unsigned int i, unsigned int j) const
{
    if(mCrossTalk){
        if((i < 20 && j >= 20) || (i >= 20 && j < 20)){return 0.;}

        unsigned short tmpI = i;
        unsigned short tmpJ = j;

        if(i >= 20){
            tmpI -= 20;
            tmpJ -= 20;
        }

        if(i<j){
            unsigned short tmp = tmpI;
            tmpI = tmpJ;
            tmpJ = tmp;
        }

        unsigned short index = 0;
        for(unsigned short sum=0; sum<=tmpI; sum++){index += sum;}
        index += tmpJ;

        if(i < 20){return mCrossTalk->crossTalk[index];}
        if(i >= 20){return mCrossTalk->crossTalk[210 + index];}
    }
    return kStFatal;
}

TH2D* StRHICfDbMaker::getLeakOutNeutron(unsigned int tower)
{
    if(tower==0){return h2_TSneuLeakOut;}
    if(tower==1){return h2_TLneuLeakOut;}
    return 0;
}

TH2D* StRHICfDbMaker::getLeakOutPhoton(unsigned int tower, unsigned int plate)
{
    if(plate==15){plate = 14;} // LeakageOut data range [0 ~ 14] in Db
    if(tower==0){return h2_TSphoLeakOut[plate];}
    if(tower==1){return h2_TLphoLeakOut[plate];}
    return 0;
}

TH2D* StRHICfDbMaker::getLeakInPhoton(unsigned int tower, unsigned int plate)
{
    if(plate==15){plate = 14;} // LeakageIn data range [0 ~ 14] in Db
    if(tower==0){return h2_TSphoLeakIn[plate];}
    if(tower==1){return h2_TLphoLeakIn[plate];}
    return 0;
}