/***************************************************************************
 * $Id: StRHICfDbMaker.h,v 1.23 2018/03/09 21:36:18 smirnovd Exp $
 * \author: akio ogawa, Minho Kim, Seunghwan Lee
 ***************************************************************************
 *
 * Description: RHICf  DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StRHICfDbMaker.h,v $
 *
 **************************************************************************/

#ifndef STRHICFDBMAKER_H
#define STRHICFDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include <TH2D.h>

struct rhicfBarMap_st;
struct rhicfBarPos_st;
struct rhicfPedestal_st;
struct rhicfPedestalF_st;
struct rhicfGain_st;
struct rhicfPlateRange_st;
struct rhicfCrossTalk_st; 
struct rhicfNeutronLeakageOut_st;
struct rhicfPhotonLeakageOut_st;
struct rhicfPhotonLeakageIn_st;

class StRHICfDbMaker : public StMaker 
{
	public: 
		StRHICfDbMaker(const Char_t *name="rhicfDb");
		virtual       ~StRHICfDbMaker();
		virtual Int_t  Init();
		virtual Int_t  InitRun(Int_t runNumber);
		virtual Int_t  Make();
		virtual Int_t  Finish();
		virtual void   Clear(const Char_t *opt);

		void setDebug(Int_t debug); ///< debug mode, 0 for minimal message, >0 for more debug messages

		unsigned int getRunTRGMAddress() const;
		unsigned int getTriggerAddress() const;
		unsigned int getBunchNumberAddress() const;
		unsigned int getRunTimeAddress(unsigned int idx) const;
		unsigned int getTDCAddress(unsigned int idx) const;
		unsigned int getCAD0Address(unsigned int idx) const;
		unsigned int getGPI0Address(unsigned int idx) const;
		unsigned int getGPI1Address(unsigned int idx) const;

		unsigned int getAdcAddress(unsigned int tower, unsigned int plate, unsigned int range) const;
		unsigned int getAdcDAddress(unsigned int tower, unsigned int plate, unsigned int range) const;
		unsigned short getScifiAddress(unsigned int tower, unsigned int layer, unsigned xy, unsigned pos) const;

		Float_t getPlatePedestal(unsigned int tower, unsigned int plate) const;
		Float_t getBarPedestal(unsigned int tower, unsigned int layer, unsigned int xy, unsigned int pos) const;
		Float_t getBarPedestalF(unsigned int tower, unsigned int layer, unsigned int xy, unsigned int pos) const;
		Float_t getPlateGain(unsigned int, unsigned int plate) const;
		Float_t getBarGain(unsigned int tower, unsigned int layer, unsigned int xy, unsigned int pos) const;
		Float_t getPlateRangePar(unsigned int tower, unsigned int plate, unsigned int parnum) const;
		Float_t getBarPos(unsigned int tower, unsigned int layer, unsigned xy, unsigned pos) const;
		Float_t getCrossTalk(unsigned int i, unsigned int j) const; 

		TH2D* getLeakOutNeutron(unsigned int tower);
		TH2D* getLeakOutPhoton(unsigned int tower, unsigned int plate);
		TH2D* getLeakInPhoton(unsigned int tower, unsigned int plate);

	private:
		void deleteArrays();
		Int_t mDebug=0; //! >0 dump tables to text files

		rhicfPedestal_st *mPedestal=0;
		rhicfPedestalF_st *mPedestalF=0;   // pedestal table.
		rhicfGain_st *mGain=0;             // Gain table.
		rhicfPlateRange_st *mPlateRange=0; // Combine two ADC ragnes to one.
		rhicfBarMap_st *mBarMap=0;         // Address for bar arrays.
		rhicfBarPos_st *mBarPos=0;         // GSOBar position table.
		rhicfCrossTalk_st *mCrossTalk=0;   // CrossTalk table.

		rhicfNeutronLeakageOut_st *mNeutronLeakageOut=0;
		rhicfPhotonLeakageOut_st *mPhotonLeakageOut=0;
		rhicfPhotonLeakageIn_st *mPhotonLeakageIn=0;

		TH2D* h2_TSneuLeakOut;
		TH2D* h2_TLneuLeakOut;
		TH2D* h2_TSphoLeakOut[15];
		TH2D* h2_TLphoLeakOut[15];
		TH2D* h2_TSphoLeakIn[15];
		TH2D* h2_TLphoLeakIn[15];

	virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
	ClassDef(StRHICfDbMaker,3)   //StAF chain virtual base class for Makers
};

#endif


