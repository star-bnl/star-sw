/***************************************************************************
*
* $Id: StChargedPionMaker.h,v 1.3 2007/03/10 16:28:28 kocolosk Exp $
*
* Author:  Adam Kocoloski
***************************************************************************
*
* Description:  Collects charged pions from muDst.  Intent is to use 
* StJetSkimMaker in the same chain to get all spin-related event info
*
***************************************************************************
*
* $Log: StChargedPionMaker.h,v $
* Revision 1.3  2007/03/10 16:28:28  kocolosk
* log each new file in job
*
* Revision 1.2  2007/03/08 22:13:59  kocolosk
* stores StMuTracks directly
*
* Revision 1.1  2007/02/02 13:59:42  kocolosk
* new Maker StChargedPionMaker intended to be used with StJetSkimEventMaker for spin analysis
*
**************************************************************************/
#ifndef ST_CHARGED_PION_MAKER_HH
#define ST_CHARGED_PION_MAKER_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif

class TFile;
class TTree;
class TClonesArray;

class StMuDstMaker;
class StMuTrack;


class StChargedPionMaker : public StMaker {
public:
	StChargedPionMaker(const char *name = "chargedPionMaker", const char *outfile = "test.tracks.root");
	virtual ~StChargedPionMaker();
	
	virtual void Clear(const char *option="");
	virtual Int_t Init();
	virtual Int_t Make();
	virtual Int_t Finish();
	
	virtual const char* GetCVS() const
	{static const char cvs[]="Tag $Name:  $ $Id: StChargedPionMaker.h,v 1.3 2007/03/10 16:28:28 kocolosk Exp $ built "__DATE__" "__TIME__; return cvs;}
	
private:
	TFile *mFile;				//!
	TTree *mTree;				//!
	
	Int_t mRun;					//!
	Int_t mEvent;				//!
	Int_t mNTracks;				//!
	TClonesArray *mPrimaries;	//!
	TClonesArray *mGlobals;		//!
	
	TString currentFile;		//!

	//pointers to makers - get them in Init()
	StMuDstMaker* muDstMaker;	//!
	
	ClassDef(StChargedPionMaker,1)
};

#endif