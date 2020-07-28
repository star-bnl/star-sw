#ifndef STSTGCQAMAKER_H
#define STSTGCQAMAKER_H

#include "StMaker.h"
#include "StStgcDbMaker/StStgcDbMaker.h"


class StStgcQAMaker : public StMaker {

public:
	StStgcQAMaker(const Char_t *name="stgcQA") : StMaker(name) {}
	virtual ~StStgcQAMaker(){}

	virtual Int_t  Init() {

		
		BookHistograms();


		return kStOK;
	}
#if !defined(__CINT__) && !defined(__CLING__)
	virtual Int_t  InitRun(Int_t runNumber) {
		LOG_INFO << "StStgcQAMaker::InitRun - run = " << runNumber << endm;
		mStgcDbMaker=static_cast<StStgcDbMaker*>(GetMaker("stgcDb"));
		LOG_INFO << "mStgcDbMaker = " << mStgcDbMaker << endm;

		return kStOK;
	}
#else
	virtual Int_t  InitRun(Int_t runNumber);
#endif
	virtual Int_t  Make();
#if !defined(__CINT__) && !defined(__CLING__)
	virtual Int_t  Finish() {
		LOG_DEBUG<<"StStgcQAMaker Finish"<<endm; 

		LOG_INFO << "StStgcQAMaker::Finish() - writing *.stgcQA.root ..." << endm;

		setHistFileName( ".stgcQA.root" );

		LOG_INFO << "StStgcQAMaker::Finish() - writing histograms to: " << mQAHistFileName.c_str() << endm;

		TFile histFile( mQAHistFileName.c_str(), "RECREATE", "stgcQA" );
		histFile.cd();

		writeHistograms();

		LOG_INFO << "StStgcQAMaker::Finish() - writing *.stgcQA.root ..." << endm;

		histFile.Close();


		return kStOK; 
	}
#else
	virtual Int_t  Finish();
#endif
	virtual void   Clear(const Char_t *opt) 
#if !defined(__CINT__) && !defined(__CLING__)
{LOG_DEBUG<<"StStgcQAMaker Clear"<<endm; StMaker::Clear();}
#else
	;
#endif
	void BookHistograms();
	void setHistFileName(  std::string );

private:

#if !defined(__CINT__) && !defined(__CLING__)
	void writeHistograms()
	{
		LOG_INFO << "writeHistograms() ... " << endm;
		for( const auto& kv : mHistograms ) {
			LOG_INFO << "write [" << kv.first << "] with " << kv.second->GetEntries() << endm;
			// if( kv.second->GetEntries() > 0 ) 
				kv.second->Write();
		}
		// for( const auto& kv : mHistograms2d ) {
		// 	if( kv.second->GetEntries() > 0 ) kv.second->Write();
		// }
	}
#else
	void writeHistograms();
#endif

	StStgcDbMaker * mStgcDbMaker;
	std::string mQAHistFileName;
	std::map< string, TH1* > mHistograms;

	virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
	ClassDef(StStgcQAMaker,1)   //StAF chain virtual base class for Makers    

};


#endif
