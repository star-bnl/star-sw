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
	virtual Int_t  InitRun(Int_t runNumber) {
		LOG_INFO << "StStgcQAMaker::InitRun - run = " << runNumber << endm;
		mStgcDbMaker=static_cast<StStgcDbMaker*>(GetMaker("stgcDb"));
		LOG_INFO << "mStgcDbMaker = " << mStgcDbMaker << endm;

		return kStOK;
	}
	virtual Int_t  Make();
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
	virtual void   Clear(const Char_t *opt) {LOG_DEBUG<<"StStgcQAMaker Clear"<<endm; StMaker::Clear();}

	void BookHistograms();
	void setHistFileName(  std::string );

private:

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


	StStgcDbMaker * mStgcDbMaker;
	std::string mQAHistFileName;
	std::map< string, TH1* > mHistograms;

	virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
	ClassDef(StStgcQAMaker,1)   //StAF chain virtual base class for Makers    

};


#endif