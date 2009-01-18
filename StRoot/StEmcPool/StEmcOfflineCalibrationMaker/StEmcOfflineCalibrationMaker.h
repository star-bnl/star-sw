#ifndef STAR_StEmcOfflineCalibrationMaker
#define STAR_StEmcOfflineCalibrationMaker

class TFile;
class TTree;
class TH2;
class TH3;

class StEmcOfflineCalibrationEvent;
class StEmcOfflineCalibrationTrack;

class StMuDstMaker;
class StEmcADCtoEMaker;
class StEmcTriggerMaker;

class StBemcTables;
class StEmcGeom;
class StEmcPosition;
class StEmcCollection;
class StMuTrack;

#include "StMaker.h"

class StEmcOfflineCalibrationMaker : public StMaker
{
private:
	const char* filename;
	TFile* myFile;
	TTree* calibTree;
	TH2* towerSlopes[2]; //[MB][HT]
	TH3* preshowerSlopes; 
	StEmcOfflineCalibrationEvent* myEvent;
	StEmcOfflineCalibrationTrack* myTrack;
	
	vector<unsigned int> mbTriggers;
	vector<unsigned int> htTriggers;
	
	//pointers to makers - get them in Init()
	StMuDstMaker*		muDstMaker;
	StEmcADCtoEMaker*	mADCtoEMaker;
	StEmcTriggerMaker*	emcTrigMaker;
	
	//all these random BEMC objects
	StBemcTables*		mTables;
	StEmcGeom*			mEmcGeom;
	StEmcPosition*		mEmcPosition;
	StEmcCollection*	mEmcCollection;
	
	//tower info (0==BTOW, 1==BPRS)
	unsigned short	mADC[2][4800];
	float			mPedestal[2][4800];
	float			mPedRMS[2][4800];
	int				mStatus[2][4800];
	unsigned char	mCapacitor[4800]; //only for BPRS
	
	void getADCs(int det); //1==BTOW, 2==BPRS
	pair<unsigned short, pair<float,float> > getTrackTower(StMuTrack* track, bool useExitRadius=false);
	float getTrackDeltaR(float track_eta, float track_phi, int id);
	pair<float, float> getTrackDetaDphi(float track_eta, float track_phi, int id);
	double highestNeighbor(int id);
	int getCorrectSignalForPRS(int softId);
	
	
public:
	StEmcOfflineCalibrationMaker(const char* name="btowCalibMaker", const char* file="test.root");
	virtual ~StEmcOfflineCalibrationMaker();

	virtual Int_t Init();
	virtual Int_t InitRun(int run);
	virtual Int_t Make();	
	virtual Int_t Finish();
	virtual void Clear(Option_t* option="");
	
	bool subtractPedestals;
	
	void addMinBiasTrigger(unsigned int trigId);
	void addHighTowerTrigger(unsigned int trigId);
	
	ClassDef(StEmcOfflineCalibrationMaker, 3)
};

#endif
