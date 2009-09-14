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
class StTriggerSimuMaker;

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
	TH2* mapcheck;
	TH2* towerSlopes[2]; //[MB][HT]
	TH2* preshowerSlopes; 
	TH2* smdSlopes[2]; //[eta][phi] no caps
	StEmcOfflineCalibrationEvent* myEvent;
	StEmcOfflineCalibrationTrack* myTrack;
	
	vector<unsigned int> mbTriggers;
	vector<unsigned int> htTriggers;
	vector<unsigned int> httpTriggers;
	vector<unsigned int> fastTriggers;	
	//pointers to makers - get them in Init()
	StMuDstMaker*		muDstMaker;
	StEmcADCtoEMaker*	mADCtoEMaker;
	//StEmcTriggerMaker*	emcTrigMaker;
	StTriggerSimuMaker* emcTrigMaker;

	//all these random BEMC objects
	StBemcTables*		mTables;
	StEmcGeom*		mEmcGeom;
	StEmcPosition*		mEmcPosition;
	StEmcCollection*	mEmcCollection;
	StEmcGeom*              mSmdEGeom;
	StEmcGeom*              mSmdPGeom;
	
	//tower info (0==BTOW, 1==BPRS, 2=BSMDE, 3=BSMDP)
	unsigned short	mADC[2][4800];
	unsigned short mADCSmd[2][18000];
	float			mPedestal[2][4800];
	float			mPedRMS[2][4800];
	float mPedestalSmd[2][18000];
	float mPedRMSSmd[2][18000];
	int				mStatus[2][4800];
	int mStatusSmd[2][18000];
	unsigned char	mCapacitor[4800]; //only for BPRS
	unsigned char mCapacitorSmd[2][18000];
	
	void getADCs(int det); //1==BTOW, 2==BPRS, 3=BSMDE, 4=BSMDP
	pair<unsigned short, pair<float,float> > getTrackTower(StMuTrack* track, bool useExitRadius=false, int det=1);
	float getTrackDeltaR(float track_eta, float track_phi, int id);
	pair<float, float> getTrackDetaDphi(float track_eta, float track_phi, int id, int det);
	double highestNeighbor(int id);
	
	
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
	void addHTTPTrigger(unsigned int trigId);
	void addFastTrigger(unsigned int trigId);
	
	ClassDef(StEmcOfflineCalibrationMaker, 3)
};

#endif
