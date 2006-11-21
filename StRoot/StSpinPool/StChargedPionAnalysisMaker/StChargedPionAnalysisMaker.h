#ifndef STAR_StChargedPionAnalysisMaker
#define STAR_StChargedPionAnalysisMaker

class TFile;
class TTree;
class TChargedPionEvent;
class TChargedPion;

class StMuDstMaker;
class StEmcTriggerMaker;
class StSpinDbMaker;
class StMcEventMaker;

#ifndef StMaker_H
#include "StMaker.h"
extern "C" void polar_(int*,double*,double*,double*,int*);
extern "C" void parpol2_(int*, double*, double*, double* ,double* , double*, double*, double*, double*, double*, double*);
extern "C" void grv98pa_(int*, double*, double*, double*, double*, double*, double*, double*, double*);
extern "C" void grv98f2_(int*, double*, double*, double*, double*, double*, double*);
extern "C" void unpolar_(int*, double*, double*, double*, int*);
extern "C" void num_(double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
extern "C" void denom_(double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
extern "C" Double_t ctq5pd_(int*,int*,double*,double*,int*);
#endif

class StChargedPionAnalysisMaker : public StMaker
{
private:
	const char* filename;
	TFile* myFile;
	TTree* pionTree;
	TChargedPionEvent* myEvent;
	TChargedPion* pion;
		
	//pointers to makers - get them in Init()
	StMuDstMaker* muDstMaker;
	StEmcTriggerMaker* emcTrigMaker;
	StSpinDbMaker* spDbMaker;
	
	//pythia stuff
	int makeSimulatedData();
	double getPartonicALL(double a, double b, double c, int d, int e, int f, int g, int h);
	double getPolPDF(int x1, double d1, double d2, int set);
	double getUnPolPDF(int x1, double d1, double d2, int set);
	
public:
	StChargedPionAnalysisMaker(const char* name="pionMaker", const char* file="test.root");
	virtual ~StChargedPionAnalysisMaker();
	
	bool isRealData;
	
	virtual Int_t Init();
	virtual Int_t InitRun(int run);
	virtual Int_t Make();
	
	virtual Int_t Finish();
	
	virtual void Clear(Option_t *option="");
	
	ClassDef(StChargedPionAnalysisMaker, 0)
};

#endif
