#include "FcsDYBGFilterSingle.h"

#include "StarGenerator/EVENT/StarGenParticle.h"
#include "StarGenerator/EVENT/StarGenEvent.h"
#include <string>
#include <iostream>
#include <fstream>
#include <cmath>
#include <vector>
#include <algorithm>

static const float ZFCS    = 710.16 + 13.9 + 15.0;
static const float XFCSMin = 16.69;
static const float XFCSMax = 16.69 + 22.0 * 5.542;
static const float YFCS    = 34.0/2.0 * 5.542;
static const float FVCUT   = 0;//2.5; //cm
static const float ETCUT   = 0.8;
static const float DRCUT   = 40.0;
static const float DYETCUT = 1.0;
static const float DYMASSCUT= 2.5;


static int ntot=0;
static int ngood=0;

using namespace std;
//_______________________________________________________________
FcsDYBGFilterSingle::FcsDYBGFilterSingle():StarFilterMaker("fcsDYBGFilterSingle") {
    cout<<"FCS DYBG filter is used!!!"<<endl;
    cout<<"FCS DYBG filter swap particles="<<mSwap<<endl;
}

FcsDYBGFilterSingle::FcsDYBGFilterSingle(int dy, int check, int swap):StarFilterMaker("fcsDYBGFilterSingle"), mDYmode(dy), mCheckmode(check), mSwap(swap){
    cout<<"FCS DYBG filter is used!!!"<<endl;
    cout<<"FCS DYBG filter DYMode="<<mDYmode<<endl;
    cout<<"FCS DYBG filter Checkmode="<<mCheckmode<<endl;
    cout<<"FCS DYBG filter swap particles="<<mSwap<<endl;
}
//_______________________________________________________________
Int_t FcsDYBGFilterSingle::Filter( StarGenEvent *mEvent){
    ntot++;
    // Get a reference to the current event 
    StarGenEvent& event = *mEvent;
    
    //event.Print();
    int np=event.GetNumberOfParticles();
    if(np <= 0) {return kError;}
    
    // apply DYBG conditions for events
    TIter Iterator = event.IterAll();
    StarGenParticle *p = 0;
    vector<StarGenParticle*> forwardParticles;
    while( ( p = (StarGenParticle*)Iterator.Next() ) ){
	int pid = abs(p->GetId());
//	if(p->GetStatus()!=1 || pid==111) continue;
        if(p->GetStatus()!=1) continue;
	if(p->GetPz()<0.0) continue; // +z direction only
	if(p->pt()<ETCUT) continue; 
	forwardParticles.push_back(p);	
    }
    unsigned int size=forwardParticles.size();
    if(size<1) return StarGenEvent::kReject;
    UInt_t res = 0x10;
    bool isInSouth=0;
    for(unsigned int i=0; i<size; i++){
	StarGenParticle *p=forwardParticles.at(i);
	float x = p->GetVx()/10.0 + p->GetPx() / p->GetPz() * (ZFCS - p->GetVz()/10.0);
        float y = fabs (p->GetVy()/10.0 + p->GetPy() / p->GetPz() * (ZFCS - p->GetVz()/10.0));
        if(x > XFCSMin && x < XFCSMax && y<YFCS) isInSouth=1;
    }

    bool isInNorth=0;
    for(unsigned int i=0; i<size; i++){
        StarGenParticle *p=forwardParticles.at(i);
        float x = p->GetVx()/10.0 + p->GetPx() / p->GetPz() * (ZFCS - p->GetVz()/10.0);
        float xabs = fabs(x);
        float y = fabs (p->GetVy()/10.0 + p->GetPy() / p->GetPz() * (ZFCS - p->GetVz()/10.0));
        if(x > 0) continue;
        if(xabs > XFCSMin && xabs < XFCSMax && y<YFCS) isInNorth=1;
    }

    if(isInSouth && !isInNorth) {
	ngood++;
	cout << Form("FcsDYBGFilterSingle : N_Genearted=%6d  N_Accepted=%6d  R=%6.4f",
		     ntot,ngood,float(ngood)/float(ntot)) <<endl;
	return (StarGenEvent::kAccept);
    }
    return StarGenEvent::kReject;
}
