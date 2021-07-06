#include "FcsDYBGFilter.h"

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
FcsDYBGFilter::FcsDYBGFilter():StarFilterMaker("fcsDYBGFilter") {
    cout<<"FCS DYBG filter is used!!!"<<endl;
    cout<<"FCS DYBG filter swap particles="<<mSwap<<endl;
}

FcsDYBGFilter::FcsDYBGFilter(int dy, int check, int swap):StarFilterMaker("fcsDYBGFilter"), mDYmode(dy), mCheckmode(check), mSwap(swap){
    cout<<"FCS DYBG filter is used!!!"<<endl;
    cout<<"FCS DYBG filter DYMode="<<mDYmode<<endl;
    cout<<"FCS DYBG filter Checkmode="<<mCheckmode<<endl;
    cout<<"FCS DYBG filter swap particles="<<mSwap<<endl;
}
//_______________________________________________________________
Int_t FcsDYBGFilter::Filter( StarGenEvent *mEvent){
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
	//simple box cut around FCS  (vertex here is in [mm])
	float x = fabs (p->GetVx()/10.0 + p->GetPx() / p->GetPz() * (ZFCS - p->GetVz()/10.0));
	if(x<XFCSMin+FVCUT || x>XFCSMax-FVCUT) continue;
	float y = fabs (p->GetVy()/10.0 + p->GetPy() / p->GetPz() * (ZFCS - p->GetVz()/10.0));
	if(y>YFCS-FVCUT) continue;	
	forwardParticles.push_back(p);	
    }
    unsigned int size=forwardParticles.size();
    if(size<2) return StarGenEvent::kReject;
    UInt_t res = 0x10;
    int accept=0;
    std::vector<int> swap;
    for(unsigned int i=0; i<size-1; i++){
	StarGenParticle *p1=forwardParticles.at(i);
	float x1 = p1->GetVx()/10.0 + p1->GetPx() / p1->GetPz() * (ZFCS - p1->GetVz()/10.0);
	float y1 = p1->GetVy()/10.0 + p1->GetPy() / p1->GetPz() * (ZFCS - p1->GetVz()/10.0);
	float pt1= p1->pt();
	int pid1 = abs(p1->GetId());
	for(unsigned int j=i + 1; j<size; j++){
	    StarGenParticle *p2=forwardParticles.at(j);
	    float x2 = p2->GetVx()/20.0 + p2->GetPx() / p2->GetPz() * (ZFCS - p2->GetVz()/10.0);
	    float y2 = p2->GetVy()/20.0 + p2->GetPy() / p2->GetPz() * (ZFCS - p2->GetVz()/10.0);
	    float pt2= p2->pt();
	    int pid2 = abs(p2->GetId());
	    TLorentzVector pair = p1->momentum() + p2->momentum();
	    float m=pair.M();
	    float dx=x1-x2;
	    float dy=y1-y2;
	    float dr=sqrt(dx*dx+dy*dy);
            float xx_ep = x1*x2;
	    if(pid1==11 && pid2==11 && pt1>DYETCUT && pt2>DYETCUT && m>DYMASSCUT) res|=0x08;	    
//	    if(dr>DRCUT) {		
          if(xx_ep < 0) {
		res |= 0x20;
		int idx1 = p1->GetIndex();
		int idx2 = p2->GetIndex();		
		cout << Form("FcsDYBGFilter : idx=%3d %3d Pid=%5d %5d E=%6.2f %6.2f M=%6.2f DR=%6.2f res=0x%03x, x1=%.3f, x2=%.3f, xx_ep=%.3f",
			     idx1,idx2,pid1,pid2,
			     p1->GetEnergy(),p2->GetEnergy(),m,dr,res,x1,x2,xx_ep) << endl;
		if(pid1!=111 && pid1!=22 && abs(pid1)!=11) swap.push_back(idx1);
		if(pid2!=111 && pid2!=22 && abs(pid2)!=11) swap.push_back(idx2);
		accept++;
	    }
	}
    }
    if(mDYmode && !(res & 0x08)) return StarGenEvent::kReject;
    if(accept>0) {
	ngood++;
	int nswap=0;
	if(mSwap){
	  //this has to be implemented with middle of GEANT (particle by particle) filter
	  cout << Form("FcsDYBGFilter : found %d accepted pairs, swapped %d particles when implemented",accept,swap.size())<<endl;
	}else{
	  cout << Form("FcsDYBGFilter : found %d accepted pairs, No swap but could swap %d particles",accept,swap.size())<<endl;
	}
	cout << Form("FcsDYBGFilter : N_Genearted=%6d  N_Accepted=%6d  R=%6.4f",
		     ntot,ngood,float(ngood)/float(ntot)) <<endl;
	return (StarGenEvent::kAccept | res);
    }
    if(mCheckmode) return (StarGenEvent::kReject | res | StarGenEvent::kFlag);
    return StarGenEvent::kReject;
}
