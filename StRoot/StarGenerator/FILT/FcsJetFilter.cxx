#include "FcsJetFilter.h"

#include "StarGenerator/EVENT/StarGenParticle.h"
#include "StarGenerator/EVENT/StarGenEvent.h"
#include <string>
#include <iostream>
#include <fstream>
#include <cmath>
#include <vector>
#include <algorithm>

static const float ZFCS    =  710.16 + 13.9 + 15.0;
static const float XFCSMin =  16.69;
static const float XFCSMax =  16.69 + 22.0*5.542;
static const float YFCS    =  34.0/2.0 * 5.542;
static const float FVCUT   =  20.0; //cm 
static const float ETHR = 50.0;

static int NTOT=0;
static int NGOOD=0;

//_______________________________________________________________
FcsJetFilter::FcsJetFilter():StarFilterMaker("fcsJetFilter")
{
    cout<<"FCS JET filter is used!!!"<<endl;
}
//_______________________________________________________________
Int_t FcsJetFilter::Filter( StarGenEvent *mEvent){
    NTOT++;
    // Get a reference to the current event 
    StarGenEvent& event = *mEvent;

    //event.Print();
    if(event.GetNumberOfParticles() <= 0) {return kError;}
    
    TIter Iterator = event.IterAll();
    StarGenParticle *p = 0;

    float etot[2]={0.0,0.0};
    while( ( p = (StarGenParticle*)Iterator.Next() ) ){
	if(p->GetStatus() != 1)continue;
	if(p->GetPz() < 0.0) continue;
	//simple box cut
	float x = p->GetVx()/10.0 + p->GetPx() / p->GetPz() * (ZFCS - p->GetVz()/10.0);
	float absx=fabs(x);
	if(absx<XFCSMin+FVCUT || absx>XFCSMax-FVCUT) continue;
	float y = fabs (p->GetVy()/10.0 + p->GetPy() / p->GetPz() * (ZFCS - p->GetVz()/10.0));
	if(y>YFCS-FVCUT) continue;	
	//it is hitting FCS
	if(x>0.0)       {etot[1]+=p->GetEnergy();} //south
	else if(x<=0.0) {etot[0]+=p->GetEnergy();} //north
    }
    if(etot[0] > ETHR || etot[1] > ETHR){
	NGOOD++;
	cout << Form("FcsJetFilter : N_Genearted=%6d  N_Accepted=%6d  R=%6.4f",
		     NTOT,NGOOD,float(NGOOD)/float(NTOT) ) << endl;
	return StarGenEvent::kAccept;
    }
    return StarGenEvent::kReject;
}
