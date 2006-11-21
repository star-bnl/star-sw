//TChargedPionEvent.cxx

#include "TChargedPionEvent.h"

ClassImp(TChargedPion)

TChargedPion::TChargedPion() { }

TChargedPion::~TChargedPion() { }

void TChargedPion::Clear(Option_t* option) { }

ClassImp(TChargedPionEvent)

TChargedPionEvent::TChargedPionEvent()
{
	pionCandidates = new TClonesArray("TChargedPion",3);
	lowPtTracks = new TClonesArray("TChargedPion",6);
	nPionCandidates = nLowPtTracks = 0;
	
	//initialize some arrays to zero because they're not always filled depending on data/simulation
	for(int i=0; i<5; i++)	spinQA[i]=0;
	for(int i=0; i<10; i++)	vx[i]=vy[i]=vz[i]=ranking[i]=0.;
	for(int i=0; i<11; i++)	parton1[i]=parton2[i]=0;
	for(int i=0; i<4; i++)	flavor[i]=0;
	for(int i=0; i<5; i++) df1[i]=df2[i]=0.;
	for(int i=0; i<2; i++) f1[i]=f2[i]=0.;
	
	//trust all basic variables are initialized to zero;
}



TChargedPionEvent::~TChargedPionEvent()
{
	pionCandidates->Clear();
	lowPtTracks->Clear();
}

void TChargedPionEvent::Clear(Option_t* option)
{
	pionCandidates->Clear();
	lowPtTracks->Clear();
	nPionCandidates = nLowPtTracks = 0;
	for(int i=0; i<10; i++)	vx[i]=vy[i]=vz[i]=ranking[i]=0.;
	
	//shouldn't be caught with stale data in other vars -- either they're filled each event or not at all
}

void TChargedPionEvent::addTrack(TChargedPion* track){
	if(track->pt > 2.)
	{
		new ( (*pionCandidates)[pionCandidates->GetLast()+1] ) TChargedPion(*track);
		nPionCandidates++;
	}
	else
	{
		new ( (*lowPtTracks)[lowPtTracks->GetLast()+1] ) TChargedPion(*track);
		nLowPtTracks++;
	}		
}
