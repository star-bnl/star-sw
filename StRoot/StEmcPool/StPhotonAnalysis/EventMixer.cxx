#include <Riostream.h>
#include <TMath.h>

#include <StEmcPool/StPhotonCommon/MyEvent.h> 
#include <StEmcPool/StPhotonCommon/MyPoint.h> 
//#include <StEmcPool/StPhotonCommon/MyMcTrack.h> 

#include "AnaCuts.h"

#include "EventMixer.h"
using namespace std;
ClassImp(EventMixer)

EventMixer::EventMixer(const char *flag)
{
  fNmixed=0;

  ev_array=new TClonesArray("MyEvent",10);
  ev_array->SetOwner(kTRUE);  

  cuts=new AnaCuts(flag);
  cout<<"EventMixer is constructed"<<endl;

  h_minvMB_mixed=new TH2F("h_minvMB_mixed","invariant mass vs pT MB (mixed events)",cuts->nPtBinsMB,cuts->ptBinsMB.GetArray(),cuts->nMinvBinsMB,cuts->mInvLowMB,cuts->mInvHighMB);
  h_minvMB_mixed->Sumw2();

}
EventMixer::~EventMixer()
{
  cout<<"EventMixer is destructed"<<endl;
}
void EventMixer::addEvent(MyEvent *ev)
{
  if(ev->trigger()&1 && TMath::Abs(ev->vertex().Z())>0. && ev->numberOfPoints()>1){
    TClonesArray &array=*ev_array;
    new(array[fNmixed++]) MyEvent(*ev);

    if(fNmixed==10){
      mix();
      ev_array->Clear();
      fNmixed=0;
    }
  }
}

void EventMixer::mix()
{

  for(int i_ev=0;i_ev<10;i_ev++){
    
    MyEvent *e1=(MyEvent*)ev_array->At(i_ev);
    TClonesArray *clA=e1->getPointArray();

    for(int j_ev=i_ev+1;j_ev<10;j_ev++){

      MyEvent *e2=(MyEvent*)ev_array->At(j_ev);
      TClonesArray *clB=e2->getPointArray();
      
      MyPoint *p;
      MyPoint *pp;
      for(Int_t j=0;j<clA->GetEntries();j++)
	{
	  p=(MyPoint*)clA->At(j);

	  TVector3 pPos=p->position();
	  TVector3 pMom=pPos - e1->vertex();
	  pMom.SetMag(p->energy());
	  
	  if(!cuts->isPointOK(p,e1->vertex())) continue;
	  

	  for(Int_t jj=0;jj<clB->GetEntries();jj++)
	    {
	      pp=(MyPoint*)clB->At(jj);

	      TVector3 ppPos=pp->position();
	      TVector3 ppMom=ppPos - e2->vertex();
	      ppMom.SetMag(pp->energy());
	      if(!cuts->isPointOK(pp,e2->vertex())) continue;
	      

	      //two neutrals:
	      TVector3 pi0Mom=pMom+ppMom;
	      
	      Float_t angle=pMom.Angle(ppMom);
	      Float_t minv=TMath::Sqrt(2.*p->energy()*pp->energy()*(1. - TMath::Cos(angle)));
	      Float_t pTpion=pi0Mom.Pt();
	      Float_t etapion=pi0Mom.PseudoRapidity();
	      Float_t asymm=TMath::Abs(p->energy()-pp->energy())/(p->energy()+pp->energy());
	      
	      if(etapion<cuts->rapPionMinCUT||etapion>cuts->rapPionMaxCUT) continue;
	      if(asymm>cuts->asymmetryCUT) continue;
	      
	      h_minvMB_mixed->Fill(pTpion,minv);
	    }
	
	}

    }
  }
  

}

