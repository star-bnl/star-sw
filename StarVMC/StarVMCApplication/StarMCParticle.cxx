// $Id: StarMCParticle.cxx,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
#include "StarMCParticle.h"
#include "Riostream.h"

using namespace std;

ClassImp(StarMCParticle)
//_____________________________________________________________________________
void StarMCParticle::Print(const Option_t *opt) const {
  cout << "Track ID:  " << fID << " IdGen: \t" << GetIdGen();
  fParticle->Print();  
  if (GetMother()) {
    cout << "\tMother:  " << GetMother()->GetParticle()->GetName() 
	 << "  with ID: " << GetMother()->GetID() << "\t" <<  GetMother()->GetIdGen();
  }
  else			    
    cout << "\tPrimary  ";
  cout << "\tNumber of daughters: " << GetNofDaughters() << endl;
}  
//_____________________________________________________________________________
void StarMCParticle::PrintDaughters() const {
  for (Int_t i=0; i<GetNofDaughters(); i++)  {
    cout << i << "th daughter: " << endl;
    GetDaughter(i)->Print();
  }  
}  
//_____________________________________________________________________________
StarMCParticle* StarMCParticle::GetDaughter(Int_t i) const {
  if (i < 0 || i >= GetNofDaughters())
    Fatal("GetDaughter", "Index out of range"); 
  return (StarMCParticle*) fDaughters.At(i);
}  

