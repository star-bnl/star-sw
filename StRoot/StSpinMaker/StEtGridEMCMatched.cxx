//////////////////////////////////////////////////////////////////////
//
// $Id: StEtGridEMCMatched.cxx,v 1.1 2002/02/11 20:30:48 akio Exp $
// $Log: StEtGridEMCMatched.cxx,v $
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
//
//////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StEventTypes.h"
#include "StEvent.h"
#include "StEtGridEMCMatched.h"
#include "TClonesArray.h"

ClassImp(StEtGridEMCMatched);

void StEtGridEMCMatched::createKeys(){
  mNEta   = 40;
  mNPhi   = 120;
  mEtaMax =-1.0;
  mEtaMin = 1.0;
  mPhiMax =-M_PI;
  mPhiMin = M_PI;

  mNEtaE = 12;
  mNPhiE = 60;
  mEtaMaxE = 1.0;
  mEtaMinE = 2.0;
  mPhiMaxE =-M_PI;
  mPhiMinE = M_PI;

  nKeys=mNEta*mNPhi + mNEtaE*mNPhiE;

  mGrid = new TClonesArray("StEtCell",nKeys);  
  etaMinKey = new float[nKeys];
  etaMaxKey = new float[nKeys];
  phiMinKey = new float[nKeys];
  phiMaxKey = new float[nKeys];

  int key = 0;
  for(int i=0; i<mNEta; i++){  
    for(int j=0; j<mNPhi; j++){
      etaMinKey[key] =     float(i)/float(mNEta)*(mEtaMax-mEtaMin) + mEtaMin;
      etaMaxKey[key] = float(i+1.0)/float(mNEta)*(mEtaMax-mEtaMin) + mEtaMin;
      phiMinKey[key] =     float(j)/float(mNPhi)*(mPhiMax-mPhiMin) + mPhiMin;
      phiMaxKey[key] = float(j+1.0)/float(mNPhi)*(mPhiMax-mPhiMin) + mPhiMin;
      key++;
    }
  }
  for(int i=0; i<mNEtaE; i++){  
    for(int j=0; j<mNPhiE; j++){
      etaMinKey[key] =     float(i)/float(mNEta)*(mEtaMaxE-mEtaMinE) + mEtaMinE;
      etaMaxKey[key] = float(i+1.0)/float(mNEta)*(mEtaMaxE-mEtaMinE) + mEtaMinE;
      phiMinKey[key] =     float(j)/float(mNPhi)*(mPhiMaxE-mPhiMinE) + mPhiMinE;
      phiMaxKey[key] = float(j+1.0)/float(mNPhi)*(mPhiMaxE-mPhiMinE) + mPhiMinE;
      key++;
    }
  }
}

int StEtGridEMCMatched::findKey(float eta, float phi){
  if(eta<1.0){
    int ieta = int( (eta-mEtaMin)/(mEtaMax-mEtaMin)*mNEta );
    while(phi>M_PI) {phi-=2*M_PI;}
    while(phi<-M_PI) {phi+=2*M_PI;}
    int iphi = int( (phi-mPhiMin)/(mPhiMax-mPhiMin)*mNPhi );
    if(ieta < 0 || ieta >= mNEta || iphi < 0 || iphi >= mNPhi){
      cout << "StEtGridEMCMatched::findKey  *ERROR*  Got invalid key "
	   << ieta << " " << iphi << " " << mNEta << " " << mNPhi << endl;
      return -1;
    }    
    return ieta*mNPhi + iphi;
  }else{
    int ieta  = int( (eta-mEtaMinE)/(mEtaMaxE-mEtaMinE)*mNEtaE );
    while(phi>M_PI) {phi-=2*M_PI;}
    while(phi<-M_PI) {phi+=2*M_PI;}
    int iphi = int( (phi-mPhiMinE)/(mPhiMaxE-mPhiMinE)*mNPhiE );
    if(ieta < 0 || ieta >= mNEta || iphi < 0 || iphi >= mNPhi){
      cout << "StEtGridEMCMatched::findKey  *ERROR*  Got invalid key "
	   << ieta << " " << iphi << " " << mNEtaE << " " << mNPhiE << endl;
      return -1;
    }    
    return mNEta*mNPhi + ieta*mNPhiE + iphi;
  }
}

