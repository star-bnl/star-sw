//////////////////////////////////////////////////////////////////////
//
// $Id: StEtGridFlat.cxx,v 1.4 2003/09/02 17:59:01 perev Exp $
// $Log: StEtGridFlat.cxx,v $
// Revision 1.4  2003/09/02 17:59:01  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.3  2002/12/04 20:28:07  thenry
// StppuDstMaker was modified to allow multiple jet analysis modules to be
// run simultaneosly with various parameters while the Maker loads the events
// and analyses them.  Four different jet analyzers exist:
//
// Konstanin's Analyzers:
//     Kt type: StppKonstKtJetAnalyzer
//     Cone type: StppKonstConeJetAnalyzer
//
// Mike's Analyzers:
//     Kt type: StppMikeKtJetAnalyzer
//     Cone type: StppMikeConeJetAnalyzer
//
// These modules all require the StJetFinder modules.
//
// Revision 1.2  2002/06/24 13:22:59  akio
// numerous bug fix & updates
//
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
//
//////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "StEventTypes.h"
#include "StEvent.h"
#include "StEtGridFlat.h"
#include "TClonesArray.h"

ClassImp(StEtGridFlat)

void StEtGridFlat::createKeys(int neta, int nphi, float eta1, float eta2, float phi1, float phi2){
  mNEta=neta;
  mNPhi=nphi;
  mEtaMin=eta1;
  mEtaMax=eta2;
  mPhiMin=phi1;
  mPhiMax=phi2;

  nKeys=mNEta*mNPhi;
  mGrid = new TClonesArray("StEtCell",nKeys);  
  etaMinKey = new float[nKeys];
  etaMaxKey = new float[nKeys];
  phiMinKey = new float[nKeys];
  phiMaxKey = new float[nKeys];

  int key = 0;
  for(int i=0; i<mNEta; i++){  
    for(int j=0; j<mNPhi; j++){      
      etaMinKey[key] =        (float)i /(float)mNEta * (mEtaMax-mEtaMin) + mEtaMin;
      etaMaxKey[key] =  (float)(i+1.0) /(float)mNEta * (mEtaMax-mEtaMin) + mEtaMin;
      phiMinKey[key] =        (float)j /(float)mNPhi * (mPhiMax-mPhiMin) + mPhiMin;
      phiMaxKey[key] =  (float)(j+1.0) /(float)mNPhi * (mPhiMax-mPhiMin) + mPhiMin;
      key++;
    }
  }
}

int StEtGridFlat::findKey(float eta, float phi){
  int ieta = int( (eta-mEtaMin)/(mEtaMax-mEtaMin)*mNEta );
  while(phi>M_PI) {phi-=2*M_PI;}
  while(phi<-M_PI) {phi+=2*M_PI;}
  int iphi = int( (phi-mPhiMin)/(mPhiMax-mPhiMin)*mNPhi );
  if(ieta < 0 || ieta >= mNEta || iphi < 0 || iphi >= mNPhi){
    cout << "StEtGridFlat::findKey  *ERROR*  Got invalid key" << endl;
    cout << "  eta, found i-eta, max i-eta = " <<eta <<" "<<ieta <<" "<<mNEta<<endl;
    cout << "  phi, found i-phi, max i-phi = " <<phi <<" "<<iphi <<" "<<mNPhi<<endl;
    return -1;
  }    
  return ieta*mNPhi + iphi;
}
