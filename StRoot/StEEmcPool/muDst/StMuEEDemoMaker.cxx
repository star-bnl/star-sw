// *-- Author : Victor Perevoztchikov
// 
// $Id: StMuEEDemoMaker.cxx,v 1.3 2003/09/02 17:57:54 perev Exp $

#include "StMuEEDemoMaker.h"

#include "StChain.h"
#include "St_DataSetIter.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/StEEmcSmdGeom.h"


#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/StEEmcDbIndexItem1.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"

ClassImp(StMuEEDemoMaker)

StMuEEDemoMaker::StMuEEDemoMaker(const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);
  geomTw= new EEmcGeomSimple(); // tower geomtry
  geomSmd =  StEEmcSmdGeom::instance(); //strip geometry, do NOT call new StEEmcSmdGeom()

  // connect to eemcDB
  eeDb = (StEEmcDbMaker*)GetMaker("eemcDb");
  if(eeDb==0) {
    printf("eemcDB must be in the chain, fix it or drop %s-maker, JB\n",GetName());
    assert(eeDb); // eemcDB must be in the chain, fix it
  }
}


StMuEEDemoMaker::~StMuEEDemoMaker(){
  //
}


//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StMuEEDemoMaker::Init(){
  // Create tables
  // Create Histograms    
   return StMaker::Init();
}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StMuEEDemoMaker::Make(){
  //
  // PrintInfo();
  //
  
  printf("%s::Make() is called .................................\n",GetName());
    
  if(eeDb->valid()<=0) {
    printf("%s aborted, due to no eemcDb records\n",GetName());
    return kStErr;
  }
  
  StMuEmcCollection* emc = mMuDstMaker->muDst()->emcCollection();
  if (!emc) {
    printf(" No EMC data for this event\n");
     return kStOK;
  }

  int isec,ieta,isub,istrip,adc,ipre;
  StMuEmcHit *hit;

  int i, nh;
 
  printf("\nTotal %d hits in Tower\n",emc->getNEndcapTowerADC());
  nh=0;
  for (i=0; i< emc->getNEndcapTowerADC(); i++) {
    emc->getEndcapTowerADC(i,adc,isec,isub,ieta);
    if (adc<=0) continue; // print only non-zero values
    nh++;
    // access geometry info
    float etaCenter     =geomTw->getEtaMean(ieta);
    float phiCenter     =geomTw->getPhiMean(isec,isub);
    TVector3 r= geomTw-> getTowerCenter(isec, isub,ieta);

    printf("\nTower %2.2dT%c%2.2d  phi/deg=%6.1f eta=%5.2f x=%4.1f y=%4.1f z=%5.1f: adc=%4d\n   ",isec+1,isub+'A',ieta+1,phiCenter/3.14*180,etaCenter,r.x(),r.y(),r.z(),adc );
 
    #if 0
    // more geometry info for towers, see .h
    float etaHalfWidth  =geomTw->getEtaHalfWidth(ieta);
    float phiHalfWidth  =geomTw->getPhiHalfWidth(isec,isub);
    
    // center of the tower in two ways
    TVector3 r1=geomTw-> getDirection( etaCenter, phiCenter);
    #endif

    // ....... Access  DB 
    const StEEmcDbIndexItem1 *dbItem=eeDb->getT(isec+1,isub+'A',ieta+1);
    assert(dbItem); //  fatal error in EEmcDb-maker
    dbItem->print();
  }
  printf("Total %d towers with ADC>0\n",nh);

  //====================== PRE/POST
  nh= emc->getNEndcapPrsHits();
  printf("\nTotal %d hits in pre1+2+post\n",nh);
  for (i=0; i<nh; i++) {
    hit=emc->getEndcapPrsHit(i,isec,isub,ieta,ipre);
    printf("\n\npre/post(%d) %2.2d%c%c%2.2d : energy=%f  adc=%d\n",ipre+1,isec+1,ipre+'P',isub+'A',ieta+1,hit->getEnergy(),hit->getAdc());
    // ....... Access  DB 
    char name[20];
    sprintf(name,"%2.2d%c%c%2.2d",isec+1,ipre+'P',isub+'A',ieta+1);
    printf("  DB: name='%s'",name);
    int index=EEname2Index(name);
    printf(", index=%d, ", index);
    const StEEmcDbIndexItem1 *dbItem=eeDb->getByIndex(index);
    assert(dbItem); //  fatal error in EEmcDb-maker
    dbItem->print();

  }
  
  //====================== SMD
  char uv='U';
 
  for(uv='U'; uv<='V'; uv++) {
    nh= emc->getNEndcapSmdHits(uv);
    printf("\nTotal %d hits in SMD-%c\n",nh,uv);
    for (i=0; i<nh; i++) {
      hit=emc->getEndcapSmdHit(uv,i,isec,istrip);
      printf("\nSMD-%c  %2.2d%c%3.3d : energy=%f  adc=%d\n",uv,isec+1,uv,istrip+1,hit->getEnergy(),hit->getAdc());
      
      // ... geometry
      StructEEmcStrip st=geomSmd->EEmcStrip(1, isec+1, istrip+1);
      
      printf("   x1=%6.2f y1=%6.2f z1=%6.2f x2=%6.2f y2=%6.2f z2=%6.2f\n",
      	     st.end1.x(),
	     st.end1.y(),st.end2.x(),st.end1.z(),st.end2.y(),st.end2.z());
      

      // ....... Access  DB 
      char name[20];
      sprintf(name,"%2.2d%c%3.3d",isec+1,uv,istrip+1);
      printf("   DB: name='%s'",name);
      int index=EEname2Index(name);
      printf(", index=%d, ", index);
      const StEEmcDbIndexItem1 *dbItem=eeDb->getByIndex(index);
      assert(dbItem); //  fatal error in EEmcDb-maker
      dbItem->print();
 
    }
  }
  
  //====================== Clusters tw,pre12po,smdu, smdv
  for (int n=eemc ; n<=esmdv; n++) {
    int nClusters = emc->getNClusters(n);
    printf("EEMC depth=%d nClust=%d\n",n,nClusters);
    for (int i=0; i<nClusters; i++) {
      StMuEmcCluster* c =  emc->getCluster(i,n);
      printf("Cluster(%02i,%02i): energy=%f phi=%f eta=%f nHits=%d\n"
             ,n,i,c->getEnergy(), c->getPhi(), c->getEta(), c->getNHits() ); 
      }
  }

  //==================== points
  int nPoints = emc->getNEndcapPoints();
  printf("Points: %d\n",nPoints);
  for (int n=0; n<nPoints; n++) {
    StMuEmcPoint* p =  emc->getPoint(n);
    printf("Point(%02i) : energy=%f phi=%f eta=%f\n",n,p->getEnergy(), p->getPhi(), p->getEta()); 
  }

 return kStOK;
}




// $Log: StMuEEDemoMaker.cxx,v $
// Revision 1.3  2003/09/02 17:57:54  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  2003/08/28 17:52:57  balewski
// works for SMD, Wei-Ming fix
//
// Revision 1.1  2003/08/27 22:56:30  balewski
// example of access to EEMC data+DB+geom from muDst
//
// Revision 1.15  2002/04/28 01:28:36  jeromel
// Reshaped comments for doxygen. Hopefully, users will propagate this good
// habit.
//
// Revision 1.14  2000/06/23 16:50:07  fisyak
// remove params
//
// Revision 1.13  1999/12/19 16:07:01  perev
// Add README
//
// Revision 1.12  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.11  1999/07/10 22:59:16  fine
// Some comments have been introduced to show html docs
//






