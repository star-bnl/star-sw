#include "StFgtGeneralBase.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"

#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StEvent/StFgtStrip.h"

#include "StRoot/StMuDSTMaker/COMMON/StMuDst.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtStrip.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtAdc.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtCluster.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtStripAssociation.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StarClassLibrary/StThreeVectorF.hh"

#include <set>
//#define COSMIC
//#define ONE_HIT_PER_QUAD
//#define USE_VTX

#ifdef COSMIC
//#include "StFgtCosmicAlignment.h"
#endif


StFgtGeneralBase::StFgtGeneralBase(const Char_t* name): StMaker( name ),m_fillFromEvent(false),evtNr(0),m_effDisk(2), fgtCollection(0), mVertexNumber(0),mUseEHTTrigs(false)
{
  sprintf(fileBase,"%s",".");
  m_isCosmic=false;
  chargeMatchCut=1.5;
  pClusters=new vector<generalCluster>*[6];
  pClusters[0]=&clustersD1;
  pClusters[1]=&clustersD2;
  pClusters[2]=&clustersD3;
  pClusters[3]=&clustersD4;
  pClusters[4]=&clustersD5;
  pClusters[5]=&clustersD6;

  pStrips=new vector<generalStrip>[50]; //for each quad so we don't have to lookup so much...
  StFgtDbMaker *fgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );
  if( !fgtDbMkr ){
    LOG_FATAL << "StFgtDb not provided and error finding StFgtDbMaker" << endm;
  }
  mDb = fgtDbMkr->getDbTables();
  if( !mDb ){
    LOG_FATAL << "StFgtDb not provided and error retrieving pointer from StFgtDbMaker '"
	      << fgtDbMkr->GetName() << endm;
  }
  chargeMaxAdcCorr=new TH2D("chargeMaxAdcCorr","chargeMaxAdcCorr",50,0,10000,50,0,1000);
  chargeMaxAdcIntCorr=new TH2D("chargeMaxIntAdcCorr","chargeMaxAdcIntCorr",50,0,10000,50,0,1000);
  hIpZEv=new TH1D("IP_ZEv","IP_ZEv",70,-150,150);
  clusWChargeMatch=new TH1D("clusWChargeMatch","clusWChargeMatch",101,0,100);
  clusWOChargeMatch=new TH1D("clusWOChargeMatch","clusWOChargeMatch",101,0,100);
  char buffer[100];
  hNumPulsesP=new TH1D*[6*4];
  hNumChargesP=new TH1D*[6*4];
  hNumPulsesR=new TH1D*[6*4];
  hNumChargesR=new TH1D*[6*4];
  hNumClustersP=new TH1D*[6*4];
  hNumClustersR=new TH1D*[6*4];


  for(int iD=0;iD<6;iD++)
    {
      for(int iQ=0;iQ<4;iQ++)
	{
	  sprintf(buffer,"validPulsesP_D%d_Q%d",iD+1,iQ);
	  hNumPulsesP[iD*4+iQ]=new TH1D(buffer,buffer,100,0,100);
	  sprintf(buffer,"validPulsesR_D%d_Q%d",iD+1,iQ);
	  hNumPulsesR[iD*4+iQ]=new TH1D(buffer,buffer,100,0,100);
	  sprintf(buffer,"validChargesR_D%d_Q%d",iD+1,iQ);
	  hNumChargesR[iD*4+iQ]=new TH1D(buffer,buffer,200,0,200);
	  sprintf(buffer,"validChargesP_D%d_Q%d",iD+1,iQ);
	  hNumChargesP[iD*4+iQ]=new TH1D(buffer,buffer,200,0,200);
	  sprintf(buffer,"validClustersP_D%d_Q%d",iD+1,iQ);
	  hNumClustersP[iD*4+iQ]=new TH1D(buffer,buffer,200,0,200);
	  sprintf(buffer,"validClustersR_D%d_Q%d",iD+1,iQ);
	  hNumClustersR[iD*4+iQ]=new TH1D(buffer,buffer,200,0,200);
	}
    }


}

void StFgtGeneralBase::SetFileBase(const Char_t* m_filebase)
{
  cout <<"setting file base to " << m_filebase <<endl;
  sprintf(fileBase,"%s",m_filebase);
}


Int_t StFgtGeneralBase::Finish()
{
  cout <<"gb finish" <<endl;
  evStatistics=new TH1D("fgtEventStatistics","fgtEventStatistics",10,0,9);
  //  TCanvas c;
  //  chargeMaxAdcCorr->Draw("colz");
  ///---->  c.SaveAs("chargeMaxAdcCorr.png");
  //  chargeMaxAdcIntCorr->Draw("colz");
  ///---->  c.SaveAs("chargeMaxAdcIntCorr.png");
  //  hIpZEv->Draw();
  ///---->  c.SaveAs("ipZEv.png");
  //  cout <<"done saving" <<endl;
  char buffer[100];
  sprintf(buffer,"%s/general.root",fileBase);
  TFile f(buffer,"recreate");
  clusWChargeMatch->Write();
  clusWOChargeMatch->Write();
  for(int iD=0;iD<6;iD++)
    {
      for(int iQ=0;iQ<4;iQ++)
	{
	  hNumPulsesP[iD*4+iQ]->Write();
	  hNumChargesP[iD*4+iQ]->Write();
	  hNumClustersP[iD*4+iQ]->Write();
	  hNumPulsesR[iD*4+iQ]->Write();
	  hNumChargesR[iD*4+iQ]->Write();
	  hNumClustersR[iD*4+iQ]->Write();
	}
    }
  f.Write();
  f.Close();
  cout <<"done" <<endl;
  return kStOk;
}

Bool_t StFgtGeneralBase::validPulse(generalStrip& strip)
{
  for(int i=0;i<4;i++)
    {

      Float_t adc1=strip.adc[i];
      Float_t adc2=strip.adc[i+1];
      Float_t adc3=strip.adc[i+2];
      Float_t cut=5*strip.pedErr;
      if(adc1>cut && adc2 >cut && adc3 > cut)
	{
	  if(adc1 <adc2 && adc2 < adc3)
	    return true;
	}
    }
  return false;
}


Int_t StFgtGeneralBase::Make()
{
  if(!m_fillFromEvent)
    {

  //if we constructed fgtCollection ourself, delete it ourself
      if(fgtCollection)
	{
	  //fgtCollection->Clear();
	  //delete fgtCollection;
	}
    }

  Int_t ierr=kStOk;
  evtNr++;
  clustersD1.clear();
  clustersD2.clear();
  clustersD3.clear();
  clustersD4.clear();
  clustersD5.clear();
  clustersD6.clear();
  //  cout << " clearing strips..." <<endl;
  for(int i=0;i<50;i++)
    {
      pStrips[i].clear();
    }
  //  cout << " done" <<endl;
  if(m_fillFromEvent)
    {
      StEvent* eventPtr = (StEvent*)GetInputDS("StEvent");
      fgtCollection=eventPtr->fgtCollection();
      if( eventPtr ) {
	fillFromStEvent(fgtCollection);
      }
    }
  else
    {
      fgtCollection=new StFgtCollection();
      fillFromMuDst(*fgtCollection);
      AddData(new TObjectSet("FGTCOLLECTION",fgtCollection,kTRUE));
      //      cout <<"read mDst " <<endl;
      fillFromStEvent(fgtCollection);
      //      cout <<"filled event" <<endl;
    }

  //do associations
  doEvAssoc();
  mapGeoId2Cluster.clear();
  return ierr;
}


void StFgtGeneralBase::doEvAssoc()
{
  for(int i=0;i<6;i++)
    {
      for(vector<generalCluster>::iterator it=pClusters[i]->begin();it!=pClusters[i]->end();it++)
	{
	  Int_t geoId=it->centralStripGeoId;
	  StFgtHitCollection *fgtHitColPtr = 0;
	  fgtHitColPtr = fgtCollection->getHitCollection( i );
	  if( fgtHitColPtr )
	    {
	      const StSPtrVecFgtHit& hitVec = fgtHitColPtr->getHitVec();
	      StSPtrVecFgtHitConstIterator hitIter;
	      for( hitIter = hitVec.begin(); hitIter != hitVec.end(); ++hitIter )
		{
		  Int_t geoId2=(*hitIter)->getCentralStripGeoId();
		  if(geoId==geoId2)//found match
		    {
		      it->fgtHit=(*hitIter);
		    }
		}
	    }

	}
    }
}
Int_t StFgtGeneralBase::fillFromStEvent(StFgtCollection* fgtCollectionPtr)
{
  Int_t ierr = kStFatal;

  StFgtHitCollection *fgtHitColPtr = 0;
  if( fgtCollectionPtr ){
    // got this far, so flag that this is the right input.
    ierr = kStOk;
    // loop over discs
    for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
      fgtHitColPtr = fgtCollectionPtr->getHitCollection( disc );
      if( fgtHitColPtr ){
	const StSPtrVecFgtHit& hitVec = fgtHitColPtr->getHitVec();
	StSPtrVecFgtHitConstIterator hitIter;
	set<int> quadsHit;

	for( hitIter = hitVec.begin(); hitIter != hitVec.end(); ++hitIter)
	  {
	    Short_t quad, discK, strip;
	    Char_t layer;
 
	    Double_t posR=(*hitIter)->getPositionR();
	    Double_t posPhi=(*hitIter)->getPositionPhi();

	    Double_t discZ=getLocDiscZ(disc);
	    float tmpX, tmpY,tmpZ,tmpP,tmpR;
	    tmpR=posR;
	    tmpP=posPhi;
	    tmpZ=discZ;
#ifdef COSMIC
	    ///can't do it here since we don't know the space points yet...?.... shouldn't matter...?
	    //		   getAlign(disc,posPhi,posR,tmpX,tmpY,tmpZ,tmpP,tmpR);
#endif
	    posR=tmpR;
	    posPhi=tmpP;
	    discZ=tmpZ;

	    Int_t geoId=(*hitIter)->getCentralStripGeoId();
	    //	    cout <<"central geoId in cluster:" << geoId <<endl;
	
	    StFgtGeom::decodeGeoId((*hitIter)->getCentralStripGeoId(),discK, quad, layer, strip);
	    if(quad<0 && discK<0)//Ithink these are the error conditions
	      {
		if(discK!=disc)
		  cout <<" disc - geo id mismatch...disk: " <<disc <<" or " << discK <<" geo id is " << (*hitIter)->getCentralStripGeoId()<<endl;
		cout <<"bad read1" <<endl;
		continue;
	      }
#ifdef ONE_HIT_PER_QUAD
	    if(quadsHit.find(quad)!=quadsHit.end())
	      {
		continue;
	      }
	    quadsHit.insert(quad);
#endif
	    //				  Int_t clusterSizePhi=(*hitIter)->getStripWeightMap().size();
	    Int_t clusterSize=(*hitIter)->getStripWeightMap().size();
	    Double_t clusterCharge=(*hitIter)->charge();
	    Double_t clusterUncert=(*hitIter)->getChargeUncert();
	    //	    cout <<" r pos : " << posR << " phi: " << posPhi << " charge: " << clusterCharge << " unert: " << clusterUncert <<endl;
	    //sometimes (rarely two clusters have the same geo id (some split I guess), don't insert twice, that messes the indexing up
	    if(mapGeoId2Cluster.find(geoId)==mapGeoId2Cluster.end())
	      {
		pClusters[disc]->push_back(generalCluster(geoId,layer,discZ,posPhi,posR,quad,disc,strip, clusterSize, clusterCharge,clusterUncert));
		//		       cout <<"inserting geo id " << geoId <<" into map at pos: " << ((pClusters[disc]->size()-1)) <<" disc " << disc <<endl;
		mapGeoId2Cluster[geoId]=((pClusters[disc]->size()-1));
	      }
	    else
	      {
		//		       cout <<"geoId  " << geoId << " already exists " <<endl;
	      }
	  }
      };
    }
    ///////////////////////
    for( Int_t disc = 0; disc < kFgtNumDiscs; disc++ )
      {
	StFgtStripCollection *stripCollectionPtr = fgtCollectionPtr->getStripCollection( disc);
	if( stripCollectionPtr)
	  {
	    StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
	    StSPtrVecFgtStripIterator stripIter;
	    //		 cout <<"we have " << stripVec.size() << " strips in disc: " << disc <<endl;
	    for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
	      StFgtStrip *strip = *stripIter;
	      //		   Float_t ped = 0, pedErr = 0;
	      if( strip ){
		Int_t geoId=strip->getGeoId();
		//		cout <<"looking at strip geo id: "<< strip->getGeoId()<<endl;
		Int_t cSeedType=strip->getClusterSeedType();
		//		     if(cSeedType==kFgtClusterSeedInSeaOfNoise)
		//		       cout <<"noisy strip " << geoId <<endl;
		Double_t charge=strip->getCharge();
		Double_t chargeUncert=strip->getChargeUncert();
		Short_t quad, discK, stripI;
		Char_t layer;
		StFgtGeom::decodeGeoId(geoId,discK, quad, layer, stripI);
		//		     cout <<"quad: " << quad <<endl;
		if(discK<0 || discK>6 || quad <0 || quad > 4 || (layer!='P' && layer !='R'))
		  {
		    //		    cout <<"bad read2" <<endl;
		    continue;
		  }
		Double_t ped=0.0; //get from DB
		Double_t pedErr=0.0; 
		Int_t rdo, arm, apv, chan; 
		mDb->getElecCoordFromGeoId(geoId, rdo,arm,apv,chan);
		Int_t elecId = StFgtGeom::encodeElectronicId( rdo, arm, apv, chan );
		ped = mDb->getPedestalFromElecId( elecId );
		pedErr = mDb->getPedestalSigmaFromElecId( elecId );
		if(quad<4)
		  {

		    if(quad<0)
		      {
		      		      cout <<"bad read3"<<endl;
		      }
		    //		  cout <<"looking at disc: " << disc <<  " quad: " << quad <<" index: " << disc*4+quad <<endl;
		    //			 cout <<"pushing back to disc: "<< disc <<" quad: " << quad <<endl;
		    pStrips[disc*4+quad].push_back(generalStrip(geoId,ped,pedErr,cSeedType,charge, chargeUncert));
		    Double_t maxAdc=-9999;
		    for(int j=0;j<7;j++)
		      {
			pStrips[disc*4+quad].back().adc[j]=strip->getAdc(j);
			if(strip->getAdc(j)>maxAdc)
			  maxAdc=strip->getAdc(j);
		      }
		    pStrips[disc*4+quad].back().maxAdc=maxAdc;
		    if(mapGeoId2Cluster.find(geoId)!=mapGeoId2Cluster.end())
		      {
			//			     cout <<"this strip geo ID ("<<geoId<<") supposedly belongs to cluster with geo " << (*pClusters[disc])[mapGeoId2Cluster[geoId] ].centralStripGeoId <<" int index: " << mapGeoId2Cluster[geoId] <<endl;
			//			     cout <<" strip disc: " << disc <<" cluster disc: " << (*pClusters[disc])[mapGeoId2Cluster[geoId] ].disc <<  " index: " << (pStrips[disc*4+quad].size()-1) << " quad: " << quad <<endl;
			if(mapGeoId2Cluster[geoId]>=(*pClusters[disc]).size())
			  {
			    //				 cout <<" bad read5" <<endl;
			    //				 cout <<"geo id: " << geoId << " map: " << mapGeoId2Cluster[geoId] <<" size:" << (*pClusters[disc]).size() <<" disc: " << disc <<endl;
			  }
			(*pClusters[disc])[mapGeoId2Cluster[geoId] ].centerStripIdx=(pStrips[disc*4+quad].size()-1);
			(*pClusters[disc])[mapGeoId2Cluster[geoId] ].maxAdc=maxAdc;
		      }
		  }

	      }
	    }
	  }
      }

	 
    ///////////----



    for(int iDx=0;iDx<6;iDx++)
      {
	int dCounter=0;
	vector<generalCluster>::iterator it=pClusters[iDx]->begin();
	for(;it!=pClusters[iDx]->end();it++)
	  {
	    dCounter++;
	    //		 it->hasMatch=true;
	    Int_t centerStripId=it->centerStripIdx;
	    if(centerStripId<0)
	      {
		//don't worry, probably a cluster at low r where the center strip is missing
		//				     cout <<"bad read4 for cluster with geoid: "<< it->centralStripGeoId <<" index: " << it->centerStripIdx <<" quad: " << it->quad << "index: " <<dCounter-1<< " disc: " << iDx  <<" phi " << it->posPhi <<" r: " << it->posR <<endl;
		continue;
	      }

	    Double_t maxChargeInt=it->maxAdc;
	    //		 cout <<"center strip" << centerStripId<<" idx: " << iDx << " quad: " << it->quad <<endl;
	    //		 cout <<" size:" <<pStrips[iDx*4+it->quad].size()<<endl;
	    if(centerStripId>=pStrips[iDx*4+it->quad].size())
	      {
		cout <<"bad read5" <<endl;
		continue;
	      }
	    Int_t oldGeoId=(pStrips[iDx*4+it->quad])[centerStripId].geoId;
	    Int_t m_seedType=(pStrips[iDx*4+it->quad])[centerStripId].seedType;
	    Int_t stripCounter=(centerStripId-1);
	    while(stripCounter>=0)     
	      {
		//		     cout <<"stripcounter"<< stripCounter <<endl;
		Int_t seedType=(pStrips[iDx*4+it->quad])[stripCounter].seedType;
		if((seedType==kFgtSeedType1)||(seedType==kFgtSeedType2)||(seedType==kFgtSeedType3))
		  {
		    m_seedType=seedType;
		  }
		if(!((seedType==kFgtClusterPart)||(seedType==kFgtClusterEndUp)||(seedType==kFgtClusterEndDown)||(seedType==kFgtSeedType1)||(seedType==kFgtSeedType2)||(seedType==kFgtSeedType3)))
		  break;
		if(fabs(oldGeoId-(pStrips[iDx*4+it->quad])[stripCounter].geoId)>1)
		  break;
		maxChargeInt+=(pStrips[iDx*4+it->quad])[stripCounter].maxAdc;
		oldGeoId=(pStrips[iDx*4+it->quad])[stripCounter].geoId;
		stripCounter--;
	      }
	    //and go up
	    stripCounter=(centerStripId+1);
	    //should be strictly smaller
	    while(stripCounter<(pStrips[iDx*4+it->quad]).size())
	      {
		Int_t seedType=(pStrips[iDx*4+it->quad])[stripCounter].seedType;
		if(!((seedType==kFgtClusterPart)||(seedType==kFgtClusterEndUp)||(seedType==kFgtClusterEndDown)||(seedType==kFgtSeedType1)||(seedType==kFgtSeedType2)||(seedType==kFgtSeedType3)))
		  break;
		if(fabs(oldGeoId-(pStrips[iDx*4+it->quad])[stripCounter].geoId)>1)
		  break;
		maxChargeInt+=(pStrips[iDx*4+it->quad])[stripCounter].maxAdc;
		oldGeoId=(pStrips[iDx*4+it->quad])[stripCounter].geoId;
		stripCounter++;
		if((seedType==kFgtSeedType1)||(seedType==kFgtSeedType2)||(seedType==kFgtSeedType3))
		  {
		    m_seedType=seedType;
		  }
	      }
	    it->maxAdcInt=maxChargeInt;
	    it->seedType=m_seedType;
	    chargeMaxAdcCorr->Fill(it->clusterCharge,it->maxAdc);
	    chargeMaxAdcIntCorr->Fill(it->clusterCharge,maxChargeInt);
	  }
      }
    //////////////////////////////
      
   

    //   cout <<"filled from event " <<endl;
    checkMatches();
    checkNumPulses();
    return ierr;

  }
}

Int_t StFgtGeneralBase::fillFromMuDst(StFgtCollection& fgtCollection)
{
  Int_t ierr = kStOk;
  StFgtStripCollection* stripCollectionPtr[kFgtNumDiscs];
  StFgtHitCollection* hitCollectionPtr[kFgtNumDiscs];
  for(int discIdx=0;discIdx<kFgtNumDiscs;discIdx++)
    {
      stripCollectionPtr[discIdx] = fgtCollection.getStripCollection( discIdx );
      hitCollectionPtr[discIdx] = fgtCollection.getHitCollection( discIdx );
    }
  // get pointer to input
  const StMuDst* muDst = (const StMuDst*)GetInputDS("MuDst");
  //temp trigger ids for ehts like stuff



  if( muDst )
    {
      StMuEvent *event = static_cast<StMuEvent*>(muDst->event());
      if( event ){
	StMuTriggerIdCollection trig=event->triggerIdCollection();
	StTriggerId l1trig=trig.nominal();
	Bool_t haveEHTTrig=false;
	for(int iTrig=0;iTrig<3;iTrig++)
	  {

	    if(l1trig.isTrigger(trigID[iTrig]))
	      {
		cout <<"found " <<endl;
		haveEHTTrig=true;
	      }
	  }
	const StThreeVectorF& v = event->primaryVertexPosition();
	StEventInfo &info=event->eventInfo();
	int nPrimV=muDst->numberOfPrimaryVertices();
	//	cout <<"nPrimV: " << nPrimV <<" vertex num: " << mVertexNumber << " useEHT: " << mUseEHTTrigs << " have EHT? " << haveEHTTrig <<endl;

	if(1 && (nPrimV>mVertexNumber) && (!mUseEHTTrigs || haveEHTTrig )) { // copy vertex info
	  StMuPrimaryVertex* V= muDst->primaryVertex(mVertexNumber);// select highest rank vertex
	  assert(V);
	  const StThreeVectorF &r=V->position();
	  vtxZ=r.z();
	  //	  cout <<"vtxZ: " << vtxZ <<endl;
	  const StThreeVectorF &er=V->posError();
	  float rank=V->ranking();

	  //printf("primVert  Z=%.2f ±%.2f   rank=%5.1e  (nVert=%d) \n",r.z(),er.z() ,rank, nPrimV);
	  //	  myJFgtEve->vertRank=rank;
	  vtxRank=rank;
	  vtxZ=r.z();
	  if(rank>0) { // use the vertex
	    //   myJFgtEve->vertPos=TVector3(r.x(),r.y(),r.z());
	    //	    myJFgtEve->vertErr=TVector3(er.x(),er.y(),er.z());
	    hIpZEv->Fill(vtxZ);
	  }
	  else
	    {
#ifdef USE_VTX
	      return false; //primary vertex is not good...
#endif
	    }
	 
	}
	else{
	  	vtxRank=-999;
	}
	//	if(fabs(ipZ)>20)
	// return false;
	//	cout <<" found z vertex: " << ipZ <<endl;
      }
    }
  else
    {
      cout <<"no muDST!!!" <<endl;
    }

  if( muDst ){
    TClonesArray *fgtClusters = muDst->fgtArray( muFgtClusters );
    TClonesArray *fgtStrips=muDst->fgtArray(muFgtStrips);
    TClonesArray *fgtStripAssoc=muDst->fgtArray(muFgtStripAssociations );
    TClonesArray *fgtAdc=muDst->fgtArray(muFgtAdcs);
    //    cout <<"got mdsts" <<endl;
    if(fgtStripAssoc && fgtClusters && fgtStrips && fgtAdc)
      {

	ierr = kStOk;
	Int_t nAssoc=fgtStripAssoc->GetEntriesFast();
	Int_t nClus=fgtClusters->GetEntriesFast();
	Int_t nStrips=fgtStrips->GetEntriesFast();
	Int_t nAdc=fgtAdc->GetEntriesFast();
	//	 cout <<"there are " << nAssoc <<" associations" <<endl;
	Int_t oldClusIdx=-1;
	Int_t rdo, arm, apv, chan; 
	Int_t geoId;
	Int_t clusKey=0;
	Short_t quad, disc, stripNum;
	Char_t layer;
	StFgtHit* pFgtHit=0;
	for( Int_t i = 0; i < nAssoc; ++i )
	  {
	    StMuFgtStripAssociation* assoc = static_cast< StMuFgtStripAssociation* >( (*fgtStripAssoc)[i] );
	    if( assoc )
	      {
		//only construct clusters for which we have the strips...
		Int_t clusIdx=assoc->getClusIdx();
		if(oldClusIdx!=clusIdx) //looking at new clusters
		  {
		    //construct new cluster
		    oldClusIdx=clusIdx;
		    if(pFgtHit>0)
		      hitCollectionPtr[disc]->getHitVec().push_back(pFgtHit);
		    StMuFgtCluster* clus = static_cast< StMuFgtCluster* >( (*fgtClusters)[clusIdx] );
		    Int_t clusCentGeoId=clus->getCentralStripGeoId();
		    //		    cout <<" looking at : " << clusCentGeoId <<endl;
		    Float_t clusCharge=clus->getCharge();
		    Float_t R=clus->getR();
		    Float_t Phi=clus->getPhi();
		    Float_t Z=getLocDiscZ(disc);
		    Float_t ErrR=clus->getErrR();
		    Float_t ErrPhi=clus->getErrPhi();
		    Float_t ErrZ=0.0;
		    StFgtGeom::decodeGeoId(clusCentGeoId,disc, quad, layer, stripNum);
		    pFgtHit=new StFgtHit(clusKey,clusCentGeoId,clusCharge, disc, quad, layer, R, ErrR,Phi, ErrPhi,Z,ErrZ);
		    clusKey++;
		    pFgtHit->setChargeUncert(clus->getChargeUncert());

		    pFgtHit->setEvenOddChargeAsy(clus->getEvenOddChargeAsy());
		    pFgtHit->setMaxTimeBin(clus->getMaxTimeBin());
		    pFgtHit->setMaxAdc(clus->getMaxAdc());
		    pFgtHit->setNstrip(clus->getNumStrips());//should check if that corresponds to the number of strips...
		    //not in mdst??
		    ///		    pFgtHit->setSeedType(clus->getSeedType());
		  }
		else
		  {

		  }
		Int_t stripIdx=assoc->getStripIdx();
		StMuFgtStrip* strip = static_cast< StMuFgtStrip* >( (*fgtStrips)[stripIdx] );
		geoId=strip->getGeoId();
		Float_t stripCharge=strip->getCharge();
		mDb->getElecCoordFromGeoId(geoId, rdo,arm,apv,chan);
		StFgtGeom::decodeGeoId(geoId,disc, quad, layer, stripNum);
		Int_t elecId = StFgtGeom::encodeElectronicId( rdo, arm, apv, chan );
		//strips are dynamically created by the strip collection
		StFgtStrip* stripPtr = stripCollectionPtr[disc]->getStrip( elecId );
		stripPtr->setGeoId(geoId);
		stripPtr->setCharge(stripCharge);
		stripPtr->setElecCoords(rdo,arm,apv,chan);
		Double_t ped = mDb->getPedestalFromElecId( elecId );
		Double_t pedErr = mDb->getPedestalSigmaFromElecId( elecId );
		stripPtr->setPed(ped);
		stripPtr->setPedErr(pedErr);
		stripPtr->setChargeUncert(strip->getChargeUncert());
		Int_t seedType=strip->getClusterSeedType();
		stripPtr->setClusterSeedType(seedType);
		if(seedType>=kFgtSeedType1&& seedType<=kFgtSeedType5 )
		  {
		    pFgtHit->setSeedType(seedType);
		  }
		//fill in strip info
		//fill in adc info
		for(Int_t iAdc=strip->getAdcStartIdx();iAdc<strip->getAdcStartIdx()+strip->getNumSavedTimeBins();iAdc++)
		  {
		    if(iAdc>=0 && (iAdc<nAdc))
		      {
			StMuFgtAdc* adc=0;
			adc=static_cast< StMuFgtAdc* >((*fgtAdc)[iAdc]);
			stripPtr->setAdc(adc->getAdc(),adc->getTimeBin());
		      }
		  }
		//even though the "strips" container can be invalidaded by added to it, the *it is a pointer to a StFgtStrip, so should stay
		(pFgtHit->getStripWeightMap())[stripPtr]=1;
	      }
	    else
	      {
		cout <<"no assoc..." << assoc <<endl;
	      }
	  }
	//put last cluster in...
	if(pFgtHit>0)
	  hitCollectionPtr[disc]->getHitVec().push_back(pFgtHit);
      }
      else
	{
	  //doesn't make sense to continue w/o clusters/strips
	  return kStErr;
	}
  }
								    
return ierr;
};

void StFgtGeneralBase::checkNumPulses()
{

  Char_t layer;
  Short_t quad, disc, strip;
  Double_t ordinate, lowerSpan, upperSpan;
  Int_t iValCluR[4];
  Int_t iValCluP[4];
  for(int iq=0;iq<4;iq++)
    {
      iValCluR[iq]=0;
      iValCluP[iq]=0;
    }
  for(int iDx=0;iDx<6;iDx++)
    {
      for(vector<generalCluster>::iterator it=(*pClusters[iDx]).begin();it!=(*pClusters[iDx]).end();it++)
	{
	  StFgtGeom::getPhysicalCoordinate(it->centralStripGeoId,disc,quad,layer,ordinate,lowerSpan,upperSpan);
	  if(layer=='R')
	    iValCluR[quad]++;
	  else
	    iValCluP[quad]++;
	}
      for(int iQ=0;iQ<4;iQ++)
	{
	  hNumClustersR[iDx*4+iQ]->Fill(iValCluR[iQ]);
	  hNumClustersP[iDx*4+iQ]->Fill(iValCluP[iQ]);
	}
    }
  for(int iDx=0;iDx<6;iDx++)
    {
      for(int iQ=0;iQ<4;iQ++)
	{

	  int iValPulseP=0;
	  int iValChargeP=0;
	  int iValPulseR=0;
	  int iValChargeR=0;
	  Char_t layer;
	  Short_t quad, disc, strip;
	  Double_t ordinate, lowerSpan, upperSpan;
	  for(vector<generalStrip>::iterator it=pStrips[iDx*4+iQ].begin();it!=pStrips[iDx*4+iQ].end();it++)
	    {
	      StFgtGeom::getPhysicalCoordinate(it->geoId,disc,quad,layer,ordinate,lowerSpan,upperSpan);
	      if(validPulse((*it)))
		{
		  if(layer=='P')
		    iValPulseP++;
		  else
		    iValPulseR++;
		}
	      ///	      if(it->charge>3*it->chargeUncert)
	      ///lens condition:
	      if(it->charge>1000)
		{
		  if(layer=='P')
		    iValChargeP++;
		  else
		    iValChargeR++;
		}
	    }
	  hNumPulsesR[iDx*4+iQ]->Fill(iValPulseR);
	  hNumPulsesP[iDx*4+iQ]->Fill(iValPulseP);
	  hNumChargesR[iDx*4+iQ]->Fill(iValChargeR);
	  hNumChargesP[iDx*4+iQ]->Fill(iValChargeP);
	  //	  cout <<"disk: " << iDx+1<< " quad: " << iQ << " has " <<iValPulse <<" valid pulses and " << iValCharge <<" valid charges " <<endl;
	}
    }
}

Bool_t StFgtGeneralBase::arePointsMatched(vector<generalCluster>::iterator  c1, vector<generalCluster>::iterator  c2)
{
  Float_t tUncert1;
  Float_t tCharge1;

  Float_t tUncert2;
  Float_t tCharge2;

  if(c1->clusterCharge<20 || c2->clusterCharge<20)
    return false;
  if(c1->clusterCharge<c2->clusterCharge)
    {
      tCharge1=c2->clusterCharge;
      tUncert1=c2->clusterChargeUncert;
      tCharge2=c1->clusterCharge;
      tUncert2=c1->clusterChargeUncert;
    }
  else
    {
      tCharge1=c1->clusterCharge;
      tUncert1=c1->clusterChargeUncert;
      tCharge2=c2->clusterCharge;
      tUncert2=c2->clusterChargeUncert;
    }


  if(((tCharge1/tCharge2) <chargeMatchCut) ||((tCharge1-tUncert1)/(tCharge2+tUncert2))<chargeMatchCut)
    return true;
  else 
    return false;
}

void StFgtGeneralBase::checkMatches()
{
  //  cout <<"checkmatches.." <<endl;
  for(int iDx=0;iDx<6;iDx++)
    {
      Int_t numMatched=0;
      Int_t numNotMatched=0;
      if((*pClusters[iDx]).size()<2)
	continue;
      for(vector<generalCluster>::iterator it=(*pClusters[iDx]).begin();it!=((*pClusters[iDx]).end()-1);it++)
	{

	  //	  if(it->hasMatch)
	  //	  continue;
	  Char_t layer=it->layer;
	  for(vector<generalCluster>::iterator it2=it+1;it2!=(*pClusters[iDx]).end();it2++)
	    {
	      if(it2->layer==layer)
		continue;
	      if(it->clusterCharge<100 || it2->clusterCharge<100)
		continue;
	      //	      if(it2->hasMatch)//can have two matches?
	      //		continue;

	      if(arePointsMatched(it,it2))
		{
		  it->hasMatch=true;
		  it2->hasMatch=true;
		}

	    }

	}
      for(vector<generalCluster>::iterator it=(*pClusters[iDx]).begin();it!=(*pClusters[iDx]).end();it++)
	{
	  if(it->hasMatch)
	    numMatched++;
	  else
	    numNotMatched++;
	}
      clusWChargeMatch->Fill(numMatched);
      clusWOChargeMatch->Fill(numNotMatched);
      //      cout <<"disk: " << iDx<<" found " << numMatched <<" clusters and " << numNotMatched << " clusters that were not matched" <<endl;
    }

}

void StFgtGeneralBase::fillFromEvent(Bool_t fillFromEv) //default, no, use mDsts
{
  m_fillFromEvent=fillFromEv;
  //  cout <<"m_fillFromEvent: " << m_fillFromEvent <<endl;
}

void StFgtGeneralBase::doLooseClustering()
{
  for(int iDx=0;iDx<6;iDx++)
    {
      if(iDx==3) //disk 3 is noisy at high HV
	continue;
      Int_t stripCounter=0;
      for(int iQ=0;iQ<4;iQ++)
	{
	  cout <<" loose clustering in disk: " << iDx << " quad: " << iQ <<" we have: " << (*pClusters[iDx]).size() <<" clusters " <<endl;
	  for(vector<generalStrip>::iterator it=pStrips[iDx*4+iQ].begin();it!=pStrips[iDx*4+iQ].end();it++)
	    {
	      //nodead strips
	      if(it->charge > 5*it->chargeUncert && it->seedType==kFgtSeedTypeNo)
		{
		  //implement seedtype3
		  Double_t pedErr=it->pedErr;
		  //   if(pStrip->getAdc(0) <3*ped && numHighBins==1 && peakAdc > pStrip->getAdc(6)&& numHighBinsAfterLeadingEdge>=1&& numAlmostHighBins>=2)
		  Int_t numHighBins=0;
		  Int_t numAlmostHighBins=0;
		  Int_t peakAdc=-9999;
		  for(int i=0;i<7;i++)
		    {
		      if(i>=2 && i<=5)
			{
			  if(it->adc[i]>3*pedErr)
			    numAlmostHighBins++;
			}
		      if(it->adc[i]>peakAdc)
			peakAdc=it->adc[i];
		      if(it->adc[i]> 5*pedErr)
			numHighBins++;
		    }

		  //		  if(!(it->adc[0]<3*pedErr && numHighBins>=1 && peakAdc> it->adc[6] && numAlmostHighBins>=2) )
		  //		    continue;
		  //not cluster midpoint
		  Char_t layerPrv, layerNext, layer;
		  Double_t posR, posPhi;
		  Int_t geoId=it->geoId;
		  Short_t quad, disc, strip;
		  Double_t ordinate, lowerSpan, upperSpan;
		  Double_t discZ=getLocDiscZ(disc);
		  Int_t clusterSize=3;
		  StFgtGeom::getPhysicalCoordinate(it->geoId,disc,quad,layer,ordinate,lowerSpan,upperSpan);
		  if((it+1)<pStrips[iDx*4+iQ].end())
		    StFgtGeom::getPhysicalCoordinate((it+1)->geoId,disc,quad,layerNext,ordinate,lowerSpan,upperSpan);
		  if((it-1)>=pStrips[iDx*4+iQ].begin())
		    StFgtGeom::getPhysicalCoordinate(it->geoId,disc,quad,layerPrv,ordinate,lowerSpan,upperSpan);

		  if((it+1)<pStrips[iDx*4+iQ].end() && layerNext==layer && (it+1)->charge > it->charge)
		    continue;
		   
		  Double_t clusterCharge=it->charge;
		  if((it+1)<pStrips[iDx*4+iQ].end() && (layer==layerNext))
		    clusterCharge+=(it+1)->charge;
		  if((it-1)>=pStrips[iDx*4+iQ].begin() && (layer==layerPrv))
		    clusterCharge+=(it-1)->charge;

		  StFgtGeom::decodeGeoId(geoId,disc, quad, layer, strip);
		  StFgtGeom::getPhysicalCoordinate(it->geoId,disc,quad,layer,ordinate,lowerSpan,upperSpan);
		  if( layer == 'R' ){
		    posR = ordinate;
		    posPhi = 0.5*(upperSpan + lowerSpan);  
		  } else {
		    posPhi=ordinate;
		    posR = 0.5*(upperSpan + lowerSpan);   // mid point of the strip
		  };		   
		  posPhi+=StFgtGeom::phiQuadXaxis(quad);

		  pClusters[iDx]->push_back(generalCluster(geoId,layer,discZ,posPhi,posR,quad,disc,strip, clusterSize, clusterCharge,0.0));		  
		  mapGeoId2Cluster[geoId]=(pClusters[iDx]->size()-1);
		  (*pClusters[disc])[mapGeoId2Cluster[geoId] ].centerStripIdx=stripCounter;
		  (*pClusters[disc])[mapGeoId2Cluster[geoId] ].maxAdc=it->maxAdc;
		  //don't care for now
		  (*pClusters[disc])[mapGeoId2Cluster[geoId] ].maxAdcInt=it->maxAdc;
		}
	      stripCounter++;
	    }
	}
    }

}
 const Int_t StFgtGeneralBase::trigID[3]={430315,430313,430312};

Float_t StFgtGeneralBase::chargeMatchCut=1.5;
ClassImp(StFgtGeneralBase);
