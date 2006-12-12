// *-- Author : Hal Spinka
// 
// $Id: StEEmcSlowMaker.cxx,v 1.5 2006/12/12 20:29:14 balewski Exp $

#include <TFile.h>
#include <TH2.h>
#include <TRandom.h>
#include <StMessMgr.h>

#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"

#include "StEEmcFastMaker.h"
#include "StEEmcSlowMaker.h"

ClassImp(StEEmcSlowMaker)

//________________________________________________
//________________________________________________
StEEmcSlowMaker::StEEmcSlowMaker( const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  eeDb=0;
  mHList=0;
  nInpEve=0; 
  memset(hA,0,sizeof(hA));

  /// By default, we do not add a pedestal offset
  mAddPed   = false;
  /// By default, we do not smear the pedestal
  mSmearPed = false;
  /// By default, all channels are kept
  mDropBad  = false;
  /// By default, overwrite ADC in muDst
  mOverwrite = true;

  mIsEmbeddingMode=false;
  /// By default, source is MuDst
  mSource = kMuDst;
}


//___________________ _____________________________
//________________________________________________
StEEmcSlowMaker::~StEEmcSlowMaker(){
}

 
//________________________________________________
//________________________________________________
Int_t StEEmcSlowMaker::Init(){
 LOG_INFO<<Form("%s::Init(), mIsEmbeddingMode=%d",GetName(),mIsEmbeddingMode)<<endm;

  if(mIsEmbeddingMode) {
    setDropBad(0);   // 0=no action, 1=drop chn marked bad in db
    setAddPed(0);    // 0=no action, 1=ped offset from db
    setSmearPed(0);  // 0=no action, 1=gaussian ped, width from db
    setOverwrite(1); // 
    setSource("StEvent");
  }


  eeDb=(StEEmcDbMaker*)GetMaker("eemcDb");
  if(eeDb==0) eeDb=(StEEmcDbMaker*)GetMaker("eeDb"); // try another name
  assert( eeDb);
  if ( mHList ) InitHisto();
  if ( mSmearPed && !mAddPed) {
       gMessMgr->Message("","E")<<GetName()<<"::Init() detected mSmearPed=true && mAddPed=false \n will not work due to ETOW hits storage container in muDst not accepting negative ADC values, ABORT" << endm; 
       exit(1);
  }
  if ( mSmearPed) gMessMgr->Message("","I")<<GetName()<<"::Init() detected mSmearPed,  (be sure peds>>N*sig are loaded in DB!), muDst can't store negative ADC for towers, the NUadc will be forced to be non-negative" << endm; 
  
  return StMaker::Init();
}

//________________________________________________
//________________________________________________
void StEEmcSlowMaker::InitHisto(){

  char tt1[100], tt2[500];
  int sectID=5;

  sprintf(tt1,"mm");
  sprintf(tt2,"freq vs. sector ID; sector ID");
  hA[12]=new TH1F(tt1,tt2,20,-0.5,19.5);
  
  sprintf(tt1,"PreADC");
  sprintf(tt2,"Revised ADC for Pre/Post Scint.");
  hA[0]=new TH1F(tt1,tt2,100,0,200.);
  
  sprintf(tt1,"Pre2ADC");
  sprintf(tt2,"Initial ADC for Pre/Post Scint.");
  hA[1]=new TH1F(tt1,tt2,100,0,200.);
  
  sprintf(tt1,"SMDADC");
  sprintf(tt2,"Revised ADC for SMD Strips");
  hA[2]=new TH1F(tt1,tt2,100,0,200.);

  sprintf(tt1,"SMD2ADC");
  sprintf(tt2,"Initial ADC for SMD Strips");
  hA[3]=new TH1F(tt1,tt2,100,0,200.);

  hA[4]=new TH1F("hADCtow","Initial ADC for towers",512+32,-32.,512.);
  hA[5]=new TH1F("hADCtow2","Finial ADC fot towers",512+32,-32.,512.);

  //..................
  sprintf(tt1,"adc%02d",sectID);
  sprintf(tt2," Adc vs, strip ID, sector=%02d; strip ID; ADC ",sectID);
  hA[20]=new TH2F(tt1,tt2,30,0,300,100,0,200.);

  sprintf(tt1,"ADCvsstr");
  sprintf(tt2," Adc vs, strip ID; strip ID; ADC ");
  hA[19]=new TH2F(tt1,tt2,60,0,300,100,0,200.);

  // add histos to the list (if provided)
  if (mHList) {
    for (int i = 0; i < mxH; ++i) {
      if (hA[i] == 0) continue;
      mHList->Add(hA[i]);
    }
  }
}

//________________________________________________
//________________________________________________
Int_t StEEmcSlowMaker::InitRun(int runNo){
  if(runNo==0) {
    gMessMgr->Message("","W")<<GetName()<<"::InitRun("<<runNo<<") ??? changed to 555, it s OK for M-C - perhaps, JB"<<endm;
    runNo=555;
  }
  return kStOK;
}



//________________________________________________
//________________________________________________
Int_t StEEmcSlowMaker::Finish(){
  return kStOK;
}


//________________________________________________
//________________________________________________
Int_t StEEmcSlowMaker::Make(){
  nInpEve++;

  LOG_INFO << GetName() << "::Make() is called , iEve " << nInpEve << " mSource="<<mSource<<endm;
  
  mKSigma = eeDb -> KsigOverPed;

  switch (mSource) {
  case kMuDst:
    /// Access to muDst .......................
    {
      if (!GetInputDS("MuDst")) {
	gMessMgr->Debug("No MuDst");
	return kStOk;
      }

      StMuEmcCollection* emc = StMuDst::muEmcCollection();
      if (!emc) {
	gMessMgr->Debug("No StMuEmcCollection");
	return kStOk;
      }

      /// Run slow simulator on towers
      MakeTower(emc);

      /// Run slow simulator on pre/postshower
      MakePrePost(emc);

      /// Run slow simulator on smd
      MakeSMD(emc);
    }
    break;

  case kStEvent:
    /// Acces to StEvent, automatic detection if in Embedding mode .....................
    {

      StEmcCollection *emc =0; 
      if(mIsEmbeddingMode) {
	StEEmcFastMaker *fast = (StEEmcFastMaker*)GetMaker("EEmcFastSim");
	if(fast==0) {
	  LOG_WARN << GetName() << "::Make()  no EEmcFastSim in the chain, ignore Endcap"<< endm;
	  return kStOk;
	}
	emc = fast->GetLocalEmcCollection();
      } else { // it is not Embedding mode
	
	StEvent* event = (StEvent*)GetInputDS("StEvent");
	if (!event) {
	  LOG_WARN << GetName() << "::Make()  no StEvent"<< endm;
	  return kStOk;
	}
	emc = event->emcCollection();
      }
      // now emc-collection should be accessible one way or another
      
      if (!emc) {
	LOG_WARN << GetName() << "::Make()  no emcCollection()" << endm;
	return kStOk;
      }

      /// Run slow simulator on towers
      MakeTower(emc);

      /// Run slow simulator on pre/postshower
      MakePrePost(emc);

      /// Run slow simulator on smd
      MakeSMD(emc);
    }
    break;

  default:
    LOG_ERROR<< "Unknown source type " << mSource << " for this event" << endm;
    break;
  }

  return  kStOK;

}
// ----------------------------------------------------------------------------
void StEEmcSlowMaker::MakeTower( StMuEmcCollection *emc )
{

  if ( !mAddPed && !mSmearPed ) return;
  /// only pedestal simu is implemented at this moment

  /// loop over all eemc towers
  for (Int_t i=0; i < emc->getNEndcapTowerADC(); i++) 
    {

      /// Get the ADC value stored for this tower
      int sec,eta,sub,adc; //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
      emc->getEndcapTowerADC(i,adc,sec,sub,eta);
      if ( mHList ) hA[4]->Fill( adc );

      /// Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
      assert(eeDb); 
      const EEmcDbItem *x=eeDb-> getTile(sec,sub-1+'A', eta,  'T' );
      if ( x==0 ) continue;

      Float_t fadc = (Float_t)adc;
      if ( mAddPed ) fadc += x -> ped;
      if ( mSmearPed ) fadc += gRandom -> Gaus( 0, x->sigPed );
      adc=(Int_t)fadc;
      if(adc<0) adc=0;
      if(adc>4095) adc=4095;

      /// may as well put the caps lock on and program in fortran
      /// if we're counting from 1.  shhesh.  
      Int_t FTNNDX = i+1;
      emc->setTowerADC(FTNNDX,adc,eemc); 
      if ( mHList ) hA[5]->Fill(adc);

     }

}

// ----------------------------------------------------------------------------
void StEEmcSlowMaker::MakePrePost( StMuEmcCollection *emc ){

  for ( Int_t i = 0; i < emc->getNEndcapPrsHits(); i++ ) {

    Int_t pre,sec,eta,sub;

    /// muDst ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post
    StMuEmcHit *hit=emc->getEndcapPrsHit(i,sec,sub,eta,pre);
    
    /// tmp, for fasted analysis use only hits from sectors init in DB
    if (sec<eeDb->mfirstSecID || sec>eeDb->mlastSecID) continue;
     
    /// Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x=eeDb-> getTile(sec,sub-1+'A', eta, pre-1+'P'); 
    if(x==0) continue;
  
    /// Drop broken channels
    if(mDropBad) if(x->fail ) continue; 
      
    Float_t adc   = hit -> getAdc();
    Float_t energy= adc / x->gain; // (GeV) Geant energy deposit 
    // printf("inp: %s adc=%.1f ene1=%g \n",x->name,adc,energy,hit -> getAdc());
    Float_t newadc    = adc;
    if(adc>0) {
      if ( mHList )  hA[1]->Fill(adc);
      
      /// Addition of Poisson fluctuations and Gaussian 
      /// 1 p.e. resolution
      Float_t mipval = energy / Pmip2ene;
      Float_t avgpe  = mipval * Pmip2pe;
      Float_t Npe    = gRandom->Poisson(avgpe);
      
      if (Npe>0) {
        /// Determine number of photoelectrons
        Float_t sigmape   = sqrt(Npe) * sig1pe;
        Float_t smearedpe = gRandom -> Gaus(Npe,sigmape);
        /// Determine new ADC value
        newadc= smearedpe * mip2ene*(x->gain)/Pmip2pe;
      }
    }// non-zero ADC on input

    /// Lookup pedestal in database (possibly zero)
    Float_t ped    = (mAddPed)?x -> ped:0;
    /// Smear the pedestal
    if ( mSmearPed ) ped = gRandom -> Gaus(ped,x->sigPed);
    
    /// add signal & pedestal back
    Int_t   NUadc     = (Int_t)(newadc + 0.5 + ped );
    
    if (adc>0 &&  mHList )   hA[0]->Fill(NUadc); //??

    // printf("out: NUadc=%d \n",NUadc);
    ///
    /// If we've made it here, overwrite the muDst
    ///
    if ( mOverwrite ) hit -> setAdc( NUadc );
    
  }

}

// ----------------------------------------------------------------------------
void StEEmcSlowMaker::MakeSMD( StMuEmcCollection *emc ) {
  
  Char_t uv='U';
  for ( uv='U'; uv <= 'V'; uv++ ) {
    
    Int_t sec,strip;
    for (Int_t i = 0; i < emc->getNEndcapSmdHits(uv); i++ ) {
      
      StMuEmcHit *hit=emc->getEndcapSmdHit(uv,i,sec,strip);
      assert(sec>0 && sec<=MaxSectors);// total corruption of muDst
      
      // tmp, for fasted analysis use only hits from sectors init in DB
      if(sec<eeDb->mfirstSecID || sec>eeDb->mlastSecID) continue;
      
      const EEmcDbItem *x=eeDb->getByStrip(sec,uv,strip);
      assert(x); // it should never happened for muDst

      /// Drop broken channels
      if(mDropBad)  if(x->fail ) continue;
      
      Float_t adc    = hit->getAdc();
      Float_t energy = adc/ x->gain; // (GeV) Geant energy deposit 
      Float_t newadc    = adc;
      
      if(adc>0) {

        if ( mHList ) { 
          hA[12]->Fill(x->sec);
          if(x->sec==5) hA[20]->Fill(x->strip,adc); 
          hA[3]->Fill(adc);
        }

        // Addition of Poisson fluctuations and 
        // Gaussian 1 p.e. resolution
        Float_t mipval = energy / mip2ene;
        Float_t avgpe  = mipval * mip2pe[strip];
        Float_t Npe    = gRandom->Poisson(avgpe);

        if (Npe>0) {
          
          /// Determine number of photoelectrons
          Float_t sigmape   = sqrt(Npe) * sig1pe;
          Float_t smearedpe = gRandom -> Gaus(Npe,sigmape);
          
          /// Determine new ADC value
          newadc    = smearedpe * mip2ene * (x->gain) / mip2pe[strip];
        }
      }
      
      /// Lookup pedestal in database (possibly zero)
      Float_t ped    = (mAddPed)?x -> ped:0;
      /// Smear the pedestal
      if ( mSmearPed ) ped = gRandom -> Gaus(ped,x->sigPed);
      
      /// add signal & pedestal back
      Int_t   NUadc     = (Int_t)(newadc + 0.5 + ped );
      
      if (adc>0 && mHList ) {
        hA[19]->Fill(x->strip,NUadc);
        hA[2]->Fill(NUadc);
      }
      
      ///
      /// If we've made it here, overwrite the muDst
      ///
      if ( mOverwrite ) hit -> setAdc( NUadc );
      
    }// loop over 1 plane
  } // loop over U,V
 
}

void StEEmcSlowMaker::setSource(const Char_t* name)
{
  if (strcmp(name, "MuDst") == 0)
    mSource = kMuDst;
  else if (strcmp(name, "StEvent") == 0)
    mSource = kStEvent;
  else {
    gMessMgr->Warning("Source must be \"MuDst\" or \"StEvent\"");
  }
}

void StEEmcSlowMaker::MakeTower(StEmcCollection* emc)
{
  StEmcDetector* det = emc->detector(kEndcapEmcTowerId);
  if (!det) {
    gMessMgr->Debug("No kEndcapEmcTowerId");
    return;
  }

  // StEvent ranges: sector=1-12, sub=1-5, eta=1-12
  for(UInt_t sector = 1; sector <= det->numberOfModules(); ++sector) {
    StSPtrVecEmcRawHit& hits = det->module(sector)->hits();
    for (UInt_t i = 0; i < hits.size(); ++i) {
      StEmcRawHit* hit = hits[i];
      if (mHList) hA[4]->Fill(hit->adc());
      // Database ranges: sector=1-12, sub=A-E, eta=1-12, type=T,P-R; Slow method
      const EEmcDbItem* x = eeDb->getTile(sector, 'A'+hit->sub()-1, hit->eta(), 'T');
      if (!x) continue;
      Float_t adc = hit->adc();
      if (mAddPed) adc += x->ped;
      if (mSmearPed) adc += gRandom->Gaus(0, x->sigPed);
      if (adc < 0) adc = 0;
      if (adc > 4095) adc = 4095;
      hit->setAdc(UInt_t(adc));
      if (mHList) hA[5]->Fill(adc);
    }
  }
}

void StEEmcSlowMaker::MakePrePost(StEmcCollection* emc)
{
  StEmcDetector* det = emc->detector(kEndcapEmcPreShowerId);
  if (!det) {
    LOG_WARN<<Form("No kEndcapEmcPreShowerId");
    return;
  }

  for (UInt_t sector =1; sector <= det->numberOfModules(); ++sector) {
    StSPtrVecEmcRawHit& hits = det->module(sector)->hits();
    for(UInt_t i = 0; i < hits.size(); ++i) {
      StEmcRawHit* hit = hits[i];
      Char_t sub = 'A'+(hit->sub()-1)%5;

      // Layer: 'P'=preshower1, 'Q'=preshower2, 'R'=postshower
      Char_t layer = 'P'+(hit->sub()-1)/5;

      // Database ranges: sector=1-12, sub=A-E, eta=1-12, type=T,P-R; Slow method
      const EEmcDbItem* x = eeDb->getTile(sector, sub, hit->eta(), layer);
      if (!x) continue;

      // Drop broken channels
      if (mDropBad && x->fail) continue;
      Float_t adc = hit->adc();
      Float_t energy = adc / x->gain; // GEANT energy deposit (GeV)
      Float_t newadc = adc;
      if(adc > 0) {
	if (mHList) hA[1]->Fill(adc);

	/// Addition of Poisson fluctuations and Gaussian 
	/// 1 p.e. resolution
	Float_t mipval = energy / Pmip2ene;
	Float_t avgpe  = mipval * Pmip2pe;
	Float_t Npe    = gRandom->Poisson(avgpe);

	if (Npe > 0) {
	  /// Determine number of photoelectrons
	  Float_t sigmape   = sqrt(Npe) * sig1pe;
	  Float_t smearedpe = gRandom->Gaus(Npe, sigmape);

	  /// Determine new ADC value
	  newadc = smearedpe * mip2ene * x->gain / Pmip2pe;
	}
      } // non-zero ADC on input

      /// Lookup pedestal in database (possibly zero)
      Float_t ped = mAddPed ? x->ped : 0;

      /// Smear the pedestal
      if (mSmearPed) ped = gRandom->Gaus(ped, x->sigPed);

      /// add signal & pedestal back
      Int_t NUadc = Int_t(newadc + 0.5 + ped);

      if (adc > 0 && mHList) hA[0]->Fill(NUadc); //??

      // printf("out: NUadc=%d \n",NUadc);
      ///
      /// If we've made it here, overwrite the muDst
      ///
      if (mOverwrite) hit->setAdc(NUadc);
    }
  }
}

void StEEmcSlowMaker::MakeSMD(StEmcCollection* emc)
{
  for (Char_t plane = 'U'; plane <= 'V'; ++plane) {
    StEmcDetector* det = 0;
    switch (plane) {
    case 'U':
      det = emc->detector(kEndcapSmdUStripId);
      if (!det) {
	gMessMgr->Debug("No kEndcapSmdUStripId");
	continue;
      }
      break;
    case 'V':
      det = emc->detector(kEndcapSmdVStripId);
      if (!det) {
	gMessMgr->Debug("No kEndcapSmdVStripId");
	continue;
      }
      break;
    }

    assert(det);

    for (UInt_t sector = 1; sector <= det->numberOfModules();++sector) {
      StSPtrVecEmcRawHit& hits = det->module(sector)->hits();
      for (UInt_t i = 0; i < hits.size(); ++i) {
	StEmcRawHit* hit = hits[i];
	Int_t strip = hit->eta();
	// Database ranges: sector=1-12, plane=U-V, strip=1-288
	const EEmcDbItem* x = eeDb->getByStrip(sector, plane, strip);
	if (!x) continue;

	/// Drop broken channels
	if (mDropBad && x->fail) continue;

	Float_t adc    = hit->adc();
	Float_t energy = adc / x->gain; // (GeV) Geant energy deposit 
	Float_t newadc = adc;

	if(adc > 0) {

	  if (mHList) { 
	    hA[12]->Fill(x->sec);
	    if (x->sec == 5) hA[20]->Fill(x->strip, adc); 
	    hA[3]->Fill(adc);
	  }

	  // Addition of Poisson fluctuations and 
	  // Gaussian 1 p.e. resolution
	  Float_t mipval = energy / mip2ene;
	  Float_t avgpe  = mipval * mip2pe[strip];
	  Float_t Npe    = gRandom->Poisson(avgpe);

	  if (Npe > 0) {

	    /// Determine number of photoelectrons
	    Float_t sigmape   = sqrt(Npe) * sig1pe;
	    Float_t smearedpe = gRandom->Gaus(Npe, sigmape);

	    /// Determine new ADC value
	    newadc = smearedpe * mip2ene * x->gain / mip2pe[strip];
	  }
	}

	/// Lookup pedestal in database (possibly zero)
	Float_t ped = mAddPed ? x->ped : 0;

	/// Smear the pedestal
	if (mSmearPed) ped = gRandom->Gaus(ped, x->sigPed);

	/// Add signal & pedestal back
	Int_t NUadc = Int_t(newadc + 0.5 + ped);

	if (adc > 0 && mHList) {
	  hA[19]->Fill(x->strip, NUadc);
	  hA[2]->Fill(NUadc);
	}

	///
	/// If we've made it here, overwrite the muDst
	///
	if (mOverwrite) hit->setAdc(NUadc);

      } // Loop over 1 plane
    }
  }
}

// $Log: StEEmcSlowMaker.cxx,v $
// Revision 1.5  2006/12/12 20:29:14  balewski
// added hooks for Endcap embedding
//
// Revision 1.4  2006/08/07 18:50:11  balewski
// added capabilty to run on StEvent, use se-method, see macros/ for example
//
// Revision 1.3  2005/09/23 17:23:01  balewski
// now peds are added also to ADC of zero
// fixed bug in ETOW ped smearing
//
// Revision 1.2  2005/09/23 01:30:10  jwebb
// Tower peds now added  if option is set.
//
// Revision 1.1  2004/12/15 17:02:56  balewski
// try 2
//
