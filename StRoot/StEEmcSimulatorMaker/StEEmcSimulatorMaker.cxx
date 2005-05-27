/////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEEmcSimulatorMaker is class for begin_html <FONT COLOR="RED">EMC Simulation</FONT> end_html dataset
//
//                                                                      
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include <assert.h>
#include <math.h>
#include "TROOT.h"
#include <TRandom.h>
#include <TBrowser.h>
#include <TPad.h>
#include <StMessMgr.h>

#include "tables/St_g2t_emc_hit_Table.h"

#include "StEEmcSimulatorMaker.h"

#include "StEmcCollection.h"
#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StEmcRawHit.h"

#include "StMcCalorimeterHit.hh"
#include "StMcEmcHitCollection.hh"

#include "StEmcUtil/others/emcInternalDef.h"  // For Emc-internal

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h" // For Endcap Emc
#include "StEEmcUtil/EEmcMC/StEEmcMCEnum.h"   // EEmc MC Ids and constants
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"

static const TString eemcDetname[] = { "etow", "eprs", "esmdu", "esmdv" };

ClassImp(StEEmcSimulatorMaker)

//_____________________________________________________________________________
StEEmcSimulatorMaker::StEEmcSimulatorMaker(const char *name):StMaker(name)
{
   mEEmc            = 2;  // EEMC  on 
   mHistControl     = 0;  // Hist  off 
   mPrint           = kTRUE;

   mSimEtow         = kTRUE;
   mSimEprs         = kTRUE;
   mSimEsmd         = kTRUE;

   mAddPed          = kTRUE;
   mSmearPed        = kTRUE;
   mDropBad         = kTRUE;
   mOverwrite       = kTRUE;

   mEmcCollection   = NULL;
   mMuEmcCollection = NULL;

   eeDb             = 0;

   geaIn            = 0;

   g2t_eem_hit      = 0;
   g2t_esm_hit      = 0;

   mC1              = NULL;
   m_nhit           = 0;   
}

//_____________________________________________________________________________
StEEmcSimulatorMaker::~StEEmcSimulatorMaker() 
{
  if(mEmcCollection) delete mEmcCollection; 
  if(mMuEmcCollection) delete mMuEmcCollection; 
  if(mMuEmcUtil) delete mMuEmcUtil;
}

//_____________________________________________________________________________
Int_t StEEmcSimulatorMaker::Init()
{
  //
  eeDb=(StEEmcDbMaker*)GetMaker("eemcDb");
  assert( eeDb);
  mKSigma = eeDb -> KsigOverPed;

  mMuEmcUtil = new StMuEmcUtil();

  if(mHistControl) for(Int_t i = 0; i<NDETECTORS; i++)  bookHistograms(i);

  Histograms()->SetName("SimuHist");

  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StEEmcSimulatorMaker::Make()
{
  mEmcCollection = NULL;
  mMuEmcCollection = NULL;

  // Changed the order of searching - xdf first.
  static Char_t* typeOfFile[3] = {"xdf", "geant.root", "fz"};
  static Char_t* nameIn[3] = {"event/geant/Event", "geantBranch", "geant"};
  //  Find  Geant  directory with hits
  for(Int_t i=0; i<3; i++)
  {
    geaIn = GetDataSet(nameIn[i]);
    if(geaIn) 
    {
      if(Debug()>=2) if(mPrint) printf("Type of file -> %s : GEANT directory -> %s\n", typeOfFile[i], nameIn[i]);
      break;
    }
  }
  if (!geaIn) 
  {
    if(Debug()>=2) if(mPrint) gMessMgr->Error()<<"Geant Data didn't find in "<< nameIn[0]<<" or "<< nameIn[1]<<endm;
    return kStWarn;
  }

  Int_t retEEmc;
  if(mEEmc) retEEmc = makeEEmc();

  fillStEvent();
  if(Debug()) if(mPrint) printEEmcHits();
/*
 We have to convert StEvent to MuDst to use the reliable EEMC database which 
 is compatible with MuDst. StMuEmcUtil does not carry energy for TOWER when 
 converting EEMC data from MuDst to StEvent or from StEvent to MuDst. All
 TOWER energy data are lost after conversion. Energy is recalculated in 
 StEEmcMixerMaker.
*/
  mMuEmcCollection = mMuEmcUtil->getMuEmc(getEmcCollection());

  if(mSimEtow) simEtow();
  if(mSimEprs) simEprs();
  if(mSimEsmd) simEsmd();
  if(mSimEtow || mSimEprs || mSimEsmd) {
    mEmcCollection = mMuEmcUtil->getEmc(mMuEmcCollection);
  }
  if(Debug()>=2) if(mPrint) printEEmcHits();
  
  return retEEmc;   
}

//_____________________________________________________________________________
Int_t StEEmcSimulatorMaker::makeEEmc() 
{ 
  for(Int_t i=0; i<NDETECTORS; i++)
  {
    TString nameHits = eemcDetname[i] + "McHits";
    mEmcMcHits[i] = new StMcEmcHitCollection(nameHits.Data());
    m_DataSet->Add(mEmcMcHits[i]);
  }
  g2t_eem_hit = (St_g2t_emc_hit *) geaIn->Find("g2t_eem_hit");
  if (g2t_eem_hit )
  {
    if (g2t_eem_hit->GetNRows()>0) makeEtowAndEprsMcHits();
    else if(mPrint) gMessMgr->Warning()
             << " makeEEmc() => table g2t_eem_hit is empty " << endm;
  }
  else if(mPrint) gMessMgr->Warning()
             << " makeEEmc() => table g2t_eem_hit isn't found " << endm;

  g2t_esm_hit = (St_g2t_emc_hit *) geaIn->Find("g2t_esm_hit");
  if (g2t_esm_hit)
  {
    if (g2t_esm_hit->GetNRows()>0) makeEsmdMcHits();
    else if(mPrint) gMessMgr->Warning()
             << " makeEEmc() => table g2t_esm_hit is empty " << endm;
  }
  else if(mPrint) gMessMgr->Warning()
             << " makeEEmc() => table g2t_esm_hit isn't found " << endm;

  if(mEEmc >= 2) makeAllRawHitsForEEmc();

  if(mHistControl)
  {
    makeHistograms(Etow);
    makeHistograms(Eprs);
    makeHistograms(Esmdu);
    makeHistograms(Esmdv);
  }

  return kStOk;
}

//_____________________________________________________________________________
Int_t StEEmcSimulatorMaker::makeEtowAndEprsMcHits()
{
  //
  // Decode g2t_eec_hit and fill Mc Hits for ETOW and EPRS.
  // See StMcEventMaker::fillEemc() method.
  //
  Float_t de;

  g2t_emc_hit_st *hit = g2t_eem_hit->GetTable();
  Int_t nhits         = g2t_eem_hit->GetNRows();
  

  for(Int_t ihit=0; ihit<nhits; ihit++,hit++) 
  { 
        de = hit->de;
        Int_t ivid = hit->volume_id;

        Short_t sec = 0;
        Short_t ssec = 0;
        ivid %= kEEmcTowerHalfId;

        Short_t phi = ivid/kEEmcTowerPhiId; ivid %= kEEmcTowerPhiId;
        Short_t eta = ivid/kEEmcTowerEtaId; ivid %= kEEmcTowerEtaId;
        Short_t depth = ivid/kEEmcTowerDepId; ivid %= kEEmcTowerDepId;

        ssec = (phi - 1) % 5 + 1; // sub-sector
        sec = (phi - 1) / 5 + 1;

        Int_t module_trans = sec; 
        Int_t eta_trans    = eta; 
        Int_t sub_trans    = ssec; 

// Summed signal for tower + pre + post 
        if(depth == kTower1Depth ||
           depth == kTower2Depth ||  
           depth == kPreShower1Depth ||
           depth == kPreShower2Depth ||
           depth == kPostShowerDepth) 
           addEtowHit(module_trans,eta_trans,sub_trans,de);

// Separated signal for pre/post. 
        if(depth == kPreShower1Depth ||
           depth == kPreShower2Depth ||
           depth == kPostShowerDepth) {
             if( depth == kPreShower2Depth)
               sub_trans = sub_trans + 5;
             else if( depth == kPostShowerDepth)
               sub_trans = sub_trans + 10;
             addEprsHit(module_trans,eta_trans,sub_trans,de);
        }
  }

  return kStOk;
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::addEtowHit(Int_t module,Int_t eta,Int_t sub,Float_t de)
{
  StMcCalorimeterHit *emchEtow = NULL;
/*
 Without tracing for track, we may end up with different EAddHit as 
 StMcEventMaker when executing addHit() because hits associated with different 
 tracks will be considered not the same even they have the same module, eta, 
 and sub. We will see less hits with EAddHit = kNew and more with 
 EAddHit = kAdd here than in StMcEventMaker! 
*/
  emchEtow = new StMcCalorimeterHit(module,eta,sub,de); //no trace for track 
  StMcEmcHitCollection::EAddHit etowNew = 
                     mEmcMcHits[Etow]->addHit(emchEtow);
  if(etowNew == StMcEmcHitCollection::kAdd)  {
      delete  emchEtow;
      emchEtow = 0;
  }
  else if(etowNew == StMcEmcHitCollection::kErr) { 
      delete emchEtow;
      emchEtow = 0;
      if(mPrint) gMessMgr->Warning()<<" Bad hit in Etow collection"<<endm;
  }
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::addEprsHit(Int_t module,Int_t eta,Int_t sub,Float_t de)
{
  StMcCalorimeterHit *emchEprs = NULL;
  emchEprs = new StMcCalorimeterHit(module,eta,sub,de); 
  StMcEmcHitCollection::EAddHit eprsNew = 
                     mEmcMcHits[Eprs]->addHit(emchEprs);
  if(eprsNew != StMcEmcHitCollection::kNew) {
      delete emchEprs;
      emchEprs = 0;
      if(mPrint) gMessMgr->Warning()<<" Bad hit in Eprs collection"<<endm;
  }
}

//_____________________________________________________________________________
Int_t StEEmcSimulatorMaker::makeEsmdMcHits()
{
  //
  // Decode g2t_emc_hit and fill Mc Hits for ESMD.  
  // See StMcEventMaker::fillEsmd() method.
  //

  Float_t de;

  g2t_emc_hit_st *hit = g2t_esm_hit->GetTable();
  Int_t nhits         = g2t_esm_hit->GetNRows();

  for(Int_t ihit=0; ihit<nhits; ihit++,hit++) 
  { 

    de   = hit->de;

    Int_t   ivid  = hit->volume_id;
    Short_t det   = 0;
    Short_t phi   = ivid/kEEmcSmdPhiId;  ivid %= kEEmcSmdPhiId;
    Short_t plane = ivid/kEEmcSmdPlaneId;ivid %= kEEmcSmdPlaneId;
    Short_t strip = ivid/kEEmcSmdStripId;ivid %= kEEmcSmdStripId;

    if (!ivid==0) {
        cout << "Critical error in EEMC-SMD volume id decoding" << endl;
    }
/*
 So far, phi in simulation does not have a reasonable value. The line below
 forces it to be in right range. The line would not affect a valid sec
 number, but may change a non-valid to a valid. It should be out later. 
*/
     Short_t sec = (phi%12 != 0)? phi%12:12; 

    if(plane >0 && plane < 4 && sec > 0 && sec < 13) {
        if(kEEmcSmdMapUV[plane-1][sec-1] == 0) {
          det = kEEmcMCSmdUStripId;
        }
        else if (kEEmcSmdMapUV[plane-1][sec-1] == 1) {
          det = kEEmcMCSmdVStripId;
        }
        else  {
          cout << " plane  =  " << plane << " sec = " << sec   
               << " strip = " << strip << " --> An empty SMD layer!" << endl;
          continue;
       }
    }
    else {
      if (Debug() >=2) {
        cout << "StEEmcSimulaterMaker: Out range Plane Id = " << plane
        << " and Sec Id = " << sec  << endl
        << "Error in indices! Unable to determine U/V layer" << endl;
       }
       continue;
    }
   
    Int_t module_trans = sec; 
    Int_t eta_trans    = strip;

// Never set sub = -1 for SMD even StEmcRawHit->sub() shows it is -1!!! 
    Int_t sub_trans    = 0;

    if(det == kEEmcMCSmdUStripId) { 
       addEsmdHit(module_trans,eta_trans,sub_trans,Esmdu,de);
    }
    else if(det == kEEmcMCSmdVStripId) {
       addEsmdHit(module_trans,eta_trans,sub_trans,Esmdv,de);
    }
  }
  return kStOk;
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::addEsmdHit(Int_t module,Int_t eta,Int_t sub,Int_t detector, Float_t de)
{
  StMcCalorimeterHit *emchEsmd;
  emchEsmd = new StMcCalorimeterHit(module,eta,sub,de); 
  StMcEmcHitCollection::EAddHit esmdNew = 
                   mEmcMcHits[detector]->addHit(emchEsmd);
      
  if(esmdNew != StMcEmcHitCollection::kNew) {
    delete emchEsmd;
    emchEsmd = 0;
    if(mPrint) gMessMgr->Warning()<<" Bad hit in Eemc(esmd) collection"<<endm;
  }
}

//_____________________________________________________________________________
Int_t StEEmcSimulatorMaker::makeAllRawHitsForEEmc()
{

  UInt_t  sec, eta, sub, adc; 
  Float_t energy, gain;

  emc_hits_st rawHit;
  for(Int_t i=0; i<NDETECTORS; i++) 
  { 

    TString nw = eemcDetname[i] + "RawHits"; 
    const ULong_t nhits   = mEmcMcHits[i]->numberOfHits();
    mEmcRawHits[i] = new St_emc_hits(nw, nhits);
    m_DataSet->Add(mEmcRawHits[i]);

    rawHit.det = i + 1;
    if(nhits>0) 
    {

      for(int is=0; is<kEEmcNumSectors; is++)
      {
        sec = is + 1; // is - C style index; sec - Fortran style index !!!!
        const StMcEmcModuleHitCollection* module = mEmcMcHits[i]->module(sec);
        const ULong_t nhsec = module->numberOfHits();
        if(nhsec>0)
        {
          rawHit.module = sec;
          const StSPtrVecMcCalorimeterHit hits = module->hits(); 
          for(UInt_t ih=0; ih<nhsec; ih++)
          {
            eta = hits[ih]->eta();
            sub = hits[ih]->sub();
            energy  = hits[ih]->dE();

// ADC which is not contained in StMcCalorimeterHit has to be generated here
// before being used by methods of simulation. 

            const EEmcDbItem *x;
            if ((int)sec>=eeDb->mfirstSecID && (int)sec<=eeDb->mlastSecID) {

              // Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R
              if(i == 0) // tower
                x=eeDb->getTile(sec,sub-1+'A', eta, 'T');

// See StMuEmcCollection ::getEndcapPrsHit for decoding
              else if(i == 1) {// pre/post 
                  int pre=1+(sub-1)/5;
                  int preSub=1+(sub-1)%5;
                  x=eeDb->getTile(sec,preSub-1+'A', eta, pre-1+'P');
              }
              else  {// SMD-U 
                Char_t uv = (i == 2) ? 'U' : 'V'; 
                x=eeDb->getByStrip(sec,uv,eta);
              }

              if(x==0) continue;
              if(mDropBad) if(x->fail ) continue;
              gain = x->gain;

// For debugging purpose, save hits of noise (low energy) in mEmcRawHits for 
// now even they will be cut by adcToEnergy() in StEEmcMixerMaker 
              if(gain > 0 && energy > 0) 
                adc = ((UInt_t)(energy*gain)>0) ? (UInt_t)(energy*gain) : 1;
              else adc = 0;
              rawHit.eta    = eta;
              rawHit.sub    = sub;
              rawHit.adc    = adc;
              rawHit.energy = energy;
              mEmcRawHits[i]->AddAt(&rawHit);
            }
          } 
        }
      }
    }  
  }

  return kStOk;
}

//_____________________________________________________________________________
Int_t StEEmcSimulatorMaker::fillStEvent()
{
  mEmcCollection = new StEmcCollection();
  for(Int_t i=0; i<NDETECTORS; i++)
  { 
    StDetectorId id = (StDetectorId)(i+kEndcapEmcTowerId);
    St_emc_hits  *table = mEmcRawHits[i];
    if(table)
    {
      StEmcDetector* detector = new StEmcDetector(id, 120);
      mEmcCollection->setDetector(detector);
      emc_hits_st *t = table->GetTable();
      for(Int_t j=0; j<table->GetNRows(); j++) if(t[j].adc>0)
      {
	   StEmcRawHit* hit = new StEmcRawHit(id,t[j].module, t[j].eta, 
           t[j].sub,t[j].adc, t[j].energy);
	   detector->addHit(hit);
      }	
    }
  }
  return kStOK;
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::simEtow() 
{
  //
  // To simulate EEMC tower (adding pedestal only for now). Not sure if
  // simulated correctly. Need to be checked later.     WMZ  5/13/2004  
  //
  for ( Int_t i = 0; i < mMuEmcCollection->getNEndcapTowerADC(); i++ ) {

    Int_t adc,sec,eta,sub;

    /// muDst ranges: sec:1-12, sub:1-5, eta:1-12 
    mMuEmcCollection->getEndcapTowerADC(i,adc,sec,sub,eta);
    
    /// tmp, for fasted analysis use only hits from sectors init in DB
    if (sec<eeDb->mfirstSecID || sec>eeDb->mlastSecID) continue;
     
    /// Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x=eeDb-> getTile(sec,sub-1+'A', eta, 'T'); 
    if(x==0) continue;
  
    /// Drop broken channels
    if(mDropBad) if(x->fail ) continue; 
      
    
    if(adc>0) {
      /// Lookup pedestal in database (possibly zero)
      Float_t ped    = (mAddPed)?x -> ped:0;
      /// Smear the pedestal
      Float_t sigped = ( x->thr - ped ) / mKSigma;
      if ( mSmearPed ) ped = gRandom -> Gaus(ped,sigped);
      
// just add ped for now
      Int_t   NUadc     = (Int_t)(adc + ped );

	///
	/// If we've made it here, overwrite the muDst
	///
// EEMC(=5) for Etow is EMC-internally defined in emcInternalDef.h 
      if ( mOverwrite ) mMuEmcCollection -> setTowerADC(i+1,NUadc,EEMC);

    }
  }
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::simEprs() 
{
  //
  // To simulate EEMC pre and post.  
  // See StEEmcSlowMaker::MakePre() method.
  //
  for ( Int_t i = 0; i < mMuEmcCollection->getNEndcapPrsHits(); i++ ) {

    Int_t pre,sec,eta,sub;

    /// muDst ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post
    StMuEmcHit *hit=mMuEmcCollection->getEndcapPrsHit(i,sec,sub,eta,pre);
    
    /// tmp, for fasted analysis use only hits from sectors init in DB
    if (sec<eeDb->mfirstSecID || sec>eeDb->mlastSecID) continue;
     
    /// Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x=eeDb-> getTile(sec,sub-1+'A', eta, pre-1+'P'); 
    if(x==0) continue;
  
    /// Drop broken channels
    if(mDropBad) if(x->fail ) continue; 
      
    Float_t adc   = hit -> getAdc();
    Float_t energy= adc / x->gain; // (GeV) Geant energy deposit 
    
    if(adc>0) {

      /// Addition of Poisson fluctuations and Gaussian 
      /// 1 p.e. resolution
      Float_t mipval = energy / Pmip2ene;
      Float_t avgpe  = mipval * Pmip2pe;
      Float_t Npe    = gRandom->Poisson(avgpe);

      /// Lookup pedestal in database (possibly zero)
      Float_t ped    = (mAddPed)?x -> ped:0;
      /// Smear the pedestal
      Float_t sigped = ( x->thr - ped ) / mKSigma;
      if ( mSmearPed ) ped = gRandom -> Gaus(ped,sigped);
      

      if (Npe>0) {

	/// Determine number of photoelectrons
	Float_t sigmape   = sqrt(Npe) * sig1pe;
	Float_t smearedpe = gRandom -> Gaus(Npe,sigmape);

	/// Determine new ADC value
	Float_t newadc    = smearedpe * mip2ene*(x->gain)/Pmip2pe;
	Int_t   NUadc     = (Int_t)(newadc + 0.5 + ped );

	///
	/// If we've made it here, overwrite the muDst
	///
	if ( mOverwrite ) hit -> setAdc( NUadc );

      }
    }
  }
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::simEsmd() 
{
  //
  // To simulate EEMC smd. 
  // See StEEmcSlowMaker::MakeSMD() method.
  //
  Char_t uv='U';
  for ( uv='U'; uv <= 'V'; uv++ ) {

    Int_t sec,strip;
    for (Int_t i = 0; i < mMuEmcCollection->getNEndcapSmdHits(uv); i++ ) {
      
      StMuEmcHit *hit=mMuEmcCollection->getEndcapSmdHit(uv,i,sec,strip);
      assert(sec>0 && sec<=MaxSectors);// total corruption of muDst
      
      // tmp, for fasted analysis use only hits from sectors init in DB
      if(sec<eeDb->mfirstSecID || sec>eeDb->mlastSecID) continue;
      
      const EEmcDbItem *x=eeDb->getByStrip(sec,uv,strip);
      assert(x); // it should never happened for muDst

      /// Drop broken channels
      if(mDropBad) {
        if(x->fail ) continue;
      }
    
      Float_t adc    = hit->getAdc();
      Float_t energy = adc/ x->gain; // (GeV) Geant energy deposit 
   
      if(adc>0) {
	
	// Addition of Poisson fluctuations and 
	// Gaussian 1 p.e. resolution
	Float_t mipval = energy / mip2ene;
	Float_t avgpe  = mipval * mip2pe[strip];
	Float_t Npe    = gRandom->Poisson(avgpe);

	/// Lookup pedestal in database (possibly zero)
	Float_t ped    = (mAddPed)?x -> ped:0;
	/// Smear the pedestal
	Float_t sigped = ( x->thr - ped ) / mKSigma;
	if ( mSmearPed ) ped = gRandom -> Gaus(ped,sigped);

	if (Npe>0) {

	  /// Determine number of photoelectrons
	  Float_t sigmape   = sqrt(Npe) * sig1pe;
	  Float_t smearedpe = gRandom -> Gaus(Npe,sigmape);

	  /// Determine new ADC value
	  Float_t newadc    = smearedpe * mip2ene * (x->gain) / mip2pe[strip];
	  Int_t NUadc       = (Int_t)(newadc + 0.5 + ped );

	  ///
	  /// If we've made it here, overwrite the muDst
	  ///
	  if ( mOverwrite ) hit -> setAdc( NUadc );
	  	  
	}
      }
    }
  }
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::bookHistograms(const Int_t i)
{
  const Char_t* tit = "Endcap ";

  const Int_t   nx[]  = {15,15,300,300};
  const Float_t xl[]  = {0.0,0.0,0.0,0.0};
  const Float_t xu[]  = {15.0,15.0,300.0,300.0};
  const Int_t   ny[]  = {15,15,15,15};
  const Float_t yl[]  = {0.0,0.0,0.0,0.0};
  const Float_t yu[]  = {15.0,15.0,15.0,15.0};
  const Int_t   binEnergySum[4] = {4000, 1000, 500, 500};
  const Float_t energySum[4] = {100., 10., 50., 50.};
  if(!m_nhit)
  {
    m_nhit = new TH2F("EEmcNHitsVsDet" ,
             "Number of hit(log) .vs. Detector #",100,0.0,5.0,5,0.0,4.0);
    m_etot = new TH2F("EEmcEtotVsDet" ,
             "Total energy(log) .vs. Detector #",100,0.0,5.0,5,0.0,4.0);
  }

  gMessMgr->Info()<<" Book hist for detector " << eemcDetname[i].Data() <<endm;

  TString name_h   = eemcDetname[i] + "Hits";
  TString name_e   = eemcDetname[i] + "Energy";
  TString name_adc = eemcDetname[i] + "Adc";

  TString title_h  = tit + eemcDetname[i] + " hits dist.";
  TString title_e  = tit + eemcDetname[i] + " energy dist.";
  TString title_adc= tit + eemcDetname[i] + " ADC dist.";
   
  m_hits[i]   = 
         new TH2F(name_h,title_h, nx[i],xl[i],xu[i], ny[i],yl[i],yu[i]);
  m_energy[i] = 
         new TH2F(name_e,title_e, nx[i],xl[i],xu[i], ny[i],yl[i],yu[i]);
  
  Int_t maxAdc = 2000;
  m_adc[i]     = 
         new TH1F(name_adc,title_adc, maxAdc+1, -1.0, float(maxAdc)); // ??

  if(mHistControl >= 2) 
  {
    TString nameS    = eemcDetname[i] + "Sec";
    TString titSector= tit + eemcDetname[i] + " #Sector dist.";
    mhSector[i]      = new TH1F(nameS,titSector, 15, 0.0, 15.0);
    nameS     = eemcDetname[i] + "Sub";
    titSector = tit + eemcDetname[i] + " #Sub. dist.";
    mhSub[i]  = new TH1F(nameS,titSector, 8, 0.0, 8.0);
   
   // this is only for checking
    name_e        = eemcDetname[i] + "EnergySum";
    title_e       = tit + eemcDetname[i] + " energy dist(sum)";
    mEnergySum[i] = 
           new TH1F(name_e, title_e, binEnergySum[i], 0.0, energySum[i]);
  }
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::makeHistograms(const Int_t det)
{
  Float_t energysum=0.0; 
  Float_t E; 
  Int_t nhit=0, m,e,s, adc;

  St_emc_hits* emc_hits = mEmcRawHits[det];
  Int_t n = emc_hits->GetNRows();

  if(n>0)
  {
    emc_hits_st *hit = emc_hits->GetTable();     
    for(Int_t i = 0; i<n; i++)
    {
      m   = (Int_t)hit[i].module; 
      e   = (Int_t)hit[i].eta;
      s   = (Int_t)hit[i].sub;
      adc = (Int_t)hit[i].adc;  // For testing only
      E   =        hit[i].energy;

      m_hits[det]->Fill(e,m); 
      m_energy[det]->Fill(e,m,E); 
      m_adc[det]->Fill(Axis_t(adc)); 
      nhit      += 1; 
      energysum += E; 
      if(mHistControl >= 2) 
        {
          if(mhSector[det]) mhSector[det]->Fill(Axis_t(m));
          if(mhSub[det])    mhSub[det]->Fill(Axis_t(s));
        }
    }
    m_nhit->Fill(log10((Double_t)nhit), (Float_t)det);
    m_etot->Fill(log10((Double_t)energysum), (Float_t)det);
    if(mHistControl >= 2) mEnergySum[det]->Fill(Double_t(energysum));
  }
}
//_____________________________________________________________________________
void StEEmcSimulatorMaker::printmEEmc()
{
  if(!mEEmc) gMessMgr->Info()<<" EEMC out of CHAIN  mEEmc="<<mEEmc<<endm;
  else       gMessMgr->Info()<<" EEMC     in CHAIN  mEEmc="<<mEEmc<<endl;
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::printSimFlags()
{
  cout << "mSimEtow = " << mSimEtow << " mSimEprs = " << mSimEprs
       << "mSimEsmd = " << mSimEsmd << endl; 
  cout << "mAddPed= " << mAddPed << " mSearPed = " << mSmearPed
       << " mDropBad = " << mDropBad << " mOverwrite " << mOverwrite <<endl; 
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::printEEmcHits()
{
  cout << "--------- StEEmcSimulatorMaker::printEEmcHits() --------" << endl;

  StEmcCollection* emcColl = getEmcCollection();
  for(Int_t i=0;i<NDETECTORS; i++) {
    mEmcMcHits[i]->print();
    StDetectorId id = (StDetectorId)(i+kEndcapEmcTowerId);
    StEmcDetector* detector = emcColl->detector(id);
    cout << " StEmcCollection: " <<eemcDetname[i].Data() << endl;
    if(detector) {
      for(int j=0;j<kEEmcNumSectors;j++) // loop over all sectors
      {
        StEmcModule* module = detector->module(j+1);
        StSPtrVecEmcRawHit& rawHit=module->hits();
        for(UInt_t k=0;k<rawHit.size();k++) {
           cout << "   hit " << k  
                << ": mod(sector) = " << rawHit[k]->module() 
                << " eta = " << rawHit[k]->eta()  
                << " sub = " << rawHit[k]->sub() 
                << " adc = " << rawHit[k]->adc()
                << " E = " << rawHit[k]->energy() << endl; 
        } 
      }
    }
// double check 
/*
    St_emc_hits  *table = mEmcRawHits[i];
    if(table)
    {
      emc_hits_st *t = table->GetTable();
      cout << " mEmcRawHits[" << i << "]" <<": Total number of hits = " 
                                 << table->GetNRows() << endl;
      for(Int_t j=0; j<table->GetNRows(); j++) 
         cout << "   hit " << j  << ": mod(sector) = " << t[j].module 
              << " eta = " << t[j].eta  << " sub = " << t[j].sub << " adc = " 
              << t[j].adc << " E = " << t[j].energy << endl; 
    }
*/
  }
}
//_____________________________________________________________________________
void StEEmcSimulatorMaker::Browse(TBrowser* b)
{
//  Will see StEmcCollection in browser as separate entity (if unzero)
  if(mEmcCollection) b->Add((TObject*)mEmcCollection);
  TDataSet::Browse(b);
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::pictureAllDetectors(Int_t print)
{
  //
  // 22-mar-2001 for convinience
  //
  if(!mC1) mC1 = new TCanvas("mC1","Picture for all detectors",0,25,600,800);
  else     mC1->SetTitle("Picture for all detectors");

  mC1->Clear();
  mC1->Divide(1,2);

  mC1->cd(1); 
  m_nhit->SetLineWidth(4);
  m_nhit->Draw();
  mC1->cd(2);
  m_etot->SetLineWidth(4);
  m_etot->Draw();
  mC1->Update();
  if(print) mC1->Print("ps/allDetectors.ps");
}

//_____________________________________________________________________________
void StEEmcSimulatorMaker::pictureForDetector(Int_t det, Int_t logy, Int_t print)
{
  if(!mC1) mC1 = new TCanvas("mC1","Picture for detector",0,25,600,800);
  else     mC1->SetTitle("Picture for detector");

  mC1->Clear();
  mC1->Divide(1,3);

  Int_t i = det-1;
  i = (i<0)?0:((i>3)?3:i);

  mC1->cd(1); 
  m_hits[i]->SetLineWidth(4);
  m_hits[i]->Draw();
  mC1->cd(2);
  m_energy[i]->SetLineWidth(4);
  m_energy[i]->Draw();
  mC1->cd(3);
  m_adc[i]->SetLineWidth(4);
  gPad->SetLogy(logy);
  m_adc[i]->Draw();
  mC1->Update();
  if(print) 
  {
    TString name("ps/Det");
    name += eemcDetname[i] + ".ps";
    mC1->Print(name.Data());
  }
}

//////////////////////////////////////////////////////////////////////////
// $Id: StEEmcSimulatorMaker.cxx,v 1.2 2005/05/27 16:16:28 wzhang Exp $
// $Log: StEEmcSimulatorMaker.cxx,v $
// Revision 1.2  2005/05/27 16:16:28  wzhang
// Include StEEmcMCEnum.h for MC Ids
//
// Revision 1.1  2005/05/23 19:41:23  wzhang
// First version
//
//
//////////////////////////////////////////////////////////////////////////
// ----------------------------------------------------------------------------
