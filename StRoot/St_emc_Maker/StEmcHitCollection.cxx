// $Id: StEmcHitCollection.cxx,v 1.3 1999/03/03 04:12:15 fisyak Exp $
// $Log: StEmcHitCollection.cxx,v $
// Revision 1.3  1999/03/03 04:12:15  fisyak
// replace kStErr to kStWarn
//
// Revision 1.2  1999/02/21 21:00:15  pavlinov
// Delete one line with debugging print
//
// Revision 1.1  1999/02/12 19:15:19  akio
// *** empty log message ***
//
// Revision 1.1  1998/12/15 22:39:53  akio
// Add emc_hit object and  adc_to_energy in here.
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEmcHitCollection class for EMC Calibrated hits                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <math.h>
#include "/afs/rhic/star/packages/SL98j/pams/global/inc/math_constants.h"
#include "/afs/rhic/star/packages/SL98j/pams/emc/inc/emc_def.h"
#include "StEmcHitCollection.h"
ClassImp(StEmcHitCollection)

//_____________________________________________________________________________
StEmcHitCollection::StEmcHitCollection() : St_DataSet("emc_hit"){
  //default constructor
  SetTitle("emc_hit");
  mMode = 0; mNRaw = 0; mNHit = 0; mEnergySum = 0.0, mEtSum = 0.0;
}
//_____________________________________________________________________________
StEmcHitCollection::StEmcHitCollection(const Char_t *Name) : St_DataSet(Name) {
  //construct with name and detector #
  SetTitle("emc_hit");
  mMode = 0; mNRaw = 0; mNHit = 0; mEnergySum = 0.0, mEtSum = 0.0;
}
//_____________________________________________________________________________
StEmcHitCollection::~StEmcHitCollection(){
}
//_____________________________________________________________________________
inline Int_t StEmcHitCollection::getID(Int_t m, Int_t e, Int_t s){
  //get ID from bin(module, eta, sub)
  return (m-1)*mNes+(e-1)*mNSub+s-1;
}
//_____________________________________________________________________________
inline Int_t StEmcHitCollection::getID(Int_t i){
  //get ID from raw#
  if(mMode) return (Int_t)mID[i];
  else return i;
}
//_____________________________________________________________________________
Int_t StEmcHitCollection::getRaw(Int_t m, Int_t e, Int_t s){
  //get raw# from bin
  //If it's not full array, search the hit. If not found, return 0
  Short_t id=(Short_t)getID(m,e,s);
  if(mMode){
    Int_t i;
    for(i=(Int_t)mModulePosition[m]; i<(Int_t)mModulePosition[m+1]; i++){if(mID[i]==id) return i;}
    return 0;
  }else{
    return id;
  }
}
//_____________________________________________________________________________
void StEmcHitCollection::getBin(Int_t i, Int_t *m, Int_t *e, Int_t *s){
  //Get Bin(module, eta, sub) from raw#.
  Int_t id=getID(i), j = id%mNes;
  *m = id/mNes + 1;
  *e = j/mNSub + 1; 
  *s = j%mNSub + 1;
}
//_____________________________________________________________________________
void StEmcHitCollection::getPos(Int_t m, Int_t e, Int_t s, Float_t *eta, Float_t *phi){
  //Get Position (eta, phi) from bin.
  const Float_t offset[2] = {75.0, 105.0};
  const Float_t sign[2]   = {-1.0 , 1.0};
  Int_t iphi, rl=1;
  Float_t nphi=mNModule*mNSub/2.0;
  St_DataSetIter local(gStChain->DataSet("params"));
  St_ems_control *st_ems_control = (St_ems_control *) local("emc/ems/ems_control");
  ems_control_st *ems_control = st_ems_control->GetTable();
  if(m <= (mNModule/2) ){
    iphi=(m - 1)*mNSub + s;
    *phi=sign[0]*(iphi - 0.5)/nphi*C_2PI + offset[0]*C_RAD_PER_DEG;
  }else{
    iphi=(m - mNModule/2 - 1)*mNSub + s;
    *phi=sign[1]*(iphi - 0.5)/nphi*C_2PI + offset[1]*C_RAD_PER_DEG;
    rl=-1;
  }
  while(*phi >= C_2PI) *phi-=C_2PI;
  while(*phi < 0.0)    *phi+=C_2PI;
  switch(mDetector){
  case BEMC: case BPRS:
    *eta=rl*(ems_control[0].bemc_eta_bin[e-1]+ems_control[0].bemc_eta_bin[e])/2.0;
    break;
  case BSMDE:
    *eta=rl*(e-0.5)/Float_t(mNEta);
      //if(e<=75){
      //  z = (e-0.5)/75.0 * 0.5;
      //}else{
      //  z = BSMDE_SEP_ETA + (e-BSMDE_SEP_BIN-0.5) / (Float_t)BSMDE_SEP_BIN
      //    * (ems_control[0].bemc_eta_bin[neta]-BSMDE_SEP_ETA);
      //}
      //z *= rl*ems_control[0].bemc_max_z;
      //*eta=-1.0*log(tan(atan2(ems_control[0].bemc_inner_r,z)/2.0));
    break;
  case BSMDP:
    *eta=rl*(ems_control[0].bemc_eta_bin[e*2-2]+ems_control[0].bemc_eta_bin[e*2])/2.0;
    break;
  case EEMC: case EPRS:
    *eta=rl*(ems_control[0].eemc_eta_bin[e-1]+ems_control[0].eemc_eta_bin[e])/2.0;
    break;
  default:
    *eta=rl;
  }
}
//_____________________________________________________________________________
void StEmcHitCollection::getPos(Int_t i, Float_t *eta, Float_t *phi){
  //Get Position (eta, phi) from raw#.
  Int_t m, e, s;
  getBin(i, &m, &e, &s);
  getPos(m,e,s,eta,phi);
}
//_____________________________________________________________________________
void StEmcHitCollection::getGrid(Int_t m, Int_t e, Int_t s, Int_t *ieta, Int_t *iphi){
  //Get grid(eta, phi) from bin.
  Float_t eta, phi;
  getPos(m,e,s,&eta,&phi);
  *ieta=((eta<0.0) ? -1 : 1)*e;
  *iphi=(Int_t)(phi/C_2PI*mNModule*mNSub/2.0);
}
//_____________________________________________________________________________
void StEmcHitCollection::getGrid(Int_t i, Int_t *ieta, Int_t *iphi){
  //Get grid(eta, phi) from raw#.
  Int_t m, e, s;
  getBin(i,&m,&e,&s);
  getGrid(m,e,s,ieta,iphi);
}
//_____________________________________________________________________________
St_emc_hits* StEmcHitCollection::copyToTable(const Char_t *Name){
  //Create a table emc_hits and restore hits. 
  Int_t   i, m, e, s, n=0;
  emc_hits_st raw;
  St_emc_hits *emc_hits = new St_emc_hits((Text_t*)Name, mNHit);
  for(i=0; i<mNRaw; i++){
    if(mEnergy[i]>0.0){
      getBin(i,&m,&e,&s);
      raw.det    = mDetector;
      raw.module = m;
      raw.eta    = e;
      raw.sub    = s;
      raw.adc    = -2;
      raw.energy = mEnergy[i];
      emc_hits->AddAt(&raw, n);
      n++;
    }
  }
  return emc_hits;
}
//_____________________________________________________________________________
void StEmcHitCollection::init(Int_t det){
  //Initiarize private data members
  St_DataSetIter local(gStChain->DataSet("params"));
  St_ems_control *st_ems_control = (St_ems_control *) local("emc/ems/ems_control");
  ems_control_st *ems_control = st_ems_control->GetTable();
  const char *dname[]={"bemc","bprs","bsmde","bsmdp","eemc","eprs","esmde","esmdp"};
  mName     = dname[det-1];
  mDetector = det;
  mNModule  = Int_t(ems_control->nmodule[det-1]);
  mNEta     = Int_t(ems_control->neta[det-1]); 
  mNSub     = Int_t(ems_control->nsub[det-1]);
  mNes      = mNEta*mNSub;
}
//_____________________________________________________________________________
void StEmcHitCollection::getMemory(Int_t n){
  //Get memory for hits, if n>0 then also ID and Mod
  if(n < 0){
    mMode     = 0;
    mNRaw     = mNModule*mNes;
    mEnergy.Set(mNRaw);
  }else{
    mMode     = 1;
    mNRaw     = n;
    mEnergy.Set(mNRaw);
    mID.Set(mNRaw);
    mModulePosition.Set(mNModule+1);
  }
}
//_____________________________________________________________________________
Int_t StEmcHitCollection::ADCtoEnergy(St_emc_hits *emc_hit, TArrayF *E){
  //Apply calibration and convert ADC to energy
  Int_t i;
  Int_t n = emc_hit->GetNRows();   
  emc_hits_st *hit = emc_hit->GetTable();
  //Getting calibration data
  TString n_ped_h="emc/new/"+mName+"/pedestal/ped_"+mName+"_h";
  TString n_ped  ="emc/new/"+mName+"/pedestal/ped_"+mName;
  TString n_slp_h="emc/new/"+mName+"/gain/slp_"+mName+"_h";
  TString n_slp  ="emc/new/"+mName+"/gain/slp_"+mName;
  St_DataSetIter  local(gStChain->DataSet("calib"));
  St_emc_calib_header *m_ped_h  = (St_emc_calib_header *) local(n_ped_h);
  St_emc_pedestal     *m_ped    = (St_emc_pedestal *)     local(n_ped);
  St_emc_calib_header *m_slp_h  = (St_emc_calib_header *) local(n_slp_h);
  St_emc_adcslope     *m_slp    = (St_emc_adcslope *)     local(n_slp);
  if(m_ped_h && m_ped && m_slp_h && m_slp){
    emc_calib_header_st *ped_h = m_ped_h->GetTable();
    emc_pedestal_st     *ped   = m_ped->GetTable();
    emc_calib_header_st *slp_h = m_slp_h->GetTable();
    emc_adcslope_st     *slp   = m_slp->GetTable();
    if(ped_h->det != mDetector){
      cout << "***StEmcHitCollection::ADCtoEnergy: Pedestal data is not for this datector: "
	   << mName << endl;
      return  kStWarn;
    }
    if(slp_h->det != mDetector){
      cout << "***StEmcHitCollection::ADCtoEnergy: Gain data is not for this datector: "
	   << mName << endl;
      return  kStWarn;
    }
    if(ped_h->nmodule != mNModule  ||
       ped_h->neta    != mNEta     ||
       ped_h->nsub    != mNSub     ||
       slp_h->nmodule != mNModule  ||
       slp_h->neta    != mNEta     ||
       slp_h->nsub    != mNSub     ){      
      cout << "***StEmcHitCollection::ADCtoEnergy: Inconsistent Ped or Gain data: "
	   << mName << endl;
      return  kStWarn;
    }
    mNHit=0; mEnergySum=0.0; mEtSum =0.0; 
    switch(slp_h->func){
    case 1:
      for(i = 0; i<n; i++){
	Float_t eta, phi;
	Int_t id  = getID((Int_t)hit[i].module, (Int_t)hit[i].eta, (Int_t)hit[i].sub);
	getPos((Int_t)hit[i].module, (Int_t)hit[i].eta, (Int_t)hit[i].sub, &eta, &phi);
        if(hit[i].adc>-1){   
          if(slp[id].p0 > 0.0){
	    Float_t Et = (hit[i].adc-ped[id].ped)/slp[id].p0;	      
            (*E)[id] = Et * cosh(eta);
	    if((*E)[id]<=0.0) (*E)[id]=0.0;
	    else{mNHit++; mEnergySum+=(*E)[id]; mEtSum+=Et;}
          } else {
            cout << "StEmcHitCollection::ADCtoEnergy: Adcslope is less equal 0: "
		 << mName << " : " << id <<endl;
            (*E)[id] = -1.0;
          }
	}else{                        // if adc<0, use GEANT energy
	  (*E)[id] = hit[i].energy;
	  mNHit++; mEnergySum+=hit[i].energy; mEtSum+=hit[i].energy/cosh(eta);
	} 
      }
      break;
    default:
      cout<< "***StEmcHitCollection::ADCtoEnergy: Function has not been implimented yet: "
	  <<mName<<" : "<<slp_h->func<<endl;
      return  kStWarn;
    }    
  }else{
    cout<< "StEmcHitCollection::ADCtoEnergy: Not enough information, use GEANT energy: "
	<< mName << endl;    
    for(i = 0; i<n; i++){
      if(hit[i].energy>0.0){
	Float_t eta, phi;
	Int_t id  = getID((Int_t)hit[i].module, (Int_t)hit[i].eta, (Int_t)hit[i].sub);
	getPos((Int_t)hit[i].module, (Int_t)hit[i].eta, (Int_t)hit[i].sub, &eta, &phi);
	(*E)[id] = hit[i].energy;
	mNHit++; mEnergySum+=hit[i].energy; mEtSum+=hit[i].energy/cosh(eta);
      }
    }
  }
  return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcHitCollection::fill(St_emc_hits *emc_hits){
  //Fill energy from emc_hit table(ADC).
  //Switch between two data storing method depending on
  //the (number of hit)/(number of total bin).
  Int_t n=emc_hits->GetNRows();   
  if(n==0) return kStOK;
  emc_hits_st *hit = emc_hits->GetTable();
  if(hit[0].det<1 || hit[0].det>MAXDET){
    cout<< "StEmcHitCollection::Fill: No hits or bad detector# in emc_hits_st : "
	<< hit[0].det<<"  "<< getDetName() <<endl;
    return kStWarn;
  }
  init(hit[0].det);
  if(n > mNModule*mNes*2/3){
    getMemory(-1);
    if(ADCtoEnergy(emc_hits, &mEnergy) != kStOK) return kStWarn;
  }else{
    TArrayF E(mNModule*mNes);
    if(ADCtoEnergy(emc_hits, &E) != kStOK) return kStWarn;
    getMemory(mNHit);     
    Int_t m, e, s, i=0, j=0;
    for(m=0; m<mNModule; m++){
      mModulePosition[m]=(Short_t)j;
      for(e=0; e<mNEta; e++){
	for(s=0; s<mNSub; s++){
	  if(E[i]>0.0){mID[j]=(Short_t)i; mEnergy[j]=E[i]; j++;}
	  i++;
	}
      }
    }
    mModulePosition[mNModule]=mNHit;
  }
  return kStOK;
}
//_____________________________________________________________________________
St_TableSorter *StEmcHitCollection::getSortedID(){
// Create index sorted by energy
  St_TableSorter *index = new St_TableSorter(mEnergy.GetArray(), mNRaw);
  //  StChain dummy;
  //  dummy.SortDown(mNRaw, energy.GetArray(), index->GetArray(), down);  
  return index;
}
//_____________________________________________________________________________
TArrayI *StEmcHitCollection::getNeighborID(int org, int* deta, int* dphi){
// Get neighbor hit id
  TArrayI *index = new TArrayI(mNRaw);
  index->AddAt(org,0);
  return index;
}
//_____________________________________________________________________________
void StEmcHitCollection::printHits(Int_t n, Int_t start){
  //Print hits
  Int_t i, j=0, m, e, s;
  cout << endl << mName << " : ";
  if(mNRaw<=0){cout << "No hits" << endl; return;}
  else{cout << mNHit << " hits" << endl;} 
  cout << "Raw  ID  Module  Eta  Sub  Energy"<<endl;
  if(start<0) start=0;
  if(n<=0) n=1;
  if(start>=mNRaw) start=mNRaw-1;
  if(start+n>=mNRaw) n=mNRaw-start;
  for(i=start; i<start+n; i++){
    if(mMode){ 
      while(mModulePosition[j]<i) j++;
      if(mModulePosition[j]==i){
	cout << "-------------------------------"<<endl;
	j++;
      }
    }
    getBin(i,&m,&e,&s);
    cout <<i<<"  "<<getID(i)<<"  "<<m<<"  "<<e<<"  "<<s<<"  "<<mEnergy[i]<<endl;
  }
}
//_____________________________________________________________________________
void StEmcHitCollection::printHitsAll(){
  //Print all the hits
  printHits(mNRaw, 0);
}
//_____________________________________________________________________________
void StEmcHitCollection::Browse(TBrowser *b){
  //Print all the hits
  printHits(mNRaw, 0);
}

