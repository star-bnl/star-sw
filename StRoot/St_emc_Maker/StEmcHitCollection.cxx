// $Id: StEmcHitCollection.cxx,v 1.9 2004/05/03 23:32:39 perev Exp $
// $Log: StEmcHitCollection.cxx,v $
// Revision 1.9  2004/05/03 23:32:39  perev
// Possible non init WarnOff
//
// Revision 1.8  2003/09/02 17:59:28  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.7  1999/09/24 01:23:38  fisyak
// Reduced Include Path
//
// Revision 1.6  1999/07/16 18:04:00  pavlinov
// Little correction for StEclMake
//
// Revision 1.5  1999/07/02 03:01:56  pavlinov
// Little corrections for Linux
//
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
#include <Stiostream.h>
#include <math.h>
#include "StEmcHitCollection.h"
#include "St_TableSorter.h"
#include "TArrayF.h"
#include "TArrayL.h"
#include "emc_def.h"
ClassImp(StEmcHitCollection)

StEmcHitCollection::StEmcHitCollection() : St_DataSet("bemc") , StEmcGeom("bemc") {
  SetTitle("emc_hit");
  mNHit = 0; mEnergySum = 0.0, mEtSum = 0.0;
  mModule = 0;
  mNumsModule.Set(120); mIndexFirstLast.Set(121);
}
//_____________________________________________________________________________
StEmcHitCollection::StEmcHitCollection(const Char_t *Name) : St_DataSet(Name) , StEmcGeom(Name) {
  //construct with name and detector #
  SetTitle("emc_hit");
  mNHit = 0; mEnergySum = 0.0, mEtSum = 0.0; 
  mModule = 0;
  mNumsModule.Set(120); mIndexFirstLast.Set(121);
}
//_____________________________________________________________________________
StEmcHitCollection::~StEmcHitCollection(){ /* noopt */ }
//_____________________________________________________________________________
Int_t StEmcHitCollection::ADCtoEnergy(St_emc_hits *emc_hit){
  //Apply calibration and convert ADC to energy
  Float_t E;
  Int_t i; TString name(GetName());
  Int_t n = emc_hit->GetNRows();   
  emc_hits_st *hit = emc_hit->GetTable();
  //Getting calibration data
  TString n_ped_h="emc/new/"+name+"/pedestal/ped_"+name+"_h";
  TString n_ped  ="emc/new/"+name+"/pedestal/ped_"+name;
  TString n_slp_h="emc/new/"+name+"/gain/slp_"+name+"_h";
  TString n_slp  ="emc/new/"+name+"/gain/slp_"+name;

  St_DataSetIter  local(mEmcCalib);
  St_emc_calib_header *m_ped_h  = (St_emc_calib_header *) local(n_ped_h);
  St_emc_pedestal     *m_ped    = (St_emc_pedestal *)     local(n_ped);
  St_emc_calib_header *m_slp_h  = (St_emc_calib_header *) local(n_slp_h);
  St_emc_adcslope     *m_slp    = (St_emc_adcslope *)     local(n_slp);
  if(m_ped_h && m_ped && m_slp_h && m_slp){
    emc_calib_header_st *ped_h = m_ped_h->GetTable();
    emc_pedestal_st     *ped   = m_ped->GetTable();
    emc_calib_header_st *slp_h = m_slp_h->GetTable();
    emc_adcslope_st     *slp   = m_slp->GetTable();
    if(ped_h->det != Detector()){
      cout << "***StEmcHitCollection::ADCtoEnergy: Pedestal data is not for this datector: "
	   << name << endl;
      return  kStWarn;
    }
    if(slp_h->det != Detector()){
      cout << "***StEmcHitCollection::ADCtoEnergy: Gain data is not for this datector: "
	   << name << endl;
      return  kStWarn;
    }
    if(ped_h->nmodule != NModule()  ||
       ped_h->neta    != NEta()     ||
       ped_h->nsub    != NSub()     ||
       slp_h->nmodule != NModule()  ||
       slp_h->neta    != NEta()     ||
       slp_h->nsub    != NSub()     ){      
      cout << "***StEmcHitCollection::ADCtoEnergy: Inconsistent Ped or Gain data: "
	   << name << endl;
      return  kStWarn;
    }

    mNHit=0; mEnergySum=0.0; mEtSum =0.0; 
    switch(slp_h->func){
    case 1:

      for(i = 0; i<n; i++){
	Float_t eta, phi; Int_t id,m,e,s;
        m = (Int_t)hit[i].module; 
        e = (Int_t)hit[i].eta;
        s = (Int_t)hit[i].sub;

        if(!getId(m, e, s, id)){  // Check bound of index
          getEta(m, e, eta); getPhi(m, s, phi);

          if(hit[i].adc>-1){   
            if(slp[id].p0 > 0.0){
	      Float_t Et = (hit[i].adc-ped[id].ped)/slp[id].p0;	      
              E          = Et * cosh(eta);  //  ??
	      if(E<=0.0) E=0.0;
	      else{
                mEnergy[mNHit]=E; mId[mNHit]=id;
                mNHit++; mEnergySum+=E; mEtSum+=Et; 
              }
          } else {
              cout << "StEmcHitCollection::ADCtoEnergy: Adcslope is less equal 0: "
		   << name << " : " << id <<endl;
              E = -1.0;
            }
	  }else{                        // if adc<0, use GEANT energy
	    E = hit[i].energy;
	    mEnergy[mNHit]=E; mId[mNHit]=id;
            mNHit++; mEnergySum+=E; mEtSum+=E/cosh(eta);
	  } 
        } else {
          cout<<" Bad index m "<<m<<" e "<<e<<" s "<<s<<endl;   
        }
      }
      break;
    default:
      cout<< "***StEmcHitCollection::ADCtoEnergy: Function has not been implimented yet: "
	  <<name<<" : "<<slp_h->func<<endl;
      return  kStWarn;
    }    
  }else{
    cout<< "StEmcHitCollection::ADCtoEnergy: Not enough information, use GEANT energy: "
	<< name << " Nhits " << n << endl;    
    for(i = 0; i<n; i++){
      if(hit[i].energy>0.0){
	Float_t eta, phi; Int_t id,m,e,s;
        m = (Int_t)hit[i].module; 
        e = (Int_t)hit[i].eta;
        s = (Int_t)hit[i].sub;

        if(!getId(m, e, s, id)){  // Check bound of index
          getEta(m, e, eta); getPhi(m, s, phi);
	  E = hit[i].energy;
          if(E>0.0){
	    mEnergy[mNHit]=E; mId[mNHit]=id;
	    mNHit++; mEnergySum+=E; mEtSum+=E/cosh(eta);
          }
        } else {
          cout<<" Bad index m "<<m<<" e "<<e<<" s "<<s<<endl;   
        }
      }
    }
  }
  return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcHitCollection::fill(St_emc_hits *emc_hits)
{
  //Fill energy from emc_hit table(ADC).
  int i;

  Int_t n=emc_hits->GetNRows();   
  if(n==0) { return kStOK; cout<<" No ADC in "<<GetName()<<endl;}

  emc_hits_st *hit = emc_hits->GetTable();
  if(hit[0].det<1 || hit[0].det>MAXDET){
    cout<< "StEmcHitCollection::Fill: Bad detector# in emc_hits_st : "
	<< hit[0].det<<"  "<< GetName() <<endl;
    return kStWarn;
  }

  mId.Set(n); mEnergy.Set(n); // Memory alocation
  if(ADCtoEnergy(emc_hits ) != kStOK) return kStWarn;

  // Sort on id
  TArrayF ecopy=mEnergy;
  TArrayL idcopy(mNHit);
  for(i=0; i< mNHit; i++) {idcopy[i] =(Long_t)mId[i];}

  St_TableSorter sort(idcopy.GetArray(), mNHit);
  for(i=0; i< mNHit; i++) {
    Int_t j=sort.GetIndex(i);
    mId[i]     = idcopy[j];
    mEnergy[i] = ecopy[j];
  }

  // To calculate modules boundary => Service information
  Int_t id, m, e, s, mold;
  id   = HitId(0);  // First hit
  getBin(id, mold, e, s);
  mNumsModule[0]     = (Short_t)mold;
  mIndexFirstLast[0] = 0;
  mIndexFirstLast[1] = mNHit;
  mModule = 1;

  if(mNHit > 1){
    for(i=1; i<mNHit; i++){
      id   = HitId(i);
      getBin(id, m, e, s);
      if(m != mold){
        mold = m;
        mNumsModule[mModule]       = (Short_t)mold;
        mIndexFirstLast[mModule]   = i;
        mIndexFirstLast[mModule+1] = mNHit;
        ++mModule; 
      }
    }
  }
  //  mNumsModule.Set(mModule);
  //mIndexFirstLast.Set(mModule+1);
  
  return kStOK;
}
//_____________________________________________________________________________
St_emc_hits* StEmcHitCollection::copyToTable(const Char_t *Name){
  //Create a table emc_hits and restore hits. 
  Int_t   i, id, m, e, s, n=0;
  emc_hits_st raw;
  St_emc_hits *emc_hits = new St_emc_hits((Text_t*)Name, mNHit);
  for(i=0; i<mNHit; i++){
    if(mEnergy[i]>0.0){
      id = (Int_t) mId[i];
      getBin(id, m, e, s);
      raw.det    = Detector();
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
void StEmcHitCollection::printHits(Int_t n, Int_t start){
  //Print hits
  Int_t i, m, e, s,id;
  cout << endl << GetName() << " : ";
  if(mNHit<=0){cout << "No hits" << endl; return;}
  else{cout << mNHit << " hits" << " Modules "<<mModule<< endl;} 
  cout << "Raw  ID  Module  Eta  Sub  Energy"<<endl;
  if(start<0) start=0;
  if(n<=0) n=1;
  if(start>=mNHit) start=mNHit-1;
  if(start+n>=mNHit) n=mNHit-start;

  int mold=-999999;
  for(i=start; i<start+n; i++){
    id = (Int_t) mId[i]; getBin(id, m, e, s);
    if(i == start) mold=m;
    if(mold != m) {
      cout << "-------------------------------"<<endl;
      mold = m;
    }
    cout <<i<<"  "<<id<<"  "<<m<<"  "<<e<<"  "<<s<<"  "<<mEnergy[i]<<endl;
  }

  for(i=0; i<mModule; i++){
    Int_t jm=mIndexFirstLast[i+1] - mIndexFirstLast[i];
    cout<<i<<" #Modules "<<mNumsModule[i]<<" jm "<<jm<<
	" First "<<mIndexFirstLast[i]<<" Last "<<mIndexFirstLast[i+1]-1<<endl;
  }
}
//_____________________________________________________________________________
void StEmcHitCollection::printHitsAll(){
  //Print all the hits
  printHits(mNHit, 0);
}
//_____________________________________________________________________________
void StEmcHitCollection::Browse(TBrowser *b){
  //Print all the hits
  printHits(mNHit, 0);
}
//_____________________________________________________________________________
void StEmcHitCollection::printNameTable(){
  if(mEmcCalib)   printf(" mEmcCalib unzero,  name %s \n", mEmcCalib->GetName());
  else printf(" <E> Pointer of mEmcCalib   is zero  ************** \n");
}
