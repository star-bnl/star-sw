// $Id: St_emc_hit.cxx,v 1.1 1998/12/15 22:39:53 akio Exp $
// $Log: St_emc_hit.cxx,v $
// Revision 1.1  1998/12/15 22:39:53  akio
// Add emc_hit object and  adc_to_energy in here.
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_emc_hit class for EMC Calibrated hits                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <math.h>
#include "/afs/rhic/star/packages/SL98j/pams/global/inc/math_constants.h"
#include "/afs/rhic/star/packages/SL98j/pams/emc/inc/emc_def.h"
#include "St_emc_hit.h"
ClassImp(St_emc_hit)

//_____________________________________________________________________________
St_emc_hit::St_emc_hit() : St_DataSet("emc_hit"){
  //default constructor
  SetTitle("emc_hit");
  mode = 0; nraw = 0; nhit = 0; etot = 0.0;
}
//_____________________________________________________________________________
St_emc_hit::St_emc_hit(const Char_t *Name) : St_DataSet(Name) {
  //construct with name and detector #
  SetTitle("emc_hit");
  mode = 0; nraw = 0; nhit = 0; etot = 0.0;
}
//_____________________________________________________________________________
St_emc_hit::~St_emc_hit(){
}
//_____________________________________________________________________________
inline Int_t St_emc_hit::GetId(Int_t m, Int_t e, Int_t s){
  //get ID from bin(module, eta, sub)
  return (m-1)*nes+(e-1)*nsub+s-1;
}
//_____________________________________________________________________________
inline Int_t St_emc_hit::GetId(Int_t i){
  //get ID from raw#
  if(mode) return (Int_t)ID[i];
  else return i;
}
//_____________________________________________________________________________
Int_t St_emc_hit::GetRaw(Int_t m, Int_t e, Int_t s){
  //get raw# from bin
  //If it's not full array, search the hit. If not found, return 0
  Short_t id=(Short_t)GetId(m,e,s);
  if(mode){
    Int_t i;
    for(i=(Int_t)Mod[m]; i<(Int_t)Mod[m+1]; i++){if(ID[i]==id) return i;}
    return 0;
  }else{
    return id;
  }
}
//_____________________________________________________________________________
void St_emc_hit::GetBin(Int_t i, Int_t *m, Int_t *e, Int_t *s){
  //Get Bin(module, eta, sub) from raw#.
  Int_t id=GetId(i), j = id%nes;
  *m = id/nes + 1;
  *e = j/nsub + 1; 
  *s = j%nsub + 1;
}
//_____________________________________________________________________________
void St_emc_hit::GetPos(Int_t m, Int_t e, Int_t s, Float_t *eta, Float_t *phi){
  //Get Position (eta, phi) from bin.
  const Float_t offset[2] = {75.0, 105.0};
  const Float_t sign[2]   = {-1.0 , 1.0};
  Int_t iphi, rl=1;
  Float_t z, nphi=nmodule*nsub/2.0;
  St_DataSetIter local(gStChain->DataSet("params"));
  St_ems_control *st_ems_control = (St_ems_control *) local("emc/ems/ems_control");
  ems_control_st *ems_control = st_ems_control->GetTable();
  if(m <= (nmodule/2) ){
    iphi=(m - 1)*nsub + s;
    *phi=sign[0]*(iphi - 0.5)/nphi*C_2PI + offset[0]*C_RAD_PER_DEG;
  }else{
    iphi=(m - nmodule/2 - 1)*nsub + s;
    *phi=sign[1]*(iphi - 0.5)/nphi*C_2PI + offset[1]*C_RAD_PER_DEG;
    rl=-1;
  }
  while(*phi >= C_2PI) *phi-=C_2PI;
  while(*phi < 0.0)    *phi+=C_2PI;
  switch(detector){
  case BEMC: case BPRS:
    *eta=rl*(ems_control[0].bemc_eta_bin[e-1]+ems_control[0].bemc_eta_bin[e])/2.0;
    break;
  case BSMDE:
    *eta=rl*(e-0.5)/Float_t(neta);
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
void St_emc_hit::GetPos(Int_t i, Float_t *eta, Float_t *phi){
  //Get Position (eta, phi) from raw#.
  Int_t m, e, s;
  GetBin(i, &m, &e, &s);
  GetPos(m,e,s,eta,phi);
}
//_____________________________________________________________________________
void St_emc_hit::GetGrid(Int_t m, Int_t e, Int_t s, Int_t *ieta, Int_t *iphi){
  //Get grid(eta, phi) from bin.
  Float_t eta, phi;
  GetPos(m,e,s,&eta,&phi);
  *ieta=((eta<0.0) ? -1 : 1)*e;
  *iphi=(Int_t)(phi/C_2PI*nmodule*nsub/2.0);
}
//_____________________________________________________________________________
void St_emc_hit::GetGrid(Int_t i, Int_t *ieta, Int_t *iphi){
  //Get grid(eta, phi) from raw#.
  Int_t m, e, s;
  GetBin(i,&m,&e,&s);
  GetGrid(m,e,s,ieta,iphi);
}
//_____________________________________________________________________________
St_emc_hits* St_emc_hit::CopyToTable(const Char_t *Name){
  //Create a table emc_hits and restore hits. 
  Int_t   i, m, e, s, n=0;
  emc_hits_st raw;
  St_emc_hits *emc_hits = new St_emc_hits((Text_t*)Name, nhit);
  for(i=0; i<nraw; i++){
    if(energy[i]>0.0){
      GetBin(i,&m,&e,&s);
      raw.det    = detector;
      raw.module = m;
      raw.eta    = e;
      raw.sub    = s;
      raw.adc    = -2;
      raw.energy = energy[i];
      emc_hits->AddAt(&raw, n);
      n++;
    }
  }
  return emc_hits;
}
//_____________________________________________________________________________
void St_emc_hit::Init(Int_t det){
  //Initiarize private data members
  St_DataSetIter local(gStChain->DataSet("params"));
  St_ems_control *st_ems_control = (St_ems_control *) local("emc/ems/ems_control");
  ems_control_st *ems_control = st_ems_control->GetTable();
  const char *dname[]={"bemc","bprs","bsmde","bsmdp","eemc","eprs","esmde","esmdp"};
  name     = dname[det-1];
  detector = det;
  nmodule  = Int_t(ems_control->nmodule[det-1]);
  neta     = Int_t(ems_control->neta[det-1]); 
  nsub     = Int_t(ems_control->nsub[det-1]);
  nes      = neta*nsub;
}
//_____________________________________________________________________________
void St_emc_hit::GetMem(Int_t n){
  //Get memory for hits, if n>0 then also ID and Mod
  if(n < 0){
    mode     = 0;
    nraw     = nmodule*nes;
    energy.Set(nraw);
  }else{
    mode     = 1;
    nraw     = n;
    energy.Set(nraw);
    ID.Set(nraw);
    Mod.Set(nmodule+1);
  }
}
//_____________________________________________________________________________
Int_t St_emc_hit::ADCtoEnergy(St_emc_hits *emc_hit, TArrayF *E){
  //Apply calibration and convert ADC to energy
  Int_t i, id;
  Int_t n = emc_hit->GetNRows();   
  emc_hits_st *hit = emc_hit->GetTable();
  //Getting calibration data
  TString n_ped_h="emc/new/"+name+"/pedestal/ped_"+name+"_h";
  TString n_ped  ="emc/new/"+name+"/pedestal/ped_"+name;
  TString n_slp_h="emc/new/"+name+"/gain/slp_"+name+"_h";
  TString n_slp  ="emc/new/"+name+"/gain/slp_"+name;
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
    if(ped_h->det != detector){
      cout << "***St_emc_hit::ADCtoEnergy: Pedestal data is not for this datector: "
	   << name << endl;
      return  kStErr;
    }
    if(slp_h->det != detector){
      cout << "***St_emc_hit::ADCtoEnergy: Gain data is not for this datector: "
	   << name << endl;
      return  kStErr;
    }
    if(ped_h->nmodule != nmodule ||
       ped_h->neta    != neta    ||
       ped_h->nsub    != nsub    ||
       slp_h->nmodule != nmodule ||
       slp_h->neta    != neta    ||
       slp_h->nsub    != nsub    ){      
      cout << "***St_emc_hit::ADCtoEnergy: Inconsistent Ped or Gain data: "
	   << name << endl;
      return  kStErr;
    }
    switch(slp_h->func){
    case 1:
      for(i = 0; i<n; i++){
        if(hit[i].adc>-1){   
	  id  = GetId(hit[i].module, hit[i].eta, hit[i].sub);  
          if(slp[id].p0 > 0.0){
            (*E)[id] = (hit[i].adc-ped[id].ped)/slp[id].p0;
	    if((*E)[id]<=0.0) (*E)[id]=0.0;
	    else{nhit++; etot+=(*E)[id];}
          } else {
            cout << "St_emc_hit::ADCtoEnergy: Adcslope is less equal 0: "
		 << name << " : " << id <<endl;
            (*E)[id] = -1.0;
          }
	}else{                        // if adc<0, use GEANT energy
	  (*E)[id] = hit[i].energy;
	  nhit++; etot+=hit[i].energy;
	} 
      }
      break;
    default:
      cout<< "***St_emc_hit::ADCtoEnergy: Function has not been implimented yet: "
	  <<name<<" : "<<slp_h->func<<endl;
      return  kStErr;
    }    
  }else{
    cout<< "St_emc_hit::ADCtoEnergy: Not enough information, use GEANT energy: "
	<< name << endl;    
    for(i = 0; i<n; i++){
      if(hit[i].energy>0.0){
	(*E)[GetId(hit[i].module, hit[i].eta, hit[i].sub)] = hit[i].energy;
	nhit++; etot+=hit[i].energy;
      }
    }
  }
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_emc_hit::Fill(St_emc_hits *emc_hits){
  //Fill energy from emc_hit table(ADC).
  //Switch between two data storing method depending on
  //the (number of hit)/(number of total bin).
  Float_t ene;
  Int_t n=emc_hits->GetNRows();   
  emc_hits_st *hit = emc_hits->GetTable();
  if(hit[0].det<1 || hit[0].det>MAXDET){
    cout<< "St_emc_hit::Fill: No hits or bad detector# in emc_hits_st : "
	<< hit[0].det<<"  "<< GetName() <<endl;
    return kStOK;
  }
  Init(hit[0].det);
  if(n > nmodule*nes*2/3){
    GetMem(-1);
    if(ADCtoEnergy(emc_hits, &energy) != kStOK) return kStErr;
  }else{
    TArrayF E(nmodule*nes);
    if(ADCtoEnergy(emc_hits, &E) != kStOK) return kStErr;
    GetMem(nhit);     
    Int_t m, e, s, i=0, j=0;
    for(m=0; m<nmodule; m++){
      Mod[m]=(Short_t)j;
      for(e=0; e<neta; e++){
	for(s=0; s<nsub; s++){
	  if(E[i]>0.0){ID[j]=(Short_t)i; energy[j]=E[i]; j++;}
	  i++;
	}
      }
    }
    Mod[nmodule]=nhit;
  }
  return kStOK;
}
//_____________________________________________________________________________
void St_emc_hit::PrintHits(Int_t n, Int_t start){
  //Print hits
  Int_t i, j=0, m, e, s;
  cout << endl << name << endl; 
  if(nraw<=0){cout << "No hits" << endl; return;}
  cout << "Raw  ID  Module  Eta  Sub  Energy"<<endl;
  if(start<0) start=0;
  if(n<=0) n=1;
  if(start>=nraw) start=nraw-1;
  if(start+n>=nraw) n=nraw-start;
  for(i=start; i<start+n; i++){
    if(mode){ 
      while(Mod[j]<i) j++;
      if(Mod[j]==i){
	cout << "-------------------------------"<<endl;
	j++;
      }
    }
    GetBin(i,&m,&e,&s);
    cout <<i<<"  "<<GetId(i)<<"  "<<m<<"  "<<e<<"  "<<s<<"  "<<energy[i]<<endl;
  }
}
//_____________________________________________________________________________
void St_emc_hit::PrintHitsAll(){
  //Print all the hits
  PrintHits(nraw, 0);
}







