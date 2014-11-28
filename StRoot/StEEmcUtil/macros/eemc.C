//=======================================================================
// owner: Oleg Rogachevsky
//=======================================================================
//

#include <iostream.h>

#define EEMC  5         
#define EPRS  6
#define ESMDU 7         
#define ESMDV 8
#define EPSTS 9

//class    StChain;
class    St_geant_Maker;

TBrowser       *b    = 0;
//StChain        *chain= 0;
St_geant_Maker *geant= 0;

const TString *detname[] = {
 "bemc", "bprs", "bsmde", "bsmdp",
 "eemc", "eprs", "esmdv", "esmdu", "epst" };

// _____________________________________________________________________
Int_t getVolIdEemc(const Int_t ivid, Int_t &sector,Int_t &eta,
Int_t &sub, Int_t &detector)
{

  static Int_t emcIvid[4]={100000,1000,10,1};
  Int_t emcChid[4], i, ividw, lowup, phi, dep;

  ividw = ivid;

  for(i=0; i<4; i++){
    emcChid[i] = ividw/emcIvid[i];
    ividw      = ividw%emcIvid[i];
  }

  if(ividw == 0){
    lowup  = emcChid[0];  // low/up half: =1 for lower, and =2 for upper
    phi    = emcChid[1];  // sector phi [1-60]
    eta    = emcChid[2];  // pseudorapidity bin number [1-12]
    dep    = emcChid[3];  // depth section [1-4];

    if (dep > 4) 
      printf(" getVolIdEemc => wrong value of section %i \n",dep);
    detector = EEMC;
    if (dep ==  1) detector = EPRS;
    else if 
       (dep ==  4) detector = EPSTS;

    sector=(phi-1)/5 + 1;
    sector=(phi-1)/5 + 1;
    sub   = (phi-1)%5 + 1;
    //      cout<<" Phi  "<<phi<<endl;
    
  }
  else{
    printf("getVolIdEemc: error decoding EEMC Geant volume Id %i; lowup=%i\n",
	   ivid, lowup);
    return 1;
  }
  
  //   TPC sector
  if (lowup ==  1) sector = sector + 3;
  else {
    sector = sector + 9;
    if (sector > 12 )
      sector = sector - 12;
  }
  
  //printf(" vol_id=%i TPC sector=%2i  subsector=%1i eta=%2i section=%2i\n",
  //        ivid,sector,sub,eta,dep);  
  
  return 0;
}

// _____________________________________________________________________
Int_t getVolIdEsmd(const Int_t ivid, Int_t &sector,Int_t &plane,
Int_t &strip, Int_t &detector)
{
  static Int_t smdIvid[4]={1000000,10000,1000,1}; //matched with AGI&G2T
  Int_t smdChid[5], i, ividw, lowup, phi;

  ividw = ivid;
  for(i=0; i<4; i++){
    smdChid[i] = ividw/smdIvid[i];
    ividw      = ividw%smdIvid[i];
  }

	  if(ividw == 0){       // 
    lowup     = smdChid[0];  // low/up half: =1 for lower, and =2 for upper
    phi    = smdChid[1];  // sector phi [1-6]
    plane  = smdChid[2];  // SMD plane 1-3
    strip  = smdChid[3];  // strip number 1-288

    if  (phi == 1 || phi == 4) {
			if (plane == 3) detector = ESMDV;
			if (plane == 1) detector = ESMDU;
		}
    else if  (phi == 2 || phi == 5) {
			if (plane == 1) detector = ESMDV;
			if (plane == 2) detector = ESMDU;
		}
    else if  (phi == 3 || phi == 6) {
			if (plane == 2) detector = ESMDV;
			if (plane == 3) detector = ESMDU;
		}
    else{
      printf(" getVolIdEsmd -- error decoding ESMD Geant volume Id %i; lowup=%i\n",
      ivid, lowup);
      return 1;
    }
	}
		
		sector = phi;

//   TPC sector
    if (lowup ==  1) sector = sector + 3;
		else {
		  sector = sector + 9;
			if (sector > 12 )
			  sector = sector - 12;
		}
      

    //printf("  lowup=%i TPC sector=%i plane=%i strip=%i\n",
    //			   lowup,sector,plane,strip);

  return 0;
}

// _____________________________________________________________________
void eemc(const Int_t Nevents=2,
	   const Char_t *fzfile ="pt20y11phi50_0100.fzd")
{

  Float_t E;
  Int_t  nhit=0, id,m,e,s, adc;
  Int_t  ivid,sector,eta,plane,strip,sub,det;
  Int_t  i=0;

  gROOT->LoadMacro("bfc.C");

  bfc(0,"fzin sim_T gen_T",fzfile);

  for (i=1; i<=Nevents; i++ ) {
    chain->Clear();
    if (chain->Make(i)>=kStEOF) break;

    St_g2t_emc_hit *g2t_emc_hit = (St_g2t_emc_hit *) chain->FindObject("g2t_eem_hit");
    if (g2t_emc_hit!=0) { 
      Int_t nhits         = g2t_emc_hit->GetNRows();
      g2t_emc_hit_st *hit = g2t_emc_hit->GetTable();
      printf("#-# Emc hits :volume_id,energy\n");
      for(Int_t ihit=0; ihit<nhits; ihit++,hit++) { 
	ivid  = hit->volume_id;
	E     = hit->de;
	getVolIdEemc(ivid,sector,eta,sub,det);
	fprintf(stderr,"%2i %8i %s %8.6f \n",ihit,ivid,detname[det-1],E);
      }
    }
      
    St_g2t_emc_hit *g2t_smd_hit = (St_g2t_emc_hit *) chain->FindObject("g2t_esm_hit");
    if (g2t_smd_hit !=0 ) { 
      Int_t nhits         = g2t_smd_hit->GetNRows();
      g2t_emc_hit_st *hit = g2t_smd_hit->GetTable();
      printf("#-# Smd hits :volume_id,energy\n");
      for(Int_t ihit=0; ihit<nhits; ihit++,hit++) { 
	ivid = hit->volume_id;
	E   =  hit->de;
	getVolIdEsmd(ivid,sector,plane,strip,det);
	printf("%2i %8i %s %8.6f \n",ihit,ivid,detname[det-1],E);
      }
    }
    printf ("=========================================== Done with Event no. %d\n",i);
    
  }
}

