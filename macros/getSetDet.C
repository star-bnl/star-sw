#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TGiant3.h"
#include "TDataSet.h"
#include "TGenericTable.h"
#include "TInterpreter.h"
#include "StarCallf77.h"
#include "tables/St_det_hit_Table.h"
#include "tables/St_det_path_Table.h"
#define agfdig0  F77_NAME(agfdig0,AGFDIG0)
#define agfdpar  F77_NAME(agfdpar,AGFDPAR)
#define acfromr  F77_NAME(acfromr,ACFROMR)
R__EXTERN "C" {
  int  type_of_call agfdig0 (const char*,const char*,int,int);
  void type_of_call agfdpar (float &hits,const char *chit, float &alim, float &blim, float &bin, int);
}
#else
class TGiant3;
class TDataSet;
#endif
#define Debug() 1
TGiant3 *fGeant3 = 0;
TDataSet *m_Constants = 0;
#if 0
struct  det_path_st 
{ // from SJDU bank -- 'NVL' description
  Char_t VName[8]; // Volume of branching
  Int_t  Ncopy;    // number of branchings
  Int_t  Nb;       // number of bit needed
};

struct  det_hit_st
{ // from SJDU bank -- 'HIT' description
  Char_t  hit[8]; //  encoded hit name
  Int_t   option; //  encoded hit option (R-rounding,S-single step)
  Int_t   Nb;     //  number of bit requested
  Float_t Fmin;   //  hit low limit
  Float_t Fmax;   //  hit upper limit
  Float_t Origin; //  Geant hit origin (-Fmin)
  Float_t Factor; //  Geant packing factor
  Int_t   Nbit;   //  number of bit allocated
  Int_t   Iext;   //  address of the Geant user step routine
  Int_t   Ifun;   //  hit function code (1-18 at present)
};

// class ADetectorHit : public TTable
// {
//   public:
//     ClassDefTable(ADetectorHit,det_hit_st)
//     ClassDef(ADetectorHit,1) //C++ container for <ASimADetectorHit> hits

// class ADetectorSet : public TTable
// {
//   public:
//     ClassDefTable(ADetectorSet,det_path_st)
//     ClassDef(ADetectorSet,1) //C++ container for <ASimADetectorSet> hits
// };
// TableClassImpl(ADetectorSet,det_path_st);
// TableClassImpl(ADetectorHit,det_hit_st);
#endif
//________________________________________________________________________________
Char_t *acfromr(Float_t r=8009359) {// 'TYPE'
  const Char_t *S=" 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz ";
  Char_t *charm = new Char_t[5];
  memset (&charm[0], 0, 5);
  Int_t k = (int) r;
  for (int i = 3; i >= 0; i--) {
    int j = 077 & k; k = k >> 6; charm[i] = S[j];
    //    cout << "i\t" << i << "\tj\t" << j << "\tk\t" << k << "\t" << charm[i] << endl;
  }
  //  cout << charm << endl;
  return charm;
}
//________________________________________________________________________________
void getSetDet() {
  gROOT->LoadMacro("bfc.C");
  TString cmd("bfc(0,\"gstar,sim_T,nodefault\")");
  gInterpreter->ProcessLine(cmd.Data());
  fGeant3 = (TGiant3 *) StarMC::GetMC();
  if (! fGeant3) return;
  // Load Address of Geant3 commons    
  fGeant3->LoadAddress(); 
  //  Quest_t  *cquest = (Quest_t  *) fGeant3->Quest();
  Gclink_t *clink  = (Gclink_t *) fGeant3->Gclink();
  //  Gcflag_t *cflag  = (Gcflag_t *) fGeant3->Gcflag();
  //  Gcvolu_t *cvolu  = (Gcvolu_t *) fGeant3->Gcvolu();
  //  Gcnum_t  *cnum   = (Gcnum_t  *) fGeant3->Gcnum();
  Int_t    *z_iq   = (Int_t    *) fGeant3->Iq();
  Int_t    *z_lq   = (Int_t    *) fGeant3->Lq();
  Float_t  *z_q    = (Float_t  *) fGeant3->Q();
  //  Gcsets_t *csets  = (Gcsets_t *) fGeant3->Gcsets();
  Int_t JSET = clink->jset;
  if (JSET <= 0) return;
  Int_t  NSET=z_iq[JSET-1];
  Char_t Uset[8], Udet[8], Uvol[8];
  memset (Uset, 0, 8);
  memset (Udet, 0, 8);
  memset (Uvol, 0, 8);
  if (! m_Constants) m_Constants = new TDataSet("Constants");
  TDataSet *m_Detectors = new TDataSet("Detectors"); m_Constants->Add(m_Detectors);
  for (Int_t ISET=1;ISET<=NSET;ISET++) {
    Int_t JS=z_lq[JSET-ISET];
    if (JS <= 0) continue;
    Int_t NDET=z_iq[JS-1];
    memcpy (Uset, &z_iq[JSET+ISET], 4);
    TDataSet *set = new TDataSet(Uset);
    m_Detectors->Add(set);
    for (Int_t IDET=1;IDET<=NDET;IDET++) {
      Int_t JD=z_lq[JS-IDET];
      if (JD <=0) continue;
      Int_t NV=z_iq[JD+2];
      Int_t NWHI=z_iq[JD+7];
      Int_t NWDI=z_iq[JD+8];
      memcpy (Udet, &z_iq[JS+IDET], 4);
      if (Debug()) {
	cout << "  Set " << Uset << " Detector " << Udet
	     << "  NV " << NV << " NWHI " << NWHI << " NWDI " << NWDI << endl;
      }
      Int_t JDU = z_lq[JD-3];
      Int_t ivd = 0;
      if (JDU > 0) {
	TDataSet *det = new TDataSet(Udet);
	set->Add(det);
	Int_t i;
	Int_t i0      = (int) z_q[JDU+1]; 
	Int_t N       = (int) z_q[JDU+2]; 
	Int_t i1      = (int) z_q[JDU+3]; 
	Int_t Nva     = (int) z_q[JDU+4]; 
	Int_t i2      = (int) z_q[JDU+5]; 
	Int_t Nvb     = (int) z_q[JDU+6]; 
	Int_t Goption = (int) z_q[JDU+7]; 
	Int_t Serial  = (int) z_q[JDU+8]; 
	Int_t IdType  = (int) z_q[JDU+9]; 
	Int_t Iprin   = (int) z_q[JDU+10];
	if (Debug()) {
	  cout << " displacement for hit description part    = 10                " << i0 << endl;     
	  cout << " Number of all hit descriptors (both in non- and cum. parts)  " << N  << endl;     
	  cout << " displacement for volume description part=10+10*Nh            " << i1 << endl;     
	  cout << " Number of all volume descriptors (branching or not)          " << Nva << endl;    
	  cout << " displacement for the free space   = 10+10*Nh+3*Nv            " << i2 << endl;     
	  cout << " number of real volume branchings for NUMBV                   " << Nvb << endl;    
	  cout << " Hit option: 1 - single step, 4 - Calorimetry                 " << Goption << endl;
	  cout << " Valid serial number for this subset                          " << Serial << endl; 
	  cout << " USER detector number                                         " << IdType << endl; 
	  cout << " current print flag both for HITS and DIGI                    " << Iprin << endl;  
	}
#if 1
#if 0
	TGenericTable *detuV = new TGenericTable("det_path_st","Path",Nva);
	TGenericTable *detuH = new TGenericTable("det_hit_st","Hit",N);
#else
	St_det_path *detuV = new St_det_path("Path",Nva);
	St_det_hit  *detuH = new St_det_hit("Hit",N);
#endif
	det->Add(detuV);
	det->Add(detuH);
	det_path_st rowV;
	det_hit_st rowH;
#if 1
	agfdig0(Uset,Udet,strlen(Uset),strlen(Udet));
	float    hits[15],alim[15],blim[15],bin[15];
	memset (&hits[0],0,15*sizeof(float));
	memset (&alim[0],0,15*sizeof(float));
	memset (&blim[0],0,15*sizeof(float));
	memset (&bin[0] ,0,15*sizeof(float));
	const char chit[60]="";
	agfdpar(hits[0],chit,alim[0],blim[0],bin[0],4);	
#endif
	for (i = 0; i < N; i++) {
	  memset(&rowH,0,detuH->GetRowSize());
	  Int_t j = JDU + i0 + 10*i;
	  //	  Int_t Nam   = (int) z_q[j+ 1];//     encoded hit name in Display code
	  //	  memcpy (&rowH.hit[0], &chit[4*i], 4);
	  Char_t *HitName = acfromr(z_q[j+ 1]);
	  memcpy (&rowH.hit[0], HitName, 4);
	  delete [] HitName;
	  rowH.option = (int) z_q[j+ 2];//     encoded hit option (R-rounding,S-single step)
	  rowH.Nb     = (int) z_q[j+ 3];//     number of bit requested
	  rowH.Fmin   =       z_q[j+ 4];//     hit low limit
	  rowH.Fmax   =       z_q[j+ 5];//     hit upper limit
	  rowH.Origin =       z_q[j+ 6];//     Geant hit origin (-Fmin)
	  rowH.Factor =       z_q[j+ 7];//     Geant packing factor
	  rowH.Nbit   = (int) z_q[j+ 8];//     number of bit allocated
	  rowH.Iext   = (int) z_q[j+ 9];//     address of the Geant user step routine
	  rowH.Ifun   = (int) z_q[j+10];//     hit function code (1-18 at present)
//  Case  IC of ( X  Y  Z   R    RR   PHI  THET ETA  TDR  CP    _
//                U  V  W   ETOT ELOS BIRK STEP LGAM TOF  USER  _
//                XX YY ZZ  PX   PY   PZ   SLEN PTOT LPTO rese )
	  if (Debug()) {
	    if (! i) 
	    cout << "\thit \toption \tNb \tFmin \tFmax \tOrigin \tFactor \tNbit \tIext \tIfun" << endl;
	    cout << "\t"  << setw(4) << rowH.hit 
		 << "\t"  << rowH.option
		 << "\t"  << rowH.Nb    
		 << "\t"  << rowH.Fmin  //<< "/" << alim[i]
		 << "\t"  << rowH.Fmax  //<< "/" << blim[i]
		 << "\t"  << rowH.Origin
		 << "\t"  << rowH.Factor //<< "/" << bin[i]
		 << "\t"  << rowH.Nbit  
		 << "\t"  << rowH.Iext  
		 << "\t"  << rowH.Ifun  
		 << endl;
	  }
	  detuH->AddAt(&rowH);
	}
	if (Debug()) detuH->Print(0,N);
	for (i = i1; i < i2; i += 3) {
	  memset(&rowV,0,detuV->GetRowSize());
	  Int_t j    = JDU+i;
	  Int_t iv   = (int) z_q[j+1];
	  rowV.Ncopy = (int) z_q[j+2];
	  Int_t Nam  = (int) z_iq[clink->jvolum+iv];
	  rowV.Nb    = (int) z_q[j+3];
	  memcpy (&rowV.VName[0], &Nam, 4);
	  Char_t Udvol[] = "     ";
          if (rowV.Nb > 0) {
	    Int_t Namd = (int) z_iq[JD+2*ivd+11]; ivd++;
	    memcpy (Udvol, &Namd, 4);
	  }
	  if (Debug()) {
	    cout << "\t" << setw(4) << rowV.VName <<  "/" << Udvol 
		 << "\t" << rowV.Ncopy << "\t" << rowV.Nb << endl;
	  }
	  detuV->AddAt(&rowV);
	}
	if (Debug()) {
	  for (; ivd<NV; ivd++) {
	    Int_t Namd = (int) z_iq[JD+2*ivd+11]; ivd++;
	    Char_t Udvol[] = "     ";
	    memcpy (Udvol, &Namd, 4);
	    cout << "\t" << "    " <<  "/" << Udvol << endl;
	  }
	  Int_t n = detuV->GetNRows();
	  detuV->Print(0,n);
	}
#endif
      }
    }
  }
 }
