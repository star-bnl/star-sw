#include <assert.h>
#include <stdio.h>
#include "Stiostream.h"
#include "StarVMCDetector.h"
#include "StarDetectorMap.h"
#include "StarMCHits.h"
#include "TMath.h"
#include "TClass.h"
#include "TROOT.h"
#include "TCallf77.h"
#include "TEnv.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
//______________________________________________________________________
extern "C"
{
  Int_t type_of_call g2t_volume_id_(DEFCHARD, Int_t * DEFCHARL); 
  Int_t type_of_call StarVMCDetector::g2t_volume_id(const Char_t *det, Int_t *numbv) 
  {return g2t_volume_id_(PASSCHARD(det), numbv PASSCHARL(det));}
  //  Float_t type_of_call getvalue_(const Char_t *parN) { TString par(parN,sizeof(parN)); return gEnv->GetValue(par, 0.);}
  Float_t type_of_call getvalue_(DEFCHARD parN DEFCHARL parL) { 
    TString par(parN,parL); 
    return gEnv->GetValue(par, 0.);
  }
}
ClassImp(StarVMCDetector);
//________________________________________________________________________________
StarVMCDetector::StarVMCDetector(const Char_t *name) : TDataSet(name), fId(kUnknownId), 
						       fK(-1), fFMT(""), 
						       fNVmax(0), fN10(0), fVolIdoffset(0), 
						       fIds(0), fSortedId(0),
						       fSId(0),  fChair(0) {
  TString Name(GetName());
  SetBit(kInvalidObject);
  for (Int_t i = 0; i < NoDetectors; i++) 
    if (Name == TString(Detectors[i].det)) {fId = Detectors[i].Id; SetTitle(Detectors[i].Csys); fK = i; break;}
  //  assert(fK >= 0);
  if (fK >= 0) InvertBit(kInvalidObject);
  else {
    cout << "Ignore sensitive volume: " << Name.Data() << endl;
  }
}
//________________________________________________________________________________
StarVMCDetector::~StarVMCDetector() {
  if (fSId) {delete [] fSId; fSId = 0;}
}
//________________________________________________________________________________
void StarVMCDetector::SetFMT(const Char_t *fmt) {
  fFMT = fmt;
  //  if (fFMT == "/HALL_1/CAVE_1/FPDM_1/FLGD_%d/FLGT_%d/FPCT_1") fFMT = "/HALL_1/CAVE_1/FBOX_8/FLGT_56/FPCT_1"
}
//________________________________________________________________________________
void StarVMCDetector::SetIds(Int_t N, Int_t *array) {
  if (N > 0 && array)  {fIds.Set(N, array);}
}
//________________________________________________________________________________
void StarVMCDetector::Clear(const Option_t* opt) {
  SafeDelete(fChair);
}
//________________________________________________________________________________
Int_t StarVMCDetector::GetElement(Int_t volumeId) {
  Int_t N = fIds.GetSize();
  if (N < 1) return 0;
  if (! fSId) {
    fSId = new Int_t*[N];
    fSortedId.Set(N);
    Int_t *ids = fIds.GetArray();
    Int_t *indx = fSortedId.GetArray();
    TMath::Sort(N, ids, indx, kFALSE);
    for (Int_t i = 0; i < N; i++)  fSId[i] = ids + indx[i];
  }
  Long64_t e = TMath::BinarySearch(N, (const Int_t **) fSId, volumeId);
  return fSortedId[e];
}
//________________________________________________________________________________
const Char_t *StarVMCDetector::FormPath(const Char_t *FMT, Int_t N, Int_t *numbv) {
  switch (N) {
  case  1: return Form(FMT, numbv[0]);
  case  2: return Form(FMT, numbv[0],numbv[1]);
  case  3: return Form(FMT, numbv[0],numbv[1],numbv[2]);
  case  4: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3]);
  case  5: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3],numbv[4]);
  case  6: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3],numbv[4],numbv[5]);
  case  7: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3],numbv[4],numbv[5],numbv[6]);
  case  8: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3],numbv[4],numbv[5],numbv[6],numbv[7]);
  case  9: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3],numbv[4],numbv[5],numbv[6],numbv[7],numbv[8]);
  case 10: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3],numbv[4],numbv[5],numbv[6],numbv[7],numbv[8],numbv[9]); 
  case 11: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3],numbv[4],numbv[5],numbv[6],numbv[7],numbv[8],numbv[9],	   
		       numbv[10]);
  case 12: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3],numbv[4],numbv[5],numbv[6],numbv[7],numbv[8],numbv[9],
		       numbv[10],numbv[11]);
  case 13: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3],numbv[4],numbv[5],numbv[6],numbv[7],numbv[8],numbv[9],
		       numbv[10],numbv[11],numbv[12]);
  case 14: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3],numbv[4],numbv[5],numbv[6],numbv[7],numbv[8],numbv[9],
		       numbv[10],numbv[11],numbv[12],numbv[13]);
  case 15: return Form(FMT, numbv[0],numbv[1],numbv[2],numbv[3],numbv[4],numbv[5],numbv[6],numbv[7],numbv[8],numbv[9],
		       numbv[10],numbv[11],numbv[12],numbv[13],numbv[14]);
  default:
    cout << "StarVMCDetector::FormPath illegal NVL = " << N << endl;
    assert(0);
  }
  return 0;
}
//________________________________________________________________________________
const Char_t *StarVMCDetector::FormPath(const Char_t *FMT, Int_t *numbvR) {
  // Reduce to no. of active numb (fNVmax[] != 1)
  Int_t N = fNVmax.GetSize();
  Int_t NR = 0;
  Int_t numbv[15];
  for (Int_t i = 0; i < N; i++) {
    if (fNVmax[i] != 1) {numbv[i] = numbv[NR]; NR++;} 
    else                 numbv[i] = 1;
  }
  return FormPath(FMT, N, numbv);
}
//________________________________________________________________________________
void StarVMCDetector::GetNumbv(const Char_t *path, Int_t *numbvR) {
  Int_t N = fNVmax.GetSize();
  Int_t nread = 0;
  Int_t numbv[15];
  switch (N) {
  case  1: nread = sscanf(path, fFMT.Data(), &numbv[0]); break;
  case  2: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1]); break;
  case  3: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2]); break;
  case  4: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2],&numbv[3]); break;
  case  5: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2],&numbv[3],&numbv[4]); break;
  case  6: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2],&numbv[3],&numbv[4],&numbv[5]); break;
  case  7: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2],&numbv[3],&numbv[4],&numbv[5]
			  ,&numbv[6]); break;
  case  8: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2],&numbv[3],&numbv[4],&numbv[5],
			  &numbv[6],&numbv[7]); break;
  case  9: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2],&numbv[3],&numbv[4],&numbv[5],
			  &numbv[6],&numbv[7],&numbv[8]); break;
  case 10: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2],&numbv[3],&numbv[4],&numbv[5],
			  &numbv[6],&numbv[7],&numbv[8],&numbv[9]); break;
  case 11: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2],&numbv[3],&numbv[4],&numbv[5],
			  &numbv[6],&numbv[7],&numbv[8],&numbv[9],&numbv[10]); break;
  case 12: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2],&numbv[3],&numbv[4],&numbv[5],
			  &numbv[6],&numbv[7],&numbv[8],&numbv[9],&numbv[10],&numbv[11]); break;
  case 13: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2],&numbv[3],&numbv[4],&numbv[5],
			  &numbv[6],&numbv[7],&numbv[8],&numbv[9],&numbv[10],&numbv[11],&numbv[12]); break;
  case 14: nread = sscanf(path, fFMT.Data(), &numbv[0],&numbv[1],&numbv[2],&numbv[3],&numbv[4],&numbv[5],
			  &numbv[6],&numbv[7],&numbv[8],&numbv[9],&numbv[10],&numbv[11],&numbv[12],&numbv[13]); break;
  default:
    cout << "StarVMCDetector::GetNumbv for volume " << GetName() << " illegal NVL = " << N << endl;
    //    assert(0);
  }
  if (N != nread) {
    cout << "StarVMCDetector::GetNumbv for volume " << GetName() << " mismatched" << endl;
    cout << "path  \t" << path << "\tand" << endl;
    cout << "format\t" << fFMT << endl;
    N = 0;
  }
  // Reduce to no. of active numb (fNVmax[] != 1)
  Int_t NR = 0;
  for (Int_t i = 0; i < N; i++) {
    if (fNVmax[i] != 1) {numbvR[NR] = numbv[i]; NR++;} 
  }
}
//________________________________________________________________________________
Int_t StarVMCDetector::GetVolumeId(const Char_t *path) {
  Int_t numbv[15];
  GetNumbv(path,numbv);
  Int_t g2tVolumeId = GetVolumeId(numbv);
  return g2tVolumeId;
}
//________________________________________________________________________________
const Char_t *StarVMCDetector::FormPath(Int_t volumeId) {
  static TString path;
  path = "UnKnown";
  Int_t numbv[15] = {0};
  if (fId == kTpcId) {
    Int_t sector = (volumeId/100)%100;
    Int_t row    = volume%100;
    numbv[0] = (sector <= 12) ? 1 : 2;
    numbv[1] = (sector-1)%12 + 1;
    if (fNVmax[3] == 73) {// old TPC
      if (row <= 13) numbv[2] = 2 + 3*(row -  1);
      else           numbv[2] = 41 +  (row - 14);
    } else if (fNVmax[3] == 76) {// new iTPC
      if (row <= 40) numbv[2] = 1 + row;
      else           numbv[2] = 2 + row;
    }
    path = FormPath(fFMT,numbv);
  }
  return path.Data();
}
//________________________________________________________________________________
Int_t StarVMCDetector::GetVolumeId(Int_t *numbv) {
  Int_t NIds = fIds.GetSize();
  if (NIds > 0) {
    Int_t N = fNVmax.GetSize();
    Int_t indx = numbv[0]-1;
    for (Int_t j = 1; j < N; j++) {indx *= fNVmax[j]; indx += numbv[j] - 1;}
    return fIds[indx];
  } else {
    return GetVolumeIdFromNubv(numbv);
  }
}
//________________________________________________________________________________
St_g2t_Chair  *StarVMCDetector::GetChair() {
  if (! fChair) fChair = NewChair(Detectors[fK].G2T_type,Detectors[fK].G2T_name);
  return fChair;
}
//________________________________________________________________________________
TTable *StarVMCDetector::NewTable(const Char_t *classname, const Char_t *name, Int_t nrows) {
  TTable *table = 0;
  if (classname) {
    TClass *cl = gROOT->GetClass(classname);
    if (cl) {
      table = (TTable *)cl->New();
      if (table) {
	table->Set(nrows);
	if (name && strlen(name)) table->SetName(name);
      }
    } 
  }
  return table; 
}
//________________________________________________________________________________
St_g2t_Chair *StarVMCDetector::NewChair(const Char_t *type, const Char_t *name) {
  TTable *table = (TTable *) StarMCHits::instance()->GetHitHolder()->Find(name);
  TString classname("St_");
  classname += type;
  if (! table) {
    table = NewTable(classname,name,100); StarMCHits::instance()->GetHitHolder()->Add(table);
  }
  St_g2t_Chair *chair = 0;
  classname += "C";
  TClass *cl = gROOT->GetClass(classname);
  if (cl) {
    chair = (St_g2t_Chair *)cl->New();
    if (chair) chair->SetTable(table);
  } 
  return chair;
}
//________________________________________________________________________________
void StarVMCDetector::SetNVmax(Int_t N, Int_t *array) {
  fNVmax.Set(N, array);
  if (fN10.GetSize() == 0) {
    fN10.Set(N);
    fN10[N-1] = 1;
    for (Int_t i = N-2; i >= 0; i--) {
      fN10[i] = (Int_t ) TMath::Power(10,(Int_t)(TMath::Log10(fNVmax[i])+1))*fN10[i+1];
    }
  }
}
//________________________________________________________________________________
Int_t StarVMCDetector::GetVolumeIdFromNubv(Int_t *numbv) {
  Int_t nbv[15] = {0};
  Int_t N = fNVmax.GetSize();  
  TCL::ucopy(numbv,nbv,N);
  return g2t_volume_id(Detectors[fK].Csys, nbv);
}
//________________________________________________________________________________
