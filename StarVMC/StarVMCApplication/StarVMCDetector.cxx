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
						       fK(0), fFMT(""), 
						       fNVmax(0), fN10(0), fVolIdoffset(0), 
						       fIds(0), fSortedId(0),
						       fSId(0),  fChair(0) {
  TString Name(GetName());
  for (Int_t i = 0; i < NoDetectors; i++) 
    if (Name == TString(Detectors[i].det)) {fId = Detectors[i].Id; SetTitle(Detectors[i].G2T_sys); fK = i; break;}
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
const Char_t *StarVMCDetector::GetPath(Int_t volumeId) {
  static TString path("");
  Int_t numbv[15];
  Int_t N = fNVmax.GetSize();
  Int_t NIds = fIds.GetSize();
  if (NIds > 0) {
    Int_t elem = GetElement(volumeId);
    for (Int_t i = N-1; i >= 0; i--) {numbv[i] = elem%fNVmax[i] + 1; elem = elem/fNVmax[i];} 
  } else {
    GetNumbvFromVolueId(volumeId, numbv);
  }
  return FormPath(fFMT.Data(), N, numbv);
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
void StarVMCDetector::GetNumbv(const Char_t *path, Int_t *numbv) {
  Int_t N = fNVmax.GetSize();
  Int_t nread = 0;
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
}
//________________________________________________________________________________
Int_t StarVMCDetector::GetVolumeId(const Char_t *path) {
  Int_t numbv[15];
  GetNumbv(path,numbv);
  return GetVolumeId(numbv);
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
#if 0
  Int_t N = fNVmax.GetSize();  
  Int_t VolumeID = 0;
  switch (fId) {
  case kTpcId: VolumeID = TpcVolumeId(N,numbv); break;
  default: break;
  };
  if (VolumeID) return VolumeID;
  VolumeID = fVolIdoffset;
  for (Int_t n = 0; n < N; n++) {
    VolumeID += fN10[n]*numbv[n];
  }
  Int_t volume_id = g2t_volume_id(Detectors[fK].Csys, numbv);
  assert(VolumeID == volume_id);
  return VolumeID;
#else
  Int_t nbv[15] = {0};
  Int_t N = fNVmax.GetSize();  
  TCL::ucopy(numbv,nbv,N);
  return g2t_volume_id(Detectors[fK].Csys, nbv);
#endif
}
//________________________________________________________________________________
void StarVMCDetector::GetNumbvFromVolueId(Int_t VolumeID, Int_t *numbv) {
  Int_t N = fNVmax.GetSize();  
  VolumeID -= fVolIdoffset;
  TArrayI idxT(N); Int_t *indx = idxT.GetArray();
  Int_t  *n10 = fN10.GetArray();
  TMath::Sort(N,n10,indx,kTRUE);
  for (Int_t n = 0; n < N; n++) {
    numbv[indx[n]] = 0;
    if (fN10[indx[n]]) {
      numbv[indx[n]]  = VolumeID/fN10[indx[n]];
      VolumeID -= numbv[indx[n]]*fN10[indx[n]];
    }
  }
}
#if 0
//________________________________________________________________________________
Int_t StarVMCDetector::TpcVolumeId(Int_t N, Int_t *Numbv) {
  // HALL[1]/CAVE[1]/TpcRefSys[1]/TPCE[1]/TPGV[2]/TPSS[12]/TPAD[146]
  Int_t VolumeId = 0;
  if (N == 3) {
    Int_t tpgv   = Numbv[0];
    Int_t tpss   = Numbv[1];
    Int_t sector = tpss + 12*(tpgv-1);
    Int_t tpad   = Numbv[2];
    Int_t isdet  = 0;
    Int_t nbpads = fNVmax[2]/2;
    if (tpad > nbpads) tpad = tpad - nbpads;
    if (nbpads == 73) { // default configuration 13 "inner" + 32 "outer" + 4 "edge/fake" + 24 "inner fake"
      static Int_t tpads[73] = { 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 
				 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 
				 7, 8, 8, 8, 9, 9, 9,10,10,10, 
				 11,11,11,12,12,12,13,13,13,14, 
				 14,15,16,17,18,19,20,21,22,23, 
				 24,25,26,27,28,29,30,31,32,33, 
				 34,35,36,37,38,39,40,41,42,43, 
				 44,45,45};
      static Int_t isdets[73] = { 1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
				  0, 2, 1, 0, 2, 1, 0, 2, 1, 0,
				  2, 1, 0, 2, 1, 0, 2, 1, 0, 2,
				  1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
				  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				  0, 0, 2 };
      isdet = isdets[tpad-1];
      tpad  = tpads[tpad-1];
    } else if (nbpads == 68) {// nbpads = 32 "inner" + 32 "outer" + 4 "edge/fake"
       static Int_t tpads[68] = { 1, 1, 2, 3, 4, 5, 6, 7, 8, 9,
				  10,11,12,13,14,15,16,17,18,19,
				  20,21,22,23,24,25,26,27,28,29,
				  30,31,32,32,33,33,34,35,36,37,
				  38,39,40,41,42,43,44,45,46,47,
				  48,49,50,51,52,53,54,55,56,57,
				  58,59,60,61,62,63,64,64 };
       static Int_t isdets[68] = { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				   0, 0, 0, 2, 1, 0, 0, 0, 0, 0,
				   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				   0, 0, 0, 0, 0, 0, 0, 2 };
       isdet = isdets[tpad-1];
       tpad  = tpads[tpad-1];
    }
    VolumeId = 100000*isdet+100*sector+tpad;
  }
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::SvtVolumeId(Int_t N, Int_t *Numbv) {
  Int_t VolumeId = 0;
  //1*                                          Ken Wilson
  // Helen altered SVT volume IDs so agrees with hardware defs.
  //        if  (Cd=='SVTD') {
  Int_t lnumber    = Numbv[0];
  Int_t ladder     = Numbv[1];
  Int_t wafer      = Numbv[2];
  Int_t nladder, nwafer;
  if ( ladder  ==  0) {
    // This is the year 1 ladder
    nladder = 1;
    wafer   = lnumber;
    ladder  = 12;
    lnumber = 4;
    // Set First barrel ids
  } else if (lnumber <= 2) {
    nladder = 8;
    nwafer  = 4;
    //               wafer   = 5-wafer 
    // Set 2nd barrel ids
  } else if (lnumber <= 4) {
    nladder  = 12;
    nwafer   = 6;
    //               wafer   = 7-wafer   
  }
  // Set 3rd barrel ids
  else if (lnumber <= 6) {
    nladder  = 16;
    nwafer   = 7;
    //               wafer   = 8-wafer
  } else {
    //             print *,' G2T warning: layer number ',lnumber,
    //     >               '     in svt hits not found' 
  }
  
  // PN: change geant numbering (CCW) to STAR numbering(CW):
  static Int_t svtg_version = 2;
  if (nladder>1) {
    //             inner sub-layer - 0, outer - 1:
    Int_t lsub    = (lnumber-1)%2;
    if (svtg_version==1) { 
      //             OLD: 3 o'clock is geant's first and STAR N/4 element:
      ladder=nladder/4-(ladder-1)*2-lsub;;
      ladder=(nladder+ladder-1)%nladder+1;
    } else
      //             NEW: 12 o'clock is geant's first and STAR last element:
      ladder=nladder-(ladder-1)*2-lsub;
  }
  if (ladder < 1 || ladder > nladder || wafer < 1 || wafer > nwafer) {}
  else     VolumeId  = 1000*lnumber+100*wafer+ladder;
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::SsdVolumeId(Int_t N, Int_t *Numbv) {
  Int_t VolumeId = 7000+100*Numbv[1]+Numbv[0];
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::TofVolumeId(Int_t N, Int_t *Numbv) {
  Int_t VolumeId = 0;
  static Int_t btog_version = 4, btog_choice = 2;
  static Int_t btog_posit1[2] = {32,33};
  //4*                                             Frank Geurts
  Int_t rileft, sector, innout, sub_sector, section;
  if (btog_version==1) {
    rileft     = Numbv[0];
    sector     = Numbv[1];
    sub_sector = Numbv[2]; 
    innout     = Numbv[3];
    VolumeId  = 100000*rileft+1000*innout+10*sector+sub_sector;
  } else if (btog_version==2) {
    //          simulations done in 2000 - one tof tray on west side
    if (btog_choice==4) {
      rileft     = 1;
      sector     = btog_posit1[0];
      sub_sector = Numbv[0]; 
      innout     = Numbv[1];
      VolumeId  = 100000*rileft+1000*innout+10*sector+sub_sector;
      //      else
      //	print *,' g2t_VolumeId : choice not coded yet '
    }        
  } else if (btog_version>=3) {
    //          For simulations after 28-sep-00, before it was version 2
    if (btog_choice==2) {      // Full TOF
      rileft     = Numbv[0];       //     west(1)/east(2)
      sector     = Numbv[1];       //     tray(1-60)
      innout     = Numbv[2];       //     4wide(1)/5wide(2) sections
      sub_sector = Numbv[3];       //     theta-tray(4w:1-4, 5w:1-5)
      section    = Numbv[4];       //     phi-tray(4w:1-9,5w:1)
    } else if (btog_choice==3) {  // ~25% TOF (only on east side)
      rileft     = 2;              //     east (pre-set)
      sector     = Numbv[0];       //     tray
      innout     = Numbv[1];       //     4wide/5wide section
      sub_sector = Numbv[2];       //     theta-tray
      section    = Numbv[3];       //     phi-tray
    } else if (btog_choice<=7) {  //  TOFp (single tray)
      rileft     = 2;              //     east (pre-set)
      if (btog_choice!=7)          //
	sector    = btog_posit1[0]; //     tray (pre-set)
      else                        //
	sector    = btog_posit1[1]; //
    } else {                      //
      innout     = Numbv[0];       //     4wide/5wide section
      sub_sector = Numbv[1];       //     theta-tray
      section    = Numbv[2];       //     phi-tray
    }
//            else
//              print *,' g2t_VolumeId: unknown TOF choice.'
//              print *,' g2t_VolumeId: btog_choice=',btog_choice
//            }
  }
//  -------- sanity check ---------
  if ((rileft < 1) || (rileft >  2) || 
      (sector < 1) || (sector > 60) || 
      (innout < 1) || (innout >  2) || 
      (sub_sector < 1) || 
      ((innout==1) && (sub_sector > 4)) || 
      ((innout==2) && (sub_sector > 5)) || 
      (section < 1) || (section > 9) || 
      ((innout == 2) && (section != 1))) {
    //    print *,' g2t_VolumeId: TOF sanity check failed.',      rileft,sector,innout,sub_sector,section
    return 0;
  }
//  -------- combine 4wide and 5wide sections ---------
  if (innout==1) {
    section = section+1;  //  phi-tray (4wide-sections)
  }
  
  //  -------- encode VolumeId ---------
  VolumeId = 100000*rileft+1000*sector+100*sub_sector+section;
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::CtbVolumeId(Int_t N, Int_t *Numbv) {
  Int_t VolumeId = 1000*Numbv[0]+100*Numbv[2]+Numbv[1];
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::EmcVolumeId(Int_t N, Int_t *Numbv) {// barrel calorimeter
  Int_t VolumeId = 0;
  //6*                                barrel calorimeter - K.Shester
  //          both left and right barrels:
  Int_t rileft = Numbv[0];
  Int_t phi    = Numbv[1];
  Int_t superl = Numbv[2];
  //
  Int_t eta=idigi(1)+1;
  Int_t phi_sub=idigi(2);
  if (rileft==1) {
    phi=60-phi+1;
    if (phi_sub==0) phi_sub=2;
    else {
      phi=60+phi;
      phi_sub=phi_sub+1;
    }

    if(rileft<1  ||  rileft>2) {            
      //	  print *,'**ERROR at g2t_VolumeId: emc rl ',rileft
      //	else if(eta<1  ||  eta>CALG_NetaT)  {                 
      //	  print *,'**ERROR at g2t_VolumeId: emc eta ',eta
      //	else if(phi<1  ||  phi>CALG_MaxModule)  {            
      //	  print *,'**ERROR at g2t_VolumeId: emc phi ',phi
      //	else if(superl<1  ||  superl>CALG_NSub) {            
      //	  print *,'**ERROR at g2t_VolumeId: emc superl ',superl
    }  else 
      VolumeId=10000000*rileft+100000*eta+100*phi+10*phi_sub+superl;
  }
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::SmdVolumeId(Int_t N, Int_t *Numbv) {
  Int_t VolumeId = 0;
  //else if (Csys=='smd') {
  //7*
  
  Int_t rileft   =Numbv[0];
  Int_t phi      =Numbv[1];
  Int_t forw_back=Numbv[2];
  
  Int_t eta  =idigi(2)+1;
  Int_t strip=idigi(3)+1;
  if (forw_back==4) forw_back=3;
  if (rileft==1)
    phi=60-phi+1;
  else
    phi=60+phi;
  if(rileft<1  ||  rileft>2)  {                        
    //	  print *,'**ERROR at g2t_VolumeId: smd rl ',rileft
    //	else if(eta<1  ||  eta>calg_NetaSMDp) {                  
    //	  print *,'**ERROR at g2t_VolumeId: smd eta ',eta
    //	else if(phi<1  ||  phi>CALG_MaxModule) {            
    //	  print *,'**ERROR at g2t_VolumeId: smd phi ',phi
  }  else if(forw_back<1  ||  forw_back>3) {            
    //    print *,'**ERROR at g2t_VolumeId: smd forw_back ',forw_back
  } else if(strip<1) {            
    // print *,'**ERROR at g2t_VolumeId: smd strip ',strip
  } else if(forw_back=1 && strip>calg_Netfirst) {            
    //    print *,'**ERROR at g2t_VolumeId: smd strip ',strip, forw_back
  } else if(forw_back=2 && strip>calg_Netsecon)  {            
    // print *,'**ERROR at g2t_VolumeId: smd strip ',strip, forw_back
  } else if(forw_back=3 && strip>calg_NPhistr) {            
    //  print *,'**ERROR at g2t_VolumeId: smd strip ',strip, forw_back
  } else {
    VolumeId=100000000*rileft+1000000*eta+1000*phi+100*forw_back+strip;
  }
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::EemVolumeId(Int_t N, Int_t *Numbv) {
  //	if (cd=='ESCI') {
  Int_t VolumeId = 0;
  //      else if (Csys=='eem') {
  //8*
  // PN, MAX:
  //      OnOff    = (0..3)  -  East-West config:  0-none,            1 - west,     2-east,   3-both
  //      FillMode = (1..3)  -  Sectors fill mode: 1- one 3rd filled, 2 - one half, 3 - full
  //
 
  static Int_t emcg_onoff = 3;
  Int_t rileft, shift;
  if (emcg_onoff < 3) {
    rileft    = emcg_onoff;
    shift     = 0;
  } else {
    rileft    = Numbv[0];
    shift     = 1;
  }
  // we may have versions later than 5, that's why this has been changed to >=
  static Int_t emcg_version = 5, emcg_FillMode = 3;
  Int_t iWheel;
  if (emcg_version >= 5) { // need to look at fillmode:
    if (emcg_FillMode <= 2 ) {
      iWheel = 1;
    } else {
      iWheel = Numbv[shift]; 
      shift  += 1;
    }
    static Int_t  sector_hash[2][6] = {
      { 4, 5, 6, 7, 8, 9}, 
      {10,11,12, 1, 2, 3}
    };
    Int_t section   = Numbv[shift];                        // ECVO
    Int_t phi_30d   = sector_hash[Numbv[shift+1]-1][iWheel-1];    // EMOD
    Int_t zsubsect  = Numbv[shift+2];                        // ESEC (no readout)
    //    Int_t zsublayer = Numbv[shift+3];                        // EMGT (no readout)
    Int_t phi       = Numbv[shift+4];                        // EPER (5 fingers)
    Int_t eta       = Numbv[shift+5];                        // ETAR (radial division)
    // we signal this once
    //   if(printOnce) {
    //  if (6+shift != nv) print *,' G2T_VOL_ID: new inconsistency in ECAL
    // printOnce=.false.
    // }
    Int_t eemc_depth = zsubsect + 3*(section-1);
    VolumeId = 100000*rileft + 1000*(5*(phi_30d-1)+phi) + 10*eta + eemc_depth;
  } else {// version other than 5
    shift     = 1;
    rileft    = Numbv[shift-1];
    Int_t phi_30d   = Numbv[shift];
    Int_t section   = Numbv[shift+1];
    Int_t phi       = Numbv[shift+2];
    Int_t eta       = Numbv[shift+4];
    
//             if(printOnce) {
//                if (5+shift != nv) print *,' G2T_VOL_ID: old inconsistency in ECAL'
//                printOnce=.false.
//             }

    VolumeId = 100000*rileft+1000*(5*(phi_30d-1)+phi)+10*eta+section;
  }
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::EsmVolumeId(Int_t N, Int_t *Numbv) {
  Int_t VolumeId = 0;
  //      else if (Csys=='esm') {
  //9* 
  static Int_t emcg_onoff = 3;
  Int_t rileft, shift;
  if (emcg_onoff < 3) {
    rileft    = emcg_onoff;
    shift     = 0;
  } else {
    rileft    = Numbv[0];
    shift     = 1;
  }
  // see comment above about >=
  static Int_t emcg_version = 5, emcg_FillMode = 3;
  Int_t iWheel;
  Int_t phi_30d, depth, strip, phi;
  if (emcg_version >= 5) {
    if (emcg_FillMode <= 2 ) {
      iWheel = 1;
    } else {
      iWheel = Numbv[shift]; 
      shift  += 1;
    }
    //    Int_t depth     = Numbv[shift];
    //         phi       = Numbv[shift+1]; 
    static Int_t  sector_hash[2][6] = {
      { 4, 5, 6, 7, 8, 9}, 
      {10,11,12, 1, 2, 3}
    };
    phi_30d   = sector_hash[Numbv[shift+1]-1][iWheel-1];
    strip     = Numbv[shift+2];
  } else {
    // version before 5
    rileft    = Numbv[0];
    depth     = Numbv[1];
    phi       = Numbv[2];
    strip     = Numbv[3]; 
  }       
  VolumeId = 1000000*rileft+10000*phi_30d+1000*depth+strip;
  return VolumeId;
} 
//________________________________________________________________________________
Int_t StarVMCDetector::FtpVolumeId(Int_t N, Int_t *Numbv) { 
  Int_t VolumeId = 0; 
  //10*
  // ftpv=1 for west, 2 for east part of the FTPC
  // ftpc_sector is the phi division of the gas layer
  // the numbering scheme below designed by Janet Seyboth,
  // but I'm adding the correct mapping between GEANT id's
  // and the ones found on:
  // http://wwwstar.mppmu.mpg.de/ftpc_calibration_page/calibration/calibration.html
  // I use a hash table for a unique and clean way to number sectors
  // --max--
  static Int_t ftpc_hash[2][6] =  {
    {1, 6, 5, 4, 3, 2}, 
    {6, 1, 2, 3, 4, 5}};
  Int_t ftpv       = Numbv[0];
  Int_t padrow     = Numbv[1];
  Int_t ftpc_sector= ftpc_hash[ftpv-1][Numbv[2]-1];
  VolumeId  = (100*ftpv+padrow)*10 + ftpc_sector;
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::VpdVolumeId(Int_t N, Int_t *Numbv) { 
  Int_t VolumeId = 0; 
  //      else if (Csys=='vpd') {
  //11*    Vertex position detector - Frank Geurts <geurts@rice.edu>
  static Int_t vpdg_version = 2;
  Int_t rileft, innout, sector;
  if (vpdg_version == 1) {
    rileft    = Numbv[0];
    innout    = Numbv[1];
    sector    = Numbv[2];
  } else {
    rileft    = Numbv[0];
    innout    = 0;
    sector    = Numbv[1];
  }
  VolumeId  =  1000*rileft + 100*innout + sector;
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::RchVolumeId(Int_t N, Int_t *Numbv) {
  Int_t VolumeId = 0;
  //13*
  //      else if (Csys=='rch') {
//         is=0
//         if       (cd=='RGAP') {is=1}
//         else if  (cd=='RCSI') {is=2} 
//         else if  (cd=='QUAR') {is=3} 
//         else if  (cd=='FREO') {is=4} 
//         else if  (cd=='OQUA') {is=5} 

//         VolumeId = Numbv[0] + Is*1000
  return VolumeId;
} 
//________________________________________________________________________________
Int_t StarVMCDetector::ZdcVolumeId(Int_t N, Int_t *Numbv) { 
  Int_t VolumeId = 0;
  //14*
  //      else if (Csys=='zdc') {
  VolumeId = Numbv[0]*1000+Numbv[1];
  return VolumeId; 
} 
//________________________________________________________________________________
Int_t StarVMCDetector::PmdVolumeId(Int_t N, Int_t *Numbv) { 
  return Numbv[0]*1000000 + Numbv[1]*100000 + Numbv[2]*10000 + Numbv[3]*100 + Numbv[4];
} 
//________________________________________________________________________________
Int_t StarVMCDetector::BbcVolumeId(Int_t N, Int_t *Numbv) { 
  //16*                          Mikhail Kopytine for the BBC group
  //  else if (Csys=='bbc') {
  //        
  //       BBC has 4 levels: west/east, annulus, triple module, single module
  return Numbv[0]*1000 + Numbv[1]*100 + Numbv[2]*10 + Numbv[3];    
}
//________________________________________________________________________________
Int_t StarVMCDetector::PixVolumeId(Int_t N, Int_t *Numbv) {
  //17*                                 Kai Schweda
  //      else if (Csys=='pix') {
  return Numbv[0]*1000000 + Numbv[1]*10000 + Numbv[2]*100  + Numbv[3];
}
//________________________________________________________________________________
Int_t StarVMCDetector::IstVolumeId(Int_t N, Int_t *Numbv) { 
  Int_t VolumeId = 0; 
  //18*                                 Maxim Potekhin
  //    else if (Csys=='ist') {
  static Int_t istVersion = 4;
  Int_t istLayer;
  if(istVersion != 3 && istVersion != 4) {
    istLayer=Numbv[0]+1;
    //            write(*,*) istVersion,'+_+_+_+_+_+_+_+_+_+',istLayer,' ',Numbv[1],' ',Numbv[2],' ',Numbv[3];
    VolumeId = istLayer*1000000 + Numbv[1]*10000 + 100*Numbv[2]  + Numbv[3];
  } else if(istVersion == 3) {
    istLayer=3;
    //            write(*,*) istVersion,'+_+_+_+_+_+_+_+_+_+',istLayer,' ',Numbv[0],' ',Numbv[1],' ',Numbv[2];
    VolumeId = istLayer*1000000 + Numbv[0]*10000 + 100*Numbv[1]  + Numbv[2];
  } else if(istVersion == 4) {
    istLayer=2;
    //            write(*,*) istVersion,'+_+_+_+_+_+_+_+_+_+',istLayer,' ',Numbv[0],' ',Numbv[1],' ',Numbv[2];
    VolumeId = istLayer*1000000 + Numbv[0]*10000 + 100*Numbv[1]  + Numbv[2];
  }
  return VolumeId; 
} 
//________________________________________________________________________________
Int_t StarVMCDetector::FstVolumeId(Int_t N, Int_t *Numbv) { 
  //19*                                 Kai Schweda
  //   else if (Csys=='fst') {
  return Numbv[0]*1000000 + Numbv[1]*10000 + Numbv[2]*100  + Numbv[3];
}
//________________________________________________________________________________
Int_t StarVMCDetector::FgtVolumeId(Int_t N, Int_t *Numbv) {
  //20*                                 Kai Schweda
  //  else if (Csys=='fgt') {
  return Numbv[0]*100 + Numbv[1];
}
//________________________________________________________________________________
Int_t StarVMCDetector::IgtVolumeId(Int_t N, Int_t *Numbv) {
  //21*                                 Gerrit van Nieuwenhuizen
  //      else if (Csys=='igt') {
  return Numbv[0]*1000000 + Numbv[1]*10000 + Numbv[2]*100  + Numbv[3];
}
//________________________________________________________________________________
Int_t StarVMCDetector::FpdVolumeId(Int_t N, Int_t *Numbv) {
  Int_t VolumeId = 0;
  //23*                                 Pibero Djawotho
  //      else if (Csys=='fpd') {
  static Int_t ifpdmgeo = 3;
  Int_t n1, sl, ew = 0, nstb = 0, n2, ch = 0;
  if (ifpdmgeo==3 ) {                                                    // FMS Geometry
    n1 = Numbv[0]; n2 = Numbv[1]; sl = -999;
    //    if(cd=='FLGR') sl=1;
    //    if(cd=='FLXF') sl=2; assert(sl > 0) // Wrong sensitive detector in FPD/FMS
    sl = 1;
    ew=(n1-1)/2+1;
    nstb = -999;
    if(n1 == 1) nstb=1; if(n1 == 2) nstb=2;
    if(n1 == 3  &&  sl == 2) nstb=1;
    if(n1 == 4  &&  sl == 2) nstb=2;
    if(n1 == 3  &&  sl == 1) nstb=3;
    if(n1 == 4  &&  sl == 1) nstb=4 ; assert(nstb > 0); // Wrong nstb in FPD/FMS
    ch=n2;
    if(ew == 1) {
      if(ch > 49  &&  ch <= 56) {
	ch=ch-49;
	nstb=nstb+4;
      }
      if(nstb == 1  ||  nstb == 5) {
	ch=ch + 6 - 2*(ch-1)%7;
      } else if(nstb == 3  ||  nstb == 4) {
	ch=ch + 4 - 2*(ch-1)%5;
      }
    } else if(ew == 2) {
      if(nstb <= 2) {  
	if(n2 >= 11   &&  n2 <= 21 )  ch=n2 +  7;
	if(n2 >= 22   &&  n2 <= 33 )  ch=n2 + 13;
	if(n2 >= 34   &&  n2 <= 46 )  ch=n2 + 18;
	if(n2 >= 47   &&  n2 <= 60 )  ch=n2 + 22;
	if(n2 >= 61   &&  n2 <= 75 )  ch=n2 + 25;
	if(n2 >= 76   &&  n2 <= 91 )  ch=n2 + 27;
	if(n2 >= 92   &&  n2 <= 125)  ch=n2 + 28;
	if(n2 >= 126  &&  n2 <= 269)  ch=n2 + 36 + 8*((n2-126)/9);
	if(n2 >= 270  &&  n2 <= 319)  ch=n2 +156;
	if(n2 >= 320  &&  n2 <= 334)  ch=n2 +157;
	if(n2 >= 335  &&  n2 <= 348)  ch=n2 +159;
	if(n2 >= 349  &&  n2 <= 361)  ch=n2 +162;
	if(n2 >= 362  &&  n2 <= 373)  ch=n2 +166;
	if(n2 >= 374  &&  n2 <= 384)  ch=n2 +171;
	if(n2 >= 385  &&  n2 <= 394)  ch=n2 +177;   
	//write(*,*) 'matrix check - Large cells FMS: ',nstb, ch,n2
      } else {
	if(n2 >=  85  &&  n2 <= 154)  ch=n2 +  5 + 5*((n2-85)/7);
	if(n2 >= 155  &&  n2 <= 238)  ch=n2 + 50;
	//write(*,*) 'matrix check - Small cells FMS: ',nstb, ch,n2
      }
    }
  }
  VolumeId=ew*10000+nstb*1000+ch;       
  return VolumeId; 
}
//________________________________________________________________________________
Int_t StarVMCDetector::FscVolumeId(Int_t N, Int_t *Numbv) {
  //24*                                 Dmitry Arkhipkin
  //  else if (Csys=='fsc') {
  return Numbv[0];
}
//________________________________________________________________________________
Int_t StarVMCDetector::MtdVolumeId(Int_t N, Int_t *Numbv) {
  Int_t VolumeId = 0;
//25*                                 Frank Geurts
//      else if (Csys=='mtd') {
  Int_t sector = Numbv[0];
  Int_t module = Numbv[1];
  Int_t layer  = Numbv[2];
  VolumeId = 1000*sector + 100*module + layer;
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::EtrVolumeId(Int_t N, Int_t *Numbv) {
  Int_t VolumeId = 0;
  // ******************************************************************************************
  // elseif (Csys=='etr') {
  Int_t sector = (Numbv[0]-1)% 12;  //  "Sectors count from 0 - 11"
  Int_t layer  = (Numbv[0]-1)/ 12;  //   "Layers  count from 0 - 2"
  Int_t section= Numbv[1] - 1;

  //  """ sector  from 0 - 11  (phi division)      """
  // """ layer   from 0 - 2   (z-division)        """
  // """ section from 0 - 29  (division of layer) """
  
  VolumeId = section + 100*layer + 10000*sector;

  //          <W> Numbv[0], Numbv[1], sector, layer, section, VolumeId; 
  //    ('Numbv =',I4,2X,I4,' sector=',I4,' layer=',I4,' section=',I4,' vid=', I7);
  return VolumeId;
}
//________________________________________________________________________________
Int_t StarVMCDetector::SvtVolumeId(Int_t N, Int_t *Numbv) {
  Int_t VolumeId = 0;
  return VolumeId;
}
      function g2t_VolumeId(Csys,Numbv)
          // Intialize TPADs based on TPC version 
          if (TPCG_tpadconfig==1  || 
             TPCG_tpadconfig==4  || 
             TPCG_tpadconfig==6 "Inner TPC upgrade 32 pads" {

             nbpads = 32 "inner" + 32 "outer" + 4 "edge/fake"
 
             tpads = { 1, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                      10,11,12,13,14,15,16,17,18,19,
                      20,21,22,23,24,25,26,27,28,29,
                      30,31,32,32,33,33,34,35,36,37,
                      38,39,40,41,42,43,44,45,46,47,
                      48,49,50,51,52,53,54,55,56,57,
                      58,59,60,61,62,63,64,64 };

              isdets = { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 2, 1, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 2 };

          } else {           

             nbpads = 73
             tpads = { 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 
                       4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 
                       7, 8, 8, 8, 9, 9, 9,10,10,10, 
                      11,11,11,12,12,12,13,13,13,14, 
                      14,15,16,17,18,19,20,21,22,23, 
                      24,25,26,27,28,29,30,31,32,33, 
                      34,35,36,37,38,39,40,41,42,43, 
                      44,45,45};

             isdets = { 1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
	                0, 2, 1, 0, 2, 1, 0, 2, 1, 0,
		        2, 1, 0, 2, 1, 0, 2, 1, 0, 2,
		        1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 2 };

          }



          // IFPDMGEO indicates which major version of the FPD/FMS module is present
          // IFPD    indicates which geometry version is in place (to allow changes
          //         w/in each major version.)
          if ( ifpd > 0 ) { 
                         ifpdmgeo=0      // fpdmgeo.g
                         ifpd   =fpdg_version
          } 
          if ( ifms > 0 ) { 
             ifms = fmcg_version
             if (ifms=6) ifpdmgeo=1      // fpdmgeo1.g
             if (ifms=7) ifpdmgeo=2      // fpdmgeo2.g
             if (ifms=8) ifpdmgeo=3      // fpdmgeo3.g                                       
          }
		  if (ifsc>=0) print *,' g2t_VolumeId: FSC version =',fscg_version

      }
//
      VolumeId = 0	
      g2t_VolumeId = VolumeId
      
      nv        = nvb(1)  // number of real volume levels in NUMBV
//
//  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      if (Csys=='svt') {
//1*                                          Ken Wilson
// Helen altered SVT volume IDs so agrees with hardware defs.
        if  (Cd=='SVTD') {
           lnumber    = Numbv[0];
           ladder     = Numbv[1];
           wafer      = Numbv[2];
        
           if ( ladder  ==  0) {
// This is the year 1 ladder
                nladder = 1
                wafer   = lnumber
                ladder  = 12
                lnumber = 4
// Set First barrel ids
           else if (lnumber <= 2) {
                nladder = 8
		nwafer  = 4
//               wafer   = 5-wafer 
// Set 2nd barrel ids
           else if (lnumber <= 4) {
                nladder  = 12
		nwafer   = 6	
//               wafer   = 7-wafer   
// Set 3rd barrel ids
           else if (lnumber <= 6) {
                nladder  = 16
		nwafer   = 7
//               wafer   = 8-wafer
           else
             print *,' G2T warning: layer number ',lnumber,
     >               '     in svt hits not found' 
           }

// PN: change geant numbering (CCW) to STAR numbering(CW):
           if (nladder>1) {
//             inner sub-layer - 0, outer - 1:
              lsub    = mod(lnumber-1,2)
              if (svtg_version==1) { 
//             OLD: 3 o'clock is geant's first and STAR N/4 element:
                ladder=nladder/4-(ladder-1)*2-lsub
                ladder=mod(nladder+ladder-1,nladder)+1
              else
//             NEW: 12 o'clock is geant's first and STAR last element:
                ladder=nladder-(ladder-1)*2-lsub
              }
           }
	if (ladder < 1 | ladder > nladder | wafer < 1 | wafer > nwafer) return
        VolumeId  = 1000*lnumber+100*wafer+ladder
        else if (Cd=='SFSD') {
           VolumeId =  7000+100*Numbv[1]+Numbv[0];
        }
        
      else if (Csys=='ssd') {
        VolumeId = 7000+100*Numbv[1]+Numbv[0];
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      else if (Csys=='tpc') {
//2*                                        Peter M. Jacobs
        tpgv  = Numbv[0];
        tpss  = Numbv[1];
        sector= tpss+12*(tpgv-1) 
        tpad  = Numbv[2];
        isdet = 0

        if  (tpcg_version==1) {
          if (cd=='TPAI')  isdet=1
          if (cd=='TPAO')  isdet=2
//PN:      outermost pseudopadrow:
          if (cd=='TPAO' & tpad==14) tpad=45
        else
!//		tpad >nbpads (73) prompt hits
          if (tpad  >  nbpads) tpad = tpad - nbpads
          isdet = isdets(tpad);
          tpad  = tpads (tpad);

        }

        VolumeId=100000*isdet+100*sector+tpad
//
      else if (Csys=='mwc') {
//3*
        rileft    = Numbv[0];
        sector    = Numbv[1]; 
        innout    = Numbv[2];
        innour    = Numbv[3];
        VolumeId = 1000*rileft+100*innout+10*innour+sector
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      else if (Csys=='tof') {
//4*                                             Frank Geurts
        if (btog_version==1) {
           rileft     = Numbv[0];
           sector     = Numbv[1];
           sub_sector = Numbv[2]; 
           innout     = Numbv[3];
           VolumeId  = 100000*rileft+1000*innout+10*sector+sub_sector   
        else if (btog_version==2) {
//          simulations done in 2000 - one tof tray on west side
           if (btog_choice==4) {
              rileft     = 1
              sector     = btog_posit1(1)
              sub_sector = Numbv[0]; 
              innout     = Numbv[1];
              VolumeId  = 100000*rileft+1000*innout+10*sector+sub_sector   
           else
              print *,' g2t_VolumeId : choice not coded yet '
           }        
        else if (btog_version>=3) {
//          For simulations after 28-sep-00, before it was version 2
           if (btog_choice==2) {      // Full TOF
             rileft     = Numbv[0];       //     west(1)/east(2)
             sector     = Numbv[1];       //     tray(1-60)
             innout     = Numbv[2];       //     4wide(1)/5wide(2) sections
             sub_sector = Numbv[3];       //     theta-tray(4w:1-4, 5w:1-5)
             section    = Numbv[4];       //     phi-tray(4w:1-9,5w:1)
           else if (btog_choice==3) {  // ~25% TOF (only on east side)
             rileft     = 2              //     east (pre-set)
             sector     = Numbv[0];       //     tray
             innout     = Numbv[1];       //     4wide/5wide section
             sub_sector = Numbv[2];       //     theta-tray
             section    = Numbv[3];       //     phi-tray
           else if (btog_choice<=7) {  //  TOFp (single tray)
             rileft     = 2              //     east (pre-set)
             if (btog_choice!=7) {    //
              sector    = btog_posit1(1) //     tray (pre-set)
             else                        //
              sector    = btog_posit1(2) //
             }                       //
             innout     = Numbv[0];       //     4wide/5wide section
             sub_sector = Numbv[1];       //     theta-tray
             section    = Numbv[2];       //     phi-tray
           else
             print *,' g2t_VolumeId: unknown TOF choice.'
             print *,' g2t_VolumeId: btog_choice=',btog_choice
           }

//  -------- sanity check ---------
           if ((rileft < 1) || (rileft >  2) || 
     +        (sector < 1) || (sector > 60) || 
     +        (innout < 1) || (innout >  2) || 
     +        (sub_sector < 1) || 
     +         ((innout==1) && (sub_sector > 4)) || 
     +         ((innout==2) && (sub_sector > 5)) || 
     +        (section < 1) || (section > 9) || 
     +         ((innout == 2) && (section != 1))) {
          print *,' g2t_VolumeId: TOF sanity check failed.',
     +              rileft,sector,innout,sub_sector,section
	  return
	}

//  -------- combine 4wide and 5wide sections ---------
          if (innout==1) {
             section = section+1  //  phi-tray (4wide-sections)
          }

//  -------- encode VolumeId ---------
          VolumeId = 100000*rileft+1000*sector+100*sub_sector+section
        else
          print *,' g2t_VolumeId : TOF version not coded yet'
          print *,' g2t_VolumeId : btog_version=',btog_version
        }

// ------- TOFr detector (single tray) --------------
      else if (Csys=='tfr') {   // TOFr
         if (btog_choice==5  ||  btog_choice==7) {      //  single tray
            rileft     = 2               //  east (pre-set)
            sector     = btog_posit2     //  tray (pre-set)
            module     = Numbv[0];        //  module (eta)
            layer      = Numbv[1];        //  layer (1-6, gap in module)
	 else if (btog_choice==8  ||  btog_choice==9  ||  btog_choice==10) {  //  single tray (different location)
           rileft     = 2               //  east (pre-set)
           sector     = btog_posit3     //  tray (pre-set)
           module     = Numbv[0];        //  module (eta)
           layer      = Numbv[1];        //  layer (1-6, gap in module)
         else if (btog_choice==11) {                     // Run 8
            rileft     = 2               //  east (pre-set)
            sector     = Numbv[0];        //  tray(1-60)
            module     = Numbv[1];        //  module (eta)
            layer      = Numbv[2];        //  layer (1-6, gap in module)
         else if (btog_choice==6  ||  btog_choice==12) { // full TOF or Run 9
            rileft     = Numbv[0];        //  west(1)/east(2)
            sector     = Numbv[1];        //  tray(1-60)
            module     = Numbv[2];        //  module (eta)
            layer      = Numbv[3];        //  layer (1-6, gap in module)
         else if (btog_choice==13) {  // full TOF with GMT trays (Run 13+)
            rileft     = Numbv[0];        //  west(1)/east(2)
            sector     = Numbv[1];        //  tray(1-60)
            module     = Numbv[2];        //  module (eta)
            layer      = Numbv[3];        //  layer (1-6, gap in module)
//           GMT replacement only affects trays 8 (W8), 23 (W23), 93 (E33), and 108 (E48)
            if ((rileft==1  &&  (sector== 8  ||  sector==23))  || 
     +          (rileft==2  &&  (sector==33  ||  sector==48))) { 
            module     = module + 4    // account for 4 modules that have replaced with GMT
            }
         else
            print *,' g2t_VolumeId: unknown TOFr choice.'
            print *,' g2t_VolumeId: btog_choice=',btog_choice
         }
         VolumeId = layer +10*(module +100*(sector+100*rileft) )

      else if (Csys=='ctb') {
//5*
        VolumeId = 1000*Numbv[0]+100*Numbv[2]+Numbv[1];
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//   ------------------  calorimetry  ---------------------

      else if (Csys=='emc') {
//6*                                barrel calorimeter - K.Shester
        if (CALG_Nmodule(1)*CALG_Nmodule(2)>0) {
//          both left and right barrels:
           rileft = Numbv[0];
           phi    = Numbv[1];
           superl = Numbv[2];
	else                   
//          only one barrel - left or write 
	   if(CALG_Nmodule(1)>0) {
              rileft=1
           else
              rileft=2
	   }
           phi    = Numbv[0];
           superl = Numbv[1];
        }
//
        eta=idigi(1)+1
        phi_sub=idigi(2)
        if (rileft==1) {
          phi=60-phi+1
          if (phi_sub==0) phi_sub=2
        else
          phi=60+phi
          phi_sub=phi_sub+1
        }

	if(rileft<1  ||  rileft>2) {            
	  print *,'**ERROR at g2t_VolumeId: emc rl ',rileft
//	else if(eta<1  ||  eta>CALG_NetaT)  {                 
//	  print *,'**ERROR at g2t_VolumeId: emc eta ',eta
//	else if(phi<1  ||  phi>CALG_MaxModule)  {            
//	  print *,'**ERROR at g2t_VolumeId: emc phi ',phi
//	else if(superl<1  ||  superl>CALG_NSub) {            
//	  print *,'**ERROR at g2t_VolumeId: emc superl ',superl
	else 
	  VolumeId=10000000*rileft+100000*eta+100*phi+
     +	              +10*phi_sub+superl
	}

      else if (Csys=='smd') {
//7*
        if (CALG_Nmodule(1)*CALG_Nmodule(2)>0) {
           rileft   =Numbv[0];
           phi      =Numbv[1];
           forw_back=Numbv[2];
        else
           if (CALG_Nmodule(1)>0) {
              rileft=1
           else
              rileft=2
           }
           phi      =Numbv[0];
           forw_back=Numbv[1];
        }

        eta  =idigi(2)+1
        strip=idigi(3)+1

        if (forw_back==4) forw_back=3
        if (rileft==1) {
          phi=60-phi+1
        else
          phi=60+phi
        }     

	if(rileft<1  ||  rileft>2)  {                        
	  print *,'**ERROR at g2t_VolumeId: smd rl ',rileft
//	else if(eta<1  ||  eta>calg_NetaSMDp) {                  
//	  print *,'**ERROR at g2t_VolumeId: smd eta ',eta
//	else if(phi<1  ||  phi>CALG_MaxModule) {            
//	  print *,'**ERROR at g2t_VolumeId: smd phi ',phi
	else if(forw_back<1  ||  forw_back>3) {            
	  print *,'**ERROR at g2t_VolumeId: smd forw_back ',forw_back
	else if(strip<1) {            
	  print *,'**ERROR at g2t_VolumeId: smd strip ',strip
	else if(forw_back=1 && strip>calg_Netfirst) {            
	  print *,'**ERROR at g2t_VolumeId: smd strip ',strip, forw_back
	else if(forw_back=2 && strip>calg_Netsecon)  {            
	  print *,'**ERROR at g2t_VolumeId: smd strip ',strip, forw_back
	else if(forw_back=3 && strip>calg_NPhistr) {            
	  print *,'**ERROR at g2t_VolumeId: smd strip ',strip, forw_back
	else 
          VolumeId=100000000*rileft+1000000*eta+1000*phi+
     +              100*forw_back+strip
	}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      else if (Csys=='eem') {
//8*
// PN, MAX:
//      OnOff    = (0..3)  -  East-West config:  0-none,            1 - west,     2-east,   3-both
//      FillMode = (1..3)  -  Sectors fill mode: 1- one 3rd filled, 2 - one half, 3 - full
//
 
	if (cd=='ESCI') {

          if (emcg_onoff < 3) {
            rileft    = emcg_onoff
            shift     = 0
          else
            rileft    = Numbv[0];
            shift     = 1
          }
// we may have versions later than 5, that's why this has been changed to >=
	  if (emcg_version >= 5) { // need to look at fillmode:
            if (emcg_FillMode <= 2 ) {
                iWheel = 1
            else
                iWheel = Numbv[shift]; 
                shift  += 1 
            }
      static Int_t           sector_hash[2][6] = {
      { 4, 5, 6, 7, 8, 9}, 
      {10,11,12, 1, 2, 3}};

            section   = Numbv[shift];                        // ECVO
            phi_30d   = sector_hash[Numbv[shift+1]-1][iWheel-1]    // EMOD
            zsubsect  = Numbv[shift+2]                        // ESEC (no readout)
            zsublayer = Numbv[shift+3]                        // EMGT (no readout)
            phi       = Numbv[shift+4]                        // EPER (5 fingers)
            eta       = Numbv[shift+5]                        // ETAR (radial division)

// we signal this once
            if(printOnce) {
	      //               if (6+shift != nv) print *,' G2T_VOL_ID: new inconsistency in ECAL
               printOnce=.false.
            }

            eemc_depth = zsubsect + 3*(section-1)

            VolumeId = 100000*rileft + 1000*(5*(phi_30d-1)+phi) + 10*eta + eemc_depth
	  else // version other than 5
            shift     = 1
            rileft    = Numbv[shift-1]
            phi_30d   = Numbv[shift];
            section   = Numbv[shift+1];
            phi       = Numbv[shift+2]
            eta       = Numbv[shift+4]

            if(printOnce) {
               if (5+shift != nv) print *,' G2T_VOL_ID: old inconsistency in ECAL'
               printOnce=.false.
            }

            VolumeId = 100000*rileft+1000*(5*(phi_30d-1)+phi)+10*eta+section
          }

	} // cd==ESCI

		  
      else if (Csys=='esm') {
//9* 
          if (emcg_onoff < 3) {
            rileft    = emcg_onoff
            shift     = 0
          else
            rileft    = Numbv[0];
            shift     = 1
          }
// see comment above about >=
	  if (emcg_version >= 5) {

            if (emcg_FillMode <= 2 ) {
                iWheel = 1
            else
                iWheel = Numbv[shift]; 
                shift  += 1 
            }

          depth     = Numbv[shift];
//         phi       = Numbv[shift+1]; 
          phi_30d   = sector_hash[Numbv[shift+1]-1][iWheel-1]
          strip     = Numbv[shift+2] 

        else

// version before 5
          rileft    = Numbv[0];
          depth     = Numbv[1];
          phi       = Numbv[2];
          strip     = Numbv[3]; 

	}       
	VolumeId = 1000000*rileft+10000*phi_30d+1000*depth+strip
 
//   ------------------ forward region ---------------------

      else if (Csys=='ftp') {
//10*
// ftpv=1 for west, 2 for east part of the FTPC
// ftpc_sector is the phi division of the gas layer
// the numbering scheme below designed by Janet Seyboth,
// but I'm adding the correct mapping between GEANT id's
// and the ones found on:
// http://wwwstar.mppmu.mpg.de/ftpc_calibration_page/calibration/calibration.html
// I use a hash table for a unique and clean way to number sectors
// --max--
      static Int_t           ftpc_hash[2][6] =  {
      {1, 6, 5, 4, 3, 2}, 
      {6, 1, 2, 3, 4, 5}};

        ftpv       = Numbv[0];
        padrow     = Numbv[1];
	ftpc_sector= ftpc_hash[ftpv-1][Numbv[2]-1];
        VolumeId  = (100*ftpv+padrow)*10 + ftpc_sector
//   ---------------------
      else if (Csys=='vpd') {
//11*    Vertex position detector - Frank Geurts <geurts@rice.edu>

        if (vpdg_version == 1) {
          rileft    = Numbv[0];
          innout    = Numbv[1];
          sector    = Numbv[2];
        else
          rileft    = Numbv[0];
          innout    = 0
          sector    = Numbv[1];
        }
        VolumeId  =  1000*rileft + 100*innout + sector
//
      else if (Csys=='pgc') {
//12*
      else if (Csys=='psc') {
//13*
      else if (Csys=='rch') {
        is=0
        if       (cd=='RGAP') {is=1}
        else if  (cd=='RCSI') {is=2} 
        else if  (cd=='QUAR') {is=3} 
        else if  (cd=='FREO') {is=4} 
        else if  (cd=='OQUA') {is=5} 

        VolumeId = Numbv[0] + Is*1000
//14*
      else if (Csys=='zdc') {
        VolumeId = Numbv[0]*1000+Numbv[1];

//15*                                 pmd,     Bedanga
      else if (Csys=='pmd') {
        VolumeId = Numbv[0]*1000000 + Numbv[1]*100000 + Numbv[2]*10000 _
                                     + Numbv[3]*100    + Numbv[4];

//16*                          Mikhail Kopytine for the BBC group
      else if (Csys=='bbc') {
//        
//       BBC has 4 levels: west/east, annulus, triple module, single module
        VolumeId = Numbv[0]*1000 + Numbv[1]*100 + Numbv[2]*10 + Numbv[3];    
//17*                                 Kai Schweda
      else if (Csys=='pix') {
        VolumeId = Numbv[0]*1000000 + Numbv[1]*10000 + Numbv[2]*100  + Numbv[3];

//18*                                 Maxim Potekhin
      else if (Csys=='ist') {
        if(istVersion != 3 && istVersion != 4) {
            istLayer=Numbv[0]+1
//            write(*,*) istVersion,'+_+_+_+_+_+_+_+_+_+',istLayer,' ',Numbv[1],' ',Numbv[2],' ',Numbv[3];
            VolumeId = istLayer*1000000 + Numbv[1]*10000 + 100*Numbv[2]  + Numbv[3];
        }
        if(istVersion == 3) {
            istLayer=3
//            write(*,*) istVersion,'+_+_+_+_+_+_+_+_+_+',istLayer,' ',Numbv[0],' ',Numbv[1],' ',Numbv[2];
            VolumeId = istLayer*1000000 + Numbv[0]*10000 + 100*Numbv[1]  + Numbv[2];
        }
        if(istVersion == 4) {
            istLayer=2
//            write(*,*) istVersion,'+_+_+_+_+_+_+_+_+_+',istLayer,' ',Numbv[0],' ',Numbv[1],' ',Numbv[2];
            VolumeId = istLayer*1000000 + Numbv[0]*10000 + 100*Numbv[1]  + Numbv[2];
        }
//19*                                 Kai Schweda
      else if (Csys=='fst') {
        VolumeId = Numbv[0]*1000000 + Numbv[1]*10000 + Numbv[2]*100  + Numbv[3];
//20*                                 Kai Schweda
      else if (Csys=='fgt') {
        VolumeId = Numbv[0]*100 + Numbv[1];

//21*                                 Gerrit van Nieuwenhuizen
      else if (Csys=='igt') {
        VolumeId = Numbv[0]*1000000 + Numbv[1]*10000 + Numbv[2]*100  + Numbv[3];
//22*                                 Sevil Salur
      else if (Csys=='hpd') {
        VolumeId = Numbv[0]*1000000 + Numbv[1]*10000 + Numbv[2]*100  + Numbv[3];
//23*                                 Pibero Djawotho
      else if (Csys=='fpd') {

        if (ifpdmgeo==3 ) {                                                    // FMS Geometry
           n1 = Numbv[0]; n2 = Numbv[1]; sl = -999
	   if(cd=='FLGR') sl=1
           if(cd=='FLXF') sl=2; assert(sl > 0) // Wrong sensitive detector in FPD/FMS
           ew=(n1-1)/2+1
           nstb = -999
	   if(n1 == 1) nstb=1; if(n1 == 2) nstb=2
	   if(n1 == 3  &&  sl == 2) nstb=1
	   if(n1 == 4  &&  sl == 2) nstb=2
	   if(n1 == 3  &&  sl == 1) nstb=3
	   if(n1 == 4  &&  sl == 1) nstb=4 ; assert(nstb > 0) // Wrong nstb in FPD/FMS
           ch=n2
           if(ew == 1) {
           if(ch > 49  &&  ch <= 56) {
              ch=ch-49
              nstb=nstb+4
           }
           if(nstb == 1  ||  nstb == 5) {
              ch=ch + 6 - 2*mod(ch-1,7)
           else if(nstb == 3  ||  nstb == 4) {
              ch=ch + 4 - 2*mod(ch-1,5)
           }
           else if(ew == 2) {
	    if(nstb <= 2) {  
	    if(n2 >= 11   &&  n2 <= 21 )  ch=n2 +  7
	    if(n2 >= 22   &&  n2 <= 33 )  ch=n2 + 13
	    if(n2 >= 34   &&  n2 <= 46 )  ch=n2 + 18
	    if(n2 >= 47   &&  n2 <= 60 )  ch=n2 + 22
	    if(n2 >= 61   &&  n2 <= 75 )  ch=n2 + 25
	    if(n2 >= 76   &&  n2 <= 91 )  ch=n2 + 27
	    if(n2 >= 92   &&  n2 <= 125)  ch=n2 + 28
	    if(n2 >= 126  &&  n2 <= 269)  ch=n2 + 36 + 8*((n2-126)/9)
	    if(n2 >= 270  &&  n2 <= 319)  ch=n2 +156
	    if(n2 >= 320  &&  n2 <= 334)  ch=n2 +157
	    if(n2 >= 335  &&  n2 <= 348)  ch=n2 +159
	    if(n2 >= 349  &&  n2 <= 361)  ch=n2 +162
	    if(n2 >= 362  &&  n2 <= 373)  ch=n2 +166
	    if(n2 >= 374  &&  n2 <= 384)  ch=n2 +171
	    if(n2 >= 385  &&  n2 <= 394)  ch=n2 +177   
            //write(*,*) 'matrix check - Large cells FMS: ',nstb, ch,n2
	    else
	    if(n2 >=  85  &&  n2 <= 154)  ch=n2 +  5 + 5*((n2-85)/7)
	    if(n2 >= 155  &&  n2 <= 238)  ch=n2 + 50 
            //write(*,*) 'matrix check - Small cells FMS: ',nstb, ch,n2
            }
            }
            VolumeId=ew*10000+nstb*1000+ch       
        }
//24*                                 Dmitry Arkhipkin
      else if (Csys=='fsc') {
        VolumeId = Numbv[0];

//25*                                 Frank Geurts
      else if (Csys=='mtd') {
        sector = Numbv[0];
        module = Numbv[1];
        layer  = Numbv[2];
        VolumeId = 1000*sector + 100*module + layer

// ******************************************************************************************
       elseif (Csys=='etr') {

          sector = MOD( (Numbv[0]-1), 12 );   "Sectors count from 0 - 11"
          layer  =      (Numbv[0]-1)/ 12;     "Layers  count from 0 - 2"
          section= Numbv[1] - 1

          """ sector  from 0 - 11  (phi division)      """
          """ layer   from 0 - 2   (z-division)        """
          """ section from 0 - 29  (division of layer) """

          VolumeId = section + 100*layer + 10000*sector

          <W> Numbv[0], Numbv[1], sector, layer, section, VolumeId; 
    ('Numbv =',I4,2X,I4,' sector=',I4,' layer=',I4,' section=',I4,' vid=', I7);


// *******************************************************************************************
      

      else
          print *,' G2T warning: volume  ',Csys,'  not found '  
      }
    g2t_VolumeId = VolumeId


    end
      
#endif
