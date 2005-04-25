#include <assert.h>
#include <stdio.h>
#include "Stiostream.h"
#include "StarVMCApplication.h"
#include "StarMCHits.h"
#include "TGeoManager.h"
#include "TCL.h"
#include "TDataSetIter.h"
#include "TPDGCode.h"
#include "TGeant3.h"
#include "TArrayI.h"
#include "TObjArray.h"
#include "TObjString.h"
#include "TClass.h"
#include "TROOT.h"
#ifdef __ROOT__1
#include "StMaker.h"
#endif
#include "tables/St_g2t_ctf_hit_Table.h"
#include "tables/St_g2t_emc_hit_Table.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_fst_hit_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"
#include "tables/St_g2t_ist_hit_Table.h"
#include "tables/St_g2t_mwc_hit_Table.h"
#include "tables/St_g2t_pix_hit_Table.h"
#include "tables/St_g2t_pmd_hit_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#include "tables/St_g2t_rch_hit_Table.h"
#include "tables/St_g2t_ssd_hit_Table.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_vpd_hit_Table.h"
#include "tables/St_particle_Table.h"

StarMCHits *StarMCHits::fgInstance = 0;
TGeant3    *StarMCHits::fgGeant3 = 0;
ClassImp(StarMCHits);
enum EHITtypes{kX=1,  kY,  kZ,   kR,    kRR,   kPHI,  kTHET, kETA,  kTDR,  kCP,
               kU,    kV,  kW,   kETOT, kELOS, kBIRK, kSTEP, kLGAM, kTOF,  kUSER,
	       kXX,   kYY, kZZ,  kPX,   kPY,   kPZ,   kSLEN, kPTOT, kLPTO, krese};
enum EDetectorTypes {
  kBPOL, kBCSB, kBRSG, kBXSA, kCSDA, kCSUP, kEHMS, kELGR, kEPCT, kESCI,
  kEXSE, kFDSW, kFREO, kFSEC, kIBSS, kPDGS, kPLAC, kQSCI, kQUAR, kRCSI,
  kRGAP, kSFSD, kSVTD, kTMSE, kTPAD, kVRAD, kALL };
struct Detector_G2T_t {
  EDetectorTypes kTYpe;
  Char_t        *Name;
  Char_t        *G2T_type;
  Char_t        *G2T_name;
  Char_t        *G2T_sys;
  Char_t        *G2T_geom;
};
/*
  g2t_event
  particle
  g2t_vertex
  g2t_track
  g2t_event
  g2t_vertex
  g2t_track
  g2t_pythia
  g2t_svt_hit SVTD SFSD	      
  g2t_pix_hit PLAC		      
  g2t_ist_hit IBSS		      
  g2t_fst_hit FDSW		      
  g2t_tpc_hit TPAD		      
  g2t_mwc_hit TMSE		      
  g2t_ftp_hit FSEC		      
  g2t_ctb_hit BXSA		      
  g2t_tof_hit BCSB		      
  g2t_tfr_hit BRSG		      
  g2t_rch_hit RGAP RCSI FREO QUAR 
  g2t_emc_hit CSUP		      
  g2t_smd_hit CSDA		      
  g2t_eem_hit ESCI ELGR EPCT      
  g2t_esm_hit EXSE EHMS	      
  g2t_vpd_hit VRAD		      
  g2t_pmd_hit PDGS		      
  g2t_zdc_hit QSCI		      
  g2t_bbc_hit BPOL                
*/
static const Detector_G2T_t g2t[kALL] = {
  //              type(cd)      name          sys    geom version
  { kBPOL,"BPOL","g2t_ctf_hit","g2t_bbc_hit","BBCM",""}, //
  { kBCSB,"BCSB","g2t_ctf_hit","g2t_tof_hit","BTOF","btof_btog"}, //
  { kBRSG,"BRSG","g2t_ctf_hit","g2t_tfr_hit","BTOF","btof_btog"}, //
  { kBXSA,"BXSA","g2t_ctf_hit","g2t_ctb_hit","BTOF","btof_btog"}, //
  { kCSDA,"CSDA","g2t_emc_hit","g2t_smd_hit","CALB",""}, 
  { kCSUP,"CSUP","g2t_emc_hit","g2t_emc_hit","CALB",""}, 
  { kEHMS,"EHMS","g2t_emc_hit","g2t_esm_hit","ECAL","ecal_emcg"}, 
  { kELGR,"ELGR","g2t_emc_hit","g2t_eem_hit","ECAL",""}, 
  { kEPCT,"EPCT","g2t_emc_hit","g2t_eem_hit","ECAL",""}, 
  { kESCI,"ESCI","g2t_emc_hit","g2t_eem_hit","ECAL","ecal_emcg"}, 
  { kEXSE,"EXSE","g2t_emc_hit","g2t_esm_hit","ECAL",""}, 
  { kFDSW,"FDSW","g2t_fst_hit","g2t_fst_hit","FSTD",""},
  { kFREO,"FREO","g2t_rch_hit","g2t_rch_hit","RICH",""}, 
  { kFSEC,"FSEC","g2t_ftp_hit","g2t_ftp_hit","FTPC","ftpc_ftpg"}, 
  { kIBSS,"IBSS","g2t_ist_hit","g2t_ist_hit","ISTB",""}, 
  { kPDGS,"PDGS","g2t_pmd_hit","g2t_pmd_hit","PHMD",""}, 
  { kPLAC,"PLAC","g2t_pix_hit","g2t_pix_hit","PIXL",""}, 
  { kQSCI,"QSCI","g2t_emc_hit","g2t_zdc_hit","ZCAL",""}, 
  { kQUAR,"QUAR","g2t_rch_hit","g2t_rch_hit","RICH",""}, 
  { kRCSI,"RCSI","g2t_rch_hit","g2t_rch_hit","RICH",""}, 
  { kRGAP,"RGAP","g2t_rch_hit","g2t_rch_hit","RICH",""}, 
  { kSFSD,"SFSD","g2t_svt_hit","g2t_svt_hit","SISD",""}, //g2t_ssd_hit
  { kSVTD,"SVTD","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg"}, //
  { kTMSE,"TMSE","g2t_mwc_hit","g2t_mwc_hit","TPCE",""}, 
  { kTPAD,"TPAD","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg"}, //
  { kVRAD,"VRAD","g2t_vpd_hit","g2t_vpd_hit","VPDD","vpdd_vpdg"}
};
//________________________________________________________________________________
StarMCHits::StarMCHits(const Char_t *name,const Char_t *title) : 
  TDataSet(name,title),  fDetectors(0),fGeoData(0),  fDetList(0), 
  fVolUserInfo(0), fCurrentDetector(0) 
{ 
  fgInstance = this; fHitHolder = this; 
  if (! fgGeant3) {
    fgGeant3 = (TGeant3 *) gMC; 
    assert(fgGeant3);
  }
}
//________________________________________________________________________________
Int_t StarMCHits::Init() {
  cout << "StarMCHits::Init() -I- Get Detectors" <<endl;
  if (! fDetectors ) delete fDetectors;
  fDetectors = 0;
#ifdef __ROOT__1
  if (! StMaker::GetChain()) {
#endif
    cout << "StarMCHits::Init() -I- There is no chain. Get Detectors for y2005x" <<endl;
    TFile *f = new TFile("$STAR/StarDb/VmcGeometry/Detectors.y2005x.root");
    assert(f);
    fDetectors = (TDataSet *) f->Get("Detectors");
    delete f;
    f = new TFile("$STAR/StarDb/VmcGeometry/geom.y2005x.root");
    assert(f);
    fGeoData = (TDataSet *) f->Get("geom");
    delete f;
    
#ifdef __ROOT__1
  } else {
    fDetectors = StMaker::GetChain()->GetDataBase("VmcGeometry/Detectors");
    fGeoData   = StMaker::GetChain()->GetDataBase("VmcGeometry/geom");
 }
#endif
  assert(fDetectors);
  // Make list of detector elements
  TDataSetIter next( fDetectors , 99);
  TDataSet *set = 0;
  if (fDetList) delete fDetList;
  fDetList = new THashList(100,0);
  Int_t N = 0;
  while ((set = next())) {
    if (TString(set->GetName()) == "Path") {
      TDataSet *parent = set->GetParent();
      TDataSet *hit = 0;
      TDataSet *user = 0;
      TString VolName("");
      if (parent) {
	hit = parent->Find("Hit");
	user = parent->Find("User");
      }
      St_det_path* path = (St_det_path*) set;
      StHitDescriptor *det = new StHitDescriptor(parent,(St_det_user *) user, path, (St_det_hit *) hit);
      Int_t N = path->GetNRows();
      det_path_st *Path = path->GetTable() + N - 1;
      VolName = Path->VName;
      for (Int_t k = 0; k < kAll; k++) {
	if (VolName == TString(g2t[k].Name)) {
	  det->SetTitle(g2t[k].G2T_sys);
	  if (TString(g2t[k].G2T_geom) != "") {
	    TTable *table = ( TTable * ) fGeoData->Find(g2t[k].G2T_geom);
	  }
	  break;
	}
      }
      fDetList->Add(det);
      N++;
    }
  }
  fDetList->Rehash(N);
  cout << "StarMCHits::Init() -I- Get Volume Info" << endl;
  //  TObjArray *UniqueVolumes = gGeoManager->GetListOfUVolumes();
  TObjArray *Volumes = gGeoManager->GetListOfUVolumes();
  Int_t Nids = Volumes->GetEntriesFast();
  if (! fVolUserInfo ) fVolUserInfo = new TObjArray(256);
  for (Int_t i = 0; i < Nids; i++) {
    TGeoVolume *vol  = (TGeoVolume *) Volumes->At(i);
    if (! vol) continue;
    Int_t uid = vol->GetNumber();
#if 0
    cout << "Volume:\t" << i << "\t" << vol->GetName() << "\t" << vol->GetTitle() << "\t" << uid;
#endif
    TString title(vol->GetTitle());
    if (title == "" || title == "Top volume") title =  vol->GetName();
    TObject *det = fDetList->FindObject(title.Data());
#if 0
    if (det) cout << "\tDetector: " << det->GetName();// << "\t" << det->GetTitle();
#endif
    fVolUserInfo->AddAtAndExpand(det,uid);
#if 0
    cout << endl;
#endif
  }
  return 0;
}
//________________________________________________________________________________
Float_t  StarMCHits::GetHitK(Int_t k) {
  /* A g G H I T
   *                                                                    *
   *  Description:  general hit coding for any standard detector        *
   */
 Float_t hit = 0;
#if 0
 Float_t da, dk, r, the, v[3], a[2], c[2];
 Int_t i = 0;
 Double_t d;
 if (k >= kX && k <= krese) {
   switch (k) {
   case kX: case kY: case kZ:
     hit=fHit.xloc[k-1];                      
     break;
   case kR:         
     hit=TMath::Sqrt(fHit.xloc[0]*fHit.xloc[0]+fHit.xloc[1]*fHit.xloc[1]);
     break;
   case kRR:        
     hit=TMath::Sqrt(fHit.xloc[0]*fHit.xloc[0]+fHit.xloc[1]*fHit.xloc[1]+fHit.xloc[2]*fHit.xloc[2]);
     break;
   case kPHI:   
     if (fHit.xloc[0] && fHit.xloc[1]) hit = TMath::ATan2(fHit.xloc[1],fHit.xloc[0]);
     break;
   case kTHET:      
     r = TMath::Sqrt(fHit.xloc[0]*fHit.xloc[0]+fHit.xloc[1]*fHit.xloc[1]+fHit.xloc[2]*fHit.xloc[2]);
     if (r>0) hit = TMath::ACos(fHit.xloc[2]/r);              
     break;
   case kETA:
     for (i = 0; i < 3; i++) v[i] = (fHit.vect[i] + fHit.vect0[i])/2; 
     r = TMath::Sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);
     if (r > 0) {
       the = TMath::ACos(v[2]/r);
       hit = - TMath::Log(the/2);
     }
     break;
   case kTDR: 
     TCL::vsub(fHit.vloc,fHit.vloc0,a,2);
     da=TCL::vdot(a,a,2);  
     dk=-1;  
     if (da>0) dk=-TCL::vdot(a,fHit.vloc0,2)/da;
     if (0 <= dk && dk <= 1) {//point is insided, fHit.vec=fHit.vloc+a*dk 
       TCL::vlinco(fHit.vloc0,1.,a,dk,fHit.vect,2); 
       hit =  TMath::Sqrt(TCL::vdot(fHit.vect,fHit.vect,2));
     } else {
       hit = TMath::Sqrt(TMath::Min(TCL::vdot(fHit.vloc0,fHit.vloc0,2),TCL::vdot(fHit.vloc,fHit.vloc,2)));
     };              
     break;
   case kCP:        // vector a=fHit.vloc-fHit.vloc0
     TCL::vsub(fHit.vloc,fHit.vloc0,a,2);
     TCL::vlinco(fHit.vloc0,-1.,a,-0.5,c,2); 
     hit=TCL::vdot(a,c,2)/TMath::Sqrt(TCL::vdot(a,a,2)*TCL::vdot(c,c,2));
     break;
   case kU: case kV: case kW: 
     i=k-10+3-1;   hit=fHit.xloc[i];                   
     break;
   case kETOT:
     hit=fHit.getot; 
     break;
   case kELOS:      
     hit=fHit.destep; 
     break;
   case kBIRK:      
     hit=fHit.destep; //? Call GBIRK(hit);
     break;
   case kSTEP:      
     hit=fHit.step;
     break;
   case kLGAM:     
     if (fHit.amass > 0 && fHit.gekin > 0 && fHit.charge != 0) hit = TMath::Log10(fHit.gekin/fHit.amass);
     else {
       if (fHit.amass<=0) hit = -999;
       else {
	 if (fHit.gekin <=0) hit = -998;
	 else           hit = -997;
       }
     }
     break;
   case kTOF:       
     hit=fHit.tofg;
     break;
   case kUSER:      
     hit=fHit.destep;
     break;
   case kXX: case kYY: case kZZ:  
     i=k-20-1; hit=(fHit.vect[i]+fHit.vect0[i])/2;  
     break;
   case kPX: case kPY: case kPZ:  
     i=k-20-1; hit=(fHit.vect[i]*fHit.vect[6]+fHit.vect0[i]*fHit.vect0[6])/2;    
     break;
   case kSLEN:      
     hit= fHit.sleng;
     break;
   case kPTOT:      
     hit= (fHit.vect[6]+fHit.vect0[6])/2;
     break;
   case kLPTO:      
     d = (fHit.vect[6]+fHit.vect0[6])/2;
     hit= TMath::Log10(TMath::Max(d,.1e-9));
     break;
   case krese:      
   default:
     hit= 0;
     break;
   }
 }
#endif
 return hit;
}
//________________________________________________________________________________
void StarMCHits::Step() {
  //  static Int_t Idevt0 = -1;
  static Double_t Gold = 0;
  static St_det_user *user = 0;
  static St_det_path *path = 0;
  static St_det_hit  *hit  = 0;

  //  cout << "Call StarMCHits::Step" << endl;
  TGeoNode *nodeT = gGeoManager->GetCurrentNode();
  assert(nodeT);
  TGeoVolume *volT = nodeT->GetVolume();
  assert(volT);
  fCurrentDetector = (StHitDescriptor *) fVolUserInfo->At(volT->GetNumber());
  const TGeoMedium   *med = volT->GetMedium(); 
  /*   fParams[0] = isvol;
       fParams[1] = ifield;
       fParams[2] = fieldm;
       fParams[3] = tmaxfd;
       fParams[4] = stemax;
       fParams[5] = deemax;
       fParams[6] = epsil;
       fParams[7] = stmin; */
  Int_t Isvol = (Int_t) med->GetParam(0);
  if (Isvol <= 0 && ! fCurrentDetector) return;
  if (Isvol && ! fCurrentDetector) {
    cout << "Active medium:" << med->GetName() << "\t for volume " << volT->GetName() 
	 << " has no detector description" << endl;
    return;
  }
  if (Isvol <= 0 &&  fCurrentDetector) {
    cout << "Dead medium:" << med->GetName() << "\t for volume " << volT->GetName() 
	 << " has detector description" << endl;
    return;
  }
  //  Int_t Idevt =  fgGeant3->CurrentEvent();
  fgGeant3->TrackPosition(fHit.Current.Global.xyzT);
  fgGeant3->TrackMomentum(fHit.Current.Global.pxyzE);
  TGeoHMatrix  *matrixC = gGeoManager->GetCurrentMatrix();
  fHit.Current.Global2Local(matrixC);
  if (fgGeant3->IsTrackEntering()) {
    user = fCurrentDetector->GetUserDesc(); det_user_st *User = user->GetTable();
    fHit.Detector= fCurrentDetector;
    fHit.IdType  = User->IdType;
    fHit.Serial  = User->Serial;
    fHit.Goption = User->Goption;
    fHit.Nva     = User->Nva;
    fHit.Nvb     = User->Nvb;
    path = fCurrentDetector->GetPathDesc();
    hit  = fCurrentDetector->GetHitDesc();
    fHit.Entry = fHit.Current;
    fHit.Sleng = fgGeant3->TrackLength();
    fHit.Charge = (Int_t) fgGeant3->TrackCharge();
    fHit.Mass = fgGeant3->TrackMass();
    fHit.AdEstep = fHit.AStep = 0;
    return;
  }
  Double_t GeKin = fHit.Current.Global.pxyzE.E() - fHit.Mass;
  fHit.Sleng = fgGeant3->TrackLength();
  if (fHit.Sleng == 0.) Gold = GeKin;
  Double_t dEstep = fgGeant3->Edep();
  Double_t Step = fgGeant3->TrackStep();
  fHit.iPart = fgGeant3->TrackPid();
  fHit.iTrack = StarVMCApplication::Instance()->GetStack()->GetCurrentTrackNumber();
  // - - - - - - - - - - - - - energy correction - - - - - - - - - -
  if (fgGeant3->IsTrackStop() && TMath::Abs(fHit.iPart) == kElectron) {
    TArrayI proc;
    Int_t Nproc = fgGeant3->StepProcesses(proc);
    Int_t Mec = 0;
    for (Int_t i = 0; i < Nproc; i++) if (proc[i] == kPAnnihilation || proc[i] == kPStop) Mec = proc[i];
    Int_t Ngkine = fgGeant3->NSecondaries();
    if (fHit.iPart == kElectron && Ngkine == 0 && Mec == kPStop) dEstep = Gold;
    else {
      if (fHit.iPart == kPositron && Ngkine < 2 && Mec == kPAnnihilation) {
	dEstep = Gold + 2*fHit.Mass;
	if (Ngkine == 1) {
	  TLorentzVector x;
	  TLorentzVector p;
	  Int_t IpartSec;
	  fgGeant3->GetSecondary(0,IpartSec,x,p);
	  dEstep -= p.E();
	}
      }
    }
  }
  // - - - - - - - - - - - - - - - - user - - - - - - - - - - - - - - -
  // user step
  // - - - - - - - - - - - - - - - sensitive - - - - - - - - - - - - -
  fHit.AdEstep += dEstep;  
  fHit.AStep   += Step;
  if (fHit.Charge == 0 && fHit.AdEstep == 0) return;
  if (! fgGeant3->IsTrackExiting() && ! fgGeant3->IsTrackStop()) return;
  fHit.Exit     = fHit.Current;
  fHit.Middle   = fHit.Entry;
  fHit.Middle  += fHit.Exit;
  fHit.Middle  *= 0.5;
  TString thePath(gGeoManager->GetPath());
  static const TString separator("/_");
  TObjArray *array = thePath.Tokenize(separator);
  Int_t N = array->GetEntriesFast();
  path = fCurrentDetector->GetPathDesc();
  Int_t NL = path->GetNRows();
  assert(N == 2*NL);
  Int_t i, j;
  TObjString *objs;
  fHit.NVL = 0;
  memset (fHit.NUMBV, 0, sizeof(fHit.NUMBV));
  det_path_st *Path = path->GetTable();// path->Print(0,NL);
  cout << "Path: " << thePath;
  for (i = j = 0; i < NL; i++, j += 2, Path++) {
    objs = (TObjString *) array->At(j);
    assert(objs->GetString().BeginsWith(TString(Path->VName,3)));
    if (Path->Ncopy != 1) {
      objs = (TObjString *) array->At(j+1);
      fHit.NUMBV[fHit.NVL] = atoi(objs->GetString().Data());
      cout << "\t" << fHit.NUMBV[fHit.NVL];
      fHit.NVL++;
    }
  }
  cout << endl;
  delete array;
  FillG2Table();
}
//________________________________________________________________________________
void StarMCHits::FillG2Table() {
  St_g2t_Chair *chair = fCurrentDetector->GetChair();
  if (! chair ) {
    TString name(fCurrentDetector->GetName());
    for (Int_t k = 0; k < kALL; k++) {
      if (name != TString(g2t[k].Name)) continue;
      chair = NewChair(g2t[k].G2T_type,g2t[k].G2T_name);
      assert(chair);
      fCurrentDetector->SetChair(chair);
      break;
    }
  }
  chair->Fill(fHit);
}
//________________________________________________________________________________
TTable *StarMCHits::NewTable(const Char_t *classname, const Char_t *name, Int_t nrows) {
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
St_g2t_Chair *StarMCHits::NewChair(const Char_t *type, const Char_t *name) {
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

