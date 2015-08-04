// $Id: StarMCHits.cxx,v 1.11 2015/08/04 21:00:39 jwebb Exp $
// $Log: StarMCHits.cxx,v $
// Revision 1.11  2015/08/04 21:00:39  jwebb
// Improved const-ness.  Removed unused struct.
//
// Revision 1.10  2009/02/03 16:01:05  fisyak
// Add includes
//
// Revision 1.9  2007/01/09 04:51:43  potekhin
// Added legacy mode
//
// Revision 1.8  2007/01/05 21:38:58  potekhin
// Add CVS tags
//
#include <assert.h>
#include <stdio.h>
#include "Stiostream.h"
#include "StarVMCApplication.h"
#include "StarMCHits.h"

//please be patient and don't get confused here  :)
#include "StarMCHit.h"

#include "TGeoManager.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "TDataSetIter.h"
#include "TPDGCode.h"
#include "TVirtualMC.h"
#include "TArrayI.h"
#include "TObjArray.h"
#include "TObjString.h"
#include "TClass.h"
#include "TROOT.h"
#include "TRandom.h"
#include "TFile.h"
#include "TLorentzVector.h"
#ifdef __ROOT__
#include "StMaker.h"
#endif
#include "St_g2t_Chair.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_particle_Table.h"
//#include "tables/St_g2t_run_Table.h"
#include "tables/St_g2t_gepart_Table.h"
StarMCHits *StarMCHits::fgInstance = 0;
ClassImp(StarMCHits);
enum EHITtypes{kX=1,  kY,  kZ,   kR,    kRR,   kPHI,  kTHET, kETA,  kTDR,  kCP,
               kU,    kV,  kW,   kETOT, kELOS, kBIRK, kSTEP, kLGAM, kTOF,  kUSER,
	       kXX,   kYY, kZZ,  kPX,   kPY,   kPZ,   kSLEN, kPTOT, kLPTO, krese};
enum EDetectorTypes {
  kBCSB,//      BCSB
  kBPOL,//      BPOL
  kBRSG,//      BRSG
  kBXSA,//      BXSA

  kCSDA,//      CSDA
  kCSUP,//      CSUP      

  kEHMS,//      EHMS      
  kELGR,//            g2t_eem.F:        if (isys.eq.2) call G2R_GET_SYS ('ECAL','ELGR',Iprin,Idigi)
  kEPCT,//            g2t_eem.F:        if (isys.eq.3) call G2R_GET_SYS ('ECAL','EPCT',Iprin,Idigi)
  kESCI,//      ESCI      
  kEXSE,//             g2t_esm.F:      if (Isys.eq.1) call G2R_GET_SYS ('ECAL','EXSE',Iprin,Idigi)
  kFGTC, 
  kFHMS,//      ->     FHMS      fpdmgeo1.g:Block FHMS is sHower Max Strip
  kFLGR,//      ->     FLGR      fpdmgeo1.g:Block FLGR is Lead Glass detector
  kFDSW,//      fstdgeo.g:Block FDSW is the Silicon Wafer (all active), g2t_fst.F:      call G2R_GET_SYS ('FSTD','FDSW',Iprin,Idigi)
  kFPCT,//      ->     FPCT  fpdmgeo.g:Block FPCT is Photo Cathode    
  kFREO,//      FREO      
  kFSCI,//      ->    FSCI  fpdmgeo.g:Block FSCI  is the active scintillator (polystyren) layer    
  kFSEC,//      FSEC      

  kIBSS,//            g2t_ist.F:      call G2R_GET_SYS ('ISTB','IBSS',Iprin,Idigi)

  kOQUA,//      ->      OQUA  richgeo.g:block OQUA e me scelto per labellare il quarzo opaco    

  kPDGS,//      PDGS      
  kPLAC,//            g2t_pix.F:      call G2R_GET_SYS ('PIXL','PLAC',Iprin,Idigi)

  kQSCI,//      QSCI      
  kQUAR,//      QUAR      

  kRCSI,//      RCSI      
  kRGAP,//      RGAP      

  kSFSD,//      SFSD      
  kSVTD,//      SVTD      

  kTMSE,//      TMSE      
  kTPAD,//      TPAD      
  kVRAD,//      VRAD	    
  kALL
};
struct Detector_G2T_t {
  EDetectorTypes kType;
  const Char_t        *Name;
  const Char_t        *G2T_type;
  const Char_t        *G2T_name;
  const Char_t        *G2T_sys;
  const Char_t        *G2T_geom;
};
static const Detector_G2T_t g2t[kALL] = {
  //              type(cd)      name          sys    geom version
  { kBCSB,"BCSB","g2t_ctf_hit","g2t_tof_hit","BTOF","btof_btog"}, // + 
  { kBRSG,"BRSG","g2t_ctf_hit","g2t_tfr_hit","BTOF","btof_btog"}, // +
  { kBXSA,"BXSA","g2t_ctf_hit","g2t_ctb_hit","BTOF","btof_btog"}, // +

  { kBPOL,"BPOL","g2t_ctf_hit","g2t_bbc_hit","BBCM",""}, // +

  { kCSDA,"CSDA","g2t_emc_hit","g2t_smd_hit","CALB",""}, // +
  { kCSUP,"CSUP","g2t_emc_hit","g2t_emc_hit","CALB",""}, // +

  { kEHMS,"EHMS","g2t_emc_hit","g2t_esm_hit","ECAL","ecal_emcg"}, // +
  { kELGR,"ELGR","g2t_emc_hit","g2t_eem_hit","ECAL",""}, // +
  { kEPCT,"EPCT","g2t_emc_hit","g2t_eem_hit","ECAL",""}, // +
  { kESCI,"ESCI","g2t_emc_hit","g2t_eem_hit","ECAL","ecal_emcg"}, // +
  { kEXSE,"EXSE","g2t_emc_hit","g2t_esm_hit","ECAL",""}, // +

  { kFGTC,"FGTC","g2t_fgt_hit","g2t_fgt_hit","FGTD",""}, // +
  { kFHMS,"FHMS","g2t_emc_hit","g2t_fpd_hit","FPDH",""},
  { kFLGR,"FLGR","g2t_emc_hit","g2t_fpd_hit","FPDH",""},
  { kFDSW,"FDSW","g2t_fst_hit","g2t_fst_hit","FSTD",""}, // +
  { kFPCT,"FPCT","g2t_emc_hit","g2t_fpd_hit","FPDH",""},
  { kFSCI,"FSCI","g2t_emc_hit","g2t_fpd_hit","FPDH",""},

  { kFREO,"FREO","g2t_rch_hit","g2t_rch_hit","RICH",""}, // +
  { kFSEC,"FSEC","g2t_ftp_hit","g2t_ftp_hit","FTPC","ftpc_ftpg"}, // +

  { kIBSS,"IBSS","g2t_ist_hit","g2t_ist_hit","ISTB",""},  // +

  //?  { kOQUA,"OQUA","g2t_rch_hit","g2t_rch_hit","RICH",""}, 
  { kPLAC,"PLAC","g2t_pix_hit","g2t_pix_hit","PIXL",""}, // +

  { kPDGS,"PDGS","g2t_pmd_hit","g2t_pmd_hit","PHMD",""}, // +

  { kQSCI,"QSCI","g2t_emc_hit","g2t_zdc_hit","ZCAL",""}, // +
  { kQUAR,"QUAR","g2t_rch_hit","g2t_rch_hit","RICH",""}, // +

  { kRCSI,"RCSI","g2t_rch_hit","g2t_rch_hit","RICH",""}, // +
  { kRGAP,"RGAP","g2t_rch_hit","g2t_rch_hit","RICH",""}, // +

  { kSFSD,"SFSD","g2t_svt_hit","g2t_ssd_hit","SISD",""}, // +
  { kSVTD,"SVTD","g2t_svt_hit","g2t_svt_hit","SVTT","svtt_svtg"}, // +

  { kTMSE,"TMSE","g2t_mwc_hit","g2t_mwc_hit","TPCE",""}, // +
  { kTPAD,"TPAD","g2t_tpc_hit","g2t_tpc_hit","TPCE","tpce_tpcg"}, // +
  { kVRAD,"VRAD","g2t_vpd_hit","g2t_vpd_hit","VPDD","vpdd_vpdg"} // +
};


Int_t      StarMCHit::count = 0;
StarMCHit* StarMCHit::lastHit = NULL;

//________________________________________________________________________________
StarMCHits::StarMCHits(const Char_t *name,const Char_t *title) : 
  TDataSet(name,title), fDetectors(0),fGeoData(0),  fDetList(0), 
  fVolUserInfo(0), fCurrentDetector(0), fDebug(0), fLegacy(0), fSeed(0), fEventNumber(0)
{ 
  fgInstance = this;
  fHitHolder = this; 
  fHitCollection = new StarMCHitCollection();
}
//________________________________________________________________________________
Int_t StarMCHits::Init() {

  if(Legacy()) return InitLegacy(); // revert to the legacy Init

  if (!fDetectors ) delete fDetectors;
  fDetectors = 0;

  if (! StMaker::GetChain()) {
    cout << "StarMCHits::Init() -I- There is no chain." <<endl;
    assert(StMaker::GetChain());
  }
  else {
    fGeoData   = StMaker::GetChain()->GetDataBase("VmcGeometry/geom");
  }

  SetDebug(1);
  return 0;
}
//________________________________________________________________________________
Int_t StarMCHits::InitLegacy() {

  cout << "StarMCHits::Init() -I- Get Detectors" <<endl;

  if (! fDetectors ) delete fDetectors;

  fDetectors = 0;

#ifdef __ROOT__
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
    
#ifdef __ROOT__
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
#if 0
	  if (TString(g2t[k].G2T_geom) != "") {
	    TTable *table = ( TTable * ) fGeoData->Find(g2t[k].G2T_geom);
	  }
#endif
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
    TString title(vol->GetName(),4);
    TObject *det = fDetList->FindObject(title.Data());
#if 0
    if (det) cout << "\tDetector: " << det->GetName();// << "\t" << det->GetTitle();
#endif
    fVolUserInfo->AddAtAndExpand(det,uid);
#if 0
    cout << endl;
#endif
  }
  SetDebug(1);
  return 0;
}
//________________________________________________________________________________
void StarMCHits::Step() {

  if(Legacy()) {
    StepLegacy(); // revert to the legacy step routine
    return;
  }

  TGeoNode *nodeT        = gGeoManager->GetCurrentNode();  assert(nodeT);
  TGeoVolume *volT       = nodeT->GetVolume();             assert(volT);
  const TGeoMedium  *med = volT->GetMedium(); 

  // -- RETURN POINT 1: NON SENSITIVE VOLUME
  Int_t Isvol = (Int_t) med->GetParam(0);
  if (Isvol <= 0) return;

  // if we are entering a volume, we've got to create a new hit; if not, pick the current hit
  StarMCHit* hit = gMC->IsTrackEntering() ? new StarMCHit() : StarMCHit::LastHit();
  assert(hit);

  cout<<StarMCHit::Count()<<" hits created so far"<<endl; //  cout<<StarMCHitCollection::Count()<<" in collection"<<endl;

  TGeoHMatrix  *matrixC = gGeoManager->GetCurrentMatrix();
  gMC->TrackPosition(hit->currentPointGlobal);
  hit->getLocal(matrixC);

  // -- RETURN POINT 2: SET ENTRY POINT AND LEAVE
  if (gMC->IsTrackEntering()) {
    cout<<"Entering "<<volT->GetName()<<endl;
    hit->setEntry();


    TString GeoManagerPath(gGeoManager->GetPath());
    TString LogicalPath(volT->GetTitle());

    static const TString GeoManagerSeparator("/_");
    TObjArray *GeoManagerPathNodes  = GeoManagerPath.Tokenize(GeoManagerSeparator);
    Int_t      GeoManagerPathLength = GeoManagerPathNodes->GetEntriesFast();

    static const TString LogicalSeparator("/");
    TObjArray *LogicalPathNodes = LogicalPath.Tokenize(LogicalSeparator);

    Int_t i=0,j=0;

    cout << "Geo Path: " << GeoManagerPath << " Logical Path:"<< LogicalPath<< endl;

    //    TString numPath="";

    for (i = 0; i < GeoManagerPathLength; i+=2) {
      TString GeoManagerString = ((TObjString*) GeoManagerPathNodes->At(i))->GetString();
      TString LogicalString    = ((TObjString*) LogicalPathNodes->At(j))   ->GetString();
      if(GeoManagerString.Contains(LogicalString)) {
	cout<<"Found:"<< GeoManagerString << " " << LogicalString << endl;
	TString stringN = ((TObjString*) GeoManagerPathNodes->At(i+1))->GetString();
        Int_t n = stringN.Atoi();

	hit->setPathAt(j++,(Short_t) n);	// numPath+="/";numPath+=stringN;
      }
    }
    //    cout<<"NumPath:"<<numPath<<endl; for(int i=0;i<StarMCHit::MaxDepth();i++) {cout<<" "<<hit->getPathAt(i);} cout<<endl;

    Int_t iPart  = gMC->TrackPid();
    Int_t iTrack = StarVMCApplication::Instance()->GetStack()->GetCurrentTrackId();
    hit->setTrack(iTrack);
    hit->setPid(iPart);
    return;
  }

  Double_t dedx = gMC->Edep();
  Double_t step = gMC->TrackStep();

  cout<<"Track "<<hit->getTrack()<<", particle "<<hit->getPid()<<", dE:"<<dedx<<", step:"<<step<<endl;
  hit->AddDedx(dedx);
  hit->AddStep(step);

  // -- RETURN POINT 3: KEEP GOING THROUGH THE VOLUME
  if (! gMC->IsTrackExiting() && ! gMC->IsTrackStop()) {
    cout<<"Propagating in "<<volT->GetName()<<endl;
    return;
  }


  // -- RETURN POINT 4: FINALIZE THE HIT

  hit->setActive(false);
  hit->setExit();
  HitCollection()->Add(hit);
  cout<<"Leaving "<<volT->GetName()<<endl;

  return;
}
//________________________________________________________________________________
void StarMCHits::StepLegacy() {

  static Double_t    Gold  = 0;
  static St_det_user *user = 0;
  static St_det_path *path = 0;
  //  static St_det_hit  *hit  = 0;

  TGeoNode *nodeT  = gGeoManager->GetCurrentNode();  assert(nodeT);
  TGeoVolume *volT = nodeT->GetVolume();             assert(volT);

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
  cout << "StarMCHits::Step - volT:"<<volT->GetName()<<"  Isvol " << Isvol<<endl;

  if (Isvol <= 0 && ! fCurrentDetector) return;
  if (Isvol && ! fCurrentDetector ) {
    if (Debug())
      cout << "Active medium:" << med->GetName() << "\t for volume " << volT->GetName() 
	   << " has no detector description" << endl;
    return;
  }
  if (Isvol <= 0 &&  fCurrentDetector && Debug()) {
    cout << "Dead medium:" << med->GetName() << "\t for volume " << volT->GetName() 
	 << " has detector description" << endl;
    return;
  }
  //  Int_t Idevt =  gMC->CurrentEvent();
  gMC->TrackPosition(fHit.Current.Global.xyzT);
  gMC->TrackMomentum(fHit.Current.Global.pxyzE);
  TGeoHMatrix  *matrixC = gGeoManager->GetCurrentMatrix();
  fHit.Current.Global2Local(matrixC);
  if (gMC->IsTrackEntering()) {
    user = fCurrentDetector->GetUserDesc(); det_user_st *User = user->GetTable();
    fHit.Detector= fCurrentDetector;
    fHit.IdType  = User->IdType;
    fHit.Serial  = User->Serial;
    fHit.Goption = User->Goption;
    fHit.Nva     = User->Nva;
    fHit.Nvb     = User->Nvb;
    path = fCurrentDetector->GetPathDesc();
    //    hit  = fCurrentDetector->GetHitDesc();
    fHit.Entry = fHit.Current;
    fHit.Sleng = gMC->TrackLength();
    fHit.Charge = (Int_t) gMC->TrackCharge();
    fHit.Mass = gMC->TrackMass();
    fHit.AdEstep = fHit.AStep = 0;
    return;
  }
  Double_t GeKin = fHit.Current.Global.pxyzE.E() - fHit.Mass;
  fHit.Sleng = gMC->TrackLength();
  if (fHit.Sleng == 0.) Gold = GeKin;
  Double_t dEstep = gMC->Edep();
  Double_t Step = gMC->TrackStep();
  fHit.iPart = gMC->TrackPid();
  fHit.iTrack = StarVMCApplication::Instance()->GetStack()->GetCurrentTrackId(); // GetCurrentTrackNumber() + 1 to be consistent with g2t
  // - - - - - - - - - - - - - energy correction - - - - - - - - - -
  if (gMC->IsTrackStop() && TMath::Abs(fHit.iPart) == kElectron) {
    TArrayI proc;
    Int_t Nproc = gMC->StepProcesses(proc);
    Int_t Mec = 0;
    for (Int_t i = 0; i < Nproc; i++) if (proc[i] == kPAnnihilation || proc[i] == kPStop) Mec = proc[i];
    Int_t Ngkine = gMC->NSecondaries();
    if (fHit.iPart == kElectron && Ngkine == 0 && Mec == kPStop) dEstep = Gold;
    else {
      if (fHit.iPart == kPositron && Ngkine < 2 && Mec == kPAnnihilation) {
	dEstep = Gold + 2*fHit.Mass;
	if (Ngkine == 1) {
	  TLorentzVector x;
	  TLorentzVector p;
	  Int_t IpartSec;
	  gMC->GetSecondary(0,IpartSec,x,p);
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
  if (fHit.AdEstep == 0) return;
  if (! gMC->IsTrackExiting() && ! gMC->IsTrackStop()) return;
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
  if (Debug() > 1) cout << "Path: " << thePath;
  for (i = j = 0; i < NL; i++, j += 2, Path++) {
    objs = (TObjString *) array->At(j);
    assert(objs->GetString().BeginsWith(TString(Path->VName,4)));
    if (Path->Ncopy != 1) {
      objs = (TObjString *) array->At(j+1);
      fHit.NUMBV[fHit.NVL] = atoi(objs->GetString().Data());
      if (Debug() > 1) cout << "\t" << fHit.NUMBV[fHit.NVL];
      fHit.NVL++;
    }
  }
  if (Debug() > 1) cout << endl;
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
  assert(chair);
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
//________________________________________________________________________________
void StarMCHits::FinishEvent() {
  static const Double_t pEMax = 1 - 1.e-10;

  if(!Legacy()) {
    cout<<"Number of added hits: "<<HitCollection()->GetEntriesFast()<<endl;
    return;
  }

  TDataSet *m_DataSet = StarMCHits::instance()->GetHitHolder();
  if (! m_DataSet) return;
  St_g2t_event *g2t_event = new St_g2t_event("g2t_event",1);  
  m_DataSet->Add(g2t_event);
  g2t_event_st event;
  memset (&event, 0, sizeof(g2t_event_st));
  fEventNumber++;
  event.n_event            = fEventNumber;//IHEAD(2)
  event.ge_rndm[0]         =        fSeed;//IHEAD(3)
  event.ge_rndm[1]         =            0;//IHEAD(4)
  event.n_run              =            1;
  event.n_track_eg_fs      = StarVMCApplication::Instance()->GetStack()->GetNtrack();
  event.n_track_prim       = StarVMCApplication::Instance()->GetStack()->GetNprimary();
  event.prim_vertex_p      =            1;
  event.b_impact           =           99;
  event.phi_impact         =          0.5;
  g2t_event->AddAt(&event);
  Int_t NoVertex = 1;
  St_g2t_vertex  *g2t_vertex  = new St_g2t_vertex("g2t_vertex",NoVertex);
  m_DataSet->Add(g2t_vertex); 
  g2t_vertex_st vertex;
  Int_t NTracks = StarVMCApplication::Instance()->GetStack()->GetNtrack();
  St_g2t_track   *g2t_track   = new St_g2t_track ("g2t_track",NTracks);
  m_DataSet->Add(g2t_track);
  g2t_track_st track;
  StarMCParticle  *particle = 0;   
  Int_t iv = 0;
  TLorentzVector oldV(0,0,0,0);
  TLorentzVector newV(0,0,0,0);
  TLorentzVector devV(0,0,0,0);
  for (Int_t it = 0; it <NTracks; it++) {
    memset(&track, 0, sizeof(g2t_track_st));
    particle = (StarMCParticle*) StarVMCApplication::Instance()->GetStack()->GetParticle(it);
    TParticle  *part = (TParticle *) particle->GetParticle();
    part->ProductionVertex(newV);
    devV = newV - oldV;
    if (iv == 0 || devV.Mag() > 1.e-7) {
      if (iv > 0) g2t_vertex->AddAt(&vertex);
      memset (&vertex, 0, sizeof(g2t_vertex_st));
      iv++;
      vertex.id           = iv             ;// primary key 
      vertex.event_p      = 0              ;// pointer to event
      vertex.eg_label     = 0              ;// generator label (0 if GEANT)
      vertex.eg_tof       = 0              ;// vertex production time
      vertex.eg_proc      = 0              ;// event generator mechanism
      memcpy(vertex.ge_volume,"   ",4);    ;// GEANT volume name
      vertex.ge_medium    = 0              ;// GEANT Medium
      vertex.ge_tof       = 0              ;// GEANT vertex production time
      vertex.ge_proc      = 0              ;// GEANT mechanism (0 if eg)
      vertex.ge_x[0]      = newV.X()       ;// GEANT vertex coordinate
      vertex.ge_x[1]      = newV.Y()       ;
      vertex.ge_x[2]      = newV.Z()       ;
      vertex.ge_tof       = newV.T()       ;
      vertex.n_parent     = 0              ;// number of parent tracks
      vertex.parent_p     = 0              ;// first parent track
      vertex.is_itrmd     = 0              ;// flags intermediate vertex
      vertex.next_itrmd_p = 0              ;// next intermedate vertex 
      vertex.next_prim_v_p= 0              ;// next primary vertex
      oldV                = newV;
    }
    vertex.n_daughter++;
    track.id             = it+1;
    track.eg_label       = particle->GetIdGen();
    track.eg_pid         = part->GetPdgCode();
    track.ge_pid         = gMC->IdFromPDG(track.eg_pid);
    track.start_vertex_p = iv;
    track.p[0]           = part->Px();
    track.p[1]           = part->Py();
    track.p[2]           = part->Pz();
    track.ptot           = part->P();
    track.e              = part->Energy();
    track.charge         = part->GetPDG()->Charge()/3;
    Double_t   ratio     = part->Pz()/part->Energy();
    ratio                = TMath::Min(1.-1e-10,TMath::Max(-1.+1e-10, ratio));
    track.rapidity       = TMath::ATanH(ratio);
    track.pt             = part->Pt();
    ratio                = part->Pz()/part->P();
    ratio                = TMath::Min(pEMax,TMath::Max(-pEMax, ratio));
    track.eta            = TMath::ATanH(ratio);
    g2t_track->AddAt(&track);
  }
  g2t_vertex->AddAt(&vertex);   
}
//________________________________________________________________________________
void StarMCHits::Clear(const Option_t* opt) {
  if(Legacy()) {
    TObjArrayIter next(fVolUserInfo);
    StHitDescriptor *desc = 0;
    St_g2t_Chair *chair = 0;
    while ((desc = (StHitDescriptor *) next())) {
      chair = desc->GetChair();
      if (chair) {
	delete chair;
	desc->SetChair(0);
      }
    }
  }
  if (gRandom) fSeed = gRandom->GetSeed();
}
#if 0
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
#endif
