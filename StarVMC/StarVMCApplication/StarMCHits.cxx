#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "Stiostream.h"
#include "StarVMCApplication.h"
#include "StarMCHits.h"
#include "TGeoManager.h"
#include "TGeant3TGeo.h"
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
#include "TGeoPhysicalNode.h"
#include "TGeoBBox.h"
#include "TLorentzVector.h"
#include "TFile.h"
#include "St_g2t_Chair.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_particle_Table.h"
//#include "tables/St_g2t_run_Table.h"
#include "tables/St_g2t_gepart_Table.h"
#include "StEnumerations.h"
#include "TGeoExtension.h"
#include "TCallf77.h"
StarMCHits *StarMCHits::fgInstance = 0;
ClassImp(StarMCHits);
//______________________________________________________________________
# define gcohit gcohit_
# define g3birk g3birk_
extern "C"
{
  void    type_of_call gcohit(DEFCHARD, Int_t*& DEFCHARL);
  bool*   type_of_call gcaddb(bool *arg);
  char*   type_of_call gcaddc(char *arg);
  double* type_of_call gcaddd(double *arg);
  int*    type_of_call gcaddi(int  *arg);
  float*  type_of_call gcaddf(float *arg);
  int*    type_of_call gcaddl(int *arg);
  void    type_of_call g3birk(float *arg);
};
//________________________________________________________________________________
StarMCHits::StarMCHits(const Char_t *name,const Char_t *title) : 
  TDataSet(name,title), fCurrentDetector(0), fDebug(0), fSeed(0), fEventNumber(0) { 
  fgInstance = this; fHitHolder = this; 
  gcohit(PASSCHARD("AGCDIGI"),(int*&) fAgcdigi  PASSCHARL("AGCDIGI"));
  gcohit(PASSCHARD("AGCHITV"),(int*&) fAgchitv  PASSCHARL("AGCHITV"));
}
//________________________________________________________________________________
Int_t StarMCHits::Init() {
  cout << "StarMCHits::Init() -I- Get Volume Info" << endl;
  StarVMCDetectorSet::instance()->GetDetectorHash();
  return 0;
}
//________________________________________________________________________________
void StarMCHits::Step() {
  //  static Int_t Idevt0 = -1;
  static Double_t Gold = 0;
#if 0
  if (Debug() && TVirtualMC::GetMC()->IsA()->InheritsFrom("TGeant3TGeo")) {
    TGeant3TGeo *geant3 = (TGeant3TGeo *)TVirtualMC::GetMC();
    geant3->Gdebug();
  }
#endif
  //  cout << "Call StarMCHits::Step" << endl;
  TGeoNode *nodeT = gGeoManager->GetCurrentNode();
  assert(nodeT);
  TGeoVolume *volT = nodeT->GetVolume();
  assert(volT);
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
  fCurrentDetector = 0;
  if (Isvol <= 0) return;
  TGeoRCExtension *ext = (TGeoRCExtension *)volT->GetUserExtension();
  if (ext) {
    fCurrentDetector = (StarVMCDetector *) ext->GetUserObject();
    if (! fCurrentDetector) {
      volT = nodeT->GetMotherVolume();
      fCurrentDetector = (StarVMCDetector *) ext->GetUserObject();
      if (! fCurrentDetector) {
	TString path(gGeoManager->GetPath());
	TObjArray *obj = path.Tokenize("_/");
	Int_t N = obj->GetEntries();
	for (Int_t i = N-2; i >= 0; i -= 2) {
	  TObjString *o = (TObjString  *) obj->At(i);
	  const Char_t *name = o->GetName();
	  volT = gGeoManager->GetVolume(name);
	  assert (volT);
	  fCurrentDetector = (StarVMCDetector *) ext->GetUserObject();
	  if (fCurrentDetector) break;
	}
	delete obj;
      }
    }
  }
  if (Debug()) {
    if (! fCurrentDetector) {
      cout << "Active medium:" << med->GetName() << "\t for volume " << volT->GetName() 
	   << " has no detector description" << endl;
    } else {
      TString dName(fCurrentDetector->GetName());
      if (dName != volT->GetName()) {
	cout << "Detector name " << dName << " does not match volume name " << volT->GetName() << endl;
      }
    }
  }
  if (! fCurrentDetector) return;
  //  Int_t Idevt =  TVirtualMC::GetMC()->CurrentEvent();
  TVirtualMC::GetMC()->TrackPosition(fHit.Current.Global.xyzT);
  TVirtualMC::GetMC()->TrackMomentum(fHit.Current.Global.pxyzE);
  TGeoHMatrix  *matrixC = gGeoManager->GetCurrentMatrix();
  fHit.Current.Global2Local(matrixC);
  if (TVirtualMC::GetMC()->IsTrackEntering()) {
    fHit.Detector= fCurrentDetector;
    fHit.Entry = fHit.Current;
    fHit.Sleng = TVirtualMC::GetMC()->TrackLength();
    fHit.Charge = (Int_t) TVirtualMC::GetMC()->TrackCharge();
    fHit.Mass = TVirtualMC::GetMC()->TrackMass();
    fHit.AdEstep = fHit.AStep = fHit.birk = 0;
    return;
  }
  Double_t GeKin = fHit.Current.Global.pxyzE.E() - fHit.Mass;
  fHit.Sleng = TVirtualMC::GetMC()->TrackLength();
  if (fHit.Sleng == 0.) Gold = GeKin;
  Float_t dEstep = TVirtualMC::GetMC()->Edep();
  Float_t Step = TVirtualMC::GetMC()->TrackStep();
  fHit.iPart = TVirtualMC::GetMC()->TrackPid();
  fHit.iTrack = ((StarStack *)TVirtualMC::GetMC()->GetStack())->GetCurrentTrackNumber() + 1; // GetCurrentTrackNumber() + 1 to be consistent with g2t
  // - - - - - - - - - - - - - energy correction - - - - - - - - - -
  if (TVirtualMC::GetMC()->IsTrackStop() && TMath::Abs(fHit.iPart) == kElectron) {
    TArrayI proc;
    Int_t Nproc = TVirtualMC::GetMC()->StepProcesses(proc);
    Int_t Mec = 0;
    for (Int_t i = 0; i < Nproc; i++) if (proc[i] == kPAnnihilation || proc[i] == kPStop) Mec = proc[i];
    Int_t Ngkine = TVirtualMC::GetMC()->NSecondaries();
    if (fHit.iPart == kElectron && Ngkine == 0 && Mec == kPStop) dEstep = Gold;
    else {
      if (fHit.iPart == kPositron && Ngkine < 2 && Mec == kPAnnihilation) {
	dEstep = Gold + 2*fHit.Mass;
	if (Ngkine == 1) {
	  TLorentzVector x;
	  TLorentzVector p;
	  Int_t IpartSec;
	  TVirtualMC::GetMC()->GetSecondary(0,IpartSec,x,p);
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
  Float_t birk = dEstep;
  if (fHit.AdEstep == 0) return;
  g3birk(&birk);
  fHit.birk    += birk;
  if (! TVirtualMC::GetMC()->IsTrackExiting() && ! TVirtualMC::GetMC()->IsTrackStop()) return;
  fHit.Exit     = fHit.Current;
  fHit.Middle   = fHit.Entry;
  fHit.Middle  += fHit.Exit;
  fHit.Middle  *= 0.5;
  if (! fCurrentDetector) return;
  FillG2Table();
}
//________________________________________________________________________________
void StarMCHits::FillG2Table() {
  St_g2t_Chair *chair = fCurrentDetector->GetChair();
  assert(chair);
  TGeoVolume *volT = gGeoManager->GetCurrentVolume();
  strncpy(Agchitv()->cd, volT->GetName(), 4);
  TString csys(fCurrentDetector->GetTitle());
  Double_t eta = fHit.Middle.Global.xyzT.Eta();
  if        (csys ==  "eem") {
    //    cout << "eem" << endl;
  } else if (csys ==  "emc") {
    static Double_t deta = 1./20;
    Int_t ieta = TMath::Abs(eta)/deta;// + 1
    Int_t phi_sub = 0;
    if (fHit.Middle.Local.xyzT.Y() > 0) phi_sub = 1;
    Agcdigi()->idigi[0] = ieta;
    Agcdigi()->idigi[1] = phi_sub;
  } else if (csys ==  "smd") {
    static Double_t etaStripPitch1 = 1.54, etaStripPitch2 = 1.96, phiStripPitch = 1.49;
    TGeoNode *node = gGeoManager->GetCurrentNavigator()->GetCurrentNode();
    Double_t xyzCSCI[4];
    fHit.Middle.Local.xyzT.GetXYZT(xyzCSCI);
    Double_t xyzCSME[3];
    TGeoMatrix *rot = node->GetMatrix();
    rot->LocalToMaster(xyzCSCI, xyzCSME);
    TGeoNode *nodeCSME = gGeoManager->GetCurrentNavigator()->GetMother(1);
    TGeoNode *nodeCSDA = gGeoManager->GetCurrentNavigator()->GetMother(2);
    //    cout << "smd;" << nodeCSDA->GetName() << endl;
    // type = 1 & 2 : eta strip; type = 3 & 4 : phi strip
    Int_t type = 0;
    Int_t n = sscanf(nodeCSDA->GetName(), "CSDA_%i", &type);
    assert(n == 1);
    // On wire
    xyzCSME[0] = 0;
    Double_t xyzCSDA[3];
    TGeoMatrix *rot2CSME = nodeCSME->GetMatrix();
    rot2CSME->LocalToMaster(xyzCSME, xyzCSDA);
    TGeoVolume *CSDA = nodeCSDA->GetVolume();
    TGeoBBox *box = dynamic_cast<TGeoBBox*>(CSDA->GetShape());
    assert(box);
    Int_t strip = -1;
    if (type <= 2) {// eta strip
      Double_t z = xyzCSDA[2] + box->GetDZ();
      if (type == 1) strip = z/etaStripPitch1;
      else           strip = z/etaStripPitch2;
    } else         {// phi strip
      Double_t y = xyzCSDA[1] + box->GetDY();
      strip = y/phiStripPitch;
    }
#if 0
    Int_t ieta = 0;
    if (type == 2 || type == 4) ieta = 1;
#endif
    static Double_t deta = 1./10;
    Int_t ieta = TMath::Abs(eta)/deta;// + 1
    Int_t phi_sub = 0;
    if (fHit.Middle.Local.xyzT.Y() > 0) phi_sub = 1;
    TObjArray *objs = gGeoManager->GetListOfPhysicalNodes();
    TGeoPhysicalNode *nodeP = 0;
    TString path(gGeoManager->GetPath());
    if (objs) nodeP = (TGeoPhysicalNode *) objs->FindObject(path);
    if (! nodeP) 	nodeP = gGeoManager->MakePhysicalNode(path);
    Agcdigi()->idigi[0] = ieta;
    Agcdigi()->idigi[1] = phi_sub;
    Agcdigi()->idigi[2] = strip;
  } else if (csys ==  "fpd") {
    //    cout << "fpd" << endl;
  }
  fHit.VolumeId = fCurrentDetector->GetVolumeId(gGeoManager->GetPath());
  if (fHit.VolumeId <= 0) return;
  chair->Fill(fHit);
}
//________________________________________________________________________________
void StarMCHits::FinishEvent() {
  static const Double_t pEMax = 1 - 1.e-10;
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
  event.n_track_eg_fs      = TVirtualMC::GetMC()->GetStack()->GetNtrack();
  event.n_track_prim       = TVirtualMC::GetMC()->GetStack()->GetNprimary();
  event.prim_vertex_p      =            1;
  event.b_impact           =           99;
  event.phi_impact         =          0.5;
  g2t_event->AddAt(&event);
  Int_t NoVertex = 1;
  St_g2t_vertex  *g2t_vertex  = new St_g2t_vertex("g2t_vertex",NoVertex);
  m_DataSet->Add(g2t_vertex); 
  g2t_vertex_st vertex;
  Int_t NTracks = TVirtualMC::GetMC()->GetStack()->GetNtrack();
  St_g2t_track   *g2t_track   = new St_g2t_track ("g2t_track",NTracks);
  m_DataSet->Add(g2t_track);
  g2t_track_st track;
  //  TParticle  *particle = 0;   
  Int_t iv = 0;
  TLorentzVector oldV(0,0,0,0);
  TLorentzVector newV(0,0,0,0);
  TLorentzVector devV(0,0,0,0);
  for (Int_t it = 0; it < NTracks; it++) {
    memset(&track, 0, sizeof(g2t_track_st));
    TParticle  *part = (TParticle*) ((StarStack *) TVirtualMC::GetMC()->GetStack())->Particle(it);
    part->ProductionVertex(newV);
    devV = newV - oldV;
    if (iv == 0 || devV.P() > 1.e-7) {// 3D distance
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
    //    track.eg_label       = particle->GetIdGen();
    track.eg_pid         = part->GetPdgCode();
    track.ge_pid         = TVirtualMC::GetMC()->IdFromPDG(track.eg_pid);
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
  if (StarVMCDetectorSet::instance()->GetDetectorHash()) {
    TListIter next(StarVMCDetectorSet::instance()->GetDetectorHash());
    StarVMCDetector *desc = 0;
    while ((desc = (StarVMCDetector *) next())) {    desc->Clear();  }
  }
  if (gRandom) fSeed = gRandom->GetSeed();
}
//________________________________________________________________________________
void StarMCHits::SetDebug(Int_t m) {
  if (fDebug == m) return;
  fDebug = m;
  if (TVirtualMC::GetMC() && TVirtualMC::GetMC()->IsA()->InheritsFrom("TGeant3TGeo")) {
    TGeant3TGeo *geant3 = (TGeant3TGeo *)TVirtualMC::GetMC();
    Gcflag_t* cflag = geant3->Gcflag();
    cflag->idebug = Debug();
    cflag->idemax = 10000;
    cflag->iswit[0] = 2;
    cflag->iswit[1] = 2;
  }
}
