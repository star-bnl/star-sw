//
#include <stdio>
#include <iomanip>
#include "T.h"

class T;
T *t = 0;
//==============================================================================
// Constants.
//------------------------------------------------------------------------------
const Double_t kR_MAX = 250.;
const Double_t kZ_MAX = 250.;

const Double_t kR_TPC_min = 46;
const Double_t kR_TPC_max = 200;
const Double_t kZ_TPC_d   = 200;

const Double_t kR_HFT_min = 0;
const Double_t kR_HFT_max = 30;
const Double_t kZ_HFT_d   = 30;

const Double_t kR_BP_min = 2.00;
const Double_t kR_BP_max = 2.076;
const Double_t kZ_BP_d   = 30;

const Int_t NLADDER_PXL = 40;
const Int_t NLADDER_IST = 24;
const Int_t NLADDER_SSD = 20;

const Double_t kR_IST = 14.0;
const Double_t kR_IST_min = 12.0;
const Double_t kR_IST_max = 16.0;
const Double_t kZ_IST_d = 20;

// Solenoid field along z, in Tesla.
const Double_t kMagField = 0.498948;
const Int_t kPXL_Cluster_Min = 1;  // hit cluster > Min
const Int_t kPXL_Cluster_Max = 20;  // hit cluster <= Max

// Draw Options
const Bool_t kPlotHFTSupport = false;
const Color_t  kColors[3] = { kRed, kGreen, kYellow };
const Double_t kTrackHitOffset = 0.5;

// sensor status
const Int_t NSEC_PXL = 10;
const Int_t NLAD_PXL = 4;
const Int_t NSEN_PXL = 10;
const Int_t NLAD_IST = 24;
const Int_t NSEN_IST = 6;
const Int_t N_PXL = NSEC_PXL*NLAD_PXL*NSEN_PXL;
const Int_t N_IST = NLAD_IST*NSEN_IST;
Int_t Status_PXL[N_PXL];
Int_t Status_IST[N_IST];

//==============================================================================
// Global variables.
//------------------------------------------------------------------------------

// Implemented in MultiView.C
class MultiView;
MultiView* gMultiView = 0;

TEveTrackList *gTrackList = 0;
TEvePointSet  *gVtxList = 0;
TEvePointSet  *gHitList = 0;
TEvePointSet  *gTrackHitList = 0;
TRandom3 *gRandom = new TRandom3;
Int_t index = 0;
Int_t NMAX = 0;
TGeoManager *gGeoManager = new TGeoManager;

//==============================================================================
// Forward decalarations of CINT functions.
//------------------------------------------------------------------------------


//==============================================================================
// Main - hft_display()
//------------------------------------------------------------------------------

void hft_display(const double B = 1, const Int_t runnumber = 15037068)
{
  if (gROOT->LoadMacro("$ROOTSYS/tutorials/eve/MultiView.C+") != 0)
    {
      Error("hft_display()", "Failed loading MultiView.C in compiled mode.");
      return;
    }

  //========================================================================
  loadStatus(runnumber);
  //========================================================================

  //========================================================================
  //========================================================================
  TChain *chain = new TChain("T");
  char inname[100];
  sprintf(inname, "output/Event_%d.root",runnumber);
  chain->AddFile(inname);
  NMAX = chain->GetEntries();
  cout << " Total number of events = " << NMAX << endl;
  index = 0;
  
  t = new T(chain);
  
  //========================================================================
  // Create views and containers.
  //========================================================================
  
  TEveManager::Create();
  
  make_geometry();
  
  init(B);
  
  //========================================================================
  //========================================================================
  
  hft_make_gui();
  process_event(index);
  
  gEve->Redraw3D(kTRUE);
  
}

//========================================================================
void loadStatus(const int runnumber)
{
  // Read in the status - maybe moved int othe loop when loading multiple runs
  memset(Status_PXL, 0, sizeof(Status_PXL));
  memset(Status_IST, 0, sizeof(Status_IST));

  char inname[100];
  ifstream inData;
  sprintf(inname,"status/PXL_%d.txt",runnumber);
  inData.open(inname);
  for(int i=0;i<N_PXL;i++) {
    inData >> Status_PXL[i];
    int id = i+1;
    int sector, ladder, sensor;
    decodeId(id, &sector, &ladder, &sensor);
    if(Status_PXL[i]) cout << " Bad PXL Sensor # " << sector << "/" << ladder << "/" << sensor << endl;
  }
  inData.close();

  sprintf(inname,"status/IST_%d.txt",runnumber);
  inData.open(inname);
  for(int i=0;i<N_IST;i++) {
    inData >> Status_IST[i];
    int id = i+1+1000;
    int sector, ladder, sensor;
    decodeId(id, &sector, &ladder, &sensor);
    if(Status_IST[i]) cout << " Bad IST Sensor # " << ladder << "/" << sensor << endl;
  }
  inData.close();

}
//========================================================================
void decodeId(int id, int* sector, int* ladder, int* sensor)
{
  if(id>0 && id<=N_PXL) {
    *sector = (id-1)/(NSEC_PXL*NLAD_PXL) + 1;
    *ladder = (id-1)%(NSEC_PXL*NLAD_PXL)/NSEN_PXL + 1;
    *sensor = (id-1)%NSEN_PXL + 1;
  } else if(id>1000 && id<=1000+N_IST) {
    *sector = 0;
    *ladder = (id-1000-1)/NSEN_IST + 1;
    *sensor = (id-1000-1)%NSEN_IST + 1;
  }
}

//==============================================================================
// intitalize track/hit lists
//------------------------------------------------------------------------------


void init(const double B)
{
  gEve->GetBrowser()->GetTabRight()->SetTab(1);
  gTrackList = new TEveTrackList("Rec Tracks"); 
  gTrackList->SetMainColor(kYellow);
  gTrackList->SetMarkerColor(kRed);
  gTrackList->SetMarkerStyle(4);
  gTrackList->SetMarkerSize(0.5);
  gEve->AddElement(gTrackList);
  
  TEveTrackPropagator* trkProp = gTrackList->GetPropagator();
  trkProp->SetMagField(kMagField*B);
  // trkProp->SetMaxR(kR_TPC_max);
  // trkProp->SetMaxZ(kZ_TPC_d);
  trkProp->SetMaxR(kR_MAX);
  trkProp->SetMaxZ(kZ_MAX);
  
  gVtxList = new TEvePointSet("Primary Vertex");
  gVtxList->SetMainColor(kRed);
  gVtxList->SetMarkerColor(kYellow);
  gVtxList->SetMarkerStyle(20);
  gVtxList->SetMarkerSize(1.0);
  
  gHitList = new TEvePointSet("HFT Rec Hits");
  gHitList->SetMainColor(kRed);
  gHitList->SetMarkerColor(kWhite);
  gHitList->SetMarkerStyle(20);
  gHitList->SetMarkerSize(1.0);
  
  gTrackHitList = new TEvePointSet("Track Rec Hits");
  gTrackHitList->SetMainColor(kRed);
  gTrackHitList->SetMarkerColor(kYellow);
  gTrackHitList->SetMarkerStyle(20);
  gTrackHitList->SetMarkerSize(0.8);
  
}

//==============================================================================
// Next event
//------------------------------------------------------------------------------

void process_event(Int_t iEvt)
{
  if(iEvt>=NMAX) {
    cout << " End of the tree! Go backward! " << endl;
  } else if(iEvt<0) {
    cout << " Beginning of the tree! Go forward! " << endl;
  }
  
  cout << "begin " << index << "th entry...." << endl;
  t->GetEntry(iEvt);
  
  gTrackList->DestroyElements();
  gVtxList->Reset();
  gHitList->Reset();
  gTrackHitList->Reset();
  
  int runId = t->fEvtHdr_fRun;
  int evtId = t->fEvtHdr_fEvtNum;
  
  // Load verteice/hits
  cout << " Event vertex = " << t->fVertex[0] << " " << t->fVertex[1] << " " << t->fVertex[2] << endl;
  //  gVtxList->SetNextPoint(t->fVertex[0], t->fVertex[1], t->fVertex[2]);
  //  gEve->AddElement(gVtxList);
  
  Int_t nHits = t->fHits_;
  for(int j=0;j<nHits;j++) {
    int id = t->fHits_Id[j];
    if(id<1000 && ( t->fHits_nRawHits[j] <= kPXL_Cluster_Min ||  t->fHits_nRawHits[j] > kPXL_Cluster_Max ) ) continue;
    
    if(id<1000 && Status_PXL[id-1]) continue; // remove noisy channels
    if(id>1000 && Status_IST[id-1-1000]) continue;

    cout << " Adding a new hit " << id << " " <<  t->fHits_xG[j] << " " << t->fHits_yG[j] << " " << t->fHits_zG[j] << endl;
    gHitList->SetNextPoint(t->fHits_xG[j], t->fHits_yG[j], t->fHits_zG[j]);
  }
  gEve->AddElement(gHitList);
  
  
  
  // Load tracks
  TEveTrackPropagator *trkProp = gTrackList->GetPropagator();
  Int_t nTracks = t->fTracks_;
  for (Int_t j = 0; j < nTracks; ++j) {
    TEveVectorT<double> origin(t->fTracks_fOriginXDca[j], t->fTracks_fOriginYDca[j], t->fTracks_fOriginZDca[j]);      
    TEveVectorT<double> mom(t->fTracks_fPxDca[j], t->fTracks_fPyDca[j], t->fTracks_fPzDca[j]);
    Int_t charge = (t->fTracks_fNpoint[j]>0) ? +1 : -1;
    
    TEveRecTrackT<double> tR;
    tR.fIndex = j;
    tR.fP = mom;
    tR.fV = origin;
    tR.fSign = charge;
    
    TEveTrack* track = new TEveTrack(&tR, trkProp);
    track->SetName(Form("%s [%d]", "rec", j));
    track->SetStdTitle();
    track->SetAttLineAttMarker(gTrackList);
    if (charge == +1)
      track->SetLineColor(kColors[0]);
    else
      track->SetLineColor(kColors[1]);
    
    cout << " Adding a new track " << t->fTracks_fPx[j] << " " << t->fTracks_fPy[j] << " " << t->fTracks_fPz[j] << endl;
    gTrackList->AddElement(track);
    
    // add track hit
    
    for(int k=0; k<3; k++) {
      if(TMath::Sqrt(TMath::Power(t->fTracks_fPxlHitX[j][k],2)+TMath::Power(t->fTracks_fPxlHitY[j][k],2))<2.) continue;
      gTrackHitList->SetNextPoint(t->fTracks_fPxlHitX[j][k]+kTrackHitOffset, t->fTracks_fPxlHitY[j][k]+kTrackHitOffset, t->fTracks_fPxlHitZ[j][k]+kTrackHitOffset);
    }
    for(int k=0; k<2; k++) {
      if(TMath::Sqrt(TMath::Power(t->fTracks_fIstHitX[j][k],2)+TMath::Power(t->fTracks_fIstHitY[j][k],2))<2.) continue;
      gTrackHitList->SetNextPoint(t->fTracks_fIstHitX[j][k]+kTrackHitOffset, t->fTracks_fIstHitY[j][k]+kTrackHitOffset, t->fTracks_fIstHitZ[j][k]+kTrackHitOffset);
    }
    for(int k=0; k<2; k++) {
      if(TMath::Sqrt(TMath::Power(t->fTracks_fSsdHitX[j][k],2)+TMath::Power(t->fTracks_fSsdHitY[j][k],2))<2.) continue;
      gTrackHitList->SetNextPoint(t->fTracks_fSsdHitX[j][k]+kTrackHitOffset, t->fTracks_fSsdHitY[j][k]+kTrackHitOffset, t->fTracks_fSsdHitZ[j][k]+kTrackHitOffset);
    }
    for(int k=0; k<45; k++) {
      if(TMath::Sqrt(TMath::Power(t->fTracks_fTpcHitX[j][k],2)+TMath::Power(t->fTracks_fTpcHitY[j][k],2))<2.) continue;
      gTrackHitList->SetNextPoint(t->fTracks_fTpcHitX[j][k], t->fTracks_fTpcHitY[j][k], t->fTracks_fTpcHitZ[j][k]);
    }
    
    int nhits = abs(t->fTracks_fNpoint[j]);
    int nhits_tpc = nhits%100;
    int nhits_ssd = (nhits%1000)/100;
    int nhits_ist = (nhits%10000)/1000;
    int nhits_pxl2 = (nhits%100000)/10000;
    int nhits_pxl1 = (nhits%1000000)/100000;
    
    cout << " Number of hits on PXL1/PXL2/IST/SSD/TPC = " << nhits_pxl1 << "/" << nhits_pxl2 << "/" << nhits_ist << "/" << nhits_ssd << "/" << nhits_tpc << endl;
  }  
  gTrackList->MakeTracks();
  gEve->AddElement(gTrackHitList);
  
  gEve->SetStatusLine(Form("run#%d event#%d",runId,evtId));
  
  TEveElement* top = gEve->GetCurrentEvent();
  
  gMultiView->DestroyEventRPhi();
  gMultiView->ImportEventRPhi(top);
  
  gMultiView->DestroyEventRhoZ();
  gMultiView->ImportEventRhoZ(top);
  
  gEve->Redraw3D();
  
}

void selectDaughterVisible(TGeoNode *node, const char *name)
{
  int nn=node->GetVolume()->GetNdaughters();
  for(int i = 0; i< nn; i++) {
    TGeoNode *daughter = node->GetVolume()->GetNode(i);
    if(!daughter) continue;
    if(strstr(daughter->GetName(), name)!=0) {
      cout << "  Found this node " << daughter->GetName() << " set to be visible" << endl;
      daughter->GetVolume()->SetVisibility(1);
    } else {
      daughter->GetVolume()->SetVisibility(0);
      if(daughter->GetVolume()->GetNdaughters()!=0) {
	selectDaughterVisible(daughter, name);
      }
    }
  }
}

void make_geometry()
{
  TEveElementList *STAR = new TEveElementList("Geometry");
  
  // gROOT->LoadMacro("y2014.C");
  // y2014();
  gGeoManager = gEve->GetGeometry("star_2014.root");
  
  TGeoVolume* top = gGeoManager->GetTopVolume()->FindNode("CAVE_1")->GetVolume();
  
  TGeoNode *tpc_mom = top->FindNode("TPCE_1");
  selectDaughterVisible(tpc_mom, "TPCM");  // Central Membrane
  //  selectDaughterVisible(tpc_mom, "TSAW");  // 
  //  selectDaughterVisible(tpc_mom, "TWMR");
  //  selectDaughterVisible(tpc_mom, "TWRB"); // Sector ribs
  TEveGeoTopNode* tpc = new TEveGeoTopNode(gGeoManager, tpc_mom);
  tpc->SetMainTransparency(80);
  tpc->SetVisLevel(5);
  STAR->AddElement(tpc);
  
  TGeoNode *bp_mom = top->FindNode("IDSM_1")->GetVolume()->FindNode("PIPI_1");
  selectDaughterVisible(bp_mom, "PBES");
  TEveGeoTopNode* beampipe = new TEveGeoTopNode(gGeoManager, bp_mom);
  beampipe->SetMainTransparency(80);
  beampipe->SetVisLevel(3);
  STAR->AddElement(beampipe);
  
  TGeoNode *ssd_mom = top->FindNode("IDSM_1")->GetVolume()->FindNode("SFMO_1");
  if(!kPlotHFTSupport) selectDaughterVisible(ssd_mom, "SFSD");
  TEveGeoTopNode* ssd = new TEveGeoTopNode(gGeoManager, ssd_mom);
  ssd->SetVisLevel(6);
  //  STAR->AddElement(ssd);
  
  TGeoNode *ist_mom = top->FindNode("IDSM_1")->GetVolume()->FindNode("IBMO_1");
  if(!kPlotHFTSupport) selectDaughterVisible(ist_mom, "IBSS");
  TEveGeoTopNode* ist = new TEveGeoTopNode(gGeoManager, ist_mom);
  ist->SetVisLevel(6);
  STAR->AddElement(ist);
  
  TGeoNode *pxl_mom = top->FindNode("IDSM_1")->GetVolume()->FindNode("PXMO_1");
  if(!kPlotHFTSupport) selectDaughterVisible(pxl_mom, "PLAC");
  TEveGeoTopNode* pxl = new TEveGeoTopNode(gGeoManager, pxl_mom);
  pxl->SetVisLevel(6);
  STAR->AddElement(pxl);
  
  gEve->AddGlobalElement(STAR);
  
  gMultiView = new MultiView;
  gMultiView->ImportGeomRPhi(STAR);
  gMultiView->ImportGeomRhoZ(STAR);
}
//==============================================================================
// GUI stuff
//------------------------------------------------------------------------------
class EvNavHandler
{
public:
  void Fwd()
  {
    process_event(++index);
  }
  void Bck()
  {
    index--;
    process_event(index);
  }
};

//______________________________________________________________________________
void hft_make_gui()
{
  // Create minimal GUI for event navigation.
  
  TEveBrowser* browser = gEve->GetBrowser();
  browser->StartEmbedding(TRootBrowser::kLeft);
  
  TGMainFrame* frmMain = new TGMainFrame(gClient->GetRoot(), 400, 400);
  frmMain->SetWindowName("XX GUI");
  frmMain->SetCleanup(kDeepCleanup);
  
  TGHorizontalFrame* hf = new TGHorizontalFrame(frmMain);
  {
    TString icondir( Form("%s/icons/", gSystem->Getenv("ROOTSYS")) );
    TGPictureButton* b = 0;
    EvNavHandler    *fh = new EvNavHandler;
    
    b = new TGPictureButton(hf, gClient->GetPicture(icondir+"GoBack.gif"));
    //      b->SetEnabled(kFALSE);
    b->SetToolTipText("Go to previous event - not supported.");
    hf->AddFrame(b);
    b->Connect("Clicked()", "EvNavHandler", fh, "Bck()");
    
    b = new TGPictureButton(hf, gClient->GetPicture(icondir+"GoForward.gif"));
    b->SetToolTipText("Generate new event.");
    hf->AddFrame(b);
    b->Connect("Clicked()", "EvNavHandler", fh, "Fwd()");

  }
  frmMain->AddFrame(hf);
  
  frmMain->MapSubwindows();
  frmMain->Resize();
  frmMain->MapWindow();
  
  browser->StopEmbedding();
  browser->SetTabTitle("Event Control", 0);
}
