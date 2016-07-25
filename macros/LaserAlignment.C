/* 
   foreach f (`ls -1d 15*All.root`)
     set b = `basename ${f} .root`; root.exe -q -b 'makeAvLaserHits.C("'${f}'")' >& ${b}.Av.log &
   end
   Inner to Outer Alignment 
-------------------------------
   FPE_OFF
   foreach f (`ls -1d 15*.tags.root`)
     echo ${f}; set b = `basename ${f} .tags.root`; root.exe -q -b lLaser.C ${f} 'LaserAlignment.C+(kTRUE)'  >& ${b}.C.log &
   end
   root.exe 15*.Laser.root 'Chain.C("FitP")' lDb.C 'LaserTpcOuterSectorPositionB.C+(tchain,20140101,608)'
   cp *.C StarDb/Geometry/tpc/
   mkdir saveSteps/608 ; mv *.C *.Av.log *.C.log *event.root *aser.root *tags.root *.png saveSteps/608
-------------------------------
   Sector as whole to mirror aligment
-------------------------------
   FPE_OFF
   foreach f (`ls -1d 15*.tags.root`)
     echo ${f}; set b = `basename ${f} .tags.root`; root.exe -q -b lLaser.C ${f} 'LaserAlignment.C+(kFALSE)'  >& ${b}.M.log &
   end
   

*/
#include <string.h>
#include "Riostream.h"
#include "TFile.h"
#include "TTree.h"
#include "TChain.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TStopwatch.h"
#include "TMath.h"
#include "TGraphErrors.h"
#include "TMultiGraph.h"
#include "TGeoMatrix.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TH2.h"
#include "TF1.h"
#include "TNtuple.h"
#include "TVirtualFitter.h"
#include "TLinearFitter.h"
#include "TBits.h"
#include "StLaserAnalysisMaker/LaserEvent.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "TLegend.h"
#include "StChain.h"
#include "StEvtHddr.h"
#include "Ask.h"
#include <vector>
#include <map>
#include "TMinuit.h"
//#define __MEMBRANE__
//#define __NOT_MEMBRANE__
//#define __DEBUG__
#define __R123__
using namespace std;
TLinearFitter *lfit = 0;
LaserEvent *event = 0;
TNtuple *FitP = 0;
TNtuple *FitM = 0;
TFile *fOut = 0;
TTree *tree = 0;
TCanvas *c1 = 0;
TCanvas *c2 = 0;
TCanvas *c3 = 0;
TCanvas *c4 = 0;
Int_t nPng = 0;
TH2D *dev[2][2] = {{0,0}, {0,0}};
TH2D *devZ[2][2] = {{0,0}, {0,0}};
static Int_t _debug = 0;
//________________________________________________________________________________
class FittedSegment : public TObject {
public:
  FittedSegment() {memset (beg, 0, end-beg+1);}
  FittedSegment &operator+= (FittedSegment &t) {
    Float_t *vals1[12] = {  &pol1Iy0,   &pol1Iy1,   &pol1Iz0,   &pol1Iz1,   &pol1Oy0,   &pol1Oy1,   &pol1Oz0,   &pol1Oz1,    &dY,    &dYdX,    &dZ,    &dZdX};
    Float_t *errs1[12] = {  &dpl1Iy0,   &dpl1Iy1,   &dpl1Iz0,   &dpl1Iz1,   &dpl1Oy0,   &dpl1Oy1,   &dpl1Oz0,   &dpl1Oz1,   &ddY,   &ddYdX,   &ddZ,   &ddZdX};
    Float_t *vals2[12] = {&t.pol1Iy0, &t.pol1Iy1, &t.pol1Iz0, &t.pol1Iz1, &t.pol1Oy0, &t.pol1Oy1, &t.pol1Oz0, &t.pol1Oz1,  &t.dY,  &t.dYdX,  &t.dZ,  &t.dZdX};
    Float_t *errs2[12] = {&t.dpl1Iy0, &t.dpl1Iy1, &t.dpl1Iz0, &t.dpl1Iz1, &t.dpl1Oy0, &t.dpl1Oy1, &t.dpl1Oz0, &t.dpl1Oz1, &t.ddY, &t.ddYdX, &t.ddZ, &t.ddZdX};
    for (Int_t i = 0; i < 12; i++) {
      Float_t &v1 = *vals1[i]; 
      Float_t &e1 = *errs1[i];
      Float_t &v2 = *vals2[i]; 
      Float_t &e2 = *errs2[i];
      if (e2 <= 0) continue; 
      if (e1 <= 0.0) {v1 = v2; e1 = e2; ndfY = t.ndfY; ndfZ =t.ndfZ; continue;}
      if (e2 > 1e3) continue;
      if (i == 0) ndfY += t.ndfY;
      if (i == 6) ndfZ += t.ndfZ;
      Double_t w1 = 1./(e1*e1);
      Double_t w2 = 1./(e2*e2);
      Double_t vw = (v1*w1 + v2*w2)/(w1 + w2);
      v1 = vw;
      e1 = 1./TMath::Sqrt(w1+w2);
    }
    return *this;
  }
  void Check() {
    flag = ddY>0&&ddY<1e3&&ddZ<1e3&&ndfY>0&&ndfZ>0;
  }
  virtual void Print(const Char_t *opt = "") const {
    cout << Form("%4s",opt);
    if (sec < 10) cout << " ";
    cout << sec;  
    cout << Form(" dY = %8.2f +/- %8.2f[mkm] dZ =  %8.2f +/- %8.2f[mkm] dY/dX = %8.2f +/- %8.2f[mrad] ",
		 dY,ddY,dZ,ddZ,dYdX,ddYdX)
         << Form("dZ/dX = %8.2f +/- %8.2f[mrad] Z = %8.2f [cm]",dZdX,ddZdX,Z) 
	 << " ndfY = " << ndfY << " ndfZ = " << ndfZ << " secM " << secM << endl;
  }
  Char_t beg[1]; //!
  Float_t sec, trackID, Z, Imp,
    pol1Iy0, pol1Iy1, pol1Iz0, pol1Iz1, pol1Oy0, pol1Oy1, pol1Oz0, pol1Oz1, 
    dpl1Iy0, dpl1Iy1, dpl1Iz0, dpl1Iz1, dpl1Oy0, dpl1Oy1, dpl1Oz0, dpl1Oz1;
  Float_t dY, ddY, dYdX, ddYdX, dZ, ddZ, dZdX, ddZdX;
  Float_t flag;
  Float_t ndfY, ndfZ;
  Float_t xM, yM, zM; // mirror position in Sec. CS.
  Float_t secM;
  Char_t end[1]; //!
  ClassDef(FittedSegment,1)
};
//________________________________________________________________________________
class LaserTrackSegment : public TObject {
public:
  LaserTrackSegment(Track *track = 0, Int_t keyS = -1)  {
    memset (beg, 0, end-beg+1);
    fTrack = track;
    keySegment = keyS;
    sector = (keySegment/10)%100;
    SegTrack.sec = sector;
    inout = keySegment%10;
    if (fTrack) {
      keyTrack = fTrack->mKey;
      dca_Z = fTrack->fDca.z();
      dca_XY = track->fDca.impact();
    }
  }
  virtual ~LaserTrackSegment() {
  }
  void AddHit(Hit *hit) {hit->flag = 0; HitList.push_back(*hit);}
  vector<Hit> HitList;
  FittedSegment SegTrack;
  Char_t beg[1]; //!
  Track *fTrack;
  Int_t keyTrack;
  Int_t keySegment;
  Int_t sector;
  Int_t inout; // 0 => Inner, 1 => Outer, 2 => Inner and Outer; 
  Double_t dca_Z, dca_XY;		 
  Float_t xM, yM, zM; // mirror position in Sec. CS.
  //              yz  k, k = 0 for yz, k = 1 for dyz, k = 2 for pad and tb
  Float_t secM;
  TGraphErrors *yz[2][3];
  Char_t end[1]; //!
  void MakeIOGraphs();
  void MakeGraphs();
  ClassDef(LaserTrackSegment,1)
};
typedef map<UInt_t,LaserTrackSegment *> TrackSegmentMap;
typedef map<UInt_t,LaserTrackSegment *>::iterator TrackSegmentMapIter;
typedef vector<LaserTrackSegment *> TrackSegmentVect;
typedef multimap<UInt_t,UInt_t> Track2SegmentMap;
typedef multimap<UInt_t,UInt_t>::iterator Track2SegmentIter;
TrackSegmentVect TrackSegmentV;
//________________________________________________________________________________
void DrawPng(TCanvas *c) {
  TString pngName("");
  if (c) {
    c->Update(); 
    if (gDirectory) {pngName = gDirectory->GetName(); pngName.ReplaceAll(".root",".");}
    pngName += c->GetName();
    pngName.ReplaceAll(" ","_");
    pngName.ReplaceAll("(","_");
    pngName.ReplaceAll(")","_");
    pngName.ReplaceAll("{","_");
    pngName.ReplaceAll("}","_");
    pngName.ReplaceAll("<","lt");
    pngName.ReplaceAll(">","gt");
    pngName.ReplaceAll(".","_");
    pngName.ReplaceAll("/","_");
    pngName.ReplaceAll("^","_");
    pngName.ReplaceAll("__","_");
    pngName.ReplaceAll("__","_");
    pngName += ".png"; 
    TVirtualX::Instance()->WritePixmap(c->GetCanvasID(),-1,-1,(Char_t *)pngName.Data());
    nPng++;
    cout << "Draw #\t" << nPng << "\t" << pngName << endl;
  }
}
//________________________________________________________________________________
TGraphErrors *FitRobust(TGraphErrors *grr, Int_t k=0) {
  TF1 *Pol1 = (TF1 *) gROOT->GetFunction("Pol1");
  if (! Pol1) {
    //    pol1 = new TF1("Pol1","[0]+[1]*(x-123)",0,200);
    //    Pol1 = new TF1("Pol1","1++(x-123)",-100,100);
    Pol1 = new TF1("Pol1","pol1",-100,100);
    Pol1->SetLineColor(2);
  }
#if 0
  TF1 *pol1 = (TF1 *) gROOT->GetFunction("pol1");
  if (! pol1) {
    pol1 = new TF1("pol1","1++x",-100,100);
  }
#endif
  TGraphErrors *gr = 0;
  TGraphErrors *difgr = 0;
  if (! grr ) return gr;
  Int_t Np = grr->GetN();
  Int_t Niter = Np/2 - 2;
  if (Niter < 1) Niter = 1;
  gr = new TGraphErrors(*grr);
  gr->SetName(Form("%sNew",grr->GetName()));
  Bool_t ok = kFALSE;
  if (! gROOT->IsBatch()) {
    if (! c2) c2 = new TCanvas();
    else      c2->cd();
    if (! k) {
      c2->Clear();
      c2->Divide(2,2);
    }
    c2->cd(2*k+1);
    gr->Draw("ap");
    c2->Update();
  }
  Int_t iter = 0;
  do {
    Int_t N = gr->GetN();
    if (c2) c2->cd(2*k+1);
    if (! _debug) gr->Fit(Pol1,"q");
    else          gr->Fit(Pol1);
#if 0    
    TF1* pol1 = gr->GetFunction("Pol1");
#if 0
    gr->Fit(Pol1,"q+rob=0.75");
#else
    if (! lfit) {
      lfit = new TLinearFitter(2);
    }
    lfit->ClearPoints();
    lfit->SetFormula(Pol1);
    lfit->SetObjectFit(gr);
    Double_t h = 075;
    lfit->ExecuteCommand("FitGraph",&h,1);
#endif
#endif
    if (! gROOT->IsBatch()) {
      gr->SetMarkerStyle(20+iter+1);
      if (c2) {
	gr->Draw("axp");
	Pol1->Draw("same");
	c2->Update();
      }
      //      Ask();
    }
    TF1 *f = gr->GetFunction("Pol1");
    if (! f) return 0;
    Double_t *x=gr->GetX();
    Double_t *y=gr->GetY();
    Double_t *ey = gr->GetEY();
    TArrayD E(N); Double_t *e = E.GetArray();
    TArrayD  dY(N); Double_t *dy = dY.GetArray(); 
    Double_t chisq = 0;
    Double_t dev2B = 0;
    Int_t ibad = -1; 
    for (Int_t i = 0; i < N; i++) {
      dy[i] = y[i] - f->Eval(x[i]);
      Double_t dev = dy[i]/ey[i];
      Double_t dev2 = dev*dev;
      chisq += dev2;
      if (dev2 > dev2B) {
	dev2B = dev2;
	ibad = i;
      }
    }
    if (c2) {
      if (difgr) delete difgr;
      difgr = new TGraphErrors(N,x,dy,0,ey); 
      difgr->SetName(Form("dif%s",gr->GetName()));
      difgr->SetTitle(Form("dif%s",gr->GetTitle()));
      c2->cd(2*k+2); 
      difgr->Draw("axp");
    }
    if (ibad < 0 || chisq/N > 9*dev2B || Np > N + 5 || N <= 4 || iter >= Niter) {
      ok = kTRUE;
      if (! _debug) gr->Fit(Pol1,"q");
      else          gr->Fit(Pol1);
      break;
    } else {
      // take out the worst point
      TArrayD X(N-1); Double_t *xx = X.GetArray();
      TArrayD Y(N-1); Double_t *yy = Y.GetArray();
      TArrayD EE(N-1); Double_t *ee = EE.GetArray();
      Int_t j = 0;
      for (Int_t i = 0; i < N; i++) {
	if (i == ibad) continue;
	xx[j] = x[i];
	yy[j] = y[i];
	ee[j] = ey[i];
	j++;
      }
      TGraphErrors *grnew = new TGraphErrors(j, xx, yy, 0, ee);
      grnew->SetName(Form("%sNew_%i",grr->GetName(),iter));
      delete gr;
      gr = grnew;
      iter++;
    }
  } while (! ok);
  return gr;
}
//________________________________________________________________________________
void LaserTrackSegment::MakeIOGraphs() {
  static Int_t iBreak = 0;
  Int_t Nsize = HitList.size();
  if (Nsize < 4) return;
  Int_t N = Nsize+1;
  TArrayD X   = TArrayD(N);   Double_t *x   = X.GetArray();
  TArrayD Pad = TArrayD(N);   Double_t *pad = Pad.GetArray();
  TArrayD TB  = TArrayD(N);   Double_t *tb  = TB.GetArray();
  TArrayD Y   = TArrayD(N);   Double_t *y   = Y.GetArray();
  TArrayD Z   = TArrayD(N);   Double_t *z   = Z.GetArray();
  TArrayD dY  = TArrayD(N);   Double_t *dy  = dY.GetArray();
  TArrayD dZ  = TArrayD(N);   Double_t *dz  = dZ.GetArray();
#ifdef __R123__
  Double_t R123 = 123;
#else
  Double_t R123 = 0;
#endif
  Int_t j = 0;
  for (Int_t i = 0; i < Nsize; i++) {
    Hit &hit = HitList[i];
    if (hit.flag) continue;
    x[j]  =  hit.xyzS.x() - R123;
    y[j]  =  hit.xyzS.y();
    dy[j] =  hit.hit.positionError().y();
    z[j]  =  hit.xyzS.z();
    dz[j] =  hit.hit.positionError().z();
    pad[j] = hit.hit.pad()        - TMath::Nint(hit.hit.pad());
    tb[j]  = hit.hit.timeBucket() - TMath::Nint(hit.hit.timeBucket());
    j++;
  }
  if (fTrack && fTrack->Laser.Sector == sector && inout == 1) {
    x[j] =  fTrack->Laser.XyzS.x() - R123;
    y[j] =  fTrack->Laser.XyzS.y();
    z[j] =  fTrack->Laser.XyzS.z();
    dy[j] = 0.1;
    dz[j] = 0.1;
    pad[j] = -1;
    tb[j]  = -1;
    j++;
  }
  N = j;
  const Char_t *gName[2] = {"yFit","zFit"};
  for (Int_t k = 0; k < 2; k++) {
    if (! k) yz[k][0] = new TGraphErrors(N, x, y, 0, dy); 
    else     yz[k][0] = new TGraphErrors(N, x, z, 0, dz); 
    yz[k][0]->SetName(gName[k]); yz[k][0]->SetTitle(gName[k]); 
    TGraphErrors *gr = FitRobust(yz[k][0],k);
    SafeDelete(yz[k][0]);
    if (gr) {
      yz[k][0] = gr;
      TF1 *pol1 = (TF1 *) gr->GetFunction("Pol1");
      if (pol1) pol1->SetLineColor(2+inout);
    }
  }
  if (! yz[0][0] || ! yz[1][0]) return;
  for (Int_t k = 0; k < 2; k++) {
    TF1 *pol1 = (TF1 *) yz[k][0]->GetFunction("Pol1");
    if (pol1) {
      N = yz[k][0]->GetN();
      for (Int_t j = 0; j < N; j++) {
	Double_t Z = z[j];
	if (k == 0) {y[j] -= pol1->Eval(x[j]); dev[k][inout]->Fill(pad[j],y[j]); devZ[k][inout]->Fill(Z,y[j]);}
	else        {z[j] -= pol1->Eval(x[j]); dev[k][inout]->Fill(tb[j],z[j]);  devZ[k][inout]->Fill(Z,z[j]);}
      }
      if (k == 0) {
	yz[k][1] = new TGraphErrors(N, x, y, 0, dy); 
	yz[k][2] = new TGraphErrors(N, pad, y, 0, dy); 
      } else {
	yz[k][1] = new TGraphErrors(N, x, z, 0, dz); 
	yz[k][2] = new TGraphErrors(N, tb,  z, 0, dz); 
      }
    } else {
      SafeDelete(yz[k][0]);
    }
  }
}
//________________________________________________________________________________
void FillSegments(vector<Int_t>  &trackVect, Track2SegmentMap &MapTrack2Segment, TrackSegmentMap &trackSegments, Int_t k1 = 0) {
  // k1 = 0 => separate inner and outer segments, k1 = 1 don't 
  tree = (TTree *) gDirectory->Get("laser");
  if (! tree ) return;
  TBranch *branch = tree->GetBranch("event");
  if (! branch) return;
  branch->SetAddress(&event);
  TString NameOut(gDirectory->GetName());
#if 1
  StMaker *chain = StChain::GetChain();
  StMaker *laser = chain->Maker("Laser");
  laser->SetActive(kFALSE);
  StEvtHddr* EvtHddr = (StEvtHddr*) chain->GetDataSet("EvtHddr");
  if (! EvtHddr) {cout << "No Header" << endl; return;}
  Int_t nFound = tree->Draw("fEvtHdr.fRun:fEvtHdr.fDate:fEvtHdr.fTime","","goff");
  if ( nFound < 1) {cout << "Date/Time is not found" << endl; return;}
  Int_t run = tree->GetV1()[0];
  Int_t date = tree->GetV2()[0];
  Int_t time = tree->GetV3()[0];
  EvtHddr->SetRunNumber(run);
  EvtHddr->SetDateTime(date,time);
  chain->Init();
  chain->Make();
#endif
#ifdef __MEMBRANE__
  NameOut.ReplaceAll(".root",".LasMembrane.root");
#elif defined( __NOT_MEMBRANE__)
  NameOut.ReplaceAll(".root",".LasNonMemb.root");
#else /*  __NOT_MEMBRANE__ */
  NameOut.ReplaceAll(".root",".Laser.root");
  if (k1) NameOut.ReplaceAll(".Laser.root",".LaserM.root");
#endif
  fOut = TFile::Open(NameOut,"recreate");
  for (Int_t iyz = 0; iyz < 2; iyz++) {
    for (Int_t io = 0; io < 2; io++) {
      TString Name((!iyz) ? "dY" : "dZ");
      TString Title("deviation ");
      Title += (! iyz) ? "Y" : "Z";
      Name += (!io) ? "I" : "O";
      Title += (!io) ? " for Inner" : " for Outer";
      Title += " sector";
      dev[iyz][io] = new TH2D(Name,Title,64,-0.5,0.5,500,-0.25,0.25);
      Name  += "Z";
      Title += " versus Z";
      devZ[iyz][io] = new TH2D(Name,Title,210,-210,210,500,-0.25,0.25);
    }
  }
  //  static Int_t iBreak = 0;
  Track2SegmentIter it;
  pair<Track2SegmentIter,Track2SegmentIter> ret;
  Int_t nentries = (Int_t)tree->GetEntries();
  Long64_t nb = 0;
  //by setting the read cache to -1 we set it to the AutoFlush value when writing
  Int_t cachesize = -1; 
  Int_t punzip = 0; // punzip = 1;  //read sequential + parallel unzipping
  if (punzip) tree->SetParallelUnzip();
  tree->SetCacheSize(cachesize);
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nentries);
  for (Int_t ev = 0; ev < nentries; ev++) {
    tree->LoadTree(ev);  //this call is required when using the cache
    nb += tree->GetEntry(ev);        //read complete event in memory
    cout << "Event " << ev << " Run " << event->GetHeader()->GetRun() << endl; 
    Int_t NTracks = event->GetNtrack();
    for (Int_t trackI = 0; trackI < NTracks; trackI++) {
      Track *track = (Track *) event->GetTracks()->UncheckedAt(trackI);
#ifdef __MEMBRANE__
      // Membrane tracks
      if (TMath::Abs(track->fDca.impact()) > 5 ||
	  TMath::Abs(track->fDca.z())      > 5) continue;
#else /* ! __MEMBRANE__ */
#ifdef __NOT_MEMBRANE__
      if (! (TMath::Abs(track->fDca.impact()) > 5 ||
	     TMath::Abs(track->fDca.z())      > 5)) continue;
#endif /*  __NOT_MEMBRANE__ */
#endif /* __MEMBRANE__ */
#ifdef __DEBUG__
      cout << "track " << trackI << " key " << track->mKey << " Length " << track->mLength 
	   << " dca_XY " << track->fDca.impact() << " dca_Z " << track->fDca.z() 
	   << " NFP " << track->mNumberOfFitPointsTpc << endl;
#endif
      UInt_t trackKey = 10000*ev+track->mKey;
      trackVect.push_back(trackKey);
      Int_t Nhits = event->GetNhit();
      for (Int_t hitJ = 0; hitJ < Nhits; hitJ++) {
	Hit *hit = (Hit *) event->GetHits()->UncheckedAt(hitJ);
	if (hit->trackKey != track->mKey) continue;
	//	for (Int_t k = 0, k < 2; k++) { // k = 0 make segments separated for Inner and Outer, k = 1 make whole segment track 
	Int_t k = k1;
	Int_t io = 0; 
	if ( hit->hit.padrow() > 13) io = 1;
	if (k == 1) io = 2;
	Int_t sector = hit->hit.sector();
	LaserTrackSegment *trackSegment = 0;
#ifdef __DEBUG__
	cout << hit->hit << endl;
#endif
	UInt_t segmentKey = 1000*trackKey + 10*sector + io;
	Int_t nseg = MapTrack2Segment.count(trackKey);
#ifdef __DEBUG__
	cout << "hit key = " << hit->trackKey << " sector = " << hit->hit.sector() 
	     << " io = " << io << " nseg = " << nseg << endl;
#endif
	if (nseg) {
	  ret = MapTrack2Segment.equal_range(trackKey);
	  for (it = ret.first; it != ret.second; it++) {
	    Int_t keyS = (*it).second;
	    trackSegment = trackSegments[keyS];
	    if (! trackSegment) continue;
#ifdef __DEBUG__
	    cout << "track segment key = " <<  trackSegment->keyTrack 
		 << " sector = " << trackSegment->sector 
		 << " io = " << trackSegment->inout << " trackSegment = " << trackSegment 
		 << " hits = " << trackSegment->HitList.size()
		 << endl;
#endif
	    if (trackSegment->sector == (Int_t) hit->hit.sector() && trackSegment->inout == io) break;
	    trackSegment = 0;
	  }
	}
	if (! trackSegment) {
	  trackSegments[segmentKey] = new LaserTrackSegment(track,segmentKey);
	  trackSegment = trackSegments[segmentKey];
	  MapTrack2Segment.insert(pair<UInt_t,UInt_t>(trackKey,segmentKey));
	}
	trackSegment->AddHit(hit);
      }
    }
  }
}
//________________________________________________________________________________
Int_t FitIOSegments(UInt_t sec, vector<Int_t>  &trackVect, Track2SegmentMap &MapTrack2Segment, TrackSegmentMap &trackSegments) {
  if (! FitP) {
    fOut->cd();
    const Char_t *vars = 
      "sec:trackID:Z:Imp:pol1Iy0:pol1Iy1:pol1Iz0:pol1Iz1:pol1Oy0:pol1Oy1:pol1Oz0:pol1Oz1:dpl1Iy0:dpl1Iy1:dpl1Iz0:dpl1Iz1:dpl1Oy0:dpl1Oy1:dpl1Oz0:dpl1Oz1"
      ":dY:ddY:dYdX:ddYdX:dZ:ddZ:dZdX:ddZdX:flag:ndfY:ndfZ";
    FitP = new TNtuple("FitP","fits from both inner and outer sectors",vars);
  }
  static FittedSegment SegTrackAv;
  if (sec > 24) {
    if (SegTrackAv.sec > 0) {
      SegTrackAv.Check();
      if (SegTrackAv.flag) {
	SegTrackAv.Print("Av.");
	FitP->Fill(&SegTrackAv.sec);
      }
    }
    return 0;
  }
  c1->Clear();
  c1->SetName(Form("IOyz%02i",sec));
  c1->SetTitle(Form("Inner Outer Y and Z fits for sector = %02i",sec));
  c1->Divide(1,4);
  struct Limit_t {Double_t xmin, xmax, ymin, ymax;};
  //               io xd yz
  const Limit_t xy[2][2] = {
    {{ 50,200, -35, 35}, { 50,200, -2.0, 2.0}}, {{ 50,200, -.2, .2}, { 50,200, -.2, .2}}
  };
  const Char_t *Title[4][2] = {
    {"x_%02i","z_%02i"},
    {"dY_%02i","dZ_%02i"},
    {"x_%02i","z_%02i"},
    {"dY_%02i","dZ_%02i"}
  };
  Int_t nfoundInSector = 0;
  for (Int_t iyz = 0; iyz < 2; iyz++) {
    for (Int_t k = 0; k < 2; k++) { // k => 1 residuals
      TVirtualPad *pad = c1->cd(2*iyz + k +1);
#ifdef __R123__
      TH1F *frame = pad->DrawFrame(xy[k][iyz].xmin - 123,xy[k][iyz].ymin,
     				   xy[k][iyz].xmax - 123,xy[k][iyz].ymax);
#else
      TH1F *frame = pad->DrawFrame(xy[k][iyz].xmin,xy[k][iyz].ymin,
				   xy[k][iyz].xmax,xy[k][iyz].ymax);
#endif
      frame->SetXTitle("x (cm)");
      frame->SetTitle(Form(Title[k][iyz],sec));
    }
  }   
  TCanvas *c = 0;
  UInt_t Ntrk = trackVect.size();
  Int_t trackcolor = 1;
  Track2SegmentIter it;
  pair<Track2SegmentIter,Track2SegmentIter> ret;
  for (UInt_t trackI = 0; trackI < Ntrk; trackI++) {
    UInt_t TrackID = trackVect[trackI];
    ret = MapTrack2Segment.equal_range(TrackID);
    if ((*ret.first).first != TrackID) continue;
    LaserTrackSegment *trackSegmentIO[2] = {0,0};
    for (it = ret.first; (*it).first != (*ret.second).first; ++it) {
  //  cout << "*it " << (*it).first  << "\t" << (*it).second  << endl;

      Int_t keyS = (*it).second;
//       size_t size  = trackSegments.size();
      LaserTrackSegment *trackSegment = trackSegments[keyS];
      if (! trackSegment) continue;
      if ((Int_t) sec != trackSegment->sector) continue;
      for (Int_t io = 0; io < 2; io++) {
	if (io  != trackSegment->inout) continue;
	Int_t N = trackSegment->HitList.size();
	if (N < 3) continue;
	trackSegmentIO[io] = trackSegment;
	//	cout << "Sector = " << sec << " " <<  io << endl;
	trackSegment->MakeIOGraphs();
	nfoundInSector++;
	for (Int_t iyz = 0; iyz < 3; iyz++) {
	  if (! trackSegment->yz[0][0] || ! trackSegment->yz[1][0]) continue;
	  if (iyz < 2) {
	    for (Int_t k = 0; k < 2; k++) { // k => 1 residuals
	      // 		if (k < 2) {
	      c = c1; c->cd(2*iyz + k +1);
	      // 		}
	      // 		else       {c = c3; c->cd(2*iyz + io + 1);}
	      if (trackSegment->yz[iyz][k]) {
		trackSegment->yz[iyz][k]->SetMarkerColor(trackcolor);
		trackSegment->yz[iyz][k]->Draw("P");
		c->Update();
	      }
	    }
	  }
	}
      }
    }
    if (! trackSegmentIO[0] || ! trackSegmentIO[1]) continue;
    if (trackSegmentIO[0]->yz[0][0] && trackSegmentIO[1]->yz[0][0] &&
	trackSegmentIO[0]->yz[1][0] && trackSegmentIO[1]->yz[1][0]) {
      TF1 *Pol1Iy = trackSegmentIO[0]->yz[0][0]->GetFunction("Pol1");
      TF1 *Pol1Oy = trackSegmentIO[1]->yz[0][0]->GetFunction("Pol1");
      TF1 *Pol1Iz = trackSegmentIO[0]->yz[1][0]->GetFunction("Pol1");
      TF1 *Pol1Oz = trackSegmentIO[1]->yz[1][0]->GetFunction("Pol1");
      if (Pol1Iy && Pol1Oy &&
	  Pol1Iz && Pol1Oz) {
	FittedSegment &SegTrack = trackSegmentIO[0]->SegTrack;
	SegTrack.ndfY = Pol1Oy->GetNDF() +  Pol1Iy->GetNDF();
	SegTrack.dY    = 1e4*(Pol1Oy->GetParameter(0) - Pol1Iy->GetParameter(0)); 
	SegTrack.ddY   = 1e4*(TMath::Sqrt(Pol1Oy->GetParError(0)*Pol1Oy->GetParError(0) +
					  Pol1Iy->GetParError(0)*Pol1Iy->GetParError(0)));
	SegTrack.dYdX  = 1e3*(Pol1Oy->GetParameter(1) - Pol1Iy->GetParameter(1));
	SegTrack.ddYdX = 1e3*(TMath::Sqrt(Pol1Oy->GetParError(1)*Pol1Oy->GetParError(1) +
					  Pol1Iy->GetParError(1)*Pol1Iy->GetParError(1)));
	SegTrack.ndfZ = Pol1Oz->GetNDF() +  Pol1Iz->GetNDF();
	SegTrack.dZ    = 1e4*(Pol1Oz->GetParameter(0) - Pol1Iz->GetParameter(0)); 
	SegTrack.ddZ   = 1e4*(TMath::Sqrt(Pol1Oz->GetParError(0)*Pol1Oz->GetParError(0) +
					  Pol1Iz->GetParError(0)*Pol1Iz->GetParError(0)));
	SegTrack.dZdX  = 1e3*(Pol1Oz->GetParameter(1) - Pol1Iz->GetParameter(1));
	SegTrack.ddZdX = 1e3*(TMath::Sqrt(Pol1Oz->GetParError(1)*Pol1Oz->GetParError(1) +
					  Pol1Iz->GetParError(1)*Pol1Iz->GetParError(1)));
	//	    Track *track = (Track *) event->GetTracks()->UncheckedAt(TrackID%10000);
	SegTrack.sec     = trackSegmentIO[0]->sector;
	SegTrack.trackID = trackSegmentIO[0]->keyTrack;
	SegTrack.Z       = trackSegmentIO[0]->dca_Z;
	SegTrack.Imp     = trackSegmentIO[0]->dca_XY;
	SegTrack.pol1Iy0 = Pol1Iy->GetParameter(0);
	SegTrack.pol1Iy1 = Pol1Iy->GetParameter(1);
	SegTrack.pol1Iz0 = Pol1Iz->GetParameter(0);
	SegTrack.pol1Iz1 = Pol1Iz->GetParameter(1);
	SegTrack.pol1Oy0 = Pol1Oy->GetParameter(0);
	SegTrack.pol1Oy1 = Pol1Oy->GetParameter(1);
	SegTrack.pol1Oz0 = Pol1Oz->GetParameter(0);
	SegTrack.pol1Oz1 = Pol1Oz->GetParameter(1);
	SegTrack.dpl1Iy0 = Pol1Iy->GetParError(0);
	SegTrack.dpl1Iy1 = Pol1Iy->GetParError(1);
	SegTrack.dpl1Iz0 = Pol1Iz->GetParError(0);
	SegTrack.dpl1Iz1 = Pol1Iz->GetParError(1);
	SegTrack.dpl1Oy0 = Pol1Oy->GetParError(0);
	SegTrack.dpl1Oy1 = Pol1Oy->GetParError(1);
	SegTrack.dpl1Oz0 = Pol1Oz->GetParError(0);
	SegTrack.dpl1Oz1 = Pol1Oz->GetParError(1);
	SegTrack.Check();
	if (! SegTrack.flag) continue;
	SegTrack.Print();
	trackSegmentIO[1]->SegTrack = SegTrack;
	FitP->Fill(&SegTrack.sec);
	if (! SegTrack.flag)  continue;
	if (SegTrackAv.sec != SegTrack.sec) {
	  if (SegTrackAv.sec > 0) {	
	    SegTrackAv.Check();
	    if (SegTrackAv.flag) {
	      SegTrackAv.Print("Av.");
	      FitP->Fill(&SegTrackAv.sec);
	    }
	  }
	  SegTrackAv = SegTrack; 
	  SegTrackAv.trackID = 0;
	} else {
          SegTrackAv += SegTrack;
	  SegTrackAv.Print("Add.");
	}
      }
    }
    trackcolor++;
    if (trackcolor > 8) trackcolor = 1;
  }
  return nfoundInSector;
}
//________________________________________________________________________________
void LaserAlignmentIO() { // Int_t secMin = 1, Int_t secMax = 24) {
  vector<Int_t>          trackVect;
  Track2SegmentMap  MapTrack2Segment;
  MapTrack2Segment.insert(pair<UInt_t,UInt_t>(0,0));
  //  bool(*fn_pt)(Int_t,Int_t) = fncomp;
  //  map<Int_t,LaserTrackSegment,bool(*)(Int_t,Int_t)> trackSegments(fn_pt);
  TrackSegmentMap trackSegments;
  Track2SegmentIter it;
  pair<Track2SegmentIter,Track2SegmentIter> ret;
  
  FillSegments(trackVect, MapTrack2Segment, trackSegments);
  c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("IOyz");
  if (! c1 ) c1 = new TCanvas("IOyz","Inner Outer X and Z fits" ,900,1000);
  for (UInt_t sec = 1; sec <= 25; sec++) {
    if (! FitIOSegments(sec, trackVect, MapTrack2Segment,trackSegments )) continue;
    if (! gROOT->IsBatch() && Ask()) return;
    //    DrawPng(c1);
  }
  // clean 

  for (TrackSegmentMapIter it=trackSegments.begin(); it!=trackSegments.end(); ++it) {
    if ((*it).second) {
      //      std::cout << (*it).first << " => " << (*it).second << '\n';
      delete (*it).second;
      (*it).second = 0;
    }
  }
  fOut->Write();
}
//________________________________________________________________________________
void Draw() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  //  TChain *chain = new TChain("FitP");
  Int_t NF = 0;
  while ( (f = (TFile *) next()) ) { 
    FitFiles[NF] = f; NF++;
    //    chain->Add(f->GetName());
  }  
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","Resolution versus Track Length");
  c1->Clear();
  c1->Divide(2,2);
  const Char_t *ytitles[4] = {"dY [#mum]", "dY/dX [mrad]", "dZ [#mum]", "dZ/dX [mrad]"};
  const Char_t *histn[4]   = {"dYsec","dYdXsec",
			      "dZsec","dZdXsec"};
  const Char_t *plot[4]    = {"dY:sec", "dYdX:sec", 
			      "dZ:sec", "dZdX:sec"};
  
  TTree *FitP = 0;
  TString label("All");
  TLegend **l = new TLegend *[NF];
  for (Int_t i = 0; i < 4; i++) {
    Double_t ymax = 1500;
    if (i%2) ymax = 5;
    TH1F *frame = c1->cd(i+1)->DrawFrame(0.5,-ymax,24.5,ymax);
    frame->GetXaxis()->SetLabelSize(0.1);
    frame->GetYaxis()->SetLabelSize(0.1);
    frame->SetXTitle("sector");
    frame->SetTitle(ytitles[i]);
    for (Int_t k = 0; k < NF; k++) {
      if (k == 0)  {l[i] = new TLegend(0.15,0.6,0.25,0.9); l[i]->Draw();}
      //       if (k < 0) {
      // 	label = "All";
      // 	FitP  = chain;
      // 	f = (TFile *) gDirectory;
      //       } else {
      f = FitFiles[k];
      f->cd();
      label = gSystem->BaseName(f->GetName()); 
      label.ReplaceAll("L.root",""); cout << "Label " << label.Data() << endl;
      FitP = (TTree *) f->Get("FitP");
      //      }
      FitP->SetMarkerColor(k+1);
      FitP->Draw(Form("%s >> %s%i(24,0.5,24.5)",plot[i],histn[i],k+1),"!trackID","profsame");
      TH1 *h = (TH1 *) gDirectory->Get(Form("%s%i",histn[i],k+1));
      if (! h) continue;
      l[i]->AddEntry(h,label);
    }
  }
}
//________________________________________________________________________________
void LaserTrackSegment::MakeGraphs() { // fit segment to mirror
  static Int_t iBreak = 0;
  if (inout != 2) return;
  if (fTrack && ! fTrack->Laser.Sector) return;
  Int_t Nsize = HitList.size();
  if (Nsize < 4) return;
  StThreeVectorD xyzS = fTrack->Laser.XyzS;
  if (sector != fTrack->Laser.Sector) {
    //    return;
    StThreeVectorD xyzT;
    StTpcDb::instance()->SupS2Tpc(fTrack->Laser.Sector).LocalToMaster(fTrack->Laser.XyzS.xyz(), xyzT.xyz());
    StTpcDb::instance()->SupS2Tpc(sector).MasterToLocal(xyzT.xyz(), xyzS.xyz());
#if 0
    cout << "sector " << sector << " Laser from sector " << fTrack->Laser.Sector << ":" 
	 << " LS: " << fTrack->Laser.XyzS << " => " << xyzT 
	 << " LT: " << fTrack->Laser.XyzL << " => " << xyzS 
	 << " Flag:" << fTrack->Flag << endl;
#endif
  }
  xM = xyzS.x();
  yM = xyzS.y();
  zM = xyzS.z();
  secM = fTrack->Laser.Sector;
  TF1 *Pol1 = (TF1 *) gROOT->GetFunction("Pol1");
  if (! Pol1) {
    //    pol1 = new TF1("Pol1","[0]+[1]*(x-123)",0,200);
    //    Pol1 = new TF1("Pol1","1++(x-123)",-100,100);
    Pol1 = new TF1("Pol1","1++x",-100,100);
  }
#if 0
  if (! lfit) {
    lfit = new TLinearFitter(2);
    lfit->SetFormula(Pol1);
  }
  lfit->ClearPoints();
  TArrayD X[2], Y[2], Z[2], dY[2], dZ[2];
  for (Int_t iter = 0; iter < 2; iter++) {
    Int_t N = 0;
    for (Int_t j = 0; j < Nsize; j++) {if (HitList[j].flag == 0) N++;}
    if (N < 3) break;
    TBits bits(N);
    X[iter]   = TArrayD(N);   Double_t *x   = X[iter].GetArray();
    Y[iter]   = TArrayD(N);   Double_t *y   = Y[iter].GetArray();
    Z[iter]   = TArrayD(N);   Double_t *z   = Z[iter].GetArray();
    dY[iter]  = TArrayD(N);   Double_t *dy  = dY[iter].GetArray();
    dZ[iter]  = TArrayD(N);   Double_t *dz  = dZ[iter].GetArray();
    Int_t noRejected = 0;
    Int_t j = 0;
    for (Int_t i = 0; i < Nsize && j < N; i++) {
      Hit &hit = HitList[i];
      if (hit.flag) continue;
      x[j]  =  hit.xyzS.x() - xyzS.x();
      y[j]  =  hit.xyzS.y() - xyzS.y();
      dy[j] =  hit.hit.positionError().y();
      //      if (dy[j] < 0.0050) dy[j] = 0.0050;
      z[j]  =  hit.xyzS.z() - xyzS.z();
      dz[j] =  hit.hit.positionError().z();
      if (dz[j] < 0.0050) dz[j] = 0.0050;
      j++;
    }
    N = j;
    if (! iter) {
      for (Int_t k = 0; k < 2; k++) {
	if (!k) lfit->AssignData(N,1,x,y,dy);
	else    lfit->AssignData(N,1,x,z,dz);
	//	if (lfit->Eval()) {
	Double_t h = (N-1.)/2/N;
	if (h < 0.75) h = 0.75;
	lfit->EvalRobust(h);
	lfit->GetFitSample(bits);
	TVectorD params;
	//      TVectorD errors;
	lfit->GetParameters(params);
	//      lfit->GetErrors(errors);
	for (Int_t l = 0; l < N; l++) {
	  if (HitList[l].flag) continue;
	  if (! bits.TestBitNumber(l)) {HitList[l].flag = 1; noRejected++;} 
	}
	cout << "Rejected " << noRejected << " from " << N << " total." << endl;
      }
    }
    if (iter || ! noRejected) {
      for (Int_t k = 0; k < 2; k++) {
	if (! k) yz[k][0] = new TGraphErrors(N, x, y, 0, dy); 
	else     yz[k][0] = new TGraphErrors(N, x, z, 0, dz); 
	TF1 *Pol1 = (TF1 *) gROOT->GetFunction("Pol1");
	Pol1->SetLineColor(2+inout);
	if (! _debug) yz[k][0]->Fit(Pol1,"q");
	else          yz[k][0]->Fit(Pol1);
      }
      break;
    }
  } // end of iter
  delete lfit; lfit = 0;
#else
  Int_t N = HitList.size();
  if (N < 4) return;
  N++;
  TArrayD X   = TArrayD(N);   Double_t *x   = X.GetArray();
  TArrayD Y   = TArrayD(N);   Double_t *y   = Y.GetArray();
  TArrayD Z   = TArrayD(N);   Double_t *z   = Z.GetArray();
  TArrayD dY  = TArrayD(N);   Double_t *dy  = dY.GetArray();
  TArrayD dZ  = TArrayD(N);   Double_t *dz  = dZ.GetArray();
  Int_t j = 0;
  x[j] = 0;
  y[j] = 0;
  z[j] = 0;
  dy[j] = 0.2;
  dz[j] = 0.2;
  j++;
  for (Int_t i = 0; i < Nsize; i++) {
    Hit &hit = HitList[i];
    if (hit.flag) continue;
    x[j]  =  hit.xyzS.x() - xyzS.x();
    y[j]  =  hit.xyzS.y() - xyzS.y();
    dy[j] =  hit.hit.positionError().y();
    //      if (dy[j] < 0.0050) dy[j] = 0.0050;
    z[j]  =  hit.xyzS.z() - xyzS.z();
    dz[j] =  hit.hit.positionError().z();
    if (dz[j] < 0.0050) dz[j] = 0.0050;
    j++;
  }
  N = j;
  const Char_t *gName[2] = {"yFit","zFit"};
  for (Int_t k = 0; k < 2; k++) {
    if (! k) yz[k][0] = new TGraphErrors(N, x, y, 0, dy);  
    else     yz[k][0] = new TGraphErrors(N, x, z, 0, dz); 
    yz[k][0]->SetName(gName[k]); yz[k][0]->SetTitle(gName[k]); 
    TGraphErrors *gr = FitRobust(yz[k][0],k);
    SafeDelete(yz[k][0]);
    if (gr) {
      yz[k][0] = gr;
      TF1 *pol1 = (TF1 *) gr->GetFunction("Pol1");
      if (pol1) pol1->SetLineColor(2+inout);
    }
  }
#endif
  if (! yz[0][0] || ! yz[1][0]) return;
  for (Int_t k = 0; k < 2; k++) {
    Int_t N = yz[k][0]->GetN();
    TF1 *Pol1 = (TF1 *) yz[k][0]->GetFunction("Pol1");
    if (Pol1 && TMath::Abs(Pol1->Eval(0)) < 1.0) {
      TArrayD X   = TArrayD(N, yz[k][0]->GetX());   Double_t *x   = X.GetArray();
      TArrayD Y   = TArrayD(N, yz[k][0]->GetY());   Double_t *y   = Y.GetArray();
      TArrayD dY  = TArrayD(N, yz[k][0]->GetEY());  Double_t *dy  = dY.GetArray();
      for (Int_t j = 0; j < N; j++) y[j] -= Pol1->Eval(x[j]);
      yz[k][1] = new TGraphErrors(N, x, y, 0, dy); 
    } else {
      SafeDelete(yz[k][0]);
    }
  }
}
//________________________________________________________________________________
Int_t FitSegments2Mirror(UInt_t sec, vector<Int_t>  &trackVect, Track2SegmentMap &MapTrack2Segment, TrackSegmentMap &trackSegments) {
  if (! FitP) {
    fOut->cd();
    const Char_t *vars = 
      "sec:trackID:Z:Imp:pol1Iy0:pol1Iy1:pol1Iz0:pol1Iz1"
      ":pol1Oy0:pol1Oy1:pol1Oz0:pol1Oz1:dpl1Iy0:dpl1Iy1:dpl1Iz0:dpl1Iz1:dpl1Oy0:dpl1Oy1:dpl1Oz0:dpl1Oz1"
      ":dY:ddY:dYdX:ddYdX:dZ:ddZ:dZdX:ddZdX:flag:ndfY:ndfZ"
      ":xM:yM:zM:secM";
    FitP = new TNtuple("FitP","fits sector track to mirrors",vars);
 }
  c1->Clear();
  c1->SetName(Form("IOyz%02i",sec));
  c1->SetTitle(Form("Inner Outer Y and Z fits for sector = %02i",sec));
  c1->Divide(1,4);
  struct Limit_t {Double_t xmin, xmax, ymin, ymax;};
  //               io xd yz
  const Limit_t xy[2][2] = {
    {{ -120,10, -150, 150}, { -120,10, -20.0, 20.0}}, {{ -120,10, -.2, .2}, { -120,10, -.2, .2}}
  };
  const Char_t *Title[4][2] = {
    {"x_%02i","z_%02i"},
    {"dY_%02i","dZ_%02i"},
    {"x_%02i","z_%02i"},
    {"dY_%02i","dZ_%02i"}
  };
  Int_t nfoundInSector = 0;
  for (Int_t iyz = 0; iyz < 2; iyz++) {
    for (Int_t k = 0; k < 2; k++) { // k => 1 residuals
      TVirtualPad *pad = c1->cd(2*iyz + k +1);
      TH1F *frame = pad->DrawFrame(xy[k][iyz].xmin,xy[k][iyz].ymin,
				   xy[k][iyz].xmax,xy[k][iyz].ymax);
      frame->SetXTitle("x (cm)");
      frame->SetTitle(Form(Title[k][iyz],sec));
    }
  }   
  TCanvas *c = 0;
  UInt_t Ntrk = trackVect.size();
  Int_t trackcolor = 1;
  Track2SegmentIter it;
  pair<Track2SegmentIter,Track2SegmentIter> ret;
  for (UInt_t trackI = 0; trackI < Ntrk; trackI++) {
    UInt_t TrackID = trackVect[trackI];
    ret = MapTrack2Segment.equal_range(TrackID);
    if ((*ret.first).first != TrackID) continue;
    LaserTrackSegment *trackSegment = 0;
    for (it = ret.first; (*it).first != (*ret.second).first; ++it) {
      Int_t keyS = (*it).second;
      trackSegment = trackSegments[keyS];
      if (! trackSegment) continue;
      if ((Int_t ) sec != trackSegment->sector) continue;
      if (trackSegment->inout != 2) continue;
      Int_t N = trackSegment->HitList.size();
      if (N < 3) continue;
      //	cout << "Sector = " << sec << " " <<  io << endl;
      trackSegment->MakeGraphs();
      if (! trackSegment->yz[0][0] || ! trackSegment->yz[1][0]) continue;
      nfoundInSector++;
      for (Int_t iyz = 0; iyz < 3; iyz++) {
	if (! trackSegment->yz[0][0] || ! trackSegment->yz[1][0]) continue;
	if (iyz < 2) {
	  for (Int_t k = 0; k < 2; k++) { // k => 1 residuals
	    // 		if (k < 2) {
	    c = c1; c->cd(2*iyz + k +1);
	    // 		}
	    // 		else       {c = c3; c->cd(2*iyz + io + 1);}
	    if (trackSegment->yz[iyz][k]) {
	      trackSegment->yz[iyz][k]->SetMarkerColor(trackcolor);
	      trackSegment->yz[iyz][k]->Draw("P");
	      c->Update();
	    }
	  }
	}
      }
      TF1 *Pol1Iy = trackSegment->yz[0][0]->GetFunction("Pol1");
      TF1 *Pol1Iz = trackSegment->yz[1][0]->GetFunction("Pol1");
      if (! Pol1Iy  || ! Pol1Iz ) continue;
      FittedSegment &SegTrack = trackSegment->SegTrack;
      SegTrack.ndfY = Pol1Iy->GetNDF();
      SegTrack.dY    = 1e4*Pol1Iy->GetParameter(0); 
      SegTrack.ddY   = 1e4*Pol1Iy->GetParError(0);
      SegTrack.dYdX  = 1e3*Pol1Iy->GetParameter(1);
      SegTrack.ddYdX = 1e3*Pol1Iy->GetParError(1);
      SegTrack.ndfZ  = Pol1Iz->GetNDF();
      SegTrack.dZ    = 1e4*Pol1Iz->GetParameter(0); 
      SegTrack.ddZ   = 1e4*Pol1Iz->GetParError(0);
      SegTrack.dZdX  = 1e3*Pol1Iz->GetParameter(1);
      SegTrack.ddZdX = 1e3*Pol1Iz->GetParError(1);
      //	    Track *track = (Track *) event->GetTracks()->UncheckedAt(TrackID%10000);
      SegTrack.sec     = trackSegment->sector;
      SegTrack.trackID = trackSegment->keyTrack;
      SegTrack.Z       = trackSegment->dca_Z;
      SegTrack.Imp     = trackSegment->dca_XY;
      SegTrack.pol1Iy0 = Pol1Iy->GetParameter(0);
      SegTrack.pol1Iy1 = Pol1Iy->GetParameter(1);
      SegTrack.pol1Iz0 = Pol1Iz->GetParameter(0);
      SegTrack.pol1Iz1 = Pol1Iz->GetParameter(1);
      SegTrack.dpl1Iy0 = Pol1Iy->GetParError(0);
      SegTrack.dpl1Iy1 = Pol1Iy->GetParError(1);
      SegTrack.dpl1Iz0 = Pol1Iz->GetParError(0);
      SegTrack.dpl1Iz1 = Pol1Iz->GetParError(1);
      SegTrack.xM      = trackSegment->xM;
      SegTrack.yM      = trackSegment->yM;
      SegTrack.zM      = trackSegment->zM;
      SegTrack.secM    = trackSegment->secM;
      SegTrack.Check();
      if (! SegTrack.flag) continue;
      SegTrack.Print("");
      trackSegment->SegTrack = SegTrack;
      FitP->Fill(&SegTrack.sec);
      if (! SegTrack.flag)  continue;
    }
    trackcolor++;
    if (trackcolor > 8) trackcolor = 1;
  }
  return nfoundInSector;
}
//#define __XY__
//________________________________________________________________________________
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  f = 0;
  Double_t x0    = par[0];
  Double_t y0 	 = par[1];
  Double_t z0 	 = par[2];
  Double_t alpha = par[3];
  Double_t beta  = par[4];
  Double_t gamma = par[5];
  TGeoHMatrix dR;
  dR.RotateX(TMath::RadToDeg()*alpha);
  dR.RotateY(TMath::RadToDeg()*beta);
  dR.RotateZ(TMath::RadToDeg()*gamma);
  dR.SetTranslation(&par[0]);
  /*
    RV:R1-V3
    [-yM*gamma + beta* zM + x0      ]
    [ xM*gamma - alpha*zM + y0 + dY ]
    [ alpha*yM - beta* xM + z0 + dZ ]
    t2 = dZdX**2  + dYdX**2  + 1
    RV.x() = - yM*gamma + beta* zM + x0;
    RV.y() =   xM*gamma - alpha*zM + y0 + dY
    RV.z() = alpha*yM - beta*xM + dZ + z0
    d2:transpose(RV) . RV-(transpose(RV) . T)^2/transpose(T) . T
    d2 = RV.mag2() - TMath::Power( RV.x() + dYdX*RV.y() +dZdX*RV.z(), 2)/t2; 
    d2ddY:diff(d2,dY) 
    2*RV.y() - (2*dYdX*(dYdX*RV.y() + RV.x() + dZdX*RV.z()))/t2

    d2ddZ:diff(d2,dZ)
    *RV.z() - (2*dZdX*(dYdX*RV.y() + RV.x() + dZdX*RV.z()))/t2

    d2ddYdX:diff(d2,dYdX)
    (2*dYdX*TMath::Power(dYdX*RV.y() + RV.x() + dZdX*RV.z(), 2))/t2  - (2*RV.y()*(dYdX*RV.y() RV.x() + dZdX*RV.z()))/t2

    d2ddZdX:diff(d2,dZdX)
    (2*dZdX*TMath::Power(dYdX*RV.y() + RV.x() + dZdX*RV.z(), 2))/t2 - (2*RV.z()*(dYdX*RV.y() + RV.x() + dZdX*RV.z()))/t2
  */
  static Bool_t first = kTRUE;
  Int_t    NoSegm  =  0;
  Double_t d2worst = -1.;
  Int_t     kworst = -1;
  for (UInt_t i = 0; i < TrackSegmentV.size(); i++) {
    LaserTrackSegment *segm = TrackSegmentV[i];
    if (! segm->SegTrack.flag) continue;
    NoSegm++;
    Double_t dY    = 1e-4*segm->SegTrack.dY;
    Double_t ddY   = 1e-4*segm->SegTrack.ddY;
    Double_t dYdX  = 1e-3*segm->SegTrack.dYdX;
    Double_t ddYdX = 1e-3*segm->SegTrack.ddYdX;
    Double_t dZ    = 1e-4*segm->SegTrack.dZ;
    Double_t dZdX  = 1e-3*segm->SegTrack.dZdX;
    Double_t ddZ   = 1e-4*segm->SegTrack.ddZ;
    Double_t ddZdX = 1e-3*segm->SegTrack.ddZdX;
    Double_t xM    = segm->xM;
    Double_t yM    = segm->yM;
    Double_t zM    = segm->zM;
    StThreeVectorD M(xM,yM,zM); // mirror in Local CS
    StThreeVectorD Xv;
    dR.MasterToLocal(M.xyz(),Xv.xyz());
    StThreeVectorD V = Xv - M;
    StThreeVectorD R1(0,dY,dZ);
    StThreeVectorD T (1,dYdX,dZdX);
    Double_t t2 = T.mag2();
    StThreeVectorD RV = R1 - V;
    Double_t RVT = RV*T;
    Double_t dS = - RVT/t2;
    StThreeVectorD Delta = RV + T*dS;
    Double_t d2      = Delta.mag2();
    Double_t dd2 = ddY*ddY + ddZ*ddZ;
#if 0
#if 0
    Double_t d2      = RV.mag2() - RVT*RVT/t2;
    Double_t dd2 = ddY*ddY + ddZ*ddZ;
#else
    //    Double_t d2      = RV.mag2() - RVT*RVT/t2;
    Double_t d2ddY   = 2*(RV.y() - dYdX*RVT)/t2;
    Double_t d2ddZ   = 2*(RV.z() - dZdX*RVT)/t2;
    Double_t d2ddYdX = 2*RVT*(dYdX*RVT - RV.y())/t2;
    Double_t d2ddZdX = 2*RVT*(dZdX*RVT - RV.z())/t2;
    Double_t dd2 = TMath::Power(d2ddY*ddY,2) + TMath::Power(d2ddZ*ddZ,2) + TMath::Power(d2ddYdX*ddYdX,2) + TMath::Power(d2ddZdX*ddZdX,2);
#endif
#endif
    Double_t dev = d2/dd2;
    f += dev;
    if (iflag == 3) { // || first) {
      segm->SegTrack.Print();
      cout << "d " << TMath::Sqrt(d2) << " +/- " << TMath::Sqrt(dd2) << " dev " << dev << endl;
      if (d2worst < dev) {
	kworst = i;
	d2worst = dev;
      }
    }
  }
  if (iflag == 3 && kworst >= 0 && NoSegm > 2 && d2worst > 4*f/NoSegm) {
    LaserTrackSegment *segm = TrackSegmentV[kworst];
    cout << "Worst " << kworst << " d2:" << d2worst << "\t"; segm->SegTrack.Print();
    segm->SegTrack.flag = 0;
  }
  if (iflag != 3 ) first = kFALSE;
  else             first = kTRUE;
}
//________________________________________________________________________________
void FitOneSector(Int_t sec, TrackSegmentMap &trackSegments) {
  if (! FitM) {
    fOut->cd();
    const Char_t *vars =  "sec:NoTracks:x0:y0:z0:alpha:beta:gamma:dx0:dy0:dz0:dalpha:dbeta:dgamma:chidq";
    FitM = new TNtuple("FitM","Sector parameters fit",vars);
  } 
  struct FitM_t {
    Float_t sec, NoTracks;
    Float_t  x0,  y0,  z0,  alpha,  beta,  gamma;
    Float_t dx0, dy0, dz0, dalpha, dbeta, dgamma; 
    Float_t chisq; 
  };
  FitM_t F; memset(&F, 0, sizeof(FitM_t));
  F.sec = sec;
  Bool_t Bad = kFALSE;
  if (gMinuit) delete gMinuit;
  gMinuit = new TMinuit(6);
  gMinuit->SetFCN(fcn);
  Double_t arglist[10];
  Int_t ierflg = 0;
  Int_t iter = 0;
  UInt_t NoTrack1I = 0;
  do {// Check for cleaning up
    iter++;
    TrackSegmentV.clear();
    for (TrackSegmentMapIter it=trackSegments.begin(); it!=trackSegments.end(); ++it) {
      LaserTrackSegment *segm = (*it).second;
      if (segm->sector != sec) continue;
      if (TMath::Abs(segm->SegTrack.dY) > 1e4 || TMath::Abs(segm->SegTrack.dZ) > 1e4) segm->SegTrack.flag = 0;
      if (! segm->SegTrack.flag) continue;
      TrackSegmentV.push_back(segm);
    }
    cout << "sector " << sec << " total no. of segments " << TrackSegmentV.size() << endl;
    UInt_t NoTracks = TrackSegmentV.size();
    if (! NoTrack1I && 2*NoTracks <= NoTrack1I) break; // at least half of tracks should be good
    F.NoTracks = NoTracks;
    if (! NoTrack1I) NoTrack1I = F.NoTracks;
    //    fMinut->SetPrintLevel(-1);
    arglist[0] = 1;
#if 0
    gMinuit->mnexcm("set print",arglist, 1, ierflg);
    gMinuit->mnexcm("set NOW",arglist, 0, ierflg);
    gMinuit->mnexcm("CLEAR",arglist, 0, ierflg);
    arglist[0] = 0.5;
    gMinuit->mnexcm("SET ERR", arglist ,1,ierflg);
#endif
    gMinuit->mnparm(0, "x0", 0.0, 0, -2.0,2.0,ierflg); //First Guess
    gMinuit->mnparm(1, "y0", 0.0, 1e-4, -2.0,2.0,ierflg); //First Guess
    gMinuit->mnparm(2, "z0", 0.0, 1e-4, -2.0,2.0,ierflg); //First Guess
    gMinuit->mnparm(3, "alpha", 0.0, 0, -2e-3,2e-3,ierflg); //First Guess
    gMinuit->mnparm(4, "beta",  0.0, 0, -2e-3,2e-3,ierflg); //First Guess
    gMinuit->mnparm(5, "gamma", 0.0, 0, -2e-3,2e-3,ierflg); //First Guess 
    Int_t NDF =  2*TrackSegmentV.size() - gMinuit->GetNumFreePars();
    if (NDF < 1) return;
   //  arglist[0] = 1.;   // 1.
    //  arglist[0] = 0.;   // Check gradient 
    //  gMinuit->mnexcm("SET GRAD",arglist,1,ierflg);
    arglist[0] = 500;
    arglist[1] = 1.;
    //  gMinuit->mnexcm("MIGRAD", arglist ,2,ierflg);
    gMinuit->mnexcm("MINIMIZE", arglist ,2,ierflg);
    gMinuit->mnexcm("HESSE  ",arglist,0,ierflg);
    Double_t edm,errdef;
    Int_t nvpar,nparx,icstat;
    Double_t chisq;
    gMinuit->mnstat(chisq,edm,errdef,nvpar,nparx,icstat);
    Double_t  x0,  y0,  z0,  alpha,  beta,  gamma;
    Double_t dx0, dy0, dz0, dalpha, dbeta, dgamma;
    gMinuit->GetParameter(0,x0,    dx0);     F.x0    = x0   ; F.dx0    = dx0   ;
    gMinuit->GetParameter(1,y0,    dy0);     F.y0    = y0   ; F.dy0    = dy0   ;
    gMinuit->GetParameter(2,z0,    dz0);     F.z0    = z0   ; F.dz0    = dz0   ;
    gMinuit->GetParameter(3,alpha, dalpha);  F.alpha = alpha; F.dalpha = dalpha;
    gMinuit->GetParameter(4,beta , dbeta);   F.beta  = beta ; F.dbeta  = dbeta ;
    gMinuit->GetParameter(5,gamma, dgamma);  F.gamma = gamma; F.dgamma = dgamma;
    gMinuit->mnprin(3,chisq);
    F.chisq = chisq;
    gMinuit->mnexcm("END",arglist,0,ierflg);
    cout << Form("iter %2i sec %2i",iter,sec) 
	 << Form(" x0 = %7.1f+/-%6.1f[mkm]",1e4*F.x0,1e4*F.dx0)
	 << Form(" y0 = %7.1f+/-%6.1f[mkm]",1e4*F.y0,1e4*F.dy0)
	 << Form(" z0 = %7.1f+/-%6.1f[mkm]",1e4*F.z0,1e4*F.dz0)
	 << Form(" alpha = %7.2f+/-%6.2f[mrad]",1e3*F.alpha,1e3*F.dalpha)
	 << Form(" beta = %7.2f+/-%6.2f[mrad]",1e3*F.beta,1e3*F.dbeta)
	 << Form(" gamma = %7.2f+/-%6.2f[mrad]",1e3*F.gamma,1e3*F.dgamma) 
	 << endl;
    UInt_t NoLeft = 0;
    for (UInt_t i = 0; i < TrackSegmentV.size(); i++) {
      LaserTrackSegment *segm = TrackSegmentV[i];
      if (! segm->SegTrack.flag) continue;
      NoLeft++;
    }
    Bad = kFALSE;
    if (NoLeft < TrackSegmentV.size()) Bad = kTRUE;
  } while(Bad);
  
  FitM->Fill(&F.sec);
}
//________________________________________________________________________________
void SectorAsWhole2Mirror() {
  vector<Int_t>          trackVect;
  Track2SegmentMap  MapTrack2Segment;
  MapTrack2Segment.insert(pair<UInt_t,UInt_t>(0,0));
  //  bool(*fn_pt)(Int_t,Int_t) = fncomp;
  //  map<Int_t,LaserTrackSegment,bool(*)(Int_t,Int_t)> trackSegments(fn_pt);
  TrackSegmentMap trackSegments;
  Track2SegmentIter it;
  pair<Track2SegmentIter,Track2SegmentIter> ret;
  Int_t k1 = 1;
  FillSegments(trackVect, MapTrack2Segment, trackSegments,k1);
  c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("IOyz");
  if (! c1 ) c1 = new TCanvas("yz","Laser X and Z fits" ,900,1000);
  for (UInt_t sec = 1; sec <= 24; sec++) {
    if (! FitSegments2Mirror(sec, trackVect, MapTrack2Segment,trackSegments )) continue;
    FitOneSector(sec,trackSegments);
    if (! gROOT->IsBatch() && Ask()) return;
    //    DrawPng(c1);
  }
  // clean 
  
  for (TrackSegmentMapIter it=trackSegments.begin(); it!=trackSegments.end(); ++it) {
    if ((*it).second) {
      //      std::cout << (*it).first << " => " << (*it).second << '\n';
      delete (*it).second;
      (*it).second = 0;
    }
  }
  fOut->Write();
}
//________________________________________________________________________________
void LaserAlignment(Bool_t IO = kTRUE) {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  if (IO) LaserAlignmentIO();
  else    SectorAsWhole2Mirror();
}
