// $Id: StLaserAnalysisMaker.cxx,v 1.4 2000/02/09 21:38:14 fisyak Exp $
// $Log: StLaserAnalysisMaker.cxx,v $
// Revision 1.4  2000/02/09 21:38:14  fisyak
// Increase cluster size
//
// Revision 1.3  2000/01/31 23:50:11  fisyak
// Clean up
//
// Revision 1.2  2000/01/31 15:14:52  fisyak
// Try to find out memory leak
//
// Revision 1.1.1.1  2000/01/29 16:23:06  fisyak
// First release of StLaserAnalysisMaker
//
//////////////////////////////////////////////////////////////////////////
#include <stdio.h>      // For binary file input (the DAQ data file).
#include <string.h>     // For binary file input (the DAQ data file).
#include <sys/types.h>  // For binary file input (the DAQ data file).
#include <sys/stat.h>   // For binary file input (the DAQ data file).
#include <fcntl.h>      // For binary file input (the DAQ data file).
///////////////////////////////////////////////////////////////////////////
#include "StDaqLib/TPC/trans_table.hh"
#include "StSequence.hh"
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StMemoryInfo.hh"
#include "TTree.h"
#include "TH1.h"
#include "TH2.h"
#include "TMath.h"
#include "TGeant3.h"
#include "StLaserAnalysisMaker.h"
#include "StBFChain.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"
#include "StarCallf77.h" 
#include "Prediction.h"
#include "StDAQMaker/StDAQReader.h"
#include "tables/St_tpg_pad_plane_Table.h"
#include "tables/St_tpg_detector_Table.h"
#define g2t_volume_id F77_NAME(g2t_volume_id,G2T_VOLUME_ID)
extern "C" {Int_t g2t_volume_id(char*, int*, int);}
static TGeant3 *geant3 = 0;
ClassImp(StLaserAnalysisMaker)
  
LaserTrack LaserTracks[] = {
  //#include "Yuri.table"
  //#include "Y2k.table"
#include "Y2kchop.table"
  {  24,   7,   1,       0,  127.358,  0.439,  -0.0222,  0.0009,   -2.260,  0.114,  197.376,  0.088,  -33.480,  0.170 }
};
Int_t NoLaserTracks = sizeof (LaserTracks)/sizeof (LaserTrack);
Quest_t  *cquest; 
Gclink_t *clink; 
Gcflag_t *cflag; 
Gcvolu_t *cvolu; 
Gcnum_t  *cnum; 
Gctrak_t *ctrak;
Gcsets_t *csets;
Ertrio_t *certrio;
Eropts_t *ceropts;
Int_t *z_iq, *z_lq; 
Float_t *z_q; 
const Int_t NY = 7;
const Int_t NZ = 7;
const Int_t NYT = 2*NY + 1;
const Int_t NZT = 2*NZ + 1;
TH1F *fRatio;
TH1F *fADC;
TH1F *fADC3x3;
TH2F *fPadI;
TH2F *fTimeI;
TH2F *fPadO;
TH2F *fTimeO;
//_____________________________________________________________________________
StLaserAnalysisMaker::StLaserAnalysisMaker(const char *name):StMaker(name),fNPrediction(0),fPredictions(0){
}
//_____________________________________________________________________________
StLaserAnalysisMaker::~StLaserAnalysisMaker() {
}
//_____________________________________________________________________________
Int_t StLaserAnalysisMaker::Init(){
  // Create tables
  //   St_DataSetIter       local(GetDataBase("params"));
  // Create Histograms    
  Int_t i,j;
  geant3 = TGeant3::Geant3();
  if (geant3) {
    cquest = (Quest_t  *) geant3->Quest();
    clink  = (Gclink_t *) geant3->Gclink();
    cflag  = (Gcflag_t *) geant3->Gcflag();
    cvolu  = (Gcvolu_t *) geant3->Gcvolu();
    cnum   = (Gcnum_t  *) geant3->Gcnum();
    ctrak  = (Gctrak_t *) geant3->Gctrak();
    csets  = (Gcsets_t *) geant3->Gcsets();
    certrio = (Ertrio_t *) geant3->Ertrio();
    ceropts = (Eropts_t *) geant3->Eropts();
    z_iq   = (Int_t    *) geant3->Iq();
    z_lq   = (Int_t    *) geant3->Lq();
    z_q    = (Float_t  *) geant3->Q();
  }
  St_DataSet *tpg =  GetInputDB("params/tpc/tpgpar"); assert(tpg);
  ftpg_pad_plane = (St_tpg_pad_plane *) tpg->Find("tpg_pad_plane");  
  assert(ftpg_pad_plane);
  St_tpg_pad_plane &tpg_pad_plane = *ftpg_pad_plane;
  ftpg_detector = (St_tpg_detector *) tpg->Find("tpg_detector");  
  assert(ftpg_detector);
  St_tpg_detector &tpg_detector = *ftpg_detector;
  // Predicions
  fPredictions = new TClonesArray("Prediction", 1000);
  TClonesArray &predictions = *fPredictions;
  const Char_t *PredictionFile  = "Prediction.root";
  TBranch *branch = 0;
  TFile *fPred = new TFile(PredictionFile);
  if (fPred->IsOpen()) {
    fTree = (TTree*) fPred->Get("P");
    if (fTree) {
      branch = fTree->GetBranch("Predictions");
      if (branch) {
	branch->SetAddress(&fPredictions);
	fTree->GetEntry(0);  //read complete event in memory
      }
    }
  }
  if (!branch) {
    SafeDelete (fPred);
    fPred = new TFile(PredictionFile,"RECREATE");
    // Need to get some data from tables, e.g. globtrk for momentum etc,
    // Also primary vertex. Make a loop over tracks too.
    const Double_t degrad = TMath::Pi()/180;
    Int_t nPrediction=1;
    Int_t idParticle;
    const Int_t kPiPlus=8;// kPiMinus=9;
    Float_t pt;
    Float_t x1[3]; //start point
    Float_t p1[3]; //start momentum
    Float_t x2[3]; //end point
    Float_t p2[3]; //end momentum
    // set the error matrix to zero
    Float_t xzero[3] = {0,0,0};
    Float_t xdire[3] = {1,0,0};
    Float_t ydire[3] = {0,1,0};
    Float_t zdire[3] = {0,0,1};
    Float_t xyzL[3];
    Float_t pxyzL[3];
    Float_t inputError[15] = {0};  // should set remaining elements to zero too
    // 2D arrays are to be given to FORTRAN routines
    // so they should be stored appropriately
    Float_t inputPlane[] = {1,0,0,0,1,0}; // what is this for?
    // ertrak starts from a point which is enough to specify problem ..?
    Float_t predictionPlane[] = {0,1,0,0,0,1,0,0,0,1,0,0}; //XY plane
    Char_t *tpc  = "tpc";
    Int_t iovlIn[2] = {1,1};
    Int_t   numv[15] = {0};
    Float_t par[50];
    Float_t att[10];
    //Loop over tracks
    Int_t Id = 0;
    Int_t No = NoLaserTracks;
    for(i=0; i < No; i++){
      if (! LaserTracks[i].NoTracks) continue;
      pt = 1000;
      p1[0] = pt*TMath::Cos(degrad*LaserTracks[i].psi);
      p1[1] = pt*TMath::Sin(degrad*LaserTracks[i].psi);
      p1[2] = pt*LaserTracks[i].tanl;
      
      //Fill position vector
      x1[0] = LaserTracks[i].xl;
      x1[1] = LaserTracks[i].yl;
      x1[2] = LaserTracks[i].zl;
      idParticle=kPiPlus;
      Int_t medi = 0;
      cvolu->nlevel = 0;
      geant3->Gmedia(x1,medi);
      ctrak->inwvol = 0;
      Int_t ierr = 0;
      Int_t nGetVol = 0;
      Char_t *VoptB = "VBO";
      Char_t *VoptF = "VO";
      Int_t  OptX = 1;
      Int_t  OptD = 2;
      for (Int_t kTry=0; kTry<2 && !nGetVol; kTry++) {
	while (ctrak->inwvol != 3 || !ierr) {
	  Int_t Npred = 2;
	  geant3->Eufilv (Npred, inputError, "TPADTPA1", numv, iovlIn);
	  if (kTry) ierr = geant3->Ertrak(x1,p1,x2,p2,idParticle,VoptB);
	  else      ierr = geant3->Ertrak(x1,p1,x2,p2,idParticle,VoptF);
	  if (ierr || ctrak->inwvol == 3) break;
	  nGetVol++;
	  if (Debug()) {
	    gMessMgr->Info() << "\tx1:" << x1[0] << "\t" << x1[1] << "\t" << x1[2] 
			     << "\tp1:" << p1[0] << "\t" << p1[1] << "\t" << p1[2] << endm;
	    gMessMgr->Info() << "\tx2:" << x2[0] << "\t" << x2[1] << "\t" << x2[2] 
			     << "\tp2:" << p2[0] << "\t" << p2[1] << "\t" << p2[2] << endm;
	  }
	  memcpy (x1,x2,12);
	  memcpy (p1,p2,12);
	  geant3->Gfinds();
	  Int_t g2t_id = g2t_volume_id(tpc,csets->numbv,strlen(tpc));
	  Int_t isdet  = g2t_id/100000;
	  if (isdet) continue;
	  geant3->Gdtom(ydire,&predictionPlane[0],OptD);
	  geant3->Gdtom(zdire,&predictionPlane[3],OptD);
	  geant3->Gdtom(xzero,&predictionPlane[6],OptX);
	  geant3->Gdtom(xdire,&predictionPlane[9],OptD);
	  geant3->Eufilp(nPrediction,inputError,inputPlane,predictionPlane);
	  if (Debug()) {
	    gMessMgr->Info() << "\tx1:" << x1[0] << "\t" << x1[1] << "\t" << x1[2] 
			     << "\tp1:" << p1[0] << "\t" << p1[1] << "\t" << p1[2] << endm;
	    gMessMgr->Info() << "\tx2:" << x2[0] << "\t" << x2[1] << "\t" << x2[2] 
			     << "\tp2:" << p2[0] << "\t" << p2[1] << "\t" << p2[2] << endm;
	  }
	  geant3->Gmtod(x2,xyzL,OptX);
	  geant3->Gmtod(p2,pxyzL,OptD);
	  if (Debug()) {
	    gMessMgr->Info() << "\txyzL:" 
			     << xyzL[0] << "\t" 
			     << xyzL[1] << "\t" 
			     << xyzL[2] << "\t pxyzL"
			     << pxyzL[0] << "\t" 
			     << pxyzL[1] << "\t" 
			     << pxyzL[2] << endm;
	  }
	  Int_t k; 
	  Float_t dirx = 0; for (k=0;k<3;k++) dirx +=  xyzL[k]*predictionPlane[9+k]; 
	  Float_t dirp = 0; for (k=0;k<3;k++) dirp += pxyzL[k]*predictionPlane[9+k];
	  Float_t dir = - dirx/dirp;
	  if (Debug()) gMessMgr->Info() << "\tdir = " << dir << endm;
	  if (dir < 0) ierr = geant3->Ertrak(x1,p1,x2,p2,idParticle,"PBOM");
	  else         ierr = geant3->Ertrak(x1,p1,x2,p2,idParticle,"POM"); 
	  if (ctrak->inwvol == 3) {ctrak->inwvol = 0; continue;}
	  if (ierr) break;
	  geant3->Gfinds();
	  g2t_id = g2t_volume_id(tpc,csets->numbv,strlen(tpc));
	  isdet  = g2t_id/100000;
	  Int_t sector = (g2t_id%100000)/100;
	  Int_t padrow   =  g2t_id%100;
	  Int_t npar,natt;
	  geant3->Gfpara((const char *) &cvolu->names[cvolu->nlevel-1], 
			 cvolu->number[cvolu->nlevel-1],0,npar,natt,par,att);
	  Float_t padsize;
	  if (padrow <= tpg_pad_plane[0].nrow_in) padsize = tpg_pad_plane[0].pad_sep_in;
	  else                                  padsize = tpg_pad_plane[0].pad_sep_out;
	  geant3->Gmtod(x2,xyzL,OptX);
	  xyzL[1] += par[1];
	  xyzL[2] = par[2] - xyzL[2];
	  geant3->Gmtod(p2,pxyzL,OptD);
	  Float_t Xpad    =  (xyzL[1]/padsize);
	  Int_t pad = (int) (Xpad + 1);
	  Float_t X = Xpad - pad + 0.5;
	  Float_t vdrift = tpg_detector[0].vdrift; // drift velocity (cm/s) ;
	  Float_t clock  = tpg_detector[0].clock_frequency; // frequency of the clock driving FEE(1/s) ;
	  Float_t t0_offset = tpg_detector[0].trigger_offset; // time diff between the event and theRDO trigger ;
	  Float_t z_offset = tpg_detector[0].drift_length - 2*par[2];
	  // offset in z between the inner and the outer sector (cm) ;
	  if (padrow <=  tpg_pad_plane[0].nrow_in) z_offset += tpg_detector[0].z_inner_offset;
	  Float_t z = xyzL[2] + z_offset;
	  Float_t ZBin  = (z*clock/vdrift - t0_offset*clock);
	  Int_t TimeBin = (int) (ZBin + 1);
	  Float_t Z = ZBin - TimeBin + 0.5;
	  if (Debug()) {
	    gMessMgr->Info() << "\txyzL:" 
			     << xyzL[0] << "\t" 
			     << xyzL[1] << "\t" 
			     << xyzL[2] << endm;
	  }
	  if (Debug()) {
	    gMessMgr->Info() << "\tg2t_id \t" << g2t_id << endm;
	    gMessMgr->Info() << "\tSector \t" << sector << "\t PadRow \t" 
			     << padrow << "\t isdet \t" << isdet <<  "\tpad \t" << pad << endm;
	  }
	  //      TMath::Normalize(pxyzL);
	  new(predictions[fNPrediction++]) Prediction(i,++Id, sector, padrow, pad, TimeBin,
			pxyzL[1]/pxyzL[0], pxyzL[2]/pxyzL[0], X, Z);
	  memcpy (x1,x2,12);
	  memcpy (p1,p2,12);
	}
      }
      if (Debug()) gMessMgr->Info() << "Done with track no." << i << endm;
    }
    gMessMgr->Info() << "Tracks loop is over with " << Id << "/" << fNPrediction << " predictions" << endm;
    TObjArrayIter next(fPredictions);  
    Prediction *pred, *pred2;
    //    while ((pred = (Prediction *) next())) {
    for (i=0;i<fNPrediction-1; i++){
      pred = (Prediction *) fPredictions->UncheckedAt(i);
      if (Debug()) pred->Print();
      //      TObjArrayIter next2(fPredictions);
      //      while ((pred2 = (Prediction *) next2())) {
      for (j=i+1;j<fNPrediction;j++){
	pred2 = (Prediction *) fPredictions->UncheckedAt(j);
	if (pred->GetId() == pred2->GetId()) continue;
	if (pred->GetSector() != pred2->GetSector()) continue;
	if (pred->GetPadrow() != pred2->GetPadrow()) continue;
	if (TMath::Abs(pred->GetPadNo() - pred2->GetPadNo()) > NYT) continue;
	if (TMath::Abs(pred->GetTimeBin() - pred2->GetTimeBin()) > NZT) continue;
	pred->DisActivate();// cout << "Disactivate"; pred->Print();
	pred2->DisActivate();// cout << "Disactivate"; pred2->Print();
      }
    }
    fPred->cd();
    fTree = new TTree("P","Tpc laser track prediction tree");
    fTree->Branch("Predictions", &fPredictions);
    fTree->Fill();
    fPred->Write();
  }
  fTree = (TTree*) fPred->Get("P");
  branch = fTree->GetBranch("Predictions");
  fNPrediction = fPredictions->GetEntries(); cout << "Read " << fNPrediction << " predictions" << endl;
  SafeDelete (fPred);
  // Open DAQ file
  OpenDAQ();
  //Histograms
  fevent = new LEvent;
  TFile *f = (TFile *) ((StBFChain *)GetChain())->GetTFile();
  if (f) {
    f->cd();
    fTree        = new TTree("L","Tpc laser track tree");
    //    fTree->SetAutoSave(10000000);
    fTree->Branch("LEvent", "LEvent",&fevent);
  }
  fADC      = new TH1F("ADC","ADC sum",500,0,10000.);
  fADC3x3   = new TH1F("ADC3x3","ADC sum 3x3",500,0,10000.);
  fRatio    = new TH1F("Ratio","Ratio ADC3x3/ADC",100,0.,1.0);
#if 0
  fPadI     = new TH2F("PadI","space distribution for inner sectors",10*NYT,-NY-0.5,NY+0.5,100,0.,1.);  
  fPadO     = new TH2F("PadO","space distribution for outer sectors",10*NYT,-NY-0.5,NY+0.5,100,0.,1.);  
  fTimeI    = new TH2F("TimeI","time distribution for inner sectors",10*NZT,-NZ-0.5,NZ+0.5,100,0.,1.);  
  fTimeO    = new TH2F("TimeO","time distribution for outer sectors",10*NZT,-NZ-0.5,NZ+0.5,100,0.,1.);  
#endif
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StLaserAnalysisMaker::OpenDAQ()
{
  St_DataSet *ds = GetDataSet("StDAQReader"); assert(ds);
  fDAQReader=(StDAQReader*)(ds->GetObject()); assert(fDAQReader);
  return kTRUE;
}
//_____________________________________________________________________________
Int_t StLaserAnalysisMaker::Make(){
  fTPCReader = fDAQReader->getTPCReader();   assert(fTPCReader); 
  // clean and fill tree
  Option_t *Option = "";
  // Predictions
  Float_t adc[NZT][NYT];
  //  TObjArrayIter next(fPredictions);
  Prediction *pred;
  //  while ((pred = (Prediction *) next())) {
  Int_t l;
#if 0
  StMemoryInfo::instance()->snapshot();
#endif
  for (Int_t Sector=1;Sector<=24;Sector++){
    for (l=0;l<fNPrediction-1; l++){
      pred = (Prediction *) fPredictions->UncheckedAt(l);
      if (pred->GetId() <= 0) continue;
      fevent->Clear(Option);
      Int_t noLT         = pred->GetLT();
      fevent->SetPred(pred);
      fevent->SetLTrack(LaserTracks[noLT]);
      Int_t sectorNumber =  pred->GetSector();
      if (sectorNumber != Sector) continue;
      Int_t padRowNumber =  pred->GetPadrow();
      Int_t padNumber    =  pred->GetPadNo();
      Int_t timeBin      =  pred->GetTimeBin();
      Float_t Y          =  pred->GetY();
      Float_t Z          =  pred->GetZ();
      //    Float_t tY         =  pred->GettY();
      //    Float_t tZ         =  pred->GettZ();
      Float_t ADC        = 0;
      Float_t ADC3x3     = 0; 
      Int_t   overflow   = 0;
      u_char  *padlist; 
      //    if (TMath::Abs(tY) > 0.05 || TMath::Abs(tZ) > 0.1) continue;
      memset (adc, 0, sizeof(adc));
      int count = fTPCReader->getPadList(sectorNumber,padRowNumber, padlist);
      if (!count) continue; // any pads with data?
      for (int padnum = 0; padnum<count && !overflow; padnum++) {
	int pad = padlist[padnum];
	if (!pad) continue;
	Int_t iY = pad - padNumber + NY;
	if (iY < 0 || iY >= NYT) continue;
	Int_t    nSeq;
	TPCSequence *Seq;
	fTPCReader->getSequences(sectorNumber,padRowNumber, pad, nSeq, Seq);
	for (int seq=0; seq<nSeq; seq++){
	  Int_t start = Seq[seq].startTimeBin;
	  int len   = Seq[seq].Length;
	  UChar_t *p = Seq[seq].FirstAdc;
	  for (int j=0; j<len; j++) {
	    Int_t iT = start + j - timeBin + NZ;
	    if (iT < 0 || iT >= NZT) continue;
	    adc[iT][iY] += log8to10_table[*(p++)];
	    if (adc[iT][iY] == 920) {overflow = 1; break;}
	    ADC += adc[iT][iY];
	    if (TMath::Abs(iT - NZ) < 2 && TMath::Abs(iY - NY) < 2) ADC3x3 += adc[iT][iY];
	  }
	}
      }
      //    continue;
      if (overflow) continue;
      Float_t ratio = 0;
      if (ADC > 0) {
	fADC->Fill(ADC);
	fADC3x3->Fill(ADC3x3);
	ratio = ADC3x3/ADC;
	fRatio->Fill(ratio);
	int i,j;
	if (ratio < 0.4) continue;
	if (ADC < 200 || ADC > 10000) continue;
	Float_t Yadc[NYT];
	Float_t Zadc[NZT];
	memset (Yadc, 0, sizeof(Yadc));
	memset (Zadc, 0, sizeof(Zadc));
	Int_t nY1 = NY;
	Int_t nY2 = -NY;
	Int_t nZ1 = NZ;
	Int_t nZ2 = -NZ;
	Float_t Yav = 0;
	Float_t Zav = 0;
	Float_t DYY = 0;
	Float_t CYZ = 0; 
	Float_t DZZ = 0;
	Float_t y,z;
	Int_t   yy,zz;
	for (i=0;i<NYT;i++) {
	  yy = i - NY;
	  y  = yy - Y;
	  for (j=0;j<NZT;j++) {
	    zz = j - NZ;
	    z  = zz - Z;
	    adc[j][i] /=ADC;
	    Yadc[i] += adc[j][i];
	    Zadc[j] += adc[j][i];
	    if (adc[j][i] > 0) {
	      if (yy < nY1) nY1 = yy;
	      if (zz < nZ1) nZ1 = zz;
	      if (yy > nY2) nY2 = yy;
	      if (zz > nZ2) nZ2 = zz;
	      Yav += adc[j][i]*yy;
	      Zav += adc[j][i]*zz;
	      DYY += adc[j][i]*yy*yy;
	      CYZ += adc[j][i]*yy*zz;
	      DZZ += adc[j][i]*zz*zz;
	      //yf	      fevent->AddAdc(y,z,adc[j][i]);
	    }
	  }
	}
	DYY -= Yav*Yav;
	CYZ -= Yav*Zav;
	DZZ -= Zav*Zav;
	fevent->SetAverage(ADC,ADC3x3,ratio,nY1,nY2,nZ1,nZ2,Yav,Zav,DYY,CYZ,DZZ);
	TH2F *hPad  = fPadO;
	TH2F *hTime = fTimeO;
	if (padRowNumber <= 13) {hPad  = fPadI; hTime = fTimeI;}
	for (i=0; i<NYT; i++) {
	  yy = i - NY;
	  y  = yy - Y;
	  //	if (Yadc[i] > 0) {
	  //	  hPad->Fill(y,Yadc[i]);
	  fevent->AddYProf(y,Yadc[i]);
	  //	}
	}
	for (j=0; j<NZT; j++) {
	  zz = j - NZ;
	  z  = zz - Z;
	  //	if (Zadc[j] > 0) {
	  //	hTime->Fill(j-NZ-Z,Zadc[j]);
	  fevent->AddZProf(Z,Zadc[j]);
	  //	}
	}
#if 0
	printf ("===================================\n");
	printf ("Total ADC %f ADC3x3 %f ratio: %f Sector %i padRow %i pad %i Time %i Y/Z tY/tZ %f %f %f %f\n",
		ADC,ADC3x3,ratio,sectorNumber,padRowNumber,padNumber,timeBin,Y,Z,tY,tZ);
	printf ("Y/ dY \t:%f  \tZ/ dZ \t:%f\n",Y,Yav-Y,Z,Zav-Z);
	printf ("|\tidx");
	for (j=0;j<NZT;j++) printf("|\t%5i",j-NZ);  printf ("\n");
	for (i=0;i<NYT;i++){
	  printf ("%i",i-NY);
	  for (j=0;j<NZT;j++) printf ("|\t%5.0f",1000*adc[j][i]);
	  printf ("|\t%5.0f",1000*Yadc[i]); 
	  if (i == NZ) printf("<=== Y %f dY %f" ,Y, Yav-Y);
	  printf("\n");
	}
	for (j=0;j<NZT;j++) printf("|\t%5.0f",1000*Zadc[j]);  printf ("\n");
#endif
	if (fevent->GetNoY() > 0 && TMath::Abs(Yav - Y) < 2.5)     fTree->Fill();
	//    else                           fevent->Print();
      }
    }
  }
  //  fTree->AutoSave();
#if 0
  StMemoryInfo::instance()->snapshot();
  StMemoryInfo::instance()->print();
#endif
  return kStOK;
}
