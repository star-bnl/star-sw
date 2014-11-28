// $Id: StLaserAvClusterMaker.cxx,v 1.2 2013/09/28 13:01:53 fisyak Exp $
// $Log: StLaserAvClusterMaker.cxx,v $
// Revision 1.2  2013/09/28 13:01:53  fisyak
// Freeze
//
// Revision 1.1.1.1  2012/08/27 22:59:19  fisyak
// Rename StLaserAvEvent => StLaserAvClusterMaker
//
// Revision 1.1  2012/06/11 14:24:24  fisyak
// The first version based on TSpectrum2
//
// Revision 1.1.1.1  2011/10/13 15:02:39  hejdar
// Hejdar makers
//
#include <assert.h>
#include "StLaserAvClusterMaker.h"
#include "StEvtHddr.h"
#include "StEvent.h"
#include "StTpcHit.h"
#include "StTpcHitCollection.h"
#include "StThreeVector.hh"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StTpcDb/StTpcDb.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
#include "StDetectorDbMaker/St_tpcPadGainT0C.h"
#include "StDetectorDbMaker/St_tpcElectronicsC.h"
#include "StTpcDb/StTpcDb.h"
#include "TDirIter.h"
#include "TTree.h"

#define Cluster_cxx
#include "Cluster.h"
static Cluster *fCluster = 0;
ClassImp(StLaserAvClusterMaker)
//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StLaserAvClusterMaker::Init(){
  const Char_t *TreeName = "FitP";
  TDirIter Dir(fFileName);
  TChain *FitP = new TChain(TreeName);
  Char_t *file = 0;
  Int_t NFiles = 0;
  ULong64_t nEvents = 0;
  ULong64_t nEvTot = 0;
  
  while ((file = (Char_t *) Dir.NextFile())) {
    TFile *infile = TFile::Open(file);
    if (! infile) continue;
    TTree *tree = (TTree *) infile->Get(TreeName);
    if (tree) {
      // set date time for StEvent
      StEvtHddr* header = dynamic_cast<StEvtHddr*>(infile->Get("EvtHddr"));
      assert(header);
      header->Print();
      StEvtHddr* head = GetEvtHddr();
      *head = *header;
      head->Print();
      NFiles++;
      nEvents = tree->GetEntries();
      cout << "#\t" << NFiles << "\t" << infile->GetName() << "\t" << nEvents << endl;
      nEvTot += nEvents;
      FitP->Add(infile->GetName());
    } else {
      cout << "#\t" << NFiles << "\t" << infile->GetName() << "\t Chain is missing" << endl;
    }
    SafeDelete(infile); 
  }
  assert(nEvTot>0);
  fCluster = new Cluster(FitP);
  return kStOK;
}
//_____________________________________________________________________________
Int_t StLaserAvClusterMaker::Make(){  
  StEvtHddr* header = (StEvtHddr*)GetDataSet("EvtHddr");
  assert(header);
  header->SetEventNumber(fToken+1);
  header->Print();
  StEvent *pEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));
  if (Debug()) {LOG_INFO << "StLaserAvClusterMaker::Make : StEvent has been retrieved " <<pEvent<< endm;}
  if (! pEvent) {LOG_INFO << "StLaserAvClusterMaker::Make : StEvent has not been found " << endm; return 0;}
  StTpcHitCollection *hitCollection = pEvent->tpcHitCollection();
  if ( !hitCollection )  {
    // Save the hit collection to StEvent...if needed
    hitCollection = new StTpcHitCollection();
    pEvent->setTpcHitCollection(hitCollection);
  }
  static Int_t NoOfInnerRows = St_tpcPadPlanesC::instance()->innerPadRows();
  Double_t samplingFrequency     = 1.e6*gStTpcDb->Electronics()->samplingFrequency(); // Hz
  Double_t TimeBinWidth = 1e9/samplingFrequency; // nsec
  Double_t zBinWidth     =  gStTpcDb->DriftVelocity()/samplingFrequency;
  if (fCluster == 0) return kStWarn;
  Long64_t nentries = fCluster->fChain->GetEntries();
  Long64_t nbytes = 0, nb = 0;
  Int_t nc = 0;
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = fCluster->LoadTree(jentry);
    if (ientry < 0) break;
    nb = fCluster->GetEntry(jentry);   nbytes += nb;
    Int_t token = fCluster->sector/100;
    if (token != fToken) continue;
    if (fCluster->NormLX <= 1) continue;
    if (fCluster->NormLY <= 1) continue;
    if (fCluster->NDFX < 1 || fCluster->NDFY < 1) continue;
    if (fCluster->chisqX/fCluster->NDFX > 2000) continue; 
    if (fCluster->chisqY/fCluster->NDFY > 2000) continue;
    if (fCluster->dmvpX        >  0.04) continue;
    if (fCluster->dmuY         >  0.04) continue;
    Double_t PadPitch        = gStTpcDb->PadPlaneGeometry()->innerSectorPadPitch();
    if (fCluster->row > NoOfInnerRows) 
      PadPitch               = gStTpcDb->PadPlaneGeometry()->outerSectorPadPitch();
    Int_t sector = ((Int_t) fCluster->sector)%100;
    Double_t time = fCluster->mvpX + St_tpcPadGainT0BC::instance()->T0(sector,fCluster->row,TMath::Nint(fCluster->muY));
    time += 3.0 * St_tpcElectronicsC::instance()->tau()/TimeBinWidth;
    StTpcPadCoordinate padcood(sector, fCluster->row, fCluster->muY, time);
    hitCollection->addHit(CreateTpcHit(&padcood, PadPitch*fCluster->dmuY, zBinWidth*fCluster->dmvpX));
    nc++;
  }
  if (! nc) return kStEOF;
  return kStOK;
}
//_____________________________________________________________
StTpcHit *StLaserAvClusterMaker::CreateTpcHit(StTpcPadCoordinate *padcoord, Double_t sigmaY, Double_t sigmaZ) {
  if (! padcoord) return 0;
  // Create  an instance of the StTpcHit from the tpcReader data
  static StTpcCoordinateTransform transform(gStTpcDb);
  static StTpcLocalSectorCoordinate local;
  static StTpcLocalCoordinate global;
  transform(*padcoord,local,kFALSE);
  transform(local,global);
  UInt_t hw = 1;   // detid_tpc
  hw += int(padcoord->sector()) << 4;     // (row/100 << 4);   // sector
  hw += int(padcoord->row())    << 9;     // (row%100 << 9);   // row
  
  Int_t npads = 0;
  hw += (npads   << 15);  // npads
  
  Int_t ntmbk = 0;
  hw += (ntmbk << 22);  // ntmbks...

  StThreeVector<double> hard_coded_errors(0,sigmaY,sigmaZ);//(fgDp,fgDt,fgDperp);
  Double_t q = 100;// * ((Double_t)St_tss_tssparC::instance()->ave_ion_pot() * 
  // (Double_t)St_tss_tssparC::instance()->scale())/(gain*wire_coupling) ;

  StTpcHit * hit = new StTpcHit(global.position(),hard_coded_errors,hw,q
				, (UChar_t ) 0  // c
				, (UShort_t) 0  // idTruth=0
				, (UShort_t) 0  // quality=0,
				, ++fId         // id
				, 0//cluster->p1 //  mnpad
				, 0//cluster->p2 //  mxpad
				, 0//cluster->t1 //  mntmbk
				, 0//cluster->t2 //  mxtmbk
				, padcoord->pad()
				, padcoord->timeBucket()
				, q);
  return hit;
}
