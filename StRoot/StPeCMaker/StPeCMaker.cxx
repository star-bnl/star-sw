// $Id: StPeCMaker.cxx,v 1.5 1999/07/15 13:57:20 perev Exp $
// $Log: StPeCMaker.cxx,v $
// Revision 1.5  1999/07/15 13:57:20  perev
// cleanup
//
// Revision 1.4  1999/06/27 22:45:29  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.3  1999/05/01 00:57:02  fisyak
// Change Clear function to defualt
//
// Revision 1.2  1999/04/08 16:37:15  nystrand
// MakeBranch,SetBranch removed
//
// Revision 1.1  1999/04/06 20:47:27  akio
// The first version
//
// Revision 1.0  1999/03/05 11:00:00  Nystrand
// initial version
//
//
///////////////////////////////////////////////////////////////////////////////
//
// StPeCMaker
//
// Description: 
//  Sample maker to access and analyze Peripheral Collisions through StEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Joakim Nystrand, LBNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StPeCMaker.h"
#include "StChain/StChain.h"
#include "StRun.h"
#include "StEvent.h"
#include "StL0Trigger.h"
#include "StGlobalTrack.h"
#include "TH1.h"
#include "SystemOfUnits.h"
#include "/afs/rhic/star/packages/SL99b/pams/global/inc/phys_constants.h"
#include <vector>


static const char rcsid[] = "$Id: StPeCMaker.cxx,v 1.5 1999/07/15 13:57:20 perev Exp $";

double minv(double m1, double px1, double py1, double pz1, double m2, double px2, double py2, double pz2);
void tagFiller(StEvent& event, HighPtTag_st& hptTag);

// Parameters
double rmax=2.0;
double Zmax=2.0;

Int_t StPeCMaker::Make() {
#if 0
  StEventReaderMaker* evMaker = (StEventReaderMaker*) gStChain->Maker("events");
  if (! event()) return kStOK; // If no event, we're done
  StEvent& ev = *(evMaker->event());
#endif
  StEvent* mEvent = (StEvent *) GetInputDS("StEvent");
  if (!mEvent) return kStOK; // If no event, we're done
  StEvent& ev = *mEvent;

  cout<<endl<<endl<<"This is StPeCMaker (JN 990305)"<<endl;
  // OK, we've got the event. Pass it and process it.

  // Create and fill a tag
  if (theTag) delete theTag;
  theTag = new HighPtTag_st;
  tagFiller(ev,*theTag);

  m_hstat->Fill(0);
  FindVertex(ev);

  GetMwcHits(ev);

  GetCtbHits(ev);

  return kStOK;
}


void StPeCMaker::FindVertex(StEvent& event) {

#ifdef ST_NO_TEMPLATE_DEF_ARGS
vector<long, allocator<long> > vertex[10];
#else
vector<long> vertex[10];
#endif
  StTrackCollection *tracks = event.trackCollection();
  StTrackIterator iter;
  StGlobalTrack *track;
  StGlobalTrack pectrk[16];
  StGlobalTrack vtxtrk[16];
  StThreeVectorD pectrkorg[16];

  double bfield=0.5*tesla;

  // Loop over Tracks and Check Number of main TPC Hits
  long ntk=0; long ntk5=0; long ntk10=0;
  for (iter = tracks->begin();
      iter != tracks->end(); iter++) {
    track = *iter;
    int nhits = track->tpcHits().size();
    ntk++; if ( nhits > 5) ntk5++; if ( nhits >10) ntk10++;
  }
  m_hntrk->Fill(ntk);
  m_hntrk5->Fill(ntk5);
  m_hntrk10->Fill(ntk10);

  if( ntk5 < 2 || ntk5 > 15 ){
    cout<<"Number of main TPC Tracks: "<<ntk5<<". Not a Peripheral Event. Return."<<endl;
    return;
  } 
  m_hstat->Fill(1);

  // Loop over Tracks and Select those that come close to the beam-axis
  long itk=0;
  for (iter = tracks->begin();
      iter != tracks->end(); iter++) {
    track = *iter;
    int nhits = track->tpcHits().size();
    // Scan Z-axis from -50 to +50 cm and determine min distance
    double rmin=100.0;
    StThreeVectorD temppoint;
    for( int i=0; i<2001; i++){
      double zv = -50.0 + 0.05*i;
      StThreeVectorD testpoint;
      testpoint = StThreeVectorD(0,0,zv);
      double s3 = track->helix().pathLength(testpoint);
      double xv = track->helix().x(s3);
      double yv = track->helix().y(s3);
      double rv = sqrt( xv*xv + yv*yv );
      if ( rv < rmin ){
        rmin=rv;
        temppoint=testpoint;
      }
    }
    if( rmin < rmax && nhits > 5 ){ 
      pectrk[itk] = *track;
      pectrkorg[itk] = temppoint;
      itk++;
    }
  }

  if( itk <= 1 ){
    cout<<"Number of Vertex Tracks: "<<itk<<". Not a Peripheral Event. Return"<<endl; 
    return;
  } 

  // Now we have itk (>1) good tracks. Let's see if there are any vertices.
  long ivtx=0;
  for ( long i=0; i<itk; i++) {
    long nhits = pectrk[i].tpcHits().size();
    double vpi = pectrk[i].helix().pathLength(pectrkorg[i]);
    double xv  = pectrk[i].helix().x(vpi);
    double yv  = pectrk[i].helix().y(vpi);
    double rad = sqrt(xv*xv+yv*yv);
    double zi  = pectrk[i].helix().z(vpi);
    for ( long j=i+1; j<itk; j++) {
      // Check if any of the 2 tracks belongs to a vertex already
      for( long icheck=0; icheck<ivtx; icheck++) {
        for( long jcheck=0; jcheck<vertex[icheck].size(); jcheck++){
          if( i==vertex[icheck][jcheck] || j==vertex[icheck][jcheck] )goto newcomb;
        }
      }
      double vpj = pectrk[j].helix().pathLength(pectrkorg[j]);
      double zj  = pectrk[j].helix().z(vpj);
      double Dz = fabs( zi - zj );
      if ( Dz > 2.0*Zmax ) continue;
      //Vertex Found!
      cout<<"Vertex Found!, i,j= "<<i<<" "<<j<<" "<<"zi,zj= "<<zi<<" "<<zj<<endl;
      vertex[ivtx].push_back(i);
      vertex[ivtx].push_back(j);
      double Zvert = 0.5*(zi+zj);
      // Loop over the other tracks to see if any more are in this vertex
      for ( long k=0; k<itk; k++ ) {
        if( k == i || k == j ) continue;
        double vpk = pectrk[k].helix().pathLength(pectrkorg[k]);
        double zk  = pectrk[k].helix().z(vpk);
        double Dz2 = fabs( zk - Zvert );
        if( Dz2 < Zmax ) {
          // Antother Vertex Track Found
          vertex[ivtx].push_back(k);
        }
      }
      ivtx++;
      if( ivtx > 9 ){cout<<" ERROR: Number of Vertices: "<<ivtx<<endl; return;}
    }
  newcomb: ;
  }
  cout<<"Number of Vertices: "<<ivtx<<endl;
  m_hnvtx->Fill(ivtx);
  for (int iloop = 0; iloop<ivtx; iloop++ ) {
    cout<<"Vertex: "<<iloop<<" Number of tracks:"<<vertex[iloop].size()<<endl;
    m_hnvtxtrk->Fill(vertex[iloop].size());
  }

  //Select Events with only 1 Primary Vertex
  if( ivtx != 1 ) return;

  //Divide Events according to number of Primary Vertex Tracks
  if( vertex[ivtx-1].size() == 2){
    m_hstat->Fill(2);
    long i1 = vertex[ivtx-1][0];
    long i2 = vertex[ivtx-1][1];

    //Track 1
    double vp1 = pectrk[i1].helix().pathLength(pectrkorg[i1]);
    double px1 = pectrk[i1].helix().momentumAt(vp1,bfield).x();
    double py1 = pectrk[i1].helix().momentumAt(vp1,bfield).y();
    double pz1 = pectrk[i1].helix().momentumAt(vp1,bfield).z();
    double pt1 = sqrt( px1*px1 + py1*py1 );
    long   q1  = 0;
    if( bfield*(pectrk[i1].helix().h()) < 0.0 )q1=1;
    if( bfield*(pectrk[i1].helix().h()) > 0.0 )q1=-1;
    //Track 2
    double vp2 = pectrk[i2].helix().pathLength(pectrkorg[i2]);
    double px2 = pectrk[i2].helix().momentumAt(vp2,bfield).x();
    double py2 = pectrk[i2].helix().momentumAt(vp2,bfield).y();
    double pz2 = pectrk[i2].helix().momentumAt(vp2,bfield).z();
    double pt2 = sqrt( px2*px2 + py2*py2 );
    long   q2  = 0;
    if( bfield*(pectrk[i2].helix().h()) < 0.0 )q2=1;
    if( bfield*(pectrk[i2].helix().h()) > 0.0 )q2=-1;

    if( q1==0 || q2==0 )cout<<"WARNING: q1,q2= "<<q1<<" "<<q2<<endl;

    long sumQ = q1+q2;
    m_hsumq->Fill(sumQ);
    double ZVertex = 0.5*(pectrk[i1].helix().z(vp1)+pectrk[i2].helix().z(vp2));
    m_hzvert->Fill(ZVertex);

    if( pt1>0.001 && pt2>0.001 && sumQ==0 ){
      m_hstat->Fill(3);

      double sumpt = sqrt( (px1+px2)*(px1+px2) + (py1+py2)*(py1+py2) );
      m_hsumpt->Fill(sumpt); 

      //Assume pion mass
      double m1 = M_PION_PLUS; double m2 = M_PION_PLUS;
      double M = minv(m1,px1,py1,pz1,m2,px2,py2,pz2);
      m_hminvpi->Fill(M);
      
      //Assume kaon mass
      m1 = M_KAON_PLUS; m2 = M_KAON_PLUS;
      M = minv(m1,px1,py1,pz1,m2,px2,py2,pz2);
      m_hminvk->Fill(M);
    }

  }

  if( vertex[ivtx-1].size() == 4){
  }

  if( vertex[ivtx-1].size() == 6){
  }

  return;

}

void StPeCMaker::GetCtbHits(StEvent& event) {
  // Not available at this time

}

void StPeCMaker::GetMwcHits(StEvent& event) {
  // Not avaliable at this time

  //  StTriggerDetectorCollection *trigs = event.triggerDetectorCollection();
  //  StVecMwcSector mwcvec = trigs->mwcSectors();
  //  cout<<"Size of mwcvec: "<<mwcvec.size()<<endl;
  //  cout<<"Do it this way: "<<trigs->mwcSectors().size()<<endl;

}

StPeCMaker::StPeCMaker(const Char_t *name, const Char_t *title) : StMaker(name, title) {
  drawinit = kFALSE;
  theTag = 0;
}

StPeCMaker::~StPeCMaker() {
}

Int_t StPeCMaker::Init() {
  m_hstat     = new TH1F("hstat","Statistics: Neve, N2-15, N2VtxTrk, Nmom",4,-0.5,3.5);
  m_hntrk     = new TH1F("hntrk","Number of TPC Tracks",50,-0.5,49.5);
  m_hntrk5    = new TH1F("hntrk5","#TPC Tracks, nhits>5",50,-0.5,49.5);
  m_hntrk10   = new TH1F("hntrk10","#TPC Tracks, nhits>10",50,-0.5,49.5);
  m_hnmwchts  = new TH1F("hnmwchts","#MWC Hits",50,-0.5,49.5);
  m_hnctbhts  = new TH1F("hnctbhts","#CTB Hits",50,-0.5,49.5);
  m_hnvtx     = new TH1F("hnvtx","#Primary Vertices",10,-0.5,9.5);
  m_hnvtxtrk  = new TH1F("hnvtxtrk","#Primary Vertex Tracks",50,-0.5,49.5);
  m_hsumq     = new TH1F("hsumq","Sum Q",11,-5.5,5.5);
  m_hsumpt    = new TH1F("hsumpt","Sum Pt",50,0.0,0.75);
  m_hzvert    = new TH1F("hzvert","Z Vertex (cm)",50,-60.0,60.0);
  m_hminvpi   = new TH1F("hminvpi","2-Track Evts. Minv pions",50,0.2,1.5);
  m_hminvk    = new TH1F("hminvk","2-Track Evts. Minv kaons",50,0.8,2.0);
  return StMaker::Init();
}


void StPeCMaker::Clear(Option_t *opt) {
  if (theTag) {
    delete theTag;
    theTag = 0;
  }
  StMaker::Clear();
}

Int_t StPeCMaker::Finish() {
  //TFile *myfile = new TFile("file.root","RECREATE");
  //m_vrad->Write();
  //myfile->Close();
  return kStOK;
}

ClassImp(StPeCMaker)




