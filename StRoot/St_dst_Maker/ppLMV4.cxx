// $Id: ppLMV4.cxx,v 1.1 2001/11/29 00:19:08 balewski Exp $
// $Log: ppLMV4.cxx,v $
// Revision 1.1  2001/11/29 00:19:08  balewski
// *** empty log message ***
//
// Revision 1.1  2001/06/12 23:16:22  balewski
// reject pileup in ppLMV
//
// Revision 1.1  2001/04/12 15:46:27  balewski
// *** empty log message ***
///////////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <vector>
#include "StChain.h"
#include "MatchedTrk.h"
#include "tables/St_g2t_ctf_hit_Table.h"
#include "tables/St_g2t_vertex_Table.h" // only for histo
#include "tables/St_g2t_track_Table.h"

#include "TH2.h"
void cts_get_ctb_indexes ( long volume, long &i_phi, long &i_eta ) ;

#include "StPrimaryMaker.h"

#include "SystemOfUnits.h"
#if !defined(ST_NO_NAMESPACES)
using namespace std::vector;
using namespace units;
#endif
#include "StThreeVectorD.hh"
#include "StHelixD.hh"
#include "StPhysicalHelixD.hh"
#include "TMath.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_vertex_Table.h"
#include "StDetectorDefinitions.h"

#include "Stypes.h"
#include "StEventTypes.h" // for StEvent only
#include "StVertexId.h"
#include "math_constants.h"

#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)

//static const char rcsid[] = "$Id: ppLMV4.cxx,v 1.1 2001/11/29 00:19:08 balewski Exp $";

struct Jcyl {float eta,phi;};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
long StPrimaryMaker::ppLMV4(MatchedTrk &maTrk,St_dst_track *trackAll, St_dst_vertex *vertex, Int_t mdate)
{
  //int bXing=maTrk.getTrigBXing();
  //int bXing=maTrk.getPileupBXing();
  int bXing=trigBXing;// temp
  printf(" THIS IS ppLMV4 -START, use only tracks matched to CTB in bXing=%d\n",bXing+firstBXing);

  if(bXing<0) 
    {printf("No tracks matched to selected bXing\n"); return kStOk;}

  vector <Jtrk> *tracks=&(maTrk.tracks[bXing]);

  int Ntrk=(*tracks).size();
  printf("passed tracks match to CTB  nTracks=%d\n",Ntrk);

  if( Ntrk <= 1 ){
    cout<<"ppLMV4: Event contains "<<Ntrk<<" global tracks. ";
    cout<<"No vertex can be reconstructed. continue.."<<endl;
    //return kStWarn; 
  }

  static int eveId=0;
  eveId++;
   
  // Get BField from gufld(,) 
  //  cout<<"Trying to Get the BField the old way..."<<endl;
  float x[3] = {0,0,0};
  float b[3];
  gufld(x,b);
  double bfield = 0.1*b[2]; //This is now Tesla.

  // Parameters
  double DVtxMax      = 4.0;

  // Determine if SVT is in or not, depending on geometry and date in db
  long Is_SVT = 0;
  if( mdate > 20000700 )Is_SVT=1;


  //  ----------  D O   F I N D    V E R T E X
  
  printf("%s - search for vertex using %d matched tracks: start ...\n",GetName(),Ntrk);

  //Do the actual vertex fitting, continue until good
  double A11=0.0,A12=0.0,A13=0.0,A21=0.0,A22=0.0,A23=0.0;
  double A31=0.0,A32=0.0,A33=0.0; // Matrix Elements
  double C11=0.0,C12=0.0,C13=0.0,C21=0.0,C22=0.0,C23=0.0;
  double C31=0.0,C32=0.0,C33=0.0; // C = A^-1
  int done = 0;
  double chi2=0;
  StThreeVectorD XVertex(999.,888.,777.);
  while( done != 1 ){

    // Check that there at least are 2 tracks
    if( (*tracks).size() <= 1 ){
      cout<<"ppLMV4: Fewer than 2 track remains. No vertex found."<<endl;
      return kStWarn;
    }
  
    // Begin by doing a fit
    A11=0.0,A12=0.0,A13=0.0,A21=0.0,A22=0.0,A23=0.0;
    A31=0.0,A32=0.0,A33=0.0; // Matrix Elements
    C11=0.0,C12=0.0,C13=0.0,C21=0.0,C22=0.0,C23=0.0;
    C31=0.0,C32=0.0,C33=0.0; // C = A^-1
    double b1=0.0,b2=0.0,b3=0.0;
    // Compute matrix A and vector b
    for(unsigned int itr=0; itr < (*tracks).size(); itr++){ 
      //      sigma[itr]=1.0;
      double xo=0.0,yo=0.0;
      double spath = (*tracks)[itr].helix.pathLength(xo,yo);
      StThreeVectorD XClosest = (*tracks)[itr].helix.at(spath);
      StThreeVectorD XMomAtClosest = (*tracks)[itr].helix.momentumAt(spath,bfield*tesla);
      double xp   = XClosest.x(); double yp= XClosest.y(); double zp= XClosest.z();  
      double xhat = XMomAtClosest.x()/XMomAtClosest.mag();
      double yhat = XMomAtClosest.y()/XMomAtClosest.mag();
      double zhat = XMomAtClosest.z()/XMomAtClosest.mag();
      A11=A11+(yhat*yhat+zhat*zhat)/(*tracks)[itr].sigma;
      A12=A12-(xhat*yhat)/(*tracks)[itr].sigma;
      A13=A13-(xhat*zhat)/(*tracks)[itr].sigma;
      A22=A22+(xhat*xhat+zhat*zhat)/(*tracks)[itr].sigma;
      A23=A23-(yhat*zhat)/(*tracks)[itr].sigma;
      A33=A33+(xhat*xhat+yhat*yhat)/(*tracks)[itr].sigma;
      b1=b1 + ( (yhat*yhat+zhat*zhat)*xp - xhat*yhat*yp - xhat*zhat*zp )/(*tracks)[itr].sigma;
      b2=b2 + ( (xhat*xhat+zhat*zhat)*yp - xhat*yhat*xp - yhat*zhat*zp )/(*tracks)[itr].sigma;
      b3=b3 + ( (xhat*xhat+yhat*yhat)*zp - xhat*zhat*xp - yhat*zhat*yp )/(*tracks)[itr].sigma;
    }
    A21 = A12; A31=A13; A32=A23;

    // Invert A
    double detA =   A11*A22*A33 + A12*A23*A31 + A13*A21*A32;
    detA = detA   - A31*A22*A13 - A32*A23*A11 - A33*A21*A12;
    //    cout<<"Determinant= "<<detA<<endl;
    //    cout<<"A11,A12,A13: "<<A11<<" "<<A12<<" "<<A13<<endl;
    //    cout<<"A21,A22,A23: "<<A21<<" "<<A22<<" "<<A23<<endl;
    //    cout<<"A31,A32,A33: "<<A31<<" "<<A32<<" "<<A33<<endl;
    //    cout<<"b1,b2,b3 "<<b1<<" "<<b2<<" "<<b3<<endl;
    C11=(A22*A33-A23*A32)/detA; C12=(A13*A32-A12*A33)/detA; C13=(A12*A23-A13*A22)/detA;
    C21=C12;                    C22=(A11*A33-A13*A31)/detA; C23=(A13*A21-A11*A23)/detA;
    C31=C13;                    C32=C23;                    C33=(A11*A22-A12*A21)/detA;

    // Find Vertex Position
    double Xv = C11*b1 + C12*b2 + C13*b3;
    double Yv = C21*b1 + C22*b2 + C23*b3;
    double Zv = C31*b1 + C32*b2 + C33*b3;
    XVertex.setX(Xv); XVertex.setY(Yv); XVertex.setZ(Zv);
    //    cout<<"Vertex Position   : "<<XVertex.x()<<" "<<XVertex.y()<<" "<<XVertex.z()<<endl;
    //    cout<<"Error in Position : "<<sqrt(C11)<<" "<<sqrt(C22)<<" "<<sqrt(C33)<<endl;
    

    // Check if the fit is any good
    // Loop over tracks again to get Chi2 and check each track's deviation
    double dmax=0.0;
#ifdef ST_NO_TEMPLATE_DEF_ARGS
  wrong lines  
  vector<StPhysicalHelixD,allocator<StPhysicalHelixD> >::iterator itehlx=(*tracks).helix.begin(), i1keep;
    vector<double,allocator<double> >::iterator itesig=sigma.begin(),i2keep;
    vector<long,allocator<long> >::iterator iteind=index.begin(),i3keep;
#else
    vector<Jtrk >::iterator itehlx=(*tracks).begin(), i1keep;
#endif
    while( itehlx != (*tracks).end()){
      //      sigma[itr]=1.0;
      StPhysicalHelixD hlx = (*itehlx).helix;
      double sig = (*itehlx).sigma;
      double spath = hlx.pathLength(XVertex); 
      StThreeVectorD XHel = hlx.at(spath);
      double d=(XHel.x()-XVertex.x())*(XHel.x()-XVertex.x());
         d = d+(XHel.y()-XVertex.y())*(XHel.y()-XVertex.y());
         d = d+(XHel.z()-XVertex.z())*(XHel.z()-XVertex.z());
         d = sqrt(d);
      chi2 = chi2 + (d*d)/(sig*sig);
      double drel = d/sig;
      if( drel > dmax ){
	// Save the track that deviates the most from vertex
        dmax = drel;
        i1keep = itehlx;
      }

      itehlx++; 
    }

    if( dmax > DVtxMax ){
      //      cout<<"Removing a track! dmax= "<<dmax<<endl;
      (*tracks).erase(i1keep);
      done=0;
    }
    else{
      done=1;
    }
  } // End While Loop

  double  chi2pdof = chi2/((*tracks).size()-1);


  cout<<"ppLMV4: Primary Vertex found! Position: "<<XVertex<<", used tracks="<<(*tracks).size()<<endl;

  {// fill some histos
    hPiFi[13]->Fill((*tracks).size());
    for(int j=0;j<(*tracks).size();j++) {
      float pt=1./(*tracks)[j].glb_track_pointer->invpt;
      int npoint=(*tracks)[j].glb_track_pointer->n_point;
     hPiFi[14]->Fill(pt);
     hPiFi[15]->Fill(npoint);
  }

    float rXver=XVertex.x();
    float rYver=XVertex.y();
    float rZver=XVertex.z();

    ((TH2F*)hPiFi[6])->Fill(rXver,rYver);
    hPiFi[7]->Fill(rXver);
    hPiFi[8]->Fill(rYver);
    hPiFi[9]->Fill(rZver);

    // --------------  A C C E S S    G E A N T   V E R T E X  (for histo)
    g2t_vertex_st *GVER=(g2t_vertex_st *)maTrk.GVER[bXing];  
    //  printf("GVER add=%d\n",GVER);
    if(GVER) {
      hPiFi[10]->Fill(GVER->ge_x[2]-rZver);
      hPiFi[11]->Fill(GVER->ge_x[0]-rXver);
      hPiFi[12]->Fill(GVER->ge_x[1]-rYver);
      printf("Z Geant-found=%.2f, dx=%.2f, dy=%.2f\n",GVER->ge_x[2]-rZver,GVER->ge_x[0]-rXver,GVER->ge_x[1]-rYver);
    }

  }  // end of histos


  Int_t nrows = vertex->GetNRows();
  long IVertex = nrows+1; //By definition
  dst_track_st *sec_pointer = trackAll->GetTable();


  // printf(" Mark the vertex tracks in the global_trk table\n");
  for (long ll=0; ll<trackAll->GetNRows(); ll++){
    long idt = sec_pointer->id;
    long icheck;
    icheck = 0;
    for(unsigned int ine=0; ine < (*tracks).size(); ine++){
      if( idt == (*tracks)[ine].glb_track_pointer->id )icheck=1;
    }  
    Int_t istart_old;
    if( icheck == 1 ){
      // This track was included in the Vertex
      istart_old = sec_pointer->id_start_vertex;
      sec_pointer->id_start_vertex = 10*IVertex + istart_old;
    }
    sec_pointer++;
  }


  // printf(" Fill the dst_vertex table\n");
  dst_vertex_st primvtx;
  primvtx.vtx_id      = kEventVtxId;
  primvtx.n_daughters = (*tracks).size();
  primvtx.id          = IVertex;
  primvtx.iflag       = 1;
  if( (Is_SVT == 1) ){
    primvtx.det_id    = kTpcSsdSvtIdentifier; 
  }
  else{
    primvtx.det_id    = kTpcIdentifier; //TPC track by definition
  }
  primvtx.id_aux_ent  = 0;
  primvtx.x           = XVertex.x();
  primvtx.y           = XVertex.y();
  primvtx.z           = XVertex.z();
  primvtx.covar[0]    = C11;
  primvtx.covar[1]    = C12;
  primvtx.covar[2]    = C22;
  primvtx.covar[3]    = C13;
  primvtx.covar[4]    = C23;
  primvtx.covar[5]    = C33;
  primvtx.chisq[0]    = chi2pdof;
  primvtx.chisq[1]    = 1.0; // Need to find the prob func in Root
  vertex->AddAt(&primvtx,nrows);

  // printf("end of ppLMV4\n");
  return kStOk;
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

void StPrimaryMaker::ppLMVuse(int *parI, float *parF) {
  char *nameI[10]={"CtbThres/ch","MinTrkPonits","i2","i3","i4","i5","i6","i7","i8","i9"};
  char *nameF[10]={"CtbThres/MeV","MaxTrkDcaRxy","MinTrkPt/GeV","CtbEtaErr","CtbPhiErr/deg","MaxTrkDcaZ","f6","f7","f8","f9"};
  printf("\nppLMV use new set of params\n    INT:  "); 
  int i;
  for( i=0;i<10;i++) { 
    ppLMVparI[i]=parI[i];
    printf("%s=%d   ", nameI[i],ppLMVparI[i]);
  }
  printf("\n   FLOAT:  "); 
  for( i=0;i<10;i++) { 
    ppLMVparF[i]=parF[i];
    printf("%s=%f  ", nameF[i],ppLMVparF[i]);
  }
  printf("\n\n");
  zCutppLMV=ppLMVparF[5];
}


    
#if 0
##############################################################


  StEvent *Steve= (StEvent *) GetInputDS("StEvent");
  assert(Steve);
  // Tracks from StEvent
  StSPtrVecTrackNode& trackNodes = Steve->trackNodes();
  cout<<"** StPreEclRMaker:: Node size **"<<trackNodes.size()<<endl;
  int allGlobals =0;
  StTrack* track1;
  for (size_t nodeIndex=0; nodeIndex<trackNodes.size(); nodeIndex++) {
    
    size_t numberOfTracksInNode =  trackNodes[nodeIndex]->entries(global);
    for (size_t trackIndex=0; trackIndex<numberOfTracksInNode; trackIndex++) {
      track1 =trackNodes[nodeIndex]->track(global,trackIndex);
      if (track1){
	
	//emcFinalCoord(fieldB*tesla, track, &finalPosition, &finalMomentum);
      }
    }
  }
#endif
