#include "MatchedTrk.h"
#include "StTrsMaker/include/StTpcDbSlowControl.hh" // for drift velocity

#include "tables/St_g2t_vertex_Table.h" // only for printout

#include "TH2.h"


MatchedTrk:: MatchedTrk(StVertexMaker* head, int *ipar, float *fpar, CtbResponse* ctbRes,St_dst_track * track ){
  assert(head);
  assert(ctbRes);
  assert(track);
  float aa=gStTpcDb->DriftVelocity();
  long Ntrk = track->GetNRows();
  printf("Do matching of %d dstTracks to CTB hist, drift vel=%f\n",(int)Ntrk,aa);
 
  const double Rctb=213.6; // (cm) radius of the CTB 
  const float CtbEtaSeg=0.5, CtbPhiSeg=C_PI/30;

  // params 
  int  MinTrkPoints        = ipar[1];
  double MaxTrkDcaRxy      =fpar[1]; //DCA to nominal beam line for each track
  double MinTrkPt=fpar[2];// ~ pT=0.16(GeV/c) == R=2 (m )in 2001
  float  MatchCtbMax_eta=CtbEtaSeg/2.+fpar[3];
  float  MatchCtbMax_phi=CtbPhiSeg/2.+C_PI*fpar[4]/180.;

  // Get BField from gufld(,) 
  //  cout<<"Trying to Get the BField the old way..."<<endl;
  float x[3] = {0,0,0};
  float b[3];
  gufld(x,b);
  double bfield = 0.1*b[2]; //This is now Tesla.
  if(fabs(bfield)<0.0001) bfield=0.00222; // to make helix heappy

  dst_track_st *glb_track_pointer = track->GetTable();
  int n1=0,n2=0,n3=0, n4=0,n5=0,n6=0;
  for (int l=0; l<Ntrk; l++,glb_track_pointer++) {
    if(glb_track_pointer->iflag<=0) continue;
    n1++;
    head->hmtr[0]->Fill(1);
    
    long NPoints = glb_track_pointer->n_point;

    // First point on Helix
    double x0 = glb_track_pointer->r0*cos(C_RAD_PER_DEG*glb_track_pointer->phi0);
    double y0 = glb_track_pointer->r0*sin(C_RAD_PER_DEG*glb_track_pointer->phi0);
    double z0 = glb_track_pointer->z0;
    StThreeVectorD origin(x0*centimeter, y0*centimeter, z0*centimeter);
    
    // Helicity / Sense of Curvatutre
    double h  = 1.0;  if( bfield*glb_track_pointer->icharge > 0.0 )h=-1.0;
    double qtrk = 1.0; if( h*bfield > 0.0)qtrk=-1.0;
    
    // get Track direction at first point
    double ptinv  = glb_track_pointer->invpt;
    double tanl   = glb_track_pointer->tanl;
    double psi    = (C_PI/180.0)*glb_track_pointer->psi; 
    if(psi<0.0){psi=psi+2.*C_PI;}
    
    double px   = (1./ptinv)*cos(psi);
    double py   = (1./ptinv)*sin(psi);
    double pz   = (1./ptinv)*tanl;

    StThreeVectorD MomFstPt(px*GeV, py*GeV, pz*GeV);
    
    StPhysicalHelixD TrkHlx(MomFstPt, origin, bfield*tesla, qtrk);
    //printf("\ntrack i=%d px,y,z=%f %f %f psi/deg=%f tanl=%f, pT=%.3f\n",(int)l,px,py,pz,psi/C_PI*180,tanl,1./ptinv);
    //printf(" track x0,y0,z0=%.1f, %.1f, %.1f, phi0/deg=%.1f, r0/cm=%.1f\n",x0,y0,z0,glb_track_pointer->phi0,glb_track_pointer->r0);
 

    //           check Rxy_min condition      
    double xorigin = 0.0; double yorigin = 0.0;
    double spath = TrkHlx.pathLength(xorigin, yorigin);
    StThreeVectorD posDCA = TrkHlx.at(spath);
    //cout<<" DCA Position: "<<posDCA<<endl;
    double x_m = posDCA.x(), y_m = posDCA.y();
    double dmin = sqrt(x_m*x_m + y_m*y_m);
    if( dmin > MaxTrkDcaRxy ) continue;
    n2++;
    head->hmtr[0]->Fill(2);

    CanTrk trk0;
    trk0.x0=posDCA.x();
    trk0.y0=posDCA.y();
    trk0.z0=posDCA.z();
    trk0.glb_track_pointer=glb_track_pointer;
    primCan.push_back(trk0);

    if(NPoints <= MinTrkPoints) continue;
    n3++;
    head->hmtr[0]->Fill(3);


    // reject low pT tracks
    if(1./ptinv<MinTrkPt){
      //printf("ignore this low PT=%f track\n",1./ptinv);
      continue;
    }

    //Find momentum direction at vertex point
    StThreeVectorD pmom;
    pmom = TrkHlx.momentumAt(spath, bfield*tesla);
    double beta = pmom.mag()/sqrt(pmom.mag()*pmom.mag()+0.139*0.139); //Assume pion 
    float strag=0.0136/beta/pmom.mag()*fabs(spath);
    if(fabs(bfield)<0.01) strag=0.0136*fabs(spath);

    head->hmtr[1]->Fill(strag); 
    head->hmtr[2]->Fill(-spath); 
    printf("stragling=%f %f %f %f \n",strag,beta,pmom.mag(),spath);


    n4++;
    head->hmtr[0]->Fill(4);

    pairD  d2; 
    d2 = TrkHlx.pathLength(Rctb);
    //printf(" path 1=%f, 2=%f, period=%f, R=%f\n",d2.first ,d2.second,TrkHlx.period(),1./TrkHlx.curvature());

    // assert(d2.first<0); // propagate backwards
    //assert(d2.second>0); // propagate forwards
    if(d2.first>=0 || d2.second<=0) {
      n5++;
      head->hmtr[0]->Fill(5);
      printf("WARN MatchTrk , unexpected solution for track crossing CTB\n");
      printf(" tack=%d, d2.firts=%f, second=%f, track ignored\n",
	     l,d2.first, d2.second);
    }
    
    StThreeVectorD posCTB = TrkHlx.at(d2.second);
    double xmagn = sqrt( posCTB.x()*posCTB.x() + posCTB.y()*posCTB.y() );
    // printf(" punch2 x,y,z=%.1f, %.1f, %.1f, Rxy=%.1f\n",posCTB.x(),posCTB.y(),posCTB.z(),xmagn);
    
    float phi=atan2(posCTB.y(),posCTB.x());
    if(phi<0) phi+=2*C_PI;
    
    //printf("posCTB.z()=%f posDCA.z()=%f\n",posCTB.z(),posDCA.z());
    
    int tSl;
    float zOff=99999; 
    int bXing=-1;
    for(tSl=0;tSl<MxTimeSlot;tSl++) { // loop over timeSlots
      
      if(ctbRes->hits[tSl].size()<=0) continue; // no CTB hits were gathered
      // get shift along Z-axis in TPC for this track & bXing
      zOff=(firstBXing+tSl)*bXingTimeSep*1.e-9*gStTpcDb->DriftVelocity();
      if(posCTB.z()<0) zOff*=-1; // change sign for negative Z

      // check Zdca condition      
      float Zdca=posDCA.z()-zOff;
      //printf("tSl=%d, zOff=%f, Zdca=%f",tSl,zOff,Zdca);
      //printf(" zOff= %f *%d\n",bXingTimeSep*1.e-9*gStTpcDb->DriftVelocity(),tSl+firstBXing);

      if(fabs(Zdca) >head->zCutppLMV) continue;
      
      uint ih;
      for(ih=0;ih<ctbRes->hits[tSl].size();ih++) {// loop over CTB hits
	
	// match to CTB slats in phi
	
	float del_phi=phi-ctbRes->hits[tSl][ih].phi;
       	if(del_phi>C_PI) del_phi-=2*C_PI;
	if(del_phi<-C_PI) del_phi+=2*C_PI;
	//printf("match ih=%d del_phi=%f/deg\n",ih, del_phi/C_PI*180);
	if(fabs(del_phi) >MatchCtbMax_phi) continue;

	// match to CTB slats in eta
	float eta=asinh((posCTB.z()-zOff)/xmagn);
	float del_eta=eta-ctbRes->hits[tSl][ih].eta;
	//printf("  match ih=%d del_eta=%f\n",ih, del_eta);
	if(fabs(del_eta) >MatchCtbMax_eta) continue;
	
	bXing=tSl;
	//printf("  CTB match OK:  del_eta=%.2f, del_phi/deg=%.1f for %d bXing\n", del_eta,del_phi/C_PI*180,bXing+firstBXing);
	n6++;

	// Now it is a good track
	head->hmtr[0]->Fill(6);
	head->hctb[6]->Fill(ctbRes->hits[tSl][ih].ID);
	// add this track to my list
	StThreeVectorD origin1(x0*centimeter,y0*centimeter,(z0-zOff)*centimeter);
	struct Jtrk trk1;
	trk1.glb_track_pointer=glb_track_pointer;
	trk1.helix=StPhysicalHelixD(MomFstPt, origin1, bfield*tesla, qtrk);
	trk1.sigma=strag;
	tracks[tSl].push_back(trk1);

	((TH2F *)head->hPiFi[2])->Fill(eta,phi/C_PI*180);
	head->hPiFi[3]->Fill(del_eta);
	head->hPiFi[4]->Fill(del_phi/C_PI*180);

	break;
      }
      if(bXing>=0) break;
    }
      
    //if(bXing<0)       printf(" CTB match result NONE \n");
    
  } // end of loop over tracks
  
  printf(", used n1=%d n2=%d n3=%d n4=%d  n6=%d\n",n1,n2,n3,n4,n6);

  // copy Geant vertex info
  for(int tSl=0;tSl<MxTimeSlot;tSl++) 
    GVER[tSl]=ctbRes->GVER[tSl];

  head->hPiFi[5]->Fill(tracks[trigBXing].size());

  printf("total match to CTB \nbXing\tnTracks    GVER  \n");
  for(int i=0;i<MxTimeSlot;i++) {
    if(!tracks[i].size()) continue; 
    printf("%d    \t%d  ",i+firstBXing,tracks[i].size());
    g2t_vertex_st *v=(g2t_vertex_st *)GVER[i];
    if(v)
      printf("  x=%6.2f y=%6.2f z=%6.2f \n",v->ge_x[0],v->ge_x[1],v->ge_x[2]);
    else
      printf("    no vertex\n");
  }

  printf("total primCandidates=%d\n",primCan.size());
  printf("CHeck :getPileupBXing()=%d (-26 == NONE)\n",getPileupBXing()+firstBXing);

}


//====================================================
//====================================================
int MatchedTrk::getPileupBXing(){
  int bXing0=getTrigBXing();

  int d2=999999;
  int bXing=-1;
  for (int i=0;i<MxTimeSlot;i++) {
    if(i==bXing0) continue;
    if(tracks[i].size()<2) continue;
    int j=i-bXing0;
    int j2=j*j;
    //printf("%d %d %d\n",i,j2,d2);
    if(d2<j2) continue;
    d2=j2;
    bXing=i;
  }
  
  return bXing;
}








