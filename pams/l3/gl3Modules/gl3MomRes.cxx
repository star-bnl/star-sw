//:>------------------------------------------------------------------
//: FILE:       gl3MomRes.cxx
//: HISTORY:
//:              4may2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3MomRes.h"
#define BFACT 0.0029980

//####################################################################
//
//####################################################################
int gl3MomRes::init ( l3List* histos ) {
  char hid[50];
  char title[100];

  strcpy(hid,"momres");
  strcpy(title,"Momentum resolution");
  hist=new gl3Histo(hid,title,100,-50.,50.);
  histos->append((void*)hist);
 
  return 0 ;
}
//####################################################################
//
//####################################################################
int gl3MomRes::process ( gl3Event* event ) {
  
  // printf("In momres\n");

  float todeg = 180./ M_PI;
  float dpsi=0.03;
  float dx=0.5,dy=0.5,dz=1;
  int MinHits=20;

   //only consider the event if more than one track.
   int Ntracks=event->getNTracks();
   if(Ntracks<2) return 0;
   
   float *x0 = new float[Ntracks];
   float *y0 = new float[Ntracks];
   float *z0 = new float[Ntracks];
   float *psi = new float[Ntracks];
   float *pt = new float[Ntracks];
   float *px = new float[Ntracks];
   float *py = new float[Ntracks];
   float *pz = new float[Ntracks];
   float *p = new float[Ntracks];
   int   *hits= new int[Ntracks];

   //calculate coordinates of tracks in this event:
   for(int c=0; c<Ntracks; c++)
     {
       
       gl3Track *cotrack=(gl3Track*)event->getTrack(c);
       hits[c]=cotrack->nHits;
       if(hits[c]<MinHits) continue;
       
       x0[c]=cotrack->r0*cos(cotrack->phi0);
       y0[c]=cotrack->r0*sin(cotrack->phi0);
       z0[c]=cotrack->z0;
       psi[c]=cotrack->psi;
       pt[c]=cotrack->pt;
     
       px[c]=cotrack->pt*cos(cotrack->psi);
       py[c]=cotrack->pt*sin(cotrack->psi);
       pz[c]=cotrack->pt*cotrack->tanl;
       p[c]=sqrt(px[c]*px[c]+py[c]*py[c]+pz[c]*pz[c]);
    
     }
   /*gl3Track *track1=(gl3Track*)event->getTrack(0);
   gl3Track *track2=(gl3Track*)event->getTrack(1);

   double pz1=track1->pt*track1->tanl;
   double pz2=track2->pt*track2->tanl;
   double dpz1=sqrt(track1->dpt*track1->dpt+track1->dtanl*track1->dtanl);
   double dpz2=sqrt(track2->dpt*track2->dpt+track2->dtanl*track2->dtanl);
   double dpz;
   if(dpz1>dpz2) dpz=dpz1;
   else dpz=dpz2;*/
   
   // FILE *file=fopen("momres.dat","a");
   // fprintf(file,"%f %f\n",dPt,track1->pt);
   //fclose(file);
   
   for(int i=0; i<Ntracks; i++)
     {
       if (hits[i]<MinHits) continue;
       //compare this track with the others to find match
       for(int j=i+1; j<Ntracks; j++)
	 {
	   if(hits[j]<MinHits) continue;
	   if(M_PI-dpsi<fabs(psi[i]-psi[j]) && fabs(psi[i]-psi[j])<M_PI+dpsi
	      && fabs(x0[i]-x0[j])<dx && fabs(y0[i]-y0[j])<dy && fabs(z0[i]-z0[j])<dz)
	     {
	       //printf("matching x0 %f %f y0 %f %f z0 %f %f\n",x0[i],x0[j],y0[i],y0[j],z0[i],z0[j]);
	       double dP=p[i]-p[j];
	       
	       FILE *file=fopen("momres.dat","a");
	       fprintf(file,"%f %f\n",dP,p[i]);
	       fclose(file);
	       
	       //intf("dPt %f\n",dPt);
//	       hist->Fill(100*dPt/pt[i],1);
	     
	     }
	   
	 }
     }
   
   delete [] x0;
   delete [] y0;
   delete [] z0;
   delete [] psi;
   delete [] px;
   delete [] py;
   delete [] pz;
   delete [] p;
   delete [] pt;
   delete [] hits;
   	  
   //printf("End of momres\n");
  return 1 ;
}
