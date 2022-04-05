// C-style global function call by Minuit
#include <math.h>
#include <assert.h>
#include <TVector3.h>
#include <TH1.h>

#include "UtilBeamLine3D.h"
const float detEpsilon=1e-4; // tolernace for paralell lines for DCA computation.

//==========================================================
int twoLineDca3D(double& lambda,double& kappa,TVector3& V, TVector3& U, TVector3& R,TVector3& P){ // returns non-zero error code
  /* find lambda and kappa which define the two points on 
     each vector that are on either end of the DCA.  */
  double  a= P*U;
  double det=1-a*a;
  if(util.fcnMon1) {
    util.hA[20]->Fill(det);
  }
  if(det<detEpsilon) return -1; // abort processing of this track

  TVector3 VmR=V-R; // will be faster
  double  b= VmR*P;
  double  c = VmR*U;
  lambda = (b-a*c)/det;
  kappa =  (a*b-c)/det;
  if(util.fcnMon1&0x2) {
    printf("\nbm   V=%.1f %.1f %.1f,  U=%.3f %.3f %.3f\n",V.x(),V.y(),V.z(),U.x(),U.y(),U.z());
    
    printf("  tr R=%.1f %.1f %.1f,  P=%.3f %.3f %.3f\n",R.x(),R.y(),R.z(),P.x(),P.y(),P.z());
    printf("  lam=%f  kap=%f  det=%f a=%f b=%f c=%f\n",lambda,kappa, det,a,b,c);
  }
  return 0;
}

//===========================================================
//   Function computes total likelohood for Minuit, returns : fcnval
//===========================================================
void beamLineLike3D(int &npar,double *grad,double &fcnval, double *inpPar,int iflag){
  double dmax2=util.cut_Dmax*util.cut_Dmax; // to speed up 

  
                       
  fcnval=9999; // make default large
  assert (util.track.size()>10); // will prevent on running w/o  tracks
  TVector3 V;TVector3 U;
  V.SetXYZ(inpPar[0],inpPar[1],0.);
  U.SetXYZ(inpPar[2],inpPar[3],1.); // direction, needs normalization
  U=U.Unit();
  // printf("U=%f %f %f  %f\n",U.x(),U.y(),U.z(),U.Mag2());
  assert(fabs(1.-U.Mag2())<1.e-6);
  if(util.fcnMon1) {
    char txt[1000];
    sprintf(txt,"FCN: 3D dca, beamLine X=%.2f Y=%.2f",V.x(),V.y());
    printf("monitor %s\n",txt);
    util.hA[21]->SetTitle(txt);
  }

  double chi2 = 0;
  int nBadDet=0;
  
  vector<TrackStump>::iterator it;
  for (it=util.track.begin() ; it < util.track.end(); it++) {
    TrackStump *t= &(*it);  
    if (t->bad) continue;
    double lambda=0, kappa=0;
    TVector3 r(t->r),p(t->p);
    if( twoLineDca3D(lambda,kappa,V,U,r,p)) {
      nBadDet++;  continue;
    }
    if(util.fcnMon1&0x2) {
      TVector3 r1=r+lambda*p; // track
      TVector3 r2=V+kappa*U; // vertex
      TVector3 D=r1-r2;
      printf("  dca T=%.1f %.1f %.1f  B=%.1f %.1f %.1f  dca=%.1f\n",r1.x(),r1.y(),r1.z(),r2.x(),r2.y(),r2.z(),D.Mag());
    }

    TVector3 D=((r-V)+(lambda*p-kappa*U));
    if(util.fcnMon1) {
      util.hA[21]->Fill(D.Mag());
      TVector3 r1=r+lambda*p;
      util.hA[22]->Fill(r1.z(),D.Mag());
      if (D.Mag()<dmax2) {
	util.hA[23]->Fill(r.z());
	util.hA[0]->Fill(10);
      }
    }
    double dT2=D.Perp2(); // compute distance only once
    double dZ2=D.z()*D.z();

      if (dT2<dmax2)      chi2+=dT2/t->ery2;
      else      chi2+=dmax2/t->ery2;
      if (dZ2<dmax2)      chi2+=dZ2/t->erz2;
      else      chi2+=dmax2/t->erz2;
  }
  //printf(" nBadDet=%d\n",nBadDet);
  fcnval = chi2;
  util.fcnMon1=0; // clear this flag so only one set is monitored
  if(util.fcnCount<0) return; // skip if in scanning mode
  if(util.fcnCount%10==0)printf("nCall=%d fcn=%.1f position:V=%.2f,%.2f tilt(Z=100cm):U=%.2f,%.2f\n",util.fcnCount,chi2,V.x(),V.y(),U.x()*100.,U.y()*100.);
  util.fcnCount++;
}
