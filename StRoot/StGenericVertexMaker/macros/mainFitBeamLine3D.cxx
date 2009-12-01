#include <stdio.h>
#include <TMinuit.h>
#include <TFile.h>
#include <TH1.h>
#include <TRandom3.h>

#include "UtilBeamLine3D.h"

//#define FIT_XYSLOPES
// Do not use it for now, fit stops at strange minima

UtilBeamLine3D util; // global instance
int twoLineDca3D(double& lambda,double& kappa,TVector3& V, TVector3& U, TVector3& R,TVector3& P);

//===========================
int main() {
  int mode = 3; //0 = just migrad, no tilt
                //1 = migrad - position + tilt, but not both
                //2 = migrad - position + tilt together
                //3 = simplex-migrad-simplex-migrad

  char *chararray[40];
  chararray[0] = "F10383";
  chararray[1] = "F10398";
  chararray[2] = "F10399";
  chararray[3] = "F10402";
  chararray[4] = "F10403";
  chararray[5] = "F10404";
  chararray[6] = "F10407";
  chararray[7] = "F10412";
  chararray[8] = "F10415";
  chararray[9] = "F10426";
  chararray[10] = "F10434";
  chararray[11] = "F10439";
  chararray[12] = "F10448";
  chararray[13] = "F10449";
  chararray[14] = "F10450";
  chararray[15] = "F10454";
  chararray[16] = "F10455";
  chararray[17] = "F10463";
  chararray[18] = "F10464";
  chararray[19] = "F10465";
  chararray[20] = "F10471";
  chararray[21] = "F10476";
  chararray[22] = "F10478";
  chararray[23] = "F10482";
  chararray[24] = "F10486";
  chararray[25] = "F10490";
  chararray[26] = "F10494";
  chararray[27] = "F10505";
  chararray[28] = "F10507";
  chararray[29] = "F10508";
  chararray[30] = "F10517";
  chararray[31] = "F10525";
  chararray[32] = "F10526";
  chararray[33] = "F10527";
  chararray[34] = "F10528";
  chararray[35] = "F10531";
  chararray[36] = "F10532";
  chararray[37] = "F10535";
  chararray[38] = "F10536";
  
//   char *chararray[11];
//   chararray[0] ="MC-pQCD-setE1"; // asorted M-C
//   chararray[1] ="MC-pQCD-setE2"; // asorted M-C
//   chararray[2] ="MC-pQCD-setE3"; // asorted M-C
//   chararray[3] ="MC-pQCD-setE4"; // asorted M-C
//   chararray[4] ="MC-pQCD-setE5"; // asorted M-C
//   chararray[5] ="MC-pQCD-setE6"; // asorted M-C
//   chararray[6] ="MC-pQCD-setE3+4"; // asorted M-C
//   chararray[7] ="MC-pQCD-setE3+5"; // asorted M-C
//   chararray[8] ="MC-pQCD-setE3+6"; // asorted M-C
//   chararray[9] ="MC-pQCD-setE4+5"; // asorted M-C
//   chararray[10] ="MC-pQCD-setE5+6"; // asorted M-C
  
  TRandom3* rnd = new TRandom3(1997);
  for (int i = 34;i<35;++i){
    
    //define variables needed to manipulate parameters after fit
    Double_t amin,edm,errdef;
    Int_t nvpar,nparx,icstat;
    double pval[mxPar],perr[mxPar],plo[mxPar],phi[mxPar];
    TString paraName[mxPar];
    int istat;
    char *core = chararray[i];
    double dmax2=util.cut_Dmax*util.cut_Dmax; // to speed up 

    TObjArray  HList;
    util.initHisto(&HList);

    for (int j = 0;j<1;++j){ 
      pval[0] = (rnd->Rndm()-0.5)*2;
      pval[1] = (rnd->Rndm()-0.5)*2;
      pval[2] = (rnd->Rndm()-0.5)/25;
      pval[3] = (rnd->Rndm()-0.5)/25;
      //pval[2] = 0;
      //pval[3] = 0;
      
      printf("rand # starting point = %f , %f , %f , %f \n",pval[0],pval[1],pval[2],pval[3]);
      printf("main fit 3D beamline to %s start...\n",core);
      // select input file:
      TString inpFile="data/globTr_"; inpFile+=core; inpFile+=".rosi";
      util.readTracks(inpFile);
      
      util.print();
      util.qaTracks();
      
      TMinuit *gMinuit = new TMinuit(4);
      //..... pass name of the minimized function ......
      gMinuit->SetFCN(beamLineLike3D);
      double arglist[10];
      int error_flag = 0;
      
      arglist[0] = 1;
      gMinuit->mnexcm("SET ERR",arglist,1,error_flag);
      //Interprets a command and takes appropriate action
      //void mnexcm(const char* comand, Double_t* plist, Int_t llist, Int_t& ierflg)
      double f_null = 0.0;
      
      //define range, step size and name of all parameters
      gMinuit->mnparm(0, "X0 (cm)",pval[0],0.01,-2,2,error_flag);
      gMinuit->mnparm(1, "Y0 (cm)",pval[1],0.01,-2,2,error_flag);
      gMinuit->mnparm(2, "Ux",0.0,0.001,-0.1,0.1,error_flag);
      gMinuit->mnparm(3, "Uy",0.0,0.001,-0.1,0.1,error_flag);
      
      
      arglist[0] = 500.;
      arglist[1] = 1.;
      
      //pick MINUIT algorithim to use
      
      const char* com = "MIGRAD";
      const char* com1 = "SIMPLEX";
      const char* com2 = "MIGRAD";
      //const char* com = "MINOS";
      
      /* ******* 
	 minimize
	 ******* */
      //fix tilt
      gMinuit->FixParameter(2);
      gMinuit->FixParameter(3);
      
      if (mode<3)
	gMinuit->mnexcm(com,arglist,2,error_flag);
      else
	{
	  gMinuit->mnexcm(com1,arglist,2,error_flag);
	  gMinuit->mnexcm(com2,arglist,2,error_flag);
	  gMinuit->mnexcm(com1,arglist,2,error_flag);
	  gMinuit->mnexcm(com2,arglist,2,error_flag);
	}
      
      gMinuit->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
      gMinuit->mnprin(3,amin);
      //Prints the values of the parameters at the time of the call*-
      // 3    values, errors, step sizes, first derivs.
      
      util.fcnCount=-1; // disable ceratin monitorings for final plots
      
      
      for(int ip=0;ip<mxPar;ip++) {
	gMinuit->mnpout(ip,paraName[ip],pval[ip],perr[ip],plo[ip],phi[ip],istat);
	util.hA[19]->Fill(paraName[ip],pval[ip]);
	util.hA[19]->SetBinError(ip+1,perr[ip]);  
      }
      //printf("PARAValues X,Y,Ux,Uy for %s = %f , %f , %f , %f \n",core,pval[0],pval[1],pval[2],pval[3]);
      printf("PARAValues X,Y for %s = %f , %f \n",core,pval[0],pval[1]);
      if (1){
	if (mode > 0){
	  printf("\n Now fitting tilt");
	  
	  TMinuit *tMinuit = new TMinuit(4);
	  tMinuit->SetFCN(beamLineLike3D);
	  arglist[0] = 1;
	  tMinuit->mnexcm("SET ERR",arglist,1,error_flag);
	  tMinuit->mnparm(0, "X0 (cm)",pval[0],0.01,-2,2,error_flag);
	  tMinuit->mnparm(1, "Y0 (cm)",pval[1],0.01,-2,2,error_flag);
	  tMinuit->mnparm(2, "Ux",pval[2],0.001,-0.02,0.02,error_flag);
	  tMinuit->mnparm(3, "Uy",pval[3],0.001,-0.02,0.02,error_flag);
	  
	  arglist[0] = 500.;
	  arglist[1] = 1.;
	  
	  tMinuit->FixParameter(0);
	  tMinuit->FixParameter(1);
	  
	  if (mode < 3)
	    tMinuit->mnexcm(com,arglist,2,error_flag);
	  else
	    {
	      tMinuit->mnexcm(com1,arglist,2,error_flag);
	      tMinuit->mnexcm(com2,arglist,2,error_flag);
	      tMinuit->mnexcm(com1,arglist,2,error_flag);
	      tMinuit->mnexcm(com2,arglist,2,error_flag);
	    }
	  tMinuit->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
	  tMinuit->mnprin(3,amin);
	  for(int ip=0;ip<mxPar;ip++) {
	    tMinuit->mnpout(ip,paraName[ip],pval[ip],perr[ip],plo[ip],phi[ip],istat);}
	  printf("PARAValues UX,UY for %s = %f , %f \n",core,pval[2],pval[3]);
	  tMinuit->Delete();
	}
	if (mode > 1){
	  printf("\n Now fitting both position and tilt");
	  TMinuit *aMinuit = new TMinuit(4);
	  aMinuit->SetFCN(beamLineLike3D);
	  arglist[0] = 1;
	  aMinuit->mnexcm("SET ERR",arglist,1,error_flag);
	  aMinuit->mnexcm("SET ERR",arglist,1,error_flag);
	  aMinuit->mnparm(0, "X0 (cm)",pval[0],0.01,-2,2,error_flag);
	  aMinuit->mnparm(1, "Y0 (cm)",pval[1],0.01,-2,2,error_flag);
	  aMinuit->mnparm(2, "Ux",pval[2],0.001,-0.02,0.02,error_flag);
	  aMinuit->mnparm(3, "Uy",pval[3],0.001,-0.02,0.02,error_flag);
	  arglist[0] = 500.;
	  arglist[1] = 1.;
	  if (mode < 3)
	    aMinuit->mnexcm(com,arglist,2,error_flag);  
	  else
	    {
	      aMinuit->mnexcm(com1,arglist,2,error_flag);
	      aMinuit->mnexcm(com2,arglist,2,error_flag);
	      aMinuit->mnexcm(com1,arglist,2,error_flag);
	      aMinuit->mnexcm(com2,arglist,2,error_flag);
	    }
	  aMinuit->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
	  aMinuit->mnprin(3,amin);
	  for(int ip=0;ip<mxPar;ip++) {
	    aMinuit->mnpout(ip,paraName[ip],pval[ip],perr[ip],plo[ip],phi[ip],istat);}
	  printf("PARAValues X,Y,UX,UY for %s = %f , %f , %f, %f  \n",core,pval[0],pval[1],pval[2],pval[3]);
	  printf("PARAErrors X,Y,UX,UY for %s = %f , %f , %f, %f  \n",core,perr[0],perr[1],perr[2],perr[3]);
	  aMinuit->Delete();
	}
      }
      
    TVector3 V(pval[0],pval[1],0);
    TVector3 U(pval[2],pval[3],1);
    float errxy = 0;
    float errz = 0;
    int nTrackxy=0;
    int nTrackz=0;
    float chi2 = 0;
    U=U.Unit();
    if (1){
      vector<TrackStump>::iterator it;
      for (it=util.track.begin() ; it < util.track.end(); it++) {
	TrackStump *t= &(*it);  
	if (t->bad) continue;
	double lambda=0;double kappa=0;
	TVector3 r(t->r),p(t->p);
	twoLineDca3D(lambda,kappa,V,U,r,p);
	TVector3 D=((r-V)+(lambda*p-kappa*U));
	double dT2=D.Perp2(); // compute distance only once
	double dZ2=D.z()*D.z();
	if((dT2+t->ery2)<dmax2){
	  nTrackxy++;
	  errxy+=t->ery2;
	  // chi2+=dT2/t->ery2;
	  chi2+=exp(-1*D.x()*D.x()/(2*t->ery2))+exp(-1*D.y()*D.y()/(2*t->ery2));
	}
	if((dZ2+t->erz2)<dmax2){
	  errz+=t->erz2;nTrackz++;
	  // chi2+=dZ2/t->erz2;
	  chi2+=exp(-1*dZ2/(2*t->erz2));
	}
	//if((dT2+t->ery2<dmax2)&&(dZ2+t->erz2<dmax2))
	// chi2+=exp(-1*D.Mag()*D.Mag()/(2*t->ery2+2*t->erz2));
      }
      
    }
    printf("Uncertainty for %s = %i , %f ,%i , %f chi^2 %f  \n",core,nTrackxy,errxy,nTrackz,errz,chi2);
    gMinuit->Delete();
    }
    util.hA[19]->GetXaxis()->SetTitle(core);
    util.hA[19]->GetYaxis()->SetTitle("cm");
    
    printf("\n    ***done!***, scan Chi2 around solution\n");    
    util.evalSolution(pval);
    util.scanChi2(pval,0); //0=Vx-Vy
#ifdef FIT_XYSLOPES
    util.scanChi2(pval,1); //1=Vx-Ux
#endif  
    // Save histograms
    HList.ls();
    // return 0;
    TString outName="out/3D_beam_"; outName+=core;outName+=".hist.root";
    TFile f( outName,"recreate");
    assert(f.IsOpen());
    printf("%d histos are written  to '%s' ...\n",HList.GetEntries(),outName.Data());
    HList.Write();
    f.Close();
    //   gMinuit->Delete();

  }
}


