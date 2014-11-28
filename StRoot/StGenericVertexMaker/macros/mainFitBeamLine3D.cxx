#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <TMinuit.h>
#include <TFile.h>
#include <TH1.h>
#include <TObjArray.h>
#include <TRandom3.h>

#include "UtilBeamLine3D.h"

UtilBeamLine3D util; // global instance
int twoLineDca3D(double& lambda,double& kappa,TVector3& V, TVector3& U, TVector3& R,TVector3& P);


//pick MINUIT algorithim to use:
const char* comAlgo = "MIGRAD";
//const char* comAlgo = "SIMPLEX"; // does NOT compute errors , but is fast
//const char* com = "MIGRAD";
//const char* com = "MINOS";


//===========================
int main(int argc, char *argv[]) {
  printf("MAIN: %s(%s)  nPar=%d\n",argv[0],comAlgo, argc-1);

  if(argc<2) { printf("Abort, expected: %s  coreName [x=doChi2plot] \n",argv[0]); return 1;}
 
  TRandom3* rnd = new TRandom3(0); // use random start each time 

    
  //define variables needed to manipulate parameters after fit
  Double_t amin,edm,errdef;
  Int_t nvpar,nparx,icstat;
  double pval[mxPar],perr[mxPar],plo[mxPar],phi[mxPar];
  TString paraName[mxPar];
  int istat;
  const char *core = argv[1]; assert(strlen(core)>5);
  
  TObjArray  HList;
  util.initHisto(&HList);
  
  pval[0] = (rnd->Rndm()-0.5)*2.;
  pval[1] = (rnd->Rndm()-0.5)*2.;
  pval[2] = (rnd->Rndm()-0.5)/25.;
  pval[3] = (rnd->Rndm()-0.5)/25.;
  
  printf("rand # starting point = %f , %f , %f , %f \n",pval[0],pval[1],pval[2],pval[3]);
  printf("main fit 3D beamline to %s start...\n",core);
  // select input file:
  TString inpFile=Form("inp/globTr_%s.txt",core);
  util.readTracks(inpFile);
  
  util.print();
  int nOkTr=util.qaTracks();
#if 1
  assert(nOkTr>2000); // temporary

  TMinuit *gMinuit = new TMinuit(4);
  //..... pass name of the minimized function ......
  gMinuit->SetFCN(beamLineLike3D);
  double arglist[10];
  int error_flag = 0;
  
  arglist[0] = 1;
  gMinuit->mnexcm("SET ERR",arglist,1,error_flag);
  //Interprets a command and takes appropriate action
  //void mnexcm(const char* comand, Double_t* plist, Int_t llist, Int_t& ierflg)
  
  //define range, step size and name of all parameters
  gMinuit->mnparm(0, "X0 (cm)",pval[0],0.01,-2,2,error_flag);
  gMinuit->mnparm(1, "Y0 (cm)",pval[1],0.01,-2,2,error_flag);
  gMinuit->mnparm(2, "Ux",pval[3],0.001,-0.1,0.1,error_flag);
  gMinuit->mnparm(3, "Uy",pval[3],0.001,-0.1,0.1,error_flag);
  
      
  arglist[0] = 500.;
  arglist[1] = 1.;
  
  
  /* ******* 
     minimize
     ******* */

  gMinuit->mnexcm(comAlgo,arglist,2,error_flag);
  
  gMinuit->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
  gMinuit->mnprin(3,amin);
  
  //Prints the values of the parameters at the time of the call*-
  // 3    values, errors, step sizes, first derivs.
  
  util.fcnCount=-1; // disable ceratin monitorings for final plots
  
  for(int ip=0;ip<mxPar;ip++) {
    gMinuit->mnpout(ip,paraName[ip],pval[ip],perr[ip],plo[ip],phi[ip],istat);
    TString tt=paraName[ip];  float val=pval[ip], err=perr[ip];
    if(ip>1) { tt="30x "+tt; val*=30.;  err*=30.;}
    util.hA[19]->Fill(tt,val);
    util.hA[19]->SetBinError(ip+1,err);  
  }
  util.hA[19]->GetXaxis()->SetTitle(core);
 
  printf("\n#beamLine for %s Xcm,Ycm,Ux,Uy = %.3f  %.3f  %.5f  %.5f  nOkTr= %d\n",core,pval[0],pval[1],pval[2],pval[3], nOkTr);
  printf("#beamLineWerr  for %s Xcm,Ycm,Ux,Uy = %.3f %.2e  %.3f %.2e  %.5f %.2e  %.5f %.2e  nOkTr= %d\n",core,pval[0],perr[0],pval[1],perr[1],pval[2],perr[2],pval[3],perr[3], nOkTr );
  //  gMinuit->Delete();
    
  printf("\n    ***done!***, scan Chi2 around solution\n");    
  util.evalSolution(pval);
 
  if(argc>2) {
    util.scanChi2(pval,0); //0=x-y
    util.scanChi2(pval,1); //1=x-nx 
    util.scanChi2(pval,2); //2= y-ny
  } else
    printf("skip generation of 2D chi2 plots\n");
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


