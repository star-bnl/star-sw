// *-- Author : J.Balewski
// 
// \date   July 2007
// $Id: StFgtSlowSimu_response.cxx,v 1.1 2011/04/07 19:31:22 balewski Exp $

#include <TRandom3.h>
#include <TH2.h>
#include <TF1.h>
#include <TVector2.h>


#include "StFgtSlowSimuMaker.h"
#include "HexLatice.h"

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void
StFgtSlowSimuMaker::initFrankModel(TString fname){
  memset(meLossTab,0,sizeof(meLossTab));
  
  LOG_INFO<<"::initFrankModel() load:"<<fname<<  endm; 
  FILE *fd=fopen(fname,"r");
  assert(fd);

  const int mx=1000;
  char buf[mx];

  for (int i = 0; i <eLossDim ; i++) {
     char * ret=fgets(buf,mx,fd);
     assert(ret);// too short input file
     //printf("=%s=",buf);
     float cl1, cl2, cl3, cl4, cl5, cl6, cl7;
     int ret1=sscanf(buf,"%f %f %f %f %f %f  %f ",&cl1, &cl2, &cl3, &cl4, &cl5, &cl6, &cl7);
     assert(ret1==7);
     meLossTab[i] = cl4;
  }
  LOG_INFO<<"::initFrankModel() OK"<<endm;

}




//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void
StFgtSlowSimuMaker::responseFrankModel(TVector3 Rloc, TVector3 Dloc){
  static const double dPi=4*acos(0.);

  /* in order to simulate the energy loss for individual ionizing collisions a table from Bichsel is used. 
     This is given in figure 9 in NIM A562, 154 (2006)
     This table gives the energy loss for a given random number (from 0 to 1 in steps of 10^-4)
     Two files, low and high beta gamma:
     file Low: beta gamma .31623      1.00000      3.16228     10.00000     31.62278
     file High: beta gamma 100.00000    316.22777   1000.00000   3162.27766  10000.00000

     here use column 2 for High file
  */

  // parameters:
  double par_pairsPerCm = 40.; // # of primary pairs produced per cm of path (Frank: 4 pairs/mm)
  double zLocEnd=Rloc.z()+Dloc.z(); // for transverse diffusion
  double pathLength=Dloc.Mag(); // convert from cm to mm
  TVector3 rv=Dloc; rv=rv.Unit(); // unit vector along the path
  double rvT=rv.Perp(); // only for QA histos
//    printf("|v|=%.3f  x,y,z=%.3f,%.3f,%.3f, pT=%.3f\n",rv.Mag(), rv.x(),rv.y(),rv.z(),rvT);

  double totalEnergy_eV = 0;
  double path = 0; // in mm, traveled so far
  int nPrimPair = 0; //# primary pairs  for this path
  int nTotPair = 0; //# any pairs  for this path
  
  double sum1=0,sum2=0; // to calculate average charge position along the tracks
  while (1) {
    // make a random step
    double stepLength = - TMath::Log(mRnd->Uniform()) / par_pairsPerCm;
    path += stepLength;
    if (path > pathLength) break;
    nPrimPair++;
    // additional weight according to secondary energy distribution, according to Bichsel dist
    int rndBin = ((int) (10000.0 * mRnd->Uniform()));
    // Cutoff at 9992  WMZ
    if(rndBin > par_cutoffOfBichel) rndBin = par_cutoffOfBichel;
    double eL_eV = meLossTab[rndBin];
    int nAnyPair = 1 + ((int) ((eL_eV-15.4)/26.)); // # of pairs for this sub-step, includes amplification
    totalEnergy_eV += eL_eV;
    if (nAnyPair < 0)   continue; // skip electron inf absorbed
    // add this electron as hit
    TVector3 r=Rloc+ path*rv; // here we use cm as in the whole GSTAR
    // printf("Loc  %f %f %f  Rxy=%f  path=%f ns=%d\n", r.x(),r.y(),r.z(),r.Perp(),path,nAnyPair);
    nTotPair+= nAnyPair;

    //............... transverse difusion in drift gap
    if(par_transDiffusionPerPath>0.001) {
      double zDrift=zLocEnd-r.z();
/* WMZ 10/13/09
  Negative zDrift would happen in some events. The next line is added
to avoid it temporarily. Negative zDrift of hits in FGT disks (Z>0)
with a negative P_z is probably caused by "back-scattering" tracks.
Ignoring the sign of zDrift here does not seem to affect the estimation
of PERPENDICULAR diffusion to a hit. But, a better understanding of the
"back-scattering" is needed to make a permanent solution for it.
*/
      if(zDrift < 0) zDrift *= -1.0;
//      cout << "Debug: zDrift = " << zDrift << endl;
      double perpDiffRms=par_transDiffusionPerPath*sqrt(zDrift);
      double phi=mRnd->Uniform(dPi);
      double perp=mRnd->Gaus(0,perpDiffRms);
      TVector3 dR; dR.SetPtThetaPhi(perp,0.,phi);
      r+=dR; // add diffusion to current hit location
      // printf("aaa/um %.1f %.1f --> %.1f %.1f %.1f\n", zDrift*1e4,  perpDiffRms*1e4, dR.x()*1e4, dR.y()*1e4, dR.z()*1e4);
      hA[27]->Fill(zDrift*10); // histo is in mm
      hA[28]->Fill( dR.x()*1e4, dR.y()*1e4);
    }

    //...............snap to the nearest hole in hexagonal GEM lattice.......
    if(hexLat) { 
      TVector2 r2=r.XYvector(); int kU,kV;
      TVector2 r2s= hexLat->snap(r2,kU,kV); 
      r.SetX(r2s.X());    r.SetY(r2s.Y()); // replace location of this electron
    }

    addHit(r,nAnyPair); //position & amplitude of total ionisation caused by this primary pair   
    hA[20]->Fill(eL_eV);    
    sum1+=nAnyPair;
    sum2+=nAnyPair*path;    
  }
  hA[21]->Fill(nPrimPair );
  hA[22]->Fill(totalEnergy_eV/ 1000.);
  hA[23]->Fill(nTotPair );
  hA[24]->Fill(path*10 ); // convert to mm
  double meanPath=sum2/sum1;
  double meanTpath=meanPath*rvT;
  //  printf("nAnyEle weighted meanL/mm=%.3f , rvT=%4f, meanLT/mm=%3f\n",meanPath*10, rvT,meanTpath*10);
  hA[25]->Fill(meanPath*10);
  hA[26]->Fill(meanTpath*10);
} 



//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void // linear energy loss, no fluctuations
StFgtSlowSimuMaker::responseLinearModel(TVector3 r1, TVector3 Dloc){
  //r1; entrance in Local
  //  double par_dsStep=0.1;
  //double par_amplStep=3;

  TVector3 rv=Dloc; rv.Unit(); // unit vector along the path
  //  double ds=Dloc.Mag(); // length of the path
  
  // now r1,r2,rv are hit entrance, exit, direction in the local reference frame
  int ns=1;  //(int)(ds/par_dsStep+0.5);
  double ddS=0; //ds/ns; // this will be the final step for depositing energy
  double amplS=10; //par_amplStep/par_dsStep*ddS;
  // printf("ds=%f ns=%d  ddS=%f  amplS=%.1f\n",ds,ns,ddS,amplS);

  int is;
  for(is=0;is<ns;is++) {
    TVector3 rLoc=r1+ (is+0.5)*ddS*rv;
    //  printf("LAB %f %f %f  Rxy=%f \n", rLoc.x(),rLoc.y(),rLoc.z(),rLoc.Perp());
    addHit(rLoc,amplS);
  }
} 


//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void
StFgtSlowSimuMaker::addHit(TVector3 rLoc, double ampl) {
  int par_binStep=6; // in both directions

  //printf("addH %f %f %f  Rxy=%f  ampl=%f\n", rLoc.x(),rLoc.y(),rLoc.z(),rLoc.Perp(), ampl);
  float xH=fabs(rLoc.x()); // hit centroid in local ref frame
  float yH=fabs(rLoc.y());
  assert(xH>0);
  assert(yH>0);
  //digXY->Fill(xH,yH); // store just one value per hit


  TAxis *axX=digXY->GetXaxis();
  int ixH=axX->FindFixBin(xH);
  int mxX=axX->GetNbins();
  int ix1=ixH-par_binStep,ix2=ixH+par_binStep;
  if(ix1<1) ix1=1;
  if(ix1>mxX) ix1=mxX;
  //  printf("hh2x %f %d   %d %d\n",xH,ixH,ix1,ix2);
  TAxis *axY=digXY->GetYaxis();
  int iyH=axY->FindFixBin(yH);
  int mxY=axY->GetNbins();
  int iy1=iyH-par_binStep,iy2=iyH+par_binStep;
  if(iy1<1) iy1=1;
  if(iy1>mxY) iy1=mxY;
  // printf("hh2y %f %d   %d %d\n",yH,iyH,iy1,iy2);
  
  float valMax=0;
  int ix,iy;
  for(ix=ix1;ix<=ix2;ix++) {
    float x=axX->GetBinCenter(ix);
    float val_x=amplF->Eval(x-xH);
    // printf("hh3 ix=%d x=%f dx=%f, ampl_x=%f\n",ix,x,x-xH,val_x);
    for(iy=iy1;iy<=iy2;iy++) {
      float y=axY->GetBinCenter(iy);
      float val_y=amplF->Eval(y-yH);
      float val2D=ampl*val_x*val_y;
      //  printf("hh4 iy=%d y=%f dy=%f, ampl_y=%f  ampl_xy=%f\n",iy,y,y-yH,val_y,val2D);
      digXY->Fill(x,y,val2D);
      digXYAll->Fill(x,y,val2D);
      if(valMax<val2D) valMax=val2D;
    }
  }
  // printf("hh5 valMax=%f\n",valMax);

  //- ************************
  // testing simplified response reco :one entry per pair, ignore # of electrons
#if 0  
    int iPhiID, iRadID;// strip coordinates
    int iQuad=0; // assume that
    bool ok=geom->localXYtoStripID(iQuad,rLoc.x(),rLoc.y(),iRadID, iPhiID);
    if (!ok) return;
    hA[31]->Fill(iRadID);
    hA[32]->Fill(iPhiID);
#endif
}



/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// $Log: StFgtSlowSimu_response.cxx,v $
// Revision 1.1  2011/04/07 19:31:22  balewski
// start
//


 


