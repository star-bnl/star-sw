/***************************************************************************
 *
 * $Id: StResidualMaker.cxx,v 1.6 2008/01/09 23:56:07 jeromel Exp $
 *
 * Author: malisa
 ***************************************************************************
 *
 * Extract hit residual distributions from primary and global tracks
 *
 ***************************************************************************
 *
 * $Log: StResidualMaker.cxx,v $
 * Revision 1.6  2008/01/09 23:56:07  jeromel
 * (ignore)
 *
 * Revision 1.5  2006/08/15 21:42:15  jeromel
 * Fix rhic -> rhic.bnl.gov
 *
 * Revision 1.4  2003/09/02 17:59:13  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2003/04/30 20:38:53  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.2  2002/03/23 22:47:04  lisa
 * macros and documentation added - small changes to code itself
 *
 * Revision 1.1.1.1  2001/03/15 00:22:23  lisa
 * Making new StTpcPool area and putting StResidualMaker into it
 *
 *
 **************************************************************************/

#include "StResidualMaker.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "TNtuple.h"
#include "TText.h"
#include "TFile.h"
#include "StMessMgr.h"
#include "SystemOfUnits.h"
#include "TCanvas.h"

#ifndef ROOT_TH1
#include "TH1.h"
#include "TF1.h"
#endif

#ifndef ROOT_TH2
#include "TH2.h"
#include "TF2.h"
#endif

#ifndef ROOT_TH3
#include "TH3.h"
#include "TF3.h"
#endif




ClassImp(StResidualMaker)


StResidualMaker::StResidualMaker(const Char_t *name) : StMaker(name)
{
  mEventCounter = 0;

  // reasonable(?) defaults (can be overwritten by Set's)
  mNbinsDrift = 10;
  mNbinsAlpha = 20;
  mNbinsLambda = 20;
  mNbinsResid = 80;
  mResidMax = 2.0; // cm

  for (unsigned int index=0; index<14; index++)
    {
      mPrimaryHistos[index] = 0;
      mGlobalHistos[index] = 0;
    }

}
//=====================================================================
StResidualMaker::~StResidualMaker() { /* noop */ }
//=====================================================================
Int_t StResidualMaker::Init()
{
  cout << "In StResidualMaker::Init()" << endl;

  // set up the 3D histograms

  mPrimaryHistos[0] = new TH3D("Primary_Xinn","Prim_Xinn",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0,mNbinsResid,-mResidMax,mResidMax);
  mPrimaryHistos[1] = new TH3D("Primary_Xout","Prim_Xout",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0,mNbinsResid,-mResidMax,mResidMax);
  mPrimaryHistos[2] = new TH3D("Primary_Zinn","Prim_Zinn",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0,mNbinsResid,-mResidMax,mResidMax);
  mPrimaryHistos[3] = new TH3D("Primary_Zout","Prim_Zout",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0,mNbinsResid,-mResidMax,mResidMax);
  mPrimaryHistos[4] = new TH2D("RMS_Xinn_Prim","RMS_Xinn_Prim",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0);
  mPrimaryHistos[5] = new TH2D("RMS_Xout_Prim","RMS_Xout_Prim",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0);
  mPrimaryHistos[6] = new TH2D("RMS_Zinn_Prim","RMS_Zinn_Prim",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0);
  mPrimaryHistos[7] = new TH2D("RMS_Zout_Prim","RMS_Zout_Prim",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0);
  mPrimaryHistos[8] = new TH1D("Primary_Chisq1","Prim_Chisq1",40,0.0,4.0);
  mPrimaryHistos[9] = new TH1D("Primary_Chisq2","Prim_Chisq2",40,0.0,4.0);
  mPrimaryHistos[10] = new TH2D("Sigma_Xinn_Prim","Sigma_Xinn_Prim",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0);
  mPrimaryHistos[11] = new TH2D("Sigma_Xout_Prim","Sigma_Xout_Prim",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0);
  mPrimaryHistos[12] = new TH2D("Sigma_Zinn_Prim","Sigma_Zinn_Prim",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0);
  mPrimaryHistos[13] = new TH2D("Sigma_Zout_Prim","Sigma_Zout_Prim",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0);


  mGlobalHistos[0] = new TH3D("Global_Xinn","Glob_Xinn",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0,mNbinsResid,-mResidMax,mResidMax);
  mGlobalHistos[1] = new TH3D("Global_Xout","Glob_Xout",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0,mNbinsResid,-mResidMax,mResidMax);
  mGlobalHistos[2] = new TH3D("Global_Zinn","Glob_Zinn",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0,mNbinsResid,-mResidMax,mResidMax);
  mGlobalHistos[3] = new TH3D("Global_Zout","Glob_Zout",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0,mNbinsResid,-mResidMax,mResidMax);
  mGlobalHistos[4] = new TH2D("RMS_Xinn_Glob","RMS_Xinn_Glob",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0);
  mGlobalHistos[5] = new TH2D("RMS_Xout_Glob","RMS_Xout_Glob",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0);
  mGlobalHistos[6] = new TH2D("RMS_Zinn_Glob","RMS_Zinn_Glob",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0);
  mGlobalHistos[7] = new TH2D("RMS_Zout_Glob","RMS_Zout_Glob",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0);
  mGlobalHistos[8] = new TH1D("Global_Chisq1","Glob_Chisq1",40,0.0,4.0);
  mGlobalHistos[9] = new TH1D("Global_Chisq2","Glob_Chisq2",40,0.0,4.0);
  mGlobalHistos[10] = new TH2D("Sigma_Xinn_Glob","Sigma_Xinn_Glob",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0);
  mGlobalHistos[11] = new TH2D("Sigma_Xout_Glob","Sigma_Xout_Glob",mNbinsDrift,0.0,200.0,mNbinsAlpha,-40.0,40.0);
  mGlobalHistos[12] = new TH2D("Sigma_Zinn_Glob","Sigma_Zinn_Glob",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0);
  mGlobalHistos[13] = new TH2D("Sigma_Zout_Glob","Sigma_Zout_Glob",mNbinsDrift,0.0,200.0,mNbinsLambda,-60.0,60.0);

  // define the PadRowPlanes...

  // padrow radii stolen from Pavel Nevski's file
  // /afs/rhic.bnl.gov/star/packages/DEV/pams/geometry/tpcegeo/tpcegeo.g
  // note numbering starts at ONE ("waste" zeroth position)
  double rPadRows[46] = {0.0, 60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 93.6, 98.8,
			   104.0,109.2,114.4,119.6,                     // done inner sector
			   127.195, 129.195, 131.195, 133.195, 135.195,
			   137.195, 139.195, 141.195, 143.195, 145.195,
			   147.195, 149.195, 151.195, 153.195, 155.195,
			   157.195, 159.195, 161.195, 163.195, 165.195,
			   167.195, 169.195, 171.195, 173.195, 175.195,
			   177.195, 179.195, 181.195, 183.195, 185.195,
			   187.195, 189.195};

  unsigned int isector2;
  for (unsigned int ipr=1; ipr < 46; ipr++){
    StThreeVectorD RowCenter(0.0,rPadRows[ipr],0.0);
    for (unsigned int isector=1; isector < 13; isector++){
      RowCenter.rotateZ(-M_PI/6.0);                // note minus sign
      mPadRowPlanes[isector][ipr] = RowCenter;
      isector2 = (isector!=12) ? 24-isector : 24;
      mPadRowPlanes[isector2][ipr] = RowCenter;
      //      cout << "StResidualMaker::Init() - sector " << isector << " padrow " << ipr << ":" << RowCenter << endl;
    }
  }

  // now set up the sector angles
  for (unsigned int isector=1; isector < 13; isector++){
    mSectorAngle[isector] = double(isector)*(M_PI/6.0);  // no negative sign
    isector2 = (isector!=12) ? 24-isector : 24;
    mSectorAngle[isector2] = double(isector)*(M_PI/6.0);  // no negative sign
  }
  cout << "Done StMaker::Init() " << endl;

  return StMaker::Init();
}
//=====================================================================
void StResidualMaker::Clear(Option_t *opt)
{
  StMaker::Clear();
}
//=====================================================================
Int_t StResidualMaker::Finish()
{
  cout << "StResidualMaker::Finish()\n";
  cout << "Processed " << mEventCounter << " events " << endl;
  cout << "Now filling 2D RMS histograms" << endl;


  makeRMShisto(mPrimaryHistos[0],mPrimaryHistos[4],mPrimaryHistos[10]);  // X-dist Inner sector
  makeRMShisto(mPrimaryHistos[1],mPrimaryHistos[5],mPrimaryHistos[11]);  // X-dist Outer sector
  makeRMShisto(mPrimaryHistos[2],mPrimaryHistos[6],mPrimaryHistos[12]);  // Z-dist Inner sector
  makeRMShisto(mPrimaryHistos[3],mPrimaryHistos[7],mPrimaryHistos[13]);  // Z-dist Outer sector

  makeRMShisto(mGlobalHistos[0],mGlobalHistos[4],mGlobalHistos[10]);  // X-dist Inner sector
  makeRMShisto(mGlobalHistos[1],mGlobalHistos[5],mGlobalHistos[11]);  // X-dist Outer sector
  makeRMShisto(mGlobalHistos[2],mGlobalHistos[6],mGlobalHistos[12]);  // Z-dist Inner sector
  makeRMShisto(mGlobalHistos[3],mGlobalHistos[7],mGlobalHistos[13]);  // Z-dist Outer sector

  return kStOK;
}
//=====================================================================
Int_t StResidualMaker::Make()
{

  mEventCounter++;

  StEvent* event;
  event = (StEvent *) GetInputDS("StEvent");
  if (!event) return kStOK;        // if no event, we're done

  // huh, all this stuff of GetInputDS is "magic" to me... see if the following works:
  //   StRun* run;
  //   run = (StRun*)GetInputDS("StRun");
  //   if (!run){
  //     cout << "Hey I cannot get the StRun object! " << endl;
  //     assert(0);
  //   }
  //   mBfield = run->magneticField();
  // above looks good but doesn't work.... just hard-wire

  //  mBfield = 0.25*tesla;
  // nope. now I found what works (21mar02)
  mBfield = event->summary()->magneticField()*tesla/10.0;


  cout << "************\n B-field is " << mBfield << "\n************\n";


  //  See if this event survives the event filter.
  //  If not we stop here right away.
  //
  if (!accept(event)) return kStOK;

  cout << "Event accepted \n";

  StTrack* track;
  StSPtrVecTrackNode& nodes = event->trackNodes();


  int nPrimaryTracks=0;
  int nPrimaryAccepted=0;

  for(unsigned int j=0; j<nodes.size(); j++){
    // Helen suggests that when making global track plots, ONLY use those globals
    // which also have primary nodes.  These are the "good" globals anyhow, and
    // also then we can attribute any differences in the residuals to the fitting,
    // and not have to worry about comparing two different track sets
    //
    track = nodes[j]->track(primary);
    if (track){
      nPrimaryTracks++;
      if (accept(track))
	{
	  fill3Dhistos(track,mPrimaryHistos);
	  nPrimaryAccepted++;
	}
      track = nodes[j]->track(global);
      if (accept(track)) fill3Dhistos(track,mGlobalHistos);
    }
  }

  cout << nPrimaryAccepted << " out of " << nPrimaryTracks << " primary tracks accepted \n";
  return kStOK;
}
//=====================================================================
void StResidualMaker::fill3Dhistos(StTrack* track, TH1** histos)
{
  /*
   * job here is to loop over all hits on the track, find the x- and z-residuals,
   * and fill the appropriate histograms.  Note that "x" means ALONG THE PADROW
   */


  TH3D* resXinn = (TH3D*)histos[0];
  TH3D* resXout = (TH3D*)histos[1];
  TH3D* resZinn = (TH3D*)histos[2];
  TH3D* resZout = (TH3D*)histos[3];
  TH1D* chi2_1  = (TH1D*)histos[8];
  TH1D* chi2_2  = (TH1D*)histos[9];

  chi2_1->Fill(track->fitTraits().chi2(0));
  chi2_2->Fill(track->fitTraits().chi2(1));

  StPtrVecHit TpcHits = track->detectorInfo()->hits(kTpcId);
  StTpcHit* tpcHit;

  float drift;
  float lambda = (track->geometry()->dipAngle())*57.29578;  // to have it in degrees
  float alpha;
  StPhysicalHelixD helix = track->geometry()->helix();
  int padrow, sector;
  StThreeVectorD xyzProjected,momentumProjected;  // position and momentum at padrow, according to helix
  StThreeVectorD delXYZ;

  for (int i=0; i<(int)TpcHits.size(); i++){
    tpcHit = (StTpcHit*)TpcHits[i];
    // Helen points out that we should only plot residuals for "used" hits
    if (!(tpcHit->usedInFit())) continue;

    StThreeVectorF xyz = tpcHit->position();
    drift = 208.0 - fabs(xyz.z());
    if ((drift < 0.0) || (drift > 208.0)){
      cout << "Problem: " << xyz << " " << drift;
      assert(0);
    }
    padrow = tpcHit->padrow();
    sector = tpcHit->sector();
    //    cout << "(" << sector << "," << padrow << ")\n";
    double pathlen = helix.pathLength(mPadRowPlanes[sector][padrow],mPadRowPlanes[sector][padrow]);
    xyzProjected = helix.at(pathlen);

    momentumProjected = helix.momentumAt(pathlen,mBfield);  // here i am
    momentumProjected.rotateZ(mSectorAngle[sector]-M_PI/2.0);
    alpha = momentumProjected.phi()*180.0/M_PI;

    //    cout << "global hit: " << xyz << "proj: " << xyzProjected << endl;


    delXYZ = xyzProjected - xyz;
    //    cout << "difference in global coords: " << delXYZ;

    delXYZ.rotateZ(mSectorAngle[sector]);

    //    cout << " in local coords: " << delXYZ << endl;


    if (padrow < 14)
      {
	resXinn->Fill(drift,alpha,delXYZ.x());
	resZinn->Fill(drift,lambda,delXYZ.z());
      }
    else
      {
	resXout->Fill(drift,alpha,delXYZ.x());
	resZout->Fill(drift,lambda,delXYZ.z());
      }
  }

}

//=====================================================================
bool StResidualMaker::accept(StEvent* event)
{
    //
    //  This is a kind of very simple event filter.
    //  We select only events with a valid event vertex,
    //  i.e. event->primaryVertex() returns a non-zero pointer.
    //



    return event->primaryVertex();
}
//=====================================================================
bool StResidualMaker::accept(StTrack* track)
{
  if ((!track) || (track->flag() < 0)) return 0;

  int nHits = track->detectorInfo()->numberOfReferencedPoints();
  float pT = track->geometry()->helix().momentum(mBfield).perp();

  //  cout << "nHits, pT: " << nHits << " " << pT << endl;
  //  cout << "Limits: " << mPt[0] << " - " << mPt[1] << ", " << mNTpcHits[0] << " - " << mNTpcHits[1] << endl;

  return ((nHits >= mNTpcHits[0]) && (nHits <= mNTpcHits[1]) &&
	  (pT >= mPt[0]) && (pT <= mPt[1]));

}
//=====================================================================
//  Here we provide more simple methods to access the histograms (projections and such)
//=====================================================================
void StResidualMaker::ShowProjections(char PrimaryOrGlobal, char InnerOrOuter, char XorZ)
{
  string Stemp;
  char Ctemp[100];
  string StempTitle;
  char CtempTitle[100];

  TH1** histos;

  switch (PrimaryOrGlobal) {
  case 'p':
  case 'P':
    histos = mPrimaryHistos;
    sprintf(Ctemp,"Primary Tracks ");
    sprintf(CtempTitle,"Primary");
    break;
  case 'g':
  case 'G':
    histos = mGlobalHistos;
    sprintf(Ctemp,"Global Tracks ");
    sprintf(CtempTitle,"Global");
    break;
  default:
    cout << "Specify global or primary with a P or a G " << endl;
    return;
    break;
  }
  Stemp=Ctemp;
  StempTitle=CtempTitle;

  unsigned int histoIndex;

  switch (InnerOrOuter){
  case 'i':
  case 'I':
    if ((XorZ == 'x')||(XorZ == 'X')){
      histoIndex = 0;
      sprintf(Ctemp,"Inner Sector X-resid\n ");
      sprintf(CtempTitle,"InnerX");
    }
    else{
      histoIndex = 2;
      sprintf(Ctemp,"Inner Sector Z-resid\n ");
      sprintf(CtempTitle,"InnerZ");
    }
    break;
  case 'o':
  case 'O':
    if ((XorZ == 'x')||(XorZ == 'X')){
      histoIndex = 1;
      sprintf(Ctemp,"Outer Sector X-resid\n ");
      sprintf(CtempTitle,"OuterX");
    }
    else{
      histoIndex = 3;
      sprintf(Ctemp,"Outer Sector Z-resid\n ");
      sprintf(CtempTitle,"OuterZ");
    }
    break;
  default:
    cout << "Specify Inner/Outer sector with I or O " << endl;
    return;
    break;
  }
  Stemp+=Ctemp;
  StempTitle+=CtempTitle;

  TH3D* theHisto = (TH3D*)histos[histoIndex];

  TH1* oneDhisto;
  TCanvas* cv = new TCanvas(StempTitle.c_str(), Stemp.c_str(),200,10,600,900);
  cv->Divide(2,3);
  cv->cd(1);
  oneDhisto = theHisto->Project3D("x");
  oneDhisto->SetTitle("DriftLength");
  oneDhisto->Draw();
  cv->cd(2);
  oneDhisto = theHisto->Project3D("y");
  oneDhisto->SetTitle("CrossingAngle");
  oneDhisto->Draw();
  cv->cd(3);
  oneDhisto = theHisto->Project3D("z");
  oneDhisto->SetTitle("Residual");
  oneDhisto->Draw();

  histoIndex += 4;
  TH2D* RMSplot = (TH2D*)histos[histoIndex];
  cv->cd(4);
  RMSplot->SetTitle("RMS vs Drift vs Angle");
  RMSplot->Draw("Lego");

  histoIndex +=6;
  TH2D* SigmaPlot = (TH2D*)histos[histoIndex];
  cv->cd(5);
  SigmaPlot->SetTitle("Gaussian Sigma vs Drift vs Angle");
  SigmaPlot->Draw("Lego");

  return;
}
//===============================================================
void StResidualMaker::ShowChiSquares()
{
  TH1D* oneDhisto;
  TCanvas* cv = new TCanvas("Chi2_values","Chi2_values",200,10,600,600);
  cv->Divide(2,2);
  cv->cd(1);
  oneDhisto = (TH1D*)mPrimaryHistos[8];
  oneDhisto->Draw();
  cv->cd(2);
  oneDhisto = (TH1D*)mPrimaryHistos[9];
  oneDhisto->Draw();

  cv->cd(3);
  oneDhisto = (TH1D*)mGlobalHistos[8];
  oneDhisto->Draw();
  cv->cd(4);
  oneDhisto = (TH1D*)mGlobalHistos[9];
  oneDhisto->Draw();
}


//===============================================================
TCanvas* StResidualMaker::fitRMShisto(TH1* rmshisto)
{
  TH2D* h2 = (TH2D*)rmshisto;

  // NB: root uses "abs" instead of "fabs"
  TF2* fitfunc = new TF2("BlumRolandi",
			 "::sqrt(abs([0])+abs([1])*x/(cos(y/57.2958))**2+abs([2])*tan(y/57.2958)**2)",
			 0.0,200.0,-50.0,50.0);
  fitfunc->SetParName(0,"Sig2Intrinsic");
  fitfunc->SetParName(1,"Sig2Diffusion");
  fitfunc->SetParName(2,"Sig2Crossing");
  fitfunc->SetParameter(0,0.005);
  fitfunc->SetParameter(1,0.0005);
  fitfunc->SetParameter(2,0.005);

  h2->Fit("BlumRolandi","NW");

  TCanvas* cv = new TCanvas("FitCanv","Fit_canv",200,10,600,600);
  cv->Divide(2,2);
  cv->cd(1);
  h2->GetXaxis()->SetTitle("Drift");
  h2->GetYaxis()->SetTitle("Crossing Angle");
  h2->GetZaxis()->SetTitle("Residual Width");
  h2->GetXaxis()->SetTitleOffset(1.5);
  h2->GetYaxis()->SetTitleOffset(1.5);
  h2->GetZaxis()->SetTitleOffset(1.2);
  h2->GetXaxis()->SetTitleColor(4);
  h2->GetYaxis()->SetTitleColor(4);
  h2->GetZaxis()->SetTitleColor(4);
  h2->Draw("Lego");
  cv->cd(2);

  // now make the a histogram with the same binning as the data, and show the fit in that...
  // (yes this is a memory leak...)
  TH2D* FitHisto = new TH2D;
  *FitHisto = *h2;  // get binning the same
  FitHisto->Eval(fitfunc);
  FitHisto->SetStats(kFALSE);
  FitHisto->GetXaxis()->SetTitle("Drift");
  FitHisto->GetYaxis()->SetTitle("Crossing Angle");
  FitHisto->GetZaxis()->SetTitle("Residual Width");
  FitHisto->GetYaxis()->SetTitleOffset(1.5);
  FitHisto->GetXaxis()->SetTitleOffset(1.5);
  FitHisto->GetZaxis()->SetTitleOffset(1.2);
  FitHisto->GetXaxis()->SetTitleColor(4);
  FitHisto->GetYaxis()->SetTitleColor(4);
  FitHisto->GetZaxis()->SetTitleColor(4);
  FitHisto->Draw("Lego");

  Stat_t maximumFitValue = FitHisto->GetBinContent(FitHisto->GetMaximumBin());


  // now draw some projections...
  cv->cd(3);
  int icolor=1;
  int idriftbin = h2->GetNbinsX();
  TH1D* YprojData = h2->ProjectionY("Dbin1",idriftbin,idriftbin);
  TH1D* YprojFit  = FitHisto->ProjectionY("Fbin1",idriftbin,idriftbin);
  YprojData->SetStats(kFALSE);
  YprojFit->SetStats(kFALSE);
  YprojData->SetMarkerColor(icolor);
  YprojData->SetMarkerStyle(29);
  YprojFit->SetLineColor(icolor);
  YprojData->SetMaximum(1.1*maximumFitValue);
  YprojData->GetXaxis()->SetTitle("Crossing Angle");
  YprojData->GetYaxis()->SetTitle("Residual Width");
  YprojData->GetYaxis()->SetTitleOffset(1.2);
  YprojData->Draw("P");
  YprojFit->Draw("SAME");
  for (idriftbin-=3; idriftbin>0; idriftbin -=3){
    icolor++;
    YprojData = h2->ProjectionY("Dbin1",idriftbin,idriftbin);
    YprojFit  = FitHisto->ProjectionY("Fbin1",idriftbin,idriftbin);
    YprojData->SetMarkerColor(icolor);
    YprojData->SetMarkerStyle(29);
    YprojFit->SetLineColor(icolor);
    YprojData->SetStats(kFALSE);
    YprojFit->SetStats(kFALSE);
    YprojData->Draw("P,SAME");
    YprojFit->Draw("SAME");
  }

  // now put some informational stuff in 4th panel
  cv->cd(4);
  gPad->Range(0.0,0.0,20.0,20.0);
  TText *txt = new TText();
  txt->SetTextFont(32);
  txt->SetTextColor(1);
  txt->SetTextSize(0.06);
  txt->SetTextAlign(12);

  char legend[40];
  char number[20];

  icolor=0;
  for (idriftbin = h2->GetNbinsX(); idriftbin>0; idriftbin-=3){
    icolor++;
    float yval = ((float)icolor)*2.0;
    txt->SetTextColor(icolor);
    strcpy(legend,"Drift = ");
    double driftDistance = h2->GetXaxis()->GetBinCenter(idriftbin);
    sprintf(number,"%f",driftDistance);
    strcat(legend,number);
    txt->DrawText(3.0,yval,legend);
  }

  txt->SetTextColor(1);
  float yval=10.0;
  for(int ipar=0; ipar<3; ipar++){
    yval += 2.0;
    strcpy(legend,fitfunc->GetParName(ipar));
    sprintf(number,"%e",fabs(fitfunc->GetParameter(ipar)));
    strcat(legend," = ");
    strcat(legend,number);
    txt->DrawText(1.0,yval,legend);
  }

  return cv;
  //  return fitfunc;

  //  delete fitfunc;  don't delete or it disappears from the canvas!!!!!  (yes it is a small memory leak)
}

//===============================================================
void StResidualMaker::makeRMShisto(TH1* threeDhisto, TH1* twoDhisto, TH1* GaussSigmaHisto)
{

  // Since ROOT doesn't have 2D profile histograms, this method takes a 3Dhisto
  // and fills a 2Dhisto with the RMS values along one direction as the "weight"

  TH3D* h3 = (TH3D*)threeDhisto;
  TH2D* h2 = (TH2D*)twoDhisto;
  TH2D* SigHist = (TH2D*)GaussSigmaHisto;

  int nDriftBins = h3->GetNbinsX();
  int nAngleBins = h3->GetNbinsY();

  TH1D* ResidDist;
  double RMS,Sigma,NotReallyAnUncertainty;

  Int_t bin;
  TF1* fit;
  for (int iD=1; iD < nDriftBins+1; iD++){
    for (int iA=1; iA < nAngleBins+1; iA++){
      ResidDist = h3->ProjectionZ("proj",iD,iD,iA,iA);
      //      if (ResidDist->GetEntries() > 20){
      if (ResidDist->GetEntries() > 40){
	RMS =  ResidDist->GetRMS();
	// now get Gaussian Sigma from Gaussian fit (for Al)
	cout << "try to fit with Gaussian"<< endl;
	ResidDist->Fit("gaus","0");
	fit = ResidDist->GetFunction("gaus");
	Sigma = fit->GetParameter(2);
	NotReallyAnUncertainty = 1.0;
      }
      else{
	RMS=0.0;
	Sigma = 0.0;
	NotReallyAnUncertainty = 10000000000000000000000.0;
      }
      // this little "two-step" below is such bullshit - why can't root handle this internally?
      bin = h2->GetBin(iD,iA);
      h2->SetBinContent(bin,RMS);
      //      h2->SetBinError(bin,NotReallyAnUncertainty);
      SigHist->SetBinContent(bin,Sigma);
      //      SigHist->SetBinError(bin,NotReallyAnUncertainty);
    }
  }


}

//===============================================================
void StResidualMaker::writeHistoFile()
{
  TFile* file = new TFile("ResidualMakerHistos.root","recreate");
  for(unsigned int index=0; index<14; index++)
    {
      mPrimaryHistos[index]->Write();
      mGlobalHistos[index]->Write();
    }
  file->Close();
}

//===============================================================
 void StResidualMaker::readHistoFile(char* FileName)
{
  // overwrite the Maker's histos with the ones from the file...


  for(unsigned int index=0; index<14; index++)
    {
      if (mPrimaryHistos[index]) delete mPrimaryHistos[index];
      if (mGlobalHistos[index])  delete mGlobalHistos[index];
    }

  TH3D* h3d;
  TH2D* h2d;
  TH1D* h1d;



  TFile* file = new TFile(FileName);

  mPrimaryHistos[0] = new TH3D;
  mPrimaryHistos[1] = new TH3D;
  mPrimaryHistos[2] = new TH3D;
  mPrimaryHistos[3] = new TH3D;
  mPrimaryHistos[4] = new TH2D;
  mPrimaryHistos[5] = new TH2D;
  mPrimaryHistos[6] = new TH2D;
  mPrimaryHistos[7] = new TH2D;
  mPrimaryHistos[8] = new TH1D;
  mPrimaryHistos[9] = new TH1D;
  mPrimaryHistos[10] = new TH2D;
  mPrimaryHistos[11] = new TH2D;
  mPrimaryHistos[12] = new TH2D;
  mPrimaryHistos[13] = new TH2D;

  mGlobalHistos[0] = new TH3D;
  mGlobalHistos[1] = new TH3D;
  mGlobalHistos[2] = new TH3D;
  mGlobalHistos[3] = new TH3D;
  mGlobalHistos[4] = new TH2D;
  mGlobalHistos[5] = new TH2D;
  mGlobalHistos[6] = new TH2D;
  mGlobalHistos[7] = new TH2D;
  mGlobalHistos[8] = new TH1D;
  mGlobalHistos[9] = new TH1D;
  mGlobalHistos[10] = new TH2D;
  mGlobalHistos[11] = new TH2D;
  mGlobalHistos[12] = new TH2D;
  mGlobalHistos[13] = new TH2D;


  h3d = (TH3D*)file->Get("Primary_Xinn;1");
  h3d->Copy(*(mPrimaryHistos[0]));
  h3d = (TH3D*)file->Get("Primary_Xout;1");
  h3d->Copy(*(mPrimaryHistos[1]));
  h3d = (TH3D*)file->Get("Primary_Zinn;1");
  h3d->Copy(*(mPrimaryHistos[2]));
  h3d = (TH3D*)file->Get("Primary_Zout;1");
  h3d->Copy(*(mPrimaryHistos[3]));
  h2d = (TH2D*)file->Get("RMS_Xinn_Prim;1");
  h2d->Copy(*(mPrimaryHistos[4]));
  h2d = (TH2D*)file->Get("RMS_Xout_Prim;1");
  h2d->Copy(*(mPrimaryHistos[5]));
  h2d = (TH2D*)file->Get("RMS_Zinn_Prim;1");
  h2d->Copy(*(mPrimaryHistos[6]));
  h2d = (TH2D*)file->Get("RMS_Zout_Prim;1");
  h2d->Copy(*(mPrimaryHistos[7]));
  h1d = (TH1D*)file->Get("Primary_Chisq1;1");
  h1d->Copy(*(mPrimaryHistos[8]));
  h1d = (TH1D*)file->Get("Primary_Chisq2;1");
  h1d->Copy(*(mPrimaryHistos[9]));
  h2d = (TH2D*)file->Get("Sigma_Xinn_Prim;1");
  h2d->Copy(*(mPrimaryHistos[10]));
  h2d = (TH2D*)file->Get("Sigma_Xout_Prim;1");
  h2d->Copy(*(mPrimaryHistos[11]));
  h2d = (TH2D*)file->Get("Sigma_Zinn_Prim;1");
  h2d->Copy(*(mPrimaryHistos[12]));
  h2d = (TH2D*)file->Get("Sigma_Zout_Prim;1");
  h2d->Copy(*(mPrimaryHistos[13]));


  h3d = (TH3D*)file->Get("Global_Xinn;1");
  h3d->Copy(*(mGlobalHistos[0]));
  h3d = (TH3D*)file->Get("Global_Xout;1");
  h3d->Copy(*(mGlobalHistos[1]));
  h3d = (TH3D*)file->Get("Global_Zinn;1");
  h3d->Copy(*(mGlobalHistos[2]));
  h3d = (TH3D*)file->Get("Global_Zout;1");
  h3d->Copy(*(mGlobalHistos[3]));
  h2d = (TH2D*)file->Get("RMS_Xinn_Glob;1");
  h2d->Copy(*(mGlobalHistos[4]));
  h2d = (TH2D*)file->Get("RMS_Xout_Glob;1");
  h2d->Copy(*(mGlobalHistos[5]));
  h2d = (TH2D*)file->Get("RMS_Zinn_Glob;1");
  h2d->Copy(*(mGlobalHistos[6]));
  h2d = (TH2D*)file->Get("RMS_Zout_Glob;1");
  h2d->Copy(*(mGlobalHistos[7]));
  h1d = (TH1D*)file->Get("Global_Chisq1;1");
  h1d->Copy(*(mGlobalHistos[8]));
  h1d = (TH1D*)file->Get("Global_Chisq2;1");
  h1d->Copy(*(mGlobalHistos[9]));
  h2d = (TH2D*)file->Get("Sigma_Xinn_Glob;1");
  h2d->Copy(*(mGlobalHistos[10]));
  h2d = (TH2D*)file->Get("Sigma_Xout_Glob;1");
  h2d->Copy(*(mGlobalHistos[11]));
  h2d = (TH2D*)file->Get("Sigma_Zinn_Glob;1");
  h2d->Copy(*(mGlobalHistos[12]));
  h2d = (TH2D*)file->Get("Sigma_Zout_Glob;1");
  h2d->Copy(*(mGlobalHistos[13]));

  for(unsigned int index=0; index<14; index++){
    mPrimaryHistos[index]->SetDirectory(0);
    mGlobalHistos[index]->SetDirectory(0);
  }







  file->Close();
}





