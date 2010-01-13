// $Id: StFtpcLaserCalib.cc,v 1.11 2010/01/13 15:43:33 jcs Exp $
//
// $Log: StFtpcLaserCalib.cc,v $
// Revision 1.11  2010/01/13 15:43:33  jcs
// improved  LOG_DEBUG output
//
// Revision 1.10  2009/12/09 14:41:30  jcs
// delta_t0 and delta_gas can now both = 0
// new space point calculation always necessary since reconstruction done with data t0
//
// Revision 1.9  2009/10/14 15:59:55  jcs
// changes to be able to vary the gas temperature in addition to varying t0 and
// gas composition
//
// Revision 1.8  2009/10/06 14:51:45  jcs
// exit laser_fit to avoid FPE if either <=2 hits on track or if helix fit fails to converge
//
// Revision 1.7  2008/05/15 22:39:47  jcs
// re-activate helix fit
//
// Revision 1.6  2007/01/22 13:08:15  jcs
// replace cout with LOG
//
// Revision 1.5  2007/01/19 08:22:59  jcs
// replace true, false with kTRUE, kFALSE
//
// Revision 1.4  2006/06/19 12:37:58  jcs
// Use negative value of zfieldcage for FTPC east
//
// Revision 1.3  2006/04/04 10:57:05  jcs
// Fix memory leak
//
// Revision 1.2  2006/03/15 15:13:56  jcs
// add lines for listing CVS update info
//

#include "StFtpcLaserCalib.hh"
#include "StFtpcTrackMaker/StFtpcPoint.hh"
#include "StFtpcTrackMaker/StFtpcTrack.hh"
#include "StFtpcTrackMaker/StFtpcVertex.hh"
#include "TObjArray.h"

#define HELIX_FIT

const Float_t rad2grad=180/TMath::Pi();

// define functions etc. used for fit  added on 11/21/2003
static  Double_t funcxz(float x,float z,Double_t *par);
static  Double_t funcyz(float y,float z,Double_t *par);
static void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);

//

// im prinzip unschoen !!! Warning : defined but not used !!!
static int nhits; // needed by the fcn used for minuit.
static Float_t z[11],x[11],y[11];
static Float_t ex[11],ey[11];

//______________________________________________________________________________

StFtpcLaserCalib::StFtpcLaserCalib()
{
  mMinuit=new TMinuit(4);
  //mMinuit->SetPrintLevel(-1);
}

//______________________________________________________________________________

//StFtpcLaserCalib::StFtpcLaserCalib(int ftpc, int lsec, int straight, int gfit, int minz, int maxz, int minrad, int maxrad, float gt0, float ggas, StFtpcLaserTrafo *trafo, StMagUtilities *gmagf)
StFtpcLaserCalib::StFtpcLaserCalib(int ftpc, int lsec, int straight, int gfit, int minz, int maxz, int minrad, int maxrad, float gt0, float ggas, float gTemp, StFtpcLaserTrafo *trafo, StarMagField *gmagf)
{
  FTPC=ftpc;
  LSEC=lsec;
  STRAIGHT=straight;
  MINZ=minz;
  MAXZ=maxz;
  MINRAD=minrad;
  MAXRAD=maxrad;
  GAUSFIT=gfit;
  mMinuit=new TMinuit(4);
  deltat0=gt0;
  deltagas=ggas;
  deltaTemp=gTemp;

  usedfit=0;

#ifdef HELIX_FIT
  trcharge=0;
  p=pt=invp=invpt=0;
#endif

  magf=gmagf;

  //if (deltat0!=0 || deltagas!=0)
    mtrafo=trafo;
}

//______________________________________________________________________________

//static 
Double_t funcxz(float x,float z,Double_t *par)
{

  return x-(par[0]*z+par[1]);
} 

//static 
Double_t funcyz(float y,float z,Double_t *par)
{
  return y-(par[2]*z+par[3]);
} 

//______________________________________________________________________________

//static 
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
//void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{
  
  Int_t i; //calculate chisquare

  Double_t chisq = 0;
  Double_t deltaxz, deltayz;
  //LOG_DEBUG<<"in fcn vor for .."<<nhits<<" hits"<<endm;

  for (i=0;i<nhits; i++) 
    {
      // Fehler aus Daten noch intgerieren !
      // so richtig !??????
      deltaxz=funcxz(x[i],z[i],par)/ex[i];
      deltayz=funcyz(y[i],z[i],par)/ey[i];
      chisq += (deltaxz*deltaxz+deltayz*deltayz);
    }

  //LOG_DEBUG<<" f = "<<f<<endm;
  f = chisq;
}

//______________________________________________________________________________

bool StFtpcLaserCalib::cut(int i)
{
  LOG_DEBUG<<"StFtpcLaserCalib::cut i "<<i<<" z "<< z[i]<<" MINZ "<<MINZ<<" MAXZ "<<MAXZ<<" radius "<<radius[i]<<" MINRAD "<<MINRAD<<" MAXRAD "<<MAXRAD<< " laser_sector "<<hsec[i]<<endm;
  if ((z[i]>MINZ && z[i]<MAXZ) && (radius[i]>MINRAD && radius[i]<MAXRAD) && laser_sector(FTPC,LSEC,hsec[i]) )
    return kTRUE;
  else
    return kFALSE;
}

//______________________________________________________________________________

void StFtpcLaserCalib::minuit_init()
{
  //LOG_DEBUG<<"Minuit init ..."<<endm;
  //mMinuit = new TMinuit(4); //???? why is there a link error !?
  mMinuit->SetPrintLevel(-1);
  mMinuit->SetFCN(&fcn);
  //LOG_DEBUG<<"set fcn"<<endm;
  ierflg = 0;    
  arglist[0] = 1;
  
  mMinuit->mnexcm("SET ERR", arglist ,1,ierflg);
  //LOG_DEBUG<<"Minuit Error-Code : "<<ierflg<<endm;
}

//______________________________________________________________________________

void StFtpcLaserCalib::minuit_set_par()
{
  mMinuit->SetPrintLevel(-1);
  //LOG_DEBUG<<"Minuit set parameter ..."<<endm;
  //static Double_t vstart[4] = {1,1,1,1};
  Double_t vstart[4] = {1,1,1,1};
  // ggf. daten werte als Startparameter benutzen !
  //static Double_t step[4] = {0.1,0.1,0.1,0.1};
  Double_t step[4] = {0.1,0.1,0.1,0.1};
  mMinuit->mnparm(0, "mxz", vstart[0], step[0], 0,0,ierflg);
  mMinuit->mnparm(1, "bxz", vstart[1], step[1], 0,0,ierflg);
  mMinuit->mnparm(2, "myz", vstart[2], step[2], 0,0,ierflg);
  mMinuit->mnparm(3, "byz", vstart[3], step[3], 0,0,ierflg);
  //LOG_DEBUG<<"Minuit Error-Code : "<<ierflg<<endm;
}

//______________________________________________________________________________

void StFtpcLaserCalib::minuit_print()
{
  // Print results
  Double_t amin,edm,errdef;
  Int_t nvpar,nparx,icstat;
  LOG_INFO<<"StFtpcLaserCalib::minuit_print():"<<endm;
  mMinuit->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
  mMinuit->mnprin(5,amin);
  LOG_INFO<<" "<<endm;
}

//______________________________________________________________________________


void StFtpcLaserCalib::calc_res()
{
  //LOG_DEBUG<<"Calculate residuals in x,y & r,phi  ..."<<endm;
  //LOG_DEBUG<<" "<<endm;


  TF1 *fxz=new TF1("fxz","[0]*x+[1]",0,6);
  TF1 *fyz=new TF1("fyz","[0]*x+[1]",0,6); 

  // set fitted parameter to lin. function
  Double_t v,ev;
  for (int i=0;i<4;i++)
    {
      mMinuit->GetParameter(i,v,ev);
      if (i<2) fxz->SetParameter(i,v);
      else fyz->SetParameter(i-2,v);
    }
  
  // calc residuals in x and y

  hnhits->Fill(nhits);
#ifdef HELIX_FIT
  htrcharge->Fill(trcharge);
  hp->Fill(p);
  hpt->Fill(pt);
  hinvp->Fill(1/p*trcharge);
  hinvpt->Fill(1/pt*trcharge);
#endif

  for (int i=0;i<nhits;i++)
    {
      calcx[i]=fxz->Eval(z[i]);
      calcy[i]=fyz->Eval(z[i]);
      
      usedfit++;

      resx[i]=(x[i]-fxz->Eval(z[i]));
      resy[i]=(y[i]-fyz->Eval(z[i]));
      calcrad[i]=sqrt(fxz->Eval(z[i])*fxz->Eval(z[i])+fyz->Eval(z[i])*fyz->Eval(z[i]));
      if (FTPC==1)
         rpol[i]=sqrt(fxz->Eval(zfieldcage)*fxz->Eval(zfieldcage)+fyz->Eval(zfieldcage)*fyz->Eval(zfieldcage));
      if (FTPC==2)
         rpol[i]=sqrt(fxz->Eval(-zfieldcage)*fxz->Eval(-zfieldcage)+fyz->Eval(-zfieldcage)*fyz->Eval(-zfieldcage));
      calcphi[i]=zyltrafo(fxz->Eval(z[i]),fyz->Eval(z[i]),z[i]);
      resrad[i]=radius[i]-calcrad[i];
      resphi[i]=(phi[i]-calcphi[i])*Rad2Grad;

    }
  
  delete fxz;
  delete fyz;
}

//______________________________________________________________________________


int StFtpcLaserCalib::laser_fit(int getnhits)
{
 
  nhits=getnhits;

  int new_nhits=0;
  
  for (int i=0;i<nhits;i++)
    {
      if (cut(i))
	{
	  fillarray(x[i],y[i],z[i],ex[i],ey[i],new_nhits,hsec[i],ppos[i],ppossigma[i],softsec[i],softrow[i],timepos[i],padl[i],timel[i],maxadc[i],charge[i]);
	  new_nhits++;
	}
    }
  
  nhits=new_nhits;
  if (nhits <= 2 ) {
    //LOG_WARN << "StFtpcLaserCalib::laser_fit - Can't fit a laser track with " << nhits << " hits"  << endm;
    return -1;
  }

  // ***************************************************
  // * calculate new space point depending on gas & t0 *
  // ***************************************************


  //if ((deltat0!=0 || deltagas!=0) && nhits>0)
  if (nhits>0)
    {
      //LOG_DEBUG<<"calculating new space point..."<<endm;
      for (int i=0;i<nhits;i++) // debug : <=nhits
	{

	  //LOG_DEBUG<<sqrt(x[i]*x[i]+y[i]*y[i])<<endm;
	  //LOG_DEBUG<<softrow[i]<<" "<<softsec[i]<<" "<<timepos[i]<<" "<<ppos[i]<<" "<<x[i]<<" "<<y[i]<<endm;

	  if (mtrafo->padtrans(softrow[i],softsec[i],timepos[i],ppos[i],&x[i],&y[i]))
	    {
	      
	      radius[i]=sqrt(x[i]*x[i]+y[i]*y[i]);
	      //LOG_DEBUG<<"radius["<<i<<"] = "<<radius[i]<<endm;
	      phi[i]=zyltrafo(x[i],y[i],z[i]);
	    }
	  else {
	    LOG_ERROR<<"ERROR padtrans !"<<endm;
          }
	}
      //LOG_DEBUG<<"after new space point calculation !"<<endm;
    }
  
#ifdef HELIX_FIT
  // ************************************************************************
  // * include Helix fit & B-field distortion corrections (StFtpcTrack) !!!) *
  // * keine Trafo noetig da nur im Global, ggf. fuer East !???             *
  // ************************************************************************
  
  // create Laser Dummy Vertex (fuer data aus Tree auslesen !!! oder global nur);

  StFtpcVertex *laser_vertex=new StFtpcVertex(0,0,0,0,0,0);

  // create new track

  StFtpcTrack *mLaserTrack=new StFtpcTrack(1);

  TObjArray *mTrackHits=new TObjArray(0);

  // create new point and add to track
  for (int i=0;i<nhits;i++)
    {
//      StFtpcPoint *mNewPoint=new StFtpcPoint(softrow[i],softsec[i],0,0,0,0,x[i],y[i],z[i],ex[i],ey[i],0,0,0,0);
      StFtpcPoint *mNewPoint=new StFtpcPoint(softrow[i],softsec[i],0,0,0,0,0,0,0,0,x[i],y[i],z[i],ex[i],ey[i],0,0,0,0);
      mTrackHits->AddLast(mNewPoint);
      //LOG_DEBUG<<mNewPoint->GetX()<<" "<<mNewPoint->GetY()<<" "<<mNewPoint->GetZ()<<endm;
      mLaserTrack->AddPoint((StFtpcPoint*) mTrackHits->Last());
      //delete mNewPoint;
    }
  
  // DEBUG :
  //LOG_DEBUG<<mLaserTrack->GetTrackNumber()<<" "<<mLaserTrack->GetNumberOfPoints()<<endm;
  //LOG_DEBUG<<laser_vertex->GetX()<<" "<<laser_vertex->GetY()<<" "<<laser_vertex->GetZ()<<endm;
  
  // ********************************
  // * Fit laser track with Helix ! *
  // ********************************

//  mLaserTrack->Fit(laser_vertex,10.,true);
  mLaserTrack->Fit(laser_vertex,10.,false);
  if (mLaserTrack->GetP() == 0 && mLaserTrack->GetPt() == 0) {
    //LOG_WARN << "StFtpcLaserCalib::laser_fit - Fit laser track with Helix was unsuccessful" << endm;
    return -1;
  }
  
  // DEBUG :
  //LOG_DEBUG<<"p = "<<mLaserTrack->GetP()<<" | pt = "<<mLaserTrack->GetPt()<<" | charge = "<<mLaserTrack->GetCharge()<<endm;

  // get momentum and charge sign of Helix Fit !!!
  trcharge=mLaserTrack->GetCharge();
  p=mLaserTrack->GetP();
  pt=mLaserTrack->GetPt();
  //pt=1/(mLaserTrack->curvature());
  //LOG_DEBUG<<mLaserTrack->curvature()<<endm;

  // delete Hit Array and get residuals before ...
  for (int i=0;i<nhits;i++)
    {
      
      resx2[i]=((StFtpcPoint*) mTrackHits->At(i))->GetXGlobResidual();
      resy2[i]=((StFtpcPoint*) mTrackHits->At(i))->GetYGlobResidual();
      resrad2[i]=((StFtpcPoint*) mTrackHits->At(i))->GetRGlobResidual();
      resphi2[i]=((StFtpcPoint*) mTrackHits->At(i))->GetPhiGlobResidual();
      
      // DEBUG :
      //LOG_DEBUG<<" i = "<<i<<endm;
      //LOG_DEBUG<<((StFtpcPoint*) mTrackHits->At(i))->GetXS()<<" "<<((StFtpcPoint*) mTrackHits->At(i))->GetX()<<endm;
      //LOG_DEBUG<<((StFtpcPoint*) mTrackHits->At(i))->GetZS()<<" "<<((StFtpcPoint*) mTrackHits->At(i))->GetZ()<<endm; 
      // noch nicht alle Punkte richtig !!!!
      // letzter Punkt wird nicht gesetzt (kleinstes z) vgl StFtpcTrack.cc !!!
      // nehme dann standard !!!

      //LOG_DEBUG<<x[i]-((StFtpcPoint*) mTrackHits->At(i))->GetXS()<<endm;

      // create shiftet space position !!!

//      if (i<nhits-1)
//	{
//	  x_s[i]=((StFtpcPoint*) mTrackHits->At(i))->GetXS();
//	  y_s[i]=((StFtpcPoint*) mTrackHits->At(i))->GetYS();
//	}
//      else
//	{
	  x_s[i]=((StFtpcPoint*) mTrackHits->At(i))->GetX();
	  y_s[i]=((StFtpcPoint*) mTrackHits->At(i))->GetY();
//	}

      //LOG_DEBUG<<x_s[i]-x[i]<<endm;

      // calc difference as check !!!
      x_d[i]=x[i]-x_s[i];y_d[i]=y[i]-y_s[i];

      delete (StFtpcPoint*) mTrackHits->At(i);
    }

  delete mTrackHits;
  
  delete laser_vertex;
  delete mLaserTrack;

  // ***********************************
  // * Straight line Fit with Miniut ! *
  // ***********************************

 
  // if gas or t0 changes take B-field corrected space pos.
  // (check if chain default = corrected !?)

  //if ((deltat0!=0 || deltagas!=0) && nhits>0)

 
  for (int i=0;i<nhits;i++)
    {
      x[i]=x_s[i];
      y[i]=y_s[i];
    }

#endif

  minuit_init();
  minuit_set_par();
  //mMinuit->SetPrintLevel(-1);

  arglist[0] = 500;
  arglist[1] = 1;

  if (nhits>0)
    {
      //LOG_DEBUG<<"start fit"<<endm;
      mMinuit->mnexcm("MIGRAD", arglist , 2 ,ierflg);
      //LOG_DEBUG<<"Minuit Error-Code : "<<ierflg<<endm;
      //check if fit okay !
      if (ierflg==0) 
	{
	  //minuit_print();
	  calc_res();
	  
	   // Fill histograms !!!!
	  for (int i=0;i<nhits;i++)
	    Fill(i);

	}
    }
  else
    {
      LOG_ERROR<<"StFtpcLaserCalib::laser_fit - Number cluster on tracks = 0 !"<<endm;
      ierflg=-1;
    }

  //LOG_DEBUG<<"Minuit Error-Code : "<<ierflg<<endm;

  nhits=0;
  return ierflg;
}

//______________________________________________________________________________

// erstmal test fuer Spectrum !
void StFtpcLaserCalib::defl_angle_transv()
{  
  TSpectrum *specrad=new TSpectrum();
  //LOG_DEBUG<<"*******************************"<<endm;
  //LOG_DEBUG<<"Anzahl der Hits (rad) : "<<specrad->Search(hrad,3)<<endm;
  LOG_INFO<<"*******************************"<<endm;
  for (int i=0;i<specrad->Search(hrad,2.5);i++)
    {
      LOG_INFO<<"Radius "<<i<<" : "<<specrad->GetPositionX()[i]<<endm;
    }
   LOG_INFO<<"*******************************"<<endm;
  specrad->Delete();
}

//______________________________________________________________________________

void StFtpcLaserCalib::defl_histograms_st()
{
  char name[30];
  char name2[30];
  for (int i=1;i<11;i++)
    {
      sprintf(name,"rad_l%d",i);
      sprintf(name2,"pad_l%d",i);
      if (i<4)
	{
	  hpadcut[i]=new TH1F(name2,name2,200,105,125);
	  hradcut[i]=new TH1F(name,name,(int)((radcutmaxst[i]-radcutminst[i])/0.1),radcutminst[i],radcutmaxst[i]);
	}
      else
	{
	  hpadcut[i]=new TH1F();
	  hradcut[i]=new TH1F();
	}
    }
}
 
//______________________________________________________________________________

void StFtpcLaserCalib::defl_histograms()
{
  char name[30];
  char name2[30];
  for (int i=1;i<11;i++)
    {
      sprintf(name,"rad_l%d",i);
      sprintf(name2,"pad_l%d",i);

      hpadcut[i]=new TH1F(name2,name2,300,120,150);
      hradcut[i]=new TH1F(name,name,(int)((radcutmax[i]-radcutmin[i])/0.1),radcutmin[i],radcutmax[i]);
    }
}
 
//______________________________________________________________________________


void StFtpcLaserCalib::extrapol_histograms()
{
  char name[30];

  for (int i=1;i<4;i++)
    {
      sprintf(name,"radpol_l%d",i);
      hradpolcut[i]=new TH1F(name,name,(int)((radcutmaxst[i]-radcutminst[i])/0.025),radcutminst[i],radcutmaxst[i]);
    }
}

//______________________________________________________________________________

void StFtpcLaserCalib::fill_extrapol_histograms(float getradpol)
{
  for (int i=1;i<4;i++)
    {
      if (getradpol>radcutminst[i] && getradpol<radcutmaxst[i])
	{
	  ((TH1F*) hradpolcut[i])->Fill(getradpol);
	}
    }
}

//______________________________________________________________________________

void StFtpcLaserCalib::fill_defl_histograms(float getrad, float getpadpos)
{
  for (int i=1;i<11;i++)
    {
      if (getrad>radcutmin[i] && getrad<radcutmax[i])
	{
	  ((TH1F*) hradcut[i])->Fill(getrad);
	  ((TH1F*) hpadcut[i])->Fill(getpadpos);
	  
	}
    }
}

//______________________________________________________________________________


void StFtpcLaserCalib::fill_defl_histograms_st(float getrad, float getpadpos)
{
  for (int i=1;i<4;i++)
    {
      if (getrad>radcutminst[i] && getrad<radcutmaxst[i])
	{
	  ((TH1F*) hradcut[i])->Fill(getrad);
	  ((TH1F*) hpadcut[i])->Fill(getpadpos);
	  
	}
    }
}

//______________________________________________________________________________

void StFtpcLaserCalib::analyse_defl()
{
  ofstream padfile,padfile2;
  padfile.open(filename+"_padpos.log",ios::out);
  padfile2.open(filename+"_padpos2.log",ios::out);

  // use mean
  padfile<<"Padposition and radius with mean of histograms :"<<endl;
  padfile<<"------------------------------------------------"<<endl;
  padfile<<endl;
  padfile<<"radius [cm] | padposition"<<endl;
  padfile<<"-------------------------"<<endl;
  padfile<<endl;

  for (int i=1;i<11;i++)
    {
      if (((TH1F*) hradcut[i])->GetEntries()>0)
	{
	  padfile<<((TH1F*) hradcut[i])->GetMean()<<"+-"<<((TH1F*) hradcut[i])->GetRMS()<<" ";
	  padfile<<((TH1F*) hpadcut[i])->GetMean()<<"+-"<<((TH1F*) hpadcut[i])->GetRMS()<<endl;
	  padfile2<<((TH1F*) hradcut[i])->GetMean()<<" "<<((TH1F*) hradcut[i])->GetRMS()<<" ";
	  padfile2<<((TH1F*) hpadcut[i])->GetMean()<<" "<<((TH1F*) hpadcut[i])->GetRMS()<<endl;
	}
    } 

  padfile<<endl;

  padfile<<"Padposition and radius with mean of gausfit :"<<endl;
  padfile<<"------------------------------------------------"<<endl;
  padfile<<endl;
  padfile<<"radius [cm] | padposition"<<endl;
  padfile<<"-------------------------"<<endl;
  padfile<<endl;

  // use gausfit !!!
  TF1 *gausfit3=new TF1("gausfit3","gaus");

  for (int i=1;i<11;i++)
    {
      if (((TH1F*) hradcut[i])->GetEntries()>0)
	{
	  ((TH1F*) hradcut[i])->Fit("gausfit3","NQ");
	  padfile<<gausfit3->GetParameter(1)<<"+-"<<gausfit3->GetParameter(2)<<" ";
	  padfile2<<gausfit3->GetParameter(1)<<" "<<gausfit3->GetParameter(2)<<" ";
	  ((TH1F*) hpadcut[i])->Fit("gausfit3","NQ");
	  padfile<<gausfit3->GetParameter(1)<<"+-"<<gausfit3->GetParameter(2)<<endl;
	  padfile2<<gausfit3->GetParameter(1)<<" "<<gausfit3->GetParameter(2)<<endl;
	}
    }

  // make ps file !!!
  

  padfile.close();
  padfile2.close();
}

//______________________________________________________________________________

void StFtpcLaserCalib::PositionLog()
{
  
  ofstream logfile;
  logfile.open(filename+".log",ios::out);
  
  TSpectrum *spec=new TSpectrum();
  //LOG_DEBUG<<" "<<endm;
  //LOG_DEBUG<<"Gaus fit to radius distribution of the "<<spec->Search(hrad,6)<<" reconstructed straight Laser tracks..."<<endm;
  //LOG_DEBUG<<"-----------------------------------------------------------------------------"<<endm;
  for (int i=0;i<spec->Search(hrad,6);i++)
    {
      //LOG_DEBUG<<"Radius after Peakfinder : "<<(spec->GetPositionX())[i]<<endm;
      logfile<<"Radius after Peakfinder : "<<(spec->GetPositionX())[i]<<endl;
      if (GAUSFIT==1)
	{
	  float min=(spec->GetPositionX()[i]-1);
	  float max=(spec->GetPositionX()[i]+1);
	  TF1 *gausfit=new TF1("gausfit","gaus",min,max);
	  gausfit->SetParameter(1,(spec->GetPositionX())[i]);
	  // ggf. ParLimits einfuehren um richtige Fehlerbehandlung !!!
	  hrad->Fit(gausfit,"RQN");
	  //LOG_DEBUG<<"Radius after Gausfit : "<<gausfit->GetParameter(1)<<" +- "<<gausfit->GetParameter(2)<<endm;
	  logfile<<"Radius after Gausfit : "<<gausfit->GetParameter(1)<<" +- "<<gausfit->GetParameter(2)<<endl;
	}
    }
  //LOG_DEBUG<<" "<<endm;
  //LOG_DEBUG<<"Gaus fit to phi distribution of the "<<spec->Search(hphi,2)<<" reconstructed straight Laser tracks..."<<endm;
  //LOG_DEBUG<<"-----------------------------------------------------------------------------"<<endm;
  for (int i=0;i<spec->Search(hphi,6);i++)
    {
      //LOG_DEBUG<<"Phi after Peakfinder : "<<(spec->GetPositionX())[i]<<endm;
      logfile<<"Phi after Peakfinder : "<<(spec->GetPositionX())[i]<<endl;
      if (GAUSFIT==1)
	{
	  float min=(spec->GetPositionX()[i]-0.5);
	  float max=(spec->GetPositionX()[i]+0.5);
	  TF1 *gausfit2=new TF1("gausfit2","gaus",min,max);
	  gausfit2->SetParameter(1,(spec->GetPositionX())[i]);
	  hphi->Fit(gausfit2,"RQN");
	  //LOG_DEBUG<<"Phi after Gausfit : "<<gausfit2->GetParameter(1)<<" +- "<<gausfit2->GetParameter(2)<<endm;
	  logfile<<"Phi after Gausfit : "<<gausfit2->GetParameter(1)<<" +- "<<gausfit2->GetParameter(2)<<endl;
	}
    }
  logfile.close();
}

//______________________________________________________________________________

void StFtpcLaserCalib::fillarray(float tx,float ty,float tz,float tex,float tey,int n,int nsec,float gppos,float gppossigma, int gsoftsec, int gsoftrow, float gtimepos, float getpadl, float gettimel, float getmaxadc, float getcharge)
{

  radius[n]=sqrt(tx*tx+ty*ty);
  phi[n]=zyltrafo(tx,ty,tz);
  //phi[n]=atan2(ty,tx);
  x[n]=tx;
  y[n]=ty;
  z[n]=tz;
  hsec[n]=nsec;
  ex[n]=tex;
  ey[n]=tey;
  ppos[n]=gppos;
  ppossigma[n]=gppossigma;
  softsec[n]=gsoftsec;
  softrow[n]=gsoftrow;
  timepos[n]=gtimepos;
  timel[n]=gettimel;
  padl[n]=getpadl;
  charge[n]=getcharge;
  maxadc[n]=getmaxadc;

  //LOG_DEBUG<<radius[n]<<" "<<maxadc[n]<<endm;
}

//______________________________________________________________________________

void StFtpcLaserCalib::Fill(int l)
{
   hrad->Fill(radius[l]);
   hradpol->Fill(rpol[l]);
   hphi->Fill(phi[l]*rad2grad);
   hcalcrad->Fill(calcrad[l]);
   hcalcphi->Fill(calcphi[l]);
   hphiz->Fill(z[l],phi[l]);
   hradz->Fill(z[l],radius[l]);
   hresx->Fill(resx[l]);
   hresy->Fill(resy[l]);
   hresrad->Fill(resrad[l]);
   hresphi->Fill(resphi[l]);
   hresrad2->Fill(resrad[l],radius[l]);
   hresphi2->Fill(resphi[l],radius[l]);
   hresx2->Fill(resx[l],radius[l]);
   hresy2->Fill(resy[l],radius[l]);

#ifdef HELIX_FIT
   hhresx->Fill(resx2[l]);
   hhresy->Fill(resy2[l]);
   hhresrad->Fill(resrad2[l]);
   hhresphi->Fill(resphi2[l]*rad2grad);
   hhresrad2->Fill(resrad2[l],radius[l]); // radius ggf. da Helix mit schieben anders !????
   hhresphi2->Fill(resphi2[l]*rad2grad,radius[l]);
   hhresx2->Fill(resx2[l],radius[l]);
   hhresy2->Fill(resy2[l],radius[l]);
#endif

   hpad->Fill(ppos[l]);
   htime->Fill(timepos[l]);
   hpadrad->Fill(radius[l],ppos[l]);
   hpadsigma->Fill(ppossigma[l]);
   hpadl->Fill(padl[l]);
   htimel->Fill(timel[l]);
   hcharge->Fill(charge[l]);
   hmaxadc->Fill(maxadc[l]);

#ifdef HELIX_FIT
   hbdiffx->Fill(x_d[l]);
   hbdiffy->Fill(y_d[l]);
#endif

   if (STRAIGHT==0 || STRAIGHT==3)
     fill_defl_histograms(radius[l],ppos[l]);
   else if (STRAIGHT==1)
     fill_defl_histograms_st(radius[l],ppos[l]);

   fill_extrapol_histograms(rpol[l]); 
}


//______________________________________________________________________________

void StFtpcLaserCalib::MakePs()
{
  LOG_INFO<<"Create "<<filename+".ps"<<endm;

  gStyle->SetPalette(1);

  TCanvas *c1 = new TCanvas("c1","ps",200,10,700,500);
  TPostScript *fps=new TPostScript(filename+".ps",112);
  

  fps->NewPage();
  c1->Divide(2,2);
  c1->Update();
  c1->cd(1);
  hrad->DrawCopy();
  c1->cd(2);
  //hphi->DrawCopy();
  hnhits->DrawCopy();
  c1->cd(3);
  hradz->DrawCopy("box");
  c1->cd(4);
  hphi->DrawCopy();
  //hphiz->DrawCopy("box");
  c1->Update();
  fps->NewPage();
  c1->cd(1);
 
  TF1 *gfit=new TF1("gfit","gaus",-0.5,0.5);
  gfit->FixParameter(1,0.0);
  //gfit->SetParameter(1,0.0);
  //gfit->SetParLimits(1,0.0,0.0);
  gfit->SetLineColor(2);
  gfit->SetLineWidth(1);
  
  ofstream logfile;
  logfile.open(filename+"_res.log",ios::out);
  c1->cd(1);
  hresx->DrawCopy();
  hresx->Fit(gfit,"QR");
  logfile<<deltat0<<" "<<deltagas<<" "<<deltaTemp<<" "<<gfit->GetParameter(2)<<" "<<gfit->GetChisquare()<<endl;
  c1->cd(2);
  hresy->DrawCopy();
  hresy->Fit(gfit,"QR");
  logfile<<deltat0<<" "<<deltagas<<" "<<deltaTemp<<" "<<gfit->GetParameter(2)<<" "<<gfit->GetChisquare()<<endl;
  c1->cd(3);
  hresrad->DrawCopy();
  hresrad->Fit(gfit,"QR");
  logfile<<deltat0<<" "<<deltagas<<" "<<deltaTemp<<" "<<gfit->GetParameter(2)<<" "<<gfit->GetChisquare()<<endl;
  c1->cd(4);
  hresphi->DrawCopy();
  hresphi->Fit(gfit,"QR");
  logfile<<deltat0<<" "<<deltagas<<" "<<deltaTemp<<" "<<gfit->GetParameter(2)<<" "<<gfit->GetChisquare()<<endl;
  //logfile<<"Delta t0 | Delta Gas | Sigma res (x,y,r,phi)"<<endl;
  //logfile.close();
  
  c1->Update();
  fps->NewPage();
  c1->cd(1);
  hresrad2->DrawCopy("colz");
  c1->cd(2);
  hresphi2->DrawCopy("colz");
  //c1->cd(3);
  //hrad->DrawCopy();hcalcrad->DrawCopy("same");
  //c1->cd(4);
  //hphi->DrawCopy();hcalcphi->DrawCopy("same");
  c1->cd(3);
  hresx2->DrawCopy("colz");
  c1->cd(4);
  hresy2->DrawCopy("colz");
  c1->Update();
  fps->NewPage();
  c1->cd(1);
  hradpol->Draw();
  //logfile<<deltat0<<" "<<deltagas<<" "<<hradpol->GetMean()<<endl;
  //logfile.close();
  c1->cd(2);
  hpad->DrawCopy();
  c1->cd(3);
  hpadrad->DrawCopy("colz");
  c1->cd(4);
  hpadsigma->DrawCopy();
  c1->Update();
  fps->NewPage();

  for (int i=1;i<4;i++)
    {
      c1->cd(i);
      ((TH1F*) hradpolcut[i])->DrawCopy();
      logfile<<deltat0<<" "<<deltagas<<" "<<deltaTemp<<" "<<((TH1F*) hradpolcut[i])->GetMean()<<" "<<((TH1F*) hradpolcut[i])->GetRMS()<<endl;
      logfile<<deltat0<<" "<<deltagas<<" "<<deltaTemp<<" "<<(((TH1F*) hradpolcut[i])->GetXaxis())->GetBinCenter(((TH1F*) hradpolcut[i])->GetMaximumBin())<<endl;
    }
  c1->Update();

  fps->NewPage();
  c1->cd(1);hpadl->DrawCopy();
  c1->cd(2);htimel->DrawCopy();
  c1->cd(3);hmaxadc->DrawCopy();//hmaxadc->Fit("landau");
  c1->cd(4);hcharge->DrawCopy();//hcharge->Fit("landau");
  c1->Update();
    
#ifdef HELIX_FIT
  fps->NewPage();
  //c1->cd(1);htrcharge->DrawCopy();
  c1->cd(1);hinvp->DrawCopy();hinvp->Fit(gfit,"QR");
  c1->cd(2);hinvpt->DrawCopy();hinvpt->Fit(gfit,"QR");
  c1->cd(3);hbdiffx->DrawCopy();
  c1->cd(4);hbdiffy->DrawCopy();
  c1->Update();

  gfit->SetLineColor(3);

  fps->NewPage();
  c1->cd(1);
  hhresx->DrawCopy();
  hhresx->Fit(gfit,"QR");  
  c1->cd(2);
  hhresy->DrawCopy();
  hhresy->Fit(gfit,"QR");
  c1->cd(3);
  hhresrad->DrawCopy();
  hhresrad->Fit(gfit,"QR");
  c1->cd(4);
  hhresphi->DrawCopy();
  hhresphi->Fit(gfit,"QR");
  c1->Update();

  fps->NewPage();
  c1->cd(1);
  hhresrad2->DrawCopy("colz");
  c1->cd(2);
  hhresphi2->DrawCopy("colz");
  c1->cd(3);
  hhresx2->DrawCopy("colz");
  c1->cd(4);
  hhresy2->DrawCopy("colz");
  c1->Update();

  fps->NewPage();
  c1->cd(1);
  hhresx->DrawCopy();
  hresx->SetLineColor(2);
  hresx->DrawCopy("same");
  c1->cd(2);
  hhresy->DrawCopy();
  hresy->SetLineColor(2);
  hresy->DrawCopy("same");
  c1->cd(3);
  hhresrad->DrawCopy();
  hresrad->SetLineColor(2);
  hresrad->DrawCopy("same");
  c1->cd(4);
  hhresphi->DrawCopy();
  hresphi->SetLineColor(2);
  hresphi->DrawCopy("same");
  c1->Update();
#endif

  logfile.close(); 
  fps->Close();
  //c1->Delete();
}

//______________________________________________________________________________

void StFtpcLaserCalib::MakeOutput(TString eingabe,char* t0, char* gas, float gastemp)
{

stringstream dT;
dT << gastemp;
std::string dTstring = dT.str();
const char *dTemp = dTstring.c_str();
  
  eingabe +="-pos_";  
  if (FTPC==1)
    eingabe +="w_lsec_";
  else if (FTPC==2)
    eingabe +="e_lsec_";
  else if (FTPC==0)
    eingabe +="all_lsec";

  if (FTPC!=0)
    eingabe +=LSEC;

  if (STRAIGHT==1)
    eingabe+="_g";
  else if (STRAIGHT==0)
    eingabe+="_s";
  else if (STRAIGHT==2)
    eingabe+="_s2";
  else if (STRAIGHT==3)
    eingabe+="_all";

  eingabe +="_dt"; eingabe +=t0;
  eingabe +="_dg"; eingabe +=gas;
  eingabe +="_dT"; eingabe +=dTemp;

  filename=eingabe;

  LOG_INFO<<"Create file : "<<filename+".root"<<endm;
  outfile=new TFile(filename+".root","RECREATE");

  hrad=new TH1F("rad","radius (straight) laser tracks",124*8,0.5,31.5);
  hradpol=new TH1F("radpol","radius laser tracks extrapoliert fieldcage",124*8,0.5,31.5);
  //hphi=new TH1F("phi","phi (straight) laser tracks",48*8,-6,6);
  hphi=new TH1F("phi","phi laser tracks",360*4,-90,270);
  hcalcrad=new TH1F("clacrad","calc radius",124*8,0.5,31.5);
  hcalcphi=new TH1F("calcphi","calc phi",48*8,-6,6);
  hcalcrad->SetLineColor(2);hcalcphi->SetLineColor(2);

  if (FTPC==1)
    {
      hradz=new TH2F("radz","radius (straight) laser tracks",100,140,275,124*4,0.5,31.5);
      hphiz=new TH2F("phiz","phi (straight) laser tracks",100,140,275,48*2,0,4);
    }
  else if (FTPC==2)
    {
      hradz=new TH2F("radz","radius (straight) laser tracks",100,-275,-140,124*4,0.5,31.5);
      hphiz=new TH2F("phiz","phi (straight) laser tracks",100,-275,-140,48*2,0,4);
    }
  else if (FTPC==0)
    {
      hradz=new TH2F("radz","radius (straight) laser tracks",200,-275,275,124*4,0.5,31.5);
      hphiz=new TH2F("phiz","phi (straight) laser tracks",200,-275,275,48*2,0,4);
    }

  Int_t r_vs_z_bin=124/2;

  hresx=new TH1F("resx","Residual x",100,-0.5,0.5);
  hresy=new TH1F("resy","Residual y",100,-0.5,0.5);
  hresrad=new TH1F("resrad","Residual radius",100,-0.5,0.5);
  hresphi=new TH1F("resphi","Residual phi",100,-0.5,0.5);
  hresx2=new TH2F("resx2","Residual x vs.radius",100,-0.5,0.5,r_vs_z_bin,0.5,31.5);
  hresx2->SetMinimum(0);
  hresy2=new TH2F("resy2","Residual y vs. radius",100,-0.5,0.5,r_vs_z_bin,0.5,31.5);
  hresy2->SetMinimum(0);
  hresrad2=new TH2F("resrad2","Residual radius vs.radius",100,-0.5,0.5,r_vs_z_bin,0.5,31.5);
  hresrad2->SetMinimum(0);
  hresphi2=new TH2F("resphi2","Residual phi vs. radius",100,-0.5,0.5,r_vs_z_bin,0.5,31.5);
  hresphi2->SetMinimum(0);
  hpad=new TH1F("pados","Padposition",1600,0,160);
  htime=new TH1F("timepos","Timeposition",1800,0,180);
  hpadrad=new TH2F("radpados","Padposition vs. radius",r_vs_z_bin,0.5,31.5,1600,0,160);
  hpadsigma=new TH1F("padsigma","Sigma padposition",50,0,5);

  hmaxadc=new TH1F("maxadc","MaxAdc",150,0.5,150.5);
  hcharge=new TH1F("charge","Charge",300,0.5,1500.5);
  hpadl=new TH1F("padl","Padlength",15,0.5,15.5);
  htimel=new TH1F("timel","Timelength",20,0.5,20.5);

  hnhits=new TH1F("nhits","Number hits on laser track",8,3.5,11.5);

#ifdef HELIX_FIT
  htrcharge=new TH1F("trcharge","Charge of Helix Fit",3,-1.5,1.5);
  hp=new TH1F("p","p Helix Fit",100, -0.5, 0.5);
  hpt=new TH1F("pt","pt Helix Fit",100, -0.5, 0.5);
  hinvp=new TH1F("invp","1/p * charge of Helix Fit",100, -0.5, 0.5);
  hinvpt=new TH1F("invpt","1/pt * charge of Helix Fit",50, -0.5, 0.5);

  hhresx=new TH1F("hresx","Residual x Helix Fit",100,-0.5,0.5);
  hhresy=new TH1F("hrresy","Residual y Helix Fit",100,-0.5,0.5);
  hhresrad=new TH1F("hrresrad","Residual radius Helix Fit",100,-0.5,0.5);
  hhresphi=new TH1F("hresphi","Residual phi Helix Fit",100,-0.5,0.5);
  hhresrad2=new TH2F("hresrad2","Residual radius vs.radius Helix Fit",100,-0.5,0.5,r_vs_z_bin,0.5,31.5);
  hhresphi2=new TH2F("hresphi2","Residual phi vs. radius Helix Fit",100,-0.5,0.5,r_vs_z_bin,0.5,31.5);
  hhresx2=new TH2F("hresx2","Residual x vs.radius Helix Fit",100,-0.5,0.5,r_vs_z_bin,0.5,31.5);
  hhresy2=new TH2F("hresy2","Residual y vs. radius Helix Fit",100,-0.5,0.5,r_vs_z_bin,0.5,31.5);

  hbdiffx=new TH1F("bdiffx","Differenz x and B-field corr. x",100,-0.01,0.01);
  hbdiffy=new TH1F("bdiffy","Differenz y and B-field corr. y",100,-0.01,0.01);
  // if you activate the following two histograms be sure to activate the corresponding delete statement
  //hbdiffx2=new TH2F("bdiffx2","Differenz x and B-field corr. x vs. z",);
  //hbdiffy2=new TH2F("bdiffy2","Differenz y and B-field corr. y vs. z",);
#endif

  if (STRAIGHT==0)
    defl_histograms(); 
  else
    defl_histograms_st(); 

  //LOG_DEBUG<<"For extrapolhistos"<<endm;
  extrapol_histograms();
  
}


//______________________________________________________________________________

StFtpcLaserCalib::~StFtpcLaserCalib()
{
  //if (deltat0=0)
  //delete trafo;
  
  outfile->Write();
  
  delete mMinuit;

  //LOG_DEBUG<<"for normal histos"<<endm;
  
  delete hrad;delete hradpol;delete hphi; delete hcalcrad;
  delete hcalcphi; delete hradz; delete hphiz;
  delete hresx;delete hresy;delete hresphi; delete hresrad; delete hresrad2;
  delete hresphi2; delete hpad; delete hpadrad; delete hpadsigma; delete htime;
  delete hnhits; 
#ifdef HELIX_FIT
  delete hhresx;delete hhresy;delete hhresphi; delete hhresrad; delete hhresrad2; delete hhresphi2;
  delete htrcharge;
  delete hp; delete hpt; delete hinvp; delete hinvpt;
  delete hhresx2; delete hhresy2;
  delete hresx2; delete hresy2;
  delete hbdiffx; delete hbdiffy;
  //delete hbdiffx2;delete hbdiffy2;
#endif

  if (STRAIGHT==0)
    {
      for (int i=1;i<4;i++)
	{delete hpadcut[i];delete hradcut[i];}
    }
  else
    {
      for (int i=1;i<11;i++)
	{delete hpadcut[i];delete hradcut[i];}
    }
  outfile->Close();
  //LOG_DEBUG<<"StFtpcLaserCalib() deconstructed !"<<endm;
}
