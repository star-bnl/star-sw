// StFtpcClusterDebug

//#include <iostream.h>
#include <Stiostream.h>
#include <stdlib.h>
#include "StMessMgr.h"
#include "StFtpcClusterDebug.hh"
#include "math_constants.h"
#include <math.h>
#include "TH1.h"
#include "TH2.h"
#include "TBranch.h"

#include "StFtpcTrackMaker/StFtpcConfMapper.hh"

namespace {
struct RUN
{
  Int_t run;
  Int_t date;
  Int_t time;
  Float_t micropertimebin;
  Float_t normalizedNowPressure, standardPressure;
  Float_t baseTemperature, gasTemperatureWest, gasTemperatureEast;
  Float_t deltapW,deltapE;
} Run;

struct HIT 
{
  Float_t x,y,z;
  Float_t rad,phi;
  Float_t raderror,phierror;
} hit;

struct CLUSTER
{
  Float_t timepos,padpos,timesigma,padsigma;
  Float_t peakheight, charge;
  Int_t timebin,pad;
  Int_t padlength,timelength;
  Int_t row,sec;
  Int_t flag;
  Int_t numpeaks;
} cluster;

struct EVENT
{
  Int_t run;
  Int_t nevent;
}event;

struct TEVENT
{
  Int_t run;
  Int_t nevent;
}tevent;


struct TCLUSTER
{
  Int_t trow,tsec, tpadlength, ttimelength;
  Float_t tpeakheight, tcharge;
  Int_t ntracks;
  Float_t padpos, timepos;
  Float_t padpossigma, timepossigma;
} tcluster;

struct THIT
{
  Float_t tx,ty,tz;
  Float_t ex,ey,ez;
  Float_t globResX,globResY,globResPhi,globResR;
  Float_t primResX,primResY,primResPhi,primResR;
}thit;

struct TREVENT
{
  Int_t run;
  Int_t nevent;
}trevent;

struct TRACK
{
  Float_t px,py,pz;
  Float_t eta,p,pt;
  Int_t npoints;
  Int_t charge;
  Int_t type;
  Int_t sec;
  //Double_t chi2;
}track;

struct MVERTEX
{
  Float_t x,y,z;
}mvertex;

struct RAW
{
  Int_t sec,row,time,pad;
  Float_t adc;
}raw;

// so nur vor Klasse !? genauer !!!
}

StFtpcClusterDebug::StFtpcClusterDebug() 
  : histofile(0), hardsecold(-1), hardrowold(-1) 
  , hardsecold2(-1), hardrowold2(-1)
  , run(-1956), nevent(0), neventold(0)
  , clusterhisto(new TH2F()), clusterhisto2(new TH2F())
  , vertex_east(0),vertex_west(0),vertex_both(0)
  , fileopen(false),dir(false),dir2(false)
  , bRun(0),bhit(0), bevent(0), bcluster(0)
  , bthit(0), btcluster(0), btevent(0)
  , btrevent(0), btrack(0), btrvertex(0)
  , bclusterraw(0)

  , histdir(0), histdir2(0), vertexdir(0), topdir(0)
  , drtree (0), dtree(0), dttree(0), dtrtree(0), dtreeraw(0)
  , drawclhisto(0), drawvertexhisto(0)
{
  // default constructor
  //   LOG_INFO << "StFtpcClusterDebug constructed" << endm; 
}

StFtpcClusterDebug::StFtpcClusterDebug(int grun, int gevent)
  : histofile(0)
  , hardsecold(-1), hardrowold(-1) 
  , hardsecold2(-1), hardrowold2(-1)
  , run(grun), nevent(gevent), neventold(0)
  , clusterhisto(new TH2F()), clusterhisto2(new TH2F())
  , vertex_east(new TH1F()),vertex_west(new TH1F()),vertex_both(new TH1F())
      
  , fileopen(false),dir(false),dir2(false)
      
  , bRun(0),bhit(0), bevent(0), bcluster(0)
  , bthit(0), btcluster(0), btevent(0)
  , btrevent(0), btrack(0), btrvertex(0)
  , bclusterraw(0)

  , histdir(0), histdir2(0), vertexdir(0), topdir(0)
  , drtree (0), dtree(0), dttree(0), dtrtree(0), dtreeraw(0)
  , drawclhisto(0), drawvertexhisto(0)
{

  // initialize filename and open file
  TFile *test=0;
  // Initialize debug root file name
  std::string fname="fdbg";
  // Read debug.ini file
  ifstream ini;
  ini.open("./debug.ini",ios::in);
  if (ini.good() ) {
     ini>>fname>>drawclhisto>>drawvertexhisto;
  }
  else {
     LOG_WARN << "debug.ini file missing - used default filename, drawclhisto = " << drawclhisto << " drawvertexhist0 = " << drawvertexhisto <<endm;
  }
  //LOG_INFO << "StFtpcClusterDebug constructed" << endm;
  stringstream histodateis;
  histodateis <<"run_" <<run<<"_" << fname <<".root";
  std::string  histodateistring = histodateis.str();
  const char *histodatei = histodateistring.c_str();

  test=new TFile(histodatei);

  if (!(test->IsOpen()))
    {
      delete test; test = 0;
      fileopen=false;
      LOG_INFO << "histodatei = " << histodatei << endm;
      histofile=new TFile(histodatei,"RECREATE","FTPC Cluster Finder histograms");

      drtree=new TTree("rinfo","Run calibration information");
        drtree->Branch("Run",&Run,"run/I:date/I:time/I:micropertimebin/F:normalizedNowPressure/F:standardPressure/F:baseTemperature/F:gasTemperatureWest/F:gasTemperatureEast/F:deltapW/F:deltapE/F");

      dtree=new TTree("cl","Cluster calibration informations");
        dtree->Branch("hit",&hit,"x/F:y/F:z/F:rad/F:phi/F:raderror/F:phierror/F");
        dtree->Branch("cluster",&cluster,"timepos/F:padpos/F:timesigma/F:padsigma/F:peakheight/F:charge/F:timebin/I:pad/I:padlength/I:timelength/I:row/I:sec/I:flag/I:numpeaks/I");
        dtree->Branch("event",&event,"run/I:nevent/I");

      dttree=new TTree("clot","Cluster on tracks calibration information");
        dttree->Branch("cluster",&tcluster,"row/I:sec/I:padlength/I:timelength/I:peakheight/F:charge/F:ntracks/I:padpos/F:timepos/F:padsigma/F:timesigma/F");
        dttree->Branch("hit",&thit,"x/F:y/F:z/F:ex/F:ey/F:ez/F:globResX/F:globResY/F:globResR/F:globResPhi/F:primResX/F:primResY/F:primResR/F:primResPhi/F");
        dttree->Branch("event",&tevent,"run/I:nevent/I");

      dtrtree=new TTree("tr","Track calibration information");
        dtrtree->Branch("event",&trevent,"run/I:nevent/I");
        dtrtree->Branch("track",&track,"px/F:py/F:pz/F:eta/F:p/F:pt/F:npoints/I:charge/I:type/I:sec/I");//:chi2/D");
        dtrtree->Branch("vertex",&mvertex,"x/F:y/F:z/F");

      dtreeraw=new TTree("raw","Cluster raw data");
        dtreeraw->Branch("cl_raw",&raw,"sec/I:row/I:time/I:pad/I:adc/F");

      topdir=histofile->mkdir("histograms");
    }
  else
    {
      delete test; test = 0;
      fileopen=true;
      histofile=new TFile(histodatei,"UPDATE","histogramme fuer fcl");
      bRun = 0;
      drtree=(TTree*) histofile->Get("rinfo");
      if (drtree) {
         bRun=drtree->GetBranch("Run");
           bRun->SetAddress(&Run);
         dtree=(TTree*) histofile->Get("cl");
           bhit=dtree->GetBranch("hit");
           bhit->SetAddress(&hit);

           bcluster=dtree->GetBranch("cluster");
           bcluster->SetAddress(&cluster);
         
           bevent=dtree->GetBranch("event");
           bevent->SetAddress(&event);

         dttree=(TTree*) histofile->Get("clot");
           btcluster=dttree->GetBranch("cluster");
           btcluster->SetAddress(&tcluster);
         
           bthit=dttree->GetBranch("hit");
           bthit->SetAddress(&thit);
           
           btevent=dttree->GetBranch("event");
           btevent->SetAddress(&tevent);

         dtrtree=(TTree*) histofile->Get("tr");
           btrevent=dtrtree->GetBranch("event");
           btrevent->SetAddress(&trevent);
           
           btrack=dtrtree->GetBranch("track");
           btrack->SetAddress(&track);
           
           btrvertex=dtrtree->GetBranch("vertex");
           btrvertex->SetAddress(&mvertex);

         dtreeraw=(TTree*) histofile->Get("raw");
           bclusterraw=dtreeraw->GetBranch("cl_raw");
           bclusterraw->SetAddress(&raw);

         histofile->Delete("rinfo;1");
         histofile->Delete("cl;1");
         histofile->Delete("clot;1");
         histofile->Delete("tr;1");
         histofile->Delete("raw;1");

         topdir=(TDirectory*) histofile->Get("histograms");
      } else {
         LOG_ERROR << "StFtpcClusterDebug::StFtpcClusterDebug():" 
                   << " No \"rinfo\" object was found on <" 
                   << histodatei << "> ROOT file"  << endm;
      }
    }

  delete test;
}

StFtpcClusterDebug::~StFtpcClusterDebug()
{
  //LOG_INFO << "StFtpcClusterDebug deconstructed" << endm;
  histofile->Write();
#if 0  
  delete clusterhisto;
  delete vertex_west; delete vertex_east; delete vertex_both;
#endif  
  //if (fileopen)
  //{
  //delete bhit; delete bcluster; delete bevent; 
  //delete btcluster; delete bthit; delete btevent;
  //delete btrevent; delete btrack; delete btrvertex;
  //  }
#if 0  
  delete dtree; delete dttree; delete dtrtree; // bei sim hier seg. violat.(auch ab > 2300003) !???
#endif  
  histofile->Close();
  delete histofile; // ???? (auch ab > 2300003)
}

void StFtpcClusterDebug::backup()
{
  // dummy fuer backup des Treefiles nach 10 events !
}

void StFtpcClusterDebug::drawgainhisto(int hardsec, int hardrow,int iPad, float gainfac,TPCSequence HSequence)
{
  // called from StFtpcClusterMaker/StFtpcClusterFinder.cc if drawclhist = 1 in debug.ini file
  // define histogramname only if new sector or row
  if (!dir2)
    {
      char dirname[25];
      sprintf(dirname,"evt_%d_gain",nevent);
      histdir2=topdir->mkdir(dirname);
      dir2=true;
    }

  if (hardsecold2!=hardsec || hardrowold2!=hardrow)
    {
      char histotitel[100],histoname[25];
      //histotitel +="(hardsec ";
      //histotitel +=hardsec;
      //histotitel +=" hardrow ";
      //histotitel +=hardrow;
      //histotitel +=") & gainfactor";
      sprintf(histotitel,"(hardsec %d hardrow %d) & gainfactor",hardsec,hardrow);
      sprintf(histoname,"hsec%d_hrow%d_gain",hardsec,hardrow);
      topdir->cd();
      histdir2->cd();
      clusterhisto2=new TH2F(histoname,histotitel,160,0,160,255,0,255);
    }
  hardsecold2=hardsec; hardrowold2=hardrow;

  //fill gain-data pad vs time histograms for each sector and row

  //LOG_INFO << "Using gainfac = " << gainfac << endm;

  for(int iIndex=0; iIndex<HSequence.Length; iIndex++)
    {
      if (((float)(unsigned int)(HSequence.FirstAdc[iIndex]))>0)
	{
	  clusterhisto2->Fill(iPad,HSequence.startTimeBin+iIndex-1,(((float)(unsigned int)(HSequence.FirstAdc[iIndex]))*gainfac));
	  //clusterhisto2->Fill(iPad,iPad);
	  // check fuer zero supressed !
	  //if (((float)(unsigned int)(HSequence.FirstAdc[iIndex]))<4) LOG_INFO<<((float)(unsigned int)(HSequence.FirstAdc[iIndex]))<<endm;
	  //clusterhisto->Fill(iPad,HSequence.startTimeBin+iIndex-1); //ohne ADC fuer contour-plot !!!
	}
    }
}

void StFtpcClusterDebug::drawhisto(int hardsec, int hardrow, int iPad, TPCSequence HSequence)
{
  // called from StFtpcClusterMaker/StFtpcClusterFinder.cc if drawclhist = 1 in debug.ini file
  // define histogramname only if new sector or row
  if (!dir)
    {
      char dirname[25];
      sprintf(dirname,"evt_%d",nevent);
      histdir=topdir->mkdir(dirname);
      dir=true;
    }

  if (hardsecold!=hardsec || hardrowold!=hardrow)
    {
      char histotitel[100],histoname[25];
      //histotitel +="(hardsec ";
      //histotitel +=hardsec;
      //histotitel +=" hardrow ";
      //histotitel +=hardrow;
      //histotitel +=")";
      sprintf(histotitel,"(hardsec %d hardrow %d)",hardsec,hardrow);
      sprintf(histoname,"hsec%d_hrow%d",hardsec,hardrow);
      topdir->cd();
      histdir->cd();
      clusterhisto=new TH2F(histoname,histotitel,160,0,160,255,0,255);
    }
  hardsecold=hardsec; hardrowold=hardrow;

  //fill raw-data pad vs time histograms for each sector and row

  for(int iIndex=0; iIndex<HSequence.Length; iIndex++)
    {
      if (((float)(unsigned int)(HSequence.FirstAdc[iIndex]))>0)
	{
	  clusterhisto->Fill(iPad,HSequence.startTimeBin+iIndex-1,((float)(unsigned int)(HSequence.FirstAdc[iIndex])));
	  // check fuer zero supressed !
	  //if (((float)(unsigned int)(HSequence.FirstAdc[iIndex]))<4) LOG_INFO<<((float)(unsigned int)(HSequence.FirstAdc[iIndex]))<<endm;
	  //clusterhisto->Fill(iPad,HSequence.startTimeBin+iIndex-1); //ohne ADC fuer contour-plot !!!
	}
    }
}

void StFtpcClusterDebug::fillRun(Int_t grun, Int_t gdate, Int_t gtime, Float_t micropertimebin, Float_t normalizedNowPressure, Float_t standardPressure, Float_t baseTemperature, Float_t gasTemperatureWest, Float_t gasTemperatureEast, Float_t deltapW, Float_t deltapE)
{
   Run.run = grun;
   Run.date = gdate;
   Run.time = gtime;
   Run.micropertimebin = micropertimebin;
   Run.normalizedNowPressure = normalizedNowPressure;
   Run.standardPressure = standardPressure;
   Run.baseTemperature = baseTemperature;
   Run.gasTemperatureWest = gasTemperatureWest;
   Run.gasTemperatureEast = gasTemperatureEast;
   Run.deltapW = deltapW;
   Run.deltapE = deltapE;
   drtree->Fill();
}

void StFtpcClusterDebug::fillraw(int hardsec, int hardrow, int iPad, TPCSequence HSequence)
{
  for(int iIndex=0; iIndex<HSequence.Length; iIndex++)
    {
      if (((float)(unsigned int)(HSequence.FirstAdc[iIndex]))>0)
	{
	  if (HSequence.startTimeBin+iIndex-1>10 && HSequence.startTimeBin+iIndex-1<200)
	    {
	      raw.sec=hardsec;
	      raw.row=hardrow;
	      raw.pad=iPad;
	      raw.time=HSequence.startTimeBin+iIndex-1;
	      raw.adc=((float)(unsigned int)(HSequence.FirstAdc[iIndex]));
	      dtreeraw->Fill();
	    }
	  //clusterhisto->Fill(iPad,HSequence.startTimeBin+iIndex-1,((float)(unsigned int)(HSequence.FirstAdc[iIndex])));
	  // check fuer zero supressed !
	  //if (((float)(unsigned int)(HSequence.FirstAdc[iIndex]))<4) LOG_INFO<<((float)(unsigned int)(HSequence.FirstAdc[iIndex]))<<endm;
	  //clusterhisto->Fill(iPad,HSequence.startTimeBin+iIndex-1); //ohne ADC fuer contour-plot !!!
	}
    }
}

void StFtpcClusterDebug::drawvertex(TH1F *veast,TH1F *vwest, TH1F *v)
{
  // called from StFtpcTrackMaker/StFtpcTrackMaker.cxx if drawvertexhist = 1 in debug.ini file
  // (currently commented out - may be obsolete)
  char dirname[10];
  sprintf(dirname,"vtx_%d",nevent);
  vertexdir=topdir->mkdir(dirname);
  topdir->cd();
  vertexdir->cd();
  vertex_west=(TH1F*) vwest->Clone();
  vertex_east=(TH1F*) veast->Clone();
  vertex_both=(TH1F*) v->Clone();
  // ggf. einfacher !? noch aendern (noch alles doppelt)!!!
  // vertexdir->Delete("*;2"); Damit Eintraege geloescht !!!
}

void StFtpcClusterDebug::fillclustertree(TPeak *Peak,TClusterUC *cl,Float_t charge, Int_t hsec, Int_t hrow, Float_t raderror, Float_t phierror, Int_t flag, float getpressure, int getnumpeaks)
{
  hit.x=Peak->x;
  hit.y=Peak->y;
  hit.z=Peak->z;
  hit.rad=Peak->Rad;
  hit.phi=Peak->Phi;
  hit.raderror=raderror;
  hit.phierror=phierror;
  cluster.timepos=Peak->TimePosition;
  cluster.padpos=Peak->PadPosition;
  if (Peak->TimeSigma<=10 && Peak->TimeSigma>=-10)
    cluster.timesigma=Peak->TimeSigma;
  if (Peak->PadSigma<=10 && Peak->PadSigma>=-10 )
    cluster.padsigma=Peak->PadSigma;
  cluster.peakheight=Peak->PeakHeight;
  cluster.charge=charge;
  cluster.timebin=Peak->Timebin;
  cluster.pad=Peak->pad;
  cluster.padlength=(cl->EndPad-cl->StartPad)+1;
  cluster.timelength=Peak->Sequence.Length;
  cluster.sec=hsec;
  cluster.row=hrow;
  cluster.flag=flag;
  cluster.numpeaks=getnumpeaks;
  //cluster.timelength=cl->NumSequences;
  event.run=run;
  event.nevent=nevent;
  dtree->Fill();
}

void StFtpcClusterDebug::fillclustertree(TPeak Peak,TClusterUC *cl,Float_t charge, Int_t hsec, Int_t hrow, Float_t raderror, Float_t phierror,Int_t flag, float getpressure, int getnumpeaks)
{
  hit.x=Peak.x;
  hit.y=Peak.y;
  hit.z=Peak.z;
  hit.rad=Peak.Rad;
  hit.phi=Peak.Phi;
  hit.raderror=raderror;
  hit.phierror=phierror;
  cluster.timepos=Peak.TimePosition;
  cluster.padpos=Peak.PadPosition;
  cluster.timesigma=Peak.TimeSigma;
  cluster.padsigma=Peak.PadSigma;
  cluster.peakheight=Peak.PeakHeight;
  cluster.charge=charge;
  cluster.timebin=Peak.Timebin;
  cluster.pad=Peak.pad;
  cluster.padlength=(cl->EndPad-cl->StartPad)+1;
  cluster.timelength=Peak.Sequence.Length;
  cluster.sec=hsec;
  cluster.row=hrow;
  cluster.flag=flag;
  cluster.numpeaks=getnumpeaks;
  //cluster.timelength=cl->NumSequences;
  event.run=run;
  event.nevent=nevent;
  dtree->Fill();
}

void StFtpcClusterDebug::clusteranalyse()
{
  // dummy fuer weitere analyse plots !!!
}

void StFtpcClusterDebug::fillgeanttree()
{
  // dummy fuer geant simulationen
}

void StFtpcClusterDebug::filltracktree(TObjArray *foundtracks, Double_t vertexpos[3])
{
  
  for (Int_t t_counter = 0; t_counter < foundtracks->GetEntriesFast(); t_counter++) 
    {
      StFtpcTrack *tracks = (StFtpcTrack*) foundtracks->At(t_counter);
      TObjArray   *fhits  = (TObjArray*) tracks->GetHits();

      StFtpcPoint *po1 = (StFtpcPoint *)(fhits->First());
      StFtpcPoint *po2 = (StFtpcPoint *)(fhits->Last());
      Double_t r1 = TMath::Sqrt(po1->GetX()*po1->GetX() + po1->GetY()*po1->GetY()), 
	r2 = TMath::Sqrt(po2->GetX()*po2->GetX() + po2->GetY()*po2->GetY());
      Double_t r = TMath::Abs(r1-r2);

      track.px=tracks->GetPx();
      track.py=tracks->GetPy();
      track.pz=tracks->GetPz();
      track.eta=tracks->GetEta();
      track.p=tracks->GetP();
      track.pt=tracks->GetPt();
      track.npoints=tracks->GetNumberOfPoints();
      track.charge=tracks->GetCharge();
      if (r > 5. && r2>r1)
	track.type=2;
      else if (r > 5. && r2<r1)
	track.type=1;
      else
	track.type=0;

      track.sec=po1->GetSector();

      trevent.run=run;
      trevent.nevent=nevent;
      //track.chi2=tracks->GetChiSq();
      mvertex.x=vertexpos[0];
      mvertex.y=vertexpos[1];
      mvertex.z=vertexpos[2];
      dtrtree->Fill();

      for (Int_t h_counter = 0; h_counter < fhits->GetEntriesFast(); h_counter++) 
	{
	  StFtpcPoint *mhit = (StFtpcPoint *) fhits->At(h_counter);
	
	  tcluster.trow=mhit->GetPadRow();
	  tcluster.tsec=mhit->GetSector();
	  tcluster.tpadlength=mhit->GetNumberPads();
	  tcluster.ttimelength=mhit->GetNumberBins();
	  tcluster.tpeakheight=mhit->GetMaxADC();

	  tcluster.padpos=mhit->GetPadPos();
	  tcluster.timepos=mhit->GetTimePos();
	  tcluster.padpossigma=mhit->GetPadPosSigma();
	  tcluster.timepossigma=mhit->GetTimePosSigma();	  

	  thit.globResX=mhit->GetXGlobResidual();
	  thit.globResY=mhit->GetYGlobResidual();
	  thit.globResR=mhit->GetRGlobResidual();
	  thit.globResPhi=mhit->GetPhiGlobResidual();

	  thit.primResX=mhit->GetXPrimResidual();
	  thit.primResY=mhit->GetYPrimResidual();
	  thit.primResR=mhit->GetRPrimResidual();
	  thit.primResPhi=mhit->GetPhiPrimResidual();

	  tcluster.tcharge=mhit->GetCharge();
	  tcluster.ntracks=t_counter;
	  thit.tx=mhit->GetX();
	  thit.ty=mhit->GetY();
	  thit.tz=mhit->GetZ();
	  thit.ex=mhit->GetXerr();
	  thit.ey=mhit->GetYerr();
	  thit.ez=mhit->GetZerr();
	  tevent.run=run;
	  tevent.nevent=nevent;

	  dttree->Fill();
	}
    }
  // warum geht nicht delete !???
  //delete foundtracks;// delete fhits;delete mhit;
}
