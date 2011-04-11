// *-- Author : J.Balewski
// 
// $Id: StFgtClustFindMaker.cxx,v 1.2 2011/04/11 19:35:38 fisyak Exp $

#include <TVector3.h>
#include <TH2.h>
#include <TF1.h>
#include <TFile.h>
#include <TLine.h>
#include <TPolyLine.h>
#include <TCrown.h>
#include <TRandom3.h>

#include "StFgtClustFindMaker.h"
#include "StFgtSlowSimuMaker.h"

ClassImp(StFgtClustFindMaker)
  

//--------------------------------------------
StFgtClustFindMaker::StFgtClustFindMaker(const char *name):StMaker(name){
  /// Class Constructor.  
  setHList(0);
  memset(hA,0,sizeof(hA));
  geom=new StFgtGeom();
  mRnd = new TRandom3(); // general use random generator
  //  mRnd->SetSeed(0); // activate, assure every set of data is different

  par_bx_valley=3; // min separation between 2 clusters
  par_cl_width=3; //min cluster width
  par_seedStripThres=0; // a.u. for valid cluster
  par_clusterMinAmpl=0.0; // a.u., min cluster ampl 
  par_stripNoiseSigma=0.0  ; // a.u.,added as gauss noise to every strip

}

//--------------------------------------------
void 
StFgtClustFindMaker::Clear(Option_t *) {
  int i;
  for(i=0;i<kFgtMxDisk;i++) {
    mRadClustList[i].clear();
    mPhiClustList[i].clear();
  }
  StMaker::Clear();
}

//--------------------------------------------
StFgtClustFindMaker::~StFgtClustFindMaker(){

}

//_______________________________________________
//________________________________________________
void
StFgtClustFindMaker::saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());

  HList->Write();
  f.Close();

}


//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtClustFindMaker::Finish(){
  LOG_INFO<<"::Finish() \n"<<  endm; 


  return StMaker::Finish();
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtClustFindMaker::Make(){
  mInpEve++;
  
  LOG_INFO <<"::Make() inpEve="<<  mInpEve<<endm;
  StFgtSlowSimuMaker *ssMk=(StFgtSlowSimuMaker *) GetMaker("FgtSlowSimu");
  assert(ssMk);

  printf(" INPUT:  strips\n disk  Rad: #strip -> #clust   Phi: #strip -> #clust \n");

  // .... find 1D clusters in every plane of all disks
  for(int iDisk=0;iDisk<kFgtMxDisk;iDisk++) {
    int nRCl=findClust1D(ssMk->mRadAdcList[iDisk],mRadClustList[iDisk]);
    int nPCl=findClust1D(ssMk->mPhiAdcList[iDisk],mPhiClustList[iDisk]);

    printf("disk=%d  %d -->%d   %d -->%d \n",iDisk+1,ssMk->mRadAdcList[iDisk].size(),nRCl,ssMk->mPhiAdcList[iDisk].size(),nPCl);
    
    UInt_t i;
    int j;

    //::::::::::::::::process Rad-strips::::::::::::::::
    int nRQuad[kFgtMxQuad]; memset(nRQuad,0,sizeof(nRQuad));
// delete the last cluster if strip ID out of boundary
    if(mRadClustList[iDisk].size() > 0) {  
      if(mRadClustList[iDisk][mRadClustList[iDisk].size()-1].fBin_mean >
                  geom->radStripLOCId_number() * kFgtMxQuad - 1)  {
          cout << "A Rad cluster deleted with fBin_mean = " << 
          mRadClustList[iDisk][mRadClustList[iDisk].size()-1].fBin_mean << endl;

          mRadClustList[iDisk].erase(mRadClustList[iDisk].end());
      }
    }

    for(i=0; i<mRadClustList[iDisk].size(); i++) {
      fgt_cluster1D *cl=&mRadClustList[iDisk][i];
      cl->position=geom->stripID2Rxy(cl->fBin_mean);
      int iRadID=(int) cl->fBin_mean; // get rid of fractional bin accuracy
      cl->iQuad=iRadID / geom->radStripLOCId_number(); 
       printf("icl=%d rRad=%.3f totAmpl=%.1f  totStrip=%d\n",i,cl->position,cl->totAmpl,cl->nbin);
//      assert(  cl->iQuad <kFgtMxQuad);
      if(cl->iQuad >= 0 && cl->iQuad < kFgtMxQuad) {
        nRQuad[cl->iQuad]++;
        hA[5]->Fill(cl->peakAmpl);
        hA[7]->Fill(cl->nbin);
        hA[9]->Fill(cl->peakAmpl/cl->totAmpl);
      }
      else 
       cout << "ClusterFinder (Rad) error: iQuad = " << cl->iQuad << endl;
    }


    //::::::::::::::::process Phi-strips::::::::::::::::
    int nPQuad[kFgtMxQuad]; memset(nPQuad,0,sizeof(nPQuad));
// delete the last cluster if strip ID out of boundary
    if(mPhiClustList[iDisk].size() > 0) {  
      if(mPhiClustList[iDisk][mPhiClustList[iDisk].size()-1].fBin_mean >
                  geom->phiStripLOCId_number() * kFgtMxQuad - 1 ) {
          cout << "A Phi cluster deleted with fBin_mean = " << 
          mPhiClustList[iDisk][mPhiClustList[iDisk].size()-1].fBin_mean << endl;

          mPhiClustList[iDisk].erase(mPhiClustList[iDisk].end());
      }
    }

    for(i=0; i<mPhiClustList[iDisk].size(); i++) {
      fgt_cluster1D *cl=&mPhiClustList[iDisk][i];
      cl->position=geom->stripID2PhiLab(cl->fBin_mean);
      int iPhiID=(int) cl->fBin_mean; // get rid of fractional bin accuracy
      cl->iQuad=iPhiID / geom->phiStripLOCId_number(); 
      printf("icl=%d rPhi/deg=%.3f totAmpl=%.1f totStrip=%d\n",i,cl->position/3.1417*180.,cl->totAmpl,cl->nbin);
      if(cl->iQuad >= 0 && cl->iQuad < kFgtMxQuad) {
        nPQuad[cl->iQuad]++;

        hA[6]->Fill(cl->peakAmpl);
        hA[8]->Fill(cl->nbin);
        hA[10]->Fill(cl->peakAmpl/cl->totAmpl);
      }
      else 
       cout << "ClusterFinder (Phi) error: iQuad = " << cl->iQuad << endl;
    }


    //... QA results for both planes
    hA[0]->Fill(20*iDisk+0,nRCl);
    hA[0]->Fill(20*iDisk+1,nPCl);

    for(j=0;j<kFgtMxQuad;j++) {
      hA[1]->Fill(nRQuad[j]);
      hA[2]->Fill(nPQuad[j]);
      hA[3]->Fill(nRQuad[j],nPQuad[j]);
      cout << "iquad nRQuad, nPQuad = " << j << " " << nRQuad[j] 
                                             << " " << nPQuad[j] << endl;
    }
   
  } // end of given disk

 return kStOK;
}

 

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtClustFindMaker::Init(){
  LOG_INFO<<"::Init() "<<  endm; 
  assert(HList);
  mInpEve=0;
  int i;
  LOG_INFO<<Form("::Init params minPeakWidth=%d, minPeakSepar=%d (# of strips), seedStripThres=%.1f a.u., clusterMinAmpl=%.1f a.u., stripNoiseSigma=%.1f a.u.",par_cl_width,par_bx_valley,par_seedStripThres,par_clusterMinAmpl,par_stripNoiseSigma)<<  endm;
  assert(par_clusterMinAmpl>=0);
  assert(par_stripNoiseSigma>=0);

  hA[0]=new TH1F("cl_Stat1D","Found 1D clusters, odd=Rad, even=Phi; x=20*disk",180, -0.5,179.5); 
  
  hA[1]=new TH1F("cl_rMul","Mult Rad-clusters, 1D , any disks; multiplicity/Quad/event",6,0.5,6.5); // zero is in underflow
  hA[2]=new TH1F("cl_pMul","Mult Phi-clusters, 1D , any disks; multiplicity/Quad/event",6,0.5,6.5); // zero is in underflow
  hA[3]=new TH2F("cl_rpMul","Mult per quadrant, 2x1D, any disks;Phi-Rad multiplicity/Quad/event; rad-mult",6,0.5,6.5,6,0.5,6.5); // zero is in underflow
  

  hA[5]= new TH1F("cl_RmxAmp"," Rad-strips 1D clust ; Max strip Amplitude (a.u.)",100,0,1000);
  hA[6]= new TH1F("cl_PmxAmp"," phi-strips 1D clust ; Max strip Amplitude  (a.u.)",100,0,1000);

  hA[7]= new TH1F("cl_Rwid"," Rad-strips 1D clust ;total cluster width ",10,0.5,10.5);
  hA[8]= new TH1F("cl_Pwid"," Phi-strips 1D clust ;total cluster width ",20,0.5,20.5);

  hA[9] = new TH1F("cl_Rpf"," Rad-strips 1D clust ; fraction of peak strip energy ",50,0.,1.);
  hA[10]= new TH1F("cl_Ppf"," Phi-strips 1D clust ; fraction of peak strip energy ",50,0.,1.);

 

  for(i=0;i<mxH;i++) 
    if(hA[i]) HList->Add(hA[i]);
  
  return StMaker::Init();
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
int
StFgtClustFindMaker::findClust1D(vector<fgt_strip> &sL, vector<fgt_cluster1D> &clL) {
  //  printf("AA %d\n",sL.size());
  if(sL.size()<=0) return 0;
  fgt_strip fake; // assures the last cluster gets saved by the code below
  fake.id=99999999; // set above highest strip
  sL.push_back(fake);

  int bx0=-999,bx1=bx0; // first and last bin of current cluster
  double sum=0, sumx=0, peakA=0;
  for(UInt_t i=0;i<sL.size();i++) {
    assert(sL[i].id>=bx1); //assumes strips are ordered  increasingly
    if(sL[i].id>=bx1+par_bx_valley) { // new clusters starts
      int nbin=bx1-bx0+1;
      if(nbin>=par_cl_width && peakA>par_seedStripThres) { 
	if(par_stripNoiseSigma>0) { // fold in noise
	  // printf(" before noise sum=%.3f sumx=%.3f  meanX=%.3f\n",sum,sumx,sumx/sum);
	  int k;
	  for(k=bx0-1;k<=bx1+1;k++){
	    double rndAdc=mRnd->Gaus(0,par_stripNoiseSigma);
	    sum+=rndAdc;
	    sumx+=rndAdc*(k+0.5); // use center of the bin
	  }
	  // printf("  after noise sum=%.3f sumx=%.3f  meanX=%.3f\n",sum,sumx,sumx/sum);

	} 
	if(sum > par_clusterMinAmpl) { // record old cluster if surviving noise
	  fgt_cluster1D cl;
	  cl.nbin=nbin;
	  cl.fBin_mean=sumx/sum;
	  cl.totAmpl=sum;
	  cl.peakAmpl=peakA;
	  printf("add cl=%d  fBin=%.3f, sum=%.1f peakA=%.1f width=%d\n",clL.size(),cl.fBin_mean,sum,peakA,nbin);
	  clL.push_back(cl);
        }
      }
      sum=sumx=peakA=0;
      bx1=bx0=sL[i].id; 
    }
    sum+=sL[i].adc;
    sumx+=sL[i].adc*(sL[i].id+0.5); // use center of thebin
    if(peakA<sL[i].adc) peakA=sL[i].adc;
    bx1=sL[i].id;
    //printf("\nnc=%d i=%d bx=%d adc=%.1f sum=%f sumx=%.1f bx0=%d bx1=%d\n",clL.size(),i,sL[i].id,sL[i].adc,sum,sumx,bx0,bx1);
  }
  return clL.size(); 
}


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// $Log: StFgtClustFindMaker.cxx,v $
// Revision 1.2  2011/04/11 19:35:38  fisyak
// Replace uint by UInt_t, use TMath
//
// Revision 1.1  2011/04/07 19:31:22  balewski
// start
//


 


