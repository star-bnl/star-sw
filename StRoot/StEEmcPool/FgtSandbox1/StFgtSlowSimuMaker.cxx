// *-- Author : J.Balewski
// 
// $Id: StFgtSlowSimuMaker.cxx,v 1.5 2011/04/11 19:35:38 fisyak Exp $
#include <TVector3.h>
#include <TH2.h>
#include <TF1.h>
#include <TFile.h>
#include <TLine.h>
#include <TPolyLine.h>
#include <TCrown.h>
#include <TRandom3.h>

#include "StFgtSlowSimuMaker.h"
#include "HexLatice.h"

#include  "tables/St_g2t_fgt_hit_Table.h"

ClassImp(StFgtSlowSimuMaker)

//--------------------------------------------
StFgtSlowSimuMaker::StFgtSlowSimuMaker(const char *name):StMaker(name){
  /// Class Constructor.  
  setHList(0);
  memset(hA,0,sizeof(hA));
  geom=new StFgtGeom();
  mRnd = new TRandom3(); // general use random generator
  //  mRnd->SetSeed(0); // activate, assure every set of data is different

  forcePerpTracks(false); // normal operation
  meLossTab[0]=-999; // default=uninitialized

  par_2DpixAmplThres=0.1; // a.u. MIP respons has maxAmpl~200-300== # of electrons
  par_stripAmplThres=1.0; // a.u., drop strips below it, default=1.0
  par_XYamplSigma=0.035 ; // cm, for 2D gauss smearing, default=0.035

  par_radStripGainMean=1.0;par_radStripGainSigma=0.0;// allow unequal energy sharing
  par_cutoffOfBichel = 9992; // meLossTab[9993-10000]=5.0E+03 to 6.0E+06 (too high) are cut off. They are replaced by meLossTab[9992]=4.78E+03 in simulation. 
  mRadStripRelativeGain=0;// clear pointer
  mPhiStripRelativeGain=0;// clear pointer
  hexLat=0;
}

//--------------------------------------------
void 
StFgtSlowSimuMaker::Clear(Option_t *) { 
  int i,j;
  for(i=0;i<kFgtMxDisk;i++){
    for(j=0;j<kFgtMxQuad;j++) {
      mG2tHitList[i][j].clear();
    }
    mRadAdcList[i].clear();
    mPhiAdcList[i].clear();
  }
  StMaker::Clear();
}


//--------------------------------------------
StFgtSlowSimuMaker::~StFgtSlowSimuMaker(){

}

//_______________________________________________
//________________________________________________
void
StFgtSlowSimuMaker::saveHisto(TString fname){

  CloseHisto();

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
StFgtSlowSimuMaker::Finish(){
  LOG_INFO<<"::Finish() \n"<<  endm; 


  return StMaker::Finish();
}

 

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtSlowSimuMaker::Init(){
  LOG_INFO<<"::Init() "<<  endm; 
  assert(HList);
  assert(  meLossTab[0]>0); //Frank's model of # of electron pairs must be initialized 
  mInpEve=0;

  /* model transverse response in 1D
     Frank: However, from test beam data I calculate the RMS of each cluster, 
     and there I get something like 0.5 - 0.6 strips, which is around 350 um.
     all clusters (both in my diploma thesis and in my email) are 1D. A
     cluster is defined as something on ONE projection of the detector. So
     the FWHM and the sigma is all 1D. So you should not divide by Sqrt(2) to
     get the 1 D value.
  */

  amplF= new TF1("Gs","exp( -(x-[0])*(x-[0])/2./[1]/[1])",-1.,1.); // X in cm
  amplF->SetParNames("X0","sig");
  amplF ->SetParameters(0.,par_XYamplSigma);
  amplF ->SetLineWidth(2); amplF->SetLineColor(kMagenta);

  InitHisto1();
  InitHisto2();

  // initialize RAD- gains
  mRadStripRelativeGain=new double[geom->radStripGBLId_number()+1]; // allocate array
  memset(mRadStripRelativeGain,0,sizeof(mRadStripRelativeGain));
  int i;
  for(i=0;i<geom->radStripGBLId_number();i++) {
    double del=99999;
    while(fabs(del)> 2.*par_radStripGainSigma)
      del= mRnd->Gaus(0,par_radStripGainSigma);
      mRadStripRelativeGain[i]=par_radStripGainMean+del;
      // if(i<20) printf("rad%d %.4f\n",i,mRadStripRelativeGain[i]);
  }
  
  // initialize PHI- gains
  mPhiStripRelativeGain=new double[geom->phiStripGBLId_number()+1]; // allocate array
  memset(mPhiStripRelativeGain,0,sizeof(mPhiStripRelativeGain));
  for(i=0;i<geom->phiStripGBLId_number();i++) {
    double del=99999;
    while(fabs(del)> 2.*par_phiStripGainSigma)
      del= mRnd->Gaus(0,par_phiStripGainSigma);
      mPhiStripRelativeGain[i]=par_phiStripGainMean+del;
      // if(i<20) printf("phi%d %.4f\n",i,mPhiStripRelativeGain[i]);
  }
  
  // initialize hexagonal Gem latice
  if(par_hexLaticePitch >0.001) {
    hexLat=new  HexLatice (par_hexLaticePitch, par_hexLaticePhi1deg);
  } else {
    LOG_INFO<<Form("::Init hexGemLatice DISABLED, too small pitch")<<endm;
    par_hexLaticePitch = 0.0;
  }

  geom->printParam();

  LOG_INFO<<Form("::Init params: X,YamplSigma=%.4f cm,  2DpixAmplThres=%.2f a.u., stripAmplThres=%.2f a.u.,  forcePerpTracks=%d  useOnlyDisk=%d RadGain(m=%.2f,sig=%.2f)  PhiGain(m=%.2f,sig=%.2f, GemHexLatice: pitch/um=%.1f, phi1/deg=%.1f, transDiffusion=%.1f um/1cmOfPath, cutoffOfBichel=%d)",par_XYamplSigma,par_2DpixAmplThres,par_stripAmplThres,par_forcePerp,par_useOnlyDisk,par_radStripGainMean,par_radStripGainSigma,par_phiStripGainMean,par_phiStripGainSigma,par_hexLaticePitch*1e4, par_hexLaticePhi1deg,par_transDiffusionPerPath*1e4,par_cutoffOfBichel)<<  endm; // fix it

  assert(par_XYamplSigma>0);

  Info("Init","testing access to TGeoManager");

  if (gGeoManager) { // Geom already there
    Info("Load","TGeoManager(%s,%s) is already there"
            ,gGeoManager->GetName(),gGeoManager->GetTitle());
  } else {
    Warning("Init","add  TGeoManager");
    //   TString geo="y2009a"; // got it from Victor
    //   TString ts("$STAR/StarDb/VmcGeometry/");
    //  ts+=geo; ts+=".h"; 
    TString ts = "SandBox.C(0,0)";
    //printf("WILL execute macro=%s=\n",ts.Data()); 
    int ierr=0;
    gROOT->Macro(ts, &ierr);
    assert(!ierr);
  }
  assert(gGeoManager);

  TGeoVolume *geoFgt = gGeoManager ->FindVolumeFast("FGMO");
  assert(geoFgt);
  geoFgt -> Print();
  geoFgt -> InspectMaterial();
  geoFgt -> InspectShape();
  printf("XXXXXXXXXXXXXXXXXXXXXXXXX\nXXXXXXXXXXXXX\nXXXXXXXXXX\n");



  return StMaker::Init();
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtSlowSimuMaker::Make(){
  mInpEve++;
  
  LOG_INFO <<"::Make() inpEve="<<  mInpEve<<endm;
  //============ FGT ==========
  St_g2t_fgt_hit *fgt_hitT = (St_g2t_fgt_hit *) GetDataSet("g2t_fgt_hit");
  if(fgt_hitT ==0) {
    LOG_FATAL<<Form("g2t_Fgt table empty")<<endm;
    return kStOK;
  }

  /* In year 2012 only 6 disks are build and installed in STAR
     This implementation allows for up to 8 disk to be used in reco and
     additional disk #9 can exist in GSTR but sort_g2t_hits()  will dropp them.
     If disk #10 or more is reported the code will crash
  */

  sort_g2t_hits( fgt_hitT );

  int iDisk=0,iQuad=0;
  for(iDisk=0;iDisk<kFgtMxDisk;iDisk++) { 
    digRad->Reset();  digPhi->Reset();  //clear once per disk  
    if(par_useOnlyDisk) if(par_useOnlyDisk-1 != iDisk) continue;
    for(iQuad=0;iQuad<kFgtMxQuad;iQuad++) {
      digXY->Reset(); // needs reset for every quadrant
      
      printf(" process %d g2t hits in disk=%d quad=%d\n", mG2tHitList[iDisk][iQuad].size(),iDisk,iQuad);
      // accumulate response in digXY array
      vector<fgt_g2t_auxil> &L=mG2tHitList[iDisk][iQuad];
      if(L.size()<=0) continue;// drop if empty quad
      for(UInt_t
 i=0;i<L.size();i++) { // populate: digXY
	//responseLinearModel(h->Rloc,h->Dloc);
	responseFrankModel(L[i].Rloc,L[i].Dloc);
	hA[11+iDisk]->Fill(L[i].Rlab.x(),L[i].Rlab.y()); // monitor hit distribution
       	printf("iQ=%d itr=%d  R/cm=%.2f  phi/deg=%.3f\n",iQuad,i,L[i].Rlab.Perp(), L[i].Rlab.Phi()/3.1416*180); // pi value corrected WMZ
      }
      // now response is stored in digXY array
      if(!projectQuadrant(iQuad)) continue;// drop if empty quad
      // strip response is generated and projected to 1D stip histos: R & Phi
      // break; // only 1st quad, tmp
    }
    // only once per disk
    exportStripPlane(digRad,mRadAdcList[iDisk]);
    exportStripPlane(digPhi,mPhiAdcList[iDisk]);
    // break; 
  } // end of disk

  printf(" Summary of fired strips\n disk  #Rad-strip  #Phi-strip \n");
  for(iDisk=0;iDisk<kFgtMxDisk;iDisk++) printf("%d  %d  %d \n",iDisk,mRadAdcList[iDisk].size(),mPhiAdcList[iDisk].size());
  
  return kStOK;
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void 
StFgtSlowSimuMaker::sort_g2t_hits( St_g2t_fgt_hit *fgt_hitT){
  g2t_fgt_hit_st *hitPtr  = fgt_hitT->GetTable();
  assert(hitPtr);
  LOG_INFO<<Form("Sorting g2t FGT hits, size=%d",fgt_hitT->GetNRows())<<endm;

  int ntot=0;
  Int_t     nhits      = fgt_hitT->GetNRows();
  for(Int_t ih=0; ih<nhits; ih++,hitPtr++) {
    Int_t   ivid  = hitPtr->volume_id;

    // decode volume_id WMZ
    Int_t numbv1 = ivid/1000000;  
    Int_t numbv2 = (ivid/10000)%100;
    int diskID, iQuad;  // diskID and quadID, both start from 0

//No Quad ID for now, it assigned to 0 for now
     diskID = numbv1 - 1;
     iQuad = 0; //temp
//    
/* Disk9 is not a concer for now, commented out 
    if(numbv2 != 0) {
       diskID = numbv1 - 1;
       iQuad = numbv2 - 1;
    } else {
       diskID = 8;
       iQuad = numbv1 - 1;
    }
*/
    cout << " Volume_id diskID QuadID: " 
	 << ivid << " " << diskID << " " << iQuad << endl;
    
    TVector3 Rlab( hitPtr->x); // entrance point  in Lab ref 
    TVector3 Rloc=Rlab; 
/*
 IQuad my not match Rlab.Phi() for hits at edges between two quadrants 
 (deadQuadEdge)! The following conversion would rotate Rlab with a wrong 
 angle if a mismatch happens. However, Rloc.x() or Rloc.y() would be still 
 very close to one of two edges of a local quadrant and the cuts on Rloc.x() 
 and Rloc.y() later would reject them.    WMZ
*/

    Rloc.RotateZ(-geom->phiQuadXaxis(iQuad));

    hA[3]->Fill(1);
    if(diskID!=8 && !geom->inDisk(Rlab)) continue;  // drop 8 ? JB
    hA[3]->Fill(2);

    if(fabs(Rloc.x()) < geom->deadQuadEdge())  continue;
    hA[3]->Fill(3);
    if(fabs(Rloc.y()) < geom->deadQuadEdge())  continue;
    hA[3]->Fill(4);

    double tof_ns=hitPtr->tof*1.e9;
    hA[5]->Fill(tof_ns);
    if(hitPtr->tof> geom-> maxTof() ) continue;
    hA[3]->Fill(5);

    TVector3 Plab(hitPtr->p);
    if(Plab.Mag()>0) hA[7]->Fill(log10(Plab.Mag()*1000.));
    if(Plab.Mag()< geom-> minPmag() ) continue;
    hA[3]->Fill(6);


    /*
      TVector3 RGeant=Rloc;    RGeant.RotateZ(-0.7854); 
      
      cout << " R = " <<  Rlab.Perp() << endl;
      cout << " Rlab_x, _y   = " << Rlab.x() << " " << Rlab.y() << endl;
      cout << " Rloc_x, _y   = " << Rloc.x() << " " << Rloc.y() << endl;
      cout << " RGeant_x, _y = " << RGeant.x() << " " << RGeant.y() << endl;
      cout << " phiG, phiLoc, phiLab  = " << RGeant.Phi()*180/3.14159 << " "
      << Rloc.Phi()*180/3.14159 << " "
      << Rlab.Phi()*180/3.14159 << " " << endl;
    */
    
    //  Compare quadID from StFgtGeom and the one decoded from Geant  WMZ
    int iQuadChk=geom->getQuad(Rlab.Phi());
    if(iQuad != iQuadChk) {
      cout << "  Something wrong in quadIDs!! " << endl;
      cout << "  iQuad from StFgtGeom and iQuad from Geant  = " 
	   << iQuadChk << ", " << iQuad << endl;
    }
    
    //  hA[3]->Fill(7...9); free
    
    //....... hit is accepted for processing ...............
    double ds=hitPtr->ds;
   
    TVector3 verLab=Plab.Unit(); // direction of track in LAB
    if(par_forcePerp) verLab=TVector3(0,0,1);// for testing ONLY:  make track perp to GEM
    TVector3 Rloc2=Rlab+ds*verLab;   Rloc2.RotateZ(-geom->phiQuadXaxis(iQuad));
    TVector3 Dloc=Rloc2-Rloc; // local vector along the path

    fgt_g2t_auxil aux;
    aux.hitPtr=hitPtr;
    aux.Rloc=Rloc;
    aux.Dloc=Dloc;
    aux.Rlab=Rlab+ds/2.*verLab; // set it in the middle of the track segment

    aux.iQuad=iQuad;
    mG2tHitList[diskID][iQuad].push_back(aux); // ID starts from 0, WMZ

    ntot++;

    //...... only QA is below .......

    // QA  accpted hits
    hA[3]->Fill(10+5*diskID);
    hA[3]->Fill(10+5*diskID+iQuad+1);
    double de_kev=hitPtr->de*1.e6;
    hA[0]->Fill(de_kev);
    hA[1]->Fill(hitPtr->ds);
    hA[2]->Fill(Rlab.z());
    hA[4]->Fill(Rlab.x(),Rlab.y());
    hA[6]->Fill(Rlab.Perp());
    
  }// loop over hits
  LOG_INFO<<Form("Sorting g2g FGT %d hits --> accepted %d",nhits,ntot)<<endm;
}


//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
bool 
StFgtSlowSimuMaker::projectQuadrant( int iquad) {

  double totAmp=digXY->Integral();
  LOG_INFO<<Form("::digiQuad()  totAmp=%g",totAmp)<<  endm;
  if(totAmp <par_stripAmplThres) return false;
  
  int nPix=0;
  
  double sumAmp=0; //for QA
  
  int nx=digXY->GetNbinsX();
  int ny=digXY->GetNbinsY();
  int bx,by;
  for(bx=1;bx<=nx;bx++)
    for(by=1;by<=ny;by++) {
      float amp=digXY->GetBinContent(bx,by); // this is not double because histo if float
      if(amp<par_2DpixAmplThres) continue; // drop too low signal
      nPix++;
      sumAmp+=amp;
      double x=digXY->GetXaxis()->GetBinCenter(bx); // SLOW
      double y=digXY->GetYaxis()->GetBinCenter(by); // SLOW
      int iRadID,iPhiID;// strip coordinates
      if( !geom->localXYtoStripID(iquad,x,y,iRadID, iPhiID)) continue;
//        printf( "ampl=%f loc: x=%f y=%f iRad=%d iPhi=%d bx=%d by=%d\n",amp,x,y,iRadID, iPhiID,bx,by); 
     
      //accumulate response of individual strips
      digRad->Fill(iRadID,amp* mRadStripRelativeGain[iRadID]);
      digPhi->Fill(iPhiID,amp* mPhiStripRelativeGain[iPhiID]);      

      digRadAll->Fill(iRadID,amp* mRadStripRelativeGain[iRadID]);
      digPhiAll->Fill(iPhiID,amp* mPhiStripRelativeGain[iPhiID]);      

    }
  printf("digi: nPix=%d   sumAmp=%g\n", nPix,sumAmp); 
  
  return true;
} 
  

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void
StFgtSlowSimuMaker::exportStripPlane(TH1F *h, vector<fgt_strip> &L) {
  float *y=h->GetArray();
  y++; // root histo counts bins from 1 - incredible silly
  float nx=h->GetNbinsX();
  for(int id=0;id<nx;id++,y++) {
    double ene=*y;
    if(ene <par_stripAmplThres) continue;
    fgt_strip st;
    st.id=id; // strips IDs are counted from zero
    st.adc=ene;
    L.push_back(st);
    //printf("%s add strID=%d adc=%.1f\n",h->GetName(),st.id,st.adc);
  }
}

        



/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// $Log: StFgtSlowSimuMaker.cxx,v $
// Revision 1.5  2011/04/11 19:35:38  fisyak
// Replace uint by UInt_t, use TMath
//
// Revision 1.4  2011/04/08 22:18:42  balewski
// added access to TGeo
//
// Revision 1.3  2011/04/08 19:25:45  wzhang
// Changed diskID assignment for Jan temporarily
//
// Revision 1.2  2011/04/08 01:14:13  balewski
// removed most of FGT from ver 3
//
// Revision 1.1  2011/04/07 19:31:22  balewski
// start
//


 


