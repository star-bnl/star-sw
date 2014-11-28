// *-- Author : J.Balewski
// 
// $Id: StFgtSlowSimuMaker.cxx,v 1.4 2012/11/08 17:16:20 akio Exp $
#include <TVector3.h>
#include <TH2.h>
#include <TF1.h>
#include <TFile.h>
#include <TLine.h>
#include <TPolyLine.h>
#include <TCrown.h>
#include <TRandom3.h>

#include "StFgtSlowSimuMaker.h"
  
#include  "tables/St_g2t_fgt_hit_Table.h"
#include "StFgtDbMaker/StFgtDbMaker.h"
#include "StFgtDbMaker/StFgtDb.h"

#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StEvent.h"

ClassImp(StFgtSlowSimuMaker)

//--------------------------------------------
  StFgtSlowSimuMaker::StFgtSlowSimuMaker(const char *name):StMaker(name){
  setHList(0);
  memset(hA,0,sizeof(hA));
  fgtDb=0;
  par_badSetup=0; // default all is OK
  mRnd = new TRandom3(); // general use random generator
  mRnd->SetSeed(0); // activate, assure every set of data is different
  switch_addPeds=1; //  re-set in bfc.C

}

const Float_t StFgtSlowSimuMaker::pulseShape[20]={0.0,0.1,0.3,0.4,0.2,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};//sum 1.0
//--------------------------------------------
//--------------------------------------------
void 
StFgtSlowSimuMaker::Clear(Option_t *) { 
  Int_t i,j;
  for(i=0;i<kFgtNumDiscs;i++){
    for(j=0;j<kFgtNumQuads;j++) {
      mG2tHitList[i][j].clear();
    }
  }  

  StMaker::Clear();
}


//--------------------------------------------
//--------------------------------------------
StFgtSlowSimuMaker::~StFgtSlowSimuMaker(){

}

//_______________________________________________
//________________________________________________
void
StFgtSlowSimuMaker::saveHisto(TString fname){

#ifdef __FGT_QA_HISTO__
  CloseHisto();

  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  //  jjassert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());

  HList->Write();
  f.Close();
#endif

}


//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtSlowSimuMaker::Finish(){ 
  LOG_INFO<<"::Finish() \n"<<  endm; 
  
#ifdef __FGT_QA_HISTO__
  TString a("fgtSlowSimQA");
  saveHisto(a);
#endif

  return StMaker::Finish();
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtSlowSimuMaker::InitRun(Int_t runNumber){
  LOG_INFO<<"::InitRun() "<< runNumber<< endm; 

  if(fgtDb==0){
    StFgtDbMaker* dbmkr=(StFgtDbMaker*)GetMaker("fgtDb");
    if(!dbmkr) {
      LOG_ERROR<<"StFgtSlowSimuMaker::InitRun could not find FgtDb Maker"<<endm;
      return kStWarn;
    }
    fgtDb=dbmkr->getDbTables();  
    if(!fgtDb){
      LOG_ERROR<<"StFgtSlowSimuMaker::InitRun could not find FgtDb table from FgtDbMaker"<<endm;
      return kStWarn;
    }
  }

  LOG_INFO<< Form("fgt-simu-params from DB, ver=%.0f:\n",fgtDb->getSimuParam(0))<<endl;
  // for(int kk=0;kk<15;kk++)  printf("par[%d]=%f\n",kk, fgtDb->getSimuParam(kk));

  // analog signal propagation
  par_cutoffOfBichel=fgtDb->getSimuParam(1); // eLossTable
  par_pairsPerCm    =fgtDb->getSimuParam(2); //( ions/cm) # of primary pairs produced per cm of path 
  par_trackTOFcutoff=fgtDb->getSimuParam(3); //  (seconds) track ignored by FGT slow sim
  par_XYamplSigma   =fgtDb->getSimuParam(4) ; // cm, for 2D gauss smearing, default=0.035 for FNAL
  par_trackPcutoff  =fgtDb->getSimuParam(5); // (GeV) track ignored by FGT slow sim
  par_transDiffusionPerPath=fgtDb->getSimuParam(6); //  (cm per 1 cm of path) , must be specified 
  par_binStep=fgtDb->getSimuParam(7); // # of bins in 2D distribution to store 2D gauss

  // digitalization
  par_stripThreshAdc=fgtDb->getSimuParam(10); //  drop strips below it
  par_2DampCutoffScale=fgtDb->getSimuParam(11); // in a.u. used in simu
  par_overalGain=fgtDb->getSimuParam(12); // a factor making simulated ADCs comparable to 2012 data 
  LOG_INFO<<"par_overalGain from DB = " << par_overalGain << ", but overwriting to 20 for now. Need fix in DB later"<<endm; 
  par_overalGain=20;
  par_PplaneChargeFraction=fgtDb->getSimuParam(13); //  divide charge between P/R plane

  LOG_INFO<<Form("::InitRun() runNo=%d  badSetup=0x%x;  params: track cutoff: TOF<%.1fns and  P>%.1f MeV/c ; prim ions/cm=%.1f, X,YamplSigma=%.4f cm,  stripThres=%.2f (ADC),   transDiffusion=%.1f um/1cm, cutoffOfBichel=%d, binStep=%d  Digi: 2DampCutoffScale=%.1f a.u., overalGain=%.2f a.u.  P-planeChargFract=%.2f  switch_addPeds=%d", runNumber, par_badSetup,
		 par_trackTOFcutoff*1e9,par_trackPcutoff*1e3,
		 par_pairsPerCm, par_XYamplSigma,par_stripThreshAdc,
		 par_transDiffusionPerPath*1e4,
		 par_cutoffOfBichel, par_binStep,
		 par_2DampCutoffScale,par_overalGain,par_PplaneChargeFraction,
		 switch_addPeds)<<  endm; // fix it
  
  //if() par_badSetup+=0x1;
  if(par_XYamplSigma<=0) par_badSetup+=0x2;//jjassert(par_XYamplSigma>0);
  if(par_pairsPerCm <=0) par_badSetup+=0x4;//jjassert(par_pairsPerCm >0);
 
  
  return kStOK;
}
 

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtSlowSimuMaker::Init(){
  
  LOG_INFO<<"::Init()"<<  endm; 

  mInpEve=0;

#ifdef __FGT_QA_HISTO__
  HList=new TObjArray;
#endif

  InitHisto1();

#ifdef __FGT_QA_HISTO__
  InitHisto2();
#endif 



  //jjassert(fgtDb);

  return StMaker::Init();
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtSlowSimuMaker::Make(){
  bool debug=false;
  mInpEve++;  
  
  if( par_badSetup) {
    LOG_ERROR<< Form(" Make skips FGT content for this event %d because of incorrect initialization from DB, val=0x%0x",mInpEve,par_badSetup);
    return kStOK;    
  }


  //  Prepare the environment for filling the FGT --> StEvent 
  StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  //jjassert(mEvent); // fix your chain
  if ( !mEvent )
    {
      LOG_WARN << "StEvent is missing.  FGT slow simulator skipping event" << endl;
      return kStWarn;
    }
  
  mEventId=mEvent->id(); 
  LOG_INFO <<Form("%s::Make() inpEve=%d, eveId=%d", GetName(),mInpEve,mEventId)<<endm;

 
  // new access to fgtCollection
  StFgtCollection*   fgtColl= mEvent-> fgtCollection();

  if(fgtColl==0) {
    StFgtCollection*   fgtX=new StFgtCollection();    
    mEvent->setFgtCollection(fgtX);
    //LOG_INFO << Form("%s::Make added a non existing StFgtCollection()",GetName())<<endm;
    fgtColl= mEvent-> fgtCollection();
  }

  //jjassert(fgtColl);// now it can't fail unless ths big is messed up
 
  //jjassert((int)fgtColl->getNumDiscs()==kFgtNumDiscs); // tmp, should be initialized from the common source - keep it for now
  //mEvent->ls(3); 


  //============ FGT ==========
  St_g2t_fgt_hit *fgt_hitT = (St_g2t_fgt_hit *) GetDataSet("g2t_fgt_hit");
  if(fgt_hitT ==0) {
    LOG_FATAL<<Form("g2t_Fgt table empty")<<endm;
    return kStOK;
  }
 

  unpack_g2t_hits( fgt_hitT ); // unpack geant response

  /*  the 2D quadrant response map is produced separately for each of
      24 quadrant due to RAM limitations for quadDigitizationXY which is humongous.
      Strip response is accumulated for whole disk, then exported
  */
  Int_t iDisc=0,iQuad=0;
  for(iDisc=0;iDisc<kFgtNumDiscs ;iDisc++) { 
    for(iQuad=0;iQuad<kFgtNumQuads;iQuad++) {
      
      quadDigitizationXY->Reset(); // needs reset for every quadrant
      quadDigitizationRad->Reset();  quadDigitizationPhi->Reset();  //clear once per quad  
      // accumulate response in quadDigitizationXY array
      vector<fgt_g2t_auxil> &L=mG2tHitList[iDisc][iQuad];
      if(L.size()<=0) continue;// drop if empty quad
      Int_t stripIdOffset= kFgtNumStrips * ( iDisc*kFgtNumQuads + iQuad)*2;      
      if(debug) LOG_INFO<<Form("Process g2t hits in disk=%d quad=%d nHit=%d idOff=%d",iDisc,iQuad,L.size(),stripIdOffset)<<endm; 
      for(UInt_t i=0;i<L.size();i++) { // populate: quadDigitizationXY
	responseMipModel(L[i].Rloc,L[i].Dloc);
#ifdef __FGT_QA_HISTO__
	hA[11+iDisc] ->Fill(L[i].Rlab.x(),L[i].Rlab.y()); // monitor hit distribution
#endif
	if(debug){
	  printf("DIGIXY Lab: Disc=%1d Quad=%1d itr=%d  x=%8.4f y=%8.4f z=%8.4f R=%8.4f  phi=%10.6f\n",
		 iDisc,iQuad,i,L[i].Rlab.X(),L[i].Rlab.Y(),L[i].Rlab.Z(),L[i].Rlab.Perp(), L[i].Rlab.Phi()); // pi value corrected WMZ
	  printf("DIGIXY Loc: Disc=%1d Quad=%1d itr=%d  x=%8.4f y=%8.4f z=%8.4f R=%8.4f  phi=%10.6f\n",
		 iDisc,iQuad,i,L[i].Rloc.X(),L[i].Rloc.Y(),L[i].Rloc.Z(),L[i].Rloc.Perp(), L[i].Rloc.Phi()); // pi value corrected WMZ
	}
      }
      // now full quadrtant response is stored in quadDigitizationXY array
      if(quadDigitizationXY->Integral()<0.1) continue;
      projectQuad2strips( iDisc, iQuad);
      // strips response is generated (and projected to 1D stip histos: R & Phi for QA)
      exportStripPlane2StEvent(quadDigitizationRad,stripIdOffset,fgtColl->getStripCollection(iDisc));
      exportStripPlane2StEvent(quadDigitizationPhi,stripIdOffset+kFgtNumStrips,fgtColl->getStripCollection(iDisc));
    }// end of quadrant
  } // end of disk
  
  LOG_INFO<<Form("End of fgt-slow-simu FgtColl: numDisc=%d, total strips=%d",fgtColl->getNumDiscs(),fgtColl -> getNumStrips()  )<<endm;

  if(debug){
    for(iDisc=0; iDisc <(int)fgtColl->getNumDiscs(); iDisc++) {
      StFgtStripCollection *stripPtr= fgtColl->getStripCollection(iDisc);
      
      //printf("  content: iDisc=%d  # of : strips=%d  clust=hits=%d\n" ,stripPtr -> getDisc() ,fgtColl ->getNumStrips(iDisc)  ,fgtColl -> getNumHits( iDisc));
      
      StSPtrVecFgtStrip &stripVec = stripPtr->getStripVec();    
      Int_t ih=0;
      for( StSPtrVecFgtStripIterator it=stripVec.begin();it!=stripVec.end();++it, ih++)    {
	// details of strip localization, use output variables ending w/ X
	Short_t discX,  quadrantX,  stripX; Char_t  layerX;
	StFgtGeom::decodeGeoId(((*it))->getGeoId(),discX,quadrantX, layerX, stripX);
	// octX is 0 for short octant and 1 for long octant
	//Int_t octX=1; if (stripX<300) octX=0;
	LOG_DEBUG<<Form("iDisc=%d ih=%d  strip: geoId=%d ADC=%d  charge=%.1f deco0: strip=%d quad=%d oct=%d plane=%c disc=%d \n",iDisc,ih,((*it))->getGeoId(),((*it))->getAdc(0),((*it))->getCharge(),stripX,quadrantX,stripX>=300,layerX,discX)<<endm;
	//printf("     decode -> disc=%d, quad=%d layer=%c, strip=%d xOct=%d\n", disc,quadrant, layer, strip,xOct);      
      }
    }
  }

  return kStOK;
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void 
StFgtSlowSimuMaker::unpack_g2t_hits( St_g2t_fgt_hit *fgt_hitT){
  g2t_fgt_hit_st *hitPtr  = fgt_hitT->GetTable();
  //jjassert(hitPtr);
  //LOG_INFO<<Form("Unpacking g2t  FGT hits, size=%d",fgt_hitT->GetNRows())<<endm;

  Int_t ntot=0;
  Int_t     nhits      = fgt_hitT->GetNRows();
  for(Int_t ih=0; ih<nhits; ih++,hitPtr++) {
    Int_t   ivid  = hitPtr->volume_id;
    Double_t de_keV= hitPtr->de*1e6;
    Double_t tof_ns=hitPtr->tof*1.e9;

    // decode volume_id , Victor: I will use volume_id = numbv(1)*100 + numbv(2)
    Int_t numbv1 = ivid/100;  
    Int_t numbv2 = ivid%100;
    Int_t iDisc; // range [0  //myMkSM->testStripMap('R');-5]
    Int_t iQuad;  // range [0-3]
        
    iDisc = numbv1-1 ;
    iQuad = numbv2 - 1;

    TVector3 Rlab( hitPtr->x); // entrance point  in Lab ref 
    Float_t   Rxy=sqrt(Rlab.X()*Rlab.X()+Rlab.Y()*Rlab.Y());
    //LOG_INFO<<Form("Volume_id ivid=%d discID=%d  QuadID=%c  LAB x=%.2f y=%.2f z=%.2f eta=%.3f  phi=%10.6f, Rxy/cm=%8.4f   de/keV=%g  tof/ns=%f", 
    //	   ivid, iDisc+1, iQuad+'A',Rlab.X(),Rlab.Y(),Rlab.Z(), Rlab.Eta(), Rlab.Phi(), Rxy ,de_keV, tof_ns);
    //LOG_INFO<<Form(" Px=%8.4f Py=%8.4f Pz=%8.4f\n",hitPtr->p[0],hitPtr->p[1],hitPtr->p[2]);

#if 0 // use only disc #1, for testing
    if(iDisc!=0 || iQuad!=1) {
      printf("TT tmp skip disc ? or quad ?\n");
      hA[3]->Fill(8);
      continue;
    }
#endif

    /*
      IQuad my not match Rlab.Phi() for hits at edges between two quadrants 
      (deadQuadEdge)! The following conversion would rotate Rlab with a wrong 
      angle if a mismatch happens. However, Rloc.x() or Rloc.y() would be still 
      very close to one of two edges of a local quadrant and the cuts on Rloc.x() 
      and Rloc.y() later would reject them.    WMZ
    */
    
    //printf("Unpack Rlab: Disc=%1d Quad=%1d x=%10.6f y=%10.6f z=%10.6f R=%10.6f  phi=%10.6f\n",
    //	   iDisc,iQuad,Rlab.X(),Rlab.Y(),Rlab.Z(),Rlab.Perp(), Rlab.Phi()); 
    TVector3 Rloc=Rlab;  // tmp, no ability to shift/tilt disc in STAR Jan
    Rloc.RotateZ(-StFgtGeom::phiQuadXaxis(iQuad));
    //printf("Unpack RLoc: Disc=%1d Quad=%1d x=%10.6f y=%10.6f z=%10.6f R=%10.6f  phi=%10.6f\n",
    //	   iDisc,iQuad,Rloc.X(),Rloc.Y(),Rloc.Z(),Rloc.Perp(), Rloc.Phi()); 

#ifdef __FGT_QA_HISTO__
    hA[3]->Fill(1);
#endif
    if(!StFgtGeom::inDisc(Rlab)) continue;
#ifdef __FGT_QA_HISTO__
    hA[3]->Fill(2);
#endif
    if(fabs(Rloc.x()) < StFgtGeom::deadQuadEdge()) continue;
#ifdef __FGT_QA_HISTO__
    hA[3]->Fill(3);
#endif
    if(fabs(Rloc.y()) < StFgtGeom::deadQuadEdge()) continue;
#ifdef __FGT_QA_HISTO__
    hA[3]->Fill(4);
#endif
    if(!StFgtGeom::belowFlat(Rloc)) continue;
#ifdef __FGT_QA_HISTO__
    hA[3]->Fill(5);
    hA[5]->Fill(tof_ns);
#endif
    if(hitPtr->tof> par_trackTOFcutoff ) continue;
#ifdef __FGT_QA_HISTO__
    hA[3]->Fill(6);
#endif

    TVector3 Plab(hitPtr->p);
    //TVector3 Plab(0,0,1);
#ifdef __FGT_QA_HISTO__
    if(Plab.Mag()>0) hA[7]->Fill(log10(Plab.Mag()*1000.));
#endif
    if(Plab.Mag()<  par_trackPcutoff ) continue;
#ifdef __FGT_QA_HISTO__
    hA[3]->Fill(7);
#endif 
    /*   *** This model assumes all tracks are MIPS ***  */
        
    //  hA[3]->Fill(7...9); free
    
    //....... hit is accepted for processing ...............
    Double_t ds=hitPtr->ds;
   
    TVector3 verLab=Plab.Unit(); // direction of track in LAB
    //    if(par_forcePerp) verLab=TVector3(0,0,1);// for testing ONLY:  make track perp to GEM
    TVector3 Rloc2=Rlab+ds*verLab;   Rloc2.RotateZ(-StFgtGeom::phiQuadXaxis(iQuad));
    TVector3 Dloc=Rloc2-Rloc; // local vector along the path
    /*
    printf("Unpack Plab: Disc=%1d Quad=%1d x=%10.6f y=%10.6f z=%10.6f R=%10.6f  phi=%10.6f\n",
	       iDisc,iQuad,Plab.X(),Plab.Y(),Plab.Z(),Plab.Perp(),Plab.Phi());
    printf("Unpack verL: Disc=%1d Quad=%1d x=%10.6f y=%10.6f z=%10.6f R=%10.6f  phi=%10.6f\n",
	       iDisc,iQuad,verLab.X(),verLab.Y(),verLab.Z(),verLab.Perp(),verLab.Phi());
    printf("Unpack RLo2: Disc=%1d Quad=%1d x=%10.6f y=%10.6f z=%10.6f R=%10.6f  phi=%10.6f\n",
	       iDisc,iQuad,Rloc2.X(),Rloc2.Y(),Rloc2.Z(),Rloc2.Perp(),Rloc2.Phi());
    printf("Unpack DLoc: Disc=%1d Quad=%1d x=%10.6f y=%10.6f z=%10.6f R=%10.6f  phi=%10.6f\n",
	       iDisc,iQuad,Dloc.X(),Dloc.Y(),Dloc.Z(),Dloc.Perp(),Dloc.Phi());
    */

    fgt_g2t_auxil aux; 
    aux.Rlab=aux.Rloc=aux.Dloc=TVector3(0,0,0); aux.hitPtr=0; aux.iQuad=-1;// clear it

    aux.hitPtr=hitPtr;
    aux.Rloc=Rloc;
    aux.Dloc=Dloc;
    aux.Rlab=Rlab+ds/2.*verLab; // set it in the middle of the track segment

    aux.iQuad=iQuad;
    mG2tHitList[iDisc][iQuad].push_back(aux); // ID starts from 0, WMZ

    ntot++;

    //...... only QA is below .......
#ifdef __FGT_QA_HISTO__
    // QA  accpted hits
    hA[3]->Fill(10+5*iDisc);
    hA[3]->Fill(10+5*iDisc+iQuad+1);
    Double_t de_kev=hitPtr->de*1.e6;
    hA[0]->Fill(de_kev);
    hA[1]->Fill(hitPtr->ds);
    hA[2]->Fill(Rlab.z());
    hA[4]->Fill(Rlab.x(),Rlab.y());
    hA[6]->Fill(Rlab.Perp());
#endif    
  }// loop over hits
  //LOG_INFO<<Form("Unpacking g2t FGT %d hits --> accepted %d",nhits,ntot)<<endm;
}


//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void
StFgtSlowSimuMaker::projectQuad2strips( Int_t iDisc, Int_t iQuad ){

  Double_t totAmp=quadDigitizationXY->Integral();
  Double_t maxAmp=quadDigitizationXY->GetMaximum();
  Double_t cut_2DampCutoff= maxAmp/par_2DampCutoffScale;

  //LOG_INFO<<Form("::digiQuad(iDsc=%d, iQuad=%d)  totAmp=%g , maxAmp=%g(a.u.)",iQuad,  iDisc,totAmp,maxAmp)<<  endm;

  Int_t nPix0=0, nPix1=0;  
  Double_t sumAmp=0.,sumAmpAtten=0., sumAdcP=0., sumAdcR=0.; //for QA
  
  Int_t nx=quadDigitizationXY->GetNbinsX();
  Int_t ny=quadDigitizationXY->GetNbinsY();
  Int_t bx,by;
  for(bx=1;bx<=nx;bx++)
    for(by=1;by<=ny;by++) {
      Float_t   amp=quadDigitizationXY->GetBinContent(bx,by); // this is not double because histo if float
      if (amp < cut_2DampCutoff) continue;
      nPix0++;
      Double_t x=quadDigitizationXY->GetXaxis()->GetBinCenter(bx); // SLOW?
      Double_t y=quadDigitizationXY->GetYaxis()->GetBinCenter(by); // SLOW?
      Double_t ampAtten=amp*par_overalGain;
      if (ampAtten < cut_2DampCutoff) continue;
      nPix1++;
      
      Double_t r=sqrt(x*x+y*y);
      Double_t phi=atan2(y,x);
      Double_t binFrac; 
      
      Int_t iRadID=StFgtGeom::rad2LocalStripId(r,phi,&binFrac);
      if(iRadID<0) { 
	//printf("bad iRad xy->R, x=%f y=%f r=%f phi/deg=%f iRid=%d\n",x,y,r,phi/3.1416*180.,iRadID);
	continue;
      } 
      //jjassert(iRadID>=0 && iRadID<720);
      
      Int_t iPhiID=StFgtGeom::phi2LocalStripId(r,phi,&binFrac);
      if(iPhiID<0) { 
	//printf("bad iPhi xy->R, x=%f y=%f r=%f phi/deg=%f iPhiID=%d\n",x,y,r,phi/3.1416*180.,iPhiID);
	continue;
      }       
      //jjassert(iPhiID>=0 && iPhiID<720);

      //printf("x=%f y=%f r=%f phi/deg=%f iRid=%d iPhi=%d amp=%8.4f AmpAttrn=%8.4f > cut_2DampCutoff=%8.4f\n",
      //     x,y,r,phi/3.1416*180.,iRadID,iPhiID,amp,ampAtten,cut_2DampCutoff);
      
      Double_t adcP=    par_PplaneChargeFraction *ampAtten;       
      Double_t adcR=(1.-par_PplaneChargeFraction)*ampAtten;
      //printf(" iRad xy->R, x=%f y=%f r=%f phi/deg=%f iRid=%d strip=%d%cR%03d adc=%.2f\n",x,y,r,phi/3.1416*180.,iRadID , iDisc+1,iQuad+'A',iRadID,adcR);

      // sums are only for monitoring
      sumAmp+=amp;      sumAmpAtten+=ampAtten;
      sumAdcP+=adcP;      sumAdcR+=adcR;
    
      //accumulate response of individual strips
      quadDigitizationRad->Fill(iRadID,adcR);
      quadDigitizationPhi->Fill(iPhiID,adcP);

      //printf("DIG disc=%1d ix=%4d iy=%4d ir=%4d ip=%4d x=%8.4f y=%8.4f r=%8.4f p=%10.6f a=%8.4f %8.4f %8.4f\n",
      //	     iDisc,bx,by,iRadID,iPhiID,x,y,r,phi,ampAtten,adcR,adcP);
    
#ifdef __FGT_QA_HISTO__
      // monitoring, not cleared, may be dropped out to speed up the code marginally
      digRAll->Fill(iRadID,adcR);
      digPAll->Fill(iPhiID,adcP);
      digRadcAll->Fill(x,y,adcR); // for monitoring
      digPadcAll->Fill(x,y,adcP); // for monitoring
#endif      
    } //...... end of loop over quadrant 
  //printf("  fgt-digi: nPix0=%d   nPix1=%d,  sumAmp=%g,  sumAmpAtten=%g  ADC: sumP=%.3g sumR=%.3g\n", nPix0,nPix1,sumAmp,sumAmpAtten,sumAdcP,sumAdcR); 

#ifdef __FGT_QA_HISTO__  
  hA[29]->Fill(sumAdcR);
  hA[30]->Fill(sumAdcP);
  hA[31]->Fill(sumAdcP,sumAdcR);
#endif
} 
  
 
//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void
StFgtSlowSimuMaker::exportStripPlane2StEvent(TH1F *h, Int_t stripIdOffset,  StFgtStripCollection  *stripCollectionPtr){

  //jjassert(stripCollectionPtr); 
  //printf("write fgt strips ,   --> StEvent: #hits=%d on input\n",stripCollectionPtr->getNumStrips());

  Float_t   *adcPtr=h->GetArray();
  //adcPtr++; // root histo counts bins from 1 - incredible silly --> double accounting with 4 lines below... removing
  Int_t   nx=h->GetNbinsX(); //is 720
  Int_t nSeq=0;
  for(Int_t iId=0;iId<nx;iId++) {
    Double_t adc=adcPtr[iId+1];
    nSeq--;
    if(adc >par_stripThreshAdc ) nSeq=3; // adds few strips after every fired for continuity
    if( nSeq<=0) continue;
    Int_t geoId=stripIdOffset+iId;
    Int_t timebin=0;

    Int_t rdo, arm, apv, chan;
    fgtDb->getElecCoordFromGeoId( geoId, rdo, arm, apv, chan );
    //LOG_INFO << Form("geoId=%5d rdo=%1d arm=%1d apv=%2d ch=%3d",geoId, rdo, arm, apv, chan);
    
    Int_t elecId =  StFgtGeom::getElectIdFromElecCoord( rdo, arm, apv, chan );
    StFgtStrip* stripPtr = stripCollectionPtr->getStrip( elecId );

    Double_t ped=0;
    Double_t sigPed=0;
    Short_t stat=0;
    if( switch_addPeds ) {
      stat=fgtDb->getStatusFromElecCoord(rdo,arm,apv,chan);
      //if(stat) continue; // drop bad strips -> Temp removed status check until DB fixes.
      ped=fgtDb   ->getPedestalFromGeoId( geoId);
      sigPed=fgtDb->getPedestalSigmaFromGeoId( geoId);
      //LOG_INFO << Form(" ped=%6.1f sig=%6.1f stat=%1d",ped,sigPed,stat);
    }
    
    //LOG_INFO << " adc-ped=";
    float sum=0;
    double adc2;
    for(int iTbOff=0;iTbOff<7;iTbOff++)
      {
	if( switch_addPeds ) {
	  adc2=adc*pulseShape[iTbOff];
	  adc2+=mRnd->Gaus(ped,sigPed);
	  if(adc2<ped-3*sigPed) adc2=ped-3*sigPed;
	  if(adc2 <0 ) adc=0;
	  if(adc2 >4095 ) adc=4095;
	  sum+=adc2-ped;
	}
	stripPtr->setAdc( (Short_t)(adc2), timebin+iTbOff );
	//LOG_INFO << Form("%7.0f",adc2-ped);
      }
    stripPtr->setGeoId( geoId );
    stripPtr->setElecCoords(rdo,arm,apv,chan);
    //LOG_INFO << Form(" sum=%7.0f",sum) << endm;  
  }
}
    


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// $Log: StFgtSlowSimuMaker.cxx,v $
// Revision 1.4  2012/11/08 17:16:20  akio
// http://www.star.bnl.gov/~akio/fgt/mc/index.php#code
//
// - get fgtDb automatically without setting by hand
// - default changed to do pedestal (switch_addPeds=1 in constructor)
// - bug fix in logic flaw when creating timebin distribution causing "flat" hit
// - bug fix which causes a event to take forever when there is a hit but no signal in a quadrant
// - bug fix for cut_2DampCutoff becoming 0 for empty quadrant digitization, causing taking forever to finish
// - bug fix in exportStripPlane2StEvent() which account for ROOT histo first bin is at index=1 TWICE
//
// Following 2 chages are temp fix and need to be removed one DB is updated
// - Overwriting par_overalgain=2 from DB by 20
// - Do NOT do status check (DB entry for 2012Dec15 has status=1 -> need fix in DB)
//
// Revision 1.3  2012/06/20 18:32:40  avossen
// setting elec ids for strips now, implemented pulse shape over 7 timebins
//
// Revision 1.2  2012/06/18 20:41:37  balewski
// corrected crash on attampt to save not initializaed histos
//
// Revision 1.1  2012/06/06 20:35:09  jeromel
// Code  review closed (requested Anselm/Jan; reviewed Jonathan/Jason)
//
// Revision 1.47  2012/05/08 16:40:25  avossen
// prepare for review
//
// Revision 1.46  2012/04/05 19:04:10  balewski
// reduced smearing back to 0.035 mm
//
// Revision 1.45  2012/03/17 03:55:29  balewski
// *** empty log message ***
//
// Revision 1.44  2012/03/17 01:17:45  balewski
// *** empty log message ***
//
// Revision 1.43  2012/03/17 01:15:23  balewski
// wider spread of GEM signal, sigma is now 1mm
//
// Revision 1.42  2012/03/17 01:08:31  balewski
// works with Anselm's cluster finder
//
// Revision 1.41  2012/03/09 12:51:12  rfatemi
// removed references to old StFgtDb methods
//
// Revision 1.40  2012/03/07 15:23:53  sgliske
// StFgtStrip no longer has a type field
//
// Revision 1.39  2012/01/30 10:42:23  sgliske
// strip containers now contain adc values for
// all time bins.  Also fixed bug where setType modified the timebin
// rather than the type.
//
// Revision 1.38  2012/01/27 14:09:34  balewski
// switch to new consts
//
// Revision 1.37  2012/01/26 18:41:43  balewski
// fixing , new constants
//
// Revision 1.36  2012/01/25 21:57:38  balewski
// removing printDB map
//
// Revision 1.35  2012/01/20 17:30:01  balewski
// *** empty log message ***
//
// Revision 1.34  2012/01/18 19:43:00  balewski
// better DB dump
//
// Revision 1.33  2012/01/18 17:48:32  sgliske
// StEvent/StFgtStrip now contains rdo/arm/apv/channel
//
// Revision 1.32  2012/01/14 01:42:19  balewski
// more printouts
//
// Revision 1.31  2012/01/13 21:35:05  balewski
// added printing of FGT -DB-map
//
// Revision 1.30  2011/12/01 23:18:59  avossen
// changed dir of StFgtDb
//
// Revision 1.29  2011/12/01 21:57:47  balewski
// revert to Anselm's version and use StFgtDb
//
// Revision 1.28  2011/12/01 19:45:54  balewski
// *** empty log message ***
//
// Revision 1.27  2011/12/01 00:58:00  avossen
// changed the use of the naive maker to use of StFgtDb, replaced geom-> with StFgtGeom::
//
// Revision 1.26  2011/11/09 19:08:01  balewski
// *** empty log message ***
//
// Revision 1.25  2011/11/09 17:50:26  balewski
// working on phi-strip cluster
//
// Revision 1.24  2011/11/08 21:43:25  balewski
// testing P-strips
//
// Revision 1.23  2011/11/08 03:40:37  balewski
// added testing of xy->phi mapping
//
// Revision 1.22  2011/11/04 17:37:55  balewski
// *** empty log message ***
//
// Revision 1.21  2011/11/04 17:01:35  balewski
// added A2C to BFC
//
// Revision 1.20  2011/11/02 20:53:07  balewski
// slow simu works for 6 discs, onlt R-plane
//
// Revision 1.19  2011/11/01 22:00:24  balewski
// using fgt-strip-collection
//
// Revision 1.18  2011/11/01 18:53:45  sgliske
// Added ''#if 0'' and ''#endif'' to comment out use of older FGT containers.
// It now compiles, but needs to be updated for FGT containers, take 2.
// All changes include a comment with 'sgliske'.
//
// Revision 1.17  2011/10/27 19:29:17  balewski
// fixed strip indexing, R-cluster works
//
// Revision 1.16  2011/10/26 19:32:36  balewski
// now fgt-geom is owned by fgtDb-maker
//
// Revision 1.15  2011/10/26 17:02:14  balewski
// get fgt event the proper way
//
// Revision 1.14  2011/10/25 18:39:55  balewski
// StEvent is working, muDst not yet
//
// Revision 1.13  2011/10/20 22:33:24  balewski
// enable fgt-sub eve - it crashed BFC
//
// Revision 1.12  2011/10/20 17:30:57  balewski
// *** empty log message ***
//
// Revision 1.11  2011/10/17 21:39:56  balewski
// added temp interface to fgt cluster finder
//
// Revision 1.10  2011/10/13 21:02:34  balewski
// the R-strip projection works now
//
// Revision 1.9  2011/10/12 21:15:02  balewski
// half way
//
// Revision 1.8  2011/10/12 18:20:38  balewski
// after testing of R-strip ID mapping
//
// Revision 1.7  2011/10/07 19:45:33  balewski
// testing strip ID
//
// Revision 1.5  2011/10/06 19:41:45  balewski
// cleanup
//
// Revision 1.4  2011/10/06 19:05:56  balewski
// Elos table is now read in from STAR DB
//
// Revision 1.3  2011/10/05 18:04:33  balewski
// storing of FGT in StEvent is almost working
//
// Revision 1.2  2011/09/29 21:36:17  balewski
// now 2D distribution of charge & fiducial cuts are workimng properly
//
// Revision 1.1  2011/09/28 20:57:37  balewski
// merging private code
//
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


 


