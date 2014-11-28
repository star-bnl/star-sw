//#Based Qinghua's StLamPhiMaker.cxx, I add the lambda tree("tlam") to store the information of lambda reconstruction. The production is a nano root file with some naive cuts. It is used for further analysis.
//#define __USE_FIT__
 
#include "StTriggerIdCollection.h"
#include "StTriggerId.h"
#include "TNtuple.h"
#include "TH1.h"
#include "TH2.h"
#include "TChain.h"
#include "TSystem.h"
#include "TFile.h"
#include "TMemStat.h"
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include "StarClassLibrary/SystemOfUnits.h"
#include "StarClassLibrary/StHelix.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
#include "StEmcTriggerMaker/StEmcTriggerMaker.h"
#include "StEmcTriggerMaker/StBemcTrigger.h"
#include "StV0sMaker.h"
#include "StBichsel/Bichsel.h"
#include "StMuDSTMaker/COMMON/StMuHelix.h"
#include "TMath.h"
ClassImp(StV0sMaker);
ClassImp(StLambdaMaker);
ClassImp(StAntilamMaker);
ClassImp(StK0sMaker);
#if 0
//________________________________________________________________________________
Double_t StV0sMaker::TpcnSigma(StMuTrack *gTrack, Double_t mass, Double_t charge) {
  Double_t z = -999.;
  if (gTrack && gTrack->probPidTraits().dEdxTrackLength() > 0 ) {
    const StMuHelix &mh = gTrack->muHelix();
    Double_t momentum  = mh.p().mag();
    Double_t log2dX = gTrack->probPidTraits().log2dX();
    if (log2dX <= 0) log2dX = 1;
    //    Double_t dedx_expected = 1.e-6*Bichsel::Instance()->GetI70M(log10(momentum/mass),log2dX);
#ifndef __USE_FIT__
    //    Double_t dedx_expected = 1.e-6*charge*charge*Bichsel::Instance()->GetI70M(TMath::Log10(momentum*TMath::Abs(charge)/mass),log2dX); 
    Double_t dedx_measured = gTrack->probPidTraits().dEdxTruncated();
    Double_t dedx_expected = 1.e-6*charge*charge*Bichsel::Instance()->GetI70M(TMath::Log10(momentum*TMath::Abs(charge)/mass)); 
    Double_t dedx_resolution = gTrack->probPidTraits().dEdxErrorTruncated();
#else
    Double_t dedx_measured = gTrack->probPidTraits().dEdxFit();
    Double_t dedx_expected = 1.e-6*charge*charge*TMath::Exp(Bichsel::Instance()->GetMostProbableZ(TMath::Log10(momentum*TMath::Abs(charge)/mass)));
    Double_t dedx_resolution = gTrack->probPidTraits().dEdxErrorFit();
#endif
    if (dedx_resolution > 0)
      z = TMath::Log(dedx_measured/dedx_expected)/dedx_resolution;
  }
  return z;
}
#endif
//________________________________________________________________________________
void StV0sMaker::InitFile(void){   
  // creating lambda nano ROOT file name
  TString FileName(outName);
  cout << GetName() << ":  output file: " << FileName << endl;
  // open udst file 
  m_outfile = new TFile(FileName,"recreate");
  // m_outfile->SetCompressionLevel(1);
  picoTree->SetDirectory(m_outfile);
  
  Hpdedx->SetDirectory(m_outfile);
  Hgdedx->SetDirectory(m_outfile);
  Hptrack->SetDirectory(m_outfile);
  Hgtrack->SetDirectory(m_outfile);
  Hpeta->SetDirectory(m_outfile);
  Hgeta->SetDirectory(m_outfile);
  Hppt->SetDirectory(m_outfile);
  Hgpt->SetDirectory(m_outfile);
  Hphit->SetDirectory(m_outfile);
  Hghit->SetDirectory(m_outfile);
  Hpdca->SetDirectory(m_outfile);
  Hgdca->SetDirectory(m_outfile);
}
//________________________________________________________________________________
Int_t StV0sMaker::Init(){ 
  //declear the lambda tree
  picoTree = new TTree("picoTree","Reconst V0");

  //event's information  
  picoTree->Branch("runID",&runID,"runID/I");
  picoTree->Branch("eventID",&eventID,"eventID/I");
  picoTree->Branch("eventNum",&eventNum,"eventNum/I");
  picoTree->Branch("bunchID",&bunchID,"bunchID/I");
  picoTree->Branch("spin",&spin,"spin/I");
  picoTree->Branch("spinbit",&spinbit,"spinbit/I");
  picoTree->Branch("spinbit8",&spinbit8,"spinbit8/I");
  picoTree->Branch("bx7",&bx7,"bx7/I");
  picoTree->Branch("bx48",&bx48,"bx48/I");
  
  picoTree->Branch("magn",&magn,"magn/F");
  picoTree->Branch("bbcrate",&bbcrate,"bbcrate/F");
  picoTree->Branch("zdcrate",&zdcrate,"zdcrate/F");
  picoTree->Branch("bbcTimebin",&bbcTimebin,"bbcTimebin/I");
  picoTree->Branch("pvx",&pvx,"pvx/F");
  picoTree->Branch("pvy",&pvy,"pvy/F");
  picoTree->Branch("pvz",&pvz,"pvz/F");
  
  picoTree->Branch("numtrig",&numtrig,"numtrig/I");
  picoTree->Branch("trigger",trigger,"trigger[numtrig]/I");  
  
  picoTree->Branch("npv",&npv,"npv/I");
  picoTree->Branch("ngtra",&ngtra,"ngtra/I");
  picoTree->Branch("nptra",&nptra,"nptra/I");
  picoTree->Branch("nlam",&nlam,"nlam/I");
    
  //Vertex's information
  picoTree->Branch("pvrank",pvrank,"pvrank[npv]/I");
  picoTree->Branch("npvx",npvx,"npvx[npv]/F");
  picoTree->Branch("npvy",npvy,"npvy[npv]/F");
  picoTree->Branch("npvz",npvz,"npvz[npv]/F");
  
  //p track's information   
  picoTree->Branch("phit",phit,"phit[nlam]/I");
  picoTree->Branch("phitpos",phitpos,"phitpos[nlam]/I");
  picoTree->Branch("pdca",pdca,"pdca[nlam]/F");
  picoTree->Branch("psigma",psigma,"psigma[nlam]/F");
  picoTree->Branch("pdedx",pdedx,"pdedx[nlam]/F");
  picoTree->Branch("ppx",ppx,"ppx[nlam]/F");
  picoTree->Branch("ppy",ppy,"ppy[nlam]/F");
  picoTree->Branch("ppz",ppz,"ppz[nlam]/F");
  picoTree->Branch("ptrackpx",ptrackpx,"ptrackpx[nlam]/F");
  picoTree->Branch("ptrackpy",ptrackpy,"ptrackpy[nlam]/F");
  picoTree->Branch("ptrackpz",ptrackpz,"ptrackpz[nlam]/F");
  picoTree->Branch("pLength",pLength,"pLength[nlam]/F");
  picoTree->Branch("pMom",pMom,"pMom[nlam]/F");
  
  //pi track's information   
  picoTree->Branch("pihit",pihit,"pihit[nlam]/I");
  picoTree->Branch("pihitpos",pihitpos,"pihitpos[nlam]/I");
  picoTree->Branch("pidca",pidca,"pidca[nlam]/F");
  picoTree->Branch("pisigma",pisigma,"pisigma[nlam]/F");
  picoTree->Branch("pidedx",pidedx,"pidedx[nlam]/F");
  picoTree->Branch("pipx",pipx,"pipx[nlam]/F");
  picoTree->Branch("pipy",pipy,"pipy[nlam]/F");
  picoTree->Branch("pipz",pipz,"pipz[nlam]/F");
  picoTree->Branch("pitrackpx",pitrackpx,"pitrackpx[nlam]/F");
  picoTree->Branch("pitrackpy",pitrackpy,"pitrackpy[nlam]/F");
  picoTree->Branch("pitrackpz",pitrackpz,"pitrackpz[nlam]/F");
  picoTree->Branch("piLength",piLength,"piLength[nlam]/F");
  picoTree->Branch("piMom",piMom,"piMom[nlam]/F");
  
  //lambda candidate's information   
  picoTree->Branch("mass",mass,"mass[nlam]/F");
  picoTree->Branch("dca2",dca2,"dca2[nlam]/F");
  picoTree->Branch("dcaV0",dcaV0,"dcaV0[nlam]/F");
  picoTree->Branch("length",length,"length[nlam]/F");
  picoTree->Branch("vleng",vleng,"vleng[nlam]/F");
  picoTree->Branch("V0x",V0x,"V0x[nlam]/F");
  picoTree->Branch("V0y",V0y,"V0y[nlam]/F");
  picoTree->Branch("V0z",V0z,"V0z[nlam]/F");
   
  //declear histogram 
  Hpdedx = new TH2D("Hpdedx","Hpdedx",100,0,10,120,0,12);
  Hgdedx = new TH2D("Hgdedx","Hgdedx",100,0,10,120,0,12);
  Hptrack = new TH1D("Hptrack","Hptrack",200,0,200);
  Hgtrack = new TH1D("Hgtrack","Hgtrack",1000,0,1000);
  Hpeta = new TH1D("Hpeta","Hpeta",80,-2,2);
  Hgeta = new TH1D("Hgeta","Hgeta",80,-2,2);
  Hppt = new TH1D("Hppt","Hppt",200,0,20);
  Hgpt = new TH1D("Hgpt","Hgpt",200,0,20);
  Hphit = new TH1D("Hphit","Hphit",50,0,50);
  Hghit = new TH1D("Hghit","Hghit",50,0,50);
  Hpdca = new TH1D("Hpdca","Hpdca",200,0,20);
  Hgdca = new TH1D("Hgdca","Hgdca",200,0,20);
  
  // assert the StSpinDbMaker
  spDb = (StSpinDbMaker*)GetMaker("spinDb");
  assert(spDb);

  // assert the StEmcTriggerMaker
  //trgMaker=(StEmcTriggerMaker*)GetMaker("StEmcTriggerMaker");
  //assert(trgMaker);
  
  InitFile();
  return StMaker::Init();
}

Int_t StV0sMaker::Make(){
  //p track's information  
  int vphit,vphitpos;
  float vpdca,vpsigma,vpdedx;
  
  //pi track's information
  int vpihit,vpihitpos;
  float vpidca,vpisigma,vpidedx;
  
  //lambda candidate's information
  float vmass,vdca2,vlength,vvleng,vdcaV0;
   
  //initialize for variables 
  runID=-1000;eventID=-1000;eventNum=-1000;
  bunchID=-1000;spin=-1000;spinbit=-1000;spinbit8=-1000;bx7=-1000;bx48=-1000;
  magn=-1000;
  bbcrate=-1000;zdcrate=-1000;
  bbcTimebin=-1000;
  
  pvx=-1000;pvy=-1000;pvz=-1000;
  npv=-1000;ngtra=-1000;nptra=-1000;
  nlam=-1000;
  
  numtrig=-1000;
  
  for(int i=0;i<kMaxLam;i++){
    trigger[i]=1000;
    
    pvrank[i]=-1000;
    npvx[i]=-1000;
    npvy[i]=-1000;
    npvz[i]=-1000;
    
    phit[i]=-1000;
    phitpos[i]=-1000;
    pdca[i]=-1000;
    psigma[i]=-1000;
    pdedx[i]=-1000;
    ppx[i]=-1000;
    ppy[i]=-1000;
    ppz[i]=-1000;
    ptrackpx[i]=-1000;
    ptrackpy[i]=-1000;
    ptrackpz[i]=-1000;
    pLength[i]=-1000;
    pMom[i]=-1000;
    
    pihit[i]=-1000;
    pihitpos[i]=-1000;
    pidca[i]=-1000;
    pisigma[i]=-1000;
    pidedx[i]=-1000;
    pipx[i]=-1000;
    pipy[i]=-1000;
    pipz[i]=-1000;
    pitrackpx[i]=-1000;
    pitrackpy[i]=-1000;
    pitrackpz[i]=-1000;
    piLength[i]=-1000;
    piMom[i]=-1000;
    
    mass[i]=-1000;
    dca2[i]=-1000;
    length[i]=-1000;
    vleng[i]=-1000;
    dcaV0[i]=-1000;
    V0x[i]=-1000;
    V0y[i]=-1000;
    V0z[i]=-1000;    
    V0px[i]=-1000;
    V0py[i]=-1000;
    V0pz[i]=-1000;
  }
  cout <<" Start " << GetName() << " :: "<< GetName() <<endl;
  
  if(muDstMaker != NULL) {
    mu = muDstMaker->muDst();
  }
  else{
    cout<< GetName() << " error!"<<endl;
    abort();
  }
  
  StMuEvent* ev=mu->event();
  StThreeVectorD posVert=ev->primaryVertexPosition();
  pvz = posVert.z();
  pvy = posVert.y();
  pvx = posVert.x();
  magn = ev->magneticField();
  
  eventNum=ev->eventNumber();
  eventID=ev->eventId();
  runID=ev->runNumber();
  StL0Trigger & spintrig= ev->l0Trigger();
  bunchID = spintrig.bunchCrossingId7bit(runID);
  bx48=spintrig.bunchCrossingId();
  bx7=spintrig.bunchCrossingId7bit(runID);
  
  spin = 0;
  spinbit = spintrig.spinBits(runID);
  spinbit8 = spintrig.spinBits(runID);
  /*
  if(spDb->isPolDirLong()) spin = 1;
  if(spDb->isPolDirTrans()) spin = 2;
  if( spDb->isMaskedUsingBX48(bx48)){
    spinbit=-1;  
    spinbit8=-1;
    //drop if bXing is masked out in DB
    //return kStSkip; 
  }
  else {
    //make sure scaler boards were in sync 
    //assert(spDb->offsetBX48minusBX7(bx48,bx7)==0); 
    spinbit=spDb->spin4usingBX7(bx7);
    spinbit8=spDb->spin8usingBX7(bx7);
  }
  */
  
  StRunInfo &runinfo =  ev->runInfo();
    
  bbcrate = runinfo.bbcCoincidenceRate();
  zdcrate = runinfo.zdcCoincidenceRate();
  
  StBbcTriggerDetector& bbc=ev->bbcTriggerDetector();
  bbcTimebin=bbc.onlineTimeDifference()/32;
  
  //get the triggerId;
  StTriggerId nominal = ev->triggerIdCollection().nominal();
  numtrig = nominal.maxTriggerIds();
  for(int i=0;i<numtrig;i++){
    trigger[i]=nominal.triggerId(i);
  }
  
  //trigger=0;
  if(nominal.isTrigger(380209)) trigger[0]=1;
  if(nominal.isTrigger(380305)) trigger[1]=1;
  if(nominal.isTrigger(370601)) trigger[0]=1;
  if(nominal.isTrigger(370611)) trigger[1]=1;
  if(nominal.isTrigger(370621)) trigger[2]=1;
  //  if(trigger[0]+trigger[1]+trigger[2]==0) return kStSkip;
  npv = mu->numberOfPrimaryVertices();
  for(int l=0;l<npv;l++){
      StMuPrimaryVertex* mupv = mu->primaryVertex(l);
      StThreeVectorD posV = mupv->position();
      pvrank[l] = mupv->ranking();
      npvx[l] = posV.x();
      npvy[l] = posV.y();
      npvz[l] = posV.z();
  }
  
  if(fabs(posVert.z())<100.0 && npv>0){
    int np = mu->primaryTracks()->GetEntries();
    int ng = mu->globalTracks()->GetEntries();
    Hptrack->Fill(np);
    Hgtrack->Fill(ng);
    for(int i=0;i<np;i++){
      StMuTrack* mup=mu->primaryTracks(i);
#ifndef __USE_FIT__
      Hpdedx->Fill(mup->momentum().mag(),mup->dEdx()*1E+6);
#else
      Hpdedx->Fill(mup->momentum().mag(),mup->probPidTraits().dEdxFit()*1E+6);
#endif
      Hpeta->Fill(mup->eta());
      Hppt->Fill(mup->pt());
      Hphit->Fill(mup->nHitsFit());
      Hpdca->Fill(mup->dca().mag());      
    }
    for(int i=0;i<ng;i++){
      StMuTrack* mug=mu->globalTracks(i);
#ifndef __USE_FIT__
      Hgdedx->Fill(mug->momentum().mag(),mug->dEdx()*1E+6);
#else
      Hgdedx->Fill(mug->momentum().mag(),mug->probPidTraits().dEdxFit()*1E+6);
#endif
      Hgeta->Fill(mug->eta());
      Hgpt->Fill(mug->pt());
      Hghit->Fill(mug->nHitsFit());
      Hgdca->Fill(mug->dca().mag());      
    }
  }
  
  nptra = mu->numberOfPrimaryTracks();
  if(nptra<1) return kStSkip;  
  ngtra = mu->numberOfGlobalTracks();
  nlam = 0;
  for (int l=0; l<ngtra; l++){   
    StMuTrack* mul=mu->globalTracks(l);
    if(mType == kLambda && mul->charge()<0)continue;	
    if(mType == kAntiLambda && mul->charge()>0)continue;	
    if(mul->flag()<0)continue;
    vphit = mul->nHitsFit();
    vphitpos = mul->nHitsPoss();
    vpdca = mul->dca().mag();
    if (mType == kLambda || mType == kAntiLambda) vpsigma  = mul->dEdxPullProtonFit(); //nSigmaProton(mul);     //    vpsigma = mul->nSigmaProton();
    else                                          vpisigma = mul->dEdxPullPionFit();   //nSigmaPion(mul);      //    vpsigma = mul->nSigmaPion();
#ifndef __USE_FIT__
    vpdedx = mul->dEdx()*1E+6;
#else
    vpdedx = mul->probPidTraits().dEdxFit()*1E+6;
#endif
    if(vphit<15)continue;
    if(fabs(vpsigma)>5.0)continue;
    StPhysicalHelixD helix_proton=mul->helix();
    StThreeVectorD   pp1=mul->momentum();
    for(int m=0; m<ngtra; m++){
      if(m==l)continue;
      StMuTrack* mum=mu->globalTracks(m);
      if(mType == kLambda && mum->charge()>0)continue;	
      if(mType == kAntiLambda && mum->charge()<0)continue;	
      if(mum->flag()<0)continue;
      vpihit = mum->nHitsFit();
      vpihitpos = mum->nHitsPoss();
      vpidca = mum->dca().mag();
      //      vpisigma = mum->nSigmaPion();
      vpisigma = mum->dEdxPullPionFit(); // nSigmaPion(mum);
#ifndef __USE_FIT__
      vpidedx = mum->dEdx()*1E+6;
#else
      vpidedx = mum->probPidTraits().dEdxFit()*1E+6;
#endif
      if(vpihit<15)continue;
      if(fabs(vpisigma)>5.0)continue;
      StPhysicalHelixD helix_pion=mum->helix();
      StThreeVectorD pp2=mum->momentum();
      
      StThreeVectorD pLambda8=pp1+pp2;
      double Eproton8;
      if (mType == kLambda || mType == kAntiLambda)  Eproton8=sqrt(0.93827*0.93827+pp1.mag2());
      else                                           Eproton8=sqrt(0.13957*0.13957+pp1.mag2());
      double Epion8=sqrt(0.13957*0.13957+pp2.mag2());
      double invartmass8=sqrt((Eproton8+Epion8)*(Eproton8+Epion8)-pLambda8.mag2());
      if (mType == kLambda || mType == kAntiLambda) {
	if(invartmass8>1.2)continue;
	if(invartmass8<1.07)continue;
      } else {
	if(invartmass8>0.65)continue;
	if(invartmass8<0.35)continue;
      }      
      double s1= helix_proton.pathLengths(helix_pion).first;
      double s2= helix_proton.pathLengths(helix_pion).second;
      StThreeVectorD V0=(helix_proton.at(s1)+helix_pion.at(s2))/2.0;
      StThreeVectorD V0mp=V0-posVert;//V0_distance to Primary Vetex
      StThreeVectorD vdca=helix_proton.at(s1)-helix_pion.at(s2);
      vdca2=vdca.mag();
      if(vdca2>2.0)continue;
      vvleng=V0mp.perp();
      vlength=V0mp.mag();  
      if (mType == kLambda || mType == kAntiLambda) {
	if(vlength<2.0)continue;
      } else {
	if(vlength<0.5)continue;
      }
      StThreeVectorD p1=helix_proton.momentumAt(s1,magn*kilogauss);
      StThreeVectorD p2=helix_pion.momentumAt(s2,magn*kilogauss);
      StThreeVectorD pLambda=p1+p2;
      if(pLambda.mag()<0.00001)continue; 
      double dot=pLambda.x()*V0mp.x()+pLambda.y()*V0mp.y()+pLambda.z()*V0mp.z();
      vdcaV0=sqrt(V0mp.mag2()-dot*dot/pLambda.mag2());
      double Eproton;
      if (mType == kLambda || mType == kAntiLambda) {
	if(vdcaV0>3.0)continue;
	Eproton=sqrt(0.93827*0.93827+p1.mag2());
      } else {
	if(vdcaV0>4.0)continue;
	Eproton=sqrt(0.13957*0.13957+p1.mag2());
      }
      double Epion=sqrt(0.13957*0.13957+p2.mag2());
      vmass = sqrt((Eproton+Epion)*(Eproton+Epion)-pLambda.mag2());
      if (mType == kLambda || mType == kAntiLambda) {
	if(vmass>1.16)continue;
	if(vmass<1.08)continue;
      } else {
	if(vmass>0.6)continue;
	if(vmass<0.4)continue;
      }
      
      phit[nlam]=vphit;
      phitpos[nlam]=vphitpos;
      pdca[nlam]=vpdca;
      psigma[nlam]=vpsigma;
      pdedx[nlam]=vpdedx;
      ppx[nlam]=p1.x();
      ppy[nlam]=p1.y();
      ppz[nlam]=p1.z();
      ptrackpx[nlam]=pp1.x();
      ptrackpy[nlam]=pp1.y();
      ptrackpz[nlam]=pp1.z();
      pLength[nlam]=mul->probPidTraits().dEdxTrackLength();
      pMom[nlam] = p1.mag();
      pihit[nlam]=vpihit;
      pihitpos[nlam]=vpihitpos;
      pidca[nlam]=vpidca;
      pisigma[nlam]=vpisigma;
      pidedx[nlam]=vpidedx;
      pipx[nlam]=p2.x();
      pipy[nlam]=p2.y();
      pipz[nlam]=p2.z();
      pitrackpx[nlam]=pp2.x();
      pitrackpy[nlam]=pp2.y();
      pitrackpz[nlam]=pp2.z();
      piLength[nlam]=mum->probPidTraits().dEdxTrackLength();
      piMom[nlam] = p2.mag();
	
      mass[nlam]=vmass;
      dca2[nlam]=vdca2;
      length[nlam]=vlength;
      vleng[nlam]=vvleng;
      dcaV0[nlam]=vdcaV0;
      V0x[nlam]=V0.x();
      V0y[nlam]=V0.y();
      V0z[nlam]=V0.z();
      
      nlam++;	
    }    
  }
  picoTree->Fill();
  return kStOk;
}

void StV0sMaker::FinishFile(void)
{
    //close file
    m_outfile->Write();
    m_outfile->Close();
    delete m_outfile;
}

Int_t StV0sMaker::Finish()
{
    FinishFile();
    cout << "=================================================================\n";
    cout << GetName() << " is finished" << endl;
    cout << "=================================================================\n";
    StMaker::Finish();
    return kStOK;
}












