/*
 *
 * Author: A. Rose, LBL, Y. Fisyak, BNL, M. Miller, MIT
 *
 * 
 **********************************************************
 * $Log: StPixelFastSimMaker.cxx,v $
 * Revision 1.44  2012/12/18 18:46:59  margetis
 * update for DEV13 geometry
 *
 * Revision 1.43  2010/09/01 20:31:47  fisyak
 * clean up unused varaibles
 *
 * Revision 1.42  2009/02/06 20:48:48  wleight
 * UPGR15 Update
 *
 * Revision 1.41  2009/01/26 14:50:46  fisyak
 * Clean up
 *
 * Revision 1.40  2007/12/03 20:42:43  wleight
 * Replaced couts with LOG_DEBUGs
 *
 * Revision 1.37  2007/11/13 19:09:51  wleight
 * Corrected bug causing pixel fast simulator to crash when there were no pixel and/or ist hits in the event
 *
 * Revision 1.36  2007/11/06 16:20:06  wleight
 * Digitized Pixel, removed all hit smearing, and implemented idTruth
 *
 * Revision 1.34  2007/10/18 16:31:44  fisyak
 * Add pile-up from weixie
 *
 * Revision 1.33  2007/10/18 14:25:13  didenko
 * updates for pile-up events
 * 
 * Revision 1.32  2007/10/16 19:53:08  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 1.31  2007/10/16 19:50:46  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 1.30  2007/09/09 17:00:32  fisyak
 * Fix bug 1056
 *
 * Revision 1.29  2007/09/08 00:33:05  andrewar
 * Modifications for pileup hit read in.
 *
 * Revision 1.28  2007/05/17 13:18:52  andrewar
 * Removed cout in shiftHit.
 *
 * Revision 1.27  2007/05/16 15:06:55  andrewar
 * Switched cout's to LOG_INFO.
 *
 * Revision 1.26  2007/04/28 17:56:36  perev
 * Redundant StChain.h removed
 *
 * Revision 1.25  2007/04/27 18:41:29  wleight
 * Removed smearing of the coordinate not controlled by the strips in the 17cm layer
 *
 * Revision 1.24  2007/04/27 14:59:10  wleight
 * Corrected another error in the creation of new hits
 *
 * Revision 1.23  2007/04/26 04:08:41  perev
 * Remove StBFChain dependency
 *
 * Revision 1.22  2007/04/25 17:44:59  wleight
 * Corrected error in assignment of reconstructed IST hits
 *
 * Revision 1.21  2007/04/23 18:11:30  andrewar
 * Removed references to Hpd (includes were obsolete)
 *
 * Revision 1.19  2007/04/23 16:32:47  wleight
 * Added explicit casting for double to int in calculating strip number
 *
 * Revision 1.18  2007/04/22 22:57:23  wleight
 * The two hits in the 17 cm layer are no longer combined into 1
 *
 * Revision 1.17  2007/04/16 19:10:52  wleight
 * Added IST simulation (digitization but no clustering)
 *
 * Revision 1.16  2007/04/13 19:17:15  andrewar
 * Removed misleading errors. Changed cout and printf to gMessMgr.
 *
 * Revision 1.15  2007/04/06 21:46:36  andrewar
 * Removed some debug messages.
 *
 * Revision 1.14  2007/04/06 14:55:11  andrewar
 * Shift of HFT hit to face of ladder.
 *
 * Revision 1.13  2007/03/28 13:33:45  mmiller
 * Removed cout/printf's.
 *
 * Revision 1.12  2006/12/21 18:11:59  wleight
 * Fixed UPGR09 compatibility so it works with all versions
 *
 * Revision 1.11  2006/12/20 16:50:21  wleight
 * Added fix for UPGR09 problem with layer number mismatch
 *
 * Revision 1.10  2006/12/15 02:17:20  wleight
 * Ist now gets hit smearing parameters from the database
 *
 * Revision 1.9  2006/12/14 23:52:51  andrewar
 * Added Sevil's hit error db loader.
 *
 * Revision 1.7  2006/11/29 21:42:07  andrewar
 * Update with Pixel resolution smearing.
 *
 * Revision 1.6  2006/11/28 22:37:42  wleight
 * Fixed minor smearing bug
 *
 * Revision 1.4  2006/10/13 20:15:45  fisyak
 * Add Hpd fast simulation (Sevil)
 *
 * Revision 1.3  2006/02/17 21:44:29  andrewar
 * Remover streaming of each Pixel hit.
 *
 * Revision 1.2  2006/02/08 20:57:33  fisyak
 * Set proper Detector Id
 *
 * Revision 1.1  2006/02/03 20:11:56  fisyak
 * The initial revision
 *
 *
 */

#include "Stiostream.h"
#include "StPixelFastSimMaker.h"
#include "StHit.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "StRnDHit.h"
#include "StMcEvent.hh"
#include "StMcHit.hh"
#include "StMcIstHit.hh"

#include "StMcPixelHit.hh"
#include "StMcEventTypes.hh"
#ifdef bug1056
#include "Sti/Base/Factory.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiDetectorBuilder.h"
#endif
#include <stdio.h>
#include <map>
#include <exception>
using namespace std;
#include <stdexcept>
#include "tables/St_g2t_ist_hit_Table.h"
#include "tables/St_g2t_pix_hit_Table.h"
#include "tables/St_HitError_Table.h"
#ifdef bug1056
#include "Sti/StiHitErrorCalculator.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/StiElossCalculator.h"
#include "Sti/StiVMCToolKit.h"
#else
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#endif
#include "StarClassLibrary/StRandom.hh"
#include "tables/St_HitError_Table.h"
#include <fstream>
#include "TFile.h"
#include "TTree.h"

ClassImp(StPixelFastSimMaker)

  
  StPixelFastSimMaker::~StPixelFastSimMaker(){ /*noop*/ }

int StPixelFastSimMaker::Init()
{
  LOG_INFO<<"StPixelFastSimMaker::Init()"<<endm;
  int seed=time(NULL);
  myRandom=new StRandom();
  myRandom->setSeed(seed);

 
  mSmear=1;
  return kStOk;
}

//____________________________________________________________
int StPixelFastSimMaker::InitRun(int RunNo)
{
  LOG_INFO<<"StPixelFastSimMaker::InitRun"<<endm;

  TDataSet *set = GetDataBase("Calibrations/tracker");
  St_HitError *istTableSet = (St_HitError *)set->Find("ist1HitError");
  St_HitError *pixelTableSet = (St_HitError *)set->Find("PixelHitError");
  //cout<<"found St_HitErrors"<<endl;
  HitError_st* istHitError = istTableSet->GetTable();
  resXIst1 = sqrt(istHitError->coeff[0]);
  resZIst1 = sqrt(istHitError->coeff[3]);
  HitError_st* pixelHitError = pixelTableSet->GetTable();
  resXPix = sqrt(pixelHitError->coeff[0]);
  resZPix = sqrt(pixelHitError->coeff[3]);

  LoadPixPileUpHits(); //.. load the pile up hits for PIXEL
  //cout<<"done with StPixelFastSimMaker::InitRun"<<endl;

  return kStOk;
}

//___________________________
void StPixelFastSimMaker::LoadPixPileUpHits()
{
  LOG_INFO<<"+++ loading the PIXEL pileup files +++"<<endm;

  pileup_on = true;

  TFile f_pileup("pileup.root");
  if (f_pileup.IsZombie()) {

    pileup_on = false;

    LOG_INFO << "no PIXEL pileup file found. Will run with regular setup" << endm;
    return;
  }

  LOG_INFO<<"+++ Loaded pileup.root for PIXEL pileup simulation +++"<<endm;

  TTree* pileup_tree = (TTree*)f_pileup.Get("pileup_tree");

  const int maxhit = 200000;
  float x[maxhit], y[maxhit], z[maxhit], px[maxhit], py[maxhit], pz[maxhit], de[maxhit], ds[maxhit];
  long key[maxhit], vid[maxhit];
  int layer[maxhit], nhits;

  TBranch *b_x = pileup_tree->GetBranch("x");
  TBranch *b_y = pileup_tree->GetBranch("y");
  TBranch *b_z = pileup_tree->GetBranch("z");
  TBranch *b_px = pileup_tree->GetBranch("px");
  TBranch *b_py = pileup_tree->GetBranch("py");
  TBranch *b_pz = pileup_tree->GetBranch("pz");
  TBranch *b_de = pileup_tree->GetBranch("de");
  TBranch *b_ds = pileup_tree->GetBranch("ds");
  TBranch *b_key = pileup_tree->GetBranch("key");
  TBranch *b_vid = pileup_tree->GetBranch("vid");
  TBranch *b_layer = pileup_tree->GetBranch("layer");
  TBranch *b_nhits = pileup_tree->GetBranch("nhits");
  b_x->SetAddress(x);
  b_y->SetAddress(y);
  b_z->SetAddress(z);
  b_px->SetAddress(px);
  b_py->SetAddress(py);
  b_pz->SetAddress(pz);
  b_de->SetAddress(de);
  b_ds->SetAddress(de);
  b_key->SetAddress(key);
  b_vid->SetAddress(vid);
  b_layer->SetAddress(layer);
  b_nhits->SetAddress(&nhits);

  pileup_tree->GetEntry(0); //.. just one events

  for(int ihit = 0; ihit<nhits; ihit++) {
    pileup_x.push_back(x[ihit]);
    pileup_y.push_back(y[ihit]);
    pileup_z.push_back(z[ihit]);

    pileup_px.push_back(px[ihit]);
    pileup_py.push_back(py[ihit]);
    pileup_pz.push_back(pz[ihit]);

    pileup_key.push_back(key[ihit]);
    pileup_vid.push_back(vid[ihit]);

    pileup_de.push_back(de[ihit]);
    pileup_ds.push_back(ds[ihit]);
  }
}
//____________________________________________________________
void StPixelFastSimMaker::AddPixPileUpHit(StMcPixelHitCollection* pixHitCol)
{
  for(unsigned int i = 0; i<pileup_x.size(); i++) {

    StThreeVectorD pos(pileup_x[i], pileup_y[i], pileup_z[i]);
    StThreeVectorF mom(pileup_px[i], pileup_py[i], pileup_pz[i]);

    float de = pileup_de[i]; 
    float ds = pileup_ds[i];

    int key = pileup_key[i];
    int vid = pileup_vid[i];

    StMcPixelHit* pixhit = new StMcPixelHit(pos, mom, de, ds, key, vid, 0);
    pixHitCol->addHit(pixhit);
  }
}
//____________________________________________________________

Int_t StPixelFastSimMaker::Make()
{
  LOG_INFO<<"StPixelFastSimMaker::Make()"<<endm;

  // Get the input data structures from StEvent and StMcEvent
  StEvent* rcEvent =  (StEvent*) GetInputDS("StEvent");
  if (! rcEvent) {LOG_INFO << "No StEvent on input" << endl; return kStWarn;}
  StMcEvent* mcEvent = (StMcEvent *) GetInputDS("StMcEvent");
  if (! mcEvent) {LOG_INFO << "No StMcEvent on input" << endl; return kStWarn;}
  TDataSetIter geant(GetInputDS("geant"));
  if (! gGeoManager) GetDataBase("VmcGeometry");
  g2t_ist_hit_st* g2tIst=0;
  g2t_pix_hit_st* g2tPix=0;
  St_g2t_ist_hit *g2t_ist_hit=(St_g2t_ist_hit *)geant("g2t_ist_hit");
  St_g2t_pix_hit *g2t_pix_hit=(St_g2t_pix_hit *)geant("g2t_pix_hit");
  if(g2t_ist_hit) g2tIst=g2t_ist_hit->GetTable();
  if(g2t_pix_hit) g2tPix=g2t_pix_hit->GetTable();

  // Store hits into RnD Hit Collection until we have our own
  StRnDHitCollection *col = new StRnDHitCollection;
  if (!col )
    {
      gMessMgr->Info()<<"StPixelFastSimMaker -E- no RnDHitCollection!\n";
      abort();
    }

  // Don't use realistic hit errors for now. When we transit to smeared
  // hits, this would be a good place to store offset info
  StThreeVectorF mHitError(0.,0.,0.);

  //Get MC Pixel hit collection. This contains all pixel hits.
  StMcPixelHitCollection* pixHitCol = mcEvent->pixelHitCollection();
  unsigned int nPixLadders[2]={9,24};
  //int pixels[6400][640];
  vector<int> pixels;
  vector<StMcPixelHit*> pixLadderHits;
  multimap<int,int> pixelToKey;
  float smearedX,smearedZ;

  if (pixHitCol){
    int counter_layer1 = 0;
    int counter_layer2 = 0;
    int counter        = 0;
    float rad          = 0;
    if(pileup_on) AddPixPileUpHit(pixHitCol); //.. add the pileup hits into the collection
    
    Int_t nhits = pixHitCol->numberOfHits();
    LOG_DEBUG<<"There are "<<nhits<<" pixel hits"<<endm;
    if (nhits){
      Int_t id = col->numberOfHits();
      if(g2tPix){
	for(int jj=0;jj<g2t_pix_hit->GetTableSize();jj++)
	  {
	    rad  = TMath::Sqrt(g2tPix[jj].x[0]*g2tPix[jj].x[0] + g2tPix[jj].x[1]*g2tPix[jj].x[1]);  
	    if((rad>2.2) && (rad<3.0)) {
	      counter_layer1++;
	      LOG_DEBUG <<"radius="<<rad<<" id="<<g2tPix[jj].id <<" x="<<g2tPix[jj].x[0]<<" y="<<g2tPix[jj].x[1]<<" z="<<g2tPix[jj].x[2]<<" truth is "<<g2tPix[jj].track_p<<" dE= "<<g2tPix[jj].de <<" counter layer 1 =" << counter_layer1 << endm;
	    }
	    else if((rad>7.9) && (rad<8.5)) {
	      counter_layer2++;
	      LOG_DEBUG <<"radius="<<rad<<" id="<<g2tPix[jj].id <<" x="<<g2tPix[jj].x[0]<<" y="<<g2tPix[jj].x[1]<<" z="<<g2tPix[jj].x[2]<<" truth is "<<g2tPix[jj].track_p<<" dE= "<<g2tPix[jj].de <<" counter layer 2 =" << counter_layer2 <<endm;
	    }
	  }
      }
      LOG_DEBUG << " there is " << counter_layer1 << " hits in the first layer " << endm;
      LOG_DEBUG << " there is " << counter_layer2 << " hits in the second layer " << endm;
      LOG_DEBUG <<" # of sectors : " << pixHitCol->numberOfLayers() << endm;
      //for(int k=0;k<g2t_pix_hit->GetNRows();k++){
      for (UInt_t k=0; k<pixHitCol->numberOfLayers(); k++){
	if (pixHitCol->layer(k)){
	  LOG_DEBUG<<"Sector "<<k+1<<endm;
	  //simple simulator for perfect hits that just converts StMcPixelHit to StRnDHit
	  //as of 11/21/08, hits are now smeared with resolution taken from hit error table
	  UInt_t nh = pixHitCol->layer(k)->hits().size();
	  LOG_DEBUG << " Number of hits in sector "<< k+1 <<" =" << nh << endm;
	  for (UInt_t i = 0; i < nh; i++){
	    counter++;
	    int vid=g2tPix[k].volume_id;
	    int layer=vid/1000000;
	    int ladder=(vid%1000000)/10000;
	    StMcHit *mcH = pixHitCol->layer(k)->hits()[i];
	    StMcPixelHit* mcPix=dynamic_cast<StMcPixelHit*>(mcH);
	    TString Path("");
	    //if(k==0) Path= Form("/HALL_1/CAVE_1/PXMO_1/PXLA_1/PLMI_%i/PLAC_1",mcPix->ladder());
	    //else Path=Form("/HALL_1/CAVE_1/PXMO_1/PXL1_2/PLM1_%i/PLA1_1",mcPix->ladder());
	    Path= Form("/HALL_1/CAVE_1/IDSM_1/PXMO_1/PXLA_%i/LADR_%i/PLAC_1",layer,ladder);
	    LOG_DEBUG<<"mc pixel hit location x: "<<g2tPix[k].x[0]<<"; y: "<<g2tPix[k].x[1]<<"; z: "<<g2tPix[k].x[2]<<endm;
	    LOG_DEBUG <<" PATH: " << Path << endm;
	    LOG_DEBUG<<"pixel hit layer/ladder is "<<layer<<"/"<<ladder<<" and volume id "<<vid<<endm;
	    gGeoManager->RestoreMasterVolume();
	    gGeoManager->CdTop();
	    gGeoManager->cd(Path);
	    //double globalPixHitPos[3]={g2tPix[k].x[0],g2tPix[k].x[1],g2tPix[k].x[2]};
	    double globalPixHitPos[3]={mcPix->position().x(),mcPix->position().y(),mcPix->position().z()};
	    double localPixHitPos[3]={0,0,0};
	    gGeoManager->GetCurrentMatrix()->MasterToLocal(globalPixHitPos,localPixHitPos);
	    smearedX=distortHit(localPixHitPos[0],resXPix,100);
	    smearedZ=distortHit(localPixHitPos[2],resZPix,100);
	    localPixHitPos[0]=smearedX;
	    localPixHitPos[2]=smearedZ;
	    double smearedGlobalPixHitPos[3]={0,0,0};
	    gGeoManager->GetCurrentMatrix()->LocalToMaster(localPixHitPos,smearedGlobalPixHitPos);
	    StThreeVectorF gpixpos(smearedGlobalPixHitPos);
	    StThreeVectorD mRndHitError(0.,0.,0.);
	    //StRnDHit* tempHit = new StRnDHit(mcPix->position(), mRndHitError, 1, 1., 0, 1, 1, id++, kPxlId);
	    StRnDHit* tempHit = new StRnDHit(gpixpos, mRndHitError, 1, 1., 0, 1, 1, id++, kPxlId);
	    tempHit->setVolumeId(mcPix->volumeId());
	    tempHit->setLayer(k+1);
	    tempHit->setLadder(mcPix->ladder());
	    tempHit->setKey(mcPix->key());
	    Int_t truth =0;
	    if((k==0)&&(counter<=counter_layer1)) {
	      truth = g2tPix[mcPix->key()-1].track_p;
	    }
	    else if((k==1)&&(counter<=(counter_layer2+pixHitCol->layer(0)->hits().size())))
	      {
		truth = g2tPix[mcPix->key()-1].track_p;
	      }
	    tempHit->setIdTruth(truth,100);
	    LOG_DEBUG<<"key() : "<< mcPix->key()-1 << " idTruth: "<< truth <<endm;
	    LOG_DEBUG <<"from g2t : x= " << g2tPix[k].x[0] <<"  y= " << g2tPix[k].x[1] <<"  z= " << g2tPix[k].x[2] << endm;
	    LOG_DEBUG<<"pixel rnd hit location x: "<<tempHit->position().x()<<"; y: "<<tempHit->position().y()<<"; z: "<<tempHit->position().z()<<endm;
LOG_DEBUG<<"pixel rnd hit location x: "<<tempHit->position().x()<<"; y: "<<tempHit->position().y()<<"; z: "<<tempHit->position().z()<<endm;
	    col->addHit(tempHit);
	  }
	}
      }
    }
    gMessMgr->Info() <<"StPixelFastSimMaker::Make() -I- Loaded "<<nhits<<"pixel hits. \n";
  }
  else{
    gMessMgr->Info() <<"No pixel hits found.\n";
  }
  
  
  const StMcIstHitCollection* istHitCol = mcEvent->istHitCollection();
  
  //new simulator for new 1-layer design
  int id=0;
  //float smearedX,smearedZ;
  TString Path("");
  if(istHitCol){
    LOG_INFO<<"ist hit collection found"<<endm;
    int nIsthits=istHitCol->numberOfHits();
    LOG_DEBUG<<"there are "<<nIsthits<<" ist hits"<<endm;
    vector<StMcIstHit*> ladderHits;
    if(nIsthits){
      if(istHitCol->layer(0)){
	//simple simulator for perfect hits that merely converts from StMcIstHit to StRnDHit
	//as of 11/21/08, the simulator now smears hits by hit resolutions taken from hit error tables
	for(unsigned int kk=0;kk<istHitCol->layer(0)->hits().size();kk++){
	  StMcHit* mcH=istHitCol->layer(0)->hits()[kk];
	  StMcIstHit* mcI=dynamic_cast<StMcIstHit*>(mcH);
	  LOG_DEBUG<<"mc ist hit location x: "<<mcI->position().x()<<"; y: "<<mcI->position().y()<<"; z: "<<mcI->position().z()<<endm;
	  TString Path("");
	  Path=Form("/HALL_1/CAVE_1/IDSM_1/IBMO_1/IBAM_%i/IBLM_%i/IBSS_1",mcI->ladder(),mcI->wafer());
	  gGeoManager->RestoreMasterVolume();
	  gGeoManager->CdTop();
	  gGeoManager->cd(Path);
	  double globalIstHitPos[3]={mcI->position().x(),mcI->position().y(),mcI->position().z()};
	  double localIstHitPos[3]={0,0,0};
	  gGeoManager->GetCurrentMatrix()->MasterToLocal(globalIstHitPos,localIstHitPos);
	  smearedX=distortHit(localIstHitPos[0],resXIst1,100);
	  smearedZ=distortHit(localIstHitPos[2],resZIst1,100);
	  localIstHitPos[0]=smearedX;
	  localIstHitPos[2]=smearedZ;
	  double smearedGlobalIstHitPos[3]={0,0,0};
	  gGeoManager->GetCurrentMatrix()->LocalToMaster(localIstHitPos,smearedGlobalIstHitPos);
	  StThreeVectorF gistpos(smearedGlobalIstHitPos);
	  StRnDHit* tempHit = new StRnDHit(gistpos, mHitError, 1, 1., 0, 1, 1, id++, kIstId);  
	  tempHit->setDetectorId(kIstId); 
	  tempHit->setVolumeId(mcI->volumeId());
	  tempHit->setKey(mcI->key());
	  tempHit->setLayer(mcI->layer());           
	  tempHit->setLadder(mcI->ladder());           
	  tempHit->setWafer(mcI->wafer());
	  tempHit->setIdTruth(g2tIst[kk].track_p,100);
	  LOG_DEBUG<<"ist hit volume id: "<<tempHit->volumeId()<<endm;
	  LOG_DEBUG<<"ist hit ladder/wafer: "<<tempHit->ladder()<<"/"<<tempHit->wafer()<<endm;
	  col->addHit(tempHit);
	  LOG_DEBUG<<"ist rnd hit location x: "<<tempHit->position().x()<<"; y: "<<tempHit->position().y()<<"; z: "<<tempHit->position().z()<<" idTruth = " << g2tIst[kk].track_p << endm;
	}
      }
    }
  }
  else{
    LOG_INFO <<"No Ist hits found."<<endm;
  }
  
  
  const StMcFgtHitCollection* fgtHitCol = mcEvent->fgtHitCollection();
  if (fgtHitCol)
    {
      Int_t nhits = fgtHitCol->numberOfHits();
      if (nhits)
	{
	  Int_t id = 0;
	  //StSPtrVecHit *cont = new StSPtrVecHit();
	  //rcEvent->addHitCollection(cont, # Name );
	  for (UInt_t k=0; k<fgtHitCol->numberOfLayers(); k++){
	    if (fgtHitCol->layer(k))
	      {
		UInt_t nh = fgtHitCol->layer(k)->hits().size();
		for (UInt_t i = 0; i < nh; i++) {
		  StMcHit *mcH = fgtHitCol->layer(k)->hits()[i];
		  StRnDHit* tempHit = new StRnDHit(mcH->position(), mHitError, 1, 1., 0, 1, 1, id++);
		  tempHit->setVolumeId(mcH->volumeId());
		  tempHit->setKey(mcH->key());

		  StMcIstHit *mcI = dynamic_cast<StMcIstHit*>(mcH);
		  if(mcI){
		    tempHit->setLayer(mcI->layer());
		    tempHit->setLadder(mcI->ladder());
		    tempHit->setWafer(mcI->wafer());
		    //tempHit->setExtraByte0(mcI->side());
		  }
		  col->addHit(tempHit);
		}
	      }
	  }
	}
    }
  
  rcEvent->setRnDHitCollection(col);
  return kStOK;
    }
//________________________________________________________________________________
Bool_t StPixelFastSimMaker::accept(StEvent* event){
  return event ? true : false;
}
//________________________________________________________________________________
Bool_t StPixelFastSimMaker::accept(StMcEvent* event){
  return event ? true : false;
}

double StPixelFastSimMaker::distortHit(double x, double res, double detLength){
  double test;
  if(mSmear){
    test = x + myRandom->gauss(0,res);
    while( fabs(test) > detLength){
      test = x + myRandom->gauss(0,res);
    }
    //cout << " x was " <<x<< " and is now " << test<< endl;
    return test;
  }
  else return x;
}
//____________________________________________________________


//____________________________________________________________
void StPixelFastSimMaker::smearGaus(StThreeVectorD &mError, 
                                    double sigma1, double sigma2)
{

  // smear hit in transverse plane, 
  // sigma's are in microns
  double u1=-1;
  double u2=-1.; 
  double v1=-1.;
  double v2=-1.;;
  double r = 2.;
  double z1 = 10.;
  double z2 = 10.;
  while(fabs(z1)>2. || fabs(z2)>2.) // sigma
    {
      r = 2.;
      while(r>1.)
	{
	  u1 = rand()/double(RAND_MAX); 
	  u2 = rand()/double(RAND_MAX);
	  v1 = 2*u1 - 1.;  
	  v2 = 2*u2 - 1.; 
	  r = pow(v1,2) + pow(v2,2);
	}
      z1 = v1*sqrt(-2.*log(r)/r);  z2 = v2*sqrt(-2.*log(r)/r);
    }

  //set to be cumulative with other transforms 
  mError.setX(mError.x()+z1*sigma1/1.e04);
  mError.setY(mError.y()+z1*sigma1/1.e04);
  mError.setZ(mError.z()+z2*sigma2/1.e04);
}

int StPixelFastSimMaker::sector(int layer, int ladder)
{
 
  if (layer ==1)
    {
      if ( ladder < 4 ) return 1;
      if ( ladder < 7 ) return 2;
      return 3;
    }
  else
    {
      if ( ladder < 9 ) return 1;
      if ( ladder < 17 ) return 2;
      return 3;
    }

  
}

int StPixelFastSimMaker::secLadder(int layer, int ladder)
{
  if (layer==1)
    return ladder - 3*(sector(layer,ladder)-1);
  else
    return ladder - 8*(sector(layer,ladder)-1)+3;
}



double StPixelFastSimMaker::phiForLadder(int layer, int ladder)
{
  int sec = sector(layer,ladder);
  int secLad = secLadder(layer,ladder);
  //double phi=0.;
  double secPhi=0.;
  double ladPhi=0.;
  switch (sec)
    {
    case 1:
      secPhi=0.;
      break;
    case 2:
      secPhi=120.;
      break;
    case 3:
      secPhi=240.;
      break;
    }

  switch (secLad)
    {
    case 1:
      ladPhi=100.;
      break;
    case 2:
      ladPhi=60.;
      break;
    case 3:
      ladPhi=20.;
      break;
    case 4:
      ladPhi=105.;
      break;
    case 5:
      ladPhi=90.;
      break;
    case 6:
      ladPhi=75.;
      break;
    case 7:
      ladPhi=60.;
      break;
    case 8:
      ladPhi=45.;
      break;
    case 9:
      ladPhi=30.;
      break;
    case 10:
      ladPhi=15.;
      break;
    case 11:
      ladPhi=0.;
      break;
    }

  return secPhi+ladPhi;
}

 
void StPixelFastSimMaker::shiftHit(StThreeVectorF &position,StThreeVectorF &mom, int layer, int ladder)
{

  //printf("Entering hit shift code. %i %i\n",
  // sector(layer,ladder), secLadder(layer,ladder));
  LOG_DEBUG<<"Pixel Hit in layer "<<layer<<" and ladder "<<ladder<<" or sector "<<sector(layer,ladder)<<" and sector ladder "<<secLadder(layer,ladder)<<endm;

  TString Path("");
  Path = Form("/HALL_1/CAVE_1/PXMO_1/PSEC_%i/PLMO_%i/PLAC_1",
	      sector(layer,ladder),secLadder(layer,ladder));
  gGeoManager->RestoreMasterVolume();
  //printf("Master volume.\n");
 
  gGeoManager->CdTop();
  gGeoManager->cd(Path);
  TGeoPhysicalNode* node= (TGeoPhysicalNode*)(gGeoManager->GetCurrentNode());
  if (!node ) LOG_ERROR<< "Failed to get node for sector(layer,ladder), secLadder(layer, ladder)"<<endm;
 
  double pos[3]={position.x(),position.y(),position.z()};
  double localpos[3]={0,0,0};
  gGeoManager->GetCurrentMatrix()->MasterToLocal(pos,localpos);
  //printf("old hit: %g %g %g\n",localpos[0],localpos[1],localpos[2]);
 
  //get ladder phi 
  //TGeoHMatrix  *hmat   = (TGeoHMatrix*)(node->GetMatrix());
  TGeoHMatrix  *hmat   =  gGeoManager->GetCurrentMatrix();
 
  if (! hmat )
    {
      LOG_ERROR<< "Can't shift hit - no hmat."<<endm;
    }
  
  Double_t     *rot    = hmat->GetRotationMatrix();
  if (! rot )
    {
      LOG_ERROR <<"Can't shift hit - no rotation matrix."<<endm;
    }
 
  StThreeVectorD normalVector(rot[1],rot[4],rot[7]);
  //double momentum = mom.magnitude();
  StThreeVectorF momUnit(mom);
  momUnit/=momUnit.magnitude();
 
  //momentum mag, pt and z stay the same; angle changes 
  momUnit.setPhi( momUnit.phi() - phiForLadder(layer,ladder)*3.141592654/180.);
  //printf("Mom dir: %g\n",momUnit.phi());
  
  // shift = x + y * tan(phi)
  //.006 is the half thickness of the active area. Hardcoded; not good; blah, blah.
  double dx = (.006)*(momUnit.y()/momUnit.x());
  double dz = (.006)*(momUnit.y()/momUnit.z());
  localpos[0] = localpos[0] - dx;
  localpos[2] = localpos[2] - dz;
  localpos[1] = localpos[1] - .006; //this isn't exactly right. The local position is just off of radius -.006, but it's close (~1-5 um).
   
  gGeoManager->GetCurrentMatrix()->LocalToMaster(localpos,pos);

  //printf("exit shift code.\n");

}
