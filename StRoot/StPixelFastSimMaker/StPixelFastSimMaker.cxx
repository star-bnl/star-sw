 /*
 * $Id: StPixelFastSimMaker.cxx,v 1.41 2009/01/26 14:50:46 fisyak Exp $
 *
 * Author: A. Rose, LBL, Y. Fisyak, BNL, M. Miller, MIT
 *
 * 
 **********************************************************
 * $Log: StPixelFastSimMaker.cxx,v $
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
  St_HitError *ist1TableSet = (St_HitError *)set->Find("ist1HitError");
  St_HitError *ist2TableSet = (St_HitError *)set->Find("ist2HitError");
  St_HitError *ist3TableSet = (St_HitError *)set->Find("ist3HitError");
  //St_HitError *pixelTableSet = (St_HitError *)set->Find("PixelHitError");
  //cout<<"found St_HitErrors"<<endl;
  HitError_st* ist1HitError = ist1TableSet->GetTable();
  resXIst1 = sqrt(ist1HitError->coeff[0]);
  resZIst1 = sqrt(ist1HitError->coeff[3]);
  HitError_st* ist2HitError = ist2TableSet->GetTable();
  resXIst2 = sqrt(ist2HitError->coeff[0]);
  resZIst2 = sqrt(ist2HitError->coeff[3]);
  HitError_st* ist3HitError = ist3TableSet->GetTable();
  resXIst3 = sqrt(ist3HitError->coeff[0]);
  resZIst3 = sqrt(ist3HitError->coeff[3]);
  //HitError_st* pixelHitError = pixelTableSet->GetTable();
  //resXPix = sqrt(pixelHitError->coeff[0]);
  //resZPix = sqrt(pixelHitError->coeff[3]);

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
  int nPixelPerWaferX=640;
  int nPixelPerWaferZ=640;
  int nWaferPerLadder=10;
  double pixelWidth=.003;
  float pixWaferHalf=.96;
  unsigned int nPixLadders[2]={9,24};
  //int pixels[6400][640];
  vector<int> pixels;
  vector<StMcPixelHit*> pixLadderHits;
  multimap<int,int> pixelToKey;
  int vid;
  /*
  for(int temp1=0;temp1<nPixelPerWaferX*nWaferPerLadder;temp1++){
    for(int temp2=0;temp2<nPixelPerWaferZ;temp2++){
      pixels[temp1][temp2]=0;
    }
  }
  */
  if (pixHitCol){
    if(pileup_on) AddPixPileUpHit(pixHitCol); //.. add the pileup hits into the collection

    Int_t nhits = pixHitCol->numberOfHits();
    LOG_DEBUG<<"There are "<<nhits<<" pixel hits"<<endm;
    if (nhits){
      Int_t id = col->numberOfHits();
      for (UInt_t k=0; k<pixHitCol->numberOfLayers(); k++){
	if (pixHitCol->layer(k)){
	  LOG_DEBUG<<"Layer "<<k+1<<endm;
	  for(unsigned int q=0;q<nPixLadders[k];q++){
	    UInt_t nh = pixHitCol->layer(k)->hits().size();
	    for (UInt_t i = 0; i < nh; i++){
	      StMcHit *mcH = pixHitCol->layer(k)->hits()[i];
	      StMcPixelHit* mcPix=dynamic_cast<StMcPixelHit*>(mcH);
	      if(mcPix->ladder()==q+1){
		pixLadderHits.push_back(mcPix);
		LOG_DEBUG<<"hit found in ladder "<<q+1<<endm;
	      }
	    }
	    TString Path("");
	    Path = Form("/HALL_1/CAVE_1/PXMO_1/PSEC_%i/PLMO_%i/PLAC_1",sector(k+1,q+1),secLadder(k+1,q+1));
	    LOG_DEBUG<<"Path: "<<Path<<endm;
	    gGeoManager->RestoreMasterVolume();
	    gGeoManager->CdTop();
	    gGeoManager->cd(Path);
	    for(unsigned int hh=0;hh<pixLadderHits.size();hh++){
	      StMcPixelHit* mcPixel=pixLadderHits[hh];
	      if (!mcPixel) continue;
	      if(hh==0) vid=mcPixel->volumeId();
	      LOG_DEBUG<<"pixel hit volume id: "<<mcPixel->volumeId()<<endm;
	      double pos[3]={mcPixel->position().x(),mcPixel->position().y(),mcPixel->position().z()};
	      LOG_DEBUG<<"original hit position: ("<<pos[0]<<","<<pos[1]<<","<<pos[2]<<")"<<endm;
	      double localpos[3]={0,0,0};
	      gGeoManager->GetCurrentMatrix()->MasterToLocal(pos,localpos);
	      LOG_DEBUG<<"transformed local x: "<<localpos[0]<<"; local z: "<<localpos[2]<<"; local y: "<<localpos[1]<<endm;
	      if(fabs(localpos[0])>pixWaferHalf){
		LOG_INFO<<"bad hit position: local x="<<mcPixel->position().x()<<endm;
		continue;
	      }
	      int xindex=static_cast<int>((localpos[0]+pixWaferHalf)/pixelWidth)+1;
	      LOG_DEBUG<<"localpos[0]: "<<localpos[0]<<"; pixelWidth: "<<pixelWidth<<"; xindex: "<<xindex<<endm;
	      int zindex=static_cast<int>((localpos[2]+pixWaferHalf*(nWaferPerLadder+1/2))/pixelWidth)+1;
	      LOG_DEBUG<<"localpos[2]: "<<localpos[2]<<"; pixelWidth: "<<pixelWidth<<"; zindex: "<<zindex<<endm;
	      //zindex=zindex+nWaferPerLadder*nPixelPerWaferZ/2;
	      //xindex=xindex+nPixelPerWaferX/2;
	      int pixdex=nWaferPerLadder*nPixelPerWaferX*(xindex-1)+zindex;
	      //pixels[xindex][zindex]=1;
	      pixels.push_back(pixdex);
	      pixelToKey.insert(std::pair<int,int>(pixdex,mcPixel->key()));
	      cout<<"added pixel-key pair "<<pixdex<<", "<<mcPixel->key()<<endl;
	      LOG_DEBUG<<"x, z and total index for pixel containing this hit (offset by 1) are x: "<<xindex<<"; z: "<<zindex<<"; total: "<<pixdex<<endm;
	      //StThreeVectorF pos(mcPixel->position());
	      //StThreeVectorF mom(mcH->localMomentum());
	      //shiftHit(pos, mom ,mcP->layer(), mcP->ladder());
	    }
	    StThreeVectorD mRndHitError(0.,0.,0.);
	    //smearGaus(mRndHitError, resXPix, resZPix);
	    /*
	      for(int t1=0;t1<nPixelPerWaferX*nWaferPerLadder;t1++){
	      for(int t2=0;t2<nPixelPerWaferZ;t2++){
	      if(pixels[t1][t2]){
	    */
	    for(unsigned int temp=0;temp<pixels.size();temp++){
	      //cout<<"pixel "<<t1<<","<<t2<<" is lit up"<<endm;
	      LOG_DEBUG<<"pixel "<<pixels[temp]<<" is lit up"<<endm;
	      int pixz=pixels[temp]%(nWaferPerLadder*nPixelPerWaferX);
	      int pixx=(pixels[temp]-pixz)/(nWaferPerLadder*nPixelPerWaferX)+1;
	      LOG_DEBUG<<"this corresponds to pixel x "<<pixx<<" and pixel z "<<pixz<<endm;
	      double flocalpos[3]={(pixx-nPixelPerWaferX/2+1/2)*pixelWidth,.0008,(pixz-nWaferPerLadder*nPixelPerWaferZ/2+1/2)*pixelWidth};
	      LOG_DEBUG<<"final local x: "<<flocalpos[0]<<"; local y: "<<flocalpos[1]<<"; local z: "<<flocalpos[2]<<endm;
	      double fpos[3]={0,0,0};
	      gGeoManager->GetCurrentMatrix()->LocalToMaster(flocalpos,fpos);
	      LOG_DEBUG<<"reconstructed hit position: ("<<fpos[0]<<","<<fpos[1]<<","<<fpos[2]<<")"<<endm;
	      StThreeVectorF fposv(fpos);
	      StRnDHit* tempHit = new StRnDHit(fposv, mRndHitError, 1, 1., 0, 1, 1, id++, kPxlId);
	      //cout <<"StPixelFastSimMaker::Make() -I- Pix Hit: "
	      //     <<*tempHit<<endl;
	      tempHit->setDetectorId(kPxlId);
	      tempHit->setVolumeId(vid);
	      tempHit->setLayer(k+1);
	      tempHit->setLadder(q+1);
	      //multimap<int,int>::iterator piter=pixelToKey.find(pixels[temp]);
	      std::pair<multimap<int,int>::iterator,multimap<int,int>::iterator> itpair=pixelToKey.equal_range(pixels[temp]);
	      LOG_DEBUG<<"there are "<<pixelToKey.count(pixels[temp])<<" pixel-key pairs containing the pixel "<<pixels[temp]<<endm;
	      int pixelcount=pixelToKey.count(pixels[temp]);
	      /*
	      if(piter!=pixelToKey.end()){
		tempHit->setKey((*piter).second);
		pixelToKey.erase(piter);
	      }
	      */
	      double topdE=0,sumdE=0;
	      int topHit=-999;
	      LOG_DEBUG<<"layer: "<<k+1<<"; ladder: "<<q+1<<endm;
	      LOG_DEBUG<<"pixel "<<pixels[temp]<<" is lit up"<<endm;
	      if(g2t_pix_hit){
		LOG_DEBUG<<"g2t_pix_hit found"<<endm;
		LOG_DEBUG<<"g2t_pix_hit number of rows: "<<g2t_pix_hit->GetNRows()<<endm;
		//for(multimap<int,int>::iterator iiit=itpair.first;iiit!=itpair.second;iiit++){
		multimap<int,int>::iterator current;
		for(int tem=0;tem<pixelcount;tem++){
		  //LOG_DEBUG<<"now dealing with pixel-key pair "<<(*iiit).first<<", "<<(*iiit).second<<endl;
		  current=pixelToKey.find(pixels[temp]);
		  LOG_DEBUG<<"now dealing with pixel-key pair "<<(*current).first<<", "<<(*current).second<<endm;
		  for(int ab=0;ab<g2t_pix_hit->GetNRows();ab++){
		    //if(g2tPix[ab].id==(*iiit).second){
		    if(g2tPix[ab].id==(*current).second){
		      LOG_DEBUG<<"g2tPix hit "<<ab<<" matches this pixel-key pair"<<endm;
		      LOG_DEBUG<<"this hit has energy "<<g2tPix[ab].de<<endm;
		      if(g2tPix[ab].de>topdE){
			topdE=g2tPix[ab].de;
			topHit=ab;
		      }
		      sumdE=sumdE+g2tPix[ab].de;
		    }
		  }
		  //pixelToKey.erase(iiit);
		  pixelToKey.erase(current);
		  LOG_DEBUG<<"topHit: "<<topHit<<"; sumdE: "<<sumdE<<endm;
		}
		LOG_DEBUG<<"done with looping through pixel-key pairs"<<endm;
		if(topHit!=-999){
		  int idTQual=static_cast<int>(topdE*100/sumdE);
		  tempHit->setIdTruth(g2tPix[topHit].track_p,idTQual);
		  tempHit->setKey(g2tPix[topHit].id);
		}
		else tempHit->setKey(99999);
	      }
	      else{
		tempHit->setKey(99999);
		LOG_DEBUG<<"g2t_pix_hit not found"<<endm;
	      }
	      col->addHit(tempHit);
	      //pixels[t1][t2]=0;
	      //}
	      //}
	    }
	    pixels.clear();
	    pixLadderHits.clear();
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
  int nLadders[2]={19,27};
  int nWafers[2]={10,13};
  double pitch=.006; 
  //note, all lengths in centimeters unless explicitly noted
  unsigned int nStrips=640;
  int nStripsComp=nStrips+1;
  unsigned int ladderCount;
  unsigned int waferCount;
  double icept;
  double sTotE;
  double pos[3];
  double localpos[3];
  double gpos[3];
  int id=0;
  double istWaferLow=-1.92;
	
  TString PathIn("");
  TString PathOut("");
  if(istHitCol){
    LOG_INFO<<"ist hit collection found"<<endm;
    int nIsthits=istHitCol->numberOfHits();
    LOG_DEBUG<<"there are "<<nIsthits<<" ist hits"<<endm;
    vector<StMcIstHit*> ladderHits;
    multimap<int, int> stripToKey;
    multimap<int, int> strip2ToKey;
    multimap<int, int> strip1ToKey;
    istStrip strips1[640];
    istStrip strips2[640];
    istStrip sStrips[1280];
    if(nIsthits){
      for(unsigned int i=0;i<2;i++){
	if(istHitCol->layer(i)){
	  ladderCount=1;
	  for(int jj=0;jj<nLadders[i];jj++){
	    LOG_DEBUG<<"now dealing with ladder "<<ladderCount<<endm;
	    waferCount=1;
	    for(unsigned int kk=0;kk<istHitCol->layer(i)->hits().size();kk++){
	      StMcHit *mcH = istHitCol->layer(i)->hits()[kk];
	      StMcIstHit *mcI = dynamic_cast<StMcIstHit*>(mcH); 
	      if(mcI->ladder()==ladderCount) ladderHits.push_back(mcI);
	    }
	    for(int ll=0;ll<nWafers[i];ll++){
	      LOG_DEBUG<<"now dealing with wafer "<<waferCount<<endm;
	      if(i+1==1) PathIn = Form("/HALL_1/CAVE_1/IBMO_1/IBMY_%i/IBAM_%i/IBLM_%i/IBSS_%i",i+1,ladderCount,waferCount,1);
	      else{
		PathIn = Form("HALL_1/CAVE_1/IBMO_1/IBMY:IBM1_%i/IBAM:IBA1_%i/IBLM:IBL1_%i/IBSS:IBS1_%i",i+1,ladderCount,waferCount,1);
		PathOut= Form("HALL_1/CAVE_1/IBMO_1/IBMY:IBM1_%i/IBAM:IBA1_%i/IBLM:IBL1_%i/IBSS:IBS1_%i",i+1,ladderCount,waferCount,2);
	      }
	      for(unsigned int nn=0;nn<ladderHits.size();nn++){
		if(ladderHits[nn]->wafer()==waferCount){
		  StMcIstHit* mcIw=ladderHits[nn];
		  pos[0]=mcIw->position().x();
		  pos[1]=mcIw->position().y();
		  pos[2]=mcIw->position().z();
		  localpos[0]=0;
		  localpos[1]=0;
		  localpos[2]=0;
		  gGeoManager->RestoreMasterVolume();
		  if(mcIw->side()==1){
		    gGeoManager->cd(PathIn);
		    LOG_DEBUG<<"pathIn: "<<PathIn<<endm;
		  }
		  if(mcIw->side()==2){
		    gGeoManager->cd(PathOut);
		    LOG_DEBUG<<"pathOut: "<<PathOut<<endm;
		  }
		  //TGeoNode* node=gGeoManager->GetCurrentNode();
		  gGeoManager->GetCurrentMatrix()->MasterToLocal(pos,localpos);
		  double x=localpos[0];
		  double z=localpos[2];
		  //note that in these local coordinates the strips give good resolution in the x coordinate in layer 1 and layer 2 side 1 and z in layer 2 side 2
		  if(fabs(z)<fabs(istWaferLow) && fabs(x)<fabs(istWaferLow)){
		    if(i==0){
		      LOG_DEBUG<<"layer 1: local x: "<<localpos[0]<<"; local y: "<<localpos[1]<<"; local z: "<<localpos[2]<<endm;
		      stripHit sh;
		      sh.localX=x;
		      sh.e=mcIw->dE();
		      int sindex;
		      sindex=static_cast<int>(x/pitch);
		      sindex=sindex+nStrips/2+1;
		      if(0<sindex && sindex<nStripsComp){
			if(z<0){
			  sStrips[sindex-1].stripHits.push_back(sh);
			}
			else{
			  sindex=sindex+nStrips;
			  sStrips[sindex-1].stripHits.push_back(sh);
			}
			stripToKey.insert(std::pair<int,int>(sindex,mcIw->key()));
												
		      }
		      else{ LOG_INFO<<"bad strip index! "<<sindex<<endm;}
		      LOG_DEBUG<<"stripHit created with local x value "<<localpos[0]<<" and e value "<<mcIw->dE()<<" and assigned to strip "<<sindex<<endm;
		    }
		    else{
		      LOG_DEBUG<<"layer 2 side "<<mcIw->side()<<"; local x: "<<localpos[0]<<"; local y: "<<localpos[1]<<"; local z: "<<localpos[2]<<endm;
		      if(mcIw->side()==2){
			stripHit sh;
			sh.localX=z;
			sh.e=mcIw->dE();
			int sindex; 
			sindex=static_cast<int>(z/pitch);
			sindex=sindex+nStrips/2+1;
			if(0<sindex && sindex<nStripsComp){
			  strips2[sindex-1].stripHits.push_back(sh);
			  strip2ToKey.insert(std::pair<int,int>(sindex,mcIw->key()));
			}
			else{ LOG_INFO<<"bad strip index! "<<sindex<<endm;}
			LOG_DEBUG<<"stripHit created with local z value "<<localpos[2]<<" and e value "<<mcIw->dE()<<" and assigned to strip "<<sindex<<endm;
		      }
		      if(mcIw->side()==1){
			stripHit sh;
			sh.localX=x;
			sh.e=mcIw->dE();
			int sindex;
			sindex=static_cast<int>(x/pitch);
			sindex=sindex+nStrips/2+1;
			if(0<sindex && sindex<nStripsComp){
			  strips1[sindex-1].stripHits.push_back(sh);
			  strip1ToKey.insert(std::pair<int,int>(sindex,mcIw->key()));
			}
			else{ LOG_INFO<<"bad strip index! "<<sindex<<endm;}
			LOG_DEBUG<<"stripHit created with local x value "<<localpos[0]<<" and e value "<<mcIw->dE()<<" and assigned to strip "<<sindex<<endm;
		      }											
		    }
		  }
		}
	      }
	      if(i==0){
		for(unsigned int oo=0;oo<nStrips*2;oo++){
		  icept=0;
		  sTotE=0;
		  if(sStrips[oo].stripHits.size()){
		    /*
		    for(unsigned int pp=0;pp<sStrips[oo].stripHits.size();pp++){
		      icept=icept+sStrips[oo].stripHits[pp].localX*sStrips[oo].stripHits[pp].e;
		      sTotE=sTotE+sStrips[oo].stripHits[pp].e;
		    }*/
		    //sStrips[oo].intercept=icept/sTotE;
		    sStrips[oo].intercept=istWaferLow+(oo+1/2)*pitch;
		    //double smearedX;
		    //smearedX=distortHit(sStrips[oo].intercept,0,100); //resXIst1,100);
		    gGeoManager->RestoreMasterVolume();
		    gGeoManager->cd(PathIn);
		    //TGeoNode* node=gGeoManager->GetCurrentNode();
		    //localpos[0]=smearedX;
		    localpos[0]=sStrips[oo].intercept;
		    if(oo>nStrips-1) localpos[2]=.96; //distortHit(.96,0,100); //resZIst1,100);
		    else localpos[2]=-.96; //distortHit(-.96,0,100); //resZIst1,100);
		    localpos[1]=-.0005;
		    LOG_DEBUG<<"final local x: "<<localpos[0]<<"; final local y: "<<localpos[1]<<" final local z: "<<localpos[2]<<endm;
		    LOG_DEBUG<<"layer ladder wafer: "<<i+1<<" "<<ladderCount<<" "<<waferCount<<endm;
		    LOG_DEBUG<<"path: "<<PathIn<<endm;
		    gpos[0]=0;
		    gpos[1]=0;
		    gpos[2]=0;
		    gGeoManager->GetCurrentMatrix()->LocalToMaster(localpos,gpos);
		    StThreeVectorF gposv(gpos);
		    StRnDHit* tempHit = new StRnDHit(gposv, mHitError, 1, 1., 0, 1, 1, id++, kIstId);  
		    tempHit->setDetectorId(kIstId); 
		    tempHit->setVolumeId(0);
		    /*multimap<int,int>::iterator iter=stripToKey.find(oo+1);
		    if(iter!=stripToKey.end()){
		      tempHit->setKey((*iter).second);
		      stripToKey.erase(iter);
		    }
		    */
		    std::pair<multimap<int,int>::iterator,multimap<int,int>::iterator> itpair1=stripToKey.equal_range(oo+1);
		    double topdE1=0,sumdE1=0;
		    int topHit1=-999;
		    if(g2t_ist_hit){
		      for(multimap<int,int>::iterator iiit1=itpair1.first;iiit1!=itpair1.second;iiit1++){
			for(int aa=0;aa<g2t_ist_hit->GetNRows();aa++){
			  if(g2tIst[aa].id==(*iiit1).second){
			    if(g2tIst[aa].de>topdE1){
			      topdE1=g2tIst[aa].de;
			      topHit1=aa;
			    }
			    sumdE1=sumdE1+g2tIst[aa].de;
			  }
			}
			stripToKey.erase(iiit1);
		      }
		      if(topHit1!=-999){
			int idTQual=static_cast<int>(topdE1*100/sumdE1);
			tempHit->setIdTruth(g2tIst[topHit1].track_p,idTQual);
			tempHit->setKey(g2tIst[topHit1].id);
		      }
		      else tempHit->setKey(99999);
		    }
		    else tempHit->setKey(99999);
		    tempHit->setLayer(i+1);           
		    tempHit->setLadder(ladderCount);           
		    tempHit->setWafer(waferCount);
		    tempHit->setExtraByte0(1);                                                                
		    col->addHit(tempHit);
		  }
		}
	      }
	      if(i==1){
		for(unsigned int o=0;o<nStrips;o++){
		  icept=0;
		  sTotE=0;
		  if(strips1[o].stripHits.size()){
		    /*
		    for(unsigned int p=0;p<strips1[o].stripHits.size();p++){
		      icept=icept+strips1[o].stripHits[p].localX*strips1[o].stripHits[p].e;
		      sTotE=sTotE+strips1[o].stripHits[p].e;
		    }
		    */
		    //strips1[o].intercept=icept/sTotE;
		    strips1[o].intercept=istWaferLow+(o+1/2)*pitch;
		    gGeoManager->RestoreMasterVolume();
		    gGeoManager->cd(PathIn);
		    //TGeoNode* node=gGeoManager->GetCurrentNode();
		    //localpos[0]=distortHit(strips1[o].intercept,0,100); //resXIst2,100);
		    localpos[0]=strips1[o].intercept;
		    //localpos[2]=distortHit(0,0,100); //resZIst2,100);
		    localpos[2]=0;
		    localpos[1]=.0005;
		    LOG_DEBUG<<"final local x: "<<localpos[0]<<"; final local y: "<<localpos[1]<<" final local z: "<<localpos[2]<<endm;
		    LOG_DEBUG<<"layer ladder wafer: "<<i+1<<" "<<ladderCount<<" "<<waferCount<<endm;
		    LOG_DEBUG<<"path: "<<PathIn<<endm;
		    gpos[0]=0;
		    gpos[1]=0;
		    gpos[2]=0;
		    gGeoManager->GetCurrentMatrix()->LocalToMaster(localpos,gpos);
		    StThreeVectorF gposv(gpos);
		    StRnDHit* tempHit = new StRnDHit(gposv, mHitError, 1, 1., 0, 1, 1, id++, kIstId);  
		    tempHit->setDetectorId(kIstId); 
		    tempHit->setVolumeId(0);
		    /*
		    multimap<int,int>::iterator iter=strip1ToKey.find(o+1);
		    if(iter!=strip1ToKey.end()){
		      tempHit->setKey((*iter).second);
		      strip1ToKey.erase(iter);
		    }
		    */
		    std::pair<multimap<int,int>::iterator,multimap<int,int>::iterator> itpair2=strip1ToKey.equal_range(o+1);
		    double topdE2=0,sumdE2=0;
		    int topHit2=-999;
		    if(g2t_ist_hit){
		      for(multimap<int,int>::iterator iiit2=itpair2.first;iiit2!=itpair2.second;iiit2++){
			for(int bb=0;bb<g2t_ist_hit->GetNRows();bb++){
			  if(g2tIst[bb].id==(*iiit2).second){
			    if(g2tIst[bb].de>topdE2){
			      topdE2=g2tIst[bb].de;
			      topHit2=bb;
			    }
			    sumdE2=sumdE2+g2tIst[bb].de;
			  }
			}
			strip1ToKey.erase(iiit2);
		      }
		      if(topHit2!=-999){
			int idTQual=static_cast<int>(topdE2*100/sumdE2);
			tempHit->setIdTruth(g2tIst[topHit2].track_p,idTQual);
			tempHit->setKey(g2tIst[topHit2].id);
		      }
		      else tempHit->setKey(99999);
		    }
		    else tempHit->setKey(99999);
		    tempHit->setLayer(i+1);           
		    tempHit->setLadder(ladderCount);           
		    tempHit->setWafer(waferCount);
		    tempHit->setExtraByte0(1);                                                                
		    col->addHit(tempHit);
		  }
		  icept=0;
		  sTotE=0;
		  if(strips2[o].stripHits.size()){
		    /*
		    for(unsigned int s=0;s<strips2[o].stripHits.size();s++){
		      icept=icept+strips2[o].stripHits[s].localX*strips2[o].stripHits[s].e;
		      sTotE=sTotE+strips2[o].stripHits[s].e;
		    }
		    */
		    //strips2[o].intercept=icept/sTotE;
		    strips2[o].intercept=istWaferLow+(o+1/2)*pitch;
		    gGeoManager->RestoreMasterVolume();
		    gGeoManager->cd(PathOut);
		    //TGeoNode* node=gGeoManager->GetCurrentNode();
		    //localpos[0]=distortHit(0,0,100); //resXIst3,100);
		    localpos[0]=0;
		    //localpos[2]=distortHit(strips2[o].intercept,0,100); //resZIst3,100);
		    localpos[2]=strips2[o].intercept;
		    localpos[1]=-.0005;
		    LOG_DEBUG<<"final local x: "<<localpos[0]<<"; final local y: "<<localpos[1]<<" final local z: "<<localpos[2]<<endm;
		    LOG_DEBUG<<"layer ladder wafer: "<<i+1<<" "<<ladderCount<<" "<<waferCount<<endm;
		    LOG_DEBUG<<"path: "<<PathOut<<endm;
		    gpos[0]=0;
		    gpos[1]=0;
		    gpos[2]=0;
		    gGeoManager->GetCurrentMatrix()->LocalToMaster(localpos,gpos);
		    StThreeVectorF gposv(gpos);
		    StRnDHit* tempHit2 = new StRnDHit(gposv, mHitError, 1, 1., 0, 1, 1, id++, kIstId);  
		    tempHit2->setDetectorId(kIstId); 
		    tempHit2->setVolumeId(0);
		    /*
		    multimap<int,int>::iterator iter=strip2ToKey.find(o+1);
		    if(iter!=strip2ToKey.end()){
		      tempHit2->setKey((*iter).second);
		      strip2ToKey.erase(iter);
		    }
		    */
		    std::pair<multimap<int,int>::iterator,multimap<int,int>::iterator> itpair3=strip2ToKey.equal_range(o+1);
		    double topdE3=0,sumdE3=0;
		    int topHit3=-999;
		    if(g2t_ist_hit){
		      for(multimap<int,int>::iterator iiit3=itpair3.first;iiit3!=itpair3.second;iiit3++){
			for(int ba=0;ba<g2t_ist_hit->GetNRows();ba++){
			  if(g2tIst[ba].id==(*iiit3).second){
			    if(g2tIst[ba].de>topdE3){
			      topdE3=g2tIst[ba].de;
			      topHit3=ba;
			    }
			    sumdE3=sumdE3+g2tIst[ba].de;
			  }
			}
			strip2ToKey.erase(iiit3);
		      }
		      if(topHit3!=-999){
			int idTQual=static_cast<int>(topdE3*100/sumdE3);
			tempHit2->setIdTruth(g2tIst[topHit3].track_p,idTQual);
			tempHit2->setKey(g2tIst[topHit3].id);
		      }
		      else tempHit2->setKey(99999);
		    }
		    else tempHit2->setKey(99999);
		    tempHit2->setLayer(i+1);           
		    tempHit2->setLadder(ladderCount);           
		    tempHit2->setWafer(waferCount);
		    tempHit2->setExtraByte0(2);                                                                
		    col->addHit(tempHit2);
		  }
		}
	      }
	      waferCount++;
	      for(unsigned int kl=0;kl<nStrips*2;kl++){
		if(kl<nStrips){
		  strips1[kl].stripHits.clear();
		  strips2[kl].stripHits.clear();
		}
		sStrips[kl].stripHits.clear();
	      }
	      stripToKey.clear();
	      strip2ToKey.clear();
	      strip1ToKey.clear();
	    }
	    ladderCount++;
	    ladderHits.clear();
	  }
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
		    tempHit->setExtraByte0(mcI->side());
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
