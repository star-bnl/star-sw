/***************************************************************************
 *
 *  StSvtAlignmentMaker.cxx
 *
 * Author: Helen Caines
 ***************************************************************************
 *
 * Description: Interface to SVT & SSD alignment code
 *
 ***************************************************************************/

#include <stdio.h>
#include <fstream.h>
#include "StMessMgr.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "TString.h"

#include "StSvtAlignmentMaker.hh"
#include "StSsdAlign.hh"

#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "tables/St_scs_spt_Table.h"
#include "tables/St_sgr_groups_Table.h"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_srs_srspar_Table.h"
#include "tables/St_svg_shape_Table.h"


#include "TFile.h"
//#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
//#include <TGraphErrors.h>
//#include <TPostScript.h>

ClassImp(StSvtAlignmentMaker)

//___________________________________________________________________________
  StSvtAlignmentMaker::StSvtAlignmentMaker(const char *name):StMaker(name){

}
//____________________________________________________________________________
StSvtAlignmentMaker::~StSvtAlignmentMaker()
{
}
//____________________________________________________________________________
Int_t StSvtAlignmentMaker::Init()
{
  if(Debug()) gMessMgr->Debug() << "In StSvtAlignmentMaker::Init() ..."
				<< GetName() <<endm;
  
  gMessMgr->Info()<<"StSvtAlignmentMaker : Init START"<<endm; 
  
  DataType = 1;
  NumberEvents =  30;
  NumberOfEventsSoFar = 0;
  gMessMgr->Info()<<"StSvtAlignmentMaker : Datatype = " << DataType <<endm;
  
  St_DataSet *dataSet = GetDataSet("StSvtConfig");
  if (dataSet){
    setConfig((StSvtConfig*)(dataSet->GetObject()));
  }
  else{
    dataSet = new St_ObjectSet("StSvtConfig");
    AddConst(dataSet);  
    mConfig = new StSvtConfig();
    setConfig("FULL");
    mConfig->setConfiguration(mConfigString.Data());
    dataSet->SetObject((TObject*)mConfig);
  }
  
  mCoordTransform =  new StSvtCoordinateTransform();
  
  St_svg_geom* SvtGeom=0;
  St_DataSetIter local(GetInputDB("svt"));
  
  if ( !strncmp(mConfigString.Data(), "Y1L", strlen("Y1L")) )
    SvtGeom  = (St_svg_geom    *) local("svgpars/geomy1l");
  else
    SvtGeom = (St_svg_geom    *) local("svgpars/geom");
  
  mSvtGeomT  = (svg_geom_st*)SvtGeom->GetTable();
  
  work = new StSsdAlign();
  work->init(mSvtGeomT,mConfig,NumberEvents);
  
  
  for (int i=216; i<536; i++) work->mWafer[i].switchOff(); // no ssd yet !
  gMessMgr->Info()<<"StSvtAlignmentMaker : SSD is Off" <<endm;
    

  if( DataType == 0) work->simulUnAlignment(0.020, 0.020); // =>(+/- 0.02 cm, +/-0.02 rad)
  //  In case of private cosmic alignment, feed wafers with random alignment parameters
  
  // geant points are given for a perfect svt/ssd geometry
  // For DataType > 0, trying to feed unaligned wafer with global geant points
  // give wrong local points.
    
  //if( DataType > 0) work->simulUnAlignment(0., 0.); // by default

  // Save Original Parameters :
  for (int i=0; i<536; i++)
    {
      unalign[i][0] = work->mWafer[i].dx();
      unalign[i][1] = work->mWafer[i].dy();
      unalign[i][2] = work->mWafer[i].dz();
      unalign[i][3] = work->mWafer[i].alpha();
      unalign[i][4] = work->mWafer[i].beta();
      unalign[i][5] = work->mWafer[i].gamma();
    };

  gMessMgr->Info()<<"StSvtAlignmentMaker : Init STOP"<<endm; 
  return  StMaker::Init();
}

//____________________________________________________________________________

Int_t StSvtAlignmentMaker::Make()
{
  gMessMgr->Info()<<"StSvtAlignmentMaker : Make START"<<endm; 
  gMessMgr->Info()<<"StSvtAlignmentMaker : Make NumberEvents = "<<NumberEvents
		  <<" NumberOfEventsSoFar = "<< NumberOfEventsSoFar+1
		  <<" (DataType = "<<DataType<< ")" << endm; 
  
  
  if (NumberOfEventsSoFar >= NumberEvents){
    gMessMgr->Warning("You are processing more events than required quitting");
    return kStWarn;
  }
  
  if( DataType == 0){
    // If doing own simulation
    int numberOfTracks = 10000;
    int option = 2; // option > 0 => detectors position resolution
    gMessMgr->Info()<<"StSvtAlignmentMaker : doing own cosmic simulation. "
		    << numberOfTracks << " tracks for event " << NumberOfEventsSoFar << endm;
    gMessMgr->Info()<<"StSvtAlignmentMaker : hits are given taking account of a random misalignment."
		    << endm;

    work->simulCosmics( NumberOfEventsSoFar, numberOfTracks, option);
  }
  else if( DataType > 0){
    gMessMgr->Info()<< "StSvtAlignmentMaker: take hits from the chain. Geometry is perfect in simulations"<< endm;
    St_svg_shape* SvtShape=0;
    St_svg_geom* SvtGeom=0;
    St_srs_srspar* SvtSrsPar=0;
    St_DataSetIter       local(GetInputDB("svt"));
    SvtShape = (St_svg_shape   *) local("svgpars/shape");
    
    if ( !strncmp(mConfigString.Data(), "Y1L", strlen("Y1L")) )
      SvtGeom  = (St_svg_geom    *) local("svgpars/geomy1l");
   else
     SvtGeom = (St_svg_geom    *) local("svgpars/geom");
    
    SvtSrsPar  = (St_srs_srspar  *) local("srspars/srs_srspar");
    
    mSvtSrsParT = (srs_srspar_st*)SvtSrsPar->GetTable();
    mSvtShapeT  = (svg_shape_st*)SvtShape->GetTable();
    mSvtGeomT  = (svg_geom_st*)SvtGeom->GetTable();
    mCoordTransform->setParamPointers(mSvtSrsParT, mSvtGeomT, mSvtShapeT, mConfig);

    work->SetTransform(mCoordTransform);
    
    // If doing real tracking
    // Get points from svt spt table
    St_scs_spt*   Stscsspt=0;
    St_DataSet *svt  = GetInputDS("svt_hits");
    
    Stscsspt = (St_scs_spt *)svt->Find("scs_spt");
    if (!Stscsspt) { 
      gMessMgr->Warning("No SVT/SSD hits !");
      return kStWarn;
    }
    
    scs_spt_st* Spts = Stscsspt->GetTable();

    // Get  groups
    St_sgr_groups*   Stgroups=0;
    svt  = GetInputDS("est");
    
    Stgroups = (St_sgr_groups *)svt->Find("EstGroups");
    sgr_groups_st* Groups = Stgroups->GetTable();
    int NGroups =  Stgroups->GetNRows();
    gMessMgr->Info()<<"StSvtAlignmentMaker : "<< NGroups << " tracks in EstGroups" << endm;
    // but faking needing alignment
    // Shuffle points by par real amounts so off of tracks
    // Then fill track class
    if (DataType == 1)
      {
	work->CreateEvent(NumberOfEventsSoFar);
	globalPoint gP[16];
	int i=0, ilp[16];
	int idLast = Groups[0].id1;
	int nTrackHit, nTrackNo=0;
	while( i< NGroups){
	  nTrackHit = 0;
	  while( Groups[i].id1 == idLast){
	    int barrelId = (Spts[Groups[i].id2-1].id_wafer-1000)/2000+1;
	    int  waferId = (Spts[Groups[i].id2-1].id_wafer-barrelId*1000)/100;
	    int ladderId = (Spts[Groups[i].id2-1].id_wafer-barrelId*1000
			    -waferId*100);
	    waferId = waferId - ((Spts[Groups[i].id2-1].id_wafer/1000)-barrelId)*10;

	    gP[nTrackHit].x = Spts[Groups[i].id2-1].x[0];
	    gP[nTrackHit].y = Spts[Groups[i].id2-1].x[1];
	    gP[nTrackHit].z = Spts[Groups[i].id2-1].x[2];
	    ilp[nTrackHit] = mConfig->getHybridIndex(barrelId,ladderId,waferId,1)/2;
	    idLast = Groups[i].id1;
	    nTrackHit++;
	    i++;
	  }
	  work->FillTrack(NumberOfEventsSoFar, nTrackNo, nTrackHit, gP, ilp);
	  nTrackNo++;
	  idLast = Groups[i].id1;
	}
	work->recordEventHits();
      }; // end if (DataType == 1)
  }// end if (DataType > 1)

  NumberOfEventsSoFar++;


  //************************************************************************
  // Alignment : 
  if( NumberOfEventsSoFar == NumberEvents){
    
    gMessMgr->Info()<<"StSvtAlignmentMaker : Doing the alignment !"<<endm; 
    //Put all hits into 

    if( DataType == 0){
      // Need to move wafers back to original position so can try and figure out what the shifts were
      gMessMgr->Info()<<"StSvtAlignmentMaker : Own cosmics simulation mode"<<endm;
      work->simulUnAlignment(0., 0.);
    }
    else if( DataType > 0){
      gMessMgr->Info()<<"StSvtAlignmentMaker : Datas from the chain"<<endm;
      gMessMgr->Info()<<"StSvtAlignmentMaker : hits have been generated with a perfect geometry."<< endm;
      for (int i=0; i<NumberEvents; i++) work->findVertex(i);
      gMessMgr->Info()<<"StSvtAlignmentMaker : alignment parameters are now randomly generated." << endm;
      work->simulUnAlignment(0.05, 0.05);
      for (int i=0; i<NumberEvents; i++) work->updateGlobalPoints(i);
      for (int i=0; i<536; i++)
	{
	  unalign[i][0] = work->mWafer[i].dx();
	  unalign[i][1] = work->mWafer[i].dy();
	  unalign[i][2] = work->mWafer[i].dz();
	  unalign[i][3] = work->mWafer[i].alpha();
	  unalign[i][4] = work->mWafer[i].beta();
	  unalign[i][5] = work->mWafer[i].gamma();
	};
    };
    for (int i=0; i<NumberEvents; i++) work->findVertex(i);    
    gMessMgr->Info()<<"StSvtAlignmentMaker : alignment START"<<endm;
    work->cosmicAlign2( 30 );
    gMessMgr->Info()<<"StSvtAlignmentMaker : alignment STOP" <<endm;
    for (int i=0; i<NumberEvents; i++) work->findVertex(i);
  }

  gMessMgr->Info()<<"StSvtAlignmentMaker : Make STOP"<<endm;
  return kStOK;
}

//____________________________________________________________________________
Int_t StSvtAlignmentMaker::Finish()
{
  if (Debug()) gMessMgr->Debug() << "In StSvtAlignmentMaker::Finish() ..."
				 << endm;
  
  fstream para;
  para.open("param1.dat",ios::out);
  
  for (int i=0; i<536; i++)
    {
      para << " " << work->mWafer[i].dx() ;
      para << " " << work->mWafer[i].dy();
      para << " " << work->mWafer[i].dz();
      para << " " << work->mWafer[i].alpha();
      para << " " << work->mWafer[i].beta();
      para << " " << work->mWafer[i].gamma() << endl;
    };
 
 
 for (int compt=0; compt<536; compt++)
   {
     para << " " << unalign[compt][0];
     para << " " << unalign[compt][1];
     para << " " << unalign[compt][2];
     para << " " << unalign[compt][3];
     para << " " << unalign[compt][4];
     para << " " << unalign[compt][5] << endl;
   };
 para.close();
 
 gMessMgr->Info()<<"StSvtAlignmentMaker : alignment evaluation in align.root"<<endm;

 dxError  = new TH1F("ErrorDx","dx reco-simu", 200, -0.05, 0.05);
 dyError   = new TH1F("ErrorDy","dy reco-simu", 200, -0.05, 0.05);
 dzError   = new TH1F("ErrorDz","dz reco-simu", 200, -0.05, 0.05);
 alphaError   = new TH1F("ErrorAlpha","alpha reco-simu", 200, -0.05, 0.05);
 betaError   = new TH1F("ErrorBeta","beta reco-simu", 200, -0.05, 0.05);
 gammaError  = new TH1F("ErrorGamma","gamma reco-simu", 200, -0.05, 0.05);
 
 for (int i=0; i<6; i++)
   hParams2d[i] = new TH2F("params2d","simu vs reco",200,-0.05, 0.05, 200,-0.05, 0.05);
 
 if (DataType == 0)
   for (int compt=0; compt<216; compt++)
     {
       for (int i=0; i<6; i++)
	 hParams2d[i]->Fill( work->mWafer[compt].param(i), unalign[compt][i] );
       dxError->Fill( work->mWafer[compt].param(0) - unalign[compt][0] );
       dyError->Fill( work->mWafer[compt].param(1) - unalign[compt][1] );
       dzError->Fill( work->mWafer[compt].param(2) - unalign[compt][2] );
       alphaError->Fill( work->mWafer[compt].param(3) - unalign[compt][3] );
       betaError->Fill( work->mWafer[compt].param(4) - unalign[compt][4] );
       gammaError->Fill( work->mWafer[compt].param(5) - unalign[compt][5] );
     }

 if (DataType == 1)
   for (int compt=0; compt<216; compt++)
     {
       for (int i=0; i<6; i++)
	 hParams2d[i]->Fill( work->mWafer[compt].param(i), 0);
       dxError->Fill( work->mWafer[compt].param(0));
       dyError->Fill( work->mWafer[compt].param(1));
       dzError->Fill( work->mWafer[compt].param(2));
       alphaError->Fill( work->mWafer[compt].param(3));
       betaError->Fill( work->mWafer[compt].param(4));
       gammaError->Fill( work->mWafer[compt].param(5));
     };

 TFile *hFile = new TFile("align1.root","RECREATE");
 hFile->SetFormat(1);
 for (int i=0; i<6; i++)
   hParams2d[i]->Write();
 dxError->Write();
 dyError->Write();
 dzError->Write();
 alphaError->Write();
 betaError->Write();
 gammaError->Write();
 hFile->Close();

  return kStOK;
}
//____________________________________________________________________________


//____________________________________________________________________________

Int_t StSvtAlignmentMaker::setConfig(StSvtConfig* config)
{
  mConfigString = TString(config->getConfiguration());
  mConfig = config;
  return kStOK;
}
//_____________________________________________________________________________

Int_t StSvtAlignmentMaker::setConfig(const char* config)
{

  gMessMgr->Message() <<"StSvtAlign:Setting configuration to "<< config << endm;
  mConfigString = config;
  return kStOK;
}
