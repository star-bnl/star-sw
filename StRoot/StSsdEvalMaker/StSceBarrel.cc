#include "StSceBarrel.hh"

#include "tables/St_sdm_geom_par_Table.h"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_scm_spt_Table.h"
#include "tables/St_sce_dspt_Table.h"

StSceBarrel::StSceBarrel(sdm_geom_par_st  *geom_par)
{
  this->setSsdParameters(geom_par);

  int nWafer  = mNLadder*mNWaferPerLadder; 
  int idWaf   = 0;

  mWafers = new StSceWafer*[nWafer];
  for (int iWaf=0; iWaf < nWafer; iWaf++)
    { 
      idWaf   = waferNumbToIdWafer(iWaf);
      mWafers[iWaf] = new StSceWafer(idWaf);
    }
}

StSceBarrel::~StSceBarrel()
{
  for (int i = 0; i < mNLadder*mNWaferPerLadder; i++)
    delete mWafers[i];
}

void StSceBarrel::setSsdParameters(sdm_geom_par_st  *geom_par)
{
  mSsdLayer            = geom_par[0].N_layer; // all layers : 1->7
  mDetectorLargeEdge   = 2.*geom_par[0].L_wafer_act_l;
  mDetectorSmallEdge   = 2.*geom_par[0].L_wafer_act_w;
  mNLadder             = geom_par[0].N_ladder;
  mNWaferPerLadder     = geom_par[0].N_waf_per_ladder;
  mNStripPerSide       = geom_par[0].N_strip_per_side;
  mStripPitch          = geom_par[0].L_strip_pitch;
  mTheta               = geom_par[0].L_stereo_angle;
}

void StSceBarrel::initWafers(St_svg_geom *svg_geom)
{
  svg_geom_st *geom = svg_geom->GetTable();
  
  for (int i = 0; i < svg_geom->GetNRows() ; i++)
    {
      if (geom[i].id > mSsdLayer*1000)
	{
 	  mWafers[idWaferToWaferNumb(geom[i].id)]->init(geom[i].id, geom[i].d, geom[i].t, geom[i].n, geom[i].x);
	}
    }
}

int StSceBarrel::readPointFromTable(St_g2t_svt_hit *g2t_svt_hit)
 {
   g2t_svt_hit_st *g2t = g2t_svt_hit->GetTable();
   
   int minWaf      = mSsdLayer*1000;
   int currWafId   = 0;
   int currWafNumb = 0;
   int counter     = 0;
   int i           = 0 ;
   int j           = 0 ;
   //   float *p        = new float[3];
   float p[3]      = {0,0,0};
   for (i = 0; i< g2t_svt_hit->GetNRows() ; i++)
    {
      currWafId=g2t[i].volume_id;
       if (currWafId > minWaf)
	{
	  counter++;
	  currWafNumb = idWaferToWaferNumb(currWafId);
	  for (j = 0; j<3; j++) p[j] = g2t[i].p[j];
 	  mWafers[currWafNumb]->addHit(g2t[i].id, g2t[i].id, g2t[i].track_p, g2t[i].x, g2t[i].de, p);
	}
    }
   //   delete [] p;
   return counter;
 }

void StSceBarrel::convertGlobalFrameToOther()
{
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
    {
      StSceWafer *currWafer = mWafers[iWaf];
      currWafer->convertGlobalToLocal();
      currWafer->convertLocalToUFrame(mDetectorLargeEdge, mDetectorSmallEdge, mTheta);
    }
}

int StSceBarrel::idWaferToWaferNumb(int idWafer)
{
  // idwafer = layer*1000+waf*100+ladder

  int iW = (int)((idWafer - mSsdLayer*1000)/100);
  int iL = idWafer - mSsdLayer*1000 - iW*100;
  return ((iL-1)*mNWaferPerLadder + iW -1);
}

int StSceBarrel::waferNumbToIdWafer(int waferNumb)
{
  int iL = 1+(int)((waferNumb)/mNWaferPerLadder);
  int iW = waferNumb-((iL-1)*mNWaferPerLadder)+1;
  return mSsdLayer*1000 + iW*100 + iL;  
}

int StSceBarrel::readClusterFromTable(St_scf_cluster *scf_cluster)
{
  scf_cluster_st *cluster = scf_cluster->GetTable();
  
  //scf_cluster->Print(0,scf_cluster->GetNRows());
  
  int NumberOfCluster = 0;
  int idWaf           = 0;
  int nCluster        = 0;
  int nPCluster       = 0;
  int nNCluster       = 0;
  int iSide           = 0;
  //  int *idMcHit        = new int[5];
  int idMcHit[5]={0,0,0,0,0};
  int e               = 0;
  int nStrip          = 0;
  int nFirstStrip     = 0;
  int nFirstAdc       = 0;
  int nLastAdc        = 0;
  int nAdcCount       = 0;
  int nNoiseCount     = 0;
  float nStripMean    = 0;
  int nFlag           = 0;

  for (int i = 0 ; i < scf_cluster->GetNRows(); i++)
    {
      nCluster    = (int)(cluster[i].id_cluster/100000.);
      idWaf       = (cluster[i].id_cluster-10000*((int)(cluster[i].id_cluster/10000.)));
      iSide       = (cluster[i].id_cluster-idWaf-nCluster*100000)/10000;
      nFirstStrip = (int)(cluster[i].first_strip/100000.);
      nStrip      = cluster[i].n_strip;
      nFirstAdc   = cluster[i].first_adc_count;
      nLastAdc    = cluster[i].last_adc_count;
      nAdcCount   = cluster[i].adc_count;
      nNoiseCount = cluster[i].noise_count;
      nStripMean  = cluster[i].strip_mean;
      nFlag       = cluster[i].flag;
      for (e = 0 ; e < 5; e++) {
	idMcHit[e] = cluster[i].id_mchit[e];
	//cout << "idMcHit["<<e<<"] = " << idMcHit[e] <<"*****" << "cluster["<<i<<"].id_mchit["<<e<<"]" << cluster[i].id_mchit[e] << endl;
      }
      StSceCluster *newCluster = new StSceCluster(nCluster, nFirstStrip, nStrip, nAdcCount, nFirstAdc, nLastAdc, nNoiseCount, nStripMean, nFlag, idMcHit);
      if (iSide == 0)
	{nPCluster++;}
      else
        {nNCluster++;}
      mWafers[idWaferToWaferNumb(idWaf)]->addCluster(newCluster, iSide);
    }
  NumberOfCluster = scf_cluster->GetNRows();  
  //  delete [] idMcHit;
  return NumberOfCluster;
}

int  StSceBarrel::doEvalCluster(sce_ctrl_st *ctrl)
{
  int nWafer    = mNLadder*mNWaferPerLadder; 
  int nClustEvaluated = 0;
   for (int iWaf = 0; iWaf < nWafer; iWaf++)
       nClustEvaluated +=  mWafers[iWaf]->doEvaluateCluster(ctrl);
  return nClustEvaluated;
}

int StSceBarrel::readRecPointFromTable(St_scm_spt *rec_spt)
{
  scm_spt_st *spt = rec_spt->GetTable();
  
  int nFlag       = 0;
  int nSpt        = 0;
  int idCluster   = 0;
  int idGlobTrk   = 0;
  int idMatch     = 0;
  int idWaf       = 0;

  int e           = 0;

//   int *idMcHit    = new int[5];
//   int *idMcTrack  = new int[5];
//   int *idTrack    = new int[5];
  int idMcHit[5]  = {0,0,0,0,0};
  int idMcTrack[5]= {0,0,0,0,0};
  int idTrack[5]  = {0,0,0,0,0};

//   float *Cov      = new float[3];
//   float *Res      = new float[3];
//   float *Xg       = new float[3];
//   float *Xl       = new float[3];
  float Cov[3]    = {0,0,0};
  float Res[3]    = {0,0,0};
  float Xg[3]     = {0,0,0};
  float Xl[3]     = {0,0,0};

//   float *Mom2     = new float[2];
//   float *De       = new float[2];
  float Mom2[2]   = {0,0};
  float De[2]     = {0,0};

  for (int i = 0; i < rec_spt->GetNRows(); i++)
    {
      nFlag       = spt[i].flag;
      nSpt        = spt[i].id;
		    
      idCluster   = spt[i].id_cluster;
      idGlobTrk   = spt[i].id_globtrk;
      idMatch     = spt[i].id_match;
      for (e = 0; e < 5; e++)
	{
	  idMcHit[e]   = spt[i].id_mchit[e];
	  idMcTrack[e] = spt[i].id_mctrack[e];
	  idTrack[e]   = spt[i].id_track[e];
	}
      idWaf = spt[i].id_wafer;
      for (e = 0; e < 3; e++)
	{
	  Cov[e] = spt[i].cov[e];
	  Res[e] = spt[i].res[e];
	  Xg[e]  = spt[i].x[e];
	  Xl[e]  = spt[i].xl[e];
	}

      for (e = 0; e < 2; e++)
	{
	  Mom2[e] = spt[i].mom2[e];
	  De[e]   = spt[i].de[e];
	}

      StScePoint *newPoint = new StScePoint(nFlag, nSpt, idCluster, idGlobTrk, idMatch, idMcHit, idMcTrack, idTrack, idWaf, Cov, Res, Xg, Xl, Mom2, De);
      mWafers[idWaferToWaferNumb(idWaf)]->addRecPoint(newPoint);
    }
//   delete [] idMcHit;
//   delete [] idMcTrack;
//   delete [] idTrack;
//   delete [] Cov;
//   delete [] Res;
//   delete [] Xg;
//   delete [] Xl;
//   delete [] Mom2;
//   delete [] De;
  return  rec_spt->GetNRows();  
}

int  StSceBarrel::doEvalSpt(sce_ctrl_st *ctrl)
{
  int nWafer    = mNLadder*mNWaferPerLadder; 
  int nSptEvaluated = 0;
   for (int iWaf = 0; iWaf < nWafer; iWaf++)
       nSptEvaluated +=  mWafers[iWaf]->doEvaluateSpt(ctrl);
  return nSptEvaluated;
}

int  StSceBarrel::writeComPointToTable(St_sce_dspt *sce_dSpt)
{
  sce_dspt_st dSpt;

  int currRecord   = 0;
  int i            = 0;
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
    {
     int idCurrentWaf = waferNumbToIdWafer(iWaf);
     StSceListComp *dSptList = mWafers[iWaf]->getComPoint();
     StSceComp *pDspt = dSptList->first();
     //     cout << "sce_dSpt->GetRowSize() = " << sce_dSpt->GetRowSize() << endl ;
     while (pDspt)
       {
	     dSpt.id            = 10000*(pDspt->getNComp())+idCurrentWaf;
     	     dSpt.prob          = pDspt->getProb();
	     dSpt.ghost_true    = pDspt->getGhostOrTrue();
	     dSpt.kind_package  = pDspt->getKindPackage();
	     // cout <<  "dSpt->kind_package" << dSpt->kind_package <<"****" << "pDspt->getKindPackage()" << pDspt->getKindPackage()<< endl;
	     dSpt.id_match      = pDspt->getIdMatch();
	     dSpt.id_wafer      = idCurrentWaf;
	  for (i = 0; i < 2; i++)
	    {
              dSpt.d2e[i]       = pDspt->getD2e(i);
	    }
	  for (i = 0; i < 3; i++)
	    {
             dSpt.dx[i]         = pDspt->getDxg(i);
	     dSpt.dxl[i]        = pDspt->getDxl(i);
	    }
	  sce_dSpt->AddAt(&dSpt);
       	  currRecord++;
	  pDspt    = dSptList->next(pDspt);
       }
    }
  return currRecord;
}
