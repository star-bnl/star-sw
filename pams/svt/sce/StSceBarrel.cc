#include "StSceBarrel.hh"

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

void StSceBarrel::initWafers(table_head_st *geom_h, svg_geom_st *geom)
{
  for (int i = 0; i < geom_h->nok; i++)
    {
      if (geom[i].id > mSsdLayer*1000)
	{
 	  mWafers[idWaferToWaferNumb(geom[i].id)]->init(geom[i].id, geom[i].d, geom[i].t, geom[i].n, geom[i].x);
	}
    }
}

int StSceBarrel::readPointFromTable(table_head_st *g2t_h, g2t_svt_hit_st *g2t)
 {
   int minWaf      = mSsdLayer*1000;
   int currWafId   = 0;
   int currWafNumb = 0;
   int counter     = 0;
   int i           = 0 ;
   int j           = 0 ;
   float *p        = new float[3];
   for (i = 0; i<g2t_h->nok ; i++)
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
  delete [] p;
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

int StSceBarrel::removeInactiveHitInTable(table_head_st *g2t_h,g2t_svt_hit_st *g2t)
{
  StSceListPoint *inactiveHits = new StSceListPoint();
  int localSize = 0;
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
    {
      StSceListPoint *currDeadList = (this->mWafers[iWaf])->getDeadHits(mDetectorLargeEdge, mDetectorSmallEdge, mStripPitch);
      inactiveHits = inactiveHits->addListPoint(currDeadList);
      delete currDeadList;
    }
  inactiveHits->sortPoint();
  localSize=inactiveHits->getSize();
  if (localSize)
    {
      int firstSsdPoint=0;
      int iP1 = 0;
      for (iP1 = 0; ((iP1 < g2t_h->nok)&&(g2t[iP1].volume_id < mSsdLayer*1000)) ; iP1++) firstSsdPoint=iP1;
      firstSsdPoint++;
      int isG2tSorted = 1;
      int iP2 = 0;
      for (iP2 = firstSsdPoint+1 ; (iP2 < g2t_h->nok)&&(isG2tSorted) ;iP2++)
	{
	  if (g2t[iP2].id < g2t[iP2 - 1].id) isG2tSorted = 0;
	}
      StScePoint *currToDele = inactiveHits->first();
      int nDeleted = 0;
      int isAllRemove = 0;
      if (isG2tSorted)
	{
	  int ipScan = 0;
	  for (ipScan = firstSsdPoint; (ipScan<g2t_h->nok); ipScan++)
	    {
	      if (!isAllRemove)
		{
		  if (g2t[ipScan].id == currToDele->getNPoint())
		    {
		      currToDele=inactiveHits->next(currToDele);
		      if (currToDele == 0)
			{
			  isAllRemove = 1;
			}
		      else
			{
			  nDeleted++;
			}
		    }
		}
	      g2t[ipScan]=g2t[ipScan + nDeleted];
	    }
	  g2t_h->nok -= nDeleted;
	}
      else
	{
	  int iLoop = 0;
	  for (iLoop = 0 ; (iLoop < localSize)&&(!isAllRemove); iLoop++)
	    {
	      int ipScan = 0;
	      int nLoopDeleted = 0;
	      for (ipScan = firstSsdPoint; ipScan < g2t_h->nok; ipScan++)
		{
		  if (!isAllRemove)
		    {
		      if (g2t[ipScan].id == currToDele->getNPoint())
			{
			  currToDele=inactiveHits->next(currToDele);
			  if (currToDele == 0)
			    {
			      isAllRemove = 1;
			    }
			  else
			    {
			      nDeleted++;
			      nLoopDeleted++;
			    }
			}
		    }
		  g2t[ipScan]=g2t[ipScan + nLoopDeleted];
		}
	      
	      g2t_h->nok -= nLoopDeleted;
	    }
	}
    }
  this->renumHitAfterRemove();
  delete inactiveHits;
  return localSize;  
}

void StSceBarrel::renumHitAfterRemove()
{
  int iLast = 0;
  int iNewLast = 0;
  int iWaf = 0;
  for (iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      iNewLast = ((this->mWafers[iWaf])->getPoint())->renumHits(iLast);
      iLast = iNewLast;
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

int StSceBarrel::readClusterFromTable(table_head_st *cluster_h, scf_cluster_st *cluster)
{
  int NumberOfCluster = 0;
  int idWaf           = 0;
  int nCluster        = 0;
  int nPCluster       = 0;
  int nNCluster       = 0;
  int iSide           = 0;
  int *idMcHit        = new int[5];
  int e               = 0;
  int nStrip          = 0;
  int nFirstStrip     = 0;
  int nFirstAdc       = 0;
  int nLastAdc        = 0;
  int nAdcCount       = 0;
  int nNoiseCount     = 0;
  float nStripMean    = 0;
  int nFlag           = 0;

  for (int i = 0 ; i < cluster_h->nok; i++)
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
      for (e = 0 ; e < 5; e++) idMcHit[e] = cluster[i].id_mchit[e];

      StSceCluster *newCluster = new StSceCluster(nCluster, nFirstStrip, nStrip, nAdcCount, nFirstAdc, nLastAdc, nNoiseCount, nStripMean, nFlag, idMcHit);
      if (iSide == 0)
	{nPCluster++;}
      else
        {nNCluster++;}
      mWafers[idWaferToWaferNumb(idWaf)]->addCluster(newCluster, iSide);
    }
  NumberOfCluster = cluster_h->nok;  
  delete [] idMcHit;
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

int StSceBarrel::readSimPointFromTable(table_head_st *sim_spt_h, sls_spt_st *sim_spt)
{
  int nFlag       = 0;
  int nSpt        = 0;
  int idCluster   = 0;
  int idGlobTrk   = 0;
  int idMatch     = 0;

  int *idMcHit    = new int[5];
  int *idMcTrack  = new int[5];
  int *idTrack    = new int[5];
  int e           = 0;

  int idWaf       = 0;

  float *Cov      = new float[3];
  float *Res      = new float[3];
  float *Xg       = new float[3];
  float *Xl       = new float[3];

  float *Mom2     = new float[2];
  float *De       = new float[2];

  for (int i = 0; i < sim_spt_h->nok; i++)
    {
      nFlag       = sim_spt[i].flag;
      nSpt        = sim_spt[i].id;

      idCluster   = sim_spt[i].id_cluster;
      idGlobTrk   = sim_spt[i].id_globtrk;
      idMatch     = sim_spt[i].id_match;

      for (e = 0; e < 5; e++)
	{
	  idMcHit[e]   = sim_spt[i].id_mchit[e];
	  idMcTrack[e] = sim_spt[i].id_mctrack[e];
	  idTrack[e]   = sim_spt[i].id_track[e];
	}
      idWaf = sim_spt[i].id_wafer;
      for (e = 0; e < 3; e++)
	{
	  Cov[e] = sim_spt[i].cov[e];
	  Res[e] = sim_spt[i].res[e];
	  Xg[e]  = sim_spt[i].x[e];
	  Xl[e]  = sim_spt[i].xl[e];
	}

      for (e = 0; e < 2; e++)
	{
	  Mom2[e] = sim_spt[i].mom2[e];
	  De[e]   = sim_spt[i].de[e];
	}

      StScePoint *newPoint = new StScePoint(nFlag, nSpt, idCluster, idGlobTrk, idMatch, idMcHit, idMcTrack, idTrack, idWaf, Cov, Res, Xg, Xl, Mom2, De);
      mWafers[idWaferToWaferNumb(idWaf)]->addSimPoint(newPoint);
    }
  delete [] idMcHit;
  delete [] idMcTrack;
  delete [] idTrack;
  delete [] Cov;
  delete [] Res;
  delete [] Xg;
  delete [] Xl;
  delete [] Mom2;
  delete [] De;
  return  sim_spt_h->nok;  
}

int StSceBarrel::readRecPointFromTable(table_head_st *rec_spt_h, scm_spt_st *rec_spt)
{
  int nFlag       = 0;
  int nSpt        = 0;
  int idCluster   = 0;
  int idGlobTrk   = 0;
  int idMatch     = 0;

  int *idMcHit    = new int[5];
  int *idMcTrack  = new int[5];
  int *idTrack    = new int[5];
  int e           = 0;

  int idWaf       = 0;

  float *Cov      = new float[3];
  float *Res      = new float[3];
  float *Xg       = new float[3];
  float *Xl       = new float[3];

  float *Mom2     = new float[2];
  float *De       = new float[2];

  for (int i = 0; i < rec_spt_h->nok; i++)
    {
      nFlag       = rec_spt[i].flag;
      nSpt        = rec_spt[i].id;

      idCluster   = rec_spt[i].id_cluster;
      idGlobTrk   = rec_spt[i].id_globtrk;
      idMatch     = rec_spt[i].id_match;
      for (e = 0; e < 5; e++)
	{
	  idMcHit[e]   = rec_spt[i].id_mchit[e];
	  idMcTrack[e] = rec_spt[i].id_mctrack[e];
	  idTrack[e]   = rec_spt[i].id_track[e];
	}
      idWaf = rec_spt[i].id_wafer;
      for (e = 0; e < 3; e++)
	{
	  Cov[e] = rec_spt[i].cov[e];
	  Res[e] = rec_spt[i].res[e];
	  Xg[e]  = rec_spt[i].x[e];
	  Xl[e]  = rec_spt[i].xl[e];
	}

      for (e = 0; e < 2; e++)
	{
	  Mom2[e] = rec_spt[i].mom2[e];
	  De[e]   = rec_spt[i].de[e];
	}

      StScePoint *newPoint = new StScePoint(nFlag, nSpt, idCluster, idGlobTrk, idMatch, idMcHit, idMcTrack, idTrack, idWaf, Cov, Res, Xg, Xl, Mom2, De);
      mWafers[idWaferToWaferNumb(idWaf)]->addRecPoint(newPoint);
    }
  delete [] idMcHit;
  delete [] idMcTrack;
  delete [] idTrack;
  delete [] Cov;
  delete [] Res;
  delete [] Xg;
  delete [] Xl;
  delete [] Mom2;
  delete [] De;
  return  rec_spt_h->nok;  
}

int  StSceBarrel::doEvalSpt(sce_ctrl_st *ctrl)
{
  int nWafer    = mNLadder*mNWaferPerLadder; 
  int nSptEvaluated = 0;
   for (int iWaf = 0; iWaf < nWafer; iWaf++)
       nSptEvaluated +=  mWafers[iWaf]->doEvaluateSpt(ctrl);
  return nSptEvaluated;
}

int  StSceBarrel::writeComPointToTable(table_head_st *dSpt_h, sce_dspt_st *dSpt)
{
  int currRecord   = 0;
  dSpt_h->nok      = 0;
  int i            = 0;
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
    {
     int idCurrentWaf = waferNumbToIdWafer(iWaf);
     StSceListComp *dSptList = mWafers[iWaf]->getComPoint();
     StSceComp *pDspt = dSptList->first();
     while (pDspt)
       {
	     dSpt[currRecord].id            = 10000*(pDspt->getNComp())+idCurrentWaf;
      	     dSpt[currRecord].prob          = pDspt->getProb();
	     dSpt[currRecord].ghost_true    = pDspt->getGhostOrTrue();
	     dSpt[currRecord].kind_package  = pDspt->getKindPackage();
	     dSpt[currRecord].id_match      = pDspt->getIdMatch();
	     dSpt[currRecord].id_wafer      = idCurrentWaf;
	  for (i = 0; i < 2; i++)
	    {
              dSpt[currRecord].d2e[i]       = pDspt->getD2e(i);
	    }
	  for (i = 0; i < 3; i++)
	    {
             dSpt[currRecord].dx[i]         = pDspt->getDxg(i);
	     dSpt[currRecord].dxl[i]        = pDspt->getDxl(i);
	    }
       	  currRecord++;
	  pDspt    = dSptList->next(pDspt);
	  dSpt_h->nok++;
       }
    }
  return currRecord;
}
