#include "StSlsBarrel.hh"
// #include "StSSDdcs.hh"
// #include "StSSDcalibRun.hh"

StSlsBarrel::StSlsBarrel(sdm_geom_par_st *geom_par)
{
  this->setSsdParameters(geom_par);

  int nWafer  = mNLadder*mNWaferPerLadder; 
  int idWaf   = 0;

  mWafers = new StSlsWafer*[nWafer];
  for (int iWaf = 0; iWaf < nWafer; iWaf++)
    { 
      idWaf   = waferNumbToIdWafer(iWaf);
      mWafers[iWaf] = new StSlsWafer(idWaf);
    }
}

StSlsBarrel::~StSlsBarrel()
{
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
    { delete mWafers[iWaf]; }
}

void StSlsBarrel::setSsdParameters(sdm_geom_par_st *geom_par)
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

void StSlsBarrel::initWafers(table_head_st *geom_h, svg_geom_st *geom)
{
  for (int i = 0; i < geom_h->nok; i++)
    {
      if (geom[i].id > mSsdLayer*1000)
	{
	  mWafers[idWaferToWaferNumb(geom[i].id)]->init(geom[i].id, geom[i].d, geom[i].t, geom[i].n, geom[i].x);
	}
    }
}

int StSlsBarrel::readPointFromTable(table_head_st *g2t_h, g2t_svt_hit_st *g2t)
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
	  currWafNumb=idWaferToWaferNumb(currWafId);
	  for (j = 0; j<3; j++) p[j] = g2t[i].p[j];
	  mWafers[currWafNumb]->addHit(g2t[i].id, g2t[i].id, g2t[i].track_p, g2t[i].x, g2t[i].de, p);
	}
    }
  delete [] p;
  return counter;
}

void StSlsBarrel::convertGlobalFrameToOther()
{
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
    {
      StSlsWafer *currWafer = mWafers[iWaf];
      currWafer->convertGlobalToLocal();
      currWafer->convertLocalToUFrame(mDetectorLargeEdge, mDetectorSmallEdge, mTheta);
    }
}

int StSlsBarrel::removeInactiveHitInTable(table_head_st *g2t_h,g2t_svt_hit_st *g2t)
{
  StSlsListPoint *inactiveHits = new StSlsListPoint();
  int localSize = 0;
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
    {
      StSlsListPoint *currDeadList = (this->mWafers[iWaf])->getDeadHits(mDetectorLargeEdge, mDetectorSmallEdge, mStripPitch);
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
      StSlsPoint *currToDele = inactiveHits->first();
      int nDeleted = 0;
      int isAllRemove = 0;
      if (isG2tSorted)
	{
	  int ipScan = 0;
	  int ipKeep = firstSsdPoint;
	  for (ipScan = firstSsdPoint; (ipScan<g2t_h->nok); ipScan++)
	    {
	      if ((!isAllRemove)&&(g2t[ipScan].id == currToDele->getNId()))
		{
		  currToDele=inactiveHits->next(currToDele);
		  if (currToDele == 0) isAllRemove = 1;
		  nDeleted++;
		}
	      else
		{
		  g2t[ipKeep]=g2t[ipScan];
		  ipKeep++;
		}
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
		      if (g2t[ipScan].id == currToDele->getNId())
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

void StSlsBarrel::renumHitAfterRemove()
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

void StSlsBarrel::chargeSharingOverStrip(sls_ctrl_st  *ctrl)
{
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      mWafers[iWaf]->convertToStrip(mStripPitch, 
				 mNStripPerSide,
				 ctrl[0].PairCreationEnergy,
				 ctrl[0].NStripInACluster,
				 ctrl[0].ParDiffP,
				 ctrl[0].ParDiffN,
				 ctrl[0].ParIndRightP,
				 ctrl[0].ParIndRightN,
				 ctrl[0].ParIndLeftP,
				 ctrl[0].ParIndLeftN);
    }
}

int StSlsBarrel::writeStripToTable(table_head_st *strip_h, sls_strip_st *strip)
{

  int currRecord = 0;
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      int idCurrentWaf = waferNumbToIdWafer(iWaf);

      StSlsListStrip *stripP = (this->mWafers[iWaf])->getStripP();
      StSlsListStrip *stripN = (this->mWafers[iWaf])->getStripN();
      
      StSlsStrip *pStripP = stripP->first();
      int iP = 0;
      for (iP = 0; iP < stripP->getSize() ; iP++)
	{
	  strip[currRecord].id          = currRecord + 1;
	  strip[currRecord].adc_count   = pStripP->getDigitSig();
	  strip[currRecord].noise_count = 0;
	  strip[currRecord].id_strip    = 10000*(10*pStripP->getNStrip() + 0)+idCurrentWaf;
	  strip[currRecord].id_cluster  = 0;
	  strip[currRecord].N_hits      = pStripP->getNHits();
	  strip[currRecord].de          = pStripP->getAnalogSig();
	  int i = 0 ;
	  for (i = 0 ; i < 5 ; i++)
	    {
	      strip[currRecord].id_hit[i]     = pStripP->getIdHit(i);
	      strip[currRecord].id_mchit[i]   = pStripP->getIdMcHit(i);
	      strip[currRecord].id_mctrack[i] = pStripP->getIdMcTrack(i);
	    }
	  currRecord++;
	  pStripP    = stripP->next(pStripP);
	  strip_h->nok = currRecord;
	}      
      
      StSlsStrip *pStripN = stripN->first();
      int iN = 0;
      for (iN = 0 ; iN < stripN->getSize() ; iN++)
	{
	  strip[currRecord].id          = currRecord + 1;
	  strip[currRecord].adc_count   = pStripN->getDigitSig();
	  strip[currRecord].noise_count = 0;
	  strip[currRecord].id_strip    = 10000*(10*pStripN->getNStrip() + 1)+idCurrentWaf;
	  strip[currRecord].id_cluster  = 0;
	  strip[currRecord].N_hits      = pStripN->getNHits() ;
	  strip[currRecord].de          = pStripN->getAnalogSig();
	  int i = 0;
	  for (i = 0 ; i < 5 ; i++)
	    {
	      strip[currRecord].id_hit[i]     = pStripN->getIdHit(i);
	      strip[currRecord].id_mchit[i]   = pStripN->getIdMcHit(i);
	      strip[currRecord].id_mctrack[i] = pStripN->getIdMcTrack(i);
	    }
	  currRecord++;
	  pStripN    = stripN->next(pStripN);
	  strip_h->nok = currRecord;  
	}
    }
  return currRecord;
}

int StSlsBarrel::idWaferToWaferNumb(int idWafer)
{
  // idwafer = layer*1000+waf*100+ladder
  int iW = (int)((idWafer - mSsdLayer*1000)/100);
  int iL = idWafer - mSsdLayer*1000 - iW*100;
  return ((iL-1)*mNWaferPerLadder + iW -1);
}

int StSlsBarrel::waferNumbToIdWafer(int waferNumb)
{
  int iL = 1+(int)((waferNumb)/mNWaferPerLadder);
  int iW = waferNumb-((iL-1)*mNWaferPerLadder)+1;
  return mSsdLayer*1000 + iW*100 + iL;  
}
