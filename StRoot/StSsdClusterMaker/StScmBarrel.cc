#include "StScmBarrel.hh"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_scm_spt_Table.h"

StScmBarrel::StScmBarrel(sdm_geom_par_st  *geom_par) // okay in ClusterBarrel
{
  this->setSsdParameters(geom_par);

  int nWafer  = mNLadder*mNWaferPerLadder;
  int idWaf   = 0;
  
  mWafers = new StScmWafer*[nWafer];
//   mDeadStripP = new int*[nWafer];
//   mDeadStripN = new int*[nWafer];

  for (int iWaf = 0; iWaf < nWafer; iWaf++)
    { 
      idWaf   = waferNumbToIdWafer(iWaf);
//       mWafers[iWaf] = new StScmWafer(idWaf, mDeadStripP[iWaf],  mDeadStripN[iWaf]);
      mWafers[iWaf] = new StScmWafer(idWaf);
    }
}

StScmBarrel::~StScmBarrel() // okay in ClusterBarrel
{
  for (int iWaf = 0 ; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      delete mWafers[iWaf];
//       delete mDeadStripP[iWaf];
//       delete mDeadStripN[iWaf];
    }
}

void StScmBarrel::setSsdParameters(sdm_geom_par_st  *geom_par) // okay in ClusterBarrel
{
  mSsdLayer             = geom_par[0].N_layer;
  mNLadder              = geom_par[0].N_ladder;
  mNWaferPerLadder      = geom_par[0].N_waf_per_ladder;
  mNStripPerSide        = geom_par[0].N_strip_per_side;
}

void StScmBarrel::initWafers(St_svg_geom *geom_class) // okay in ClusterBarrel
{
  svg_geom_st *geom =  geom_class->GetTable();

  for (int i = 0; i < geom_class->GetNRows(); i++)
    {
      if (geom[i].id > mSsdLayer*1000)
 	{
  	  mWafers[idWaferToWaferNumb(geom[i].id)]->init(geom[i].id, geom[i].d, geom[i].t, geom[i].n, geom[i].x);
 	}
    }
}

// int StScmBarrel::readDeadStripFromTable(table_head_st *condition_db_h, sdm_condition_db_st *condition_db)
// {
//   int idWaf      = 0;
//   int nStrip     = 0;
//   int nDeadStrip = 0;
//   int iSide      = 0;
//   int isActive   = 0;
//   int sizeP      = 0;
//   int sizeN      = 0; 
//   int i          = 0;
//   for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
//     {
//       for ( i = 0 ; i < condition_db_h->nok; i++)
//         {  
//           idWaf  = condition_db[i].id_strip-10000*((int)(condition_db[i].id_strip/10000.)); 
//           nStrip = (int)(condition_db[i].id_strip/100000.);
//           iSide  = (condition_db[i].id_strip-idWaf-nStrip*100000)/10000;
//           isActive = condition_db[i].is_active;
//           if ((!isActive) && (idWaferToWaferNumb(idWaf) == iWaf))
//            { nDeadStrip++;
//              if (iSide == 0)
//                sizeP++;           
//              else 
//                sizeN++;
//            }
//         }
     
//        int* TempDeadStripP = new int[sizeP];
//        int* TempDeadStripN = new int[sizeN]; 
//        int currentDeadStripP = 0;
//        int currentDeadStripN = 0;

//         for (i = 0 ; i < condition_db_h->nok; i++)
//           {
//             idWaf  = condition_db[i].id_strip-10000*((int)(condition_db[i].id_strip/10000.));
//             nStrip = (int)(condition_db[i].id_strip/100000.);
//             iSide  = (condition_db[i].id_strip-idWaf-nStrip*100000)/10000;
//             isActive = condition_db[i].is_active;
// 	    if ((!isActive) && (idWaferToWaferNumb(idWaf) == iWaf) )
// 	      {
// 		if (iSide == 0)
// 		  {
// 		    TempDeadStripP[currentDeadStripP]=nStrip;
// 		    currentDeadStripP++;}
// 	        else
// 		  {
// 		    TempDeadStripN[currentDeadStripN]=nStrip;
// 		    currentDeadStripN++;}
// 	      }
//           }
	
//         mDeadStripP[iWaf] = TempDeadStripP;
//         mDeadStripN[iWaf] = TempDeadStripN;
//         delete TempDeadStripP;
//         delete TempDeadStripN; 
//     }
//   return nDeadStrip;
// }

int StScmBarrel::readClusterFromTable(St_scf_cluster *scf_cluster) // okay in ClusterBarrel
{
  scf_cluster_st *cluster = scf_cluster->GetTable();

  int NumberOfCluster = 0;
  int idWaf           = 0;
  int nCluster        = 0;
  int nPCluster       = 0;
  int nNCluster       = 0;
  int iSide           = 0;
  //  int *idMcHit        = new int[5];
  int idMcHit[5]      = {0,0,0,0,0};
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
      for (e = 0 ; e < 5; e++) idMcHit[e] = cluster[i].id_mchit[e];
      StScmCluster *newCluster = new StScmCluster(nCluster, nFirstStrip, nStrip, nAdcCount, nFirstAdc, nLastAdc, nNoiseCount, nStripMean, nFlag, idMcHit);
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

void StScmBarrel::sortListCluster() // okay in ClusterBarrel
{
  StScmListCluster *currentList = 0;
  int isSorted = 0;
  for (int iWaf = 0 ; iWaf < mNWaferPerLadder*mNLadder; iWaf++)
    {
      currentList = mWafers[iWaf]->getClusterP();
      isSorted = currentList->isSorted();
      if (!isSorted) currentList->sortCluster();

      currentList = mWafers[iWaf]->getClusterN();
      isSorted = currentList->isSorted();
      if (!isSorted) currentList->sortCluster();
    }
}

int StScmBarrel::doClusterMatching(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl)
{
  int  NumberOfPackage = 0;
  int nSolved = 0;
  int nPerfect = 0;
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
    { 
      NumberOfPackage += mWafers[iWaf]->doFindPackage(geom_par, scm_ctrl);
      nPerfect  = mWafers[iWaf]->doSolvePerfect(geom_par, scm_ctrl);
      nSolved  += nPerfect;
                  mWafers[iWaf]->doStatPerfect(nPerfect, scm_ctrl);
      nSolved  += mWafers[iWaf]->doSolvePackage(geom_par, scm_ctrl);
    }
  cout<<"****       Remark: "<<nSolved<<"  solved packages     ****\n";
  return NumberOfPackage;
}

void StScmBarrel::convertDigitToAnalog(sls_ctrl_st *sls_ctrl)
{
  long   NElectronInAMip    = sls_ctrl[0].NElectronInAMip;
  long   ADCDynamic         = sls_ctrl[0].ADCDynamic;
  int    NBitEncoding       = sls_ctrl[0].NBitEncoding;
  double PairCreationEnergy = sls_ctrl[0].PairCreationEnergy;

  const int NAdcChannel     = (int)pow(2.0,NBitEncoding);
  const double convFactor   = (PairCreationEnergy*ADCDynamic*NElectronInAMip)/NAdcChannel;
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
    mWafers[iWaf]->convertDigitToAnalog(convFactor);
}

void StScmBarrel::convertUFrameToOther(sdm_geom_par_st *geom_par)
{
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
   {
     mWafers[iWaf]->convertUFrameToLocal(geom_par);
     mWafers[iWaf]->convertLocalToGlobal();
   }
}

int StScmBarrel::writePointToTable(St_scm_spt *scm_spt)
{
  scm_spt_st spt;
  // table size is 148 bytes
  int currRecord   = 0;
  int i            = 0;

  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
    {
     int idCurrentWaf = waferNumbToIdWafer(iWaf);
     StScmListPoint *sptList = mWafers[iWaf]->getPoint();
     StScmPoint *pSpt = sptList->first();
     
     while (pSpt)
       {
	 spt.flag          = pSpt->getFlag();
	 spt.id            = 10000*(pSpt->getNPoint())+idCurrentWaf;
	 spt.id_cluster    = pSpt->getNCluster();
	 spt.id_globtrk    = 0;
	 spt.id_match      = pSpt->getNMatched();
	 for (i = 0 ; i < 5 ; i++)
	   {	  
	     spt.id_mchit[i]   = pSpt->getNMchit(i);
	     spt.id_mctrack[i] = 0;
	     spt.id_track[i]   = 0;
	   }	  
	 spt.id_wafer      = idCurrentWaf;
	 for (i = 0 ; i < 3 ; i++)
	   {	  
	     spt.cov[i]        = 0;
	     spt.res[i]        = 0;
	     spt.x[i]          = pSpt->getXg(i);
	     spt.xl[i]         = pSpt->getXl(i);
	   }
	 for (i = 0 ; i < 2 ; i++)
	   {
	     spt.mom2[i]       = 0;
	     spt.de[i]         = pSpt->getDe(i);
	   }
	 scm_spt->AddAt(&spt);
	 currRecord++;
	 pSpt    = sptList->next(pSpt);
       }
    }
  return currRecord;
}

// StScmBarrel::StScmBarrel(const StScmBarrel & originalBarrel)
// {
//   mSsdLayer             = originalBarrel.mSsdLayer;
//   mNLadder              = originalBarrel.mNLadder;
//   mNWaferPerLadder      = originalBarrel.mNWaferPerLadder;
//   mNStripPerSide        = originalBarrel.mNStripPerSide;

//   int nWafer  = mNLadder*mNWaferPerLadder;
//   int idWaf   = 0;
  
//   mWafers = new StScmWafer*[nWafer];
//   mDeadStripP = new int*[nWafer];
//   mDeadStripN = new int*[nWafer];

//   for (int iWaf = 0; iWaf < nWafer; iWaf++)
//     { 
//       idWaf   = waferNumbToIdWafer(iWaf);
//       mWafers[iWaf] = new StScmWafer(idWaf, mDeadStripP[iWaf],  mDeadStripN[iWaf]);
//     }
// }

// StScmBarrel& StScmBarrel::operator=(const StScmBarrel  originalBarrel)
// {
//   mSsdLayer             = originalBarrel.mSsdLayer;
//   mNLadder              = originalBarrel.mNLadder;
//   mNWaferPerLadder      = originalBarrel.mNWaferPerLadder;
//   mNStripPerSide        = originalBarrel.mNStripPerSide;

//   int nWafer  = mNLadder*mNWaferPerLadder;
//   int idWaf   = 0;

//   for (int iWaf = 0; iWaf < nWafer; iWaf++)
//     { 
//       idWaf   = waferNumbToIdWafer(iWaf);
//       mWafers[iWaf] = new StScmWafer(idWaf, mDeadStripP[iWaf],  mDeadStripN[iWaf]);
//     }
//   return *this;
// }

int StScmBarrel::idWaferToWaferNumb(int idWafer)
{
  int iW = (int)((idWafer - mSsdLayer*1000)/100);
  int iL = idWafer - mSsdLayer*1000 - iW*100;
  return ((iL-1)*mNWaferPerLadder + iW -1);   
}

int StScmBarrel::waferNumbToIdWafer(int waferNumb)
{
  int iL = 1+(int)((waferNumb)/mNWaferPerLadder);
  int iW = waferNumb-((iL-1)*mNWaferPerLadder)+1;
  return mSsdLayer*1000 + iW*100 + iL; 
}
