// $Id: StScmBarrel.cc,v 1.7 2006/05/06 00:56:26 fisyak Exp $
//
// $Log: StScmBarrel.cc,v $
// Revision 1.7  2006/05/06 00:56:26  fisyak
// Add local coordinate
//
// Revision 1.6  2005/12/19 10:52:13  kisiel
// Properly encode Cluster Size and Mean strip into the hardware information for the SSDHit
//
// Revision 1.5  2005/11/22 17:39:06  bouchet
// set quality = 100 for the IdTruth
//
// Revision 1.4  2005/11/22 03:57:05  bouchet
// id_mctrack is using for setIdTruth
//
// Revision 1.3  2005/05/17 14:57:20  lmartin
// saving SSD hits into StEvent
//
// Revision 1.2  2005/05/17 14:16:34  lmartin
// CVS tags added
//
#include "StScmBarrel.hh"
#include "StEvent.h"
#include "StSsdHit.h"
#include "StSsdHitCollection.h"
#include "StarClassLibrary/StThreeVectorF.hh"
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

// OBSOLETE ?

// int StScmBarrel::writePointToContainer(St_scm_spt *scm_spt, StSsdHitCollection* ssdHitColl)
// {
//   scm_spt_st spt;
//   StSsdHit *currentSsdHit; 
//   // table size is 148 bytes
//   int currRecord   = 0;
//   int i            = 0;
//   int inContainer =0;
//   StThreeVectorF gPos; StThreeVectorF gPosError; 
//   int hw; float q; unsigned char c;

//   for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
//     {
//      int idCurrentWaf = waferNumbToIdWafer(iWaf);
//      StScmListPoint *sptList = mWafers[iWaf]->getPoint();
//      StScmPoint *pSpt = sptList->first();
     
//      while (pSpt)
//        {
// 	 if (ssdHitColl){ // If Available, Fill the StEvent Container
// 	   for (i = 0 ; i < 3 ; i++){
// 	     gPos[i]      =  pSpt->getXg(i);
// 	     gPosError[i] =  0.0; 
// 	   }
// 	   hw = idCurrentWaf;
// 	   q =  pSpt->getDe(0);
	   
// 	   currentSsdHit = new StSsdHit(gPos,gPosError,hw,q,c);
// 	   currentSsdHit->setIdTruth(pSpt->getNMchit(0));// need to check first = most probable!
// 	   // Add proper hardware info with cluster size and mean strip
// 	   hw  =         
// 	                8                                                                             
// 	     +         16 * idWaferToWaferNumb(idCurrentWaf)                                          
// 	     +       8192 * (int)pSpt->getStripMeanN()                                          
// 	     +    8388608 * ((int)pSpt->getStripMeanP() - (int)pSpt->getStripMeanN() +15)
// 	     +  268435456 * (int)(((pSpt->getClusterSizeN()-1) > 3) ? 3 : (pSpt->getClusterSizeN()-1))
// 	     + 1073741824 * (int)(((pSpt->getClusterSizeP()-1) > 3) ? 3 : (pSpt->getClusterSizeP()-1));

// 	   currentSsdHit->setHardwarePosition(hw);
// 	   inContainer += ssdHitColl->addHit(currentSsdHit);
// 	 }
// 	 spt.flag          = pSpt->getFlag();
// 	 spt.id            = 10000*(pSpt->getNPoint())+idCurrentWaf;
// 	 spt.id_cluster    = pSpt->getNCluster();
// 	 spt.id_globtrk    = 0;
// 	 spt.id_match      = pSpt->getNMatched();
// 	 for (i = 0 ; i < 5 ; i++)
// 	   {	  
// 	     spt.id_mchit[i]   = pSpt->getNMchit(i);
// 	     spt.id_mctrack[i] = 0;
// 	     spt.id_track[i]   = 0;
// 	   }	  
// 	 spt.id_wafer      = idCurrentWaf;
// 	 for (i = 0 ; i < 3 ; i++)
// 	   {	  
// 	     spt.cov[i]        = 0;
// 	     spt.res[i]        = 0;
// 	     spt.x[i]          = pSpt->getXg(i);
// 	     spt.xl[i]         = pSpt->getXl(i);
// 	   }
// 	 for (i = 0 ; i < 2 ; i++)
// 	   {
// 	     spt.mom2[i]       = 0;
// 	     spt.de[i]         = pSpt->getDe(i);
// 	   }
// 	 scm_spt->AddAt(&spt);
// 	 currRecord++;
// 	 pSpt    = sptList->next(pSpt);
//        }
//     }
//   return currRecord;
// }

int StScmBarrel::writePointToContainer(St_scm_spt *scm_spt, StSsdHitCollection* ssdHitColl ,St_scf_cluster *scf_cluster )
{
  scm_spt_st spt;
  scf_cluster_st *on_cluster = scf_cluster->GetTable(); 
  StSsdHit *currentSsdHit; 
  // table size is 148 bytes
  int currRecord   = 0;
  int i            = 0;
  int inContainer =0;
  StThreeVectorF gPos; StThreeVectorF gPosError; 
  int hw; float q; unsigned char c;

  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
    {
     int idCurrentWaf = waferNumbToIdWafer(iWaf);
     StScmListPoint *sptList = mWafers[iWaf]->getPoint();
     StScmPoint *pSpt = sptList->first();
     
     while (pSpt)
       {
	 /*if (ssdHitColl){ // If Available, Fill the StEvent Container
	   for (i = 0 ; i < 3 ; i++){
	     gPos[i]      =  pSpt->getXg(i);
	     gPosError[i] =  0.0; 
	   }
	   hw = idCurrentWaf;
	   q =  pSpt->getDe(0);
	   
	   currentSsdHit = new StSsdHit(gPos,gPosError,hw,q,c);
	   currentSsdHit->setIdTruth(pSpt->getNMchit(0));// need to check first = most probable!
	   currentSsdHit->setHardwarePosition(8+16*idWaferToWaferNumb(idCurrentWaf));
	   
	   inContainer += ssdHitColl->addHit(currentSsdHit);
	   }*/
	 //jb : we fill StEvent after getting the IdMctrack
	 //jb : as it was done too for the strip and clusters --> see StSpaBarrel.cc and StScfBarrel.cc
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
	     //we look on the clusters table to get the IdMctrack info
	     if (spt.id_mchit[i] == 0) spt.id_mctrack[i]=0;
	     else {
	       for(int j = 0 ; j < scf_cluster->GetNRows(); j++){
		 if(spt.id_mchit[i] == on_cluster[j].id_mchit[i]){
		   spt.id_mctrack[i] = on_cluster[j].id_mctrack[i];
		 }
	       }
	     }
	   }
	 //now we fill StEvent and get the correct IdTruth 
	 if (ssdHitColl){ // If Available, Fill the StEvent Container
	   for (i = 0 ; i < 3 ; i++){
	     gPos[i]      =  pSpt->getXg(i);
	     gPosError[i] =  0.0; 
	   }
	   hw = idCurrentWaf;
	   q =  pSpt->getDe(0);
	   
	   currentSsdHit = new StSsdHit(gPos,gPosError,hw,q,c);
	   currentSsdHit->setIdTruth(spt.id_mctrack[0],100);// need to check first = most probable!
	   //cout <<"***in StScmBarrel  test --> IdTruth =" <<currentSsdHit->getIdTruth()<< endl;

	   //looking for the correct clusters...
	   int Id_P_Side = pSpt->getIdClusterP();
	   int Id_N_Side = pSpt->getIdClusterN();

	   StScmListCluster *currentListP_j = mWafers[iWaf]->getClusterP();
	   StScmCluster     *cluster_P_j   = currentListP_j->first();
	   while(cluster_P_j)
	     {
	       if(cluster_P_j->getNCluster()==Id_P_Side) 
		 break;
	       cluster_P_j = currentListP_j->next(cluster_P_j);
	     }


	   StScmListCluster *currentListN_j = mWafers[iWaf]->getClusterN();
	   StScmCluster *cluster_N_j       = currentListN_j->first();
	   while(cluster_N_j)
	     {
	       if(cluster_N_j->getNCluster()==Id_N_Side) 
		 break;
	       cluster_N_j = currentListN_j->next(cluster_N_j);
	     }
	   
	   
	   // Add proper hardware info with cluster size and mean strip
	   hw  =        8                                                                             
	     +         16 * idWaferToWaferNumb(idCurrentWaf);                                          
	   if (cluster_N_j)
	     hw +=       8192 * (int)cluster_N_j->getStripMean()
	       +    268435456 * (int)((cluster_N_j->getClusterSize() > 3) ? 3 : cluster_N_j->getClusterSize()-1);
	   if (cluster_P_j)
	     hw += 1073741824 * (int)((cluster_P_j->getClusterSize() > 3) ? 3 : cluster_P_j->getClusterSize()-1); 
	   if (cluster_P_j && cluster_N_j)
	     hw +=    8388608 * ((int)cluster_P_j->getStripMean() - (int)cluster_N_j->getStripMean() +15);

	    //	   currentSsdHit->setHardwarePosition(8+16*idWaferToWaferNumb(idCurrentWaf));
	   currentSsdHit->setHardwarePosition(hw);
	   currentSsdHit->setLocalPosition(pSpt->getXl(0),pSpt->getXl(1));
	   inContainer += ssdHitColl->addHit(currentSsdHit);
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
