/***********************************************************
 *
 * $Id: StPmdClustering.cxx,v 1.30 2010/05/31 22:13:02 rashmi Exp $
 *
 * Author: based on original routine written by S. C. Phatak.
 *
 ***********************************************************
 *
 * Description: Base class for PMD clusters
 *
 ***********************************************************
 * ***
 * 2004/06/24 : Dipak : findCpvClusters() function removed as because
 * we are doing clustering same way for both the planes
 * 2004/05/05 : Dipak: New Centroid calculation algorithm has been 
 * implemented as corrected by Prof. Phatak. A new function
 * 'CentroidCal()' has been put in place of 'gaussfit()'.
 **
 * $Log: StPmdClustering.cxx,v $
 * Revision 1.30  2010/05/31 22:13:02  rashmi
 * New Clustering routine uses Edep() instead of Adc() of PmdHit; mOptCalibrate Flag set to kTRUE
 *
 * Revision 1.29  2010/05/29 00:47:10  rashmi
 * Added a call to new clustering routines in StPmdClustering
 *
 * Revision 1.28  2010/04/20 23:08:51  rashmi
 * removed refinedcluidet2.dat; no refined clusters in 2010 data
 *
 * Revision 1.27  2010/04/15 06:52:28  rashmi
 * Clustering with option to turn calibration refineclustering on/off
 *
 * Revision 1.26  2007/11/02 11:00:06  rashmi
 * Applying hitcalibration; eta,phi wrt primary vertex
 *
 * Revision 1.25  2007/08/31 10:52:30  rashmi
 * Setting cutoff from SetAdcCutOff(); default cutoff=7
 *
 * Revision 1.24  2007/04/26 04:11:11  perev
 * Remove StBFChain dependency
 *
 * Revision 1.23  2005/06/09 19:43:54  perev
 * Avoid FloatPointException
 *
 * Revision 1.22  2004/11/15 23:35:28  subhasis
 * Refs in centroidCal initialised to solve valgrind error
 *
 * Revision 1.20  2004/09/22 19:24:55  perev
 * Leak fixed + mess with i,j indexes
 *
 * Revision 1.19  2004/09/03 14:31:22  subhasis
 * memset, memcpy used (Gene's suggesstion and order() changed
 *
 * Revision 1.18  2004/08/01 06:41:31  subhasis
 * nclust limit put to <200
 *
 * Revision 1.17  2004/07/26 12:01:31  subhasis
 * sigmaL, sigmaS stored in one place till StPhmdCluster.h modified
 *
 * Revision 1.16  2004/07/21 13:02:31  subhasis
 * refclust called only when incr <2000
 *
 * Revision 1.15  2004/07/19 13:23:34  subhasis
 * checks applied on clust_cell dimension
 *
 * Revision 1.14  2004/07/15 13:32:41  subhasis
 * clust dimension changed
 *
 * Revision 1.13  2004/06/29 07:35:25  subhasis
 * const.h dropped
 *
 * Revision 1.12  2004/06/29 07:13:02  subhasis
 * limit of clust_ fixed
 *
 * Revision 1.11  2004/06/24 13:46:07  subhasis
 * several changes in clustering code
 *
 * Revision 1.7  2003/10/23 04:24:14  perev
 * Stiostream again
 *
 * Revision 1.6  2003/10/14 07:23:18  subhasis
 * Dipak's changes on centroid, correct insure warning
 *
 *
 * Revision 1.5  2003/06/24 17:50:10  Dipak,Tapan
 * 'Gaussfit()' function corrected as done by Prof. Phatak,
 * for better cluster centroid determination and
 * initialization of ncl[i] has been corrected.
 * New condtions introduced in refclust() function.
 *
 *
 * Revision 1.4  2003/05/29 13:11:51  subhasis
 * lev1, lev2 dimension increased from 20 to 50
 *
 * Revision 1.3  2003/05/14 10:49:12  subhasis
 * CPV clustering added
 *
 * Revision 1.2  2003/05/14 10:21:05  Dipak
 * Clustering for CPV plane implemented same as PMD plane
 ***********************************************************/

#include<Stiostream.h>
#include<assert.h>
#include<math.h>
#include"TROOT.h"
#include<TRandom.h>
#include<TBrowser.h>
#include<TPad.h>
#include<StMessMgr.h>
#include<TFile.h>
//#include "StConstants.hh"

#include <TTableSorter.h>

#include "StPmdUtil/StPmdGeom.h"
#include "StPmdUtil/StPmdHit.h"
#include "StPmdClustering.h"
#include "StPmdClusterMaker.h"
#include "StPmdUtil/StPmdClusterCollection.h"
#include "StPmdUtil/StPmdCluster.h"
#include "StPmdUtil/StPmdModule.h"
#include "StPmdUtil/StPmdDetector.h"

#include "StThreeVectorD.hh"
#include "StHelixD.hh"
#include "StPhysicalHelixD.hh"
#include "StThreeVector.hh"
#include "StHelix.hh"
#include "SystemOfUnits.h"
#include "StEventTypes.h"
#include "StEvent.h"



ClassImp(StPmdClustering)

  Double_t d1[96][72],clusters[6][6912], coord[2][96][72]; 
Double_t crd_org[2][96][72];

Int_t iord[2][6912], infocl[2][96][72], inford[3][6912], clno;

const Double_t pi=3.141592653, sqrth=sqrt(3.)/2.;
const Int_t nmx    = 6912;
Float_t cell_frac[200][2000];
//ofstream fout1("cluster.dat");

//ofstream fout("refinedcluidet2.dat");

StPmdGeom *geom=new StPmdGeom(); //! utility class
//-------------------------------------------------
//! constructor for getting PMD and CPV detectors

StPmdClustering::StPmdClustering(StPmdDetector *pmd_det, StPmdDetector *cpv_det):StPmdAbsClustering(pmd_det,cpv_det){
  m_pmd_det=pmd_det;
  m_cpv_det=cpv_det;
  SetAdcCutOff(7.0);
  mVertexPos = StThreeVectorF(0.,0.,0.);
  mOptCalibrate = kTRUE; // calibration is on as default . Set it to kFALSE if calibration not desired
  mOptSimulate = kFALSE; // working with real data in default. Set it to kTRUE if working with simulated data.
  mOptRefineCluster = kTRUE; // refine clustering on as default
  //  cout<<" Calibration is "<<mOptCalibrate<<" Simulated data ="<<mOptSimulate<<"  Refined Clustering ="<<mOptRefineCluster<<endl;
}

//------------------------------
StPmdClustering::~StPmdClustering()
{
}
//---------------------------------
//! finding Pmd and Cpv clusters
void StPmdClustering::findPmdClusters(StPmdDetector *mdet)
{
    cout<<"cutoff="<<cutoff<<endl;
  if(mdet)
    {
      StPmdClusterCollection * pmdclus = new StPmdClusterCollection();
      mdet->setCluster(pmdclus);
      
      Int_t i, i1, i2, j,xpad, ypad,nmx1, incr,idet;
      Int_t gsuper;  
      Double_t  edep, ave;

      for(i=0; i<96; i++){
	for(j=0;j<72;j++){
	  coord[0][i][j]=i+j/2.; coord[1][i][j]=sqrth*j; 
	  crd_org[0][i][j]=i; crd_org[1][i][j]=j; 
	}
      }
      i=0;
      for(Int_t id=1;id<=12;id++)
	{   //! loop for supermodule
	  
	  //!  id has to be 1 to 12, not 0 to 11
	memset(d1[0],0,96*72*sizeof(Double_t));
	  
	  StPmdModule * pmd_mod=mdet->module(id);  //! getting module(id)
	  
	  
	  if(mdet->module_hit(id)>0) 
	    {
	      
	      Int_t nmh=mdet->module_hit(id);  //! total no.of hits in the supermodule in PMD plane
	      //	      cout<<" number of hits in module "<<id<<" is ="<<nmh<<endl;
	      TIter next(pmd_mod->Hits());
	      StPmdHit *spmcl;   //! pointer for hits
	      for(Int_t im=0; im<nmh; im++)
		{
		  
		  spmcl = (StPmdHit*)next();
		  if(spmcl)
		    {
		      ypad=spmcl->Row();          //! row of the hit = 1 - 72
		      xpad=spmcl->Column();       //! column of the hit = 1 - 96
		      //edep=spmcl->Edep();         //! Cell Edep in keV
		      edep=spmcl->Adc();   
		      if(mOptSimulate==kTRUE){edep = spmcl->Edep();}     
		      gsuper = spmcl->Gsuper();   //! supermodule = 1 - 12
		      idet=spmcl->SubDetector();  //! detector(= 1) for Pmd and (=2) for CPV
		      
		      xpad = xpad -1; // for using as array parameter make xpad = 0
		      ypad = ypad -1; // for using as array parameter make ypad = 0
		      // 11th Oct'07 : application of gain factors in ClusterMaker
		      if(mOptCalibrate==kTRUE){
			
			Float_t cellgain=spmcl->GainCell();       //! CellGain
			Float_t smchaingain=spmcl->GainSmChain(); //! SmChain Gain      
			Float_t cellstatus=spmcl->CellStatus();    //! cellstatus
	                Float_t finalfactor = cellgain*smchaingain*cellstatus;
			if(finalfactor>0)edep/=finalfactor;
			if(finalfactor<=0)edep=0;
		      }
		      //if(idet==1)fout1<<gsuper-1<<" "<<xpad<<" "<<ypad<<" "<<cellgain<<" "<<smchaingain<<" "<<cellstatus<<endl;
		      
		      //			fout1<<"gain factors "<<gsuper<<" "<<ypad+1<<" "<<xpad+1<<" "<<edep<<" "<<cellgain<<" "<<smchaingain<<" "<<cellstatus<<" "<<finalfactor<<endl;
		      // end of application of gain factors
		      
		      d1[xpad][ypad]=d1[xpad][ypad]+edep;  //! edep added for each cell
		    }
		}
	      
	      order(idet);  //! order the data according to edep ( largest to smallest )
	      /*
		ave=0.; nmx1=-1;
		for(Int_t jj=0;jj<nmx; jj++)
		{
		i1=iord[0][jj]; i2=iord[1][jj];
		if(d1[i1][i2] > 0.){nmx1=nmx1+1;ave=ave+d1[i1][i2];}
		}
		ave=ave/nmx1;
	      */
	      ave=0.; nmx1=0;
	      for(Int_t jj=0;jj<nmx; jj++)
		{
		  i1=iord[0][jj]; i2=iord[1][jj];
		  if(d1[i1][i2] > 0.){nmx1=nmx1+1;ave=ave+d1[i1][i2];}
		}
	      
	      if(nmx1>0){
		ave=ave/nmx1;   //Earlier averaging was being done using no of max one less
		nmx1--;   // this is done, assuming later he needs one less in nmx1 : Sub
	      }
	      
	//!compare cutoff with the average energy deposited. Has no use in calc.
	      
	      //	      This is now set in Init() or
	      //explicitly by hand from StPmdClusterMaker
	      //AFTER instantiating.
	      //	      cutoff=0.4; //! cutoff(in KeV) is the threshold above which value the data is analysed.
	      
	      /* crude clusters. superclusters are formed. These are separated from each other by cells having edep smaller than cutoff. */
	      incr=crclust(ave, cutoff, nmx1, idet);
	      
	      arrange(incr);  //! arrange cells in each supercluster
	      
	      //	      cout<<"incr="<<incr<<endl;
	      if(incr<2000)refclust(mdet,incr, id, idet,pmdclus);  //! resolve superclusters into clusters //new
	    }
	}
    }
}
//-----------------------------------------

void StPmdClustering::printclust(Int_t i,Int_t m, StPmdCluster* pclust)
{
  
  //! for printing cluster details ( position ( x and y ), and strength
  //! info about # of cells in a cluster to be added. 
  Float_t x0, y0, x, y,xc,yc,zc, cells;
  
  xc = clusters[0][m];yc = clusters[1][m];  //!cluster position
  zc = clusters[2][m];   //! cluster strength
  cells = clusters[3][m];  //! number of cells in the cluster as Float

  Float_t clusigmaL = clusters[4][m];//! spread(sigma) of the cluster along large axis
  Float_t clusigmaS = clusters[5][m];//! spread(sigma) of the cluster along small axis
  
  Float_t cluedep=zc; y0 = yc/sqrth; x0 = xc - y0/2.;
  Float_t clueta,cluphi;
  
  // modified by RR on 1 Nov 2007 to get eta & phi wrt to vertex
  //  cout<<" StPmdClustering: vertex="<<mVertexPos.x()<<","<<mVertexPos.y()<<","<<mVertexPos.z()<<endl;
  if(mVertexPos.x()==0 && mVertexPos.y()==0 && mVertexPos.z()==0) {
    // This loop is for backcompatibility
    geom->DetCell_xy(i,y0+1,x0+1,x,y,clueta,cluphi);
    //    cout<<"clueta,phi="<<clueta<<","<<cluphi<<endl;
  }else{
    // Gives x,y of cluster in STAR Coordinates using x0,y0 wrt SM corner
    geom->DetCell_xy(i,y0+1,x0+1,x,y,clueta,cluphi);
    Float_t xwrtv = x-mVertexPos.x();
    Float_t ywrtv = y-mVertexPos.y();
    // StPmdGeom returns PMDZ as a positive number
    // But in STAR coordinated the PMDZ in negative
    Float_t zreal = -geom->GetPmdZ();
    Float_t zwrtv = zreal-mVertexPos.z();

    //    cout<<"x,y,z real="<<x<<","<<y<<","<<zreal<<endl;
    //    cout<<"x,y,z wrtvertex="<<xwrtv<<","<<ywrtv<<","<<zwrtv<<endl;
    Cluster_Eta_Phi(xwrtv,ywrtv,zwrtv,clueta,cluphi);
    //    cout<<"clueta,phi="<<clueta<<","<<cluphi<<endl;
  }

/////////////////////////////
 
  pclust->setCluX(x);            //! filling 'X' position of Cluster
  pclust->setCluY(y);            //! filling 'Y' position of Cluster
  pclust->setModule(i);          //! filling cluster SM #
  pclust->setCluEdep(cluedep);   //! filling cluster edep
  pclust->setCluEta(clueta);     //! filling cluster eta
  pclust->setCluPhi(cluphi);     //! filling cluster phi
  pclust->setCluSigmaL(clusigmaL); //! filling cluster sigma along large axis
  pclust->setCluSigmaS(clusigmaS); //! filling cluster sigma along small axis
  pclust->setNumofMems(cells);   //! filling # of cells (Float)
  
}
//------------------------------------------------------

//! order the data according to edep ( largest to smallest )
void StPmdClustering::order(Int_t idet)
{
  Double_t d[nmx];
  Int_t curl=0;
  Int_t curh=nmx-1;
      for (int i1=0; i1<96; i1++) {
        for(int i2=0; i2 < 72; i2++){
                          if (d1[i1][i2] > 0.) {
        // Insert where it belongs in descending order
                        int j = 0;
           while ((j<curl) && (d[j]>=d1[i1][i2])) j++; // Find insertion point
              for (int k=curl; k>j; k--) { // Shift other points forward
                   int l = k-1;
                   d[k] = d[l];
                  iord[0][k] = iord[0][l];
                  iord[1][k] = iord[1][l];
                }
         // Insert data
                 d[j] = d1[i1][i2];
                iord[0][j] = i1;
                iord[1][j] = i2;
                curl++;
		      } else {
               // Insert at back end
                iord[0][curh] = i1;
	         iord[1][curh] = i2;
		curh--;
               }
                }
                }
	}

//---------------------------------------------

//! arrange cells in each supercluster
void StPmdClustering::arrange(Int_t incr)
{
  Int_t i, j, k, i1, itest, ihld1, ihld2, ihld3;
  i1=-1; 
  for(i=0; i<96; i++){
    for(j=0; j<72; j++){
      if(infocl[0][i][j] == 1){i1=i1+1;
      inford[0][i1]=infocl[1][i][j]; inford[1][i1]=i; inford[2][i1]=j;
      }
      if(infocl[0][i][j] == 2){i1=i1+1;
      inford[0][i1]=infocl[1][i][j]; inford[1][i1]=i; inford[2][i1]=j;
      }
    }
  }
  for(i=1; i < incr; i++){
    itest=0; ihld1=inford[0][i]; ihld2=inford[1][i]; ihld3=inford[2][i];
    for(j=0; j<i; j++){
      if(itest == 0 && inford[0][j] > ihld1){
        itest=1; 
        for(k=i-1; k>=j; k--){
          inford[0][k+1]=inford[0][k]; inford[1][k+1]=inford[1][k]; 
          inford[2][k+1]=inford[2][k];
        }
        inford[0][j]=ihld1; inford[1][j]=ihld2; inford[2][j]=ihld3;
      }
    }
  }
}

//-------------------------------------------------
/*! Breaking of super clusters into smaller clusters,
 * Cluster positions,strengths,and widths has has been calculated
 * by adopting minimization process. */

void StPmdClustering::refclust(StPmdDetector* m_pmd_det0,Int_t incr, Int_t supmod, Int_t idet,StPmdClusterCollection *pmdclus)
{
  Int_t clno, i, j, k, i1, i2, id, icl, ncl[2000], iordR[2000], itest, ihld;
  Int_t ig, nsupcl;
  Double_t x1, y1, z1, x2, y2, z2, rr;
  Double_t x[2000], y[2000], z[2000];
  Double_t x_org[2000], y_org[2000];
  Double_t xc[2000], yc[2000], zc[2000], d[96][72],rcl[2000],rcs[2000],cells[2000];
  //  Double_t xcl[2000], ycl[2000],clust_cell[100][2000];
  
  
  
  //! clno counts the final clusters
  //! nsupcl =  # of superclusters; 
  //! ncl[i]= # of cells in supercluster i
  
  clno=-1;
  nsupcl=-1;
  
  for(i1=0; i1<96; i1++){
    for(i2=0; i2<72; i2++){
      d[i1][i2]=d1[i1][i2];
    }
  }
  //subhasis 15/11/04 added to initialze arrays for centroidCalc refs
  for(i=0; i<2000; i++){
    x[i]=0.;
    y[i]=0.;
    z[i]=0.;
    xc[i]=0.;
    yc[i]=0.;
    zc[i]=0.;
  }
  /////////////////////////////////
  
  for(i=0; i<2000; i++){
    ncl[i]=-1; // ncl[i] --> initialization starts from '-1'
    rcl[i] = 0.; // initialization of rcs and rcl
    rcs[i] = 0.;
    cells[i] = 0.;
  }
  for(i=0; i<incr; i++)
    {
      if(inford[0][i] != nsupcl)
	{
	  nsupcl=nsupcl+1; 
	}
      ncl[nsupcl]=ncl[nsupcl]+1;
    }
  id=-1;
  icl=-1;
  
  for(i=0; i<=nsupcl; i++)
    {
      Int_t countr = 0;
      if(ncl[i] == 0){ //! Super cluster having 'Single' cell
	id=id+1; icl=icl+1;
	
	countr +=1;
	//!Single cell super-clusters --> single cluster
	
	clno=clno+1; i1=inford[1][id]; i2=inford[2][id];
	
	clusters[0][clno]=coord[0][i1][i2];
	clusters[1][clno]=coord[1][i1][i2];
	clusters[2][clno]=d[i1][i2];
	clusters[3][clno]=1.;
	clusters[4][clno]=0.; // for single cell put sigma as 0. 
	clusters[5][clno]=0.; // for single cell put sigma as 0. 
	
	//Create the StPmdCluster and add to the ClusterCollection
	StPmdCluster *pclust = new StPmdCluster();
	pmdclus->addCluster(pclust); 
	
	printclust(supmod,clno,pclust);
	
	//Get StPmdHit* corresponding to the co-ordinate of single cell
	StPmdHit* phit = GetHit(m_pmd_det0,supmod,crd_org[0][i1][i2],crd_org[1][i1][i2]);
	if(phit)pclust->addHitCollection(phit);
	
      }
      /*
      // Set up this block to work even if the number of cells is>2 RR
      else if((ncl[i] == 1 && mOptRefineCluster==kTRUE) || (ncl[i]>0 && mOptRefineCluster==kFALSE))
      { // Super cluster having 'Two' cells when refined clustering is ON
      // Super clusters having more than one cell when refined is OFF
      //Create the StPmdCluster and add to the ClusterCollection
      */
      else if(ncl[i]==1)
	{
	  StPmdCluster *pclust = new StPmdCluster();
	  pmdclus->addCluster(pclust);
	  id=id+1; icl=icl+1;
	  clno=clno+1; i1=inford[1][id]; i2=inford[2][id]; 
	  x1=coord[0][i1][i2]; y1=coord[1][i1][i2]; z1=d[i1][i2];
	  
	  countr +=1;
	  //Get StPmdHit* corresponding to the co-ordinate of first cell
	  StPmdHit* phit = GetHit(m_pmd_det0,supmod,crd_org[0][i1][i2],crd_org[1][i1][i2]);
	  if(phit)pclust->addHitCollection(phit);
	  
	  id=id+1; i1=inford[1][id]; i2=inford[2][id];
	  x2=coord[0][i1][i2]; y2=coord[1][i1][i2]; z2=d[i1][i2];
	  
	  
	  //Get StPmdHit* corresponding to the co-ordinate of second cell
	  phit = GetHit(m_pmd_det0,supmod,crd_org[0][i1][i2],crd_org[1][i1][i2]);
	  if(phit)pclust->addHitCollection(phit);
	  
	  /*****  Adding for calculating Spread of the cluster *****/
	  Double_t xcell[2],ycell[2],zcell[2],xcl[2000],ycl[2000]; 
	  xcell[0] = x1;
	  ycell[0] = y1;
	  zcell[0] = z1;
	  xcell[1] = x2;
	  ycell[1] = y2;
	  zcell[1] = z2;
	  
	  xcl[i] = (x1*z1+x2*z2)/(z1+z2);
	  ycl[i] = (y1*z1+y2*z2)/(z1+z2);
	  Double_t sumxx,sumyy,sumxy;
	  sumxx = 0.; sumyy = 0.; sumxy = 0.;
	  for(j=0; j<2; j++)
	    {
	      sumxx = sumxx + zcell[j]*(xcell[j]-xcl[i])*(xcell[j]-xcl[i])/(z1+z2);
	      sumyy = sumyy + zcell[j]*(ycell[j]-ycl[i])*(ycell[j]-ycl[i])/(z1+z2);
	      sumxy = sumxy + zcell[j]*(xcell[j]-xcl[i])*(ycell[j]-ycl[i])/(z1+z2);
	    }
	  Double_t b1 = sumxx + sumyy;
	  Double_t c1 = sumxx*sumyy - sumxy*sumxy;
          Double_t dis = b1*b1/4.-c1;
	  if (fabs(dis) < 1e-6) dis = 0.;
          dis = sqrt(dis);
	  Double_t r1=b1/2.+dis;
	  Double_t r2=b1/2.-dis;
	  
	  
	  //! Calculate the Cluster properties for two cell SuperCluster
	  //! In two cell cluster SigmaS wil be Zero
	  if(r1 < r2)
	    {
	      clusters[4][clno] = r2; //SigmaL
	      clusters[5][clno] = r1; //SigmaS
	    }
	  else
	    {
	      clusters[4][clno] = r1; //SigmaL
	      clusters[5][clno] = r2; //SigmaS
	    }
	  
	  clusters[0][clno]=(x1*z1+x2*z2)/(z1+z2); 
	  clusters[1][clno]=(y1*z1+y2*z2)/(z1+z2);
	  clusters[2][clno]=z1+z2;
	  clusters[3][clno]=2.;
	  
	  printclust(supmod,clno,pclust);
	  
	}
      else
	{
	  //	  cout<<" big supercluster ncell="<<ncl[i]+1<<endl;
	  id=id+1; iordR[0]=0;
	  /* super-cluster of more than two cells - broken up into smaller clusters gaussian centers computed. (peaks separated by > 1 cell). Start from top */

	  i1=inford[1][id]; i2=inford[2][id];
	  x[0]=coord[0][i1][i2]; y[0]=coord[1][i1][i2]; z[0]=d[i1][i2];iordR[0]=0;
	  x_org[0]=crd_org[0][i1][i2]; y_org[0]=crd_org[1][i1][i2];
	  for(j=1;j<=ncl[i];j++)
	    {
	      id=id+1;
	      i1=inford[1][id]; i2=inford[2][id];iordR[j]=j;
	      x[j]=coord[0][i1][i2]; y[j]=coord[1][i1][i2]; z[j]=d[i1][i2];
	      x_org[j]=crd_org[0][i1][i2]; y_org[j]=crd_org[1][i1][i2];
	    }
	  //!ordering
	  for(j=1;j<=ncl[i];j++)
	    {
	      itest=0; ihld=iordR[j];
	      for(i1=0;i1<j;i1++){
		if(itest == 0 && z[iordR[i1]] < z[ihld]){
		  itest=1;
		  for(i2=j-1;i2>=i1;i2--){
		    iordR[i2+1]=iordR[i2];
		  }
		  iordR[i1]=ihld;
		}
	      }
	    }
	  //! compute the number of Gaussians and their centers ( first guess )
	  ig=0;
	  xc[ig]=x[iordR[0]]; yc[ig]=y[iordR[0]]; zc[ig]=z[iordR[0]];
	  
	  //-----------------------------------------------------------
	  // If this block is executed then the superclusters larger than
	  // 2 cells are broken into smaller clusters if the right conditions are met.
	  
	  if(mOptRefineCluster==kTRUE){
	    //Find Local maxima
	    for(j=1;j<=ncl[i];j++)
	      {
		itest=-1; x1=x[iordR[j]]; y1=y[iordR[j]];
		for(k=0;k<=ig;k++)
		  {
		    x2=xc[k]; y2=yc[k]; rr=Dist(x1,y1,x2,y2);
		    
		    if( rr >= 1.1 && rr < 1.8 && z[iordR[j]] > zc[k]*0.30)itest=itest+1;
		    if( rr >= 1.8 && rr < 2.1 && z[iordR[j]] > zc[k]*0.15)itest=itest+1;
		    if( rr >= 2.1 && rr < 2.8 && z[iordR[j]] > zc[k]*0.05)itest=itest+1;
		    if( rr >= 2.8)itest=itest+1;
		  } 
		if(itest == ig)
		  {
		    ig=ig+1; xc[ig]=x1; yc[ig]=y1; zc[ig]=z[iordR[j]];
		  }
	      }
	    // End of finding l,ocal maxima	
	  }
	  
	  //-----------------------------------------------------------------
	  
	  memset(cell_frac[0],0,sizeof(cell_frac));
	  //
	  
	  Int_t censtat=CentroidCal(ncl[i],ig,x[0],y[0],z[0],xc[0],yc[0],zc[0],rcl[0],rcs[0],cells[0]);
	  if(censtat==kStOK){
	    
	    icl=icl+ig+1;
	    
	    Float_t temp[2000];
	    Int_t take_cell[2000];
            for(Int_t jk=0; jk<2000; jk++)
	      {
		temp[jk]=0.;
		take_cell[jk]=-999;
	      }
	    
	    //VP	memset(temp,0,2000*sizeof(Float_t)); //it is already zeroed above
	    
	    for(Int_t pb=0;pb<=ig;pb++)
	      {
		for(Int_t jk=0; jk<=ncl[i]; jk++)
		  {
		    if(cell_frac[pb][jk]>temp[jk])
		      {
			take_cell[jk]=pb;
			temp[jk]=cell_frac[pb][jk];
		      }
		  }
	      }
	    
	    
	    
	    
	    
	    //! Assign the cluster properties for SuperCluster having more than Two cells
	    for(k=0; k<=ig; k++)
	      {
		countr +=1;
		clno=clno+1; 
		clusters[0][clno]=xc[k];  
		clusters[1][clno]=yc[k]; 
		clusters[2][clno]=zc[k];
		clusters[3][clno]=cells[k];
		clusters[4][clno]=rcl[k];
		clusters[5][clno]=rcs[k];
		if(clusters[3][clno]==1)
		  {
		    clusters[4][clno]=0.;
		    clusters[5][clno]=0.;
		  }
		
		
		//subhasis , ig = no of gaussians.
		//
		// looping over clusters first and cells within to 
		// create StPmdCluster and attach cells to them
		
		StPmdCluster *pclust = new StPmdCluster();
		pmdclus->addCluster(pclust); 
		
		printclust(supmod,clno,pclust);
		
		for(Int_t jk=0; jk<=ncl[i]; jk++)
		  {// loop over all cells in supercluster
		    
		    //  dist=Dist(x[jk], y[jk], xc[k], yc[k]); 
		    //  if(dist < 2.8){ //changed from 2.1 to 2.8 : dipak
		    //    StPmdHit* phit = GetHit(m_pmd_det0,supmod,x_org[jk],y_org[jk]);
		    //   if(phit)pclust->addHitCollection(phit);
		    // attach the hits
		    // } // if dist loop
		    
		    if(take_cell[jk]==k)
		      {
			StPmdHit* phit = GetHit(m_pmd_det0,supmod,x_org[jk],y_org[jk]);
			if(phit)pclust->addHitCollection(phit);
		      }
		    
		  } //for loop 'jk
	      } //for 'k' loop
	  }//censtat check
	} // 'else' loop
      
      //      if(idet == 2)fout << i <<"\t" << nsupcl << "\t" << clno << "\t" << ncl[i] << "\t" << countr <<"\t" << supmod << "\t" << idet << endl; // idet = 2 for cpv
      
    }// for loop 'i<nsupcl'
  //  cout<<"CLUSTER NUMBER IS "<<clno<<"supmod**"<<supmod<<endl;
  
}
//-------------------------------------------------------


Int_t StPmdClustering::CentroidCal(Int_t ncell,Int_t nclust,Double_t &x,
				  Double_t &y,Double_t &z,Double_t &xc,
				  Double_t &yc,Double_t &zc,
				  Double_t &rcl,Double_t &rcs,Double_t &cells)
{
  Int_t i1,i2;
  Int_t cluster[2000][10];
  Double_t sum,sumx,sum1,sumy,sumxy,sumxx,sumyy;
  Double_t  rr,x1,y1,x2,y2,b,c,r1,r2;
  Double_t xx[2000],yy[2000],zz[2000];
  Double_t xxc[2000],yyc[2000];
  Double_t zzct[2000],cellsc[2000];
  Double_t str[2000],str1[2000], cln[2000];
  Double_t xcl[2000], ycl[2000],clust_cell[200][2000];
  //Initialisation part starts
  double dis;
  if(nclust>=200 || ncell >=2000){
	  cout<<"Number of cluster of Ncell crosses limit "<<nclust<<" "<<ncell<<endl;
	  return kStWarn;
  }

  for(int i=0;i<=nclust;i++)
    {
      xxc[i] = *(&xc+i);
      yyc[i] = *(&yc+i);
      cellsc[i] = 0.;
      zzct[i] = 0.;
    }
  for(int i=0;i<=ncell;i++)
    {
      xx[i] = *(&x+i); 
      yy[i] = *(&y+i);
      zz[i] = *(&z+i);
    }

memset(clust_cell[0],0,200*2000*sizeof(Double_t));
memset(cell_frac[0],0,200*2000*sizeof(Float_t));
memset(cluster[0],0,2000*10*sizeof(Int_t));

  
  //If there is more than one local maxima
  if(nclust>0)
    {
      for(int i=0;i<=ncell;i++)
	{
	  x1 = xx[i];
	  y1 = yy[i];
	  cluster[i][0] = 0;
	  //Checking cells shared betn several clusters.
	  for(int j=0;j<=nclust;j++) // For checking the first layer neighbours
	    {
	      x2 = xxc[j];
	      y2 = yyc[j];
	      rr = Dist(x1,y1,x2,y2);
	      if(rr < 1.) //Check if cluster is one cell unit from the shared cell
		{
		  cluster[i][0]++;
		  i1 = cluster[i][0];
		  cluster[i][i1] = j;
		}
	    }
	  
	  if(cluster[i][0] ==0){  //For checking the second layer neighbours
	    for(int j=0;j<=nclust;j++){
	      x2=xxc[j];
	      y2=yyc[j];
	      rr=Dist(x1,y1,x2,y2);
	      //	      if(rr>=1.73&&rr<2.1)
	      if(rr<= sqrt(3.))
		{
		  cluster[i][0]++;
		  i1=cluster[i][0];
		  cluster[i][i1] = j;
		}
	    } //for loop 'nclust'
	  } //if loop 'cluster[i][0]
	  if(cluster[i][0] ==0){  //For checking the third layer neighbours
	    for(int j=0;j<=nclust;j++){
	      x2=xxc[j];
	      y2=yyc[j];
	      rr=Dist(x1,y1,x2,y2);
	      if(rr<= 2.)
		{
		  cluster[i][0]++;
		  i1=cluster[i][0];
		  cluster[i][i1] = j;
		}
	    }
	  }
	  
	  if(cluster[i][0] ==0){  //For checking the fourth layer neighbours
	    for(int j=0;j<=nclust;j++){
	      x2=xxc[j];
	      y2=yyc[j];
	      rr=Dist(x1,y1,x2,y2);
	      if(rr<= 2.7)
		{
		  cluster[i][0]++;
		  i1=cluster[i][0];
		  cluster[i][i1] = j;
		}
	    } // for loop 'j'
	  } // if loop 'cluster[i][0]'
	} // for loop 'ncell'
      ///Assignment(definition) part is over
      

      //Compute the cluster strength.
memset(str,0,2000*sizeof(Double_t));
memset(str1,0,2000*sizeof(Double_t));
      
      for(int i=0;i<=ncell;i++){
	if(cluster[i][0]!=0){
	  i1 = cluster[i][0];
	  for(int j=1;j<=i1;j++){
	    i2 = cluster[i][j];
	    str[i2] = str[i2] + zz[i]/i1;
	  }
	}
      }
      
      for(int k=0; k<5; k++){
	for(int i=0; i<=ncell; i++){
	  if(cluster[i][0] != 0){
	    i1=cluster[i][0];
	    sum = 0;
	    for(int j=1;j<=i1;j++){
	      sum = sum + str[cluster[i][j]];
	    }
	    for(int j=1;j<=i1;j++){
	      i2 = cluster[i][j];
	      str1[i2] = str1[i2] + zz[i]*str[i2]/sum;
	      clust_cell[i2][i] = zz[i]*str[i2]/sum;
	    }
	  }
	}
	for(int j=0; j<=nclust; j++){
	  str[j]=str1[j]; 
	  str1[j]=0.;
	}
      }


      for(int i=0;i<=nclust;i++){
	sumx=0.;sumy=0.;sum=0.;sum1=0.;
	for(int j=0;j<=ncell;j++){
	  if(clust_cell[i][j] !=0){
	    
	    sumx=sumx+clust_cell[i][j]*xx[j];
	    sumy=sumy+clust_cell[i][j]*yy[j];
	    sum=sum+clust_cell[i][j];
	    sum1=sum1+clust_cell[i][j]/zz[j];
	    //cell contribution 
	    cell_frac[i][j]=clust_cell[i][j]/zz[j];
	    
	    //
	    //cout<<"icl,icell,zzi,clust_cell "<<i<<" "<<j<<" "<<zz[j]<<" "<<clust_cell[i][j]<<" "<<sum1<<endl;
	    //	    cout << xx[j] << " " << yy[j] << " " << clust_cell[i][j] << " " << 
	    //	      clust_cell[i][j]/zz[j] <<" NClust*** "<<nclust<<" "<<i<<" "<<j<<endl; 
	  }
	}
	xcl[i]=sumx/sum; ycl[i]=sumy/sum; cln[i]=sum1;
	//      if(sum1>10)cout<<"many gaus, i,sum1, cln "<<i<<" "<<sum1<<" "<<cln[i]<<endl;
	sumxx=0.; sumyy=0.; sumxy=0.;
	for(int j=0; j<=ncell; j++){
	  sumxx=sumxx+clust_cell[i][j]*(xx[j]-xcl[i])*(xx[j]-xcl[i])/sum;
	  sumyy=sumyy+clust_cell[i][j]*(yy[j]-ycl[i])*(yy[j]-ycl[i])/sum;
	  sumxy=sumxy+clust_cell[i][j]*(xx[j]-xcl[i])*(yy[j]-ycl[i])/sum;
	}
	b=sumxx+sumyy;
	c=sumxx*sumyy-sumxy*sumxy;
        dis = b*b/4.-c;
	if (fabs(dis) < 1e-6) dis = 0.;
        dis = sqrt(dis);
	r1=b/2.+dis;
	r2=b/2.-dis;
	if(r1 < r2){
	  *(&rcl+i) = r2;
	  *(&rcs+i) = r1;
	}else{
	  *(&rcl+i) = r1;
	  *(&rcs+i) = r2;
	}
	//cout<< "1st Check########**** "<<neve<<" "<<xcl[i] << " " << ycl[i] << " " << str[i] <<" "<< xxc[i] << " " << yyc[i] <<" "<<r2<<" "<<r1<<" " << (sumxx) << " " << (sumyy) << " " << sumxy << " " << cln[i] << endl;
	
	// Final assignment 
	*(&xc + i) = xcl[i];
	*(&yc + i) = ycl[i];
	*(&zc + i) = str[i];
	*(&cells + i) = cln[i];
	
      }
    } //if loop 'nclust > 0'
  else
    {
      //      cout << " only one cluster " << endl;
      sumx=0.; sumy=0.; sum=0.; sum1=0.;
      int i=0;
      for(int j=0; j<=ncell; j++)
	{
	  sumx=sumx+zz[j]*xx[j];
	  sumy=sumy+zz[j]*yy[j];
	  sum=sum+zz[j];
	  sum1=sum1+1.;
// cell contribution to cluster
	  cell_frac[i][j]=1;
	  //	cout << xx[j] << " " << yy[j] << " " << zz[j] << endl; 
	}
      xcl[i]=sumx/sum; ycl[i]=sumy/sum; cln[i]=sum1; str[i]=sum;
      //     if(sum1>10)cout<<"one gaus, i,sum1, cln "<<i<<" "<<sum1<<" "<<cln[i]<<endl;
      sumxx=0.; sumyy=0.; sumxy=0.;
      for(int j=0; j<=ncell; j++)
	{
	  sumxx=sumxx+zz[j]*(xx[j]-xcl[i])*(xx[j]-xcl[i])/sum;
	  sumyy=sumyy+zz[j]*(yy[j]-ycl[i])*(yy[j]-ycl[i])/sum;
	  sumxy=sumxy+zz[j]*(xx[j]-xcl[i])*(yy[j]-ycl[i])/sum;
	}
      
      /**** Dipak: added for calculating 'rcl' & 'rcs' similar to above  *****/
      b=sumxx+sumyy;
      c=sumxx*sumyy-sumxy*sumxy;
      dis = b*b/4.-c;
      if (fabs(dis) < 1e-6) dis = 0.;
      dis = sqrt(dis);
      r1=b/2.+dis;
      r2=b/2.-dis;
      if(r1 < r2){
	*(&rcl+i) = r2;
	*(&rcs+i) = r1;
      }else{
	*(&rcl+i) = r1;
	*(&rcs+i) = r2;
      }
      
      *(&xc + i) = xcl[i];
      *(&yc + i) = ycl[i];
      *(&zc + i) = str[i];
      *(&cells + i) = cln[i];
      //     cout<<"cln,cells "<<cln[i]<<" "<<endl;
      
      
    }
  return kStOK;

}

//---------------------------------------------------

/*! distance between centre of two clusters */
Double_t StPmdClustering::Dist(Double_t x1, Double_t y1, Double_t x2, Double_t y2)
{
  return sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2));
}
//----------------------------------------------
/*! Super clusters are constructed by using crclust function and
 * complete algorithm for clustering has been adopted */

Int_t StPmdClustering::crclust(Double_t ave, Double_t cutoff, Int_t nmx1, Int_t idet)
{
   Double_t xx, yy, d[96][72];
   Int_t i,j,k,id1,id2,icl,clust[2][3000], numcell, cellcount;
   Int_t jd1,jd2,icell;

   //! constants used for calculating coordinate of neighbouring cells
   static Int_t neibx[6]={1,0,-1,-1,0,1}, neiby[6]={0,1,1,0,-1,-1};


   memcpy(d,d1,96*72*sizeof(Double_t));
   
   for (j=0; j < 96; j++){
     for(k=0; k < 72; k++){
       for (i=0; i < 2; i++){infocl[i][j][k] = 0;} //! initialize infocl[2][96][72]
     }
   }
   cellcount=0;
   for(i=0; i < nmx; i++){
     id1=iord[0][i]; id2=iord[1][i];
     if(d[id1][id2] <= cutoff){infocl[0][id1][id2]=-1;}
   }
   /*! crude clustering begins. Start with cell having largest adc count and 
    * loop over the cells in descending order of adc count */
   icl=-1;
   for(icell=0; icell <= nmx1; icell++){
     
     id1=iord[0][icell]; id2=iord[1][icell]; xx=id1+id2/2.; yy=sqrth*id2;
     if(infocl[0][id1][id2] == 0 ){
       
       /*! icl --cluster #, numcell -- # of cells in a cluster, clust -- stores 
	* coordinates of the cells in a cluster, infocl[0][i1][i2] is 1 
	*for primary and 2 for secondary cells, infocl[1][i1][i2] stores cluster # */
       
       icl=icl+1; numcell=0;
       for(i=0; i < 3000; i++){
         clust[0][i]=0; clust[1][i]=0;
       }
       clust[0][numcell]=id1; clust[1][numcell]=id2;
       infocl[0][id1][id2]=1; infocl[1][id1][id2]=icl;
       //! check for adc count in neighbouring cells. If ne 0 include them in this clust
       for(i=0; i<6; i++){
         jd1=id1+neibx[i]; jd2=id2+neiby[i];
         if( (jd1 >= 0 && jd1 < 96) && (jd2 >= 0 && jd2 < 72) && 
	     d[jd1][jd2] > cutoff && infocl[0][jd1][jd2] == 0)
	   {
	     numcell=numcell+1; clust[0][numcell]=jd1; clust[1][numcell]=jd2;
	     infocl[0][jd1][jd2]=2; infocl[1][jd1][jd2]=icl;
	     xx=jd1+jd2/2.; yy=sqrth*jd2; 
	   }
       }
       for(i=1;i < 3000;i++){
         if(clust[0][i] != 0){
           id1=clust[0][i]; id2=clust[1][i];
           for(j=0; j<6 ; j++){
             jd1=id1+neibx[j]; jd2=id2+neiby[j];
             if( (jd1 >= 0 && jd1 < 96) && (jd2 >= 0 && jd2 < 72) && 
                 d[jd1][jd2] >  cutoff && infocl[0][jd1][jd2] == 0 ){
               infocl[0][jd1][jd2]=2; infocl[1][jd1][jd2]=icl;
               numcell=numcell+1;
               clust[0][numcell]=jd1; clust[1][numcell]=jd2;
               xx=jd1+jd2/2.; yy=sqrth*jd2;
             }
           }
         }
       }
       cellcount=cellcount+numcell+1;
     }
   }
   //   cout<<idet<<" CRCLUST**, icl  "<<icl<<" ********* "<<cellcount<<endl;
   return cellcount;
}
//------------------------------------------

StPmdHit*
StPmdClustering::GetHit(StPmdDetector* pdet,Int_t id,Double_t xc, Double_t yc)
{
  Int_t xpad,ypad,super;
  Int_t nmh=pdet->module_hit(id);
  StPmdModule * pmod=pdet->module(id);
  TIter next(pmod->Hits());
  StPmdHit *spmcl;
  // Loop over hits for each SM
  for(Int_t im=0; im<nmh; im++)
    {
      spmcl = (StPmdHit*)next();
      if(spmcl){
	ypad=spmcl->Row();
	xpad=spmcl->Column();
	super = spmcl->Gsuper();
	if(int(yc+1)==ypad && int(xc+1)==xpad && id == super)   return spmcl;
      }
    }
  return NULL;
}
//--------------------------------------------------------
// This routine was copied from StPmdGeom with the difference that it uses
// zreal ( PMDZ wrt vertex) instead of mzreal ( PMDZ wrt 0,0,0)                                                                         
void StPmdClustering::Cluster_Eta_Phi(Float_t xreal,Float_t yreal,Float_t zreal,Float_t& eta,Float_t& phi){
  Float_t rdist = (TMath::Sqrt(xreal*xreal + yreal*yreal))/TMath::Abs(zreal);
  Float_t theta = TMath::ATan(rdist);
  //Bedanga & pawan - added theta !=0.
  // The negative sign is avoided to take care of -ve PMDZ
  if(theta !=0.){ eta = TMath::Log(TMath::Tan(0.5*theta));}
  if( xreal==0) {
    if(yreal>0) {phi = 1.571428;}
    if(yreal<0) {phi = -1.571428;}
  }
  if(xreal != 0) {phi = atan2(yreal,xreal);}

}

//---------------------------------
//! finding Pmd and Cpv clusters
Bool_t StPmdClustering::findPmdClusters2(StPmdDetector *mdet)
{
  cout<<"cutoff in findPmdClusters2="<<cutoff<<endl;
  //cout<<" Finding cluster for PMD detector"<<endl;
  
  
  // Declared in StpmdClusterin.h now
  //  StPmdClusterCollection * pmdclus;
  pmdclus = new StPmdClusterCollection();
  mdet->setCluster(pmdclus);
  
  if(mdet){
    
    
    for(Int_t ism = 1; ism<=12; ism++){
      
      TClonesArray * mPmdSuperClusters = new TClonesArray("TList",1000);
      
      // Getting the pointer to the supermodule from the detector
      // notice that StPmdDetector->module(xx) takes xx from 1 to 12 
      StPmdModule * pmd_mod = mdet->module(ism); //
      Int_t nhits = mdet->module_hit(ism);
      //      cout<<" Number of entries in "<<ism<<" module is "<<nhits<<endl;
      
      // if no hits, go to the next supermodule
      if(nhits<=0) continue;
      // pass on the StPmdModule to make superclusters
      // All the superclusters are filled into the mPmdSuperClusters TClonesArray
      Int_t nSuperClusters = MakeSuperClusters(ism,pmd_mod,mPmdSuperClusters);
      //      cout<<" number of superclusters on module "<<ism<<" is "<<nSuperClusters<<endl;
      
      // Going to form clusters now
      if (nSuperClusters==0|| nSuperClusters > nhits){
	cout<<"Warning!!! nent/nsuper "<<nhits<<"/"<<nSuperClusters<<endl;
	cout<<"The hits of this module will not be added to the outgoing TClonesArray of hits"<<endl;
	//	      return kStErr;
      }else{
	//	cout<<"Going to MakeClusters()"<<endl;
	//	Int_t gotclusters = MakeClusters(mPmdSuperClusters,pmdclus);
	Int_t gotclusters = MakeClusters(mPmdSuperClusters);
	
	if (gotclusters==0){
	  cout<<" Did not get any clusters in this sm"<<endl;
	}
	//      if (mOptDebug)
      }
      mPmdSuperClusters->Delete();
      delete mPmdSuperClusters;
    }
  }
  cout<<"Number of clusters in findPmdClusters2 = "<<pmdclus->Nclusters()<<endl;
  
  return kStOK;
}

//---------------------------------------------------------------------------------------

Int_t StPmdClustering::MakeSuperClusters(Int_t modnum,StPmdModule * mpmdMod , TClonesArray* mPmdSuperClusters){

  Int_t Columnneigh[6] = {-1,-1,0,1,1,0};
  Int_t Rowneigh[6] = {0,1,1,0,-1,-1};
  //  cout<<"In MakeSuperClusters: CutOff="<<cutoff<<endl;

  //Initializing the hitmap
  
  //Row and column numbers start from 0
  Int_t hitmap[NRowMax][NColumnMax];
  Int_t Flag[NRowMax][NColumnMax];
  for( Int_t ncellX = 0; ncellX < NRowMax; ncellX++ ){
    for( Int_t ncellY = 0; ncellY < NColumnMax; ncellY++ ){
      hitmap[ncellX][ncellY]=-1;
      Flag[ncellX][ncellY]=0;     
    }
  }
  // Get pointer to the TObjArray of pmd hits in this module
  TObjArray * PmdHits = mpmdMod->Hits();
  //  PmdHits->Sort();
  // Sorts in descending order
  PmdHits->Sort(0);
  // GetNumber of hits in the module
  Int_t Ncell_mod = PmdHits->GetEntries();
  
  // Just accountancy
  Int_t NWithin=0;
  Int_t NAccCut=0;
  // Filling hitmap will all the hits. We start making superclusters
  // with the hitmap.
  //  cout<<" number of entries in module number "<<modnum<<" is "<<Ncell_mod<<endl;
  for (Int_t i = 0; i < Ncell_mod; i++){

    StPmdHit * mPmdHit = (StPmdHit*)PmdHits->At(i);
    //    cout<<" mPmdHit deatils mod "<<mPmdHit->Gsuper()<<" detector="<<mPmdHit->SubDetector()<<endl;

    // Add those cells which have adc above cutoff to the hitmap.
    // The hitmap hold the index of the hit in the PmdHits TObjArray
    if((mPmdHit->Row()>NRowMax||mPmdHit->Row()<=0)||
       (mPmdHit->Column()>NColumnMax||mPmdHit->Column()<=0)){
      cout<<"hit out of range :"<<mPmdHit->Row()-1<<"/"<<mPmdHit->Column()-1<<endl;
    }else{  
      if (mPmdHit->Edep() > cutoff ){
	hitmap[mPmdHit->Row()-1][mPmdHit->Column()-1]= i;
	NWithin++;
      }else {
	//	cout<<"Adc less than cutoff Adc/cutoff "<<
	//mPmdHit->Edep()<<"/"<<cutoff<<endl;
	hitmap[mPmdHit->Row()-1][mPmdHit->Column()-1] = -1;
	Flag[mPmdHit->Row()-1][mPmdHit->Column()-1]=2; 
	NAccCut++;
      }
    }   
  }
  // hitmap ready here 

  // Account statement
  if ((NWithin+NAccCut)!=Ncell_mod){
    cout<<" Some hits outside hitmap NWithin/NAccCut/NTotal "<<
      NWithin<<"/"<<NAccCut<<"/"<<Ncell_mod<<endl;
  }
  if (NWithin==0){
    return 0;
  }
  //  cout<<" NWithin/NAccCut/NTotal "<<NWithin<<"/"<<NAccCut<<"/"<<Ncell_mod<<endl;


 //Make superclusters now
  //Initializing the number of Superclusters
  Int_t  nPmdSuperCluster = -1;

  for(Int_t icell = 0; icell < Ncell_mod; icell++){
    StPmdHit *mPmdHit = (StPmdHit*)PmdHits->At(icell);
    //    cout<<" flag of hit["<<icell<<"] is Flag["<<mPmdHit->Row()-1<<"]["<<mPmdHit->Column()-1<<"]="<<Flag[mPmdHit->Row()-1][mPmdHit->Column()-1]<<endl;
    if((mPmdHit->Row()>NRowMax||mPmdHit->Row()<=0)||
       (mPmdHit->Column()>NColumnMax||mPmdHit->Column()<=0)){
      cout<<"hit out of range :"<<mPmdHit->Row()-1<<"/"<<mPmdHit->Column()-1<<endl;
      continue;
    }
    
    if (Flag[mPmdHit->Row()-1][mPmdHit->Column()-1]==0){
      nPmdSuperCluster++;
      //      cout<<"npmdsuperclusters="<<nPmdSuperCluster<<endl;
      TList *  mPmdSuperCluster = (TList*)mPmdSuperClusters->New(nPmdSuperCluster);
      //      cout<<" made a new supercluster with address "<<mPmdSuperCluster<<" in list with address "<<mPmdSuperClusters<<endl;
      TIter ClusterListIter(mPmdSuperCluster);
      //Add hit to the supercluster and put flag =1
      mPmdSuperCluster->Add(mPmdHit);
      //            cout<<" added hit # "<<icell<<" to supercluster # "<<nPmdSuperCluster<<endl;
      Flag[mPmdHit->Row()-1][mPmdHit->Column()-1]=1;
      //make a list of its neighbours and their neighbours
      
      for(Int_t k=0;k<mPmdSuperCluster->GetSize();k++){
	StPmdHit* clusterhit = (StPmdHit*)mPmdSuperCluster->At(k);
	Int_t clusterhitRow = clusterhit->Row();
	Int_t clusterhitColumn = clusterhit->Column();
	if(Flag[clusterhitRow-1][clusterhitColumn-1]!=1) cout<<"???????"<<endl;
	//checking neighbours
	for( Int_t ineigh = 0;ineigh < 6; ineigh++ ){
	  Int_t neighbourRow = clusterhitRow + Rowneigh[ineigh];
	  Int_t neighbourColumn = clusterhitColumn + Columnneigh[ineigh];
	  Int_t boundary = CheckBoundary(modnum,neighbourRow-1,neighbourColumn-1);

	  if (boundary==0 && hitmap[neighbourRow-1][neighbourColumn-1]!=-1){

	    //	  if (hitmap[neighbourRow-1][neighbourColumn-1]!=-1){
	    //	    if(mOptDebug)cout<<"neigh row/col "<<neighbourRow<<"/"<<neighbourColumn<<endl;
	    StPmdHit* neighbour = (StPmdHit*)PmdHits->At(hitmap[neighbourRow-1][neighbourColumn-1]);
	    //	    cout<<" ineigh = "<<ineigh<<" Got hit number "<<hitmap[neighbourRow-1][neighbourColumn-1]<<" with adc"<<neighbour->Edep()<<" as connecting hit. k= "<<k<<" SpCl size is "<<mPmdSuperCluster->GetSize()<<endl;
	    if (neighbour){
	      if (neighbour->Edep()>cutoff
		  && Flag[neighbourRow-1][neighbourColumn-1]==0){
		mPmdSuperCluster->Add(neighbour);
		//		cout<<"The size of supercluster is "<<mPmdSuperCluster->GetSize()<<endl;
		//		cout<<"added "<<hitmap[neighbourRow-1][neighbourColumn-1]<<" with adc"<<neighbour->Edep()<<" as connecting hit"<<endl;
		Flag[neighbourRow-1][neighbourColumn-1]=1;
	      }//if neighbour is above cutoff
	    }//if neighbour exists and is not already part of the supercluster
	  }//if hitmap exists
	} //looping over 6 neighbours
      }//looping over superclusterlist (for neighbours)
      //      if (mOptDebug && (mPmdSuperCluster->GetSize()>MaxSuperSize)){
      if (mPmdSuperCluster->GetSize()>MaxSuperSize){
	cout<<"The size of supercluster "<<mPmdSuperCluster->GetSize()<<" exceeds MaxSuperSize("<<MaxSuperSize<<")"<<endl;
	return 0;
      }	
      mPmdSuperCluster->Sort(0);
      
    }//if flag=0
    
  } // looping over all hits TObjArray     
  
  //  cout<<"going out of MakerSuperClusters with ";
  Int_t Nentries = mPmdSuperClusters->GetEntries();
  //  cout<<"Got "<<Nentries<<" Superclusters."<<endl;
  
  return Nentries;
}

//--------------------------------------------------------------------------------------

//Int_t StPmdClustering::MakeClusters(TClonesArray * mPmdSuperClusters, StPmdClusterCollection * pmdclus)
Int_t StPmdClustering::MakeClusters(TClonesArray * mPmdSuperClusters)
{  
  
  //   cout<<" In MakeCluster"<<endl;
  
  Int_t nLocalPeak=0;
  Int_t sum_nclusters = 0;
  //  cout<<"MakeCluster::# of superclusters is "<<mPmdSuperClusters->GetEntries()<<endl;
  //  cout<<" address of mPmdSuperCluster list is "<<mPmdSuperClusters<<endl;
  for(Int_t i=0;i<mPmdSuperClusters->GetEntries();i++){
    //    cout<<" Got supercluster number "<<i<<" / "<<mPmdSuperClusters->GetEntries()<<endl;
    TList *mPmdSuperCluster = (TList*)mPmdSuperClusters->At(i);
    Int_t ncell = mPmdSuperCluster->GetSize();
    //    cout<<"# of cells in "<<i<<" superclusters is "<<ncell<<" address is "<<mPmdSuperCluster<<endl;
    //Initialzation of cluster parameters
    //    Int_t nmod,det;
    if (ncell==0) {
      break;
      cout<<"zero cells in this supercluster!!!"<<endl;
    }else{
      
      if ((ncell<3 && mOptRefineCluster==kTRUE)||(mOptRefineCluster==kFALSE)){
	
	//       cout<<" converting supercluster to cluster with ncell = "<<ncell<<endl;
	
	StPmdCluster *pcluster = new StPmdCluster();;
	pmdclus->addCluster(pcluster);
	Float_t hitfract[ncell];
	
	for(Int_t j = 0; j < ncell; j++ ){
	  StPmdHit * phit = (StPmdHit*)mPmdSuperCluster->At(j);
	  pcluster->addHitCollection(phit);
	  hitfract[j]=1.0;
	}
	//	Float_t nmem = 
	BuildCluster(pcluster,hitfract);
	//	cout<<" No refine and so I have a cluster with "<<nmem<<" cells"<<endl;
	
	sum_nclusters++;
      }
      
      // This option is used only if ncell >=3 and Refine Clustering is ON
      else{ 
	//	cout<<" Got a big cluster and Refined clustering ON says I have to break it"<<endl;
	// jLocalPeak is array of the hits in a supercluster which are
	// Local peaks. Since the index of hits in a supercluster starts
	// with 0, the jLocalPeaks are initialised to -1 and not 0.
	Int_t jLocalPeak[MaxLocalPeaks];
	for (Int_t j=0;j<MaxLocalPeaks;j++){
	  jLocalPeak[j] = -1; // 
	}
	// Get local peaks for SuperCluster with ncell
	Int_t *pLocalPeak = &jLocalPeak[0];
	//	cout<<"Choice="<<LocalPeakChoice<<BreakSuperClusterChoice<<SigmaChoice<<endl;
	
	nLocalPeak = GetLocalPeaks(pLocalPeak,mPmdSuperCluster);
	
	//       	if (mOptDebug)
	//	cout<<i<<"\t"<<" pLocalPeak = "<< pLocalPeak <<"\t"<<"nLocalPeak = "<<nLocalPeak<<endl;
	
	// If there is only 1 local peak then we dont have to break the supercluster
	if(nLocalPeak==1){
	  
	  StPmdCluster *pcluster = new StPmdCluster();;
	  pmdclus->addCluster(pcluster);
	  Float_t hitfract[nLocalPeak];
	  
	  // put all the hits into this cluste collection and send it to builtcluster
	  for(Int_t ihit = 0;ihit<mPmdSuperCluster->GetEntries();ihit++){
	    hitfract[ihit]=1.0;
	    StPmdHit * phit = (StPmdHit*)mPmdSuperCluster->At(ihit);
	    pcluster->addHitCollection(phit);
	  }
	  Float_t nmem = BuildCluster(pcluster,hitfract);
	  
	  if(nmem==0) cout<<" 1 local peak only  so I have a cluster with "<<nmem<<" cells"<<endl;
	}
	
	if (nLocalPeak>1 && nLocalPeak<MaxLocalPeaks){
	  sum_nclusters+=nLocalPeak;
	  /*
	  cout<<" sum_clusters = "<<sum_nclusters<<endl;
	  cout<<" number of hits in this SuperCluster ="<<mPmdSuperCluster->GetEntries()<<endl;
	  cout<<" Number of clusters in ClusterCollection is "<<pmdclus->Nclusters()<<endl;
	  cout<<" address of jLocalPeak = "<<pLocalPeak<<endl;
	  cout<<" address of mPmdSuperCluster = "<<mPmdSuperCluster<<endl;
	  cout<<" address of pmdclusCollection = "<<pmdclus<<endl;
	  */
	  //	  Bool_t broken = BreakSuperCluster(nLocalPeak,pLocalPeak,mPmdSuperCluster,pmdclus);
	  Bool_t broken = BreakSuperCluster(nLocalPeak,pLocalPeak,mPmdSuperCluster);
	  //	  if(mOptDebug)
	  
	  if(broken==1){
	    cout<<" too many local peaks I think"<<endl;
	  }
	}else{
	  // the module was cut due to number of localpeaks>MaxLocalPeaks
	  //if (mOptDebug)
	  cout<<"Module cut due to nLocalPeaks>MaxLocalPeaks"<<endl;
	  return 0;
	}
      }
    }	
  }
  //  cout<<"out of makeclusters"<<endl;
  return sum_nclusters;
}


//--------------------------------------------------------------------------------------
//------------------------------------------------------------------
//Returns cell xy in units of the cell size wrt to the cornermost cell.
// The corner most cell has row=0;col=0;
//CAUTION: Not absolute xy.

void StPmdClustering::CellXY(Int_t row, Int_t column, Float_t& xx, Float_t& yy){
  //  column = column -1;
  //  row = row -1;
  xx = (Float_t)(column + 0.5*row);
  yy = 0.5*sqrt(3.)*row;
}
//---------------------------------------------------------------------

Int_t StPmdClustering::CheckBoundary(Int_t nmod,Int_t numRow, Int_t numColumn){
  if ((numRow>=0 && numRow< NRowMax) &&
      (numColumn>=0 && numColumn<NColumnMax))
    {  //  cout<<"In checkboundary"<<endl;
      return 0;}
  else return 1;
}
//---------------------------------------------------------------------

Int_t StPmdClustering::GetLocalPeaks(Int_t* jLocalPeak,TList *mPmdSuperCluster){
  
  Int_t nLocalPeak =-1;
  Int_t ncell = mPmdSuperCluster->GetSize();
  Float_t xLocalPeak[ncell];
  Float_t yLocalPeak[ncell];
  Float_t adcLocalPeak[ncell];

  //The highest peak of the supercluster is definately a peak so:
  nLocalPeak++;
  if(nLocalPeak>=MaxLocalPeaks) cout<<" Number of Local peaks is "<<nLocalPeak+1<<" which exceeds the array size MaxLocalPeaks ="<<MaxLocalPeaks<<endl;
  StPmdHit * phitj = (StPmdHit*)mPmdSuperCluster->At(0);
  Float_t hitx1 = 0;   Float_t hity1 = 0;
  CellXY(phitj->Row()-1,phitj->Column()-1,hitx1,hity1); 
  Int_t sm = phitj->Gsuper();
  jLocalPeak[nLocalPeak]   = 0;
  xLocalPeak[nLocalPeak]   = hitx1;
  yLocalPeak[nLocalPeak]   = hity1;
  adcLocalPeak[nLocalPeak] = phitj->Edep();
  //    cout<<"Out of ncell ="<<ncell<<" cell 0 has adc ="<<phitj->adc()<<endl;
  //cout<<"nLocalPeak =  "<<nLocalPeak<<
  //      " jLocalPeak ="<<jLocalPeak[nLocalPeak]<<
  //      " adcLocalPeak =  "<<adcLocalPeak[nLocalPeak]<<
  //      " x,y = "<<xLocalPeak[nLocalPeak]<<","<<yLocalPeak[nLocalPeak]<<endl;
  // Check the next highest cell........ 
  for (Int_t j = 1; j < ncell; j++){
    StPmdHit * phitj = (StPmdHit*)mPmdSuperCluster->At(j);
    //      cout<<"cell "<<j<<" has adc ="<<phitj->adc()<<endl;
    Float_t hitx = 0;   Float_t hity = 0;
    CellXY(phitj->Row()-1,phitj->Column()-1,hitx,hity);
    //......against all the previous LocalPeaks
    Int_t nclear = -1;
    for (Int_t k = 0; k <= nLocalPeak; k++){ 
      Float_t gap = Dist(hitx,hity,xLocalPeak[k],yLocalPeak[k]);
      //      Float_t fadc = 1.0*phitj->adc()/adcLocalPeak[k];
      //the distances are in units of cell sizes. 
      if( gap >= 1.1 && gap < 1.8 && phitj->Edep() > adcLocalPeak[k]*0.30) nclear++;
      if( gap >= 1.8 && gap < 2.1 && phitj->Edep() > adcLocalPeak[k]*0.15) nclear++;
      if( gap >= 2.1 && gap < 2.8 && phitj->Edep() > adcLocalPeak[k]*0.05) nclear++;
      if( gap >= 2.8 ) nclear++;
    }      
    if (nclear==nLocalPeak){//cleared against all previous local peaks
      // then it is another local peak so add to list
      nLocalPeak++;
      if (nLocalPeak<MaxLocalPeaks){
	jLocalPeak[nLocalPeak]   = j;
	xLocalPeak[nLocalPeak]   = hitx;
	yLocalPeak[nLocalPeak]   = hity;
	adcLocalPeak[nLocalPeak] = phitj->Edep();
      }else{
	cout<<"number of local peaks is "<<nLocalPeak<<" which is more than "<<MaxLocalPeaks<<" for a supercluster of size ="<<ncell<<" in sm number ="<<sm<<endl;
	return 0;
      }
    }
  }
  return ++nLocalPeak;
    
}
//----------------------------------------------------------------------
void StPmdClustering::Cell_eta_phi(Float_t xreal,Float_t yreal,Float_t& eta,Float_t& phi){

  // Converts xyz from STAR 0,0,0 to actual vertex
  xreal = xreal - mVertexPos.x();
  yreal = yreal - mVertexPos.y();
  Float_t zreal = -1.0*geom->GetPmdZ();
  zreal = zreal - mVertexPos.z();
  //  cout<<"IN Clustering: xreal="<<xreal<<" yreal="<<yreal<<" zreal="<<zreal;
  
  Float_t rdist = (TMath::Sqrt(xreal*xreal + yreal*yreal))/zreal;
  
  Int_t flag = -1;
  if (rdist<0.) {
    flag = +1;
    rdist=TMath::Abs(rdist);
  }

  Float_t theta = TMath::ATan(rdist);
  if(theta !=0.)eta = flag*TMath::Log(TMath::Tan(0.5*theta));
  if( xreal==0) {
    if(yreal>0) phi = TMath::Pi()/2.0;
    if(yreal<0) phi = -1.0*TMath::Pi()/2.0;
  }
  if(xreal != 0) phi = atan2(yreal,xreal);

  //  cout<<" theta="<<theta<<" eta="<<eta<<" phi="<<phi<<endl;
}


//-----------------------------------------------------------------------------
Float_t StPmdClustering::BuildCluster( StPmdCluster *pcluster,Float_t* hitfract){
  

  // Get the hit collection from the pcluster and built the cluster
  TObjArray* hitCollection = pcluster->HitCollection();
  Int_t ncell = hitCollection->GetEntries();
  
  Float_t adchit[ncell],xhit[ncell],yhit[ncell];
  Float_t SigmaL = 0, SigmaS = 0;
  Float_t cluX=0, cluY=0, cluAdc=0;
  Float_t eta=0,phi=0;
  
  
  //  cout<<" Got hit collection with size = "<<ncell<<endl;
  // Getting the module number of the cluster from the module and detector number of hit.
  // and setting the module and number of member hits of pcluster
  StPmdHit* phit = (StPmdHit*)hitCollection->At(0);
  Int_t nmod = phit->Gsuper();
  Int_t det = phit->SubDetector();//1 for pmd and 2 for cpv
  //	    cout<<" nmod = "<<nmod<<" det = "<<det<<endl;
  //	    nmod =  12*(2-det) + nmod-1;//to go to 0-23 format
  Int_t nmodclu =  12*(2-det) + nmod;//to go to 0-23 format
  //	    cout<<" New module number for clusters is "<<nmodclu<<endl;
  pcluster->setModule(nmodclu);
  Float_t nmem = Float_t(ncell*1.0);
  pcluster->setNumofMems(nmem);
  
  
  // Now to calculate the eta,phi and sigmaS,L and ADC of the cluster

  // For a single cell cluster
  if (ncell==1){
    SigmaL=0;
    SigmaS=0;
    StPmdHit * phit = (StPmdHit*)hitCollection->At(0);

    //    cout<<" Building cluster for 1 cell with adc ="<<phit->Edep()<<endl;
    // Eta phi is useless here since it is wrt 0,0,0 and not wrt vertex
    geom->IntDetCell_xy(nmod,(phit->Row()),(phit->Column()),cluX,cluY,eta,phi);
    cluAdc = phit->Edep()*1.0;
    //    cout<<" Cluster position and adc is  is "<<cluX<<" "<<cluY<<" "<<cluAdc<<endl;

  }else{ 
    // If there are more than 1 hit then
    // loop over all the hits
    Float_t sumx=0,sumy=0;
    for(Int_t j = 0; j < ncell; j++ ){
      StPmdHit * phit = (StPmdHit*)hitCollection->At(j);
      if (!phit) cout<<" Did not get the phit I was hoping for "<<endl;
      if (!phit) break;
      //	  cout<<" Hi I am here with phit at address "<<phit<<endl;
      
      
      Float_t xreal=0,yreal=0;
      geom->IntDetCell_xy(nmod,(phit->Row()),(phit->Column()),xreal,yreal,eta,phi);
      adchit[j] = hitfract[j]*phit->Edep();
      xhit[j] = xreal;
      yhit[j] = yreal;
      //      cout<<"Hit in MakeCluster:"<<nmodclu<<","<<det<<","<<phit->Row()<<","<<phit->Column()<<","<<phit->Edep()<<","<<xreal<<","<<yreal<<","<<eta<<","<<phi<<endl;
      //      cout<<"Hit in BuildCluster:"<<nmodclu<<","<<det<<","<<phit->Row()<<","<<phit->Column()<<","<<hitfract[j]<<","<<phit->Edep()<<","<<xreal<<","<<yreal<<endl;
      sumx+=adchit[j]*xhit[j];
      sumy+=adchit[j]*yhit[j];
      cluAdc+=adchit[j];
      
    }
    
    cluX = sumx/cluAdc;
    cluY = sumy/cluAdc;
    //    if(ncell>3) cout<<"Cluster: Xcen,Ycen,cluADC="<<cluX<<","<<cluY<<","<<cluAdc<<endl;
    
    Float_t sumxx=0,sumxy=0,sumyy=0;
    for(Int_t j=0;j<ncell;j++){
      sumxx+=adchit[j]*(xhit[j]-cluX)*(xhit[j]-cluX)/cluAdc;
      sumyy+=adchit[j]*(yhit[j]-cluY)*(yhit[j]-cluY)/cluAdc;
      sumxy+=adchit[j]*(yhit[j]-cluY)*(xhit[j]-cluX)/cluAdc;
    }
    
    Float_t b=sumxx+sumyy;
    Float_t c=sumxx*sumyy-sumxy*sumxy;
    //      cout<<"sumxx,sumyy,sumxy,b,c="<<sumxx<<","<<sumyy<<","<<sumxy<<","<<b<<","<<c<<endl;
    Float_t r1=b/2.+sqrt(b*b/4.-c);
    Float_t r2=b/2.-sqrt(b*b/4.-c);
    //    cout<<"r1,r2="<<r1<<","<<r2<<endl;    
    if (r1<r2) {
      SigmaS = r1;
      SigmaL = r2;
    }else{
      SigmaS = r2;
      SigmaL = r1;
    }
    if(ncell==2) SigmaS=0;
  }

  // Get etaphi wrt vertex
  Cell_eta_phi(cluX,cluY,eta,phi);
  
  // I have all the cluster paramters here and now I will fill the cluster
  
  pcluster->setCluSigmaS(SigmaS);
  pcluster->setCluSigmaL(SigmaL);
  
  pcluster->setCluEta(eta);
  pcluster->setCluPhi(phi);

  Float_t edep = cluAdc*1.0;
  pcluster->setCluEdep(edep);

  pcluster->setCluX(cluX);
  pcluster->setCluY(cluY);

  Int_t pid = 0;
  Int_t edeppid = 0;
  Int_t mcpid = 0;

  
  pcluster->setMcCluPID(mcpid);
  pcluster->setCluPID(pid);
  pcluster->setCluEdepPID(edeppid);
  //   cout<<"Cluster in BuildCluster :"<<nmod<<","<<ncell<<","<<cluX<<","<<cluY<<","<<eta<<","<<phi<<","<<edep<<endl;
  
  //	cout<<" And have added it to the pmdclus (StPmdClusterCollection) which now has "<<pmdclus->Nclusters()<<endl;

  return nmem;
 
}
//-----------------------------------------------------------------------------------------
