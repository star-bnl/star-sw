//
// $id$
//
// $Log: StEmcPreClusterCollection.cxx,v $
// Revision 1.7  2000/10/17 19:37:17  suaide
// Small bug fixed
//
// Revision 1.6  2000/09/08 22:55:05  suaide
//
//
//
// some modifications to compile on Solaris
//
// Revision 1.5  2000/09/08 21:47:59  suaide
//
//
// See README for details
//
// Revision 1.4  2000/08/24 22:11:34  suaide
//
//
// restored some files for background compatibility
//
// Revision 1.3  2000/08/24 19:45:37  suaide
//
//
// small modifications: some cout has been removed
//
// Revision 1.2  2000/08/24 11:26:48  suaide
//
//
//
// by A. A. P. Suaide - 2000/08/24 07:25:00
//
// Notes:
//
// 1. Full StEvent Compatible
// 2. Read hits from StEvent object
// 3. Write clusters in StEvent format and old format to keep background
//    compatibility
// 4. Do clustering in bemc, bprs, bsmde, bsmdp
// 5. Included method StPreEclMaker::SetClusterCollection
//
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//          initial version from Akio Ogawa    
//
// 08/15/2000 - By A. A. P. Suaide
// included method checkClustersInModule to check the clusters

//////////////////////////////////////////////////////////////////////////
//                                   
// StEmcPreClusterCollection
//
// StEmcPreClusterCollectio is base class for electromagnetic cluster 
// collection. 
//
//////////////////////////////////////////////////////////////////////////
#include "StEmcPreClusterCollection.h"
#include <math.h>
#include "emc_def.h"
#include "StEvent/StEvent.h" 
#include "StEvent/StEventTypes.h"
#include "StEmcUtil/StEmcGeom.h"
ClassImp(StEmcPreClusterCollection)

TArrayF energyw;
TArrayI ew, sw, used, hitsid;
Int_t nh, first, last, keyEta, keyPhi,nhits;
Int_t nhit,nmodule,idBeforeClustering,idAfterClustering;

StEmcDetector* mDet; 
StEmcGeom *geo;

//_____________________________________________________________________________
StEmcPreClusterCollection::StEmcPreClusterCollection():St_DataSet("Default")
{
  SetTitle("ecl");
}
//_____________________________________________________________________________
StEmcPreClusterCollection::StEmcPreClusterCollection(const Char_t *Name):St_DataSet("Default")
{
  SetTitle("ecl");
}
//_____________________________________________________________________________
StEmcPreClusterCollection::StEmcPreClusterCollection(const Char_t *Name, StEmcDetector* stdet):St_DataSet(Name)
{
  SetTitle("ecl");
  mNclusters=0;
  
  mDet=stdet;
  geo=new StEmcGeom(Name);
  kCheckClustersOk=kFALSE;
   
  if(!strcmp(Name,"bemc"))
  {
    mDetector  = 1;     mEnergySeed         = 0.1;  
    mEnergyAdd = 0.001; mEnergyThresholdAll = 0.02;
    mSizeMax = 4; 
  }
  else if(!strcmp(Name,"bprs"))
  {
    mDetector  = 2;     mEnergySeed         = 0.1;  
    mEnergyAdd = 0.001; mEnergyThresholdAll = 0.02;
    mSizeMax = 4; 
  }
  else if(!strcmp(Name,"bsmde"))
  {
    mDetector  = 3;     mEnergySeed         = 0.08; 
    mEnergyAdd = 0.001; mEnergyThresholdAll = 0.001;  
    mSizeMax = 5;       kCheckClustersOk = kTRUE; 
  }
  else if(!strcmp(Name,"bsmdp"))
  {
    mDetector  = 4;     mEnergySeed         = 0.08; 
    mEnergyAdd = 0.001; mEnergyThresholdAll = 0.001;  
    mSizeMax = 5;       kCheckClustersOk = kTRUE;
  }
  else
  {
    mEnergySeed = -1.0; mEnergyAdd = 0.0; mEnergyThresholdAll = 0.0; 
    cout<<"For detector "<<Name<<" PreCluster Finder has not ipmlimented yet:"<<endl;
    kIsOk=kFALSE;
    return; 
  }
     
  
  kIsOk=kTRUE;
}
//_____________________________________________________________________________
Int_t StEmcPreClusterCollection::findClusters()
{
  nhits=mDet->numberOfHits();
  if(nhits<=0)
  {
    cout <<"***** StEmcClusterCollection: No hits in this detector\n";
    return 1;
  }
  nmodule=mDet->numberOfModules();
  for(Int_t mod=1; mod <= nmodule; mod++)   // loop in modules
  {
    StSPtrVecEmcRawHit& hits=mDet->module(mod)->hits();
    
    first = 0;                       // first hit in this module
    last  = hits.size();             // last hit in this module
    nh    = last - first;            // number of hits in this module. nh must be positive
    if(nh>0) 
    {
      energyw.Set(nh);   // set dimensions of the arrays for this module
      ew.Set(nh); 
      sw.Set(nh);
      energyw.Reset();   // reset arrays
      ew.Reset(); 
      sw.Reset();
  
      Int_t  ih, jh;

      for(ih=first; ih<last; ih++) // fill arrays
      {
        jh=ih-first;
        energyw[jh] = hits[ih]->energy();
        ew[jh]=hits[ih]->eta();
        sw[jh]=abs(hits[ih]->sub());
      }

      idBeforeClustering=mNclusters;
      findClustersInModule(mod);
      idAfterClustering=mNclusters;
      if(kCheckClustersOk) checkClustersInModule(mod);
    
    }
  }
  if(kCheckClustersOk)
  {
    printf("before check: Det %s #Clusters %i \n",GetName(), mNclusters);
    mClusters.Compress();
    mNclusters=mClusters.GetLast()+1;
    printf("after check: Det %s #Clusters %i \n",GetName(), mNclusters);
  }
  else printf("Det %s #Clusters %i \n",GetName(), mNclusters);
  return 0;
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::findClustersInModule(Int_t mod)
{
  used.Set(nh);   // array with flags of used hits in the module
  used.Reset();

// only one hit in this module
  if(nh == 1) 
  { 
    if(energyw[0]>mEnergySeed)
    {
      hitsid.Set(1); 
      hitsid[0]=first;
      addPreCluster(mod, &hitsid);
      nhit = 0;
    }
    return;
  }
//

// more than one hit in this module

  St_TableSorter index(energyw.GetArray(), nh); // The last element is biggest
  hitsid.Set(10);
  hitsid.Reset();

  Int_t i, ii;
  for(i=nh-1; i>=0; i--) //loop from the more energetic hit to the less one
  { 
    Int_t j = index.GetIndex(i); //get index of the hit
    
    if(energyw[j] < mEnergySeed) break; //if the hit is below threshold for cluster find -> break
    
    if(used[j] == 0) // test if this hit is not used
    {
      hitsid[0]=j; // First hit in cluster
      nhit=1;
      used[j]=1;
      keyEta=0; 
      keyPhi=0;

      for(ii=i-1; ii>=0; ii--)
      {
        int jj = index.GetIndex(ii);
        if(energyw[jj] < mEnergyThresholdAll) break;
        if(used[jj] == 0)
        {
          if(testOnNeighbor(jj)==0)
          {
            used[jj]=1;
            hitsid[nhit]=jj; 
            nhit++; 
            if(nhit == mSizeMax) break;
          }
        }
      }
      if(nhit>0)
      {
        TArrayI *hidw = new TArrayI(nhit);
        for(Int_t i1=0; i1<nhit; i1++)
        {
          (*hidw)[i1] = hitsid[i1] + first; // Trans. from module to all
        }
        addPreCluster(mod, hidw);
        nhit = 0; 
        delete hidw;
      }
    }
  }
}
//_____________________________________________________________________________
Int_t StEmcPreClusterCollection::testOnNeighbor(Int_t jn)
{
  static Int_t   etaFirst, etaLast;
  static Int_t   phiFirst, phiLast, etaSeed;
  extern Int_t   nhit;
  extern TArrayI ew,hitsid,sw;
         Int_t   js,imax;
  static Int_t   keyDir;
  
  if(!strcmp(GetName(),"bsmde"))
  {
    if (nhit == 1) 
    {
      etaFirst=ew[hitsid[0]]; 
      etaLast=etaFirst;
    }

    if (etaFirst-ew[jn] == 1) 
    {
      etaFirst=ew[jn]; 
      return 0;
    }
    else if (ew[jn]-etaLast  == 1) 
    {
      etaLast=ew[jn];  
      return 0;
    }
    else return 1;
  }
  else if(!strcmp(GetName(),"bsmdp"))
  {  
    if(nhit == 1) 
    {
      etaSeed=ew[hitsid[0]]; 
      phiFirst=sw[hitsid[0]]; 
      phiLast=phiFirst;
    }
  
    if(etaSeed == ew[jn])// Same eta bin
    { 
      if(phiFirst-sw[jn] == 1) 
      {
        phiFirst=sw[jn]; 
        return 0;
      }
      else if(sw[jn]-phiLast  == 1) 
      {
        phiLast=sw[jn];  
        return 0;
      }
      else return 1;
    }
  }
  else
  {
    imax = (nhit<2) ? 1 : 2; 
    for(Int_t i=0; i<imax; i++)
    {
      js=hitsid[i];
      if(ew[js] == ew[jn] && keyEta==0) // Neighbors in eta row
      {
        if(abs(sw[js]-sw[jn])!=1) 
        {     
          printf(" testOnNeighbor => sw(seed) %i sw(neigbour) %i \n",sw[js],sw[jn]);
        }
        if(nhit == 1) keyEta = 1; 
        return 0;
      }
      else if(sw[js] == sw[jn] && abs(ew[js]-ew[jn]) == 1 && keyPhi==0)
      { 
        if     (nhit == 1) 
        {
          keyPhi=1; 
          return 0;
        }
        else if(nhit == 2) 
        {
          keyDir=ew[js]-ew[jn]; 
          return 0;
        }
        else if(nhit == 3) 
        {
          if(keyDir == ew[js]-ew[jn]) return 0; // 4th hit must be at the same side as 3th !!! 
          else return 1;
        }
      }
    }
  }
  return 1;
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::setCheckClusters(Bool_t Ok)
{
  kCheckClustersOk=Ok;
  return;
}

//_____________________________________________________________________________
void StEmcPreClusterCollection::checkClustersInModule(Int_t mod)
// Added by A. A. P. Suaide. not ready yet...
{

  Int_t nclus=idAfterClustering-idBeforeClustering;
  if(nclus==0) return;
  StSPtrVecEmcRawHit& hits=mDet->module(mod)->hits();
  TArrayI ne(nclus);
  for(Int_t i=0;i<nclus;i++) ne[i]=-1;

// find neighbohor clusters in bsmde
  
  if(!strcmp(GetName(),"bsmde"))
  {
    for(Int_t i=0;i<nclus-1;i++)
    {
      Int_t clnumi=i+idBeforeClustering;
      StEmcPreCluster *ci=(StEmcPreCluster*)mClusters[clnumi];
      Int_t nhitsi=ci->Nhits();
      Int_t etamini=150,etamaxi=0;
      for(Int_t k=0;k<nhitsi;k++)
      {
        Int_t eta=hits[ci->ID(k)]->eta();
        if(eta<etamini) etamini=eta;
        if(eta>etamaxi) etamaxi=eta;
      }
      for(Int_t j=i+1;j<nclus;j++)
      {
        Int_t clnumj=j+idBeforeClustering;
        StEmcPreCluster *cj=(StEmcPreCluster*)mClusters[clnumj];
        Int_t nhitsj=cj->Nhits();
        Int_t etaminj=150,etamaxj=0;
        for(Int_t k=0;k<nhitsj;k++)
        {
          Int_t eta=hits[cj->ID(k)]->eta();
          if(eta<etaminj) etaminj=eta;
          if(eta>etamaxj) etamaxj=eta;
        }
        Int_t deta;
        if(etamaxi<etaminj) deta=etaminj-etamaxi;
        else if(etamaxj<etamini) deta=etamini-etamaxj;
        else deta=0;
        if (deta==1) ne[i]=j;
      }
    }
    applyProfile(mod,ne);
  }
  
// find neighbohor clusters in bsmdp
  if(!strcmp(GetName(),"bsmdp"))
  {
    for(Int_t i=0;i<nclus-1;i++)
    {
      Int_t clnumi=i+idBeforeClustering;
      StEmcPreCluster *ci=(StEmcPreCluster*)mClusters[clnumi];
      Int_t nhitsi=ci->Nhits();
      Float_t etai=ci->Eta();
      Int_t submini=15,submaxi=0; 
      for(Int_t k=0;k<nhitsi;k++)
      {
        Int_t sub=hits[ci->ID(k)]->sub();
        if(sub<submini) submini=sub;
        if(sub>submaxi) submaxi=sub;
      }
      for(Int_t j=i+1;j<nclus;j++)
      {
        Int_t clnumj=j+idBeforeClustering;
        StEmcPreCluster *cj=(StEmcPreCluster*)mClusters[clnumj];
        Int_t nhitsj=cj->Nhits();
        Float_t etaj=cj->Eta();
        if(fabs(etai-etaj)<0.000001)
        {
          Int_t subminj=15,submaxj=0;
          for(Int_t k=0;k<nhitsj;k++)
          {
            Int_t sub=hits[cj->ID(k)]->sub();
            if(sub<subminj) subminj=sub;
            if(sub>submaxj) submaxj=sub;
          }
          Int_t dsub;
          if(submaxi<subminj) dsub=subminj-submaxi;
          else if(submaxj<submini) dsub=submini-submaxj;
          else dsub=0;
          if (dsub==1) ne[i]=j;
        }
      }
    }
    applyProfile(mod,ne);
  }
}
//_____________________________________________________________________________
Int_t StEmcPreClusterCollection::applyProfile(Int_t mod,TArrayI ne)
{
  StSPtrVecEmcRawHit& hits=mDet->module(mod)->hits();
  Int_t nclus=idAfterClustering-idBeforeClustering;
  Int_t maxApply=0;
  TArrayI used(nclus),applyOnThese(nclus);
  
  for(Int_t i=0;i<nclus;i++)
  {
    if(ne[i]>-1 && used[i]==0)
    {
      Int_t j=i;
      maxApply=0;
      do
      {
        used[j]=1;
        applyOnThese[maxApply]=j;
        j=ne[j];
        maxApply++;
      } while (j>-1);
      
      // setting arrays with clusters and positions to evaluate chisqr
      Int_t nh=0;
      for(Int_t j=0;j<maxApply;j++)
      {
        nh+= ((StEmcPreCluster*)mClusters[applyOnThese[j]+idBeforeClustering])->Nhits();
      }
      
      TArrayF x(nh),y(nh);
      Int_t index=0;
      for(Int_t j=0;j<maxApply;j++)
      {
        StEmcPreCluster *c=(StEmcPreCluster*)mClusters[applyOnThese[j]+idBeforeClustering];
        for(Int_t k=0;k<c->Nhits();k++)
        {
          Int_t m=hits[c->ID(k)]->module();
          Int_t e=hits[c->ID(k)]->eta();
          Int_t s=abs(hits[c->ID(k)]->sub());
          Float_t xtemp;
          if(!strcmp(GetName(),"bsmde")) geo->getEta(m,e,xtemp);
          if(!strcmp(GetName(),"bsmdp")) geo->getPhi(m,s,xtemp);
          x[index]=xtemp;
          y[index]=hits[c->ID(k)]->energy();
          index++;
        }
      }
      // finished setting arrays
      
      // starting doing combinations on clusters
      Int_t shiftmin=0,inimin=0;
      Float_t chimin=1e30;
      for (Int_t ini=0;ini<maxApply-1;ini++)
      {
        for(Int_t shift=0+1*(ini!=0);shift<maxApply-ini;shift++)
        {
          Int_t nc=0,ncmax=maxApply-shift;
          TArrayF xavg(ncmax),en(ncmax);
          
          for(Int_t clus=0;clus<maxApply;clus++)
          {
            StEmcPreCluster *c=(StEmcPreCluster*)mClusters[applyOnThese[clus]+idBeforeClustering];
            
            if(!strcmp(GetName(),"bsmde")) xavg[nc]+=c->Eta()*c->Energy();
            if(!strcmp(GetName(),"bsmdp")) xavg[nc]+=c->Phi()*c->Energy();
            en[nc]+=c->Energy();
            
            if (clus<ini || clus>=ini+shift) 
            {
              xavg[nc]/=en[nc];
              nc++;
            }         
          }
          
          // applying chisqr test for this combination
          Float_t chi=calcChiSqrt(nc,en,xavg,nh,x,y);
          if(chi<chimin)
          {
            chimin=chi;
            shiftmin=shift;
            inimin=ini;
          }
          // finished applying chisqrt test for this combination
        } 
      }
      if(inimin!=0 || shiftmin!=0) setNewClusters(mod,inimin,shiftmin,applyOnThese);
      // finished doing combinations on clusters
    }
  } 
  return 0;  
}
//_____________________________________________________________________________
Float_t StEmcPreClusterCollection::calcChiSqrt(Int_t nc,TArrayF en,TArrayF xavg,Int_t nh,TArrayF x,TArrayF y)
{
  Float_t chi=0.0,yprofile;
  for(Int_t i=0;i<nh;i++)
  {
    Float_t yprofile=0.0;
    for(Int_t j=0;j<nc;j++) yprofile+=profile(x[i],xavg[j],en[j]);
    Float_t sigma=0.15*sqrt(y[i]);
    chi+=(y[i]-yprofile)*(y[i]-yprofile)/(sigma*sigma);
  }
  chi=sqrt(chi/(nh-nc));
  return chi;
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::setNewClusters(Int_t mod,Int_t ini,Int_t shift,TArrayI applyOnThese)
{
  Float_t eta=0,phi=0,en=0;
  Int_t nhits=0,index=0;
  for(Int_t clus=ini;clus<=ini+shift;clus++)
  {
    StEmcPreCluster *c=(StEmcPreCluster*)mClusters[applyOnThese[clus]+idBeforeClustering];
    nhits+=c->Nhits();        
    eta+=c->Eta()*c->Energy();
    phi+=c->Phi()*c->Energy();
    en+=c->Energy();
  }   
  TArrayI newhits(nhits);
  for(Int_t clus=ini;clus<=ini+shift;clus++)
  {
    StEmcPreCluster *c=(StEmcPreCluster*)mClusters[applyOnThese[clus]+idBeforeClustering];
    for(Int_t i=0;i<c->Nhits();i++)
    {
      newhits[index]=c->ID(i);
      index++;
    }            
  }  
  eta/=en;
  phi/=en;
  StEmcPreCluster *c=new StEmcPreCluster(mod,&newhits,mDetector);
  c->calcMeanAndRms(mDet,mod);
  mClusters.AddAt(c,applyOnThese[ini]+idBeforeClustering);
  for(Int_t i=ini+1;i<=ini+shift;i++) mClusters[applyOnThese[i]+idBeforeClustering]=0;

}
//_____________________________________________________________________________
Float_t StEmcPreClusterCollection::profile(Float_t x,Float_t xavg,Float_t e)
{
  Float_t A1,sig1,A2,sig2,assym,exp1,exp2,assym2=1.0;
  if(!strcmp(GetName(),"bsmde"))  
  {
    A1=-6.486e-4+1.149*e+0.03786*e*e;
    sig1=0.002638-0.0002124*log(e);
    A2=0.02278+0.01308*e+0.0008117*e*e;
    sig2=0.01268-0.01074*exp(-e)*pow(e,0.2686);
    assym=1.069+1.975*fabs(xavg); 
  }
  if(!strcmp(GetName(),"bsmdp"))
  {
    A1=-0.01927+0.7533*e+0.02308*e*e;
    sig1=0.004426-0.0008858*log(e);
    A2=0.03264-0.04145*e+0.01796*e*e;
    sig2=0.00833+0.03117*exp(-e)*pow(e,1.873);
    assym=1;
  }
  if((fabs(x)-fabs(xavg))>0) assym2=assym;
  
    
    exp1= A1*exp(-fabs(x-xavg)/(sig1));
    exp2= A2*exp(-fabs(x-xavg)/(sig2*assym2));
    if (sig2<=sig1) exp2=0;
  
  return (exp1+exp2);
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::addPreCluster(Int_t mod, TArrayI *hid)
{
  StEmcPreCluster *cluster = new StEmcPreCluster(mod,hid,mDetector);
  cluster->calcMeanAndRms(mDet,mod);
  mClusters.Add(cluster);
  mNclusters  += 1;
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::printCluster(Int_t ic, StEmcPreCluster *cl)
{
  cout <<" =========  PreCluster  "<<ic<<" =========== "<<endl;
  cout << (*cl) << endl;
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::printClusters(Int_t n, Int_t start)
{
  cout << endl << mDetector << "  " << GetName() << " : ";
  if(mNclusters<=0){cout << "No Clusters" << endl; return;}
  else{cout << mNclusters << " clusters" <<endl;} 
  cout<<" mEnergySeed " << mEnergySeed;
  cout<<" mEnergyAdd "  << mEnergyAdd; 
  cout<<" mEnergyThresholdAll " << mEnergyThresholdAll << endl;
  cout<<" mSizeMax " << mSizeMax;
  cout<<" Capacity "<<mClusters.Capacity()<<endl<<endl;

  TIter next(&mClusters);
  StEmcPreCluster *cl;
  if(start>0){
    for(Int_t i=0; i<start; i++) cl = (StEmcPreCluster*)next();
  }

  if(start<0) start=0;
  if(n<=0) n=1;
  if(start>=mNclusters) start=mNclusters-1;
  if(start+n>=mNclusters) n=mNclusters-start;
  for(Int_t i=start; i<start+n; i++){
    if(i<=mClusters.Capacity()){
      if( (cl=(StEmcPreCluster*)next()) ) printCluster(i,cl);
    }
    else cout<<" PreCluster "<<i<<" out of capacity "<<endl;
  }
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::printClustersAll()
{
  printClusters(mNclusters,0);
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::Browse(TBrowser *b)
{
  printClusters(mNclusters,0);
}
//_____________________________________________________________________________
void StEmcPreClusterCollection::printConf()
{
    cout <<"\n******************* PREECL INFO\n";
    cout <<"For detector: "<<GetName()<<"\n";
    cout <<"mEnergySeed            = "<<mEnergySeed<<"\n";  
    cout <<"mEnergyThresholdAll    = "<<mEnergyThresholdAll<<"\n";  
    cout <<"mEnergyAdd             = "<<mEnergyAdd<<"\n";  
    cout <<"mSizeMax               = "<<mSizeMax<<"\n";  
    cout <<"*******************\n\n";
  
}
