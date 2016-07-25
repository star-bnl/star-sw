#include "StDijetFilter.h"
#include "StGenParticle.h"
#include <string>
#include <iostream>
#include <fstream>
#include <cmath>
#include <vector>
#include <algorithm>

static StDijetFilter dijetFilter;

using namespace std;
//_______________________________________________________________
StDijetFilter::StDijetFilter():StMCFilter("dijet")
{

  mRBTOW = 225.405;
  mRcone = 0.7;
  mSeed = 0.5;
  mAssoc = 0.0;
  mSplitfraction = 0.5;
  mAddmidpoints = true;
  mStablemidpoints = true;
  mSplitmerge = true;
  nEvents = new int;
  (*nEvents) = 0;

  mParticleEtaRange = 3.5;
  mJetEtaHigh = 1.3;
  mJetEtaLow = -1.3;
  mPtLow = 7.0;
  mPtHigh = 10.0;
  mDPhi = 2.0;
  mMinJetPt = 4.0;
  mDEta = mJetEtaHigh - mJetEtaLow;

  mRecohadron = 1.1;
  mRecolepton = 1.5;

  mVertex[0] = 0.;
  mVertex[1] = 0.;
  mVertex[2] = 0.;

  readConfig();

}
//_______________________________________________________________
StDijetFilter::~StDijetFilter()
{
  delete nEvents;
}
//_______________________________________________________________
int StDijetFilter::RejectEG(const StGenParticleMaster &ptl) const
{
  (*nEvents)++;
  return 0;
}//_______________________________________________________________
int StDijetFilter::RejectGT(const StGenParticleMaster &ptl) const
{
  if(ptl.Size() <= 0)return 1;

  vector<JetFourVec*> finalparticles;
  vector<JetFourVec*> seeds;
  double v[3];

  //apply cuts on particles
  //get seeds
  for(int i = 0; i < ptl.Size(); i++){
    if(ptl(i)->GetStatusCode() != 1)continue;
    if(fabs(ptl(i)->Eta()) > mParticleEtaRange)continue;
    JetFourVec* p = new JetFourVec((StGenParticle*)ptl(i));
    finalparticles.push_back(p);
    ptl(i)->Vertex(v);

    if(p->Pt() < mSeed)continue;
    seeds.push_back(p);
  }


  if(mAddmidpoints){
    seeds = addMidPoints(seeds);
  }

  int nJets = 0;
  vector< vector<JetFourVec*> > jetFour;
  vector< vector<JetFourVec*> > recoJetFour;
  jetFour.clear();
  recoJetFour.clear();
  for(unsigned int k = 0; k < seeds.size(); k++){
    JetFourVec* j = new JetFourVec(seeds[k]);
    vector<JetFourVec*> jf;
    int nChange = 1;
    int nIter = 0;
    while(nChange != 0 && nIter < 10){
      nChange = 0;
      vector<JetFourVec*>::iterator jfiter;
      for(unsigned int l = 0; l < finalparticles.size(); l++){
	if(finalparticles[l]->getEn()*sin(finalparticles[l]->Theta()) < mAssoc)continue;

	
	jfiter = find(jf.begin(),jf.end(),finalparticles[l]);
      	if(dR(finalparticles[l],j) > mRcone && jfiter == jf.end())continue;
	if(dR(finalparticles[l],j) > mRcone && jfiter != jf.end()){
	  jf.erase(jfiter);
	  nChange++;
	  continue;
	}
	if(jfiter != jf.end())continue;
	jf.push_back(finalparticles[l]);
	nChange++;
      }
      if (j) delete j; // plug leak
      j = combineTracks(jf);
      nIter++;
    }
    if(jf.size() == 0){ 
      if(j) delete j;
      continue;
    }
    jetFour.push_back(jf);

    if (j) delete j; // plug leak
  }

  jetFour = EtOrderedList(jetFour);
  jetFour = RemoveDuplicates(jetFour);

  if(mSplitmerge){
    jetFour = doSplitMerge(jetFour);
    jetFour = RemoveDuplicates(jetFour);
  }


  nJets = 0;
  JetFourVec* j = 0;
  vector<JetFourVec*> dijetFour;
  vector<JetFourVec*> dijetFourReco;
  JetFourVec* dj = 0;
  for(unsigned int k = 0; k < jetFour.size(); k++){
    if(j)delete j;
    j = combineTracks(jetFour[k]);
    //cout<<"jet "<<k<<" "<<j->Pt()<<" "<<j->Eta()<<" "<<j->Phi()<<endl;
    if(j->Pt() < mMinJetPt)continue;
    if(fabs(j->Eta()) > mJetEtaHigh)continue;
    nJets++;
    if(j->Eta() < mJetEtaLow)continue;
    dj = combineTracks(jetFour[k]);
    dijetFour.push_back(dj);
  }
  if(j)delete j;
  int nTrJets = 0;
  dj = 0;
  for(unsigned int k = 0; k < jetFour.size(); k++){
    JetFourVec* j = recoJet(jetFour[k],v);
    if(j->getEn()*sin(j->Theta()) > 4 && fabs(j->Eta()) < 2.5) nTrJets++;
    if(j->Eta() < mJetEtaHigh && j->Eta() > mJetEtaLow){
      dj = new JetFourVec(j);
      dijetFourReco.push_back(dj);
    }
    delete j;
  }

  dijetFourReco = EtOrderedList(dijetFourReco);
  int yesdijet = 0;
  if(dijetFour.size() > 1){
    dj = new JetFourVec;
    (*dj) = *(dijetFour[0]) + *(dijetFour[1]);
    float dphi = fabs(dijetFour[0]->Phi() - dijetFour[1]->Phi());
    float deta = fabs(dijetFour[0]->Eta() - dijetFour[1]->Eta());
    if(dphi > acos(-1))dphi = fabs(2*acos(-1) - dphi);
    if(dphi > mDPhi && deta < mDEta && dj->M() > 10. && dijetFour[0]->Pt() > mPtHigh && dijetFour[1]->Pt() > mPtLow)yesdijet += 1;
    delete dj;
    dj = 0;
  }

  if(dijetFourReco.size() > 1){
    dj = new JetFourVec;
    (*dj) = *(dijetFourReco[0]) + *(dijetFourReco[1]);
    float dphi = fabs(dijetFourReco[0]->Phi() - dijetFourReco[1]->Phi());
    float deta = fabs(dijetFourReco[0]->Eta() - dijetFourReco[1]->Eta());
    if(dphi > acos(-1))dphi = fabs(2*acos(-1) - dphi);
    if(dphi > mDPhi && deta < mDEta && dj->M() > 10. && dijetFourReco[0]->Pt() > mPtHigh && dijetFourReco[1]->Pt() > mPtLow)yesdijet += 2;
    delete dj;
    dj = 0;
  }
  cout << "\n" <<"agrdl: event "<<(*nEvents)<<" "<<nJets<<" "<<nTrJets<<" "<<yesdijet<<endl;
  for(unsigned int k = 0; k < finalparticles.size(); k++)
    delete finalparticles[k];
  finalparticles.clear();
 
  if(yesdijet == 0) return 1;

  return 0;
}
//_______________________________________________________________
int StDijetFilter::RejectGE(const StGenParticleMaster &ptl) const
{
  return 0;
}
//_______________________________________________________________
void StDijetFilter::readConfig()
{
  ifstream cfile("dijet.cnf");
  while(1){
    string attr;
    float val;
    cfile >> attr >> val;
    if(!cfile.good())break;
    parseConfig(attr,val);
  }
}
//_______________________________________________________________
void StDijetFilter::parseConfig(string attr, float val)
{
  if(attr == "RBTOW"){
    mRBTOW = val;
    cout<<"RBTOW changed to "<<val<<endl;
    return;
  }
  if(attr == "RCONE"){
    mRcone = val;
    cout<<"RCONE changed to "<<val<<endl;
    return;
  }
  if(attr == "SEED"){
    mSeed = val;
    cout<<"SEED changed to "<<val<<endl;
    return;
  }
  if(attr == "ASSOC"){
    mAssoc = val;
    cout<<"ASSOC changed to "<<val<<endl;
    return;
  }
  if(attr == "SPLIT"){
    mSplitfraction = val;
    cout<<"SPLIT changed to "<<val<<endl;
    return;
  }
  if(attr == "MIDPOINT"){
    if(val == 1.0){
      mAddmidpoints = true;
      cout<<"MIDPOINT changed to TRUE"<<endl;
      return;
    }else if(val == 0.0){
      mAddmidpoints = false;
      cout<<"MIDPOINT changed to FALSE"<<endl;
      return;
    }else{
      cout<<"IMPRORER INPUT TO MIDPOINTS"<<endl;
    }
  }
  if(attr == "STABLEMIDPOINTS"){
      if(val == 1.0){
	mStablemidpoints = true;
	cout<<"STABLEMIDPOINTS changed to TRUE"<<endl;
	return;
      }else if(val == 0.0){
	mStablemidpoints = false;
	cout<<"STABLEMIDPOINTS changed to FALSE"<<endl;
	return;
      }else{
	cout<<"IMPRORER INPUT TO STABLEMIDPOINTS"<<endl;
      }
  }
  if(attr == "SPLITMERGE"){
    if(val == 1.0){
      mSplitmerge = true;
      cout<<"SPLITMERGE changed to TRUE"<<endl;
      return;
    }else if(val == 0.0){
      mSplitmerge = false;
      cout<<"SPLITMERGE changed to FALSE"<<endl;
      return;
    }else{
      cout<<"IMPRORER INPUT TO SPLITMERGE"<<endl;
    }
  }
  if(attr == "PARTICLEETA"){
    mParticleEtaRange = val;
    cout<<"PARTICLEETA changed to "<<val<<endl;
    return;
  }
  if(attr == "JETETAHIGH"){
    mJetEtaHigh = val;
    cout<<"JETETAHIGH changed to "<<val<<endl;
    return;
  }
  if(attr == "JETETALOW"){
    mJetEtaLow = val;
    cout<<"JETETALOW changed to "<<val<<endl;
    return;
  }
  if(attr == "DIJETPTLOW"){
    mPtLow = val;
    cout<<"DIJETPTLOW changed to "<<val<<endl;
    return;
  }
  if(attr == "DIJETPTHIGH"){
    mPtHigh = val;
    cout<<"DIJETPTHIGH changed to "<<val<<endl;
    return;
  }
  if(attr == "DPHI"){
    mDPhi = val;
    cout<<"DPHI changed to "<<val<<endl;
    return;
  }
  if(attr == "MINJETPT"){
    mMinJetPt = val;
    cout<<"MINJETPT changed to "<<val<<endl;
    return;
  }
  if(attr == "DETA"){
    mDEta = val;
    cout<<"DETA changed to "<<val<<endl;
    return;
  }
  if(attr == "RECOHADRON"){
    mRecohadron = val;
    cout<<"RECOHADRON changed to "<<val<<endl;
    return;
  }
  if(attr == "RECOLEPTON"){
    mRecolepton = val;
    cout<<"RECOLEPTON changed to "<<val<<endl;
    return;
  }
}
//_______________________________________________________________
JetFourVec* StDijetFilter::recoJet(vector<JetFourVec*> v, double* vert) const
{
  JetFourVec* j = new JetFourVec;
  for(unsigned int i = 0; i < v.size(); i++){
    int id = v[i]->getCode();
    if(abs(id) != 11 && id != -2212 && id != -2122){
      v[i]->setPtEtaPhiM(mRecohadron*v[i]->Pt(),v[i]->Eta(),v[i]->Phi(),v[i]->M());
    }else{
      v[i]->setPtEtaPhiM(mRecolepton*v[i]->Pt(),v[i]->Eta(),v[i]->Phi(),v[i]->M());
    }
    (*j) = (*j) + (*v[i]);
  }

  return j;
}
//_______________________________________________________________
vector<JetFourVec*> StDijetFilter::addMidPoints(vector<JetFourVec*> seeds) const
{
  unsigned int n = seeds.size();
  float pi = acos(-1.0);
  for(unsigned int k = 0; k < n; k++){
    for(unsigned int l = 0; l < k; l++){
      if(dR(seeds[k],seeds[l]) > 2*mRcone)continue;
      JetFourVec* s = new JetFourVec;
      float maxphi = max(seeds[k]->Phi(),seeds[l]->Phi());
      float minphi = min(seeds[k]->Phi(),seeds[l]->Phi());
      float dphi = fabs(maxphi - minphi);
      if(dphi > pi)minphi += 2*pi;
      float neta = 0.5*(seeds[k]->Eta() + seeds[l]->Eta());
      float nphi = 0.5*(maxphi + minphi);
      if(nphi > pi)nphi -= 2*pi;
      if(nphi < -pi) nphi += 2*pi;
      s->setPtEtaPhiM(0.001,neta,nphi,0);
      seeds.push_back(s);
    }
  }
  return seeds;
}
//_______________________________________________________________
void StDijetFilter::split(vector<JetFourVec*> &v1, vector<JetFourVec*> &v2) const
{
  JetFourVec* j1 = combineTracks(v1);
  JetFourVec* j2 = combineTracks(v2);

  vector<JetFourVec*>::iterator dit;
  for(vector<JetFourVec*>::iterator it = v1.begin(); it != v1.end(); it++){
    dit = find(v2.begin(),v2.end(),*it);
    if(dit == v2.end())continue;
    float d1 = dR(j1,*it);
    float d2 = dR(j2,*it);
    if(d2 > d1){
      v2.erase(dit);
    }
  }
  for(vector<JetFourVec*>::iterator it = v2.begin(); it != v2.end(); it++){
    dit = find(v1.begin(),v1.end(),*it);
    if(dit == v1.end())continue;
    float d1 = dR(j1,*it);
    float d2 = dR(j2,*it);
    if(d1 > d2){
      v1.erase(dit);
    }
  }

  delete j1;
  delete j2;
}
//_______________________________________________________________
vector<JetFourVec*> StDijetFilter::merge(vector<JetFourVec*> v1, vector<JetFourVec*> v2) const
{
  vector<JetFourVec*> nj = v1;
  vector<JetFourVec*>::iterator iter;
  for(iter = v2.begin(); iter != v2.end(); iter++){
    vector<JetFourVec*>::iterator it2 = find(v1.begin(),v1.end(),*iter);
    if(it2 == v1.end()) nj.push_back(*iter);
  }

  return nj;
}
//_______________________________________________________________
vector< vector<JetFourVec*> > StDijetFilter::doSplitMerge(vector< vector<JetFourVec*> >orig) const
{
  vector< vector<JetFourVec*> > njf;
  vector< vector<JetFourVec* > > jetFour = orig;
  int nChange = 0;
  int nIter = 0;
  while(1){
    if(nIter > 200)break;
    nChange = 0;
    njf.clear();
    njf = jetFour;
    vector< vector<JetFourVec*> >::iterator iter1;
    vector< vector<JetFourVec*> >::iterator iter2;
    for(iter1 = njf.begin(); iter1 != njf.end(); iter1++){
      if(nChange)continue;
      if((*iter1).size() == 0)continue;
      JetFourVec* j = combineTracks(*iter1);
      for(iter2 = iter1+1; iter2 != njf.end(); iter2++){
	if(nChange)continue;
	if((*iter2).size() == 0)continue;
	JetFourVec* nj = combineTracks(*iter2);
	if(dR(nj,j) > 2*mRcone){
	  delete nj;
	  continue;
	}
	float oe = overlapEnergy(*iter1,*iter2);
	if(oe == 0){
	  delete nj;
	  continue;
	}
	nChange = 1;
	vector< vector<JetFourVec*> >::iterator njit1;
	vector< vector<JetFourVec*> >::iterator njit2;
	njit1 = find(jetFour.begin(),jetFour.end(),*iter1);
	njit2 = find(jetFour.begin(),jetFour.end(),*iter2);
	if ( njit1 != jetFour.end() && njit2 != jetFour.end() ) {
	if(oe > mSplitfraction){
	  vector<JetFourVec*> mj = merge(*iter1,*iter2);
	  (*njit1).clear();
	  (*njit2).clear();
	  jetFour.erase(njit2);
	  jetFour.erase(njit1);
	  jetFour.insert(jetFour.begin(),mj);
	  if(nj) delete nj;
	  continue;
	}else{
	  split(*njit1,*njit2);
	  if(nj) delete nj;
	  continue;
	}}
	delete nj;
      }
      delete j;
    }
    jetFour = EtOrderedList(jetFour);
    if(!nChange)break;
    nIter++;
  }
  return njf;

}
//_______________________________________________________________
float StDijetFilter::overlapEnergy(vector<JetFourVec*> v1,vector<JetFourVec*> v2) const
{
  float e = 0;
  JetFourVec* j1 = combineTracks(v1);
  JetFourVec* j2 = combineTracks(v2);
  float high = min(j1->getEn()*sin(j1->Theta()),j2->getEn()*sin(j2->Theta()));

  for(vector<JetFourVec*>::iterator iter1 = v1.begin(); iter1 != v1.end(); iter1++){
    vector<JetFourVec*>::iterator iter2 = find(v2.begin(),v2.end(),*iter1);
    if(iter2 == v2.end())continue;
    e += (*iter2)->getEn()*sin((*iter2)->Theta());
  }

  delete j1;
  delete j2;

  return e/high;

}
//_______________________________________________________________
vector<JetFourVec*> StDijetFilter::EtOrderedList(vector<JetFourVec*> jetFour) const
{
  vector<JetFourVec*> njf;
  vector<JetFourVec*>::iterator jfvIter;
  vector<JetFourVec*>::iterator njfvIter;
  for(jfvIter = jetFour.begin(); jfvIter != jetFour.end(); jfvIter++){
    if((*jfvIter)->Pt() == 0)continue;
    int added = 0;
    for(njfvIter = njf.begin(); njfvIter != njf.end(); njfvIter++){
      if((*jfvIter)->Pt() > (*njfvIter)->Pt()){
	njf.insert(njfvIter,*jfvIter);
	added = 1;
	break;
      }
    }
    if(!added){
      njf.push_back(*jfvIter);
    }
  }

  return njf;
}
//_______________________________________________________________
vector< vector<JetFourVec*> > StDijetFilter::RemoveDuplicates(vector< vector<JetFourVec*> >jetFour) const
{
  vector< vector<JetFourVec*> > njf;
  vector< vector<JetFourVec*> >::iterator jfvIter;
  vector< vector<JetFourVec*> >::iterator njfvIter;
  for(jfvIter = jetFour.begin(); jfvIter != jetFour.end(); jfvIter++){
    if((*jfvIter).size() == 0)continue;
    JetFourVec* j = combineTracks(*jfvIter);
    int dupe = 0;
    for(njfvIter = njf.begin(); njfvIter != njf.end(); njfvIter++){
      JetFourVec* nj = combineTracks(*njfvIter);
      if(*nj == *j)dupe = 1;
      delete nj;
    }
    if(!dupe)njf.push_back(*jfvIter);
    delete j;
  }
  return njf;
}
//_______________________________________________________________
vector< vector<JetFourVec*> > StDijetFilter::EtOrderedList(vector< vector<JetFourVec*> >jetFour) const
{
  vector< vector<JetFourVec*> > njf;
  vector< vector<JetFourVec*> >::iterator jfvIter;
  vector< vector<JetFourVec*> >::iterator njfvIter;
  for(jfvIter = jetFour.begin(); jfvIter != jetFour.end(); jfvIter++){
    if((*jfvIter).size() == 0)continue;
    JetFourVec* j = combineTracks(*jfvIter);
    if(j->Pt() == 0){ delete j;continue;}
    int added = 0;
    for(njfvIter = njf.begin(); njfvIter != njf.end(); njfvIter++){
      JetFourVec* nj = combineTracks(*njfvIter);
      if(j->Pt() > nj->Pt()){
	njf.insert(njfvIter,*jfvIter);
	delete nj;
	added = 1;
	break;
      }
      delete nj;
    }
    if(!added){
      njf.push_back(*jfvIter);
    }
    delete j;
  }

  return njf;

}
//_______________________________________________________________
float StDijetFilter::dR(StGenParticle* p1,StGenParticle* p2) const
{
  float dphi = fabs(p1->Phi() - p2->Phi());
  float deta = p1->Eta() - p2->Eta();
  float pi = acos(-1.0);
  if(dphi > pi)dphi = 2*pi - dphi;
  return sqrt(pow(deta,2) + pow(dphi,2));
}
//_______________________________________________________________
float StDijetFilter::dR(StGenParticle* p,JetFourVec* v) const
{
  float dphi = fabs(p->Phi() - v->Phi());
  float deta = p->Eta() - v->Eta();
  float pi = acos(-1.0);
  if(dphi > pi)dphi = 2*pi - dphi;
  return sqrt(pow(deta,2) + pow(dphi,2));
}
//_______________________________________________________________
float StDijetFilter::dR(JetFourVec* v,StGenParticle* p) const
{
  return dR(p,v);
}
//_______________________________________________________________
float StDijetFilter::dR(JetFourVec* v1, JetFourVec* v2) const
{
  float dphi = fabs(v1->Phi() - v2->Phi());
  float  deta = v1->Eta() - v2->Eta();
  float pi = acos(-1.0);
  if(dphi > pi)dphi = 2*pi - dphi;
  return sqrt(pow(deta,2) + pow(dphi,2));
}
//_______________________________________________________________
JetFourVec* StDijetFilter::combineTracks(vector<JetFourVec*> jf) const
{
  JetFourVec* j = new JetFourVec;
  if(jf.size() == 0)return j;
  for(unsigned int i = 0; i < jf.size(); i++){
    (*j) = (*j) + (*jf[i]);
  }

  return j;

}
//_______________________________________________________________
JetFourVec::JetFourVec()
{
  px = 0,py = 0,pz = 0,en = 0;code = 0;
}
//_______________________________________________________________
JetFourVec::JetFourVec(JetFourVec* v)
{
  code = v->getCode();
  px = v->getPx();
  py = v->getPy();
  pz = v->getPz();
  en = v->getEn();
}
//_______________________________________________________________
JetFourVec::JetFourVec(StGenParticle* g)
{
  double p[4];
  g->Momentum(p);
  px = p[0];
  py = p[1];
  pz = p[2];
  en = p[3];
  code = g->GetPdgCode();
}
//_______________________________________________________________
void JetFourVec::setPxPyPzEn(float x, float y, float z, float e)
{
  setPx(x);
  setPy(y);
  setPz(z);
  setEn(e);
}
//_______________________________________________________________
float JetFourVec::P()
{
  return sqrt(px*px + py*py + pz*pz);
}
//_______________________________________________________________
float JetFourVec::Pt()
{
  return sqrt(px*px + py*py);
}
//_______________________________________________________________
float JetFourVec::Eta()
{
  float pmom = P();
  if(pmom > fabs(pz))return -log(tan(Theta()/2.));
  else return 1.e30;
}
//_______________________________________________________________
float JetFourVec::Phi()
{
  return atan2(py,px);
}
//_______________________________________________________________
float JetFourVec::Theta()
{
  return acos(pz/P());
}
//_______________________________________________________________
float JetFourVec::M()
{
  if(en*en - P()*P() < 0)return 0;
  return sqrt(en*en-P()*P());
}
//_______________________________________________________________
void JetFourVec::setPtEtaPhiM(float pt, float eta, float phi, float m)
{
  float x = pt * cos(phi);
  float y = pt * sin(phi);
  float p = pt*cosh(eta);
  float z = sqrt(p*p-pt*pt)*eta/fabs(eta);
  float e = sqrt(m*m + p*p);
  setPxPyPzEn(x,y,z,e);
}
//_______________________________________________________________
void JetFourVec::setPtEtaPhiE(float pt, float eta, float phi, float e)
{
  float x = pt * cos(phi);
  float y = pt * sin(phi);
  float p = pt*cosh(eta);
  float z = sqrt(p*p-pt*pt)*eta/fabs(eta);
  setPxPyPzEn(x,y,z,e);
}
//_______________________________________________________________
JetFourVec JetFourVec::operator +(JetFourVec x)
{
  JetFourVec t;
  t.setCode(0);
  t.setPx(x.getPx() + px);
  t.setPy(x.getPy() + py);
  t.setPz(x.getPz() + pz);
  t.setEn(x.getEn() + en);

  return t;
}
//_______________________________________________________________
bool JetFourVec::operator ==(JetFourVec x)
{
  if(code != x.getCode())return false;
  if(fabs(px - x.getPx()) > 1e-4)return false;
  if(fabs(py - x.getPy()) > 1e-4)return false;
  if(fabs(pz - x.getPz()) > 1e-4)return false;
  if(fabs(en - x.getEn()) > 1e-4)return false;

  return true;
}
