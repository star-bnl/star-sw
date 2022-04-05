/**********************************************************************
 *
 * Author: Chunhui Han
 *
 **********************************************************************/
#include "THijing.h"
#include <iostream>
#include <fstream>
#include <string.h>

using namespace std;
THijing::THijing( const char *paramFile ) {
  if (strlen(paramFile) > 255) {
      cout << "Input parameter fileName is " << paramFile << endl;
      cout << "This is longer than 255 characters. Need to increase buffer size." << endl;
    }
  hfilename_.length = strlen(paramFile);
  strcpy(hfilename_.fileName,paramFile);
  init();
}

void THijing::init() {
  mNevent = 0;
  hijev_();
}

THijing::~THijing() {
}

void THijing::SetRandomSeed(int iseed, int k1, int k2) {
  int lux = 2;
  rluxgo_(&lux,&iseed,&k1,&k2);
}
void THijing::GetRandomSeed(int *lux, int *iseed, int *k1, int *k2) {
  rluxat_(lux,iseed,k1,k2);
}
void THijing::SaveRandomSeeds(int *ivec) {
  rluxut_(ivec);
}
void THijing::RestoreRandomSeeds(int *ivec) {
  rluxin_(ivec);
}
void THijing::GenerateEvent() {
  // Comment out the following line, so that THijing always
  // generate an event when called by this method.
  // The number of events in the cardfile thus has no effect.
  //  if(mNevent == headpss_.vsshep[3]) return;
  hijev_();
  mNevent++;
}

void THijing::writeOut() const {
  if(mNevent == 0) {
    cout <<"No event generated."<<endl;
    return;
  }
  ofstream fout("particles.dat",ios::app);
  fout << "Event " << mNevent << '\t' << himain1_.natt << endl;
  // loop over all produced, stable particles
  for(int ip = 0; ip < himain1_.natt; ip++) {
    int istat = (himain2_.katt[3][ip] == 11) ? 2 : himain2_.katt[3][ip];
    fout << ip << '\t' << istat << '\t' << himain2_.katt[0][ip] << '\t'
	 << himain2_.patt[0][ip] << '\t' << himain2_.patt[1][ip] << '\t'
	 << himain2_.patt[2][ip] << '\t' << himain2_.patt[3][ip] << '\t'
	 << endl;
  }
}
