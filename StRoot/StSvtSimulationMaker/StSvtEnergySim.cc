#include<Stiostream.h>
#include "Stiostream.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "StSvtEnergySim.hh"
    
StSvtEnergySim::StSvtEnergySim()
{
  mNumberOfParticles = 0;
  mMaxEnergy = 0;
  mParticle = NULL;
}

StSvtEnergySim::~StSvtEnergySim()
{
  delete [] mParticle;
}

void StSvtEnergySim::SetParticle(int numOfPar, float maxEnergy)
{
  mNumberOfParticles = numOfPar;
  mMaxEnergy = maxEnergy;
  mParticle = new StParticle[mNumberOfParticles];

}


void StSvtEnergySim::CalculateEnAndMom(char* option, char* parType)
{
 float randNum1 =0, randNum2 = 0;

 for(int i = 0; i<mNumberOfParticles; i++)
  {

   if(parType == "proton")
     mParticle[i].mass = 931.0;

   randNum1 = 2.0*((float)rand()/(float)RAND_MAX) - 1.0;

   mParticle[i].theta = acos(randNum1);

   randNum2 = 2.0*((float)rand()/(float)RAND_MAX) - 1.0;
 
   mParticle[i].phi = (float)(2*3.14*randNum2);

  if(strncmp(option,"expdev",strlen("expdev"))== 0)
   mParticle[i].energy = mMaxEnergy*Expdev();
  else if(strncmp(option,"gausdev",strlen("gausdev"))== 0)
   mParticle[i].energy = mMaxEnergy*Gausdev();

  //cout<<mParticle[i].energy<<endl;

   mParticle[i].momentum = ::sqrt((mParticle[i].energy )-(mParticle[i].mass*mParticle[i].mass ));

   mParticle[i].momentumZ =  mParticle[i].momentum *cos( mParticle[i].theta);

   mParticle[i].momentumXY =  mParticle[i].momentum *sin( mParticle[i].theta);

   mParticle[i].momentumX =  mParticle[i].momentum *sin( mParticle[i].theta)*cos( mParticle[i].phi);

   mParticle[i].momentumY =  mParticle[i].momentum *sin( mParticle[i].theta)*sin( mParticle[i].phi);

  }

}

float StSvtEnergySim::Expdev(){

   float dum;

   do
      dum = (float)rand()/(float)RAND_MAX;
   while(dum==0.0);
    return -::log(dum);

   }

float StSvtEnergySim::Gausdev()
{

 static int iset = 0;
 static float gset;
 float fac,rsq,v1,v2;

 //if(*idum < 0) iset = 0;
  if(iset == 0)
   {
       
    do {
        v1 = 2.0*((float)rand()/(float)RAND_MAX) - 1.0;
        v2 = 2.0*((float)rand()/(float)RAND_MAX) - 1.0;
        rsq = v1*v1 + v2*v2;
    } while(rsq >= 1.0 || rsq == 1.0);

     fac = 3.0*::sqrt(-2.0*::log(rsq)/rsq);

     gset = v1*fac;  // gset = 3.0*::sqrt(-2.0*::log(rsq))*(v1/::sqrt(rsq))
     iset = 1;
     return v2*fac;
   }
  else
    {
     iset = 0;
     return gset;
    }
}


