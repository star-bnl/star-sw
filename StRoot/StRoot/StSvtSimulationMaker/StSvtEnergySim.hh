#ifndef STSVTENERGYSIM_HH
#define STSVTENERGYSIM_HH

#include<Stiostream.h>

typedef struct StParticle
{
  float mass ;
  float energy ;
  float momentum ;
  float momentumZ ;       
  float momentumXY ;
  float momentumX ;
  float momentumY ;
  float theta ;
  float phi ;

} StParticle;

class StSvtEnergySim{

 public:
      StSvtEnergySim();
      ~StSvtEnergySim();

      void SetParticle(int numOfPar, float maxEnergy);
      StParticle* particle();
      void  CalculateEnAndMom(char* option, char* parType);
      float Expdev();
      float Gausdev();

   private:
      
      int mNumberOfParticles ;
      float mMaxEnergy ;
      StParticle* mParticle;     //!
      

  // ClassDef(StSvtEnergySim,1)
           
  
    };


inline StParticle* StSvtEnergySim::particle(){ return mParticle;}

#endif
