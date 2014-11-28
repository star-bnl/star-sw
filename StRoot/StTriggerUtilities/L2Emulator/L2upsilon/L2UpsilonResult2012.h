//
// Example container for results from L2-algo
//
// L2Result must have a size N*4 char, 
//

#ifndef L2_BEMC_UPSILON_RESULT_2012_H
#define L2_BEMC_UPSILON_RESULT_2012_H

struct L2UpsilonResult2012 {
//	enum {mySizeChar=24};
	enum {mySizeChar=8};
	unsigned short L0SeedTowerID;
	unsigned short L2SeedTowerID;
	unsigned char energyOfL0Cluster;     // cluster Energy with 25.6Gev Max.  bits=energyOfL0Cluster*10
    unsigned char energyOfL2Cluster;    // cluster Energy with  25.6Gev Max.  bits=energyOfL0Cluster*10
	unsigned char invMass;  	// invMass with 25.6Gev Max.  bits=energyOfL0Cluster*10
	bool trigger;
//	unsigned short numberOfL0SeedTowers;
//	unsigned short numberOfL2SeedTowers;
};

//...................................
inline void 
L2UpsilonResult2012_print(L2UpsilonResult2012 *p){
  if(p==0) {printf("print L2UpsilonResult2012 - NULL pointer ????\n"); return;}
  printf("print L2UpsilonResult2012: L0SeedTowerID=%d, L0SeedTowerID=%d,  energyOfL0Cluster=%f, energyOfL2Cluster=%f, invMass=%f  \n", 
  p->L0SeedTowerID,
  p->L2SeedTowerID,
  p->energyOfL0Cluster*0.1,
  p->energyOfL2Cluster*0.1,
  p->invMass*0.1);

};



#endif
