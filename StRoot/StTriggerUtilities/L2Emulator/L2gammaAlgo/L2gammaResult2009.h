//
// Jan Balewski ,MIT
// Example container for results from L2-algo
//
// L2Result must have a size N*4 char, 
//

#ifndef L2_BEMC_GAMMA_RESULT_2009_H
#define L2_BEMC_GAMMA_RESULT_2009_H

struct L2gammaResult2009 {
  enum {mySizeChar=8};// negotiate size w/ John before extending 
  unsigned short TowerID;  //Tower packed with culster quadrant
                           // Bits 0 - 12 represent Tower ID,  0-4788 BEMC, 4800-5519 EEMC
                           // Bits 13-15 represent the quadrant of the cluster 0-3
  unsigned char meanEtaBin;  //Eta bin packed with 8-bit resolution each
  unsigned char meanPhiBin;  //Phi bin packed with 8-bit resolution each
  unsigned char trigger; // Trigger infromation packed
			       // Bits 0-3 EEMC trigger nibble
			       // Bits 4-7 BEMC trigger nibble
  unsigned char clusterEt;     // cluster Et with 60Gev Max.  bits=Et*256/60
  unsigned char isolationSum; // Isolation cluster energy 
			       //  isolation sum with 60Gev Max bits=E*256/60
  unsigned char Time; // elapsed time to issue trigger  find cluster
};

//...................................
inline void 
L2gammaResult2009_print(L2gammaResult2009 *p){
  if(p==0) {printf("print L2gammaResult2009 - NULL pointer ????\n"); return;}
  printf("print L2gammaResult2009: TowerID=%d, quadrant=%d, meanEtaBin=%3.3f, meanPhiBin=%3.3f, \ntrigger=%d, clusterEt=%f, isolationSum=%d\n", 
  (p->TowerID & 0x1FFF),
  (p->TowerID & 0xE000)>>13,
  p->meanEtaBin*BtowGeom::mxEtaBin/256.0,
  p->meanPhiBin*BtowGeom::mxPhiBin/256.0,
  p->trigger,
  p->clusterEt*60.0/256.0,
  p->isolationSum);/*,
  p->Time);*/


};



#endif
