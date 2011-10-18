// W-bose L2 trigger algorithm for 2012 data taking
// search for the highest 2x2 tower cluster in the barrel
// Jan Balewski ,MIT
// 
//
// L2Result must have a size N*4 char, 
//

#ifndef L2_BEMC_WBOSE_RESULT_2012_H
#define L2_BEMC_WBOSE_RESULT_2012_H

struct L2wResult2012 { // must be N*4 bytes
  enum {mySizeChar=8};// negotiate size w/ Ross before extending 
  unsigned char seedEt;      // seed Et with 60Gev Max.  bits=Et*256/60
  unsigned char clusterEt;   // cluster Et with 60Gev Max.  bits=Et*256/60
  unsigned char seedEtaBin;  // iEta bin 
  unsigned char seedPhiBin;  // iPhi bin 

  unsigned char trigger;     // bit0=rnd, bit1=ET>thr
  unsigned char dum1,dum2,dum3;
};

//...................................
inline void 
L2wResult2012_print(L2wResult2012 *p){
  
  if(p==0) {printf("print L2wResult2012 - NULL pointer ????\n"); return;}
  printf("L2wResult2012: clust ET=%.1f seed: ET=%.1f iEta=%d, iPhi=%d, trig=%d\n", 
	 p->clusterEt*60.0/256.0, 
	 p->seedEt*60.0/256.0,
	 p->seedEtaBin,
	 p->seedPhiBin,
	 p->trigger 
	 );
 
};



#endif
