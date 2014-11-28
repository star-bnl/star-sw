#ifndef STAR_PP2PPHIT_CLUSTER_H
#define STAR_PP2PPHIT_CLUSTER_H

typedef struct pp2ppRawHit_st {
  signed short   adc;             // 0 - 255                                             
  unsigned char  sec;             // sector (1: East (Sector 5) and 2: West (Sector 6) ) 
  unsigned char  sequencer;       // sequencer id: ( 1 - 8 )
  unsigned char  chain;           // 0 - 3 (A: 0, B: 1, C: 2, D: 3)                      
  unsigned char  svx;             // 0 - 3 for chains A & C; 0 - 5 for chains B and D    
  unsigned char  channel;         // 0 - 127                                             
} PP2PPRAWHIT_ST ;

/*
typedef struct pp2ppCluster_st {
  unsigned char  sequencer;       // sequencer id: ( 1 - 8 )
  unsigned char  chain;           // 0 - 3 (A: 0, B: 1, C: 2, D: 3)                      
  unsigned char length ; // > 0 
  double position ;     // 0 - 755
  double energy ;       // in ADC
  double x ;
  double y ;
  double z ;
} PP2PPCLUSTER_ST ;
*/

#endif
