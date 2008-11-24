#ifndef JAN_BARREL_EVENT_HH
#define JAN_BARREL_EVENT_HH


//----------------------------
//------- reco event container
//----------------------------

class JanBarrelEvent {
 public:
  JanBarrelEvent() { clear();}
  void clear() { 
    printf("JanBarrelEve:clear()\n");
    id=0;
    memset(bprsCap,-1,sizeof(bprsCap));
    memset(rawAdcTile,0,sizeof(rawAdcTile));
    memset(adcTile,0,sizeof(adcTile));
    memset(statTile,-1,sizeof(statTile));  // default all dead
    memset(tileIn,0,sizeof(tileIn));
  }


  // vector < BsmdCluster1D> smdCl[mxBSmd];

  int id; // eventID
  int   tileIn[mxBTile]; // 0 if no data
  int   bprsCap[mxBprsCrate];
  float rawAdcTile[mxBTile][mxBtow];
  float adcTile[mxBTile][mxBtow];
  int   statTile[mxBTile][mxBtow];

  void print( int flag=0) {
    printf("\njanBarelEve ID=%d printFlag=0x%0x",id,flag);
    printf("   BPRSin=%d caps:%d:%d:%d:%d:",tileIn[kBPrs], bprsCap[0],bprsCap[1],bprsCap[2],bprsCap[3]);
    printf("\n");
    if(flag&1) {//..................
      for(int i=0;i<120;i++) {
	int id=7+i*40;
	if(i%10==0) printf("\n  softID=%4d adc format  BTOW:BPRS=  ",id);
	printf("%.1f : %.1f, ", rawAdcTile[kBTow][id-1], rawAdcTile[kBPrs][id-1]);
      }
      printf("\n");
    }// end of k&1

    if(flag&2) {//..................
      for(int i=0;i<120;i++) {
	int id=7+i*40;
	if(i%5==0) printf("\n   BPRS id=%4d {adcRaw,adc,stat}*N=",id);
	printf("%.1f, %.1f, %d;   ", rawAdcTile[kBPrs][id-1],adcTile[kBPrs][id-1],statTile[kBPrs][id-1]);
      }
      printf("\n");
    }// end of k&1
    
  }// end of PRINT


};

#endif



#if 0
   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

class BsmdCluster1D {
 public:
  BsmdCluster1D(char x='?') {clear();plane=x;}
  void clear() {memset(this,0,sizeof(BsmdCluster1D )); plane='?';xCell=yCell=-1;}
  enum {mxTw=2}; 
  char plane; 
  int strip0; // begin of integration window, unit: strip-in-band, from 0
  float mean0; // cluster center, unit: strip-in-band, from 0
  int meanId; // cluster center, unit: absolute strip ID, counting from 1
  float ene; // cluster energy (deposit, no SF correction) in GeV
  float rms; // cluster width, units # of strips
  int inTowerId[mxTw]; 
  float towerDist; //cm 
  int xCell,yCell; // centroid location in eta-phi 0.1x0.1 cells
  void print(){ 
    if(ene<=0) { printf("bsmd%c Cl empty\n",plane); return;} 
    printf("bsmd%c CL str0=%d mean0=%.1f meanId=%d rms=%.2f ene/keV=%.2f inTw %d.or.%d dist=%.1f cell(%d,%d)\n",plane,strip0,mean0,meanId,rms,ene*1e6,inTowerId[0],inTowerId[1],towerDist, xCell,yCell);
  }
};
#endif
