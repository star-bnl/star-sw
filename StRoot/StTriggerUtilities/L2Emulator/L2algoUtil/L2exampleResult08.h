//
// Jan Balewski ,MIT
// Example container for results from L2-algo
//
// L2Result must have a size N*4 char, 
//

#ifndef L2_EXAMPLE_RESULT_08_H
#define L2_EXAMPLE_RESULT_08_H

struct L2exampleResult08 {
  enum {mySizeChar=8};// negotiate size w/ John before extending 
  unsigned char decision;
  unsigned char numberOfL2Clust;  
  unsigned short kTicksCompute;
  float clusterET;
};

//...................................
inline void 
L2exampleResult2008_print(L2exampleResult08 *p){
  if(p==0) {printf("print L2exampleResults08 - NULL pointer ????\n"); return;}
  printf("print L2exampleResults08: decison=%d noClust=%d kTicks/Compute=%d clustET=%.3f GeV\n", p->decision, p->numberOfL2Clust,p->kTicksCompute,p->clusterET);

};



#endif
