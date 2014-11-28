#ifndef BbcTail_h
#define BbcHex_h

class BbcHex {
 public:
  enum {len=16,maxAdc=80}; 
  //calibration , time walk saturates at adc>maxAdc
  
  float a,b,c,d;
  char name[len];
  int id; // hexID
  // hit
  int tdc; // raw channels
  int adc; // raw channels
  float tof; // time after all corrections (in channels)

  BbcHex(int id,char *nme);
  void clear();
  void print(int k=0);
  void setCalib(float A,float B,float C,float D) 
    { a=A; b=B; c=C; d=D;}

  void setHit(int t, int a);

};
#endif  
