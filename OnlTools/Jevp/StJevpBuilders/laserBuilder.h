#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class LaserReader;

class laserBuilder : public JevpBuilder {
 public:
  int run;
  int nlasers;
  double drift_vel;

  int tpcDataInThisRun;

  laserBuilder(JevpServer *parent=NULL); 
  ~laserBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  int n_cld;
  int n_adc;
  int event_no;


  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH1 *array[1];
    struct {
      TH1 *h_tpc_drift_vel;
    };
  } contents;

  //*** End Histogram Declarations...
    
  void  setPhiAngleMap();
  float mPhiAngleMap[24][72][144];
  LaserReader *laserReader;

  ClassDef(laserBuilder, 1);
};
