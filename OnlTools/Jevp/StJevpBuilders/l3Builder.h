#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class l3Builder : public JevpBuilder {
public:
  int run;

  l3Builder(JevpServer *parent=NULL); 
  ~l3Builder();
  
  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:

  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {
    TH1 *array[1];
    struct {
      TH1 *h88_l3_tracks;
      TH1 *h89_l3_Xvertex;
      TH1 *h90_l3_Yvertex;
      TH1 *h91_l3_Zvertex;
      TH1 *h100_l3_zdc_zvertex;
      TH1 *h230_l3_bbc_zvertex;
      TH1 *h101_l3_x_y;
      TH1 *h112_l3_vertex_zdctimediff;
      TH1 *h62_l3_pt;
      TH1 *h63_l3_phi0;
      TH1 *h64_l3_psi;
      TH1 *h65_l3_trk_vertex;

    };
  } contents;

  //*** End Histogram Declarations...

  ClassDef(l3Builder, 1);
};
