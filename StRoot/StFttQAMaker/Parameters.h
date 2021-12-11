#ifndef PARAMETARS_H
#define PARAMETARS_H

// Public Paramters for the sTGC simulator and cluster finder
#include "TF1.h"
#include "TH2Poly.h"
#include "TLine.h"
#include "TLatex.h"
#include "TString.h"

#include <iostream>
#include <map>
#include <vector>

// X and Y group edge and the detector edge, coordinate base on the simulator, need to be shift by the real detector
double x1          = 0; double x2              = 55*3.2; double x3 = 110*3.2; double x4 = 168*3.2;  // x strip edge
double py1         = 0; double y2              = 55*3.2; double y3 = 110*3.2; double y4 = 168*3.2;  // y strip edge
double x5          = 150*3.2; double x6        = 95*3.2;
double y5          = 150*3.2; double y6        = 95*3.2;
double x7          = 73*3.2; double x8         = 128*3.2;
double y7          = 73*3.2; double y8         = 128*3.2;
double base_point1 = 0*3.2; double base_point2 = 168*3.2;
double base_point3 = 179;

const int nDetEdges = 5;

//Geo Parameters
const float pent_base       = 537.0;  // in mm
const float pent_nib        = 179.0;  // in mm
const float pent_shift      = 101.6;  // in mm
const int n_clusters_to_gen = 10;     // Number of clusters in this "event"
const float noise_prob      = 0.2;    // 0 -1, 1 is max noise
// const float noise_level = 1; // in ADC units (pre integration)
const float noise_level     = 1.e-10;  // in ADC units (pre integration)
const float strip_pitch     = 3.2;     // digitize strip pitch
const size_t sat_above      = 1024;    // ADC
const float cluster_max_adc = 120;     // ADC (pre integration)
const double width          = 30;
const double length         = 90;
const int nStirps_Row0      = 166;
const int nStrips_FEB1      = 108;

// Geo Parameter of sTGC FEB group
const  int nFEB_1  = 6;
const  int nFEB_2  = 5;
const  int nFEB_5  = 5;
const  int nFEB_6  = 6;
const  int nSquare = 4;
TF1*   fun         = new TF1("edge1","-1*x+179+166*3.2",-1,800);
double p1          = fun->Eval(nStrips_FEB1*strip_pitch);
//Q1v
double v_x1[nFEB_6] = {0, nStirps_Row0*strip_pitch, nStirps_Row0*strip_pitch, nStrips_FEB1*strip_pitch, nStrips_FEB1*strip_pitch, 0       };
double v_y1[nFEB_6] = {0, 0                       , pent_nib                , p1                      , pent_nib                , pent_nib};
double v_x2[nFEB_5] = {0       , nStrips_FEB1*strip_pitch, nStrips_FEB1*strip_pitch, pent_nib  , 0         };
double v_y2[nFEB_5] = {pent_nib, pent_nib                , p1                      , pent_nib*3, pent_nib*3};
//Q2v
double v_x3[nFEB_6] = {0, -nStirps_Row0*strip_pitch, -nStirps_Row0*strip_pitch, -nStrips_FEB1*strip_pitch, -nStrips_FEB1*strip_pitch, 0       };
double v_y3[nFEB_6] = {0, 0                        , pent_nib                 , p1                       ,  pent_nib                , pent_nib};
double v_x4[nFEB_5] = {0       , -nStrips_FEB1*strip_pitch, -nStrips_FEB1*strip_pitch, -pent_nib  , 0         };
double v_y4[nFEB_5] = {pent_nib,  pent_nib                ,  p1                      ,  pent_nib*3, pent_nib*3};
//Q3v
double v_x5[nFEB_6] = {-0-pent_shift, -nStirps_Row0*strip_pitch-pent_shift, -nStirps_Row0*strip_pitch-pent_shift, -nStrips_FEB1*strip_pitch-pent_shift, -nStrips_FEB1*strip_pitch-pent_shift, -0-pent_shift};
double v_y5[nFEB_6] = {-0           , -0                                  , -pent_nib                           , -p1                                 , -pent_nib                           , -pent_nib    };
double v_x6[nFEB_5] = {-0-pent_shift, -nStrips_FEB1*strip_pitch-pent_shift, -nStrips_FEB1*strip_pitch-pent_shift, -pent_nib-pent_shift, -0-pent_shift};
double v_y6[nFEB_5] = {-pent_nib    , -pent_nib                           , -p1                                 , -pent_nib*3         , -pent_nib*3  };
//Q4v
double v_x7[nFEB_6] = {0+pent_shift, nStirps_Row0*strip_pitch+pent_shift, nStirps_Row0*strip_pitch+pent_shift, nStrips_FEB1*strip_pitch+pent_shift, nStrips_FEB1*strip_pitch+pent_shift, 0+pent_shift};
double v_y7[nFEB_6] = {-0          , -0                                 , -pent_nib                          , -p1                                , -pent_nib                          , -pent_nib   };
double v_x8[nFEB_5] = {0+pent_shift, nStrips_FEB1*strip_pitch+pent_shift, nStrips_FEB1*strip_pitch+pent_shift, pent_nib+pent_shift, 0+pent_shift};
double v_y8[nFEB_5] = {-pent_nib   , -pent_nib                          , -p1                                , -pent_nib*3        , -pent_nib*3 };

//horizontal
//Q1h
double h_x1[nFEB_6] = {0, 0                       , pent_nib                , p1                      , pent_nib                , pent_nib};
double h_y1[nFEB_6] = {0, nStirps_Row0*strip_pitch, nStirps_Row0*strip_pitch, nStrips_FEB1*strip_pitch, nStrips_FEB1*strip_pitch, 0       };
double h_x2[nFEB_5] = {pent_nib, pent_nib                , p1                      , pent_nib*3, pent_nib*3};
double h_y2[nFEB_5] = {0       , nStrips_FEB1*strip_pitch, nStrips_FEB1*strip_pitch, pent_nib  , 0         };
//Q2h
double h_x3[nFEB_6] = {0, 0                        , -pent_nib                  ,-p1                    , -pent_nib                , -pent_nib};
double h_y3[nFEB_6] = {0, nStirps_Row0*strip_pitch , nStirps_Row0*strip_pitch,  nStrips_FEB1*strip_pitch,  nStrips_FEB1*strip_pitch, 0       };
double h_x4[nFEB_5] = {-pent_nib, -pent_nib                , -p1                      , -pent_nib*3, -pent_nib*3};
double h_y4[nFEB_5] = {0        ,  nStrips_FEB1*strip_pitch,  nStrips_FEB1*strip_pitch,  pent_nib  , 0          };
//Q3h
double h_x5[nFEB_6] = {-0-pent_shift  , -0-pent_shift            , -pent_nib-pent_shift     , -p1-pent_shift           , -pent_nib-pent_shift     , -pent_nib-pent_shift};
double h_y5[nFEB_6] = {-0             , -nStirps_Row0*strip_pitch, -nStirps_Row0*strip_pitch, -nStrips_FEB1*strip_pitch, -nStrips_FEB1*strip_pitch, -0                  };
double h_x6[nFEB_5] = {-pent_nib-pent_shift    , -pent_nib-pent_shift                           , -p1-pent_shift                                 , -pent_nib*3-pent_shift         , -pent_nib*3-pent_shift  };
double h_y6[nFEB_5] = {-0, -nStrips_FEB1*strip_pitch, -nStrips_FEB1*strip_pitch, -pent_nib, -0};
//Q4h
double h_x7[nFEB_6] = {0+pent_shift              , 0+pent_shift                                     , pent_nib+pent_shift                              , p1+pent_shift                                    , pent_nib+pent_shift                             , pent_nib+pent_shift       };
double h_y7[nFEB_6] = {-(0) , -(nStirps_Row0*strip_pitch),  -(nStirps_Row0*strip_pitch),-(nStrips_FEB1*strip_pitch)  ,-(nStrips_FEB1*strip_pitch) ,-(0)  };
double h_x8[nFEB_5] = {pent_nib+pent_shift       , pent_nib+pent_shift                             , p1+pent_shift                                   , pent_nib*3+pent_shift            , pent_nib*3+pent_shift     };
double h_y8[nFEB_5] = {-(0),-(nStrips_FEB1*strip_pitch),-(nStrips_FEB1*strip_pitch), -(pent_nib), -(0)};

//Diagonal 
//Q1 Diagonal 1
double dia_x1[nFEB_6] = {0, pent_base, pent_base, pent_nib*2, strip_pitch*59/pow(2,0.5),0                         };
double dia_y1[nFEB_6] = {0, 0        , pent_nib , pent_nib*2, strip_pitch*59/pow(2,0.5),strip_pitch*59*pow(2,0.5) };
double dia_x2[nFEB_6] = {0, 0        , pent_nib , pent_nib*2, strip_pitch*59/pow(2,0.5),strip_pitch*59*pow(2,0.5) };
double dia_y2[nFEB_6] = {0, pent_base, pent_base, pent_nib*2, strip_pitch*59/pow(2,0.5),0                         };
//Q2 Diagonal 1
double dia_x3[nFEB_6] = {-0, -pent_base, -pent_base, -(pent_nib*2), -(strip_pitch*59/pow(2,0.5)),-0                         };
double dia_y3[nFEB_6] = {0, 0        , pent_nib , pent_nib*2, strip_pitch*59/pow(2,0.5),strip_pitch*59*pow(2,0.5) };
double dia_x4[nFEB_6] = {-0, -0        , -pent_nib , -pent_nib*2, -(strip_pitch*59/pow(2,0.5)),-(strip_pitch*59*pow(2,0.5)) };
double dia_y4[nFEB_6] = {0, pent_base, pent_base, pent_nib*2, strip_pitch*59/pow(2,0.5),0                         };
//Q3 Diagonal 1
double dia_x5[nFEB_6] = {-0-pent_shift, -pent_base-pent_shift, -pent_base-pent_shift, -(pent_nib*2)-pent_shift, -(strip_pitch*59/pow(2,0.5))-pent_shift,-0-pent_shift                         };
double dia_y5[nFEB_6] = {-0, -0        , -pent_nib , -(pent_nib*2), -(strip_pitch*59/pow(2,0.5)),-(strip_pitch*59*pow(2,0.5)) };
double dia_x6[nFEB_6] = {-0-pent_shift, -0-pent_shift        , -pent_nib-pent_shift , -pent_nib*2-pent_shift, -(strip_pitch*59/pow(2,0.5))-pent_shift,-strip_pitch*59*pow(2,0.5)-pent_shift };
double dia_y6[nFEB_6] = {-0, -pent_base, -pent_base, -pent_nib*2, -(strip_pitch*59/pow(2,0.5)),-0                         };
//Q4 Diagonal 1
double dia_x7[nFEB_6] = {0+pent_shift, pent_base+pent_shift, pent_base+pent_shift, (pent_nib*2)+pent_shift, (strip_pitch*59/pow(2,0.5)+pent_shift),0+pent_shift                         };
double dia_y7[nFEB_6] = {-0, -0        , -pent_nib , -(pent_nib*2), -(strip_pitch*59/pow(2,0.5)),-(strip_pitch*59*pow(2,0.5)) };
double dia_x8[nFEB_6] = {0+pent_shift, 0+pent_shift        , pent_nib+pent_shift , pent_nib*2+pent_shift, strip_pitch*59/pow(2,0.5)+pent_shift,strip_pitch*59*pow(2,0.5)+pent_shift };
double dia_y8[nFEB_6] = {-0, -pent_base, -pent_base, -pent_nib*2, -strip_pitch*59/pow(2,0.5),0                         };

//FEE Q1, FEB position will not change for different driection strips
double Q1_FEE_1x[nSquare] = {pent_nib/2.-width, pent_nib/2.+width, pent_nib/2.+width, pent_nib/2.-width};
double Q1_FEE_1y[nSquare] = {pent_nib*3       , pent_nib*3       , pent_nib*3+length, pent_nib*3+length};
double Q1_FEE_2x[nSquare] = {pent_nib+(pent_nib*2*0.2-width/pow(2,0.5))           ,pent_nib+(pent_nib*2*0.2+width/pow(2,0.5))           ,pent_nib+(pent_nib*2*0.2+width/pow(2,0.5))+length/pow(2,0.5)           ,pent_nib+(pent_nib*2*0.2-width/pow(2,0.5))+length/pow(2,0.5)            };
double Q1_FEE_2y[nSquare] = {fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5)))+length/pow(2,0.5),fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5)))+length/pow(2,0.5) };
double Q1_FEE_3x[nSquare] = {pent_nib+(pent_nib*2*0.4-width/pow(2,0.5))           ,pent_nib+(pent_nib*2*0.4+width/pow(2,0.5))           ,pent_nib+(pent_nib*2*0.4+width/pow(2,0.5))+length/pow(2,0.5)           ,pent_nib+(pent_nib*2*0.4-width/pow(2,0.5))+length/pow(2,0.5)            };
double Q1_FEE_3y[nSquare] = {fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5)))+length/pow(2,0.5),fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5)))+length/pow(2,0.5) };
double Q1_FEE_4x[nSquare] = {pent_nib+(pent_nib*2*0.6-width/pow(2,0.5))           ,pent_nib+(pent_nib*2*0.6+width/pow(2,0.5))           ,pent_nib+(pent_nib*2*0.6+width/pow(2,0.5))+length/pow(2,0.5)           ,pent_nib+(pent_nib*2*0.6-width/pow(2,0.5))+length/pow(2,0.5)            };
double Q1_FEE_4y[nSquare] = {fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5)))+length/pow(2,0.5),fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5)))+length/pow(2,0.5) };
double Q1_FEE_5x[nSquare] = {pent_nib+(pent_nib*2*0.8-width/pow(2,0.5))           ,pent_nib+(pent_nib*2*0.8+width/pow(2,0.5))           ,pent_nib+(pent_nib*2*0.8+width/pow(2,0.5))+length/pow(2,0.5)           ,pent_nib+(pent_nib*2*0.8-width/pow(2,0.5))+length/pow(2,0.5)            };
double Q1_FEE_5y[nSquare] = {fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5)))+length/pow(2,0.5),fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5)))+length/pow(2,0.5) };
double Q1_FEE_6x[nSquare] = {pent_nib*3       , pent_nib*3       , pent_nib*3+length, pent_nib*3+length};
double Q1_FEE_6y[nSquare] = {pent_nib/2.-width, pent_nib/2.+width, pent_nib/2.+width, pent_nib/2.-width};
//FEE Q2, FEB position will not change for different driection strips
double Q2_FEE_1x[nSquare] = {-(pent_nib/2.-width), -(pent_nib/2.+width), -(pent_nib/2.+width), -(pent_nib/2.-width)};
double Q2_FEE_1y[nSquare] = {(pent_nib*3)       , (pent_nib*3)       , (pent_nib*3+length), (pent_nib*3+length)};
double Q2_FEE_2x[nSquare] = {-(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5)))           ,-(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5)))           ,-(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5))+length/pow(2,0.5))           ,-(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5))+length/pow(2,0.5))            };
double Q2_FEE_2y[nSquare] = {fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5)))+length/pow(2,0.5),fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5)))+length/pow(2,0.5) };
double Q2_FEE_3x[nSquare] = {-(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5)))           ,-(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5)))           ,-(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5))+length/pow(2,0.5))           ,-(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5))+length/pow(2,0.5))            };
double Q2_FEE_3y[nSquare] = {fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5)))+length/pow(2,0.5),fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5)))+length/pow(2,0.5) };
double Q2_FEE_4x[nSquare] = {-(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5)))           ,-(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5)))           ,-(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5))+length/pow(2,0.5))           ,-(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5))+length/pow(2,0.5))            };
double Q2_FEE_4y[nSquare] = {fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5)))+length/pow(2,0.5),fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5)))+length/pow(2,0.5) };
double Q2_FEE_5x[nSquare] = {-(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5)))           ,-(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5)))           ,-(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5))+length/pow(2,0.5))           ,-(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5))+length/pow(2,0.5))            };
double Q2_FEE_5y[nSquare] = {fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5))),fun->Eval(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5)))+length/pow(2,0.5),fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5)))+length/pow(2,0.5) };
double Q2_FEE_6x[nSquare] = {-(pent_nib*3)       , -(pent_nib*3)       , -(pent_nib*3+length), -(pent_nib*3+length)};
double Q2_FEE_6y[nSquare] = {(pent_nib/2.-width), (pent_nib/2.+width), (pent_nib/2.+width), (pent_nib/2.-width)};
//FEE Q3, FEB position will not change for different driection strips
double Q3_FEE_1x[nSquare] = {-(pent_nib/2.-width)-pent_shift, -(pent_nib/2.+width)-pent_shift, -(pent_nib/2.+width)-pent_shift, -(pent_nib/2.-width)-pent_shift};
double Q3_FEE_1y[nSquare] = {-(pent_nib*3)       , -(pent_nib*3)       , -(pent_nib*3+length), -(pent_nib*3+length)};
double Q3_FEE_2x[nSquare] = {-(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5)))-pent_shift ,-(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5)))-pent_shift ,-(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5))+length/pow(2,0.5))-pent_shift ,-(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5))+length/pow(2,0.5))-pent_shift  };
double Q3_FEE_2y[nSquare] = {-(fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5)))+length/pow(2,0.5)),-(fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5)))+length/pow(2,0.5)) };
double Q3_FEE_3x[nSquare] = {-(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5)))-pent_shift ,-(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5)))-pent_shift ,-(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5))+length/pow(2,0.5))-pent_shift ,-(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5))+length/pow(2,0.5))-pent_shift  };
double Q3_FEE_3y[nSquare] = {-(fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5)))+length/pow(2,0.5)),-(fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5)))+length/pow(2,0.5)) };
double Q3_FEE_4x[nSquare] = {-(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5)))-pent_shift ,-(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5)))-pent_shift ,-(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5))+length/pow(2,0.5))-pent_shift ,-(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5))+length/pow(2,0.5))-pent_shift  };
double Q3_FEE_4y[nSquare] = {-(fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5)))+length/pow(2,0.5)),-(fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5)))+length/pow(2,0.5)) };
double Q3_FEE_5x[nSquare] = {-(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5)))-pent_shift ,-(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5)))-pent_shift ,-(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5))+length/pow(2,0.5))-pent_shift ,-(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5))+length/pow(2,0.5))-pent_shift  };
double Q3_FEE_5y[nSquare] = {-(fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5)))+length/pow(2,0.5)),-(fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5)))+length/pow(2,0.5)) };
double Q3_FEE_6x[nSquare] = {-(pent_nib*3)-pent_shift       , -(pent_nib*3)-pent_shift       , -(pent_nib*3+length)-pent_shift, -(pent_nib*3+length)-pent_shift};
double Q3_FEE_6y[nSquare] = {-(pent_nib/2.-width), -(pent_nib/2.+width), -(pent_nib/2.+width), -(pent_nib/2.-width)};
//FEE Q4, FEB position will not change for different driection strips
double Q4_FEE_1x[nSquare] = {(pent_nib/2.-width)+pent_shift, (pent_nib/2.+width)+pent_shift, (pent_nib/2.+width)+pent_shift, (pent_nib/2.-width)+pent_shift};
double Q4_FEE_1y[nSquare] = {-(pent_nib*3)       , -(pent_nib*3)       , -(pent_nib*3+length), -(pent_nib*3+length)};
double Q4_FEE_2x[nSquare] = {(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5)))+pent_shift ,(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5)))+pent_shift ,(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5))+length/pow(2,0.5))+pent_shift ,(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5))+length/pow(2,0.5))+pent_shift  };
double Q4_FEE_2y[nSquare] = {-(fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.2+width/pow(2,0.5)))+length/pow(2,0.5)),-(fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5)))+length/pow(2,0.5)) };
double Q4_FEE_3x[nSquare] = {(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5)))+pent_shift ,(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5)))+pent_shift ,(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5))+length/pow(2,0.5))+pent_shift ,(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5))+length/pow(2,0.5))+pent_shift  };
double Q4_FEE_3y[nSquare] = {-(fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.4+width/pow(2,0.5)))+length/pow(2,0.5)),-(fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5)))+length/pow(2,0.5)) };
double Q4_FEE_4x[nSquare] = {(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5)))+pent_shift ,(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5)))+pent_shift ,(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5))+length/pow(2,0.5))+pent_shift ,(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5))+length/pow(2,0.5))+pent_shift  };
double Q4_FEE_4y[nSquare] = {-(fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.6+width/pow(2,0.5)))+length/pow(2,0.5)),-(fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5)))+length/pow(2,0.5)) };
double Q4_FEE_5x[nSquare] = {(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5)))+pent_shift ,(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5)))+pent_shift ,(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5))+length/pow(2,0.5))+pent_shift ,(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5))+length/pow(2,0.5))+pent_shift  };
double Q4_FEE_5y[nSquare] = {-(fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5)))),-(fun->Eval(pent_nib+(pent_nib*2*0.8+width/pow(2,0.5)))+length/pow(2,0.5)),-(fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5)))+length/pow(2,0.5)) };
double Q4_FEE_6x[nSquare] = {(pent_nib*3)+pent_shift       ,(pent_nib*3)+pent_shift       , (pent_nib*3+length)+pent_shift, (pent_nib*3+length)+pent_shift};
double Q4_FEE_6y[nSquare] = {-(pent_nib/2.-width), -(pent_nib/2.+width), -(pent_nib/2.+width), -(pent_nib/2.-width)};

// set vertical sTGC
TH2Poly* SetsTGC_v(TString name, TString title)
{
  TH2Poly* h2p_v = new TH2Poly();
  h2p_v->SetNameTitle(name, title);
  h2p_v->SetMinimum(0.01);
  h2p_v->AddBin(6,v_x1,v_y1);
  h2p_v->AddBin(5,v_x2,v_y2);
  h2p_v->AddBin(6,v_x3,v_y3);
  h2p_v->AddBin(5,v_x4,v_y4);
  h2p_v->AddBin(6,v_x5,v_y5);
  h2p_v->AddBin(5,v_x6,v_y6);
  h2p_v->AddBin(6,v_x7,v_y7);
  h2p_v->AddBin(5,v_x8,v_y8);
  h2p_v->AddBin(4,Q1_FEE_1x,Q1_FEE_1y);
  h2p_v->AddBin(4,Q1_FEE_2x,Q1_FEE_2y);
  h2p_v->AddBin(4,Q1_FEE_3x,Q1_FEE_3y);
  h2p_v->AddBin(4,Q1_FEE_4x,Q1_FEE_4y);
  h2p_v->AddBin(4,Q1_FEE_5x,Q1_FEE_5y);
  h2p_v->AddBin(4,Q1_FEE_6x,Q1_FEE_6y);

  h2p_v->AddBin(4,Q2_FEE_1x,Q2_FEE_1y);
  h2p_v->AddBin(4,Q2_FEE_2x,Q2_FEE_2y);
  h2p_v->AddBin(4,Q2_FEE_3x,Q2_FEE_3y);
  h2p_v->AddBin(4,Q2_FEE_4x,Q2_FEE_4y);
  h2p_v->AddBin(4,Q2_FEE_5x,Q2_FEE_5y);
  h2p_v->AddBin(4,Q2_FEE_6x,Q2_FEE_6y);

  h2p_v->AddBin(4,Q3_FEE_1x,Q3_FEE_1y);
  h2p_v->AddBin(4,Q3_FEE_2x,Q3_FEE_2y);
  h2p_v->AddBin(4,Q3_FEE_3x,Q3_FEE_3y);
  h2p_v->AddBin(4,Q3_FEE_4x,Q3_FEE_4y);
  h2p_v->AddBin(4,Q3_FEE_5x,Q3_FEE_5y);
  h2p_v->AddBin(4,Q3_FEE_6x,Q3_FEE_6y);

  h2p_v->AddBin(4,Q4_FEE_1x,Q4_FEE_1y);
  h2p_v->AddBin(4,Q4_FEE_2x,Q4_FEE_2y);
  h2p_v->AddBin(4,Q4_FEE_3x,Q4_FEE_3y);
  h2p_v->AddBin(4,Q4_FEE_4x,Q4_FEE_4y);
  h2p_v->AddBin(4,Q4_FEE_5x,Q4_FEE_5y);
  h2p_v->AddBin(4,Q4_FEE_6x,Q4_FEE_6y);

  return h2p_v;
}

// set horizontal sTGC
TH2Poly* SetsTGC_h(TString name, TString title)
{
  TH2Poly* h2p_h = new TH2Poly();
  h2p_h->SetNameTitle(name, title);
  h2p_h->SetMinimum(0.01);
  h2p_h->AddBin(6,h_x1,h_y1);
  h2p_h->AddBin(5,h_x2,h_y2);
  h2p_h->AddBin(6,h_x3,h_y3);
  h2p_h->AddBin(5,h_x4,h_y4);
  h2p_h->AddBin(6,h_x5,h_y5);
  h2p_h->AddBin(5,h_x6,h_y6);
  h2p_h->AddBin(6,h_x7,h_y7);
  h2p_h->AddBin(5,h_x8,h_y8);

  h2p_h->AddBin(4,Q1_FEE_1x,Q1_FEE_1y);
  h2p_h->AddBin(4,Q1_FEE_2x,Q1_FEE_2y);
  h2p_h->AddBin(4,Q1_FEE_3x,Q1_FEE_3y);
  h2p_h->AddBin(4,Q1_FEE_4x,Q1_FEE_4y);
  h2p_h->AddBin(4,Q1_FEE_5x,Q1_FEE_5y);
  h2p_h->AddBin(4,Q1_FEE_6x,Q1_FEE_6y);

  h2p_h->AddBin(4,Q2_FEE_1x,Q2_FEE_1y);
  h2p_h->AddBin(4,Q2_FEE_2x,Q2_FEE_2y);
  h2p_h->AddBin(4,Q2_FEE_3x,Q2_FEE_3y);
  h2p_h->AddBin(4,Q2_FEE_4x,Q2_FEE_4y);
  h2p_h->AddBin(4,Q2_FEE_5x,Q2_FEE_5y);
  h2p_h->AddBin(4,Q2_FEE_6x,Q2_FEE_6y);

  h2p_h->AddBin(4,Q3_FEE_1x,Q3_FEE_1y);
  h2p_h->AddBin(4,Q3_FEE_2x,Q3_FEE_2y);
  h2p_h->AddBin(4,Q3_FEE_3x,Q3_FEE_3y);
  h2p_h->AddBin(4,Q3_FEE_4x,Q3_FEE_4y);
  h2p_h->AddBin(4,Q3_FEE_5x,Q3_FEE_5y);
  h2p_h->AddBin(4,Q3_FEE_6x,Q3_FEE_6y);

  h2p_h->AddBin(4,Q4_FEE_1x,Q4_FEE_1y);
  h2p_h->AddBin(4,Q4_FEE_2x,Q4_FEE_2y);
  h2p_h->AddBin(4,Q4_FEE_3x,Q4_FEE_3y);
  h2p_h->AddBin(4,Q4_FEE_4x,Q4_FEE_4y);
  h2p_h->AddBin(4,Q4_FEE_5x,Q4_FEE_5y);
  h2p_h->AddBin(4,Q4_FEE_6x,Q4_FEE_6y);

  return h2p_h;
}

// diagonal 1
TH2Poly* SetsTGc_Dig_1(TString name, TString title)
{
    TH2Poly* h2p_Dig_1 = new TH2Poly();
    h2p_Dig_1->SetMinimum(0.01);
    h2p_Dig_1->SetNameTitle(name, title);
    h2p_Dig_1->AddBin(6,dia_x1,dia_y1);
    h2p_Dig_1->AddBin(6,dia_x3,dia_y3);
    h2p_Dig_1->AddBin(6,dia_x5,dia_y5);
    h2p_Dig_1->AddBin(6,dia_x7,dia_y7);

    h2p_Dig_1->AddBin(4,Q1_FEE_1x,Q1_FEE_1y);
    h2p_Dig_1->AddBin(4,Q1_FEE_2x,Q1_FEE_2y);
    h2p_Dig_1->AddBin(4,Q1_FEE_3x,Q1_FEE_3y);
    h2p_Dig_1->AddBin(4,Q1_FEE_4x,Q1_FEE_4y);
    h2p_Dig_1->AddBin(4,Q1_FEE_5x,Q1_FEE_5y);
    h2p_Dig_1->AddBin(4,Q1_FEE_6x,Q1_FEE_6y);

    h2p_Dig_1->AddBin(4,Q2_FEE_1x,Q2_FEE_1y);
    h2p_Dig_1->AddBin(4,Q2_FEE_2x,Q2_FEE_2y);
    h2p_Dig_1->AddBin(4,Q2_FEE_3x,Q2_FEE_3y);
    h2p_Dig_1->AddBin(4,Q2_FEE_4x,Q2_FEE_4y);
    h2p_Dig_1->AddBin(4,Q2_FEE_5x,Q2_FEE_5y);
    h2p_Dig_1->AddBin(4,Q2_FEE_6x,Q2_FEE_6y);

    h2p_Dig_1->AddBin(4,Q3_FEE_1x,Q3_FEE_1y);
    h2p_Dig_1->AddBin(4,Q3_FEE_2x,Q3_FEE_2y);
    h2p_Dig_1->AddBin(4,Q3_FEE_3x,Q3_FEE_3y);
    h2p_Dig_1->AddBin(4,Q3_FEE_4x,Q3_FEE_4y);
    h2p_Dig_1->AddBin(4,Q3_FEE_5x,Q3_FEE_5y);
    h2p_Dig_1->AddBin(4,Q3_FEE_6x,Q3_FEE_6y);

    h2p_Dig_1->AddBin(4,Q4_FEE_1x,Q4_FEE_1y);
    h2p_Dig_1->AddBin(4,Q4_FEE_2x,Q4_FEE_2y);
    h2p_Dig_1->AddBin(4,Q4_FEE_3x,Q4_FEE_3y);
    h2p_Dig_1->AddBin(4,Q4_FEE_4x,Q4_FEE_4y);
    h2p_Dig_1->AddBin(4,Q4_FEE_5x,Q4_FEE_5y);
    h2p_Dig_1->AddBin(4,Q4_FEE_6x,Q4_FEE_6y);

    return h2p_Dig_1;

}

// diagonal 2
TH2Poly* SetsTGc_Dig_2(TString name, TString title)
{
    TH2Poly* h2p_Dig_1 = new TH2Poly();
    h2p_Dig_1->SetNameTitle(name, title);
    h2p_Dig_1->SetMinimum(0.01);
    h2p_Dig_1->AddBin(6,dia_x2,dia_y2);
    h2p_Dig_1->AddBin(6,dia_x4,dia_y4);
    h2p_Dig_1->AddBin(6,dia_x6,dia_y6);
    h2p_Dig_1->AddBin(6,dia_x8,dia_y8);

    h2p_Dig_1->AddBin(4,Q1_FEE_1x,Q1_FEE_1y);
    h2p_Dig_1->AddBin(4,Q1_FEE_2x,Q1_FEE_2y);
    h2p_Dig_1->AddBin(4,Q1_FEE_3x,Q1_FEE_3y);
    h2p_Dig_1->AddBin(4,Q1_FEE_4x,Q1_FEE_4y);
    h2p_Dig_1->AddBin(4,Q1_FEE_5x,Q1_FEE_5y);
    h2p_Dig_1->AddBin(4,Q1_FEE_6x,Q1_FEE_6y);

    h2p_Dig_1->AddBin(4,Q2_FEE_1x,Q2_FEE_1y);
    h2p_Dig_1->AddBin(4,Q2_FEE_2x,Q2_FEE_2y);
    h2p_Dig_1->AddBin(4,Q2_FEE_3x,Q2_FEE_3y);
    h2p_Dig_1->AddBin(4,Q2_FEE_4x,Q2_FEE_4y);
    h2p_Dig_1->AddBin(4,Q2_FEE_5x,Q2_FEE_5y);
    h2p_Dig_1->AddBin(4,Q2_FEE_6x,Q2_FEE_6y);

    h2p_Dig_1->AddBin(4,Q3_FEE_1x,Q3_FEE_1y);
    h2p_Dig_1->AddBin(4,Q3_FEE_2x,Q3_FEE_2y);
    h2p_Dig_1->AddBin(4,Q3_FEE_3x,Q3_FEE_3y);
    h2p_Dig_1->AddBin(4,Q3_FEE_4x,Q3_FEE_4y);
    h2p_Dig_1->AddBin(4,Q3_FEE_5x,Q3_FEE_5y);
    h2p_Dig_1->AddBin(4,Q3_FEE_6x,Q3_FEE_6y);

    h2p_Dig_1->AddBin(4,Q4_FEE_1x,Q4_FEE_1y);
    h2p_Dig_1->AddBin(4,Q4_FEE_2x,Q4_FEE_2y);
    h2p_Dig_1->AddBin(4,Q4_FEE_3x,Q4_FEE_3y);
    h2p_Dig_1->AddBin(4,Q4_FEE_4x,Q4_FEE_4y);
    h2p_Dig_1->AddBin(4,Q4_FEE_5x,Q4_FEE_5y);
    h2p_Dig_1->AddBin(4,Q4_FEE_6x,Q4_FEE_6y);

    return h2p_Dig_1;

}

// Fill the TH2Poly of sTGC
// Quadrant follow Prashanth's setting
// 1-A, 2-B, 3-C, 4-D
int Fill_sTGC(int FEB, int Quadrant, TH2Poly* his, double Weight = 1)
{
    if ( FEB < 1 || FEB > 6) return -1;
    switch (Quadrant)
    {
    case 1: 
        if (FEB == 1) {his->Fill(50,50,Weight); his->Fill(pent_nib/2.,pent_nib*3+1,Weight);return 1;}
        if (FEB == 2) {his->Fill(50,250,Weight); his->Fill(pent_nib+(pent_nib*2*0.2+1),fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5))),Weight);return 2;}
        if (FEB == 3) {his->Fill(50,250,Weight); his->Fill(pent_nib+(pent_nib*2*0.4+1),fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5))),Weight);return 3;}
        if (FEB == 4) {his->Fill(250,50,Weight); his->Fill(pent_nib+(pent_nib*2*0.6+1),fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5))),Weight);return 4;}
        if (FEB == 5) {his->Fill(250,50,Weight); his->Fill(pent_nib+(pent_nib*2*0.8+1),fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5))),Weight);return 5;}
        if (FEB == 6) {his->Fill(50,50,Weight); his->Fill(pent_nib*3+1,pent_nib/2.,Weight);return 6;}
        break;

    case 2: 
        if (FEB == 1) {his->Fill(50+pent_shift,-50,Weight); his->Fill(pent_nib*3+1+pent_shift,-(pent_nib/2.),Weight); return 1;}
        if (FEB == 2) {his->Fill(250+pent_shift,-50,Weight); his->Fill(pent_nib+(pent_nib*2*0.8+1)+pent_shift,-fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5))),Weight); return 2;}
        if (FEB == 3) {his->Fill(250+pent_shift,-50,Weight); his->Fill(pent_nib+(pent_nib*2*0.6+1)+pent_shift,-fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5))),Weight); return 3;}
        if (FEB == 4) {his->Fill(50+pent_shift,-250,Weight); his->Fill(pent_nib+(pent_nib*2*0.4+1)+pent_shift,-fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5))),Weight); return 4;}
        if (FEB == 5) {his->Fill(50+pent_shift,-250,Weight); his->Fill(pent_nib+(pent_nib*2*0.2+1)+pent_shift,-fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5))),Weight); return 5;}
        if (FEB == 6) {his->Fill(50+pent_shift,-50,Weight); his->Fill(pent_nib/2.+pent_shift,-pent_nib*3-1,Weight); return 6;}
        break;

    case 3: 
        if (FEB == 1) {his->Fill(-50-pent_shift,-50,Weight); his->Fill(-(pent_nib/2.)-pent_shift,-(pent_nib*3+1),Weight); return 1;}
        if (FEB == 2) {his->Fill(-50-pent_shift,-250,Weight); his->Fill(-(pent_nib+(pent_nib*2*0.2+1))-pent_shift,-fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5))),Weight); return 2;}
        if (FEB == 3) {his->Fill(-50-pent_shift,-250,Weight); his->Fill(-(pent_nib+(pent_nib*2*0.4+1))-pent_shift,-fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5))),Weight); return 3;}
        if (FEB == 4) {his->Fill(-250-pent_shift,-50,Weight); his->Fill(-(pent_nib+(pent_nib*2*0.6+1))-pent_shift,-fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5))),Weight); return 4;}
        if (FEB == 5) {his->Fill(-250-pent_shift,-50,Weight); his->Fill(-(pent_nib+(pent_nib*2*0.8+1))-pent_shift,-fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5))),Weight); return 5;}
        if (FEB == 6) {his->Fill(-50-pent_shift,-50,Weight); his->Fill(-(pent_nib*3+1)-pent_shift,-(pent_nib/2.),Weight); return 6;}
        break;

    case 4: 
        if (FEB == 1) {his->Fill(-50,50,Weight); his->Fill(-(pent_nib*3+1),pent_nib/2.,Weight); return 1;}
        if (FEB == 2) {his->Fill(-250,50,Weight); his->Fill(-(pent_nib+(pent_nib*2*0.8+1)),fun->Eval(pent_nib+(pent_nib*2*0.8-width/pow(2,0.5))),Weight); return 2;}
        if (FEB == 3) {his->Fill(-250,50,Weight); his->Fill(-(pent_nib+(pent_nib*2*0.6+1)),fun->Eval(pent_nib+(pent_nib*2*0.6-width/pow(2,0.5))),Weight); return 3;}
        if (FEB == 4) {his->Fill(-50,250,Weight); his->Fill(-(pent_nib+(pent_nib*2*0.4+1)),fun->Eval(pent_nib+(pent_nib*2*0.4-width/pow(2,0.5))),Weight); return 4;}
        if (FEB == 5) {his->Fill(-50,250,Weight); his->Fill(-(pent_nib+(pent_nib*2*0.2+1)),fun->Eval(pent_nib+(pent_nib*2*0.2-width/pow(2,0.5))),Weight); return 5;}
        if (FEB == 6) {his->Fill(-50,50,Weight); his->Fill(-(pent_nib/2.),pent_nib*3+1,Weight); return 6;}
        break;
    
    default: 
        break;
    }
    return 0;
}

//define the line to draw sTGC
std::map<string, TLine* > Edges;
void InitLine()
{
    Edges[ "M1Edge1" ]    = new TLine(base_point1,base_point1,base_point2,base_point1);
    Edges[ "M1Edge2" ]    = new TLine(base_point1,base_point1,base_point1,base_point2);
    Edges[ "M1Edge3" ]    = new TLine(base_point2,base_point1,base_point2,base_point3);
    Edges[ "M1Edge4" ]    = new TLine(base_point1,base_point2,base_point3,base_point2);
    Edges[ "M1Edge5" ]    = new TLine(base_point3,base_point2,base_point2,base_point3);
    Edges[ "M1hG0Edge1" ] = new TLine(x2,py1,x2,y5);
    Edges[ "M1hG0Edge2" ] = new TLine(x2,y5,73*3.2,y5);
    Edges[ "M1hG1Edge1" ] = new TLine(x3,py1,x3,y6);
    Edges[ "M1hG1Edge2" ] = new TLine(x3,y6,128*3.2,y6);
    Edges[ "M1vG0Edge1" ] = new TLine(x1,y2,x5,y2);
    Edges[ "M1vG0Edge2" ] = new TLine(x5,y2,x5,73*3.2);
    Edges[ "M1vG1Edge1" ] = new TLine(x1,y3,x6,y3);
    Edges[ "M1vG1Edge2" ] = new TLine(x6,y3,x6,128*3.2);
    Edges[ "M2Edge1"]     = new TLine(-base_point1,base_point1,-base_point2,base_point1);
    Edges[ "M2Edge2"]     = new TLine(-base_point1,base_point1,-base_point1,base_point2);
    Edges[ "M2Edge3"]     = new TLine(-base_point2,base_point1,-base_point2,base_point3);
    Edges[ "M2Edge4"]     = new TLine(-base_point1,base_point2,-base_point3,base_point2);
    Edges[ "M2Edge5"]     = new TLine(-base_point3,base_point2,-base_point2,base_point3);
    Edges[ "M2hG0Edge1" ] = new TLine(-x2,py1,-x2,y5);
    Edges[ "M2hG0Edge2" ] = new TLine(-x2,y5,-73*3.2,y5);
    Edges[ "M2hG1Edge1" ] = new TLine(-x3,py1,-x3,y6);
    Edges[ "M2hG1Edge2" ] = new TLine(-x3,y6,-128*3.2,y6);
    Edges[ "M2vG0Edge1" ] = new TLine(-x1,y2,-x5,y2);
    Edges[ "M2vG0Edge2" ] = new TLine(-x5,y2,-x5,73*3.2);
    Edges[ "M2vG1Edge1" ] = new TLine(-x1,y3,-x6,y3);
    Edges[ "M2vG1Edge2" ] = new TLine(-x6,y3,-x6,128*3.2);
    Edges[ "M3Edge1" ]    = new TLine(-base_point1-101.6,-base_point1,-base_point2-101.6,-base_point1);
    Edges[ "M3Edge2" ]    = new TLine(-base_point1-101.6,-base_point1,-base_point1-101.6,-base_point2);
    Edges[ "M3Edge3" ]    = new TLine(-base_point2-101.6,-base_point1,-base_point2-101.6,-base_point3);
    Edges[ "M3Edge4" ]    = new TLine(-base_point1-101.6,-base_point2,-base_point3-101.6,-base_point2);
    Edges[ "M3Edge5" ]    = new TLine(-base_point3-101.6,-base_point2,-base_point2-101.6,-base_point3);
    Edges[ "M3hG0Edge1" ] = new TLine(-x2-101.6,-py1,-x2-101.6,-y5);
    Edges[ "M3hG0Edge2" ] = new TLine(-x2-101.6,-y5,-73*3.2-101.6,-y5);
    Edges[ "M3hG1Edge1" ] = new TLine(-x3-101.6,-py1,-x3-101.6,-y6);
    Edges[ "M3hG1Edge2" ] = new TLine(-x3-101.6,-y6,-128*3.2-101.6,-y6);
    Edges[ "M3vG0Edge1" ] = new TLine(-x1-101.6,-y2,-x5-101.6,-y2);
    Edges[ "M3vG0Edge2" ] = new TLine(-x5-101.6,-y2,-x5-101.6,-73*3.2);
    Edges[ "M3vG1Edge1" ] = new TLine(-x1-101.6,-y3,-x6-101.6,-y3);
    Edges[ "M3vG1Edge2" ] = new TLine(-x6-101.6,-y3,-x6-101.6,-128*3.2);
    Edges[ "M4Edge1" ]    = new TLine(base_point1+101.6,-base_point1,base_point2+101.6,-base_point1);
    Edges[ "M4Edge2" ]    = new TLine(base_point1+101.6,-base_point1,base_point1+101.6,-base_point2);
    Edges[ "M4Edge3" ]    = new TLine(base_point2+101.6,-base_point1,base_point2+101.6,-base_point3);
    Edges[ "M4Edge4" ]    = new TLine(base_point1+101.6,-base_point2,base_point3+101.6,-base_point2);
    Edges[ "M4Edge5" ]    = new TLine(base_point3+101.6,-base_point2,base_point2+101.6,-base_point3);
    Edges[ "M4hG0Edge1" ] = new TLine(x2+101.6,-py1,x2+101.6,-y5);
    Edges[ "M4hG0Edge2" ] = new TLine(x2+101.6,-y5,73*3.2+101.6,-y5);
    Edges[ "M4hG1Edge1" ] = new TLine(x3+101.6,-py1,x3+101.6,-y6);
    Edges[ "M4hG1Edge2" ] = new TLine(x3+101.6,-y6,128*3.2+101.6,-y6);
    Edges[ "M4vG0Edge1" ] = new TLine(x1+101.6,-y2,x5+101.6,-y2);
    Edges[ "M4vG0Edge2" ] = new TLine(x5+101.6,-y2,x5+101.6,-73*3.2);
    Edges[ "M4vG1Edge1" ] = new TLine(x1+101.6,-y3,x6+101.6,-y3);
    Edges[ "M4vG1Edge2" ] = new TLine(x6+101.6,-y3,x6+101.6,-128*3.2);
    Edges[ "M1DiaG1Edge" ] = new TLine(x1, py1,2*pent_nib,2*pent_nib);
    Edges[ "M2DiaG1Edge" ] = new TLine(x1, py1,-2*pent_nib,2*pent_nib);
    Edges[ "M3DiaG1Edge" ] = new TLine(x1-pent_shift, py1,-(2*pent_nib+pent_shift),-2*pent_nib);
    Edges[ "M4DiaG1Edge" ] = new TLine(x1+pent_shift, py1,2*pent_nib+pent_shift,-2*pent_nib);

    Edges[ "M1hG0Edge1"]->SetLineColor(1);
    Edges[ "M1hG0Edge2"]->SetLineColor(1);
    Edges[ "M1hG1Edge1"]->SetLineColor(1);
    Edges[ "M1hG1Edge2"]->SetLineColor(1);
    Edges[ "M1vG0Edge1"]->SetLineColor(1);
    Edges[ "M1vG0Edge2"]->SetLineColor(1);
    Edges[ "M1vG1Edge1"]->SetLineColor(1);
    Edges[ "M1vG1Edge2"]->SetLineColor(1);
    Edges[ "M1Edge1" ]->SetLineColor(1);
    Edges[ "M1Edge2" ]->SetLineColor(1);
    Edges[ "M1Edge3" ]->SetLineColor(1);
    Edges[ "M1Edge4" ]->SetLineColor(1);
    Edges[ "M1Edge5" ]->SetLineColor(1);
    Edges[ "M2hG0Edge1" ]->SetLineColor(1);
    Edges[ "M2hG0Edge2" ]->SetLineColor(1);
    Edges[ "M2hG1Edge1" ]->SetLineColor(1);
    Edges[ "M2hG1Edge2" ]->SetLineColor(1);
    Edges[ "M2vG0Edge1" ]->SetLineColor(1);
    Edges[ "M2vG0Edge2" ]->SetLineColor(1);
    Edges[ "M2vG1Edge1" ]->SetLineColor(1);
    Edges[ "M2vG1Edge2" ]->SetLineColor(1);
    Edges[ "M2Edge1" ]->SetLineColor(1);
    Edges[ "M2Edge2" ]->SetLineColor(1);
    Edges[ "M2Edge3" ]->SetLineColor(1);
    Edges[ "M2Edge4" ]->SetLineColor(1);
    Edges[ "M2Edge5" ]->SetLineColor(1);
    Edges[ "M3hG0Edge1" ]->SetLineColor(1);
    Edges[ "M3hG0Edge2" ]->SetLineColor(1);
    Edges[ "M3hG1Edge1" ]->SetLineColor(1);
    Edges[ "M3hG1Edge2" ]->SetLineColor(1);
    Edges[ "M3vG0Edge1" ]->SetLineColor(1);
    Edges[ "M3vG0Edge2" ]->SetLineColor(1);
    Edges[ "M3vG1Edge1" ]->SetLineColor(1);
    Edges[ "M3vG1Edge2" ]->SetLineColor(1);
    Edges[ "M3Edge1" ]->SetLineColor(1);
    Edges[ "M3Edge2" ]->SetLineColor(1);
    Edges[ "M3Edge3" ]->SetLineColor(1);
    Edges[ "M3Edge4" ]->SetLineColor(1);
    Edges[ "M3Edge5" ]->SetLineColor(1);
    Edges[ "M4hG0Edge1" ]->SetLineColor(1);
    Edges[ "M4hG0Edge2" ]->SetLineColor(1);
    Edges[ "M4hG1Edge1" ]->SetLineColor(1);
    Edges[ "M4hG1Edge2" ]->SetLineColor(1);
    Edges[ "M4vG0Edge1" ]->SetLineColor(1);
    Edges[ "M4vG0Edge2" ]->SetLineColor(1);
    Edges[ "M4vG1Edge1" ]->SetLineColor(1);
    Edges[ "M4vG1Edge2" ]->SetLineColor(1);
    Edges[ "M4Edge1" ]->SetLineColor(1);
    Edges[ "M4Edge2" ]->SetLineColor(1);
    Edges[ "M4Edge3" ]->SetLineColor(1);
    Edges[ "M4Edge4" ]->SetLineColor(1);
    Edges[ "M4Edge5" ]->SetLineColor(1);
    Edges[ "M1DiaG1Edge" ]->SetLineColor(1);
    Edges[ "M2DiaG1Edge" ]->SetLineColor(1);
    Edges[ "M3DiaG1Edge" ]->SetLineColor(1);
    Edges[ "M4DiaG1Edge" ]->SetLineColor(1);
}

// Draw whole sTGC
void Draw_sTGC()
{
    Edges[ "M1hG0Edge1" ]->Draw("same");
    Edges[ "M1hG0Edge2" ]->Draw("same");
    Edges[ "M1hG1Edge1" ]->Draw("same");
    Edges[ "M1hG1Edge2" ]->Draw("same");
    Edges[ "M1vG0Edge1" ]->Draw("same");
    Edges[ "M1vG0Edge2" ]->Draw("same");
    Edges[ "M1vG1Edge1" ]->Draw("same");
    Edges[ "M1vG1Edge2" ]->Draw("same");
    Edges[ "M1Edge1" ]->Draw("same");
    Edges[ "M1Edge2" ]->Draw("same");
    Edges[ "M1Edge3" ]->Draw("same");
    Edges[ "M1Edge4" ]->Draw("same");
    Edges[ "M1Edge5" ]->Draw("same");

    Edges[ "M2hG0Edge1" ]->Draw("same");
    Edges[ "M2hG0Edge2" ]->Draw("same");
    Edges[ "M2hG1Edge1" ]->Draw("same");
    Edges[ "M2hG1Edge2" ]->Draw("same");
    Edges[ "M2vG0Edge1" ]->Draw("same");
    Edges[ "M2vG0Edge2" ]->Draw("same");
    Edges[ "M2vG1Edge1" ]->Draw("same");
    Edges[ "M2vG1Edge2" ]->Draw("same");
    Edges[ "M2Edge1" ]->Draw("same");
    Edges[ "M2Edge2" ]->Draw("same");
    Edges[ "M2Edge3" ]->Draw("same");
    Edges[ "M2Edge4" ]->Draw("same");
    Edges[ "M2Edge5" ]->Draw("same");

    Edges[ "M3hG0Edge1" ]->Draw("same");
    Edges[ "M3hG0Edge2" ]->Draw("same");
    Edges[ "M3hG1Edge1" ]->Draw("same");
    Edges[ "M3hG1Edge2" ]->Draw("same");
    Edges[ "M3vG0Edge1" ]->Draw("same");
    Edges[ "M3vG0Edge2" ]->Draw("same");
    Edges[ "M3vG1Edge1" ]->Draw("same");
    Edges[ "M3vG1Edge2" ]->Draw("same");
    Edges[ "M3Edge1" ]->Draw("same");
    Edges[ "M3Edge2" ]->Draw("same");
    Edges[ "M3Edge3" ]->Draw("same");
    Edges[ "M3Edge4" ]->Draw("same");
    Edges[ "M3Edge5" ]->Draw("same");

    Edges[ "M4hG0Edge1" ]->Draw("same");
    Edges[ "M4hG0Edge2" ]->Draw("same");
    Edges[ "M4hG1Edge1" ]->Draw("same");
    Edges[ "M4hG1Edge2" ]->Draw("same");
    Edges[ "M4vG0Edge1" ]->Draw("same");
    Edges[ "M4vG0Edge2" ]->Draw("same");
    Edges[ "M4vG1Edge1" ]->Draw("same");
    Edges[ "M4vG1Edge2" ]->Draw("same");
    Edges[ "M4Edge1" ]->Draw("same");
    Edges[ "M4Edge2" ]->Draw("same");
    Edges[ "M4Edge3" ]->Draw("same");
    Edges[ "M4Edge4" ]->Draw("same");
    Edges[ "M4Edge5" ]->Draw("same");
}

//Draw sTGC QA plots with vertical strip line 
void Draw_sTGC_vertical(TH2Poly* his)
{
    TH2D* frame = new TH2D("frame_vertical","vertical;X (mm); Y (mm); nHits ",1625,-900,900,8,-900,900);
    frame->SetStats(0);
    frame->Draw();

    TLatex* lat = new TLatex();
    lat->SetTextFont(22);
    lat->SetTextSize(0.04);
    //Q1(A)
    lat->DrawLatex(236.6444,630.4012,"Q_{A} FEB 2");
    lat->DrawLatex(561.4827,177.0833,"Q_{A} FEB 6");
    //Q2(D)
    lat->DrawLatex(-522.355,622.1065,"Q_{D} FEB 5");
    lat->DrawLatex(-831.5422,206.0185,"Q_{D} FEB 1");
    //Q3(C)
    lat->DrawLatex(-522.4332,-680.4124,"Q_{C} FEB 2");
    lat->DrawLatex(-831.5422,-342.1392,"Q_{C} FEB 6");
    //Q4(B)
    lat->DrawLatex(236.6444,-681.3272,"Q_{B} FEB 5");
    lat->DrawLatex(561.4044,-342.3997,"Q_{B} FEB 1");

    his->Draw("colzsame");
    Edges[ "M1vG0Edge1" ]->Draw("same");
    Edges[ "M1vG0Edge2" ]->Draw("same");
    Edges[ "M1vG1Edge1" ]->Draw("same");
    Edges[ "M1vG1Edge2" ]->Draw("same");
    Edges[ "M1Edge1" ]->Draw("same");
    Edges[ "M1Edge2" ]->Draw("same");
    Edges[ "M1Edge3" ]->Draw("same");
    Edges[ "M1Edge4" ]->Draw("same");
    Edges[ "M1Edge5" ]->Draw("same");

    Edges[ "M2vG0Edge1" ]->Draw("same");
    Edges[ "M2vG0Edge2" ]->Draw("same");
    Edges[ "M2vG1Edge1" ]->Draw("same");
    Edges[ "M2vG1Edge2" ]->Draw("same");
    Edges[ "M2Edge1" ]->Draw("same");
    Edges[ "M2Edge2" ]->Draw("same");
    Edges[ "M2Edge3" ]->Draw("same");
    Edges[ "M2Edge4" ]->Draw("same");
    Edges[ "M2Edge5" ]->Draw("same");

    Edges[ "M3vG0Edge1" ]->Draw("same");
    Edges[ "M3vG0Edge2" ]->Draw("same");
    Edges[ "M3vG1Edge1" ]->Draw("same");
    Edges[ "M3vG1Edge2" ]->Draw("same");
    Edges[ "M3Edge1" ]->Draw("same");
    Edges[ "M3Edge2" ]->Draw("same");
    Edges[ "M3Edge3" ]->Draw("same");
    Edges[ "M3Edge4" ]->Draw("same");
    Edges[ "M3Edge5" ]->Draw("same");

    Edges[ "M4vG0Edge1" ]->Draw("same");
    Edges[ "M4vG0Edge2" ]->Draw("same");
    Edges[ "M4vG1Edge1" ]->Draw("same");
    Edges[ "M4vG1Edge2" ]->Draw("same");
    Edges[ "M4Edge1" ]->Draw("same");
    Edges[ "M4Edge2" ]->Draw("same");
    Edges[ "M4Edge3" ]->Draw("same");
    Edges[ "M4Edge4" ]->Draw("same");
    Edges[ "M4Edge5" ]->Draw("same");

}

//Draw sTGC QA plots with horizontal strip
void Draw_sTGC_horizontal(TH2Poly* his)
{
    TH2D* frame = new TH2D("frame_horizontal","horizontal;X (mm); Y (mm); nHits ",1625,-900,900,8,-900,900);
    frame->SetStats(0);
    frame->Draw();

    TLatex* lat = new TLatex();
    lat->SetTextFont(22);
    lat->SetTextSize(0.04);
    //Q1(A)
    lat->DrawLatex(174.0401,562.8858,"Q_{A} FEB 1");
    lat->DrawLatex(561.4827,177.0833,"Q_{A} FEB 5");
    //Q2(D)
    lat->DrawLatex(-444.0995,567.7083,"Q_{D} FEB 6");
    lat->DrawLatex(-831.5422,206.0185,"Q_{D} FEB 2");
    //Q3(C)
    lat->DrawLatex(-424.6139,-718.5571,"Q_{C} FEB 1");
    lat->DrawLatex(-831.5422,-424.3827,"Q_{C} FEB 5");
    //Q4(B)
    lat->DrawLatex(174.0401,-719.9074,"Q_{B} FEB 6");
    lat->DrawLatex(561.4044,-425.733 ,"Q_{B} FEB 2");

    his->Draw("colzsame");
    Edges[ "M1hG0Edge1" ]->Draw("same");
    Edges[ "M1hG0Edge2" ]->Draw("same");
    Edges[ "M1hG1Edge1" ]->Draw("same");
    Edges[ "M1hG1Edge2" ]->Draw("same");
    Edges[ "M1Edge1" ]->Draw("same");
    Edges[ "M1Edge2" ]->Draw("same");
    Edges[ "M1Edge3" ]->Draw("same");
    Edges[ "M1Edge4" ]->Draw("same");
    Edges[ "M1Edge5" ]->Draw("same");

    Edges[ "M2hG0Edge1" ]->Draw("same");
    Edges[ "M2hG0Edge2" ]->Draw("same");
    Edges[ "M2hG1Edge1" ]->Draw("same");
    Edges[ "M2hG1Edge2" ]->Draw("same");
    Edges[ "M2Edge1" ]->Draw("same");
    Edges[ "M2Edge2" ]->Draw("same");
    Edges[ "M2Edge3" ]->Draw("same");
    Edges[ "M2Edge4" ]->Draw("same");
    Edges[ "M2Edge5" ]->Draw("same");

    Edges[ "M3hG0Edge1" ]->Draw("same");
    Edges[ "M3hG0Edge2" ]->Draw("same");
    Edges[ "M3hG1Edge1" ]->Draw("same");
    Edges[ "M3hG1Edge2" ]->Draw("same");
    Edges[ "M3Edge1" ]->Draw("same");
    Edges[ "M3Edge2" ]->Draw("same");
    Edges[ "M3Edge3" ]->Draw("same");
    Edges[ "M3Edge4" ]->Draw("same");
    Edges[ "M3Edge5" ]->Draw("same");

    Edges[ "M4hG0Edge1" ]->Draw("same");
    Edges[ "M4hG0Edge2" ]->Draw("same");
    Edges[ "M4hG1Edge1" ]->Draw("same");
    Edges[ "M4hG1Edge2" ]->Draw("same");
    Edges[ "M4Edge1" ]->Draw("same");
    Edges[ "M4Edge2" ]->Draw("same");
    Edges[ "M4Edge3" ]->Draw("same");
    Edges[ "M4Edge4" ]->Draw("same");
    Edges[ "M4Edge5" ]->Draw("same");

}

//Draw sTGC QA plots with Diagonal strip
void Draw_sTGC_Diag_1(TH2Poly* his)
{
    TH2D* frame = new TH2D("frame","Diagonal 1;X (mm); Y (mm); nHits ",1625,-900,900,8,-900,900);
    frame->SetStats(0);
    frame->Draw();

    TLatex* lat = new TLatex();
    lat->SetTextFont(22);
    lat->SetTextSize(0.04);
    //Q1(A)
    lat->DrawLatex(326.7164,506.3657,"Q_{A} FEB 4");
    //Q2(D)
    lat->DrawLatex(-608.5142,506.3657,"Q_{D} FEB 3");
    //Q3(C)
    lat->DrawLatex(-608.4359,-575.2315,"Q_{C} FEB 4");
    //Q4(B)
    lat->DrawLatex(338.4547,-575.2315,"Q_{B} FEB 3");

    his->Draw("colzsame");
    Edges[ "M1Edge1" ]->Draw("same");
    Edges[ "M1Edge2" ]->Draw("same");
    Edges[ "M1Edge3" ]->Draw("same");
    Edges[ "M1Edge4" ]->Draw("same");
    Edges[ "M1Edge5" ]->Draw("same");

    Edges[ "M2Edge1" ]->Draw("same");
    Edges[ "M2Edge2" ]->Draw("same");
    Edges[ "M2Edge3" ]->Draw("same");
    Edges[ "M2Edge4" ]->Draw("same");
    Edges[ "M2Edge5" ]->Draw("same");

    Edges[ "M3Edge1" ]->Draw("same");
    Edges[ "M3Edge2" ]->Draw("same");
    Edges[ "M3Edge3" ]->Draw("same");
    Edges[ "M3Edge4" ]->Draw("same");
    Edges[ "M3Edge5" ]->Draw("same");

    Edges[ "M4Edge1" ]->Draw("same");
    Edges[ "M4Edge2" ]->Draw("same");
    Edges[ "M4Edge3" ]->Draw("same");
    Edges[ "M4Edge4" ]->Draw("same");
    Edges[ "M4Edge5" ]->Draw("same");

    Edges[ "M1DiaG1Edge" ]->Draw("same");
    Edges[ "M2DiaG1Edge" ]->Draw("same");
    Edges[ "M3DiaG1Edge" ]->Draw("same");
    Edges[ "M4DiaG1Edge" ]->Draw("same");

}

void Draw_sTGC_Diag_2(TH2Poly* his)
{
    TH2D* frame = new TH2D("frame","Diagonal 2;X (mm); Y (mm); nHits ",1625,-900,900,8,-900,900);
    frame->SetStats(0);
    frame->Draw();

    TLatex* lat = new TLatex();
    lat->SetTextFont(22);
    lat->SetTextSize(0.04);
    //Q1(A)
    lat->DrawLatex(326.7164,506.3657,"Q_{A} FEB 3");
    //Q2(D)
    lat->DrawLatex(-608.5142,506.3657,"Q_{D} FEB 4");
    //Q3(C)
    lat->DrawLatex(-608.4359,-575.2315,"Q_{C} FEB 3");
    //Q4(B)
    lat->DrawLatex(338.4547,-575.2315,"Q_{B} FEB 4");

    his->Draw("colzsame");
    Edges[ "M1Edge1" ]->Draw("same");
    Edges[ "M1Edge2" ]->Draw("same");
    Edges[ "M1Edge3" ]->Draw("same");
    Edges[ "M1Edge4" ]->Draw("same");
    Edges[ "M1Edge5" ]->Draw("same");

    Edges[ "M2Edge1" ]->Draw("same");
    Edges[ "M2Edge2" ]->Draw("same");
    Edges[ "M2Edge3" ]->Draw("same");
    Edges[ "M2Edge4" ]->Draw("same");
    Edges[ "M2Edge5" ]->Draw("same");

    Edges[ "M3Edge1" ]->Draw("same");
    Edges[ "M3Edge2" ]->Draw("same");
    Edges[ "M3Edge3" ]->Draw("same");
    Edges[ "M3Edge4" ]->Draw("same");
    Edges[ "M3Edge5" ]->Draw("same");

    Edges[ "M4Edge1" ]->Draw("same");
    Edges[ "M4Edge2" ]->Draw("same");
    Edges[ "M4Edge3" ]->Draw("same");
    Edges[ "M4Edge4" ]->Draw("same");
    Edges[ "M4Edge5" ]->Draw("same");

    Edges[ "M1DiaG1Edge" ]->Draw("same");
    Edges[ "M2DiaG1Edge" ]->Draw("same");
    Edges[ "M3DiaG1Edge" ]->Draw("same");
    Edges[ "M4DiaG1Edge" ]->Draw("same");

}


#endif
