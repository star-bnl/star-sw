#ifndef StiSvtLayerLadder_h
// Define conversion Svt <=> Sti numerations for Svt Layer (1-6) & Ladder (1-??)
#define StiSvtLayerLadder_h
#define getSvtBarrel(svtLayer)         (((svtLayer)-1)/2+1)
#define getLayer(svtLayer)             ((svtLayer)-1)
#define getLadder(svtLayer,svtLadder)  (((svtLadder)-1)/2)
#endif
