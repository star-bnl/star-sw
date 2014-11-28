#ifndef COSMIC_ALIGNMENT_H
#define COSMIC_ALIGNMENT_H

void rot(float x, float y, float z,
         float x0,float y0,float z0,
         float o, float p, float q,
         float &xx,float &yy,float &zz);
void getAlign(int idisc, float pl, float rl, float &x, float &y, float &z, float &p, float &r);
#endif
