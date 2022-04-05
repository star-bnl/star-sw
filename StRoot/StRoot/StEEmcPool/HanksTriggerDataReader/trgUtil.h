#ifndef TRGUTIL_H
#define TRGUTIL_H

inline unsigned int swapI(unsigned int var)
{
        volatile unsigned int x = var ;

return
        (x&0xff000000) >> 24 |
        (x&0x00ff0000) >> 8  |
        (x&0x0000ff00) << 8  |
        (x&0x000000ff) << 24;
}

inline unsigned int swapSCC(unsigned int var)
{
        volatile unsigned int x = var ;

return
        (x&0x0000ff00) >> 8 |
        (x&0x000000ff) << 8 |
        (x&0xffff0000);
}

inline unsigned int swapSS(unsigned int var)
{
        volatile unsigned int x = var ;

return
        (x&0xff000000) >> 8 |
        (x&0x00ff0000) << 8 |
        (x&0x0000ff00) >> 8 |
        (x&0x000000ff) << 8;
}

inline float swapF(float *var)
{
        unsigned char *cs, tmp;
        cs = (unsigned char *)var;
        tmp = cs[0];
        cs[0] = cs[3];
        cs[3] = tmp;
        tmp = cs[1];
        cs[1] = cs[2];
        cs[2] = tmp;
        return(*var);
}

inline TrgDataType *swapDescSum( TrgDataType *trgD )
{
  int ii;
  volatile unsigned int *x;

  x  = (volatile unsigned int*)&trgD->EvtDesc.TCUdataBytes;

// event descriptor

  *x++ = swapSCC(*x);
  *x++ = swapI(*x);
  *x++ = swapI(*x);
  *x++ = swapSCC(*x);
  for (ii=0; ii<6; ii++) {
    *x++ = swapSS(*x);
  }

// trig summary

  *x++ = swapSS(*x);
  for (ii=0; ii<4; ii++ ) {
    *x++ = swapI(*x);
  }
  *x++ = swapSS(*x);
  for (ii=0; ii<48; ii++) {
    *x++ = swapSS(*x);
  }
  *x++ = swapSS(*x);
  for (ii=0; ii<32; ii++) {
    *x++ = swapI(*x);
  }
  *x++ = swapSS(*x);
  for (ii=0; ii<32; ii++) {
    *x++ = swapI(*x);  
  }
  return(trgD);
}

inline TrgDataType *swapTrg( TrgDataType *trgD, int pre, int post )
{
  int ii, jj;
  volatile unsigned int *x;

  x  = (volatile unsigned int*)&trgD->EvtDesc.TCUdataBytes;

// event descriptor

  *x++ = swapSCC(*x);
  *x++ = swapI(*x);
  *x++ = swapI(*x);
  *x++ = swapSCC(*x);
  for (ii=0; ii<6; ii++) {
    *x++ = swapSS(*x);
  }
  pre = trgD->EvtDesc.npre;
  post = trgD->EvtDesc.npost;

// trig summary

  *x++ = swapSS(*x);
  for (ii=0; ii<4; ii++ ) {
    *x++ = swapI(*x);
  }
  *x++ = swapSS(*x);
  for (ii=0; ii<48; ii++) {
    *x++ = swapSS(*x);
  }
  *x++ = swapSS(*x);
  for (ii=0; ii<32; ii++) {
    *x++ = swapI(*x);
  }
  *x++ = swapSS(*x);
  for (ii=0; ii<32; ii++) {
    *x++ = swapI(*x);  
  }
// raw data
  for ( jj = 0; jj < (pre+post+1); jj++ ) {
    *x++ = swapSCC(*x);
    *x++ = swapSCC(*x);
    for (ii=0; ii<64; ii++) *x++;    //ctb
    *x++ = swapSCC(*x);
    *x++ = swapI(*x);
    for (ii=0; ii<32; ii++) *x++;    //mwc
    *x++ = swapSCC(*x);
    *x++ = swapI(*x);
    for (ii=0; ii<120; ii++) *x++;   //bemc
    for (ii=0; ii<24; ii++) {        //bc1
      *x++ = swapSS(*x);
    }
    *x++ = swapSCC(*x);
    *x++ = swapI(*x);
    for (ii=0; ii<8; ii++) {         //eec layer1
      *x++ = swapSS(*x);
    }
    for (ii=0; ii<36; ii++) *x++;    //eec 
    *x++ = swapSCC(*x);
    *x++ = swapI(*x);
    for (ii=0; ii<28; ii++) *x++;    //fpe ns0 
    for (ii=0; ii<4; ii++) {         //fpe ns1
      *x++ = swapSS(*x);
    }
    for (ii=0; ii<16; ii++) *x++;    //fpe tb0
    for (ii=0; ii<4; ii++) {         //fpe tb1
      *x++ = swapSS(*x);
    }
    for (ii=0; ii<28; ii++) *x++;    //fpw ns0 
    for (ii=0; ii<4; ii++) {         //fpw ns1
      *x++ = swapSS(*x);
    }
    for (ii=0; ii<16; ii++) *x++;    //fpw tb0
    for (ii=0; ii<4; ii++) {         //fpw tb1
      *x++ = swapSS(*x);
    }
    *x++ = swapSCC(*x);
    *x++ = swapI(*x);
    for (ii=0; ii<24; ii++) *x++;    //bbc0
    for (ii=0; ii<8; ii++) {         //bbc1
      *x++ = swapSS(*x);
    }
    for (ii=0; ii<8; ii++) *x++;     //zdc-SMD
    for (ii=0; ii<4; ii++) *x++;     //zdc0
    for (ii=0; ii<4; ii++) {         //zdc1
      *x++ = swapSS(*x);
    }
  }
  return (trgD);
}



#endif

