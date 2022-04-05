#ifndef _RTSCNDTWLINK_H_
#define _RTSCNDTWLINK_H_

struct rtsCndTwLink
{
  //////////////////////////
  // int idx_rn;
  int idx_twhash;
  //////////////////////////
  
  unsigned int tw;
  unsigned int twdef;
  unsigned int PS;
  unsigned int AW;
  unsigned int pre;
  unsigned int post;
};

#endif
