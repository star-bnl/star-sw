#ifndef _RTSCNDPWCONDITION_H_
#define _RTSCNDPWCONDITION_H_

struct rtsCndPwCondition
{
  //////////////////////////
  int idx_rn;
  int idx_which;                // 1-pwc, 2-contamination, 3-trigger.L0Condition
  int idx_idx;              
  //////////////////////////

  unsigned int onbits;
  unsigned int offbits;

  unsigned int onbits1;        // Support 128 bit conditions.  onbits is lsw, onbits3 is msw
  unsigned int onbits2;
  unsigned int onbits3;

  unsigned int offbits1;
  unsigned int offbits2;
  unsigned int offbits3;
};

#endif
