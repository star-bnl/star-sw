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
};

#endif
