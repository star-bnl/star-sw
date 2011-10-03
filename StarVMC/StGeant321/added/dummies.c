#if (defined(__linux) && !defined(__ia64))
#include <fpu_control.h>
void __attribute__ ((constructor))
     trapfpe () {
  fpu_control_t cw = _FPU_DEFAULT & ~(_FPU_MASK_IM | _FPU_MASK_ZM |
				      _FPU_MASK_OM);
  _FPU_SETCW(cw);
}
void MAIN__()  {}
#endif
void izrtoc_() {}
void igmess_() {}
void igloc2_() {}
void igpxmp_() {}
void izitoc_() {}
/*
void ffinit_() {}
void ffkey_() {}
void ffgo_() {}
*/
void kuproi_() {}
void kuproc_() {}
void kupror_() {}
void kualfa_() {}
void umlog_() {}
void czgeta_() {}
void czputa_() {}
/*======================= hbook dummies ================================
 *
 * From gplmat */
void hdelet_() {}
void hphist_() {}
void hbookb_() {}
void hfill_() {}
void hidopt_() {}
/*
 * From gbhsta */
void hbook1_() {}
void hbookn_() {}
void hcopy_() {}
/*
 * From AliRun */
void hlimit_() {}










