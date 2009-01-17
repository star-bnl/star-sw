//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 1 Jan 2009
//

#ifndef FEE_HH
#define FEE_HH

struct Tcl_Interp;

struct FEE {
  enum { NTOWERS = 4800, LUT_SIZE = 4096 };

  static bool debug;

  static int pedestalShift;
  static int adc[NTOWERS+1];
  static float ped[NTOWERS+1];
  static int status[NTOWERS+1];
  static int ped4[NTOWERS+1];

  // Tcl interpreter

  static Tcl_Interp* tcl;

  int triggerPatch;	// 0-299
  int crate;		// 1-30
  int patch;		// 0-9
  int board;		// 1-5
  int mask;		// 1-2
  int towers[16];
  int highTowerStatus;
  int patchSumStatus;
  int bitConvMode;
  int formula;
  int parameters[6];
  int output;
  char lut[LUT_SIZE];

  FEE();

  int highTower() const { return output & 0x3f; }
  int patchSum() const { return output >> 6 & 0x3f; }
  int numberOfMaskedTowers() const;
  int triggermask() const;

  void clear();
  void powerUp();
  void writeLUT();
  void action();

  static void initTcl();
  static void setPedestalShift(int pedestalShift);
  static void setTowerAdc(int id, int adc);
  static void setTowerPedestal(int id, float ped);
  static void setTowerStatus(int id, int status);
  void setTriggerPatch(int triggerPatch);
};

inline FEE::FEE()
{
  clear();
  powerUp();
}

inline void FEE::clear()
{
  memset(towers, 0, sizeof(towers));
  triggerPatch = -1;
  crate = -1;
  patch = -1;
  board = -1;
  mask = -1;
  highTowerStatus = -1;
  patchSumStatus = -1;
  bitConvMode = -1;
  formula = -1;
  memset(parameters, 0, sizeof(parameters));
  output = 0;
}

inline int FEE::numberOfMaskedTowers() const
{
  int n = 0;
  for (int i = 0; i < 16; ++i) if (!FEE::status[towers[i]]) ++n;
  return n;
}

inline int FEE::triggermask() const
{
  int mask = 0;
  for (int i = 0; i < 16; ++i) mask |= FEE::status[towers[i]] << i;
  return mask;
}

inline void FEE::powerUp()
{
  for (int sum =  0; sum <   16; ++sum) lut[sum] = 0;
  for (int sum = 16; sum <   78; ++sum) lut[sum] = sum-15;
  for (int sum = 78; sum < 4096; ++sum) lut[sum] = 62;
}

#endif	// FEE_HH
