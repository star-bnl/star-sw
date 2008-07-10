// -*- mode: c++;-*-
// $Id: StBET4pMakerImpBuilder.h,v 1.2 2008/07/10 09:36:01 tai Exp $
#ifndef STBET4PMAKERIMPBUILDER_HH
#define STBET4PMAKERIMPBUILDER_HH

class StMuDstMaker;
class StBET4pMakerImp;

namespace StSpinJet {


class StBET4pMakerImpBuilder { 

public:
  StBET4pMakerImpBuilder() { }
  virtual ~StBET4pMakerImpBuilder() { }

  StBET4pMakerImp* build(bool useTPC = true, bool useBEMC = true, bool useEEMC = false, bool use2003Cuts = false, bool use2005Cuts = false, bool use2006Cuts = false, StMuDstMaker* uDstMaker = 0, bool doTowerSwapFix = true);


private:

};

}

#endif // STBET4PMAKERIMPBUILDER_HH
