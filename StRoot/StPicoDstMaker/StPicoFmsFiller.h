#ifndef StPicoFmsFiller_h
#define StPicoFmsFiller_h


class StMuDst;
class StPicoDst;
class StFmsDbMaker;


class StPicoFmsFiller
{
public:

  StPicoFmsFiller(StPicoDst& picoDst);

  void fill(const StMuDst& muDst, const StFmsDbMaker* fmsDbMaker = nullptr);

private:

  StPicoDst&  mPicoDst;

};

#endif

