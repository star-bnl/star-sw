#ifndef StEventCuts_HH
#define StEventCuts_HH

class StEvent;

class StEventCuts {

public:
  StEventCuts();
  ~StEventCuts();
  int EventSatisfiesCuts(StEvent& ev);
  void  SetMultiplicityCuts(double lowerCut, double upperCut);
  
 private:
  double upperMult,lowerMult;
};

#endif
