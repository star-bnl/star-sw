#ifndef BemcGeometry_h
#define BemcGeometry_h

class BemcGeometry {
 private:
  static BemcGeometry* _instance;
  BemcGeometry();
  double mEta[4801];
  double mPhi[4801];

 public:
  static BemcGeometry* instance();
  double eta(unsigned int softId) { return mEta[softId]; }
  double phi(unsigned int softId) { return mPhi[softId]; }
};


#endif
