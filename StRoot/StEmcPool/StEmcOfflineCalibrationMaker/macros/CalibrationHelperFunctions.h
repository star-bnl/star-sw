//Basic functions for geometries, bad towers, etc.

#ifndef CalibrationHelperFunctions_HH
#define CalibrationHelperFunctions_HH

class CalibrationHelperFunctions
{
 private:	
  static const int ntowers = 4800;
	
  float tower_eta[ntowers];
  float tower_phi[ntowers];
  float tower_theta[ntowers];
	
  //these functions use switch statements to set tower status arrays in ctor
  int towerStatus2004(int towerid);
  int towerStatus2005(int towerid);
  int towerStatus2006(int towerid);
	
 public:
  CalibrationHelperFunctions();
  ~CalibrationHelperFunctions();
	
  float getEta(int towerid)	{return tower_eta[towerid-1];}
  float getTheta(int towerid) {return tower_theta[towerid-1];}
  float getPhi(int towerid)	{return tower_phi[towerid-1];}
	
  bool isGoodTower2004(int towerid);
  bool isGoodTower2005(int towerid);
  bool isGoodTower2006(int towerid);
	
  bool isBadIso2005(int towerid);
  bool isBadPoverE2005(int towerid);
};

#endif
