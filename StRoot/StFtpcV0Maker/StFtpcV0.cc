// $Id: StFtpcV0.cc,v 1.3 2000/01/03 13:16:08 jcs Exp $
//
// $Log: StFtpcV0.cc,v $
// Revision 1.3  2000/01/03 13:16:08  jcs
// Add CVS Id strings
//
///////////////////////////////////////////////////////////
//  This is a class for defining V0s.
//
// Written by Mike Heffner 10/30/98
///////////////////////////////////////////////////////////

#include "StFtpcV0.hh"

StFtpcV0::StFtpcV0(StFtpcTrack track1,
               StFtpcTrack track2):StFtpcTrack()
{

  mTrack1Mass =0;
  mTrack2Mass =0;
  mMass       =0;
  mDecayVertex.setMag(0);
  mMomentum.setMag(0);
  mDca =0;
  mTrack1 = track1;
  mTrack2 = track2;

}

StFtpcV0::~StFtpcV0()
{

}

StDouble StFtpcV0::GetImpactParameter(StThreeVector<double> origin) const
{
  //StThreeVector<double> origin(0,0,0);
  return this->distance(origin);
}

StDouble StFtpcV0::GetTrack1Dca(StThreeVector<double> origin) const
{
  //StThreeVector<double> origin(0,0,0);
  return mTrack1.distance(origin);
}

StDouble StFtpcV0::GetTrack2Dca(StThreeVector<double> origin) const
{
  //StThreeVector<double> origin(0,0,0);
  return mTrack2.distance(origin);
}

StThreeVector<double> StFtpcV0::GetTrack1Momentum() const
{
return mTrack1.momentum(mTrack1.GetMagField());
}

StThreeVector<double> StFtpcV0::GetTrack2Momentum() const
{
return mTrack2.momentum(mTrack2.GetMagField());
}

double StFtpcV0::Refit()
{
  ////////////////////////////////////////////////////////
  //  This method refines the intersection of two helices.
  //This should be used after a good guess is in the 
  //member mDecayVertex.  A least square minimization is
  // preformed on a linerized model of the helices.
  //
  // reference: Data analysis techniques for high energy
  //physics experiments, R.K. Bock et. al. (1990) pp319
  // also see, STAR note 89
  //
  //written 20nov98 Mike Heffner
  ////////////////////////////////////////////////////////
  

  // the results go into these variables
  double distanceBetweenHelices=0;
  StThreeVector<double> Vertex(0,0,0);

  double SGuess= mTrack1.pathLength(mDecayVertex);
  StThreeVector<double> PointClosestApproach(0,0,0);
  PointClosestApproach = mTrack1.at(SGuess);
  StThreeVector<double> P(0,0,0);
  //P = mTrack1.momentumAt(SGuess,5*kilogauss);
  //cout<<"don't forget to chage field"<<endl;
  P = mTrack1.momentumAt(SGuess,mTrack1.GetMagField());
  P.setMag(1);

  StMatrix<double> M(3,3);
  M(1,1) = P.y()*P.y() + P.z()*P.z();
  M(1,2) = -1*P.x()*P.y();
  M(1,3) = -1*P.x()*P.z();
  M(2,1) = -1*P.x()*P.y();
  M(2,2) = P.x()*P.x() + P.z()*P.z();
  M(2,3) = -1*P.y()*P.z();
  M(3,1) = -1*P.x()*P.z();
  M(3,2) = -1*P.y()*P.z();
  M(3,3) = P.x()*P.x() + P.y()*P.y();
  
  StMatrix<double> SumM(3,3);
  SumM = M;
  StThreeVector<double> SumPcaM(0,0,0);
  SumPcaM = PointClosestApproach*M;

  //cout<<M<<endl;
  //cout<<PointClosestApproach<<endl;
  //cout<<SumPcaM<<endl;
  // helix two

  SGuess= mTrack2.pathLength(mDecayVertex);
  PointClosestApproach = mTrack2.at(SGuess);
  //P = mTrack2.momentumAt(SGuess,5*kilogauss);
  //cout<<"don't forget to chage field"<<endl;
  P = mTrack2.momentumAt(SGuess,mTrack2.GetMagField());
  P.setMag(1);

  M(1,1) = P.y()*P.y() + P.z()*P.z();
  M(1,2) = -1*P.x()*P.y();
  M(1,3) = -1*P.x()*P.z();
  M(2,1) = -1*P.x()*P.y();
  M(2,2) = P.x()*P.x() + P.z()*P.z();
  M(2,3) = -1*P.y()*P.z();
  M(3,1) = -1*P.x()*P.z();
  M(3,2) = -1*P.y()*P.z();
  M(3,3) = P.x()*P.x() + P.y()*P.y();
  
  //cout<<"M2"<<M<<endl;
  SumM += M;
  //cout<<SumM<<endl;
  SumPcaM += PointClosestApproach*M;
  size_t err;
  SumM.invert(err);
  if(err) cerr<<"Matrix inversion error"<<endl;

  //cout<<SumM<<endl;
  //cout<<M<<endl;
  //cout<<PointClosestApproach<<endl;
  //cout<<SumPcaM<<endl;
 
  Vertex = SumPcaM*SumM;
distanceBetweenHelices= abs(mTrack1.at(mTrack1.pathLength(Vertex)) -mTrack2.at(mTrack2.pathLength(Vertex))); 
// cout<<"Vertex" <<Vertex/centimeter<<endl;
// cout<<"dca of daughters"<<distanceBetweenHelices/centimeter<<endl;
 double temp=abs(Vertex-mDecayVertex);
  mDecayVertex = Vertex;
  mDca=distanceBetweenHelices;
  return temp;
}

void StFtpcV0::ComputeMassAndMomentum()
{
  // make the decay vertex the origin for the input tracks
  // since the decay vertex will most likely not be on the helix
  // the point of closest approach is used
  mTrack1.moveOrigin(mTrack1.pathLength(mDecayVertex));
  mTrack2.moveOrigin(mTrack2.pathLength(mDecayVertex));
  //make four vectors
  StLorentzVector<double> trk1(mTrack1.momentum(mTrack2.GetMagField()),mTrack1.momentum(mTrack2.GetMagField()).massHypothesis(mTrack1Mass));
  StLorentzVector<double> trk2(mTrack2.momentum(mTrack2.GetMagField()),mTrack2.momentum(mTrack2.GetMagField()).massHypothesis(mTrack2Mass));
  //define the V0 mass
  // cout<<"test----"<<trk1<<endl<<trk2<<endl<<mTrack1.momentum(mTrack1.GetMagField())<<endl<<mTrack1.momentum(mTrack1.GetMagField()).massHypothesis(mTrack1Mass)<<mTrack1<<endl;
  mMass = (trk1+trk2).m();
  //compute V0 momentum
  this->mMomentum=(mTrack1.momentum(mTrack1.GetMagField()) + mTrack2.momentum(mTrack2.GetMagField()));
  // now set the helix parameters for the V0
  StPhysicalHelix temp(mMomentum,mDecayVertex,0,1);
  this->setParameters(temp.curvature(),temp.dipAngle(),temp.phase(),mDecayVertex,temp.h());

}

StDouble StFtpcV0::GetKaonMass()
{
  mTrack1.moveOrigin(mTrack1.pathLength(mDecayVertex));
  mTrack2.moveOrigin(mTrack2.pathLength(mDecayVertex));
  
  StLorentzVector<double> trk1(mTrack1.momentum(mTrack2.GetMagField()),mTrack1.momentum(mTrack2.GetMagField()).massHypothesis(charged_pion_mass_c2));
  StLorentzVector<double> trk2(mTrack2.momentum(mTrack2.GetMagField()),mTrack2.momentum(mTrack2.GetMagField()).massHypothesis(charged_pion_mass_c2));

  trk1=trk1+trk2;
  return trk1.m();
}

StDouble StFtpcV0::GetLambdaMass() 
{
  mTrack1.moveOrigin(mTrack1.pathLength(mDecayVertex));
  mTrack2.moveOrigin(mTrack2.pathLength(mDecayVertex));

  StLorentzVector<double> trk1;
  StLorentzVector<double> trk2;
  trk1.setVect(mTrack1.momentum(mTrack2.GetMagField()));
  trk2.setVect(mTrack2.momentum(mTrack2.GetMagField()));
  if(mTrack1.charge(mTrack1.GetMagField())>0 && mTrack2.charge(mTrack2.GetMagField())<0){  
   
    trk1.setE(mTrack1.momentum(mTrack2.GetMagField()).massHypothesis(proton_mass_c2));
    trk2.setE(mTrack2.momentum(mTrack2.GetMagField()).massHypothesis(charged_pion_mass_c2));
  }else{
    trk1.setE(mTrack1.momentum(mTrack2.GetMagField()).massHypothesis(charged_pion_mass_c2));
    trk2.setE(mTrack2.momentum(mTrack2.GetMagField()).massHypothesis(proton_mass_c2));
  }

  return (trk1+trk2).m();
}


StDouble StFtpcV0::GetAntiLambdaMass() 
{
  mTrack1.moveOrigin(mTrack1.pathLength(mDecayVertex));
  mTrack2.moveOrigin(mTrack2.pathLength(mDecayVertex));

  StLorentzVector<double> trk1;
  StLorentzVector<double> trk2;
  trk1.setVect(mTrack1.momentum(mTrack2.GetMagField()));
  trk2.setVect(mTrack2.momentum(mTrack2.GetMagField()));
  if(mTrack1.charge(mTrack1.GetMagField())<0 && mTrack2.charge(mTrack2.GetMagField())>0){  
   
    trk1.setE(mTrack1.momentum(mTrack2.GetMagField()).massHypothesis(proton_mass_c2));
    trk2.setE(mTrack2.momentum(mTrack2.GetMagField()).massHypothesis(charged_pion_mass_c2));
  }else{
    trk1.setE(mTrack1.momentum(mTrack2.GetMagField()).massHypothesis(charged_pion_mass_c2));
    trk2.setE(mTrack2.momentum(mTrack2.GetMagField()).massHypothesis(proton_mass_c2));
  }

  return (trk1+trk2).m();
}



int StFtpcV0::helixIntersect(double zLimit1,double zLimit2)
{ 
  ///////////////////////////////////////////////////
  //  This method finds the intersection of helices.
  //The search is confined to the range specified, and
  //it is assumed that only on period of the helix 
  //occures in the given range.  One can check if only
  //one period occures in a given range with the member
  //onlyOnePeriod().  The method also checks the number
  //of periods.
  //  Fisrt I compute the possible intersection that is 
  //on the line connecting the helices' centers.  This is
  //done if they cross or not.  Then for crossing helices,
  //I find both crossing points.  Of these three possible
  //canidates I pick the one that is in the range, and has
  //smallest distance between helicies.
  //
  //written 20nov98 Mike Heffner
  ////////////////////////////////////////////////////////
  
  
  // the results go into these variables
  bool IsThereSolution=false; 
  double distanceBetweenHelices=0;
  StThreeVector<double> Vertex(0,0,0);
  //////////////////////////////////////////////////
  // set up of the helices and check for one period
  ///////////////////////////////////////////////////
  
  // sort input values to max and min
  double rangeMin = (zLimit1<zLimit2) ? zLimit1:zLimit2;  // min range value
  double rangeMax = (zLimit1>zLimit2) ? zLimit1:zLimit2;  // max range value
  
  //Move the origin of both helices to the middle of the z range
  //specified by the max and min zLimits.  This is done by finding
  //the intersection of the helix with a plane defined by
  // z=rangeMin+rangeMax/2.  
  
  StThreeVector<double> vectorMiddle(0,0,(rangeMin+rangeMax)/2);
  StThreeVector<double> vectorNormal(0,0,1);

  double sAtMiddle1 = mTrack1.pathLength(vectorMiddle,vectorNormal);
  double sAtMiddle2 = mTrack2.pathLength(vectorMiddle,vectorNormal);
  
  mTrack1.moveOrigin(sAtMiddle1);
  mTrack2.moveOrigin(sAtMiddle2);

  //Now check that only one period occurs in the range specified.
  // the sign of the dip angle is needed to check if in range
  double sign1= mTrack1.dipAngle()/fabs(mTrack1.dipAngle());
  double sign2= mTrack1.dipAngle()/fabs(mTrack2.dipAngle());

  double zMiddlePlusOneHalfPeriod1 = mTrack1.z(sign1*mTrack1.period()/2);
  double zMiddlePlusOneHalfPeriod2 = mTrack2.z(sign2*mTrack2.period()/2);
  
  if(zMiddlePlusOneHalfPeriod1<rangeMax || zMiddlePlusOneHalfPeriod2<rangeMax){
    cerr<<"FTPC V0 Warning:Helix intersection can only handle one helix period"<<endl;
    return false;
  }
  
  //////////////////////////////////////
  // find the intersection on the line
  // betweent the centers of the helices
  ///////////////////////////////////////
  
  // Define vectors to the center of each helix, center means in the
  // x-y plane.
  StThreeVector<double> helix1Center(mTrack1.xcenter(),mTrack1.ycenter(),0);
  StThreeVector<double> helix2Center(mTrack2.xcenter(),mTrack2.ycenter(),0);
  
  // take the difference
  StThreeVector<double> difference(0,0,0);
  difference=helix2Center-helix1Center;

  //compute the mid point between the two.
  StThreeVector<double> Intersection(0,0,0);
  Intersection = helix1Center +difference*(abs(difference) + 1/mTrack1.curvature() - 1/mTrack2.curvature())/2/abs(difference);

  //Check that Intersection is in the range
  StThreeVector<double> H1temp(0,0,0);
  //H1temp=mTrack1.at(mTrack1.fudgePathLength(Intersection));
  H1temp=mTrack1.at(mTrack1.pathLength(Intersection.x(),Intersection.y()));
  StThreeVector<double> H2temp(0,0,0);
  //H2temp=mTrack2.at(mTrack2.fudgePathLength(Intersection));
  H2temp=mTrack2.at(mTrack2.pathLength(Intersection.x(),Intersection.y()));
  
  if(H1temp.z()>rangeMin && H1temp.z()<rangeMax &&
     H2temp.z()>rangeMin && H2temp.z()<rangeMax){
    
    //compute dist between helices and set the Vertex vector
    distanceBetweenHelices = abs(H1temp - H2temp);
    Vertex = H1temp +(H2temp-H1temp)*(distanceBetweenHelices + 1/mTrack1.curvature() - 1/mTrack2.curvature())/2/distanceBetweenHelices;
    IsThereSolution=true;
  }
  
  ////////////////////////////////////////////////////
  // The case that the helices cross in the x-y plane.
  ///////////////////////////////////////////////////
  
  if(abs(difference)<(1/mTrack1.curvature()+1/mTrack2.curvature())){
    
    StThreeVector<double> Intersection1(0,0,0);  // These are the two
    StThreeVector<double> Intersection2(0,0,0); // possible crossings
    
    //angle from theta to intersection point in H1 system 
    //see note book page 18
    // operations are done from Helix1's point of view
    
    double cosdthH1 = (abs(difference)*abs(difference) + (1/(mTrack1.curvature()))*(1/(mTrack1.curvature())) - (1/(mTrack2.curvature()))*(1/(mTrack2.curvature())) )/(2*abs(difference)*(1/(mTrack1.curvature())));
    
    Intersection1 = difference;
    Intersection2 = difference;
    
    Intersection1.rotateZ(acos(cosdthH1));
    Intersection2.rotateZ(-1*acos(cosdthH1));
    
    Intersection1.setMag(1/mTrack1.curvature());
    Intersection2.setMag(1/mTrack1.curvature());
    
    Intersection1 = Intersection1 + helix1Center;
    Intersection2 = Intersection2 + helix1Center;
    
    //Check that Intersection1 is in the range.
    //Then check if distanceBetweenHelices is smaller
    //If so, set the current distanceBetweenHelices
    // as the best choice.
    //H1temp=mTrack1.at(mTrack1.fudgePathLength(Intersection1));
    //H2temp=mTrack2.at(mTrack2.fudgePathLength(Intersection1));
    H1temp=mTrack1.at(mTrack1.pathLength(Intersection1.x(),Intersection1.y()));
    H2temp=mTrack2.at(mTrack2.pathLength(Intersection1.x(),Intersection1.y()));
    
    if(H1temp.z()>rangeMin && H1temp.z()<rangeMax &&
       H2temp.z()>rangeMin && H2temp.z()<rangeMax){
      
      if(abs(H1temp - H2temp)<distanceBetweenHelices||!IsThereSolution){
	distanceBetweenHelices = abs(H1temp - H2temp);
	Vertex = (H1temp+H2temp)/2;
	IsThereSolution=true;
      }
    }

    //Again for Intersection2

    //H1temp=mTrack1.at(mTrack1.fudgePathLength(Intersection2));
    //H2temp=mTrack2.at(mTrack2.fudgePathLength(Intersection2));
    H1temp=mTrack1.at(mTrack1.pathLength(Intersection2.x(),Intersection2.y()));
    H2temp=mTrack2.at(mTrack2.pathLength(Intersection2.x(),Intersection2.y()));
    
    if(H1temp.z()>rangeMin && H1temp.z()<rangeMax &&
       H2temp.z()>rangeMin && H2temp.z()<rangeMax){

      if(abs(H1temp - H2temp)<distanceBetweenHelices||!IsThereSolution){
	distanceBetweenHelices = abs(H1temp - H2temp);
	Vertex = (H1temp+H2temp)/2;
	IsThereSolution=true;
      }
    }

  }

  if(IsThereSolution){
    mDecayVertex = Vertex;
    mDca=distanceBetweenHelices;
    // cout<<"Vertex" <<Vertex/centimeter<<endl;
    //cout<<"dca of daughters"<<distanceBetweenHelices/centimeter<<endl;

  }
  return IsThereSolution;
}




void StFtpcV0::ComputeDecayVertex()
{
  /////////////////////////////////////////////////////////////////////
  // This member function calls the Helix Intersection member function
  // program, and fills the correct members.
  /////////////////////////////////////////////////////////////////////
  const double first_ftpc_row =162.65;// this is temporary
  double PI = 2*asin(1.);
  double xcH1 = mTrack1.xcenter()/centimeter;
  double ycH1 = mTrack1.ycenter()/centimeter;
  double radiusH1;
  if (mTrack1.curvature()>0){
  radiusH1 =1/(mTrack1.curvature())/centimeter;
  }else{
    cerr<<"curvature is zero"<<endl;
    radiusH1=0;
  }
  double pitchH1 = (-1*mTrack1.h())*tan(mTrack1.dipAngle());
  //double zH1 = mTrack1.GetLocationOfMomentum().z()/centimeter;
  double zH1 = mTrack1.z(0)/centimeter;
  //double phiH1 = mTrack1.GetLocationOfMomentum().phi()/radian;
  double phiH1=mTrack1.phase();
  if (phiH1<0) phiH1 += 2*PI;
  if (phiH1>2*PI) phiH1 -= 2*PI;
  //cout<<"phiH1 "<<phiH1<<endl;
  phiH1 -= (first_ftpc_row-zH1)*pitchH1/radiusH1;
  zH1=first_ftpc_row;

  double xcH2 = mTrack2.xcenter()/centimeter;
  double ycH2 = mTrack2.ycenter()/centimeter;
  double radiusH2;
  if (mTrack2.curvature()>0){
  radiusH2 =1/(mTrack2.curvature())/centimeter;
  }else{
    cerr<<"curvature is zero"<<endl;
  radiusH2=0;
  }
  double pitchH2 = (-1*mTrack2.h())*tan(mTrack2.dipAngle());
  //double zH2 = mTrack2.GetLocationOfMomentum().z()/centimeter;
  double zH2 = mTrack2.z(0)/centimeter;
  // double phiH2 = mTrack2.GetLocationOfMomentum().phi()/radian;
  double phiH2=mTrack2.phase();
  if (phiH2<0) phiH2 += 2*PI;
  if (phiH2>2*PI) phiH2 -= 2*PI;
  //cout<<"phiH2 "<<phiH2<<endl;

  phiH2 -= (first_ftpc_row-zH2)*pitchH2/radiusH2;
  zH2=first_ftpc_row;

  double x,y,z,deltaZ;

  //cout<<endl<<"----------------------------------------"<<endl;
  //cout<<"helix intecpt param"<<endl;
  //cout<<xcH1<<" "<<ycH1<<" "<<radiusH1<<" "<<pitchH1<<" "<<zH1<<" "<<phiH1<<endl;
  //cout<<xcH2<<" "<<ycH2<<" "<<radiusH2<<" "<<pitchH2<<" "<<zH2<<" "<<phiH2<<endl;

  //cout<<endl;

  //cout<<HelixInter(xcH1,ycH1,pitchH1,radiusH1,zH1,phiH1,xcH2,ycH2,pitchH2,radiusH2,zH2,phiH2,x,y,z,deltaZ)<<endl;

  if(HelixInter(xcH1,ycH1,pitchH1,radiusH1,zH1,phiH1,xcH2,ycH2,pitchH2,radiusH2,zH2,phiH2,x,y,z,deltaZ) ==2) {

    //cout<<"xyzZ"<<x<<y<<z<<deltaZ<<endl;
    mDecayVertex.setX(x*centimeter);
    mDecayVertex.setY(y*centimeter);
    mDecayVertex.setZ(z*centimeter);
    mDca=deltaZ*centimeter;

    // cout<<x<<" "<<y<<" "<<z<<" "<<deltaZ<<endl;
    //cout<<"-------------------------------------------------------------------------------------------------------------------------------------------------------------------"<<endl<<endl<<endl<<endl<<endl;
  }else{

    //  cout<<"Helix intercept failed"<<endl;
    mDca=-1;
  }
   
}


int StFtpcV0::HelixInter(double xcH1,double ycH1,double pitchH1,double radiusH1,double zH1,double phiH1,double xcH2,double ycH2,double pitchH2,double radiusH2,double zH2,double phiH2,double &x,double &y,double &z,double &deltaZ)
{ 
  ////////////////////////////////////////////////////////////////////////
  //  Helix Intersection
  //
  // This program calculates the intersection of two helices.  Only the first two cycles
  // of the helix are considered.
  //
  // Input:
  // xcH,ycH - center of the helix.  (axis)
  // pitchH - r*phi/z distance along projected x-y circle/ distance along axis  
  //            the sign of the pitch is the charge of the particle
  // radiusH - well, ahh... the radius
  // zH   - z position of the first plane of the dectector in realtion to main vertex
  // phiH - phi of helix at z, phi=0 is along the x axis
  //
  //Output:
  // x,y,z - location of possible intersection,
  // deltaZ - distance between the helices in z dir
  // function output = 1 if no crossing in xy, 2 if crossing, 0 if error.
  //                   
  //
  // written by Mike Heffner 22 sept 98
  // see notebook page 18
  ///////////////////////////////////////////////////////////////////////

  // pitch is defined with the wrong sign in the rest of the program
  // I will fix it here since this is more simple.
  //  (Only two choices I and I pick the wrong one!!)

  pitchH1 = -1*pitchH1;
  pitchH2 = -1*pitchH2;

  double Zmin = 0;  // This is the minimum z that the V0 vertex can have

  double PI = 2*asin(1.);

  int status = 0;
  double w = sqrt( (xcH1-xcH2)*(xcH1-xcH2) + (ycH1-ycH2)*(ycH1-ycH2) ); //dist between centers

  double theta=atan2(ycH2-ycH1,xcH2-xcH1);  // angle between centers
  if (theta<0) theta = theta + 2*PI;
  
  if(w>=radiusH1+radiusH2){ // circles don't cross, or just touch

    double IPH1a = theta-phiH1; //Intersection point helix 1, transformed as in notebook
    if (IPH1a<0) IPH1a=IPH1a+2*PI;
    if (IPH1a>2*PI) IPH1a=IPH1a-2*PI;
  
    double IPH2a = theta-phiH2+PI; //Intersection point helix 2
    if(theta>PI) IPH2a = theta-phiH2-PI;
    if (IPH2a<0) IPH2a=IPH2a+2*PI;
    if (IPH2a>2*PI) IPH2a=IPH2a-2*PI;
  
    double zCanidateH1a[2];
    double zCanidateH2a[2];
    double DeltaTheta=0;
  
    // find z of closest points
    if(pitchH1>0) DeltaTheta=2*PI-IPH1a;
    if(pitchH1<0) DeltaTheta=IPH1a;
    zCanidateH1a[0] = zH1 - radiusH1*DeltaTheta*fabs(pitchH1);
    zCanidateH1a[1] = zH1 - radiusH1*(DeltaTheta+2*PI)*fabs(pitchH1);
  
    if(pitchH2>0) DeltaTheta=2*PI-IPH2a;
    if(pitchH2<0) DeltaTheta=IPH2a;
    zCanidateH2a[0] = zH2 - radiusH2*DeltaTheta*fabs(pitchH2);
    zCanidateH2a[1] = zH2 - radiusH2*(DeltaTheta+2*PI)*fabs(pitchH2);

    //set deltaZ and z, based on smallest deltaZ
    deltaZ=9999;

    if(zCanidateH1a[0]>Zmin && zCanidateH2a[0]>Zmin){
      deltaZ = fabs(zCanidateH1a[0]-zCanidateH2a[0]);
      z = (zCanidateH1a[0]+zCanidateH2a[0])/2;
      status =1;
    }

    double temp = fabs(zCanidateH1a[0]-zCanidateH2a[1]);
    if(deltaZ>temp && zCanidateH1a[0]>Zmin && zCanidateH2a[1]>Zmin ){
      deltaZ=temp;
      z = (zCanidateH1a[0]+zCanidateH2a[1])/2;
      status=1;
    }

    temp = fabs(zCanidateH1a[1]-zCanidateH2a[0]);
    if(deltaZ>temp && zCanidateH1a[1]>Zmin && zCanidateH2a[0]>Zmin ){
      deltaZ=temp;
      z = (zCanidateH1a[1]+zCanidateH2a[0])/2;
      status=1;
    }

    temp = fabs(zCanidateH1a[1]-zCanidateH2a[1]);
    if(deltaZ>temp && zCanidateH1a[1]>Zmin && zCanidateH2a[1]>Zmin ){
      deltaZ=temp;
      z = (zCanidateH1a[1]+zCanidateH2a[1])/2;
      status=1;
    }
    
    // set intersection points

    x = (radiusH1+(w-radiusH1-radiusH2)/2)*cos(theta) +xcH1; 
    y = (radiusH1+(w-radiusH1-radiusH2)/2)*sin(theta) +ycH1; 

    //cout<<"don't cross"<<endl;
  } //end circles don't cross, or just touch


  if(w<radiusH1+radiusH2){ //circles cross

    //    cout<<"cross"<<endl;

    //angle from theta to intersection point in H1 system
    double cosdthH1 = (w*w + radiusH1*radiusH1 - radiusH2*radiusH2)/(2*w*radiusH1);
    //angle from theta to intersection point in H2 system
    double cosdthH2 = (w*w - radiusH1*radiusH1 + radiusH2*radiusH2)/(2*w*radiusH2);

    
    double IPH1[2]; //Intersection points helix 1, transformed as in notebook
    IPH1[0] = theta-phiH1+acos(cosdthH1);
    IPH1[1] = theta-phiH1-acos(cosdthH1);    
    if (IPH1[0]<0) IPH1[0]=IPH1[0]+2*PI;
    if (IPH1[1]<0) IPH1[1]=IPH1[1]+2*PI;
    if (IPH1[0]>=2*PI) IPH1[0]=IPH1[0]-2*PI;
    if (IPH1[1]>=2*PI) IPH1[1]=IPH1[1]-2*PI;
    
    double IPH2[2]; //Intersection points helix 2, transformed as in notebook
    IPH2[0] = theta-phiH2-acos(cosdthH2)+PI;
    IPH2[1] = theta-phiH2+acos(cosdthH2)+PI;
    if(theta>PI) {
      IPH2[0] = theta-phiH2-acos(cosdthH2)-PI;
      IPH2[1] = theta-phiH2+acos(cosdthH2)-PI;
    }
    if (IPH2[0]<0) IPH2[0]=IPH2[0]+2*PI;
    if (IPH2[1]<0) IPH2[1]=IPH2[1]+2*PI;
    if (IPH2[0]>=2*PI) IPH2[0]=IPH2[0]-2*PI;
    if (IPH2[1]>=2*PI) IPH2[1]=IPH2[1]-2*PI;

    double zCanidateH1[2][2];
    double zCanidateH2[2][2];
    double DeltaTheta=0;
  
    // find z of closest points
    if(pitchH1>0) DeltaTheta=2*PI-IPH1[0];
    if(pitchH1<0) DeltaTheta=IPH1[0];
    zCanidateH1[0][0] = zH1 - radiusH1*DeltaTheta*fabs(pitchH1);
    zCanidateH1[1][0] = zH1 - radiusH1*(DeltaTheta+2*PI)*fabs(pitchH1);
  
    //cout<<"deltatheta"<<DeltaTheta<<endl;

    if(pitchH1>0) DeltaTheta=2*PI-IPH1[1];
    if(pitchH1<0) DeltaTheta=IPH1[1];
    zCanidateH1[0][1] = zH1 - radiusH1*DeltaTheta*fabs(pitchH1);
    zCanidateH1[1][1] = zH1 - radiusH1*(DeltaTheta+2*PI)*fabs(pitchH1);

    //cout<<"deltatheta"<<DeltaTheta<<endl;
  
    if(pitchH2>0) DeltaTheta=2*PI-IPH2[0];
    if(pitchH2<0) DeltaTheta=IPH2[0];
    zCanidateH2[0][0] = zH2 - radiusH2*DeltaTheta*fabs(pitchH2);
    zCanidateH2[1][0] = zH2 - radiusH2*(DeltaTheta+2*PI)*fabs(pitchH2);

    //cout<<"deltatheta"<<DeltaTheta<<endl;

    if(pitchH2>0) DeltaTheta=2*PI-IPH2[1];
    if(pitchH2<0) DeltaTheta=IPH2[1];
    zCanidateH2[0][1] = zH2 - radiusH2*DeltaTheta*fabs(pitchH2);
    zCanidateH2[1][1] = zH2 - radiusH2*(DeltaTheta+2*PI)*fabs(pitchH2);

    //cout<<"deltatheta"<<DeltaTheta<<endl;

    //cout<<"Radius1 "<<radiusH1<<" Radius2 "<<radiusH2<<endl;
    //cout<<"Pitch1 "<<pitchH1<<" Pitch2 "<<pitchH2<<endl;
    /*for(int m=0;m<2;m++){
      for(int n=0;n<2;n++){
	cout<<zCanidateH1[m][n]<<" "<<zCanidateH2[m][n]<<endl;
      }
    }*/
    //set deltaZ and z, based on smallest deltaZ
    deltaZ=9999;

      // first point

    if(zCanidateH1[0][0]>Zmin && zCanidateH2[0][0]>Zmin){
      deltaZ = fabs(zCanidateH1[0][0]-zCanidateH2[0][0]);
      z = (zCanidateH1[0][0]+zCanidateH2[0][0])/2;
      status =-1;
    }

    double temp;
    temp = fabs(zCanidateH1[0][0]-zCanidateH2[1][0]);
    if(deltaZ>temp && zCanidateH1[0][0]>Zmin && zCanidateH2[1][0]>Zmin ){
      deltaZ=temp;
      z = (zCanidateH1[0][0]+zCanidateH2[1][0])/2;
      status=-1;
    }

    temp = fabs(zCanidateH1[1][0]-zCanidateH2[0][0]);
    if(deltaZ>temp && zCanidateH1[1][0]>Zmin && zCanidateH2[0][0]>Zmin ){
      deltaZ=temp;
      z = (zCanidateH1[1][0]+zCanidateH2[0][0])/2;
      status=-1;
    }

    temp = fabs(zCanidateH1[1][0]-zCanidateH2[1][0]);
    if(deltaZ>temp && zCanidateH1[1][0]>Zmin && zCanidateH2[1][0]>Zmin ){
      deltaZ=temp;
      z = (zCanidateH1[1][0]+zCanidateH2[1][0])/2;
      status=-1;
    }

    
      // second point

    temp = fabs(zCanidateH1[0][1]-zCanidateH2[0][1]);
    if(deltaZ>temp && zCanidateH1[0][1]>Zmin && zCanidateH2[0][1]>Zmin){
      deltaZ = temp;
      z = (zCanidateH1[0][1]+zCanidateH2[0][1])/2;
      status =2;
    }

    temp = fabs(zCanidateH1[0][1]-zCanidateH2[1][1]);
    if(deltaZ>temp && zCanidateH1[0][1]>Zmin && zCanidateH2[1][1]>Zmin ){
      deltaZ=temp;
      z = (zCanidateH1[0][1]+zCanidateH2[1][1])/2;
      status=2;
    }

    temp = fabs(zCanidateH1[1][1]-zCanidateH2[0][1]);
    if(deltaZ>temp && zCanidateH1[1][1]>Zmin && zCanidateH2[0][1]>Zmin ){
      deltaZ=temp;
      z = (zCanidateH1[1][1]+zCanidateH2[0][1])/2;
      status=2;
    }

    temp = fabs(zCanidateH1[1][1]-zCanidateH2[1][1]);
    if(deltaZ>temp && zCanidateH1[1][1]>Zmin && zCanidateH2[1][1]>Zmin ){
      deltaZ=temp;
      z = (zCanidateH1[1][1]+zCanidateH2[1][1])/2;
      status=2;
    }
    // set intersection points

    if(status==2){
      x= xcH1 + radiusH1*cos(theta-acos(cosdthH1));
      y= ycH1 + radiusH1*sin(theta-acos(cosdthH1));
    }

    if(status==-1){
      x= xcH1 + radiusH1*cos(theta+acos(cosdthH1));
      y= ycH1 + radiusH1*sin(theta+acos(cosdthH1));
      status=2;
    }
    //cout<<"status "<<status<<endl;
  } //end circles cross

  return status;
}

