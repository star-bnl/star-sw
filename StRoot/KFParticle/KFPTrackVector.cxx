//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#include "KFPTrackVector.h"

void KFPTrackVector::SetParameter(const float_v& value, int iP, int iTr)
{ 
//   if( (iTr+float_vLen) < Size())
//     reinterpret_cast<float_v&>(fP[iP][iTr]) = value;
//   else
//   {
//     const uint_v index(uint_v::IndexesFromZero());
//     (reinterpret_cast<float_v&>(fP[iP][iTr])).gather(reinterpret_cast<const float*>(&value), index, float_m(index<(Size() - iTr)));
//   }
  
  if( (iTr+float_vLen) < Size())
    reinterpret_cast<float_v&>(fP[iP][iTr]) = value;
  else
    for(int i=0; i<float_v::Size; i++)
    {
      if(iTr + i >= Size()) continue;
      fP[iP][iTr+i] = value[i];
    } 
}
void KFPTrackVector::SetCovariance(const float_v& value, int iC, int iTr) 
{ 
//   if( (iTr+float_vLen) < Size())
//     reinterpret_cast<float_v&>(fC[iC][iTr]) = value;
//   else
//   {
//     const uint_v index(uint_v::IndexesFromZero());
//     (reinterpret_cast<float_v&>(fC[iC][iTr])).gather(reinterpret_cast<const float*>(&value), index, float_m(index<(Size() - iTr)));
//   }
  
  if( (iTr+float_vLen) < Size())
    reinterpret_cast<float_v&>(fC[iC][iTr]) = value;
  else
    for(int i=0; i<float_v::Size; i++)
    {
      if(iTr + i >= Size()) continue;
      fC[iC][iTr+i] = value[i];
    } 
}
  

void KFPTrackVector::Resize(const int n)
{
  for(int i=0; i<6; i++)
    fP[i].resize(n);
  for(int i=0; i<21; i++)
    fC[i].resize(n);
#ifdef NonhomogeneousField
  for(int i=0; i<10; i++)
    fField[i].resize(n);
#endif
//     fChi2.resize(n);
//     fNDF.resize(n);
  fId.resize(n);
  fPDG.resize(n);
  fQ.resize(n);
  fPVIndex.resize(n);
  fNPixelHits.resize(n);
}

void KFPTrackVector::Set(KFPTrackVector& v, int vSize, int offset)
{
  for(int iV=0; iV<vSize; iV++)
  {
    for(int i=0; i<6; i++)
      fP[i][offset+iV] = v.fP[i][iV];
    for(int i=0; i<21; i++)
      fC[i][offset+iV] = v.fC[i][iV];
#ifdef NonhomogeneousField
    for(int i=0; i<10; i++)
      fField[i][offset+iV] = v.fField[i][iV];
#endif
//       fChi2[offset+iV] = v.fChi2[iV];
//       fNDF[offset+iV] = v.fNDF[iV];
    fId[offset+iV] = v.fId[iV];
    fPDG[offset+iV] = v.fPDG[iV];
    fQ[offset+iV] = v.fQ[iV];
    fPVIndex[offset+iV] = v.fPVIndex[iV];
    fNPixelHits[offset+iV] = v.fNPixelHits[iV];
  }
}

void KFPTrackVector::SetTracks(const KFPTrackVector& track, const kfvector_uint& trackIndex, const int nIndexes)
{
  //track - input vector of tracks
  //trackIndex - indexes of tracks in a vector "track", which should be stored 
  //to the current vector

  if(nIndexes == 0) return;
  
  Resize(nIndexes);

  for(int iP=0; iP<6; iP++)
  {
    int iElement = 0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      float_v& vec = reinterpret_cast<float_v&>(fP[iP][iElement]);
      vec.gather(&(track.fP[iP][0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    float_v& vec = reinterpret_cast<float_v&>(fP[iP][iElement]);
    vec.gather(&(track.fP[iP][0]), index, float_m(iElement+uint_v::IndexesFromZero()<nIndexes));
    
  }
  for(int iC=0; iC<21; iC++)
  {
    int iElement=0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      float_v& vec = reinterpret_cast<float_v&>(fC[iC][iElement]);
      vec.gather(&(track.fC[iC][0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    float_v& vec = reinterpret_cast<float_v&>(fC[iC][iElement]);
    vec.gather(&(track.fC[iC][0]), index, float_m(iElement+uint_v::IndexesFromZero()<nIndexes));
  }
#ifdef NonhomogeneousField
  for(int iP=0; iP<10; iP++)
  {
    int iElement = 0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      float_v& vec = reinterpret_cast<float_v&>(fField[iP][iElement]);
      vec.gather(&(track.fField[iP][0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    float_v& vec = reinterpret_cast<float_v&>(fField[iP][iElement]);
    vec.gather(&(track.fField[iP][0]), index, float_m(iElement+uint_v::IndexesFromZero()<nIndexes)); 
  }
#endif
  {
    int iElement=0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      int_v& vec = reinterpret_cast<int_v&>(fId[iElement]);
      vec.gather(&(track.fId[0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    int_v& vec = reinterpret_cast<int_v&>(fId[iElement]);
    vec.gather(&(track.fId[0]), index, int_m(iElement+uint_v::IndexesFromZero()<nIndexes));
  }
  {
    int iElement=0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      int_v& vec = reinterpret_cast<int_v&>(fPDG[iElement]);
      vec.gather(&(track.fPDG[0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    int_v& vec = reinterpret_cast<int_v&>(fPDG[iElement]);
    vec.gather(&(track.fPDG[0]), index, int_m(iElement+uint_v::IndexesFromZero()<nIndexes));
  }
  {
    int iElement=0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      int_v& vec = reinterpret_cast<int_v&>(fQ[iElement]);
      vec.gather(&(track.fQ[0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    int_v& vec = reinterpret_cast<int_v&>(fQ[iElement]);
    vec.gather(&(track.fQ[0]), index, int_m(iElement+uint_v::IndexesFromZero()<nIndexes));
  }
  {
    int iElement=0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      int_v& vec = reinterpret_cast<int_v&>(fPVIndex[iElement]);
      vec.gather(&(track.fPVIndex[0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    int_v& vec = reinterpret_cast<int_v&>(fPVIndex[iElement]);
    vec.gather(&(track.fPVIndex[0]), index, int_m(iElement+uint_v::IndexesFromZero()<nIndexes));
  }
  {
    int iElement=0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      int_v& vec = reinterpret_cast<int_v&>(fNPixelHits[iElement]);
      vec.gather(&(track.fNPixelHits[0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    int_v& vec = reinterpret_cast<int_v&>(fNPixelHits[iElement]);
    vec.gather(&(track.fNPixelHits[0]), index, int_m(iElement+uint_v::IndexesFromZero()<nIndexes));
  }
}

void KFPTrackVector::GetTrack(KFPTrack& track, const int n)
{ 
  track.SetParameters(fP[0][n],fP[1][n],fP[2][n],fP[3][n],fP[4][n],fP[5][n]);
  for(int i=0; i<21; i++)
    track.SetCovariance(i,fC[i][n]);
//     track.SetChi2(fChi2[n]);
//     track.SetNDF(fNDF[n]);
  track.SetId(fId[n]);
  track.SetCharge(fQ[n]);
  
#ifdef NonhomogeneousField
    for(int i=0; i<10; i++)
      track.SetFieldCoeff( fField[i][n], i);
#endif
}


  // rotate on alpha in XY plane
void KFPTrackVector::RotateXY( float_v alpha, int firstElement )
{
  const float_v cA = KFPMath::Cos( alpha );
  const float_v sA = KFPMath::Sin( alpha );


  //float J[6][6] = { {  cA, -sA,  0,    0,    0,   0 }, // X
  //                  {  sA,  cA,  0,    0,    0,   0 }, // Y
  //                  {  0,   0,   1,    0,    0,   0 }, // Z
  //                  {  0,   0,   0,   cA,  -sA,   0 }, // Px
  //                  {  0,   0,   0,   sA,   cA,   0 }, // Py
  //                  {  0,   0,   0,    0,    0,   1 } }; // Pz

  const float_v xInit = reinterpret_cast<const float_v&>(fP[0][firstElement]);
  const float_v yInit = reinterpret_cast<const float_v&>(fP[1][firstElement]);

  float_v& x = reinterpret_cast<float_v&>(fP[0][firstElement]);
  float_v& y = reinterpret_cast<float_v&>(fP[1][firstElement]);
  
  x = -(xInit*sA +  yInit*cA);
  y = xInit*cA -  yInit*sA;

  const float_v pxInit = reinterpret_cast<const float_v&>(fP[3][firstElement]);
  const float_v pyInit = reinterpret_cast<const float_v&>(fP[4][firstElement]);

  float_v& px = reinterpret_cast<float_v&>(fP[3][firstElement]);
  float_v& py = reinterpret_cast<float_v&>(fP[4][firstElement]);
  
  px = -(pxInit*sA +  pyInit*cA);
  py = pxInit*cA -  pyInit*sA;

  float_v cov[21];
  for(int iC=0; iC<21; iC++)
    cov[iC] = reinterpret_cast<const float_v&>(fC[iC][firstElement]);

  reinterpret_cast<float_v&>(fC[0][firstElement]) = cA*cA*  cov[2] + 2* cA* cov[1]* sA + cov[0]*sA* sA;
  
  reinterpret_cast<float_v&>(fC[1][firstElement]) = -(cA*cA * cov[1]) + cA* (-cov[0] + cov[2])* sA + cov[1]*sA* sA;
  reinterpret_cast<float_v&>(fC[2][firstElement]) = cA*cA*  cov[0] - 2* cA* cov[1]* sA + cov[2]*sA* sA; 
  
  reinterpret_cast<float_v&>(fC[3][firstElement]) = -(cA* cov[4]) - cov[3]* sA;
  reinterpret_cast<float_v&>(fC[4][firstElement]) = cA* cov[3] - cov[4]* sA;
//  reinterpret_cast<float_v&>(fC[5][firstElement]) = cov[5]; 
  
  reinterpret_cast<float_v&>(fC[6][firstElement]) = cA*cA*  cov[11] + cA *(cov[10] + cov[7])* sA + cov[6]*sA* sA;
  reinterpret_cast<float_v&>(fC[7][firstElement]) = -(cA*cA * cov[10]) + cA* (cov[11] - cov[6])* sA + cov[7] *sA*sA;
  reinterpret_cast<float_v&>(fC[8][firstElement]) = -(cA *cov[12]) - cov[8] *sA;
  reinterpret_cast<float_v&>(fC[9][firstElement]) = cA*cA*  cov[14] + 2 *cA* cov[13]* sA + cov[9]* sA*sA;

  reinterpret_cast<float_v&>(fC[10][firstElement]) = -(cA*cA*  cov[7]) + cA* (cov[11] - cov[6])* sA + cov[10]*sA* sA; 
  reinterpret_cast<float_v&>(fC[11][firstElement]) = cA*cA*  cov[6] - cA* (cov[10] + cov[7]) *sA + cov[11]*sA* sA;
  reinterpret_cast<float_v&>(fC[12][firstElement]) = cA* cov[8] - cov[12]* sA; 
  reinterpret_cast<float_v&>(fC[13][firstElement]) = -(cA*cA*  cov[13]) + cA* (cov[14] - cov[9])* sA + cov[13]* sA*sA;
  reinterpret_cast<float_v&>(fC[14][firstElement]) = cA*cA*  cov[9] - 2* cA* cov[13]* sA + cov[14]* sA*sA;
  
  reinterpret_cast<float_v&>(fC[15][firstElement]) = -(cA* cov[16]) - cov[15]* sA;
  reinterpret_cast<float_v&>(fC[16][firstElement]) = cA* cov[15] - cov[16]* sA;
//  reinterpret_cast<float_v&>(fC[17][firstElement]) = cov[17]; 
  reinterpret_cast<float_v&>(fC[18][firstElement]) = -(cA* cov[19]) - cov[18]* sA;
  reinterpret_cast<float_v&>(fC[19][firstElement]) = cA* cov[18] - cov[19]* sA;
//  reinterpret_cast<float_v&>(fC[20][firstElement]) = cov[20];
} // RotateXY


void KFPTrackVector::PrintTrack(int n)
{
  for(int i=0; i<6; i++)
    std::cout << fP[i][n] << " ";
  std::cout << std::endl;
  
  for(int i=0; i<21; i++)
    std::cout << fC[i][n] << " ";
  std::cout << std::endl;
  
  std::cout //<< fChi2[n] << " " << fNDF[n] << " " <<  fId[n] << " " 
            << fPDG[n] << " " << fQ[n] << " " << fPVIndex[n] << std::endl;
}

void KFPTrackVector::Print()
{
  std::cout << "NTracks " << Size() << std::endl;
  if( Size()==0 ) return;
  
  std::cout << "Parameters: " << std::endl;
  for(int iP=0; iP<6; iP++)
  {
    std::cout << "  iP " << iP << ": ";
    for(int iTr=0; iTr<Size(); iTr++)
      std::cout << Parameter(iP)[iTr]<< " ";
    std::cout << std::endl;
  }
  
  std::cout << "Cov matrix: " << std::endl;
  for(int iC=0; iC<21; iC++)
  {
    std::cout << "  iC " << iC << ": ";
    for(int iTr=0; iTr<Size(); iTr++)
      std::cout << Covariance(iC)[iTr]<< " ";
    std::cout << std::endl;
  }

  std::cout << "Id: " << std::endl;
  for(int iTr=0; iTr<Size(); iTr++)
    std::cout <<  Id()[iTr] << " ";
  std::cout << std::endl;
  
  std::cout << "Pdg: " << std::endl;
  for(int iTr=0; iTr<Size(); iTr++)
    std::cout <<  PDG()[iTr] << " ";
  std::cout << std::endl;

  std::cout << "Q: " << std::endl;
  for(int iTr=0; iTr<Size(); iTr++)
    std::cout <<  Q()[iTr] << " ";
  std::cout << std::endl;
  
  std::cout << "PV index: " << std::endl;
  for(int iTr=0; iTr<Size(); iTr++)
    std::cout <<  PVIndex()[iTr] << " ";
  std::cout << std::endl;
  
#ifdef NonhomogeneousField
  std::cout << "Field: " << std::endl;
  for(int iF=0; iF<6; iF++)
  {
    std::cout << "  iF " << iF << ": ";
    for(int iTr=0; iTr<Size(); iTr++)
      std::cout << FieldCoefficient(iF)[iTr]<< " ";
    std::cout << std::endl;
  }
#endif
  
  std::cout << "Last particle index: " << std::endl;
  std::cout << LastElectron() << " "
            << LastMuon() << " "
            << LastPion() << " "
            << LastKaon() << " "
            << LastProton() << " "
            << LastDeuteron() << " "
            << LastTritium() << " "
            << LastHe3() << " "
            << LastHe4() << std::endl;
}
  