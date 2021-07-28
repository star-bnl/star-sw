/***************************************************************************
 *
 * Author: Frank Laue, Ohio State, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 **************************************************************************/
#include "StHbtMaker/Infrastructure/StHbtTagWriter.hh"

#ifdef __ROOT__
    ClassImp(StHbtTagWriter)
#endif

StHbtTagWriter* StHbtTagWriter::_instance=0;

StHbtTagWriter* StHbtTagWriter::Instance() {
    if (_instance == 0 ) _instance = new StHbtTagWriter();
    return _instance;
}

StHbtTagWriter::StHbtTagWriter() { cout << " StHbtTagWriter::StHbtTagWriter() " << endl; }

void StHbtTagWriter::Clear() { 
  for ( int i=0; i<5; i++) {
    mHbtTag.chargedParticles_Means[i] = 0;
    mHbtTag.chargedParticles_Sigmas[i] = 0;
    mHbtTag.positiveParticles_Means[i] = 0;
    mHbtTag.positiveParticles_Sigmas[i] = 0;
    mHbtTag.negativeParticles_Means[i] = 0;
    mHbtTag.negativeParticles_Sigmas[i] = 0;
    mHbtTag.positivePions_Means[i] = 0;
    mHbtTag.positivePions_Sigmas[i] = 0;
    mHbtTag.negativePions_Means[i] = 0;
    mHbtTag.negativePions_Sigmas[i] = 0;
    mHbtTag.positiveKaons_Means[i] = 0;
    mHbtTag.positiveKaons_Sigmas[i] = 0;
    mHbtTag.negativeKaons_Means[i] = 0;
    mHbtTag.negativeKaons_Sigmas[i] = 0;
    mHbtTag.protons_Means[i] = 0;
    mHbtTag.protons_Sigmas[i] = 0;
    mHbtTag.antiProtons_Means[i] = 0;
    mHbtTag.antiProtons_Sigmas[i] = 0;
  }
  cout << " StHbtTagWriter::Clear() " << endl;
}

void StHbtTagWriter::SetTag(const char* c, unsigned char i, float v) { 
  if ( i<5) {
    if ( strcmp(c,"")==0 && c!=0) /* no-op */;
    else if ( strcmp(c,"chargedParticlesMeans")==0 ) mHbtTag.chargedParticles_Means[i] = v; 
    else if ( strcmp(c,"chargedParticlesSigmas")==0 ) mHbtTag.chargedParticles_Sigmas[i] = v; 
    else if ( strcmp(c,"positiveParticlesMeans")==0 ) mHbtTag.positiveParticles_Means[i] = v; 
    else if ( strcmp(c,"positiveParticlesSigmas")==0 ) mHbtTag.positiveParticles_Sigmas[i] = v; 
    else if ( strcmp(c,"negativeParticlesMeans")==0 ) mHbtTag.negativeParticles_Means[i] = v; 
    else if ( strcmp(c,"negativeParticlesSigmas")==0 ) mHbtTag.negativeParticles_Sigmas[i] = v; 
    else if ( strcmp(c,"positivePionsMeans")==0 ) mHbtTag.positivePions_Means[i] = v; 
    else if ( strcmp(c,"positivePionsSigmas")==0 ) mHbtTag.positivePions_Sigmas[i] = v; 
    else if ( strcmp(c,"negativePionsMeans")==0 ) mHbtTag.negativePions_Means[i] = v; 
    else if ( strcmp(c,"negativePionsSigmas")==0 ) mHbtTag.negativePions_Sigmas[i] = v; 
    else if ( strcmp(c,"positiveKaonsMeans")==0 ) mHbtTag.positiveKaons_Means[i] = v; 
    else if ( strcmp(c,"positiveKaonsSigmas")==0 ) mHbtTag.positiveKaons_Sigmas[i] = v; 
    else if ( strcmp(c,"negativeKaonsMeans")==0 ) mHbtTag.negativeKaons_Means[i] = v; 
    else if ( strcmp(c,"negativeKaonsSigmas")==0 ) mHbtTag.negativeKaons_Sigmas[i] = v; 
    else if ( strcmp(c,"protonsMeans")==0 ) mHbtTag.protons_Means[i] = v; 
    else if ( strcmp(c,"protonsSigmas")==0 ) mHbtTag.protons_Sigmas[i] = v; 
    else if ( strcmp(c,"antiProtonsMeans")==0 ) mHbtTag.antiProtons_Means[i] = v; 
    else if ( strcmp(c,"antiProtonsSigmas")==0 ) mHbtTag.antiProtons_Sigmas[i] = v; 
    else { cerr << " StHbtTagWriter::SetTag() - unrecognized tag " << endl;}
  }
}

float StHbtTagWriter::Tag(const char* c, unsigned char i) { 
  if ( i<5) {
    if ( strcmp(c,"")==0 && c!=0) /* no-op */;
    else if ( strcmp(c,"chargedParticlesMeans")==0 ) return mHbtTag.chargedParticles_Means[i]; 
    else if ( strcmp(c,"chargedParticlesSigmas")==0 ) return mHbtTag.chargedParticles_Sigmas[i]; 
    else if ( strcmp(c,"positiveParticlesMeans")==0 ) return mHbtTag.positiveParticles_Means[i]; 
    else if ( strcmp(c,"positiveParticlesSigmas")==0 ) return mHbtTag.positiveParticles_Sigmas[i]; 
    else if ( strcmp(c,"negativeParticlesMeans")==0 ) return mHbtTag.negativeParticles_Means[i]; 
    else if ( strcmp(c,"negativeParticlesSigmas")==0 ) return mHbtTag.negativeParticles_Sigmas[i]; 
    else if ( strcmp(c,"positivePionsMeans")==0 ) return mHbtTag.positivePions_Means[i]; 
    else if ( strcmp(c,"positivePionsSigmas")==0 ) return mHbtTag.positivePions_Sigmas[i]; 
    else if ( strcmp(c,"negativePionsMeans")==0 ) return mHbtTag.negativePions_Means[i]; 
    else if ( strcmp(c,"negativePionsSigmas")==0 ) return mHbtTag.negativePions_Sigmas[i]; 
    else if ( strcmp(c,"positiveKaonsMeans")==0 ) return mHbtTag.positiveKaons_Means[i]; 
    else if ( strcmp(c,"positiveKaonsSigmas")==0 ) return mHbtTag.positiveKaons_Sigmas[i]; 
    else if ( strcmp(c,"negativeKaonsMeans")==0 ) return mHbtTag.negativeKaons_Means[i]; 
    else if ( strcmp(c,"negativeKaonsSigmas")==0 ) return mHbtTag.negativeKaons_Sigmas[i]; 
    else if ( strcmp(c,"protonsMeans")==0 ) return mHbtTag.protons_Means[i]; 
    else if ( strcmp(c,"protonsSigmas")==0 ) return mHbtTag.protons_Sigmas[i]; 
    else if ( strcmp(c,"antiProtonsMeans")==0 ) return mHbtTag.antiProtons_Means[i]; 
    else if ( strcmp(c,"antiProtonsSigmas")==0 ) return mHbtTag.antiProtons_Sigmas[i];
    else { cerr << " StHbtTagWriter::Tag() - unrecognized tag " << endl; return 0.;}
  }
  return 0;
}

