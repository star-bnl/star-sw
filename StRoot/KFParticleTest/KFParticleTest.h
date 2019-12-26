/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef KFParticleTest_H
#define KFParticleTest_H

class KFParticle;
class KFParticleSIMD;

class KFParticleTest
{
 public:
  
  KFParticleTest();
  ~KFParticleTest();
  
  void PrintTutorial();
  void RunTest();
  
 private:
   
  void RunTestSingle();
  void RunTestSIMD();
  void CompareSingleAndSIMDResults();
  
  KFParticle* fMotherSingle;
  KFParticleSIMD* fMotherSIMD;
  
#ifndef KFParticleStandalone
  ClassDef( KFParticleTest, 1 )
#endif
};

#endif //KFParticleTest_H

