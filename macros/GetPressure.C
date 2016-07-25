void GetPressure() {

gSystem->Load("StDbLib"); 
gSystem->Load("SttpcGasConditions");
SttpcGasConditions* gc= new SttpcGasConditions();
float pressure=gc->getPressure(966297600);
cout << "Barometric Pressure is="<<pressure<<endl; 

};
