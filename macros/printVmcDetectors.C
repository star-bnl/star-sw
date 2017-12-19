void printVmcDetectors() {
   for (int i = 1; i <= 40; i++) {
     StarVMCDetector *det = StarVMCDetectorSet::instance()->GetVMCDetector(i);
     if (det) cout << det->GetFMT().Data() << endl;
   }
}
