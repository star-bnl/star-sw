#ifndef ARRAY_FUNCS_H
#define ARRAY_FUNCS_H

#define ZERO_ARRAY(X) for (UInt_t i = 0;i < sizeof(X)/sizeof(X[0]);i++) X[i] = 0;

#define DERIVE_E_FROM(EX, X) Float_t EX[sizeof(X)/sizeof((X)[0])]; ZERO_ARRAY(EX)

#define SCALE_POINTS(FROM, TO, K) for (UInt_t i = 0;i < sizeof(FROM)/sizeof((FROM)[0]);i++) (TO)[i] = (FROM)[i] * (K);
#define MULTIPLY(Y, C) SCALE_POINTS(Y, Y, C)

#define DIVIDE_POINTS(NOM, DENOM, RESULT) for (UInt_t i = 0;i < sizeof(NOM)/sizeof((NOM)[0]);i++) (RESULT)[i] = (NOM)[i] / (DENOM)[i];

#define ADD_ERROR_POINTS(E, EADD) for (UInt_t i = 0;i < sizeof(E)/sizeof((E)[0]);i++) (E)[i] = TMath::Sqrt(((E)[i] * (E)[i]) + ((EADD)[i] * (EADD)[i]));

#define ADD_REL_ERROR_POINTS(Y, E, Y1, E1, Y2, E2) \
for (UInt_t i = 0;i < sizeof(E)/sizeof((E)[0]);i++) (E)[i] = (Y)[i] * TMath::Sqrt((((E1)[i]/(Y1)[i]) * ((E1)[i]/(Y1)[i])) + (((E2)[i]/(Y2)[i]) * ((E2)[i]/(Y2)[i])));

#endif
