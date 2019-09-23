#include <RxODE_model_shared.h>
#define __MAX_PROD__ 0
#define _CMT CMT
extern void  mod1__ode_solver_solvedata (rx_solve *solve){
  _solveData = solve;
}
extern rx_solve *mod1__ode_solver_get_solvedata(){
  return _solveData;
}
SEXP mod1__model_vars();
double _theta[11];
extern double* mod1__theta(double *theta){
  _theta[0] = theta[0];
_theta[1] = theta[1];
_theta[2] = theta[2];
_theta[3] = theta[3];
_theta[4] = theta[4];
_theta[5] = theta[5];
_theta[6] = theta[6];
_theta[7] = theta[7];
_theta[8] = theta[8];
_theta[9] = theta[9];
_theta[10] = theta[10];
  return _theta;
}


// prj-specific differential eqns
void mod1__dydt(int *_neq, double t, double *__zzStateVar__, double *__DDtStateVar__)
{
  int _cSub = _neq[1];
  double   Concentration,
  centr,
  V2,
  C3,
  peri,
  V3,
  C4,
  peri2,
  V4,
  depot,
  KA,
  CL,
  Q,
  Q2,
  eff,
  Kin,
  Emax,
  EC50,
  Kout;

  (void)t;
  (void)Concentration;
  (void)centr;
  (void)V2;
  (void)C3;
  (void)peri;
  (void)V3;
  (void)C4;
  (void)peri2;
  (void)V4;
  (void)depot;
  (void)KA;
  (void)CL;
  (void)Q;
  (void)Q2;
  (void)eff;
  (void)Kin;
  (void)Emax;
  (void)EC50;
  (void)Kout;

  _update_par_ptr(t, _cSub, _solveData, _idx);
  V2 = _PP[0];
  V3 = _PP[1];
  V4 = _PP[2];
  KA = _PP[3];
  CL = _PP[4];
  Q = _PP[5];
  Q2 = _PP[6];
  Kin = _PP[7];
  Emax = _PP[8];
  EC50 = _PP[9];
  Kout = _PP[10];

  depot = __zzStateVar__[0]*((double)(_ON[0]));
  centr = __zzStateVar__[1]*((double)(_ON[1]));
  peri = __zzStateVar__[2]*((double)(_ON[2]));
  peri2 = __zzStateVar__[3]*((double)(_ON[3]));
  eff = __zzStateVar__[4]*((double)(_ON[4]));

  Concentration=centr/safe_zero(V2);
  C3=peri/safe_zero(V3);
  C4=peri2/safe_zero(V4);
  __DDtStateVar__[0] = ((double)(_ON[0]))*(_IR[0] -KA*depot);
  __DDtStateVar__[1] = ((double)(_ON[1]))*(_IR[1] + KA*depot-(CL+Q+Q2)*Concentration+Q*C3+Q2*C4);
  __DDtStateVar__[2] = ((double)(_ON[2]))*(_IR[2] + Q*Concentration-Q*C3);
  __DDtStateVar__[3] = ((double)(_ON[3]))*(_IR[3] + Q2*Concentration-Q2*C4);
  __DDtStateVar__[4] = ((double)(_ON[4]))*(_IR[4] + Kin*(1+Emax*Concentration/safe_zero((EC50+Concentration)))-Kout*(1)*eff);
  (&_solveData->subjects[_cSub])->dadt_counter[0]++;
}

// Jacobian derived vars
void mod1__calc_jac(int *_neq, double t, double *__zzStateVar__, double *__PDStateVar__, unsigned int __NROWPD__) {
  int _cSub=_neq[1];
  (&_solveData->subjects[_cSub])->jac_counter[0]++;
}
// Functional based initial conditions.
void mod1__inis(int _cSub, double *__zzStateVar__){
}
// prj-specific derived vars
void mod1__calc_lhs(int _cSub, double t, double *__zzStateVar__, double *_lhs) {
  double   __DDtStateVar_0__,
  __DDtStateVar_1__,
  __DDtStateVar_2__,
  __DDtStateVar_3__,
  __DDtStateVar_4__,
  Concentration,
  centr,
  V2,
  C3,
  peri,
  V3,
  C4,
  peri2,
  V4,
  depot,
  KA,
  CL,
  Q,
  Q2,
  eff,
  Kin,
  Emax,
  EC50,
  Kout;

  (void)t;
  (void)__DDtStateVar_0__;
  (void)__DDtStateVar_1__;
  (void)__DDtStateVar_2__;
  (void)__DDtStateVar_3__;
  (void)__DDtStateVar_4__;
  (void)Concentration;
  (void)centr;
  (void)V2;
  (void)C3;
  (void)peri;
  (void)V3;
  (void)C4;
  (void)peri2;
  (void)V4;
  (void)depot;
  (void)KA;
  (void)CL;
  (void)Q;
  (void)Q2;
  (void)eff;
  (void)Kin;
  (void)Emax;
  (void)EC50;
  (void)Kout;

  _update_par_ptr(t, _cSub, _solveData, _idx);
  V2 = _PP[0];
  V3 = _PP[1];
  V4 = _PP[2];
  KA = _PP[3];
  CL = _PP[4];
  Q = _PP[5];
  Q2 = _PP[6];
  Kin = _PP[7];
  Emax = _PP[8];
  EC50 = _PP[9];
  Kout = _PP[10];

  depot = __zzStateVar__[0]*((double)(_ON[0]));
  centr = __zzStateVar__[1]*((double)(_ON[1]));
  peri = __zzStateVar__[2]*((double)(_ON[2]));
  peri2 = __zzStateVar__[3]*((double)(_ON[3]));
  eff = __zzStateVar__[4]*((double)(_ON[4]));

  Concentration=centr/safe_zero(V2);
  C3=peri/safe_zero(V3);
  C4=peri2/safe_zero(V4);
  __DDtStateVar_0__ = ((double)(_ON[0]))*(_IR[0] -KA*depot);
  __DDtStateVar_1__ = ((double)(_ON[1]))*(_IR[1] + KA*depot-(CL+Q+Q2)*Concentration+Q*C3+Q2*C4);
  __DDtStateVar_2__ = ((double)(_ON[2]))*(_IR[2] + Q*Concentration-Q*C3);
  __DDtStateVar_3__ = ((double)(_ON[3]))*(_IR[3] + Q2*Concentration-Q2*C4);
  __DDtStateVar_4__ = ((double)(_ON[4]))*(_IR[4] + Kin*(1+Emax*Concentration/safe_zero((EC50+Concentration)))-Kout*(1)*eff);

  _lhs[0]=Concentration;
  _lhs[1]=C3;
  _lhs[2]=C4;
}
// Functional based bioavailability
double mod1__F(int _cSub,  int _cmt, double _amt, double t){
 return _amt;
}
// Functional based absorption lag
double mod1__Lag(int _cSub,  int _cmt, double t){
 return t;
  double   Concentration,
  centr,
  V2,
  C3,
  peri,
  V3,
  C4,
  peri2,
  V4,
  depot,
  KA,
  CL,
  Q,
  Q2,
  eff,
  Kin,
  Emax,
  EC50,
  Kout;

  (void)t;
  (void)Concentration;
  (void)centr;
  (void)V2;
  (void)C3;
  (void)peri;
  (void)V3;
  (void)C4;
  (void)peri2;
  (void)V4;
  (void)depot;
  (void)KA;
  (void)CL;
  (void)Q;
  (void)Q2;
  (void)eff;
  (void)Kin;
  (void)Emax;
  (void)EC50;
  (void)Kout;

  _update_par_ptr(NA_REAL, _cSub, _solveData, _idx);
  V2 = _PP[0];
  V3 = _PP[1];
  V4 = _PP[2];
  KA = _PP[3];
  CL = _PP[4];
  Q = _PP[5];
  Q2 = _PP[6];
  Kin = _PP[7];
  Emax = _PP[8];
  EC50 = _PP[9];
  Kout = _PP[10];

}
// Modeled zero-order rate
double mod1__Rate(int _cSub,  int _cmt, double _amt, double t){
 return 0.0;
  double   Concentration,
  centr,
  V2,
  C3,
  peri,
  V3,
  C4,
  peri2,
  V4,
  depot,
  KA,
  CL,
  Q,
  Q2,
  eff,
  Kin,
  Emax,
  EC50,
  Kout;

  (void)t;
  (void)Concentration;
  (void)centr;
  (void)V2;
  (void)C3;
  (void)peri;
  (void)V3;
  (void)C4;
  (void)peri2;
  (void)V4;
  (void)depot;
  (void)KA;
  (void)CL;
  (void)Q;
  (void)Q2;
  (void)eff;
  (void)Kin;
  (void)Emax;
  (void)EC50;
  (void)Kout;

  _update_par_ptr(NA_REAL, _cSub, _solveData, _idx);
  V2 = _PP[0];
  V3 = _PP[1];
  V4 = _PP[2];
  KA = _PP[3];
  CL = _PP[4];
  Q = _PP[5];
  Q2 = _PP[6];
  Kin = _PP[7];
  Emax = _PP[8];
  EC50 = _PP[9];
  Kout = _PP[10];

}
// Modeled zero-order duration
double mod1__Dur(int _cSub,  int _cmt, double _amt, double t){
 return 0.0;
}
// Model Times
void mod1__mtime(int _cSub, double *_mtime){
}
extern SEXP mod1__model_vars(){
  int pro=0;
  SEXP _mv = PROTECT(_rxGetModelLib("mod1__model_vars"));pro++;
  if (!_rxIsCurrentC(_mv)){
    SEXP lst      = PROTECT(allocVector(VECSXP, 20));pro++;
    SEXP names    = PROTECT(allocVector(STRSXP, 20));pro++;
    SEXP sNeedSort = PROTECT(allocVector(INTSXP,1));pro++;
    int *iNeedSort  = INTEGER(sNeedSort);
    iNeedSort[0] = 0;
    SEXP sMtime = PROTECT(allocVector(INTSXP,1));pro++;
    int *iMtime  = INTEGER(sMtime);
    iMtime[0] = 0;
    SEXP sExtraCmt = PROTECT(allocVector(INTSXP,1));pro++;
    int *iExtraCmt  = INTEGER(sExtraCmt);
    iExtraCmt[0] = 0;
    SEXP params   = PROTECT(allocVector(STRSXP, 11));pro++;
    SEXP lhs      = PROTECT(allocVector(STRSXP, 3));pro++;
    SEXP state    = PROTECT(allocVector(STRSXP, 5));pro++;
  SEXP extraState = PROTECT(allocVector(STRSXP, 0));pro++;
    SEXP stateRmS = PROTECT(allocVector(INTSXP, 5));pro++;
    SEXP timeInt = PROTECT(allocVector(INTSXP, 1));pro++;
    INTEGER(timeInt)[0] = 1568995479;
    SEXP sens     = PROTECT(allocVector(STRSXP, 0));pro++;
    SEXP normState= PROTECT(allocVector(STRSXP, 5));pro++;
    SEXP fn_ini   = PROTECT(allocVector(STRSXP, 0));pro++;
    SEXP dfdy     = PROTECT(allocVector(STRSXP, 0));pro++;
    SEXP tran     = PROTECT(allocVector(STRSXP, 20));pro++;
    SEXP trann    = PROTECT(allocVector(STRSXP, 20));pro++;
    SEXP mmd5     = PROTECT(allocVector(STRSXP, 2));pro++;
    SEXP mmd5n    = PROTECT(allocVector(STRSXP, 2));pro++;
    SEXP model    = PROTECT(allocVector(STRSXP, 1));pro++;
    SEXP modeln   = PROTECT(allocVector(STRSXP, 1));pro++;
    SEXP version    = PROTECT(allocVector(STRSXP, 3));pro++;
    SEXP versionn   = PROTECT(allocVector(STRSXP, 3));pro++;
    SET_STRING_ELT(version,0,mkChar("0.8.1-0"));
    SET_STRING_ELT(version,1,mkChar("https://github.com/nlmixrdevelopment/RxODE"));
    SET_STRING_ELT(version,2,mkChar("2ef740cbdf83ec58b612d7c6d08130e4"));
    SET_STRING_ELT(versionn,0,mkChar("version"));
    SET_STRING_ELT(versionn,1,mkChar("repo"));
    SET_STRING_ELT(versionn,2,mkChar("md5"));
  SET_STRING_ELT(lhs,0,mkChar("Concentration"));
    SET_STRING_ELT(params,0,mkChar("V2"));
  SET_STRING_ELT(lhs,1,mkChar("C3"));
    SET_STRING_ELT(params,1,mkChar("V3"));
  SET_STRING_ELT(lhs,2,mkChar("C4"));
    SET_STRING_ELT(params,2,mkChar("V4"));
    SET_STRING_ELT(params,3,mkChar("KA"));
    SET_STRING_ELT(params,4,mkChar("CL"));
    SET_STRING_ELT(params,5,mkChar("Q"));
    SET_STRING_ELT(params,6,mkChar("Q2"));
    SET_STRING_ELT(params,7,mkChar("Kin"));
    SET_STRING_ELT(params,8,mkChar("Emax"));
    SET_STRING_ELT(params,9,mkChar("EC50"));
    SET_STRING_ELT(params,10,mkChar("Kout"));
    SET_STRING_ELT(state,0,mkChar("depot"));
    SET_STRING_ELT(normState,0,mkChar("depot"));
    _SR[0] = 0;
    SET_STRING_ELT(state,1,mkChar("centr"));
    SET_STRING_ELT(normState,1,mkChar("centr"));
    _SR[1] = 0;
    SET_STRING_ELT(state,2,mkChar("peri"));
    SET_STRING_ELT(normState,2,mkChar("peri"));
    _SR[2] = 0;
    SET_STRING_ELT(state,3,mkChar("peri2"));
    SET_STRING_ELT(normState,3,mkChar("peri2"));
    _SR[3] = 0;
    SET_STRING_ELT(state,4,mkChar("eff"));
    SET_STRING_ELT(normState,4,mkChar("eff"));
    _SR[4] = 0;
    SET_STRING_ELT(modeln,0,mkChar("normModel"));
    SET_STRING_ELT(model,0,mkChar("Concentration=centr/V2;\nC3=peri/V3;\nC4=peri2/V4;\nd/dt(depot)=-KA*depot;\nd/dt(centr)=KA*depot-(CL+Q+Q2)*Concentration+Q*C3+Q2*C4;\nd/dt(peri)=Q*Concentration-Q*C3;\nd/dt(peri2)=Q2*Concentration-Q2*C4;\nd/dt(eff)=Kin*(1+Emax*Concentration/(EC50+Concentration))-Kout*(1)*eff;\n"));
    SEXP ini    = PROTECT(allocVector(REALSXP,0));pro++;
    SEXP inin   = PROTECT(allocVector(STRSXP, 0));pro++;
    SET_STRING_ELT(names,0,mkChar("params"));
    SET_VECTOR_ELT(lst,  0,params);
    SET_STRING_ELT(names,1,mkChar("lhs"));
    SET_VECTOR_ELT(lst,  1,lhs);
    SET_STRING_ELT(names,2,mkChar("state"));
    SET_VECTOR_ELT(lst,  2,state);
    SET_STRING_ELT(names,3,mkChar("trans"));
    SET_VECTOR_ELT(lst,  3,tran);
    SET_STRING_ELT(names,4,mkChar("model"));
    SET_VECTOR_ELT(lst,  4,model);
    SET_STRING_ELT(names,5,mkChar("ini"));
    SET_VECTOR_ELT(lst,  5,ini);
    SET_STRING_ELT(names,6,mkChar("podo"));
    SET_VECTOR_ELT(lst,   6,ScalarLogical(0));
    SET_STRING_ELT(names,7,mkChar("dfdy"));
    SET_VECTOR_ELT(lst,  7,dfdy);
    SET_STRING_ELT(names,8,mkChar("sens"));
    SET_VECTOR_ELT(lst,  8,sens);
    SET_STRING_ELT(names,9,mkChar("fn.ini"));
    SET_VECTOR_ELT(lst,  9,fn_ini);
    SET_STRING_ELT(names,10,mkChar("state.ignore"));
    SET_VECTOR_ELT(lst,  10,stateRmS);
    SET_STRING_ELT(names,11,mkChar("version"));
    SET_VECTOR_ELT(lst,  11,version);
    SET_STRING_ELT(names,12,mkChar("normal.state"));
    SET_VECTOR_ELT(lst,  12,normState);
    SET_STRING_ELT(names,13,mkChar("needSort"));
    SET_VECTOR_ELT(lst,  13,sNeedSort);
    SET_STRING_ELT(names,14,mkChar("nMtime"));
    SET_VECTOR_ELT(lst,  14,sMtime);
    SET_STRING_ELT(names,15,mkChar("extraCmt"));
    SET_VECTOR_ELT(lst,  15,sExtraCmt);
    SET_STRING_ELT(names, 16, mkChar("stateExtra"));
    SET_VECTOR_ELT(lst,  16, extraState);
    SET_STRING_ELT(names, 17, mkChar("dvid"));
    SEXP sDvid = PROTECT(allocVector(INTSXP,0));pro++;
    SET_VECTOR_ELT(lst, 17, sDvid);
    SET_STRING_ELT(names,18,mkChar("timeId"));
    SET_VECTOR_ELT(lst,  18,timeInt);
    SET_STRING_ELT(names,19,mkChar("md5"));    SET_VECTOR_ELT(lst,  19,mmd5);    SET_STRING_ELT(mmd5n,0,mkChar("file_md5"));
    SET_STRING_ELT(mmd5,0,mkChar("b5f6adcf916e29d98def6fb724f55f0d"));
    SET_STRING_ELT(mmd5n,1,mkChar("parsed_md5"));
    SET_STRING_ELT(mmd5,1,mkChar("896b723dcf77517dca527807d89a226b"));
    SET_STRING_ELT(trann,0,mkChar("lib.name"));
    SET_STRING_ELT(tran, 0,mkChar("mod1_"));
    SET_STRING_ELT(trann,1,mkChar("jac"));
    SET_STRING_ELT(tran,1,mkChar("fullint"));
    SET_STRING_ELT(trann,2,mkChar("prefix"));
    SET_STRING_ELT(tran, 2,mkChar("mod1__"));
    SET_STRING_ELT(trann,3,mkChar("dydt"));
    SET_STRING_ELT(tran, 3,mkChar("mod1__dydt"));
    SET_STRING_ELT(trann,4,mkChar("calc_jac"));
    SET_STRING_ELT(tran, 4,mkChar("mod1__calc_jac"));
    SET_STRING_ELT(trann,5,mkChar("calc_lhs"));
    SET_STRING_ELT(tran, 5,mkChar("mod1__calc_lhs"));
    SET_STRING_ELT(trann,6,mkChar("model_vars"));
    SET_STRING_ELT(tran, 6,mkChar("mod1__model_vars"));
    SET_STRING_ELT(trann,7,mkChar("theta"));
    SET_STRING_ELT(tran, 7,mkChar("mod1__theta"));
    SET_STRING_ELT(trann,8,mkChar("inis"));
    SET_STRING_ELT(tran, 8,mkChar("mod1__inis"));
    SET_STRING_ELT(trann,  9,mkChar("dydt_lsoda"));
    SET_STRING_ELT(tran,   9,mkChar("mod1__dydt_lsoda"));
    SET_STRING_ELT(trann,10,mkChar("calc_jac_lsoda"));
    SET_STRING_ELT(tran, 10,mkChar("mod1__calc_jac_lsoda"));
    SET_STRING_ELT(trann,11,mkChar("ode_solver_solvedata"));
    SET_STRING_ELT(tran, 11,mkChar("mod1__ode_solver_solvedata"));
    SET_STRING_ELT(trann,12,mkChar("ode_solver_get_solvedata"));
    SET_STRING_ELT(tran, 12,mkChar("mod1__ode_solver_get_solvedata"));
    SET_STRING_ELT(trann,13,mkChar("dydt_liblsoda"));
    SET_STRING_ELT(tran, 13,mkChar("mod1__dydt_liblsoda"));
    SET_STRING_ELT(trann,14,mkChar("F"));
    SET_STRING_ELT(tran, 14,mkChar("mod1__F"));
    SET_STRING_ELT(trann,15,mkChar("Lag"));
    SET_STRING_ELT(tran, 15,mkChar("mod1__Lag"));
    SET_STRING_ELT(trann,16,mkChar("Rate"));
    SET_STRING_ELT(tran, 16,mkChar("mod1__Rate"));
    SET_STRING_ELT(trann,17,mkChar("Dur"));
    SET_STRING_ELT(tran, 17,mkChar("mod1__Dur"));
    SET_STRING_ELT(trann,18,mkChar("mtime"));
    SET_STRING_ELT(tran, 18,mkChar("mod1__mtime"));
    SET_STRING_ELT(trann,19,mkChar("assignFuns"));
    SET_STRING_ELT(tran, 19,mkChar("mod1__assignFuns"));
    setAttrib(tran, R_NamesSymbol, trann);
    setAttrib(mmd5, R_NamesSymbol, mmd5n);
    setAttrib(model, R_NamesSymbol, modeln);
    setAttrib(ini, R_NamesSymbol, inin);
    setAttrib(version, R_NamesSymbol, versionn);
    setAttrib(lst, R_NamesSymbol, names);
    SEXP cls = PROTECT(allocVector(STRSXP, 1));pro++;
    SET_STRING_ELT(cls, 0, mkChar("rxModelVars"));
    classgets(lst, cls);
    _assign_ptr(lst);
    UNPROTECT(pro);
    return lst;
  } else {
    UNPROTECT(pro);
    return _mv;
  }
}
extern void mod1__dydt_lsoda(int *neq, double *t, double *A, double *DADT)
{
  mod1__dydt(neq, *t, A, DADT);
}
extern int mod1__dydt_liblsoda(double t, double *y, double *ydot, void *data)
{
  int *neq = (int*)(data);
  mod1__dydt(neq, t, y, ydot);
  return(0);
}
extern void mod1__calc_jac_lsoda(int *neq, double *t, double *A,int *ml, int *mu, double *JAC, int *nrowpd){
  // Update all covariate parameters
  mod1__calc_jac(neq, *t, A, JAC, *nrowpd);
}

//Create function to call from R's main thread that assigns the required functions. Sometimes they don't get assigned.
extern void mod1__assignFuns(){
  _assignFuns();
}

//Initialize the dll to match RxODE's calls
void R_init0_mod1_(){
  // Get C callables on load; Otherwise it isn't thread safe
  _assignFuns();
  R_RegisterCCallable("mod1_","mod1__assignFuns", (DL_FUNC) mod1__assignFuns);
  R_RegisterCCallable("mod1_","mod1__theta", (DL_FUNC) mod1__theta);
  R_RegisterCCallable("mod1_","mod1__inis",(DL_FUNC) mod1__inis);
  R_RegisterCCallable("mod1_","mod1__dydt",(DL_FUNC) mod1__dydt);
  R_RegisterCCallable("mod1_","mod1__calc_lhs",(DL_FUNC) mod1__calc_lhs);
  R_RegisterCCallable("mod1_","mod1__calc_jac",(DL_FUNC) mod1__calc_jac);
  R_RegisterCCallable("mod1_","mod1__dydt_lsoda", (DL_FUNC) mod1__dydt_lsoda);
  R_RegisterCCallable("mod1_","mod1__calc_jac_lsoda", (DL_FUNC) mod1__calc_jac_lsoda);
  R_RegisterCCallable("mod1_","mod1__ode_solver_solvedata", (DL_FUNC) mod1__ode_solver_solvedata);
  R_RegisterCCallable("mod1_","mod1__ode_solver_get_solvedata", (DL_FUNC) mod1__ode_solver_get_solvedata);
  R_RegisterCCallable("mod1_","mod1__F", (DL_FUNC) mod1__F);
  R_RegisterCCallable("mod1_","mod1__Lag", (DL_FUNC) mod1__Lag);
  R_RegisterCCallable("mod1_","mod1__Rate", (DL_FUNC) mod1__Rate);
  R_RegisterCCallable("mod1_","mod1__Dur", (DL_FUNC) mod1__Dur);
  R_RegisterCCallable("mod1_","mod1__mtime", (DL_FUNC) mod1__mtime);
  R_RegisterCCallable("mod1_","mod1__dydt_liblsoda", (DL_FUNC) mod1__dydt_liblsoda);
}
//Initialize the dll to match RxODE's calls
void R_init_mod1_(DllInfo *info){
  // Get C callables on load; Otherwise it isn't thread safe
  R_init0_mod1_();
  static const R_CallMethodDef callMethods[]  = {
    {"mod1__model_vars", (DL_FUNC) &mod1__model_vars, 0},
    {NULL, NULL, 0}
  };

  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info,FALSE);
}

void R_unload_mod1_ (DllInfo *info){
  // Free resources required for single subject solve.
  SEXP _mv = PROTECT(_rxGetModelLib("mod1__model_vars"));
  if (!isNull(_mv)){
    _rxRmModelLib("mod1__model_vars");
  }
  UNPROTECT(1);
}
