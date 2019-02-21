#include <math.h>
#define max(a,b) (((a)>(b))?(a):(b))
#define min(a,b) (((a)<(b))?(a):(b))
extern long dadt_counter;
extern double InfusionRate[99];
extern double *par_ptr;
extern double podo;
extern double tlast;

// prj-specific differential eqns
void RxODE_mod_mod1_dydt(unsigned int neq, double t, double *__zzStateVar__, double *__DDtStateVar__)
{
double
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

	V2 = par_ptr[0];
	V3 = par_ptr[1];
	V4 = par_ptr[2];
	KA = par_ptr[3];
	CL = par_ptr[4];
	Q = par_ptr[5];
	Q2 = par_ptr[6];
	Kin = par_ptr[7];
	Emax = par_ptr[8];
	EC50 = par_ptr[9];
	Kout = par_ptr[10];

	depot = __zzStateVar__[0];
	centr = __zzStateVar__[1];
	peri = __zzStateVar__[2];
	peri2 = __zzStateVar__[3];
	eff = __zzStateVar__[4];

	Concentration = centr / V2;
	C3 = peri / V3;
	C4 = peri2 / V4;
	__DDtStateVar__[0] = InfusionRate[0] + - KA * depot;
	__DDtStateVar__[1] = InfusionRate[1] + KA * depot -( CL + Q + Q2) * Concentration + Q * C3 + Q2 * C4;
	__DDtStateVar__[2] = InfusionRate[2] + Q * Concentration - Q * C3;
	__DDtStateVar__[3] = InfusionRate[3] + Q2 * Concentration - Q2 * C4;
	__DDtStateVar__[4] = InfusionRate[4] + Kin *( 1 + Emax * Concentration /( EC50 + Concentration)) - Kout *( 1) * eff;
    dadt_counter++;
}

// prj-specific derived vars
void RxODE_mod_mod1_calc_lhs(double t, double *__zzStateVar__, double *lhs) {
double
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

	V2 = par_ptr[0];
	V3 = par_ptr[1];
	V4 = par_ptr[2];
	KA = par_ptr[3];
	CL = par_ptr[4];
	Q = par_ptr[5];
	Q2 = par_ptr[6];
	Kin = par_ptr[7];
	Emax = par_ptr[8];
	EC50 = par_ptr[9];
	Kout = par_ptr[10];

	depot = __zzStateVar__[0];
	centr = __zzStateVar__[1];
	peri = __zzStateVar__[2];
	peri2 = __zzStateVar__[3];
	eff = __zzStateVar__[4];

	Concentration = centr / V2;
	C3 = peri / V3;
	C4 = peri2 / V4;

	lhs[0]=Concentration;
	lhs[1]=C3;
	lhs[2]=C4;
}
