#include <Rcpp.h>

using namespace Rcpp;

//[[Rcpp::export]]
List dataEdaGen(DataFrame data,
		std::string id="",
		std::string evid="",
		std::string time=""){
  // This is to create
  //
  // 1. Time between dose and first observation after dose
  // 2. Time between last obervation and next dose
  // 3. Time between doses
  //
  // This function makes the following assumptions:
  // a. The ID variable is specified by the varible id or is ID, Id, id
  // b. The EVID variable is specified by the variable evid or is EVID Evid or evid
  //     - The EVID is 0 for observation records.
  //     - The EVID is !=r 0 for dosing records.
  // c. The TIME variable is specified by the time or is TIME Time or time
  int idi = -1, evidi=-1, timei=-1, i;
  CharacterVector nm = data.names();
  std::string cur;
  
  // This gets the column number of ID, EVID and TIME columns
  for (i = nm.size(); i--;){
    cur = as<std::string>(nm[i]);
    if (idi == -1 && 
	((id == "" && (cur == "ID" || cur == "Id" || cur == "id")) ||
	 id == cur)){
      idi = i;
    } else if (evidi == -1 && 
	       ((evid == "" && (cur == "EVID" || cur == "Evid" || cur == "evid")) ||
		evid == cur)){
      evidi = i;
    } else if (timei == -1 && 
	       ((time == "" && (cur == "TIME" || cur == "Time" || cur == "time")) ||
                time == cur)){
      timei = i;
    } else if (timei != -1 && evidi != -1 && idi != -1) break; // Found everything early exit.
  }
  if (timei == -1 || evidi == -1 || idi == -1) stop("Need time, evid and id variables.");
  IntegerVector idv = as<IntegerVector>(data[idi]);
  IntegerVector evidv = as<IntegerVector>(data[evidi]);
  NumericVector timev = as<NumericVector>(data[timei]);
  double lastObsTime = 1.0, lastDoseTime = -1.0;
  int lastDosei = -1, lastObsi = -1;
  int lastId=-1;
  // II dataset
  double* ii = Calloc(idv.size(),double);
  int* iiId = Calloc(idv.size(),int);
  int* iiRow1 = Calloc(idv.size(),int);
  int* iiRow2 = Calloc(idv.size(),int);
  // Dose->Observation dataest
  double* dod = Calloc(idv.size(),double);
  int* doId = Calloc(idv.size(),int);
  int* doRow1 = Calloc(idv.size(),int);
  int* doRow2 = Calloc(idv.size(),int);
  // Observation->Dose dataset
  double* odd = Calloc(idv.size(),double);
  int* odId = Calloc(idv.size(),int);
  int* odRow1 = Calloc(idv.size(),int);
  int* odRow2 = Calloc(idv.size(),int);
  unsigned int iii = 0, doi = 0, odi = 0;
  bool lastDose = true;
  for (i = idv.size(); i--;){
    if (lastId != idv[i]){
      lastObsTime = -1.0;
      lastDoseTime = -1.0;
      lastId = idv[i];
    }
    if (evidv[i] > 0){
      // Dosing Event
      if (lastDoseTime > 0){
	// II dataset:
	// ID II row1 row2
	ii[iii]   =  lastDoseTime - timev[i] ;
	iiId[iii] = idv[i];
	iiRow1[iii] = (int)(i+1);
        iiRow2[iii] = (int)(lastDosei+1);
	++iii;
      }
      if (!lastDose && lastObsTime > 0){
	// Dose->Observation
	dod[doi] = lastObsTime - timev[i];
	doId[doi] = idv[i];
	doRow1[doi] = (int)(i+1);
	doRow2[doi] = (int)(lastObsi+1);
	++doi;
      }
      lastDoseTime = timev[i];
      lastDosei = i;
      lastDose = true;
    } else {
      // Observation Event
      if (lastDose && lastDoseTime > 0){
	// Observation -> Dose (this is a backward loop)
        odd[odi] = lastDoseTime - timev[i];
	odId[odi] = idv[i];
	odRow1[odi] = (int)(i+1);
	odRow2[odi] = (int)(lastDosei+1);
	++odi;
      }
      lastObsTime = timev[i];
      lastObsi = i;
      lastDose = false;
    }
  }
  List ret(3);
  // Now create 3 datasets and return them as a list.
  
  // II Dataset
  NumericVector dfIIdouble = NumericVector(iii);
  memcpy(&dfIIdouble[0],ii, iii*sizeof(double));
  Free(ii);
  IntegerVector dfIIId = IntegerVector(iii);
  memcpy(&dfIIId[0], iiId, iii*sizeof(int));
  Free(iiId);
  IntegerVector dfIIRow1 = IntegerVector(iii);
  memcpy(&dfIIRow1[0], iiRow1, iii*sizeof(int));
  Free(iiRow1);
  IntegerVector dfIIRow2 = IntegerVector(iii);
  memcpy(&dfIIRow2[0], iiRow2, iii*sizeof(int));
  Free(iiRow2);
  DataFrame dfII = DataFrame::create(_["ID"] = dfIIId, _["II"] = dfIIdouble, _["row1"] = dfIIRow1, _["row2"] = dfIIRow2);
  ret[0] = dfII;
  
  // DO dataset
  NumericVector dfDOdouble = NumericVector(doi);
  memcpy(&dfDOdouble[0],dod, doi*sizeof(double));
  Free(dod);
  IntegerVector dfDOId = IntegerVector(doi);
  memcpy(&dfDOId[0], doId, doi*sizeof(int));
  Free(doId);
  IntegerVector dfDORow1 = IntegerVector(doi);
  memcpy(&dfDORow1[0], doRow1, doi*sizeof(int));
  Free(doRow1);
  IntegerVector dfDORow2 = IntegerVector(doi);
  memcpy(&dfDORow2[0], doRow2, doi*sizeof(int));
  Free(doRow2);
  DataFrame dfDO = DataFrame::create(_["ID"] = dfDOId, _["DO"] = dfDOdouble, _["row1"] = dfDORow1, _["row2"] = dfDORow2);
  ret[1] = dfDO;
  
  // OD dataset
  NumericVector dfODoduble = NumericVector(odi);
  memcpy(&dfODoduble[0],odd, odi*sizeof(double));
  Free(odd);
  IntegerVector dfODId = IntegerVector(odi);
  memcpy(&dfODId[0], odId, odi*sizeof(int));
  Free(odId);
  IntegerVector dfODRow1 = IntegerVector(odi);
  memcpy(&dfODRow1[0], odRow1, odi*sizeof(int));
  Free(odRow1);
  IntegerVector dfODRow2 = IntegerVector(odi);
  memcpy(&dfODRow2[0], odRow2, odi*sizeof(int));
  Free(odRow2);
  DataFrame dfOD = DataFrame::create(_["ID"] = dfODId, _["OD"] = dfODoduble, _["row1"] = dfODRow1, _["row2"] = dfODRow2);
  ret[2] = dfOD;
  ret.attr("names") = CharacterVector::create("II","DO","OD");
  return ret;
}
