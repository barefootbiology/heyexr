// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// datetime_to_raw
RawVector datetime_to_raw(int sec_epoch);
RcppExport SEXP _heyexr_datetime_to_raw(SEXP sec_epochSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type sec_epoch(sec_epochSEXP);
    rcpp_result_gen = Rcpp::wrap(datetime_to_raw(sec_epoch));
    return rcpp_result_gen;
END_RCPP
}
// raw_to_datetime
Datetime raw_to_datetime(RawVector src);
RcppExport SEXP _heyexr_raw_to_datetime(SEXP srcSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RawVector >::type src(srcSEXP);
    rcpp_result_gen = Rcpp::wrap(raw_to_datetime(src));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_heyexr_datetime_to_raw", (DL_FUNC) &_heyexr_datetime_to_raw, 1},
    {"_heyexr_raw_to_datetime", (DL_FUNC) &_heyexr_raw_to_datetime, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_heyexr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
