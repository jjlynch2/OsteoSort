// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// euclidean_distance_matrix_rcpp
Rcpp::NumericVector euclidean_distance_matrix_rcpp(Rcpp::NumericMatrix a, Rcpp::NumericMatrix b);
RcppExport SEXP _OsteoSort_euclidean_distance_matrix_rcpp(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type a(aSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(euclidean_distance_matrix_rcpp(a, b));
    return rcpp_result_gen;
END_RCPP
}
// max_directional_hausdorff_rcpp2
double max_directional_hausdorff_rcpp2(Rcpp::NumericMatrix a, Rcpp::NumericMatrix b);
RcppExport SEXP _OsteoSort_max_directional_hausdorff_rcpp2(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type a(aSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(max_directional_hausdorff_rcpp2(a, b));
    return rcpp_result_gen;
END_RCPP
}
// mean_directional_hausdorff_rcpp
double mean_directional_hausdorff_rcpp(Rcpp::NumericMatrix a, Rcpp::NumericMatrix b);
RcppExport SEXP _OsteoSort_mean_directional_hausdorff_rcpp(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type a(aSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(mean_directional_hausdorff_rcpp(a, b));
    return rcpp_result_gen;
END_RCPP
}
// max_directional_hausdorff_rcpp
double max_directional_hausdorff_rcpp(Rcpp::NumericMatrix a, Rcpp::NumericMatrix b);
RcppExport SEXP _OsteoSort_max_directional_hausdorff_rcpp(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type a(aSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(max_directional_hausdorff_rcpp(a, b));
    return rcpp_result_gen;
END_RCPP
}
// dilated_directional_hausdorff_rcpp
double dilated_directional_hausdorff_rcpp(Rcpp::NumericMatrix a, Rcpp::NumericMatrix b);
RcppExport SEXP _OsteoSort_dilated_directional_hausdorff_rcpp(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type a(aSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(dilated_directional_hausdorff_rcpp(a, b));
    return rcpp_result_gen;
END_RCPP
}
// minimum_euclidean_distances_indices
Rcpp::NumericMatrix minimum_euclidean_distances_indices(Rcpp::NumericMatrix a, Rcpp::NumericMatrix b);
RcppExport SEXP _OsteoSort_minimum_euclidean_distances_indices(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type a(aSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(minimum_euclidean_distances_indices(a, b));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_OsteoSort_euclidean_distance_matrix_rcpp", (DL_FUNC) &_OsteoSort_euclidean_distance_matrix_rcpp, 2},
    {"_OsteoSort_max_directional_hausdorff_rcpp2", (DL_FUNC) &_OsteoSort_max_directional_hausdorff_rcpp2, 2},
    {"_OsteoSort_mean_directional_hausdorff_rcpp", (DL_FUNC) &_OsteoSort_mean_directional_hausdorff_rcpp, 2},
    {"_OsteoSort_max_directional_hausdorff_rcpp", (DL_FUNC) &_OsteoSort_max_directional_hausdorff_rcpp, 2},
    {"_OsteoSort_dilated_directional_hausdorff_rcpp", (DL_FUNC) &_OsteoSort_dilated_directional_hausdorff_rcpp, 2},
    {"_OsteoSort_minimum_euclidean_distances_indices", (DL_FUNC) &_OsteoSort_minimum_euclidean_distances_indices, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_OsteoSort(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
