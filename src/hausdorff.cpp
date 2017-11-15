#include <Rcpp.h>
#include <RcppParallel.h>
#include <limits>

// [[Rcpp::depends(RcppParallel)]]
struct minimum_euclidean_distances_ind : public RcppParallel::Worker {
	//Input
	const RcppParallel::RMatrix<double> a;
	const RcppParallel::RMatrix<double> b;

	//Output
	RcppParallel::RMatrix<double> medm;

	minimum_euclidean_distances_ind(const Rcpp::NumericMatrix a, const Rcpp::NumericMatrix b, Rcpp::NumericMatrix medm) : a(a), b(b), medm(medm) {}

	void operator() (std::size_t begin, std::size_t end) {
		for(std::size_t i = begin; i < end; i++) {
			double new_low = std::numeric_limits<double>::max();
			double new_low_ind;
			for(std::size_t j = 0; j < b.nrow(); j++) {
				double dsum = 0;
				for(std::size_t z = 0; z < b.ncol(); z++) {
					dsum = dsum + pow(a(i,z) - b(j,z), 2);
				}
				dsum = pow(dsum, 0.5);
				if(dsum < new_low) {
					new_low = dsum;
					new_low_ind = j;
				}
			}
			medm(i,0) = new_low;
			medm(i,1) = new_low_ind+1;
		}
	}
};

// [[Rcpp::depends(RcppParallel)]]
struct minimum_euclidean_distances : public RcppParallel::Worker {
	//Input
	const RcppParallel::RMatrix<double> a;
	const RcppParallel::RMatrix<double> b;

	//Output
	RcppParallel::RVector<double> medm;

	minimum_euclidean_distances(const Rcpp::NumericMatrix a, const Rcpp::NumericMatrix b, Rcpp::NumericVector medm) : a(a), b(b), medm(medm) {}

	void operator() (std::size_t begin, std::size_t end) {
		for(std::size_t i = begin; i < end; i++) {
			double new_low = std::numeric_limits<double>::max();
			for(std::size_t j = 0; j < b.nrow(); j++) {
				double dsum = 0;
				for(std::size_t z = 0; z < b.ncol(); z++) {
					dsum = dsum + pow(a(i,z) - b(j,z), 2);
				}
				dsum = pow(dsum, 0.5);
				if(dsum < new_low) {
					new_low = dsum;
				}
			}
			medm[i] = new_low;
		}
	}
};


// [[Rcpp::export]]
double mean_directional_hausdorff_rcpp(Rcpp::NumericMatrix a, Rcpp::NumericMatrix b){
	Rcpp::NumericVector medm(a.nrow());
	minimum_euclidean_distances minimum_euclidean_distances(a, b, medm);
	RcppParallel::parallelFor(0, a.nrow(), minimum_euclidean_distances);	
	double results = Rcpp::sum(medm);
	results = results / a.nrow();
	return results;
}


// [[Rcpp::export]]
double max_directional_hausdorff_rcpp(Rcpp::NumericMatrix a, Rcpp::NumericMatrix b){
	Rcpp::NumericVector medm(a.nrow());
	minimum_euclidean_distances minimum_euclidean_distances(a, b, medm);
	RcppParallel::parallelFor(0, a.nrow(), minimum_euclidean_distances);	
	double results = Rcpp::max(medm);
	return results;
}

// [[Rcpp::export]]
double dilated_directional_hausdorff_rcpp(Rcpp::NumericMatrix a, Rcpp::NumericMatrix b){
	Rcpp::NumericVector medm(a.nrow());
	minimum_euclidean_distances minimum_euclidean_distances(a, b, medm);
	RcppParallel::parallelFor(0, a.nrow(), minimum_euclidean_distances);	
	double results = Rcpp::sum(medm);
	double sd = Rcpp::sd(medm);
	results = results / a.nrow();
	results = results * sd;
	return results;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix minimum_euclidean_distances_indices(Rcpp::NumericMatrix a, Rcpp::NumericMatrix b){
	Rcpp::NumericMatrix medm(a.nrow(),2);
	minimum_euclidean_distances_ind minimum_euclidean_distances_ind(a, b, medm);
	RcppParallel::parallelFor(0, a.nrow(), minimum_euclidean_distances_ind);	
	return medm;
}



